;;;Copyright (c) 2010-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; basic typed language concepts
;;
;;Tag identifiers
;;---------------
;;
;;A "tag identifier" is a bound identifier whose syntactic binding label gensym has a
;;specific   entry  in   its  property   list;  such   entry  has   an  instance   of
;;"object-type-spec" as value.  Tag identifiers must  be bound (otherwise they do not
;;have a  syntactic binding label), but  it does not  matter to what they  are bound.
;;Typical examples of tag identifiers are:
;;
;;* Struct type identifiers defined by DEFINE-STRUCT; they are automatically made tag
;;  identifiers by Vicare.
;;
;;*  R6RS   record  type   identifiers  defined   by  DEFINE-RECORD-TYPE;   they  are
;;  automatically made tag identifiers by Vicare.
;;
;;* A  set of non-core macro  identifiers (whose implementation is  integrated in the
;;  expander) are exported by the library "(vicare  expander tags)" to be the tags of
;;  built-in Vicare objects.   Some of them are:  "<fixnum>", "<string>", "<vector>",
;;  "<textual-input-port>".
;;
;;We can easily create a tag identifier as:
#|
     (import (vicare)
       (vicare expander tags)
       (for (prefix (vicare expander object-type-specs) typ.)
         expand))

     (define-syntax <my-tag>
       (let ()
         (set-identifier-tag! #'<my-tag>
           (make-object-type-spec #'<my-tag> #'<top> ...))
         (lambda (x) #f)))
|#
;;in which "<top>" is used as parent tag.
;;
;;
;;Tagged binding
;;--------------
;;
;;A "tagged binding" is a bound identifier whose syntactic binding label gensym has a
;;specific entry  in its  property list; such  entry has a  tag identifier  as value.
;;Tagged identifiers  must be bound (otherwise  they do not have  a syntactic binding
;;label).   Tagged bindings  are created  by  the built-in  binding syntaxes  LAMBDA,
;;DEFINE, LET, LETREC, LET-VALUES, etc.
;;
;;An example of tagged binding creation follows:
#|
     #!vicare
     (import (vicare)
       (vicare expander tags))

     (define {O <fixnum>}
       123)
|#
;;the braces are used to tag the first identifier with the second identifier.  At the
;;time  the tagged  binding is  created: the  tag identifier  must already  be a  tag
;;identifier.
;;


(library (psyntax.tag-and-tagged-identifiers)
  (export

    object-type-spec
    make-object-type-spec
    object-type-spec?				false-or-object-type-spec?
    object-type-spec-uids			set-object-type-spec-uids!
    object-type-spec-type-id			set-object-type-spec-type-id!
    object-type-spec-pred-stx			set-object-type-spec-pred-stx!
    object-type-spec-constructor-maker		set-object-type-spec-constructor-maker!
    object-type-spec-accessor-maker		set-object-type-spec-accessor-maker!
    object-type-spec-mutator-maker		set-object-type-spec-mutator-maker!
    object-type-spec-getter-maker		set-object-type-spec-getter-maker!
    object-type-spec-setter-maker		set-object-type-spec-setter-maker!
    object-type-spec-caster-maker		set-object-type-spec-caster-maker!
    object-type-spec-dispatcher			set-object-type-spec-dispatcher!
    object-type-spec-parent-spec		set-object-type-spec-parent-spec!
    object-type-spec-ancestry

    ;; object type specification queries
    tag-identifier-constructor-maker
    tag-identifier-predicate
    tag-identifier-accessor
    tag-identifier-mutator
    tag-identifier-getter
    tag-identifier-setter
    tag-identifier-dispatch

    ;; tag identifiers
    set-identifier-object-type-spec!		identifier-object-type-spec
    $identifier-object-type-spec
    set-label-object-type-spec!			label-object-type-spec
    tag-identifier?				false-or-tag-identifier?
    tag-identifier-and-list-sub-tag?
    assert-tag-identifier?			assert-list-sub-tag-identifier?
    tag-super-and-sub?				all-tag-identifiers?
    tag-identifier-ancestry			tag-common-ancestor

    ;; tagged identifiers
    tagged-identifier?
    set-identifier-tag!				identifier-tag		override-identifier-tag!
    set-label-tag!				label-tag		override-label-tag!
    identifier-tag-retvals-signature

    ;; fabricated tag identifiers
    make-retvals-signature-with-fabricated-procedure-tag
    fabricate-procedure-tag-identifier
    set-tag-identifier-callable-signature!
    tag-identifier-callable-signature

    ;; debugging helpers
    print-identifier-info

    ;; tag identifiers utilities
    initialise-type-spec-for-built-in-object-types
    retvals-signature-of-datum
    procedure-tag-id		$procedure-tag-id?	procedure-tag-id?
    list-tag-id			$list-tag-id?		list-tag-id?
    top-tag-id			$top-tag-id?		top-tag-id?
    boolean-tag-id		void-tag-id
    struct-tag-id		record-tag-id		predicate-tag-id

    ;; lambda formals signatures
    standard-formals-syntax?
    formals-signature-syntax?
    retvals-signature-syntax?
    formals-signature-partially-untagged-syntax?
    $formals-signature-partially-untagged-syntax?
    retvals-signature-partially-unspecified-syntax?
    $retvals-signature-partially-unspecified-syntax?
    formals-signature-super-and-sub-syntax?
    $formals-signature-super-and-sub-syntax?
    retvals-signature-super-and-sub-syntax?
    $retvals-signature-super-and-sub-syntax?
    retvals-signature-syntax-common-ancestor

    retvals-signature
    retvals-signature?
    retvals-signature-tags

    formals-signature
    formals-signature?
    formals-signature-tags

    lambda-signature
    lambda-signature?
    lambda-signature-retvals
    lambda-signature-formals

    clambda-compound
    clambda-compound?
    clambda-compound-common-retvals-signature
    clambda-compound-lambda-signatures

    make-formals-signature
    make-retvals-signature
    make-lambda-signature
    make-clambda-compound
    make-retvals-signature-single-top
    make-retvals-signature-single-procedure
    make-retvals-signature-standalone-list
    make-retvals-signature-fully-unspecified
    make-retvals-signature-single-value
    lambda-signature=?
    lambda-signature-formals-tags
    lambda-signature-retvals-tags
    list-of-lambda-signatures?
    lambda-signature-fully-unspecified?
    formals-signature=?
    formals-signature-super-and-sub?
    formals-signature-fully-unspecified?
    retvals-signature-fully-unspecified?
    retvals-signature-partially-unspecified?
    retvals-signature-super-and-sub?
    retvals-signature-single-tag?
    retvals-signature-single-top-tag?
    retvals-signature-single-tag-or-fully-unspecified?
    retvals-signature=?
    retvals-signature-common-ancestor
    tagged-identifier-syntax?
    parse-tagged-identifier-syntax
    parse-list-of-tagged-bindings
    list-of-tagged-bindings?
    parse-tagged-formals-syntax
    tagged-formals-syntax?
    parse-tagged-lambda-proto-syntax
    tagged-lambda-proto-syntax?
    )
  (import (except (rnrs)
		  generate-temporaries)
    (prefix (psyntax.lexical-environment)
	    lex.)
    (only (psyntax.syntax-match)
	  syntax-match
	  improper-list->list-and-rest
	  proper-list->head-and-last)
    (psyntax.syntactic-binding-properties)
    (only (psyntax.syntax-utilities)
	  syntax-unwrap)
    (psyntax.compat))


;;;; helpers

(define-syntax-rule (S ?sym)
  (lex.core-prim-id (quote ?sym)))

(define-syntax-rule (M ?sym)
  (lex.core-prim-id (quote ?sym)))

(include "psyntax.helpers.scm" #t)


;;;; expand-time object type specification

(define-record (object-type-spec %make-object-type-spec object-type-spec?)
  ;;A type representing  the object type to which expressions  in syntax objects will
  ;;evaluate.  All the Scheme objects are meant to be representable with this type.
  ;;
  (uids
		;A non-empty proper list of  symbols uniquely identifying this object
		;type  specification.    The  first  symbol  in   the  list  uniquely
		;identifies this record instance.
   type-id
		;The  bound identifier  representing  the name  of  this type.   This
		;identifier has this very instance  in its syntactic binding property
		;list.
   pred-stx
		;A syntax  object (wrapped  or unwrapped) representing  an expression
		;which will evaluate to a type predicate.
   constructor-maker
		;False or a constructor maker procedure.
   accessor-maker
		;False or an accessor maker procedure.
   mutator-maker
		;False or a mutator maker procedure.
   getter-maker
		;False or a getter maker procedure.
   setter-maker
		;False or a setter maker procedure.
   caster-maker
		;False or a caster maker procedure.
   dispatcher
		;False or a method dispatcher procedure.
   parent-spec
		;False or an instance of  "object-type-spec" describing the parent of
		;this type.   Only "<top>" has this  field set to false;  every other
		;"object-type-spec"  has  a parent  spec.   "<top>"  is the  implicit
		;parent of  all the type  specs; "<top>"  is the tag  of single-value
		;untagged bindings.
   ))

(case-define* make-object-type-spec
  (({type-id	lex.identifier-bound?}
    {parent-id	tag-identifier?}
    {pred-stx	lex.syntax-object?})
   (let* ((parent-spec (identifier-object-type-spec parent-id))
	  (uid         (lex.id->label type-id))
	  (uids        (list uid (object-type-spec-uids parent-spec))))
     (%make-object-type-spec uids type-id pred-stx
			     #f ;constructor-maker
			     #f ;accessor-maker
			     #f ;mutator-maker
			     #f ;getter-maker
			     #f ;setter-maker
			     #f ;cast-maker
			     #f ;method-dispatcher
			     parent-spec)))

  (({type-id	lex.identifier-bound?}
    {parent-id	tag-identifier?}
    {pred-stx	lex.syntax-object?}
    {construct	false-or-procedure?}
    {accessor	false-or-procedure?}
    {mutator	false-or-procedure?}
    {getter	false-or-procedure?}
    {setter	false-or-procedure?}
    {caster	false-or-procedure?}
    {dispatcher	false-or-procedure?})
   (let* ((parent-spec (identifier-object-type-spec parent-id))
	  (uid         (lex.id->label type-id))
	  (uids        (list uid (object-type-spec-uids parent-spec))))
     (%make-object-type-spec uids type-id pred-stx
			     construct accessor mutator getter setter caster dispatcher parent-spec))))

(define (false-or-object-type-spec? obj)
  (or (not obj)
      (object-type-spec? obj)))

;;; --------------------------------------------------------------------

(module (object-type-spec-ancestry)

  (define* (object-type-spec-ancestry {spec object-type-spec?})
    ($object-type-spec-ancestry spec))

  (define ($object-type-spec-ancestry spec)
    (cons ($object-type-spec-type-id spec)
	  (cond (($object-type-spec-parent-spec spec)
		 => (lambda (pspec)
		      ($object-type-spec-ancestry pspec)))
		(else '()))))

  #| end of module |# )


;;;; object type specification queries

(case-define* tag-identifier-constructor-maker
  ((tag-id)
   (tag-identifier-constructor-maker tag-id #f))
  (({tag-id tag-identifier?} input-form.stx)
   (cond ((identifier-object-type-spec tag-id)
	  => (lambda (spec)
	       (cond ((object-type-spec-constructor-maker spec)
		      => (lambda (constructor-maker)
			   (constructor-maker input-form.stx)))
		     (else
		      (syntax-violation __who__
			"undefined tag constructor maker" input-form.stx tag-id)))))
	 (else
	  ;;This should never happen because we  have validated the identifier in the
	  ;;fender.
	  (lex.syntax-violation/internal-error __who__
	    "tag identifier without object-type-spec" input-form.stx tag-id)))))

(case-define* tag-identifier-predicate
  ;;Given  a tag  identifier:  retrieve from  the  associated "object-type-spec"  the
  ;;predicate syntax object.   If successful: return a syntax  object representing an
  ;;expression  which,  expanded  by  itself  and  evaluated,  will  return  the  tag
  ;;predicate.
  ;;
  ((tag-id)
   (tag-identifier-predicate tag-id #f))
  (({tag-id tag-identifier?} input-form.stx)
   (cond ((identifier-object-type-spec tag-id)
	  => (lambda (spec)
	       (or (object-type-spec-pred-stx spec)
		   ;;This   should    never   happen    because   an    instance   of
		   ;;"object-type-spec" always has a defined predicate.
		   (lex.syntax-violation/internal-error __who__
		     "undefined tag predicate" input-form.stx tag-id))))
	 (else
	  ;;This should never happen because we  have validated the identifier in the
	  ;;fender.
	  (lex.syntax-violation/internal-error __who__
	    "tag identifier without object-type-spec" input-form.stx tag-id)))))

(case-define* tag-identifier-accessor
  ;;Given   a  tag   identifier  and   a  field   name:  search   the  hierarchy   of
  ;;"object-type-spec" associated  to TAG-ID for  an accessor of the  selected field.
  ;;If successful: return a syntax  object representing an expression which, expanded
  ;;by itself and evaluated, will return the field accessor; if no accessor is found:
  ;;raise an exception.
  ;;
  ((tag-id field-name-id)
   (tag-identifier-accessor tag-id field-name-id #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} input-form.stx)
   (let loop ((spec ($identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;object-type-specs  until an  object-type-spec without  parent has  been
	    ;;found.  The serach for the field accessor has failed.
	    (syntax-violation __who__
	      "object type does not provide selected field accessor"
	      input-form.stx field-name-id))
	   (($object-type-spec-accessor-maker spec)
	    => (lambda (accessor-maker)
		 (or (accessor-maker (syntax->datum field-name-id) input-form.stx)
		     ;;The field is unknown: try with the parent.
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no accessor maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-mutator
  ;;Given   a  tag   identifier  and   a  field   name:  search   the  hierarchy   of
  ;;"object-type-spec" associated to TAG-ID for an mutator of the selected field.  If
  ;;successful: return a syntax object  representing an expression which, expanded by
  ;;itself and  evaluated, will  return the  field mutator; if  no mutator  is found:
  ;;raise an exception.
  ;;
  ((tag-id field-name-id)
   (tag-identifier-mutator tag-id field-name-id #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} input-form.stx)
   (let loop ((spec ($identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field mutator has failed.
	    (syntax-violation __who__
	      "object type does not provide selected field mutator"
	      input-form.stx field-name-id))
	   (($object-type-spec-mutator-maker spec)
	    => (lambda (mutator-maker)
		 (or (mutator-maker (syntax->datum field-name-id) input-form.stx)
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no mutator maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-getter
  ;;Given  a   tag  identifier  and   a  set  of   keys:  search  the   hierarchy  of
  ;;"object-type-spec"  associated to  TAG-ID for  a getter  accepting the  keys.  If
  ;;successful: return a syntax object  representing an expression which, expanded by
  ;;itself and  evaluated, will return  the getter; if no  getter is found:  raise an
  ;;exception.
  ;;
  ((tag-id keys.stx)
   (tag-identifier-getter tag-id keys.stx #f))
  (({tag-id tag-identifier?} {keys.stx lex.syntax-object?} input-form.stx)
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field getter has failed.
	    (syntax-violation __who__
	      "object type does not provide getter syntax" input-form.stx tag-id))
	   (($object-type-spec-getter-maker spec)
	    => (lambda (getter-maker)
		 (or (getter-maker keys.stx input-form.stx)
		     ;;The keys are unknown: try with the parent.
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no getter maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-setter
  ;;Given  a   tag  identifier  and   a  set  of   keys:  search  the   hierarchy  of
  ;;"object-type-spec"  associated to  TAG-ID for  a setter  accepting the  keys.  If
  ;;successful: return a syntax object  representing an expression which, expanded by
  ;;itself and  evaluated, will return  the setter; if no  setter is found:  raise an
  ;;exception.
  ;;
  ((tag-id keys.stx)
   (tag-identifier-setter tag-id keys.stx #f))
  (({tag-id tag-identifier?} {keys.stx lex.syntax-object?} input-form.stx)
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field setter has failed.
	    (syntax-violation __who__
	      "object type does not provide setter syntax" input-form.stx))
	   (($object-type-spec-setter-maker spec)
	    => (lambda (setter-maker)
		 (or (setter-maker keys.stx input-form.stx)
		     ;;The keys are unknown: try with the parent.
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no setter maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(module (tag-identifier-dispatch)
  (define-syntax __module_who__
    (identifier-syntax 'tag-identifier-dispatch))

  (define* (tag-identifier-dispatch {tag tag-identifier?} {member.id identifier?} {input-form.stx lex.syntax-object?})
    ;;Given  a tag  identifier  and  a member  identifier:  search  the hierarchy  of
    ;;"object-type-spec" associated  to TAG for  a dispatcher accepting  MEMBER.ID as
    ;;method name  or, if not found,  an accessor maker accepting  MEMBER.ID as field
    ;;name.  If successful: return a  syntax object representing an expression which,
    ;;expanded by  itself and evaluated, will  return the method or  the accessor; if
    ;;neither a method nor an accessor is found: raise an exception.
    ;;
    ;;We expect INPUT-FORM.STX to have the format:
    ;;
    ;;   (?expr ?member ?arg ...)
    ;;
    ;;where: ?EXPR  is an expression of  type TAG; ?MEMBER is  an identifier matching
    ;;the name  of a method or  field of TAG or  one of its supertags  (the MEMBER.ID
    ;;argument); the ?ARG are additional operands.
    ;;
    (cond ((tag-super-and-sub? (procedure-tag-id) tag)
	   input-form.stx)
	  ((top-tag-id? tag)
	   (%error-invalid-tagged-syntax input-form.stx))
	  (else
	   (%try-dispatcher ($identifier-object-type-spec tag) (syntax->datum member.id)
			    input-form.stx))))

  (define (%try-dispatcher spec member.sym input-form.stx)
    (cond ((not spec)
	   ;;If   we  wre   here:  we   have   climbed  upwards   the  hierarchy   of
	   ;;"object-type-spec" until "<top>" was found;  "<top>" has no parent.  The
	   ;;expression in the tagged syntax has no matching methor nor field.
	   (%error-invalid-tagged-syntax input-form.stx))
	  (($object-type-spec-dispatcher spec)
	   => (lambda (dispatcher)
		(or (dispatcher member.sym input-form.stx)
		    (%try-accessor spec member.sym input-form.stx))))
	  (else
	   (%try-accessor spec member.sym input-form.stx))))

  (define (%try-accessor spec member.sym input-form.stx)
    (define (%try-parent-dispatcher)
      (%try-dispatcher ($object-type-spec-parent-spec spec) member.sym input-form.stx))
    (cond (($object-type-spec-accessor-maker spec)
	   => (lambda (accessor-maker)
		(or (accessor-maker member.sym input-form.stx)
		    (%try-parent-dispatcher))))
	  (else
	   ;;There is no accessor maker, try the parent's dispatcher.
	   (%try-parent-dispatcher))))

  (define (%error-invalid-tagged-syntax input-form.stx)
    (syntax-violation __module_who__ "invalid tagged syntax" input-form.stx))

  #| end of module: TAG-DISPATCH |# )


;;;; tag identifiers

(define-constant *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*
  'vicare:expander:object-type-spec)

;;; --------------------------------------------------------------------

(define* (set-identifier-object-type-spec! {type-id lex.identifier-bound?} {spec object-type-spec?})
  ;;Add to  the syntactic binding  label property list  an entry representing  a type
  ;;specification.  When this call succeeds: TYPE-ID becomes a tag identifier.
  ;;
  (let ((label (lex.id->label/or-error __who__ type-id type-id)))
    (cond (($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	   => (lambda (old-spec)
		(assertion-violation __who__
		  "identifier is already a tag, will not overwrite object-type-spec"
		  type-id old-spec spec)))
	  (($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*)
	   => (lambda (bind-tag)
		(assertion-violation __who__
		  "tagged identifier cannot become a tag identifier" type-id bind-tag spec)))
	  (else
	   ($putprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE* spec)))))

(define* ({identifier-object-type-spec false-or-object-type-spec?} {tag lex.identifier-bound?})
  ;;Retrieve from  the syntactic binding  label property list  the "object-type-spec"
  ;;describing the type specification; return false if no such entry exists.
  ;;
  ($identifier-object-type-spec tag))

(define ($identifier-object-type-spec tag)
  ;;Retrieve from  the syntactic binding  label property list  the "object-type-spec"
  ;;describing the type specification; return false if no such entry exists.
  ;;
  ($syntactic-binding-getprop tag *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-label-object-type-spec! {label symbol?} {spec object-type-spec?})
  ;;Add to LABEL's property list an entry representing a type specification; LABEL is
  ;;meant to be  a syntactic binding label.  When this  call succeeds: the associated
  ;;identifier becomes a tag identifier.
  ;;
  (cond (($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	 => (lambda (old-spec)
	      (assertion-violation __who__
		"object specification already defined for label" label old-spec spec)))
	(($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*)
	 => (lambda (bind-tag)
	      (assertion-violation __who__
		"tagged identifier's label cannot become a tag identifier" label bind-tag spec)))
	(else
	 ($putprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE* spec))))

(define* ({label-object-type-spec false-or-object-type-spec?} {label symbol?})
  ;;Retrieve from  LABEL's property list  the "object-type-spec" describing  the type
  ;;specification; return  false if  no such entry  exists.  LABEL is  meant to  be a
  ;;syntactic binding label.
  ;;
  ($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*))

;;; --------------------------------------------------------------------

(define (tag-identifier? obj)
  ;;Return true  if OBJ is a  bound identifier with "object-type-spec"  property set;
  ;;otherwise return false.
  ;;
  (and (identifier? obj)
       (lex.~identifier-bound? obj)
       (and ($identifier-object-type-spec obj)
	    #t)))

(define (tag-identifier-and-list-sub-tag? obj)
  ;;Return true if OBJ is a bound identifier with "object-type-spec" property set and
  ;;it is a sub-tag of "<list>"; otherwise return false.
  ;;
  (and (identifier? obj)
       (lex.~identifier-bound? obj)
       ($identifier-object-type-spec obj)
       (tag-super-and-sub? (list-tag-id) obj)
       #t))

(define (false-or-tag-identifier? obj)
  (or (not obj)
      (tag-identifier? obj)))

(define (assert-tag-identifier? obj)
  (unless (tag-identifier? obj)
    (syntax-violation #f
      "expected tag identifier, identifier with object-type-spec set" obj)))

(define (assert-list-sub-tag-identifier? obj)
  (unless (and (tag-identifier? obj)
	       (tag-super-and-sub? (list-tag-id) obj))
    (syntax-violation #f
      "expected sub-tag of <list> identifier" obj)))

(module (tag-super-and-sub?)

  (define* (tag-super-and-sub? {super-tag tag-identifier?} {sub-tag tag-identifier?})
    ;;Given two  tag identifiers: return  true if SUPER-TAG is  FREE-IDENTIFIER=?  to
    ;;SUB-TAG or one of its ancestors.
    ;;
    ($tag-super-and-sub? super-tag sub-tag))

  (define ($tag-super-and-sub? super-tag sub-tag)
    (or (lex.~free-identifier=? super-tag sub-tag)
	(lex.~free-identifier=? (top-tag-id) super-tag)
	(let ((pspec ($object-type-spec-parent-spec ($identifier-object-type-spec sub-tag))))
	  (and pspec
	       (let ((sub-ptag ($object-type-spec-type-id pspec)))
		 (and (not (lex.~free-identifier=? (top-tag-id) sub-ptag))
		      ($tag-super-and-sub? super-tag sub-ptag)))))))

  #| end of module |# )

(define (all-tag-identifiers? stx)
  ;;Return true  if STX is  a proper or improper  list of tag  identifiers; otherwise
  ;;return false.
  ;;
  (syntax-match stx ()
    (() #t)
    ((?arg . ?rest)
     (tag-identifier? ?arg)
     (all-tag-identifiers? ?rest))
    (?rest
     (tag-identifier? ?rest))
    (_ #f)))

;;; --------------------------------------------------------------------

(define* (tag-identifier-ancestry {tag tag-identifier?})
  (object-type-spec-ancestry ($identifier-object-type-spec tag)))

;;; --------------------------------------------------------------------

(define* (tag-common-ancestor {tag1 tag-identifier?} {tag2 tag-identifier?})
  ;;Visit the hierarchy of parents of  the given tag identifiers, determine the first
  ;;common ancestor and return its tag identifier.
  ;;
  ;;FIXME Once the object type spec representation has been stabilised: this function
  ;;must be rewritten  in a more efficient manner, comparing  symbols with EQ? rather
  ;;than comparing identifiers with LEX.~FREE-IDENTIFIER=?.  (Marco Maggi; Sat Apr 5, 2014)
  ;;
  (if (or (lex.~free-identifier=? tag1 tag2)
	  ($top-tag-id? tag1)
	  ($top-tag-id? tag2))
      tag1
    (let ((anc2 (tag-identifier-ancestry tag2)))
      (let outer ((anc1 (tag-identifier-ancestry tag1)))
	(cond ((null? anc1)
	       (top-tag-id))
	      ((let inner ((anc2 anc2))
		 (cond ((null? anc2)
			#f)
		       ((lex.~free-identifier=? ($car anc1) ($car anc2))
			($car anc1))
		       (else
			(inner ($cdr anc2))))))
	      (else
	       (outer ($cdr anc1))))))))


;;;; tagged identifiers: expand-time binding type tagging

(define-constant *EXPAND-TIME-BINDING-TAG-COOKIE*
  'vicare:expander:binding-type-tagging)

;;; --------------------------------------------------------------------

(define* (set-identifier-tag! {binding-id lex.identifier-bound?} {tag tag-identifier?})
  ;;Given a  syntactic binding identifier:  add TAG to  its property list  as binding
  ;;type  tagging.  This  tag  should represent  the object  type  referenced by  the
  ;;binding.  When this call succeeds: BINDING-ID becomes a tagged identifier.
  ;;
  (let ((label (lex.id->label/or-error __who__ binding-id binding-id)))
    (cond (($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*)
	   => (lambda (old-tag)
		(assertion-violation __who__
		  "identifier is already tagged, will not overwrite old tag"
		  binding-id old-tag tag)))
	  (($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	   => (lambda (spec)
		(assertion-violation __who__
		  "tag identifier cannot become a tagged identifier"
		  binding-id spec tag)))
	  (else
	   ($putprop label *EXPAND-TIME-BINDING-TAG-COOKIE* tag)))))

(define* (override-identifier-tag! {binding-id lex.identifier-bound?} {tag tag-identifier?})
  ;;Given a  syntactic binding identifier:  add TAG to  its property list  as binding
  ;;type  tagging,  silently  overriding  the previous  property.   This  tag  should
  ;;represent the object type referenced by the binding.
  ;;
  ($syntactic-binding-putprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE* tag))

(module (identifier-tag
	 tagged-identifier?)

  (define* (identifier-tag {binding-id lex.identifier-bound?})
    ;;Given  a  syntactic binding  identifier:  retrieve  from  its property  list  the
    ;;identifier representing  the binding  type tagging.   This tag  identifier should
    ;;represent the object type referenced by the binding.
    ;;
    ($identifier-tag binding-id))

  (define* (tagged-identifier? {id lex.identifier-bound?})
    ;;Return #t  if ID is an  identifier having a type  tagging; otherwise
    ;;return false.  If the return value is true: ID is a bound identifier
    ;;created by some binding syntaxes (define, let, letrec, ...).
    ;;
    (and ($identifier-tag id)
	 #t))

  (define ($identifier-tag binding-id)
    ($syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE*))

  #| end of module |# )

(define* (identifier-tag-retvals-signature {id lex.identifier-bound?})
  (cond ((identifier-tag id)
	 => (lambda (tag)
	      (make-retvals-signature-single-value tag)))
	(else
	 (make-retvals-signature-single-top))))

;;; --------------------------------------------------------------------

(define* (set-label-tag! id {label symbol?} {tag tag-identifier?})
  ;;Given a syntactic binding  LABEL associated to the identifier ID:  add TAG to its
  ;;property list as binding type tagging.  This tag should represent the object type
  ;;referenced by the binding.
  ;;
  (cond (($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*)
	 => (lambda (old-tag)
	      (unless (free-identifier=? old-tag tag)
		(assertion-violation __who__
		  "label binding tag already defined (redefining a binding?)"
		  id label old-tag tag))))
	(($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	 => (lambda (spec)
	      (assertion-violation __who__
		"tag identifier cannot become a tagged identifier"
		id label spec tag)))
	(else
	 ($putprop label *EXPAND-TIME-BINDING-TAG-COOKIE* tag))))

(define* (override-label-tag! {label symbol?} {tag tag-identifier?})
  ;;Given a  syntactic binding LABEL:  add TAG to its  property list as  binding type
  ;;tagging.  This tag should represent the object type referenced by the binding.
  ;;
  ($putprop label *EXPAND-TIME-BINDING-TAG-COOKIE* tag))

(define* (label-tag {label identifier?})
  ;;Given a syntactic binding LABEL: retrieve from its property list the
  ;;identifier  representing   the  binding  type  tagging.    This  tag
  ;;identifier  should  represent  the  object type  referenced  by  the
  ;;binding.
  ;;
  ($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*))


;;;; fabricated tag identifiers
;;
;;Whenever a  LAMBDA or CASE-LAMBDA form  is expanded: a signature  for the resulting
;;closure  object is  built; it  is either  an instance  of "lambda-signature"  or an
;;instance of "clambda-compound".   For example, when the following  LAMBDA syntax is
;;expanded:
;;
;;   (lambda ({_ <exact-integer>} {a <fixnum>} {b <fixnum>})
;;     (+ 1 a b))
;;
;;the LAMBDA proto parser builds the following "lambda-signature" struct:
;;
;;   #["lambda-signature"
;;       retvals=#["retvals-signature" tags=(#'<exact-integer>)]
;;       formals=#["formals-signature" tags=(#'<fixnum> #'<fixnum>)]]
;;
;;A new tag  identifier (not coming from  the source code) is built  and the callable
;;signature is stored in its properties  list; such tag identifier becomes the single
;;tag in the retvals signature of the closure object:
;;
;;   #["retvals-signature" tags=(#'?fresh-tag-identifier)]
;;
;;where ?FRESH-TAG-IDENTIFIER is built by the functions below.
;;
(module (make-retvals-signature-with-fabricated-procedure-tag
	 fabricate-procedure-tag-identifier
	 set-tag-identifier-callable-signature!
	 tag-identifier-callable-signature)

  (define* (make-retvals-signature-with-fabricated-procedure-tag {sym symbol?} {callable-signature callable-signature?})
    ;;Whenever a LAMBDA or CASE-LAMBDA syntax is  expanded, we need to create a fresh
    ;;tag identifier  to represent the  signature of  its closure object.   We assume
    ;;this function is called as:
    ;;
    ;;   (make-retvals-signature-with-fabricated-procedure-tag
    ;;      (gensym) ?lambda-signature)
    ;;
    ;;When a DEFINE syntax:
    ;;
    ;;   (define (?who . ?formals) . ?body)
    ;;
    ;;is expanded: a QRHS is created, then  the identifier ?WHO is bound, finally the
    ;;QRHS is  expanded.  When the QRHS  is expanded: we  need to create a  fresh tag
    ;;identifier to  represent the signature of  its closure object.  We  assume this
    ;;function is called as:
    ;;
    ;;   (make-retvals-signature-with-fabricated-procedure-tag
    ;;      (syntax->datum ?who) ?lambda-signature)
    ;;
    (make-retvals-signature-single-value
     ($fabricate-procedure-tag-identifier sym callable-signature)))

  (define* ({fabricate-procedure-tag-identifier tag-identifier?} {sym symbol?} {callable-signature callable-signature?})
    ;;When a DEFINE syntax:
    ;;
    ;;   (define (?who . ?formals) . ?body)
    ;;
    ;;is expanded: a QRHS is created, then  the identifier ?WHO is bound, finally the
    ;;QRHS is expanded.   When the binding for  ?WHO is created: we need  to create a
    ;;fresh tag identifier to represent the signature of its closure object; such tag
    ;;must be  created before  the LAMBDA is  expanded so that,  while the  LAMBDA is
    ;;expanded, ?WHO is already tagged.  We assume this function is called as:
    ;;
    ;;   (fabricate-procedure-tag-identifier (syntax->datum ?who) lambda-signature)
    ;;
    ($fabricate-procedure-tag-identifier sym callable-signature))

  (define ($fabricate-procedure-tag-identifier sym callable-signature)
    (receive (tag lab)
	(%fabricate-bound-identifier sym)
      ;;FIXME? We  create an instance  of "object-type-spec" with a  plain PROCEDURE?
      ;;as predicate.  This  is because there is  no way at run-time  to identify the
      ;;signature of a  closure object, so it is impossible  to properly validate it;
      ;;this is bad because  we pass on a value that is  not fully validated.  (Marco
      ;;Maggi; Fri Apr 4, 2014)
      (let ((spec  (make-object-type-spec tag (procedure-tag-id) (lex.procedure-pred-id))))
	(set-identifier-object-type-spec! tag spec)
	($putprop lab *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE* callable-signature))
      tag))

  (define* (%fabricate-bound-identifier {sym symbol?})
    ;;Build an  identifier having SYM as  name.  Return 2 values:  the identifier and
    ;;its label gensym.
    ;;
    ;;The returned identifier is bound in the sense that applying LEX.ID->LABEL to it
    ;;will return a label gensym; but applying LABEL->SYNTACTIC-BINDING to such label
    ;;will return:
    ;;
    ;;   (displaced-lexical . #f)
    ;;
    ;;as syntactic binding descriptor.  The  returned identifier if perfectly fine as
    ;;tag  identifier  because,  having  a  label,  it  can  hold  all  the  required
    ;;properties.  The only limit  is that it is not "truly  bound": the "value" slot
    ;;of the label does not reference a  syntactic binding descriptor and there is no
    ;;LEXENV in which its label is associated to a syntactic binding descriptor.
    ;;
    (let ((lab (gensym sym)))
      (values (lex.make-top-level-syntactic-identifier-from-symbol-and-label sym lab)
	      lab)))

  (define-constant *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE*
    'vicare:expander:tag-callable-signature)

  (define* (set-tag-identifier-callable-signature! {tag-id tag-identifier?} {callable-signature callable-signature?})
    (let ((label (lex.id->label/or-error __who__ tag-id tag-id)))
      (cond (($getprop label *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE*)
	     => (lambda (old-callable-signature)
		  (assertion-violation __who__
		    "identifier has an already assigned callable signature, will not overwrite old one"
		    tag-id old-callable-signature callable-signature)))
	    (else
	     ($putprop label *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE* callable-signature)))))

  (define* (tag-identifier-callable-signature {tag-id tag-identifier?})
    ;;Given a  tag identifier representing  a subtag of "<procedure>":  retrieve from
    ;;its property  list the callable  specification of the function.   If successful
    ;;return  an instance  of "lambda-signature"  or "clambda-compound";  if no  such
    ;;property is defined return false.
    ;;
    ($syntactic-binding-getprop tag-id *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE*))

  (define (callable-signature? obj)
    (or (lambda-signature? obj)
	(clambda-compound? obj)))

  #| end of module |# )


;;;; debugging helpers

(define* (print-identifier-info {id identifier?})
  ;;Given an  identifier object:  print on  the current  error port  a report  on its
  ;;properties as identifier; return unspecified values.
  ;;
  ;;FIXME We can do better in this function.  (Marco Maggi; Fri Apr 11, 2014)
  ;;
  (define (%print message . args)
    (apply fprintf (current-error-port) message args))
  (cond ((lex.id->label id)
	 => (lambda (label)
	      (%print "identifier: ~a\nlabel: ~a\n" (syntax->datum id) label)
	      (cond ((identifier-object-type-spec id)
		     => (lambda (spec)
			  (%print "tag identifier: yes\n")))
		    (else
		     (%print "tag identifier: no\n")))
	      (cond ((identifier-tag id)
		     => (lambda (tag)
			  (%print "tagged identifier: yes\ntype tag identifier: ~a\n"
				  tag)
			  (if (tag-super-and-sub? (procedure-tag-id) tag)
			      (begin
				(%print "type tag has <procedure> as parent: yes\n")
				(cond ((identifier-unsafe-variant id)
				       => (lambda (unsafe-stx)
					    (%print "identifier has unsafe variant: yes\n")
					    (%print "unsafe identifier variant: ~a\n" unsafe-stx)))
				      (else
				       (%print "identifier has unsafe variant: no\n")))
				(cond ((tag-identifier-callable-signature tag)
				       => (lambda (callable)
					    (%print "callable signature: ~a\n" callable)))
				      (else
				       (%print "callable signature: unspecified\n"))))
			    (%print "type tag has <procedure> as parent: no\n"))))
		    (else
		     (%print "tagged identifier: no\n")))
	      ))

	(else
	 (fprintf (current-error-port) "identifier: ~a not bound\n"
		  (syntax->datum id)))))


;;;; filled built-in tag template
;;
;; (let ()
;;
;;   (define-constant THE-TAG
;;     (S <tag>))
;;
;;   (define (%constructor-maker input-form.stx)
;;     (S make-object))
;;
;;   (define (%accessor-maker field.sym input-form.stx)
;;     (case field.sym
;;       ((string)		(S object->string))
;;       (else
;;        (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))
;;
;;   (define (%mutator-maker field.sym input-form.stx)
;;     (case field.sym
;;       ((value)		(S set-object-value!))
;;       (else
;;        (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))
;;
;;   (define (%getter-maker keys.stx input-form.stx)
;;     (define (%invalid-keys)
;;       (syntax-violation __who__ "invalid keys for getter" input-form.stx keys.stx))
;;     (syntax-match keys.stx ()
;;       (([?field])
;;        (identifier? ?field)
;;        (or (%accessor-maker (syntax->datum ?field) input-form.stx)
;; 	   (%invalid-keys)))
;;       (else
;;        (%invalid-keys))))
;;
;;   (define (%setter-maker keys.stx input-form.stx)
;;     (define (%invalid-keys)
;;       (syntax-violation __who__ "invalid keys for setter" input-form.stx keys.stx))
;;     (syntax-match keys.stx ()
;;       (([?field])
;;        (identifier? ?field)
;;        (or (%mutator-maker (syntax->datum ?field) input-form.stx)
;; 	   (%invalid-keys)))
;;       (else
;;        (%invalid-keys))))
;;
;;   (define (%caster-maker source.tag input-form.stx)
;;     (if source.tag
;; 	(cond ((lex.~free-identifier=? source.tag (S <string>))		(S string->object))
;; 	      (else
;; 	       (syntax-violation __who__
;; 		 "invalid cast source object type" input-form.stx source.tag)))
;;       (S any->object)))
;;
;;   (define (%dispatcher method-sym input-form.stx)
;;     (case method.sym
;;       ((putprop)		(M putprop))
;;       (else #f)))
;;
;;   (define type-spec
;;     (make-object-type-spec THE-TAG (S THE-PARENT) (S THE-PREDICATE)
;; 			   %constructor-maker
;; 			   %accessor-maker %mutator-maker
;; 			   %getter-maker %setter-maker
;; 			   %caster-maker %dispatcher))
;;
;;   (typ.set-identifier-object-type-spec! THE-TAG type-spec))
;;


;;;; empty built-in tag template
;;
;; (let ()
;;
;;   (define-constant THE-TAG
;;     (S <tag>))
;;
;;   (define %constructor-maker	#f)
;;   (define %accessor-maker	#f)
;;   (define %mutator-maker	#f)
;;   (define %getter-maker	#f)
;;   (define %setter-maker	#f)
;;   (define %caster-maker	#f)
;;   (define %dispatcher	#f)
;;
;;   (define type-spec
;;     (make-object-type-spec THE-TAG #'THE-PARENT #'THE-PREDICATE
;; 			   %constructor-maker
;; 			   %accessor-maker %mutator-maker
;; 			   %getter-maker %setter-maker
;; 			   %caster-maker %dispatcher))
;;
;;   (typ.set-identifier-object-type-spec! THE-TAG type-spec))
;;


;;;; built-in tags module

(module (initialise-type-spec-for-built-in-object-types)

  (define (initialise-type-spec-for-built-in-object-types)
    ;;The tag "<top>" is  special because it is the only one having  #f in the parent
    ;;spec field.  "<top>" is the default tag for single-value untagged bindings.
    (let ((tag-id   (top-tag-id))
	  (pred-id  (S always-true)))
      (set-identifier-object-type-spec! tag-id
	(%make-object-type-spec (list (lex.id->label tag-id)) tag-id pred-id
				#f #f #f  #f #f  #f #f #f)))

    (%basic '<void>		'<top>		'void-object?)
    (%basic '<procedure>	'<top>		'procedure?)
    (%basic '<boolean>		'<top>		'boolean?)

    (%basic '<predicate>	'<procedure>	'procedure?)
    (set-tag-identifier-callable-signature! (predicate-tag-id)
					    (make-lambda-signature
					     (make-retvals-signature-single-value (boolean-tag-id))
					     (make-formals-signature (list (top-tag-id)))))

;;; --------------------------------------------------------------------

    (%initialise-<symbol>)
    (%initialise-<keyword>)
    (%initialise-<pointer>)
    (%initialise-<char>)
    (%initialise-<transcoder>)

    (%initialise-<pair>)
    (%initialise-<string>)

    (%initialise-some-compound-object-types)
    (%initialise-numeric-object-types)
    (%initialise-input/output-port-object-types)
    (%initialise-predefined-condition-object-types)
    (void))


;;;; built-in tags module: helpers

(define (%basic name.sym parent.sym pred.sym)
  ;;Initialise a built-in tag with a basic "object-type-spec".
  ;;
  (let ((tag-id    (lex.core-prim-id name.sym))
	(parent-id (lex.core-prim-id parent.sym))
	(pred-id   (lex.core-prim-id pred.sym)))
    (set-identifier-object-type-spec! tag-id
      (make-object-type-spec tag-id parent-id pred-id))))


;;;; non-compound built-in tags: <symbol>

(define (%initialise-<symbol>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <symbol>))))

    (define-constant THE-TAG
      (S <symbol>))

    (define %constructor-maker	#f)

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((string)		(S symbol->string))
	((hash)		(S symbol-hash))
	((bound?)		(S symbol-bound?))
	((value)		(S symbol-value))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define (%mutator-maker field.sym input-form.stx)
      (case field.sym
	((value)		(S set-symbol-value!))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define %getter-maker		#f)
    (define %setter-maker		#f)

    (define (%caster-maker source.tag input-form.stx)
      (if source.tag
	  (cond ((lex.~free-identifier=? source.tag (S <string>))		(S string->symbol))
		(else
		 (syntax-violation __who__ "invalid cast source object type" input-form.stx source.tag)))
	(S any->symbol)))

    (define (%dispatcher method.sym input-form.stx)
      (case method.sym
	((putprop)	(M putprop))
	((getprop)	(M getprop))
	((remprop)	(M remprop))
	((property-list)	(M property-list))
	(else #f)))

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S symbol?)
			     %constructor-maker
			     %accessor-maker %mutator-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


;;;; non-compound built-in tags: <keyword>

(define (%initialise-<keyword>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <keyword>))))

    (define-constant THE-TAG
      (S <keyword>))

    (define %constructor-maker	#f)

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((symbol)		(S keyword->symbol))
	((string)		(S keyword->string))
	((hash)			(S keyword-hash))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define %mutator-maker	#f)
    (define %getter-maker		#f)
    (define %setter-maker		#f)
    (define %caster-maker		#f)
    (define %dispatcher		#f)

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S keyword?)
			     %constructor-maker
			     %mutator-maker %accessor-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


;;;; non-compound built-in tags: <pointer>

(define (%initialise-<pointer>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <pointer>))))

    (define-constant THE-TAG
      (S <pointer>))

    (define %constructor-maker	#f)

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((null?)			(S pointer-null?))
	((integer)		(S pointer->integer))
	((hash)			(S pointer-hash))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define (%mutator-maker field.sym input-form.stx)
      (case field.sym
	((set-null!)	(S set-pointer-null!))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define %getter-maker		#f)
    (define %setter-maker		#f)
    (define %caster-maker		#f)

    (define (%dispatcher method.sym input-form.stx)
      (case method.sym
	((=)		(M pointer=?))
	((!=)		(M pointer!=?))
	((<)		(M pointer<?))
	((>)		(M pointer>?))
	((<=)		(M pointer<=?))
	((>=)		(M pointer>=?))
	((add)		(M pointer-add))
	((diff)		(M pointer-diff))
	((clone)		(M pointer-clone))
	(else		#f)))

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S pointer?)
			     %constructor-maker
			     %accessor-maker %mutator-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


;;;; non-compound built-in tags: <char>

(define (%initialise-<char>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <char>))))

    (define-constant THE-TAG
      (S <char>))

    (define %constructor-maker	#f)

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((upcase)			(S char-upcase))
	((downcase)		(S char-downcase))
	((titlecase)		(S char-titlecase))
	((foldcase)		(S char-foldcase))

	((alphabetic?)		(S char-alphabetic?))
	((numeric?)		(S char-numeric?))
	((whitespace?)		(S char-whitespace?))
	((upper-case?)		(S char-upper-case?))
	((lower-case?)		(S char-lower-case?))
	((title-case?)		(S char-title-case?))

	((general-category)	(S char-general-category))

	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define %mutator-maker	#f)
    (define %getter-maker		#f)
    (define %setter-maker		#f)
    (define %caster-maker		#f)
    (define %dispatcher		#f)

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S char?)
			     %constructor-maker
			     %accessor-maker %mutator-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


;;;; non-compound built-in tags: <transcoder>

(define (%initialise-<transcoder>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <transcoder>))))

    (define-constant THE-TAG
      (S <transcoder>))

    (define %constructor-maker	#f)

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((codec)			(S transcoder-codec))
	((eol-style)		(S transcoder-eol-style))
	((error-handling-mode)	(S transcoder-error-handling-mode))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define %mutator-maker	#f)
    (define %getter-maker		#f)
    (define %setter-maker		#f)
    (define %caster-maker		#f)
    (define %dispatcher		#f)

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S transcoder?)
			     %constructor-maker
			     %accessor-maker %mutator-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


;;;; compound built-in tags: <pair>

(define (%initialise-<pair>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <pair>))))

    (define-constant THE-TAG
      (S <pair>))

    (define (%constructor-maker input-form.stx)
      (S cons))

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((car)		(S car))
	((cdr)		(S cdr))
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define (%mutator-maker field.sym input-form.stx)
      (case field.sym
	((car)		(S set-car!))
	((cdr)		(S set-cdr!) )
	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define (%getter-maker keys.stx input-form.stx)
      (define (%invalid-keys)
	(syntax-violation __who__ "invalid keys for getter" input-form.stx keys.stx))
      (syntax-match keys.stx ()
	(([?field])
	 (identifier? ?field)
	 (or (%accessor-maker (syntax->datum ?field) input-form.stx)
	     (%invalid-keys)))
	(else
	 (%invalid-keys))))

    (define (%setter-maker keys.stx input-form.stx)
      (define (%invalid-keys)
	(syntax-violation __who__ "invalid keys for setter" input-form.stx keys.stx))
      (syntax-match keys.stx ()
	(([?field])
	 (identifier? ?field)
	 (or (%mutator-maker (syntax->datum ?field) input-form.stx)
	     (%invalid-keys)))
	(else
	 (%invalid-keys))))

    (define %caster-maker		#f)
    (define %dispatcher		#f)

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S pair?)
			     %constructor-maker
			     %accessor-maker %mutator-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


;;;; compound built-in tags: <string>

(define (%initialise-<string>)
  (fluid-let-syntax ((__who__ (identifier-syntax (quote <string>))))

    (define-constant THE-TAG
      (S <string>))

    (define (%constructor-maker input-form.stx)
      (S string))

    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((length)			(S string-length))
	((upcase)			(S string-upcase))
	((downcase)		(S string-downcase))
	((titlecase)		(S string-titlecase))
	((foldcase)		(S string-foldcase))

	((ascii)			(S string->ascii))
	((latin1)			(S string->latin1))
	((utf8)			(S string->utf8))
	((utf16)			(S string->utf16))
	((utf16le)		(S string->utf16le))
	((utf16be)		(S string->utf16be))
	((utf16n)			(S string->utf16n))
	((utf32)			(S string->utf32))
	((percent-encoding)	(S string->uri-encoding))

	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define (%mutator-maker field.sym input-form.stx)
      (case field.sym

	(else
	 (syntax-violation __who__ "unknown field name" input-form.stx field.sym))))

    (define (%getter-maker keys.stx input-form.stx)
      (syntax-match keys.stx ()
	(([?char-index])
	 (lex.bless
	  `(lambda ({_ <char>} {str <string>})
	     (string-ref str (tag-assert-and-return (<fixnum>) ,?char-index)))))
	(else
	 (syntax-violation __who__ "invalid getter keys syntax" input-form.stx keys.stx))))

    (define (%setter-maker keys.stx input-form.stx)
      (syntax-match keys.stx ()
	(([?char-index])
	 (lex.bless
	  `(lambda ({_ <void>} {str <string>} {new-value <char>})
	     (string-set! str (tag-assert-and-return (<fixnum>) ,?char-index) new-value))))
	(else
	 (syntax-violation __who__ "invalid setter keys syntax" input-form.stx keys.stx))))

    (define (%caster-maker source-tag input-form.stx)
      (if source-tag
	  (cond ((lex.~free-identifier=? source-tag (S <symbol>))
		 (S symbol->string))
		((or (lex.~free-identifier=? source-tag (S <fixnum>))
		     (lex.~free-identifier=? source-tag (S <flonum>))
		     (lex.~free-identifier=? source-tag (S <ratnum>))
		     (lex.~free-identifier=? source-tag (S <bignum>))
		     (lex.~free-identifier=? source-tag (S <compnum>))
		     (lex.~free-identifier=? source-tag (S <cflonum>))
		     (lex.~free-identifier=? source-tag (S <exact-integer>))
		     (lex.~free-identifier=? source-tag (S <integer-valued>))
		     (lex.~free-identifier=? source-tag (S <integer>))
		     (lex.~free-identifier=? source-tag (S <rational-valued>))
		     (lex.~free-identifier=? source-tag (S <rational>))
		     (lex.~free-identifier=? source-tag (S <real-valued>))
		     (lex.~free-identifier=? source-tag (S <real>))
		     (lex.~free-identifier=? source-tag (S <complex>))
		     (lex.~free-identifier=? source-tag (S <number>)))
		 (S number->string))
		(else
		 (syntax-violation __who__ "invalid cast source object type" input-form.stx source-tag)))
	(S any->string)))

    (define (%dispatcher method.sym input-form.stx)
      (case method.sym
	((hash)			(M string-hash))
	((substring)		(M substring))
	((append)			(M string-append))
	((list)			(M string->list))
	((for-each)		(M string-for-each))
	((copy)			(M string-copy))
	((=)			(M string=?))
	(else			#f)))

    (define type-spec
      (make-object-type-spec THE-TAG (top-tag-id) (S string?)
			     %constructor-maker
			     %accessor-maker %mutator-maker
			     %getter-maker %setter-maker
			     %caster-maker %dispatcher))

    (set-identifier-object-type-spec! THE-TAG type-spec)))


(define (%initialise-some-compound-object-types)

  (set-identifier-object-type-spec! (S <vector>)
    (make-object-type-spec (S <vector>) (top-tag-id) (S vector?)))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <list>)
    ;;FIXME The truth  is that "<pair>" is  *not* a parent of  "<list>" because nulls
    ;;are "<list>" but not "<pair>".
    ;;
    ;;We temporarily set "<pair>" as parent nevertheless assuming that the primitives
    ;;acting  on lists-as-pairs  (like CAR  and CDR)  will recognise  null and  react
    ;;appropriately.  This is actually the case with CAR, CDR, SET-CAR! and SET-CDR!.
    ;;
    ;;In future  CAR, CDR and  similar primitives will  be multimethods which  can be
    ;;dispatched at expand-time for <pair> and <list> arguments.
    ;;
    ;;(Marco Maggi; Sat Apr 26, 2014)
    (make-object-type-spec (S <list>) (S <pair>) (S list?)))

  (set-identifier-object-type-spec! (S <bytevector>)
    (make-object-type-spec (S <bytevector>) (top-tag-id) (S bytevector?)))

  (set-identifier-object-type-spec! (S <hashtable>)
    (make-object-type-spec (S <hashtable>) (top-tag-id) (S hashtable?)))

  (set-identifier-object-type-spec! (S <struct>)
    (make-object-type-spec (S <struct>) (top-tag-id) (S struct?)))

  (set-identifier-object-type-spec! (S <struct-type-descriptor>)
    (make-object-type-spec (S <struct-type-descriptor>) (S <struct>) (S struct-type-descriptor?)))

  (set-identifier-object-type-spec! (S <record>)
    (make-object-type-spec (S <record>) (S <struct>) (S record?)))

  (set-identifier-object-type-spec! (S <record-type-descriptor>)
    (make-object-type-spec (S <record-type-descriptor>) (S <struct>) (S record-type-descriptor?)))

  (void))


(define (%initialise-numeric-object-types)
  (set-identifier-object-type-spec! (S <number>)
    (make-object-type-spec (S <number>) (top-tag-id) (S number?)))

  (set-identifier-object-type-spec! (S <complex>)
    (make-object-type-spec (S <complex>) (S <number>) (S complex?)))

  (set-identifier-object-type-spec! (S <real-valued>)
    (make-object-type-spec (S <real-valued>) (S <complex>) (S real-valued?)))

  (set-identifier-object-type-spec! (S <real>)
    (make-object-type-spec (S <real>) (S <real-valued>) (S real?)))

  (set-identifier-object-type-spec! (S <rational-valued>)
    (make-object-type-spec (S <rational-valued>) (S <real>) (S rational-valued?)))

  (set-identifier-object-type-spec! (S <rational>)
    (make-object-type-spec (S <rational>) (S <rational-valued>) (S rational?)))

  (set-identifier-object-type-spec! (S <integer-valued>)
    (make-object-type-spec (S <integer-valued>) (S <rational-valued>) (S integer-valued?)))

  (set-identifier-object-type-spec! (S <integer>)
    (make-object-type-spec (S <integer>) (S <rational-valued>) (S integer?)))

  (set-identifier-object-type-spec! (S <exact-integer>)
    (make-object-type-spec (S <exact-integer>) (S <integer>) (S exact-integer?)))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((even?)		(S fxeven?))
	((odd?)			(S fxodd?))
	((negative?)		(S fxnegative?))
	((positive?)		(S fxpositive?)	)
	((non-negative?)	(S fxnonnegative?))
	((non-positive?)	(S fxnonpositive?))
	((zero?)		(S fxzero?))
	((sign)			(S fxsign))
	(else
	 (syntax-violation '<fixnum> "unknown field name" input-form.stx field.sym))))

    (define (%dispatcher method.sym input-form.stx)
      #f)

    (define type-spec
      (make-object-type-spec (S <fixnum>) (S <exact-integer>) (S fixnum?)
			     #f %accessor-maker #f  #f #f
			     #f %dispatcher))

    (set-identifier-object-type-spec! (S <fixnum>) type-spec))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <bignum>)
    (make-object-type-spec (S <bignum>) (S <exact-integer>) (S bignum?)))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((integer?)			(S flinteger?))
	((finite?)			(S flfinite?))
	((infinite?)			(S flinfinite?))
	((nan?)				(S flnan?))
	((negative?)			(S flnegative?))
	((positive?)			(S flpositive?))
	((nonnegative?)			(S flnonnegative?))
	((nonpositive?)			(S flnonpositive?))
	((zero?)			(S flzero?))
	((zero?/positive)		(S flzero?/positive))
	((zero?/negative)		(S flzero?/negative))
	((even?)			(S fleven?))
	((odd?)				(S flodd?))
	(else
	 (syntax-violation '<flonum> "unknown field name" input-form.stx field.sym))))

    (define (%dispatcher method.sym input-form.stx)
      (case method.sym
	((string)				(M flonum->string))
	;; methods: arithmetic functions
	((abs)					(M flabs))
	((*)					(M fl*))
	((+)					(M fl+))
	((-)					(M fl-))
	((/)					(M fl/))
	((div)					(M fldiv))
	((mod)					(M flmod))
	((div-and-mod)				(M fldiv-and-mod))
	((div0)					(M fldiv0))
	((mod0)					(M flmod0))
	((div0-and-mod0)			(M fldiv0-and-mod0))
	;; methods: power functions
	((expt)					(M flexpt))
	((square)				(M flsquare))
	((cube)					(M flcube))
	((sqrt)					(M flsqrt))
	((cbrt)					(M flcbrt))
	;; methods: comparison functions
	((=)					(M fl=?))
	((<)					(M fl<?))
	((>)					(M fl>?))
	((<=)					(M fl<=?))
	((>=)					(M fl>=?))
	;; methods: trigonometric functions
	((sin)					(M flsin))
	((cos)					(M flcos))
	((tan)					(M fltan))
	((acos)					(M flacos))
	((asin)					(M flasin))
	((atan)					(M flatan))
	;; methods: hyperbolic functions
	((sinh)					(M flsinh))
	((cosh)					(M flcosh))
	((tanh)					(M fltanh))
	((acosh)				(M flacosh))
	((asinh)				(M flasinh))
	((atanh)				(M flatanh))
	;; methods: rounding functions
	((ceiling)				(M flceiling))
	((floor)				(M flfloor))
	((round)				(M flround))
	((truncate)				(M fltruncate))
	;; methods: rationals operations
	((numerator)				(M flnumerator))
	((denominator)				(M fldenominator))
	;; methods: exponentiation and logarithms
	((exp)					(M flexp))
	((log)					(M fllog))
	((log1p)				(M fllog1p))
	((expm1)				(M flexpm1))
	((hypot)				(M flhypot))
	;; methods: min and max
	((max)					(M flmax))
	((min)					(M flmin))
	(else #f)))

    (define type-spec
      (make-object-type-spec (S <flonum>) (S <real>) (S flonum?)
			     #f %accessor-maker #f  #f #f
			     #f %dispatcher))

    (set-identifier-object-type-spec! (S <flonum>) type-spec))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((numerator)		(S numerator))
	((denominator)		(S denominator))
	(else
	 (syntax-violation '<ratnum> "unknown field name" input-form.stx field.sym))))

    (define (%dispatcher method.sym input-form.stx)
      #f)

    (define type-spec
      (make-object-type-spec (S <ratnum>) (S <rational>) (S ratnum?)
			     #f %accessor-maker #f  #f #f
			     #f %dispatcher))

    (set-identifier-object-type-spec! (S <ratnum>) type-spec))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <compnum>)
    (make-object-type-spec (S <compnum>) (S <complex>) (S compnum?)))

  (set-identifier-object-type-spec! (S <cflonum>)
    (make-object-type-spec (S <cflonum>) (S <complex>) (S cflonum?)))

  (void))


(define (%initialise-input/output-port-object-types)
  (set-identifier-object-type-spec! (S <port>)
    (make-object-type-spec (S <port>) (top-tag-id) (S port?)))

  (set-identifier-object-type-spec! (S <input-port>)
    (make-object-type-spec (S <input-port>) (S <port>) (S input-port?)))

  (set-identifier-object-type-spec! (S <output-port>)
    (make-object-type-spec (S <output-port>) (S <port>) (S output-port?)))

  (set-identifier-object-type-spec! (S <input/output-port>)
    (make-object-type-spec (S <input/output-port>) (S <port>) (S input/output-port?)))

  (set-identifier-object-type-spec! (S <textual-port>)
    (make-object-type-spec (S <textual-port>) (S <port>) (S textual-port?)))

  (set-identifier-object-type-spec! (S <binary-port>)
    (make-object-type-spec (S <binary-port>) (S <port>) (S binary-port?)))

  (set-identifier-object-type-spec! (S <textual-input-port>)
    (make-object-type-spec (S <textual-input-port>) (S <input-port>) (S textual-input-port?)))

  (set-identifier-object-type-spec! (S <textual-output-port>)
    (make-object-type-spec (S <textual-output-port>) (S <output-port>) (S textual-output-port?)))

  (set-identifier-object-type-spec! (S <textual-input/output-port>)
    (make-object-type-spec (S <textual-input/output-port>) (S <input/output-port>) (S textual-input/output-port?)))

  (set-identifier-object-type-spec! (S <binary-input-port>)
    (make-object-type-spec (S <binary-input-port>) (S <input-port>) (S binary-input-port?)))

  (set-identifier-object-type-spec! (S <binary-output-port>)
    (make-object-type-spec (S <binary-output-port>) (S <output-port>) (S binary-output-port?)))

  (set-identifier-object-type-spec! (S <binary-input/output-port>)
    (make-object-type-spec (S <binary-input/output-port>) (S <input/output-port>) (S binary-input/output-port?)))

  (void))


;;;; predefined condition object types

(define (%initialise-predefined-condition-object-types)
  (%basic '&condition			'<record>			'condition?)
  (%basic '&message			'&condition			'message-condition?)
  (%basic '&warning			'&condition			'warning?)
  (%basic '&serious			'&condition			'serious-condition?)
  (%basic '&error			'&serious			'error?)
  (%basic '&violation			'&serious			'violation?)
  (%basic '&assertion			'&violation			'assertion-violation?)
  (%basic '&irritants			'&condition			'irritants-condition?)
  (%basic '&who				'&condition			'who-condition?)
  (%basic '&non-continuable		'&violation			'non-continuable-violation?)
  (%basic '&implementation-restriction	'&violation			'implementation-restriction-violation?)
  (%basic '&lexical			'&violation			'lexical-violation?)
  (%basic '&syntax			'&violation			'syntax-violation?)
  (%basic '&undefined			'&violation			'undefined-violation?)
  (%basic '&i/o				'&error				'i/o-error?)
  (%basic '&i/o-read			'&i/o				'i/o-read-error?)
  (%basic '&i/o-write			'&i/o				'i/o-write-error?)
  (%basic '&i/o-invalid-position	'&i/o				'i/o-invalid-position-error?)
  (%basic '&i/o-filename		'&i/o				'i/o-filename-error?)
  (%basic '&i/o-file-protection		'&i/o-filename			'i/o-file-protection-error?)
  (%basic '&i/o-file-is-read-only	'&i/o-file-protection		'i/o-file-is-read-only-error?)
  (%basic '&i/o-file-already-exists 	'&i/o-filename			'i/o-file-already-exists-error?)
  (%basic '&i/o-file-does-not-exist	'&i/o-filename			'i/o-file-does-not-exist-error?)
  (%basic '&i/o-port			'&i/o				'i/o-port-error?)
  (%basic '&i/o-decoding		'&i/o-port			'i/o-decoding-error?)
  (%basic '&i/o-encoding		'&i/o-port			'i/o-encoding-error?)
  (%basic '&no-infinities		'&implementation-restriction	'no-infinities-violation?)
  (%basic '&no-nans			'&implementation-restriction	'no-nans-violation?)
  (%basic '&interrupted			'&serious			'interrupted-condition?)
  (%basic '&source-position		'&condition			'source-position-condition?)

  (%basic '&i/o-eagain			'&i/o				'i/o-eagain-error?)
  (%basic '&errno			'&condition			'errno-condition?)
  (%basic '&h_errno			'&condition			'h_errno-condition?)
  (%basic '&out-of-memory-error		'&error				'out-of-memory-error?)

  (%basic '&procedure-argument-violation	'&assertion		'procedure-argument-violation?)
  (%basic '&expression-return-value-violation	'&assertion		'expression-return-value-violation?)

  (void))


;;;; built-in tags module: done

#| end of module |# )


;;;; helpers and utilities

(let-syntax
    ((define-tag-retriever (syntax-rules ()
			     ((_ ?who ?tag)
			      (define ?who
				(let ((memoized-id #f))
				  (lambda ()
				    (or memoized-id
					(receive-and-return (id)
					    (lex.core-prim-id '?tag)
					  (set! memoized-id id))))))))))
  (define-tag-retriever top-tag-id		<top>)
  (define-tag-retriever void-tag-id		<void>)
  (define-tag-retriever procedure-tag-id	<procedure>)
  (define-tag-retriever predicate-tag-id	<predicate>)
  (define-tag-retriever list-tag-id		<list>)
  (define-tag-retriever boolean-tag-id		<boolean>)
  (define-tag-retriever struct-tag-id		<struct>)
  (define-tag-retriever record-tag-id		<record>)
  #| end of let-syntax |# )

;;; --------------------------------------------------------------------

(define ($procedure-tag-id? id)
  (lex.~free-identifier=? id (procedure-tag-id)))

(define ($list-tag-id? id)
  (lex.~free-identifier=? id (list-tag-id)))

(define ($top-tag-id? id)
  (lex.~free-identifier=? id (top-tag-id)))

;;; --------------------------------------------------------------------

(define (procedure-tag-id? id)
  (and (identifier? id)
       ($procedure-tag-id? id)))

(define (list-tag-id? id)
  (and (identifier? id)
       ($list-tag-id? id)))

(define (top-tag-id? id)
  (and (identifier? id)
       ($top-tag-id? id)))

;;; --------------------------------------------------------------------

(define (retvals-signature-of-datum datum)
  (make-retvals-signature
   (list (cond ((boolean? datum)	(S <boolean>))
	       ((char?    datum)	(S <char>))
	       ((symbol?  datum)	(S <symbol>))
	       ((keyword? datum)	(S <keyword>))

	       ((fixnum?  datum)	(S <fixnum>))
	       ((flonum?  datum)	(S <flonum>))
	       ((ratnum?  datum)	(S <ratnum>))
	       ((bignum?  datum)	(S <bignum>))
	       ((compnum? datum)	(S <compnum>))
	       ((cflonum? datum)	(S <cflonum>))
	       ((real?    datum)	(S <real>))
	       ((complex? datum)	(S <complex>))
	       ((number?  datum)	(S <number>))

	       ((string?  datum)	(S <string>))
	       ((vector?  datum)	(S <vector>))
	       ((list?    datum)	(S <list>))
	       ((pair?    datum)	(S <pair>))
	       ((bytevector? datum)	(S <bytevector>))

	       ((eq? datum (void))	(S <void>))
	       (else			(top-tag-id))))))


;;;; formals, retvals, signature syntaxes predicates

(define (standard-formals-syntax? stx)
  ;;Return  true if  STX is  a syntax  object representing  R6RS standard  LAMBDA and
  ;;LET-VALUES formals; otherwise return false.  The return value is true if STX is a
  ;;proper or  improper list of  identifiers, with  null and a  standalone identifier
  ;;being acceptable.  Examples:
  ;;
  ;;   (standard-formals-syntax #'args)		=> #t
  ;;   (standard-formals-syntax #'())		=> #t
  ;;   (standard-formals-syntax #'(a b))	=> #t
  ;;   (standard-formals-syntax #'(a b . rest))	=> #t
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (identifier? ?id)
     (standard-formals-syntax? ?rest))
    (?rest
     (identifier? ?rest))
    ))

(define (formals-signature-syntax? stx)
  ;;Return true if STX is a syntax  object representing the tag signature of a tagged
  ;;formals syntax;  otherwise return false.   The return value is  true if STX  is a
  ;;proper  or improper  list of  tag  identifiers, with  null and  a standalone  tag
  ;;identifier being  acceptable; when the  signature is  an improper list:  the tail
  ;;standalone identifier must be "<list>" or a sub-tag of "<list>".  Examples:
  ;;
  ;;   (formals-signature-syntax #'<list>)				=> #t
  ;;   (formals-signature-syntax #'())					=> #t
  ;;   (formals-signature-syntax #'(<fixnum> <string>))			=> #t
  ;;   (formals-signature-syntax #'(<fixnum> <string> . <list>))	=> #t
  ;;
  ;;A standalone "<list>" identifier means: any number of values of any type.
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (tag-identifier? ?id)
     (formals-signature-syntax? ?rest))
    (?rest
     (tag-identifier-and-list-sub-tag? ?rest))
    ))

(define (retvals-signature-syntax? stx)
  ;;Return true if  STX is a syntax  object representing the tag  signature of tagged
  ;;return values; otherwise return  false.  The return value is true  if STX is null
  ;;or a proper or improper list of tag identifiers, with a standalone tag identifier
  ;;being acceptable;  when the signature  is an  improper list: the  tail standalone
  ;;identifier must be "<list>" or a sub-tag of "<list>".  Examples:
  ;;
  ;;   (retvals-signature-syntax #'<list>)				=> #t
  ;;   (retvals-signature-syntax #'())					=> #t
  ;;   (retvals-signature-syntax #'(<fixnum> <string>))			=> #t
  ;;   (retvals-signature-syntax #'(<fixnum> <string> . <list>))	=> #t
  ;;
  ;;A standalone "<list>" identifier means: any number of values of any type.
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (tag-identifier? ?id)
     (retvals-signature-syntax? ?rest))
    (?rest
     (tag-identifier-and-list-sub-tag? ?rest))
    ))

;;; --------------------------------------------------------------------

(define* (formals-signature-partially-untagged-syntax? {stx formals-signature-syntax?})
  ;;The argument STX must be a  syntax object representing a formals signature syntax
  ;;according  to FORMALS-SIGNATURE-SYNTAX?.   Return true  if STX  has at  least one
  ;;"<top>" tag identifier; otherwise return false.
  ;;
  ($formals-signature-partially-untagged-syntax? stx))

(define ($formals-signature-partially-untagged-syntax? stx)
  (syntax-match stx ()
    (() #f)
    ((?tag . ?rest-tags)
     (or (top-tag-id? ?tag)
	 ($formals-signature-partially-untagged-syntax? ?rest-tags)))
    (?rest
     #t)))

(define* (retvals-signature-partially-unspecified-syntax? {stx retvals-signature-syntax?})
  ;;The argument STX must be a  syntax object representing a retvals signature syntax
  ;;according to RETVALS-SIGNATURE-SYNTAX?.  Return true  if STX if the signature has
  ;;at least one "<top>" tag identifier; otherwise return false.
  ;;
  ($retvals-signature-partially-unspecified-syntax? stx))

(define $retvals-signature-partially-unspecified-syntax?
  $formals-signature-partially-untagged-syntax?)

;;; --------------------------------------------------------------------

(define* (formals-signature-super-and-sub-syntax? {super-signature formals-signature-syntax?}
						  {sub-signature   formals-signature-syntax?})
  ;;Return  true if  the super  signature syntax  and the  sub signature  syntax have
  ;;compatible structure and  the tags from the super signature  are supertags of the
  ;;tags from the sub signature; otherwise return false.
  ;;
  ;;This function can be used to determine:  if a tuple of arguments matches a lambda
  ;;formals's signature; if a tuple or return values matches the receiver signature.
  ;;
  ($formals-signature-super-and-sub-syntax? super-signature sub-signature))

(define ($formals-signature-super-and-sub-syntax? super-signature sub-signature)
  (syntax-match super-signature ()
    (()
     (syntax-match sub-signature ()
       ;;Both the signatures are proper lists with  the same number of items, and all
       ;;the items are correct super and sub: success!
       (() #t)
       ;;The signatures do not match.
       (_  #f)))

    ((?super-tag . ?super-rest-tags)
     (syntax-match sub-signature ()
       ((?sub-tag . ?sub-rest-tags)
	(and (or (top-tag-id? ?super-tag)
		 (tag-super-and-sub? ?super-tag ?sub-tag))
	     ($formals-signature-super-and-sub-syntax? ?super-rest-tags ?sub-rest-tags)))
       (_ #f)))

    (?super-rest-tag
     (syntax-match sub-signature ()
       ;;The super signature is an improper list with rest item and the sub signature
       ;;is finished.  We want the following signature to match:
       ;;
       ;;  #'(<number> <fixnum> . <list>)     #'(<complex> <fixnum>)
       ;;
       ;;because "<list>" in rest position means any number of objects of any type.
       (() #t)

       ;;The super signature is an improper  list shorter than the sub signature.  We
       ;;want the following signature to match:
       ;;
       ;;  #'(<number> . <list>)  #'(<complex> <fixnum> <fixnum>)
       ;;
       ;;because "<list>" in  rest position means any number of  objects of any type.
       ;;If ?SUPER-REST-TAG is a sub-tag of "<list>": we do not want it to match.
       ((?sub-tag . ?sub-rest-tags)
	(list-tag-id?      ?super-rest-tag))

       ;;Both the  signatures are improper lists  with the same number  of items, and
       ;;all the items are  correct super and sub; if the rest  tags are proper super
       ;;and subs: success!
       (?sub-rest-tag
	(tag-super-and-sub? ?super-rest-tag ?sub-rest-tag))
       ))
    ))

(define* (retvals-signature-super-and-sub-syntax? {super-signature retvals-signature-syntax?}
						  {sub-signature   retvals-signature-syntax?})
  ;;Return  true  if the  super  signature  and  the  sub signature  have  compatible
  ;;structure and the  tags from the super  signature are supertags of  the tags from
  ;;the sub signature; otherwise return false.
  ;;
  ;;This function can be used to determine:  if a tuple of arguments matches a lambda
  ;;retvals's signaure; if a tuple or return values matches the receiver signature.
  ;;
  ($retvals-signature-super-and-sub-syntax? super-signature sub-signature))

(define $retvals-signature-super-and-sub-syntax?
  $formals-signature-super-and-sub-syntax?)

;;; --------------------------------------------------------------------

(module (retvals-signature-syntax-common-ancestor)

  (define* (retvals-signature-syntax-common-ancestor {sig1 retvals-signature-syntax?}
						     {sig2 retvals-signature-syntax?})
    ($retvals-signature-syntax-common-ancestor sig1 sig2))

  (define ($retvals-signature-syntax-common-ancestor sig1 sig2)
    ;;
    ;;We assume that  both SIG1 and SIG2 are retvals  signature syntaxes according to
    ;;RETVALS-SIGNATURE-SYNTAX?.
    ;;
    (if (or (list-tag-id? sig1)
	    (list-tag-id? sig2))
	(list-tag-id)
      (syntax-match sig1 ()
	(()
	 (syntax-match sig2 ()
	   (() '())
	   ((?tag2 . ?rest-tags2)
	    (list-tag-id))
	   (_
	    (list-tag-id))))

	((?tag1 . ?rest-tags1)
	 (syntax-match sig2 ()
	   (()
	    (list-tag-id))
	   ((?tag2 . ?rest-tags2)
	    (cons (tag-common-ancestor ?tag1 ?tag2)
		  ($retvals-signature-syntax-common-ancestor ?rest-tags1 ?rest-tags2)))
	   (?rest-tag2
	    (list-tag-id))))

	(?rest-tag1
	 (syntax-match sig2 ()
	   (()
	    (list-tag-id))
	   ((?tag2 . ?rest-tags2)
	    (list-tag-id))
	   (?rest-tag2
	    (tag-common-ancestor ?rest-tag1 ?rest-tag2))))
	)))

  #| end of module |# )


;;;; callable syntax, callable signature, return values signature, formals signature

(define-record (retvals-signature %make-retvals-signature retvals-signature?)
  (tags
		;A syntax object representing a retvals signature syntax according to
		;RETVALS-SIGNATURE-SYNTAX?.
   ))

(define-record (formals-signature %make-formals-signature formals-signature?)
  (tags
		;A syntax object representing a formals signature syntax according to
		;FORMALS-SIGNATURE-SYNTAX?.
   ))

(define-record (lambda-signature %make-lambda-signature lambda-signature?)
  (retvals
		;An instance of "retvals-signature".
   formals
		;An instance of "formals-signature".
   ))

(define-record (clambda-compound %make-clambda-compound clambda-compound?)
  (common-retvals-signature
		;An instance of "retvals-signature".   It represents the signature of
		;the common retvals from all  the clambda clauses represented by this
		;struct.  For example:
		;
		;   (case-lambda
		;    (({_ <fixnum>}) . ?body)
		;     ({_ <fixnum>}) . ?body)))
		;
		;has common retvals "(<fixnum>)", while:
		;
		;   (case-lambda
		;    (({_ <fixnum>}) . ?body)
		;     ({_ <bignum>}) . ?body)))
		;
		;has common retvals "(<exact-integer>)".  When  it is not possible to
		;determine a common retvals signature: the default value is "<list>",
		;which means any number of objects of any type.
		;
   lambda-signatures
		;A  proper  list  of "lambda-signature"  instances  representing  the
		;signatures of the CASE-LAMBDA clauses.
   ))

(define* (make-formals-signature {tags formals-signature-syntax?})
  (%make-formals-signature (syntax-unwrap tags)))

(define* (make-retvals-signature {tags retvals-signature-syntax?})
  (%make-retvals-signature (syntax-unwrap tags)))

(define* (make-lambda-signature {retvals retvals-signature?} {formals formals-signature?})
  (%make-lambda-signature retvals formals))

(define* (make-clambda-compound {signatures list-of-lambda-signatures?})
  (%make-clambda-compound (if (null? signatures) ;this is REDUCE
			      (make-retvals-signature-standalone-list)
			    (fold-left retvals-signature-common-ancestor
			      (lambda-signature-retvals ($car signatures))
			      (map lambda-signature-retvals ($cdr signatures))))
			  signatures))

;;; --------------------------------------------------------------------
;;; special constructors

(let-syntax
    ((define-single-tag-retvals-maker (syntax-rules ()
					((_ ?who ?tag-maker)
					 (define ?who
					   (let ((sign #f))
					     (lambda ()
					       (or sign
						   (receive-and-return (S)
						       (make-retvals-signature-single-value (?tag-maker))
						     (set! sign S))))))))))
  (define-single-tag-retvals-maker make-retvals-signature-single-top		top-tag-id)
  (define-single-tag-retvals-maker make-retvals-signature-single-procedure	procedure-tag-id)
  #| end of let-syntax |# )

(define make-retvals-signature-standalone-list
  (let ((sign #f))
    (lambda ()
      (or sign
	  (receive-and-return (S)
	      (make-retvals-signature (list-tag-id))
	    (set! sign S))))))

(define-syntax-rule (make-retvals-signature-fully-unspecified)
  (make-retvals-signature-standalone-list))

(define* (make-retvals-signature-single-value {tag tag-identifier?})
  (make-retvals-signature (list tag)))


;;;; lambda-signature stuff

(define* (lambda-signature=? {signature1 lambda-signature?} {signature2 lambda-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (and (syntax=? ($formals-signature-tags ($lambda-signature-formals signature1))
		 ($formals-signature-tags ($lambda-signature-formals signature2)))
       (syntax=? ($retvals-signature-tags ($lambda-signature-retvals signature1))
		 ($retvals-signature-tags ($lambda-signature-retvals signature2)))))

;;; --------------------------------------------------------------------

(define* (lambda-signature-formals-tags {signature lambda-signature?})
  ($formals-signature-tags ($lambda-signature-formals signature)))

(define* (lambda-signature-retvals-tags {signature lambda-signature?})
  ($retvals-signature-tags ($lambda-signature-retvals signature)))

;;; --------------------------------------------------------------------

(define (list-of-lambda-signatures? obj)
  (and (list? obj)
       (for-all lambda-signature? obj)))

(define* (lambda-signature-fully-unspecified? {signature lambda-signature?})
  ;;A LAMBDA  signature has fully unspecified  types if its retvals  tag signature is
  ;;the standalone "<list>" tag and its formals signature is a proper list of "<top>"
  ;;tags:
  ;;
  ;;   (<top> ...)
  ;;
  ;;or an improper list like:
  ;;
  ;;   (<top> ... . <list>)
  ;;
  (and (formals-signature-fully-unspecified? ($lambda-signature-formals signature))
       (retvals-signature-fully-unspecified? ($lambda-signature-retvals signature))))


;;;; formals-signature stuff

(define* (formals-signature=? {signature1 formals-signature?} {signature2 formals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? ($formals-signature-tags signature1)
	    ($formals-signature-tags signature2)))

;;; --------------------------------------------------------------------

(define* (formals-signature-super-and-sub? {super-signature formals-signature?}
					   {sub-signature   formals-signature?})
  ($formals-signature-super-and-sub-syntax? ($formals-signature-tags super-signature)
					    ($formals-signature-tags sub-signature)))

(define* (formals-signature-fully-unspecified? {signature formals-signature?})
  ;;Return  true  if the  formals  signature  does  not  specify any  argument  type,
  ;;otherwise  return false;  in other  words, return  true if  the signature  is one
  ;;among:
  ;;
  ;;   (<top> ...)
  ;;   (<top> ... . <list>)
  ;;
  (let ((formals.stx ($formals-signature-tags signature)))
    (if (list? formals.stx)
	(for-all top-tag-id? formals.stx)
      (receive (head tail)
	  (improper-list->list-and-rest formals.stx)
	(and (for-all top-tag-id? head)
	     (list-tag-id? tail))))))


;;;; retvals-signature stuff

(define* (retvals-signature-fully-unspecified? {signature retvals-signature?})
  (list-tag-id? ($retvals-signature-tags signature)))

(define* (retvals-signature-partially-unspecified? {signature retvals-signature?})
  ($retvals-signature-partially-unspecified-syntax? ($retvals-signature-tags signature)))

(define* (retvals-signature-super-and-sub? {super-signature retvals-signature?}
					   {sub-signature   retvals-signature?})
  ($retvals-signature-super-and-sub-syntax? ($retvals-signature-tags super-signature)
					    ($retvals-signature-tags sub-signature)))

(define* (retvals-signature-single-tag? {signature retvals-signature?})
  ;;Return  true if  SIGNATURE represents  a  single return  value, otherwise  return
  ;;false.  We have to remember that a signature syntax can be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (syntax-match ($retvals-signature-tags signature) ()
    ((?tag)	#t)
    (_		#f)))

(define* (retvals-signature-single-top-tag? {signature retvals-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.   We have to remember that, after  parsing syntax objects
  ;;with SYNTAX-MATCH, a signature syntax can result to be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (syntax-match ($retvals-signature-tags signature) ()
    ((?tag)
     (top-tag-id? ?tag))
    (_ #f)))

(define* (retvals-signature-single-tag-or-fully-unspecified? {signature retvals-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier,  otherwise return  false.  We have  to remember  that, after
  ;;parsing syntax objects with SYNTAX-MATCH, a signature syntax can result to be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (let ((tags ($retvals-signature-tags signature)))
    (or (list-tag-id? tags)
	(syntax-match tags ()
	  ((?tag)	#t)
	  (_		#f)))))

;;; --------------------------------------------------------------------

(define* (retvals-signature=? {signature1 retvals-signature?} {signature2 retvals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? ($retvals-signature-tags signature1)
	    ($retvals-signature-tags signature2)))

;;; --------------------------------------------------------------------

(define* (retvals-signature-common-ancestor {sig1 retvals-signature?}
					    {sig2 retvals-signature?})
  (make-retvals-signature
   (retvals-signature-syntax-common-ancestor ($retvals-signature-tags sig1)
					     ($retvals-signature-tags sig2))))


;;;; tagged binding parsing: standalone identifiers

(define (tagged-identifier-syntax? stx)
  ;;Return  true  if  STX is  a  syntax  object  representing  a tagged  or  untagged
  ;;identifier, otherwise return false.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (tag-identifier? ?tag)))
    (?id
     (identifier? ?id))))

(define (parse-tagged-identifier-syntax stx)
  ;;If  STX is  a tagged  or  untagged identifier,  return 2  values: the  identifier
  ;;representing the binding name and  the identifier representing the tag; otherwise
  ;;raise  an exception.   When no  tag is  present: the  tag identifier  defaults to
  ;;"<top>".
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (begin
       (assert-tag-identifier? ?tag)
       (values ?id ?tag)))
    (?id
     (identifier? ?id)
     (values ?id (top-tag-id)))))


;;;; tagged binding parsing: proper lists of bindings left-hand sides
;;
;;The predicate and parser functions for lists of bindings are used to parse bindings
;;from LET, DO and similar syntaxes.  For example, when expanding the syntax:
;;
;;   (let (({a <fixnum>} 1)
;;         ({b <string>} "b")
;;         (c            #t))
;;     . ?body)
;;
;;the argument STX is:
;;
;;   (#'(brace a <fixnum>) #'(brace b <string>) #'c)
;;
;;and the return values are:
;;
;;   (#'a #'b #'c) (#'<fixnum> #'<string> #'<top>)
;;

(case-define* parse-list-of-tagged-bindings
  ((stx)
   (parse-list-of-tagged-bindings stx #f))
  ((stx input-form.stx)
   ;;Assume STX  is a  syntax object  representing a proper  list of  possibly tagged
   ;;binding identifiers; parse  the list and return 2 values:  a list of identifiers
   ;;representing the  binding identifiers,  a list  of identifiers  representing the
   ;;type tags;  "<top>" is  used when no  tag is present.   The identifiers  must be
   ;;distinct.
   ;;
   (define (%invalid-tagged-bindings-syntax form subform)
     (syntax-violation __who__ "invalid tagged bindings syntax" form subform))
   (define (%duplicate-identifiers-in-bindings-specification form subform)
     (syntax-violation __who__
       "duplicate identifiers in bindings specification" form subform))
   (receive-and-return (id* tag*)
       (let recur ((bind* stx))
	 (syntax-match bind* (brace)
	   (()
	    (values '() '()))
	   (((brace ?id ?tag) . ?other-id*)
	    (begin
	      (assert-tag-identifier? ?tag)
	      (receive (id* tag*)
		  (recur ?other-id*)
		(values (cons ?id id*) (cons ?tag tag*)))))
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (receive (id* tag*)
		(recur ?other-id*)
	      (values (cons ?id id*) (cons (top-tag-id) tag*))))
	   (_
	    (if input-form.stx
		(%invalid-tagged-bindings-syntax input-form.stx stx)
	      (%invalid-tagged-bindings-syntax stx #f)))
	   ))
     (unless (lex.distinct-bound-ids? id*)
       (if input-form.stx
	   (%duplicate-identifiers-in-bindings-specification input-form.stx stx)
	 (%duplicate-identifiers-in-bindings-specification stx #f))))))

(define* (list-of-tagged-bindings? lhs*)
  ;;Return true  if LHS* is a  list of possibly tagged  identifiers; otherwise return
  ;;false.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (id* tag*)
	(parse-list-of-tagged-bindings lhs*)
      #t)))


;;;; tagged binding parsing: let-values formals

(module (parse-tagged-formals-syntax)
  ;;Given a syntax object representing  tagged LET-VALUES formals: split formals from
  ;;tags.  Do test for duplicate bindings.
  ;;
  ;;Return 2 values:
  ;;
  ;;1..A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2..An object representing the LET-VALUES tagging signature.
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'parse-tagged-formals-syntax))

  (case-define* parse-tagged-formals-syntax
    (({_ standard-formals-syntax? formals-signature?} original-formals.stx)
     (parse-tagged-formals-syntax original-formals.stx #f))
    (({_ standard-formals-syntax? formals-signature?} original-formals.stx input-form.stx)
     (receive (standard-formals formals-tags)
	 (%parse-formals input-form.stx original-formals.stx original-formals.stx)
       (values standard-formals (make-formals-signature formals-tags)))))

  (define (%parse-formals input-form.stx original-formals.stx formals.stx)
    (syntax-match formals.stx (brace)

      ;;Tagged args, as in:
      ;;
      ;;   (let-values (({args list-of-fixnums} ?expr)) . ?body)
      ;;
      ;;only a sub-tag of "<list>" is acceptable here.
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (begin
	 (assert-list-sub-tag-identifier? ?args-tag)
	 (values ?args-id ?args-tag)))

      ;;Possibly tagged identifiers with tagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . {rest list-of-fixnums}) ?expr)) . ?body)
      ;;
      ;;only a sub-tag of "<list>" is accepted in rest position.
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (begin
	 (unless (and (identifier? ?rest-id)
		      (identifier? ?rest-tag)
		      (tag-super-and-sub? (list-tag-id) ?rest-tag))
	   (syntax-violation __module_who__
	     "invalid rest argument specification" original-formals.stx (list 'brace ?rest-id ?rest-tag)))
	 (assert-tag-identifier? ?rest-tag)
	 (receive-and-return (standard-formals.stx tags)
	     (let recur ((?arg* ?arg*))
	       (if (pair? ?arg*)
		   (%process-args input-form.stx original-formals.stx recur ?arg*)
		 ;;Process rest argument.
		 (values ?rest-id ?rest-tag)))
	   (%validate-formals input-form.stx original-formals.stx standard-formals.stx))))

      ;;Possibly tagged identifiers with UNtagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . rest) ?expr)) . ?body)
      ;;
      ((?arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (receive-and-return (standard-formals.stx tags)
	   (let recur ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-args input-form.stx original-formals.stx recur ?arg*)
	       (values ?rest-id (list-tag-id))))
	 (%validate-formals input-form.stx original-formals.stx standard-formals.stx)))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (begin
	 (%validate-formals input-form.stx original-formals.stx ?id*)
	 (values ?id* (map (lambda (id) (top-tag-id)) ?id*))))

      ;;Standard formals: untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-formals input-form.stx original-formals.stx (append ?id* ?rest-id))
	 (values formals.stx (cons* (map (lambda (id) (top-tag-id)) ?id*) (list-tag-id)))))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (list-tag-id)))

      ;;Possibly tagged identifiers without rest argument.
      ;;
      ((?arg* ...)
       (receive-and-return (standard-formals.stx tags)
	   (let recur ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-args input-form.stx original-formals.stx recur ?arg*)
	       (values '() '())))
	 (%validate-formals input-form.stx original-formals.stx standard-formals.stx)))
      ))

  (define (%process-args input-form.stx original-formals.stx recur args-stx)
    (receive (standard-formals tags)
	(recur (cdr args-stx))
      (let ((arg-stx (car args-stx)))
	(syntax-match arg-stx (brace)
	  ;;Untagged argument.
	  (?id
	   (identifier? ?id)
	   (values (cons ?id standard-formals) (cons (top-tag-id) tags)))
	  ;;Tagged argument.
	  ((brace ?id ?tag)
	   (and (identifier? ?id)
		(identifier? ?tag))
	   (begin
	     (assert-tag-identifier? ?tag)
	     (values (cons ?id standard-formals) (cons ?tag tags))))
	  (else
	   (syntax-violation __module_who__
	     "invalid argument specification"
	     (or input-form.stx original-formals.stx) arg-stx))))))

  (define (%validate-formals input-form.stx original-formals.stx standard-formals.stx)
    (cond ((lex.duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(syntax-violation __module_who__
		  "duplicate identifiers in formals specification"
		  (or input-form.stx original-formals.stx)
		  duplicate-id)))))

  #| end of module |# )

(define* (tagged-formals-syntax? formals.stx)
  ;;Return true if  FORMALS.STX is a syntax object representing  valid tagged formals
  ;;for a LET-VALUES syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature)
	(parse-tagged-formals-syntax formals.stx)
      #t)))


;;;; tagged binding parsing: callable signature

(case-define* parse-tagged-lambda-proto-syntax
  ;;Given a  syntax object representing  a tagged  callable spec: split  the standard
  ;;formals from the tags; do test for duplicate bindings.  Return 2 values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2. An instance of "lambda-signature".
  ;;
  (({_ standard-formals-syntax? lambda-signature?} {callable-signature.stx lex.syntax-object?})
   (parse-tagged-lambda-proto-syntax callable-signature.stx #f))
  (({_ standard-formals-syntax? lambda-signature?} {callable-signature.stx lex.syntax-object?} {input-form.stx lex.syntax-object?})
   ;;First we parse  and extract the return  values tagging, if any;  then we parse
   ;;the rest of the formals.
   (syntax-match callable-signature.stx (brace)
     ;;With return values tagging.
     (((brace ?who ?rv-tag* ... . ?rv-rest-tag) . ?formals)
      (lex.underscore-id? ?who)
      (let ((retvals.stx (append ?rv-tag*
				 ;;We  want  a  proper  list when  possible,  not  an
				 ;;improper list with the syntax object #'() as tail.
				 (if (null? (syntax->datum ?rv-rest-tag))
				     '()
				   ?rv-rest-tag))))
	(unless (retvals-signature-syntax? retvals.stx)
	  (syntax-violation __who__
	    "invalid return values signature syntax" input-form.stx retvals.stx))
	(receive (standard-formals.stx formals-signature)
	    (parse-tagged-formals-syntax ?formals input-form.stx)
	  (values standard-formals.stx
		  (make-lambda-signature (make-retvals-signature retvals.stx) formals-signature)))))
     ;;Without return values tagging.
     (?formals
      (receive (standard-formals.stx formals-signature)
	  (parse-tagged-formals-syntax ?formals input-form.stx)
	(values standard-formals.stx
		(make-lambda-signature (make-retvals-signature-standalone-list) formals-signature)))))))

(define* (tagged-lambda-proto-syntax? formals-stx)
  ;;Return true if  FORMALS-STX is a syntax object representing  valid tagged formals
  ;;for a LAMBDA syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature-tags)
	(parse-tagged-lambda-proto-syntax formals-stx)
      #t)))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'set-identifier-object-type-spec!		'scheme-indent-function 1)
;; eval: (put 'sys.syntax-case				'scheme-indent-function 2)
;; eval: (put 'sys.with-syntax				'scheme-indent-function 1)
;; eval: (put 'lex.syntax-violation/internal-error	'scheme-indent-function 1)
;; End:
