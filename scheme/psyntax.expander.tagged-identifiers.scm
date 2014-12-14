;;;Copyright (c) 2010-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (({type-id	identifier-bound?}
    {parent-id	tag-identifier?}
    {pred-stx	syntax-object?})
   (let* ((parent-spec (identifier-object-type-spec parent-id))
	  (uid         (id->label type-id))
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

  (({type-id	identifier-bound?}
    {parent-id	tag-identifier?}
    {pred-stx	syntax-object?}
    {construct	false-or-procedure?}
    {accessor	false-or-procedure?}
    {mutator	false-or-procedure?}
    {getter	false-or-procedure?}
    {setter	false-or-procedure?}
    {caster	false-or-procedure?}
    {dispatcher	false-or-procedure?})
   (let* ((parent-spec (identifier-object-type-spec parent-id))
	  (uid         (id->label type-id))
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
	  (syntax-violation/internal-error __who__
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
		   (syntax-violation/internal-error __who__
		     "undefined tag predicate" input-form.stx tag-id))))
	 (else
	  ;;This should never happen because we  have validated the identifier in the
	  ;;fender.
	  (syntax-violation/internal-error __who__
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
  (({tag-id tag-identifier?} {keys.stx syntax-object?} input-form.stx)
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
  (({tag-id tag-identifier?} {keys.stx syntax-object?} input-form.stx)
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
  (define-fluid-override __who__
    (identifier-syntax 'tag-identifier-dispatch))

  (define* (tag-identifier-dispatch {tag tag-identifier?} {member.id identifier?} {input-form.stx syntax-object?})
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
    (syntax-violation __who__ "invalid tagged syntax" input-form.stx))

  #| end of module: TAG-DISPATCH |# )


;;;; tag identifiers

(define-constant *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*
  'vicare:expander:object-type-spec)

;;; --------------------------------------------------------------------

(define* (set-identifier-object-type-spec! {type-id identifier-bound?} {spec object-type-spec?})
  ;;Add to  the syntactic binding  label property list  an entry representing  a type
  ;;specification.  When this call succeeds: TYPE-ID becomes a tag identifier.
  ;;
  (let ((label (id->label/or-error __who__ type-id type-id)))
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

(define* ({identifier-object-type-spec false-or-object-type-spec?} {tag identifier-bound?})
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
       ($identifier-bound? obj)
       (and ($identifier-object-type-spec obj)
	    #t)))

(define (tag-identifier-and-list-sub-tag? obj)
  ;;Return true if OBJ is a bound identifier with "object-type-spec" property set and
  ;;it is a sub-tag of "<list>"; otherwise return false.
  ;;
  (and (identifier? obj)
       ($identifier-bound? obj)
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
    (or (free-id=? super-tag sub-tag)
	(free-id=? (top-tag-id) super-tag)
	(let ((pspec ($object-type-spec-parent-spec ($identifier-object-type-spec sub-tag))))
	  (and pspec
	       (let ((sub-ptag ($object-type-spec-type-id pspec)))
		 (and (not (free-id=? (top-tag-id) sub-ptag))
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
  ;;than comparing identifiers with FREE-ID=?.  (Marco Maggi; Sat Apr 5, 2014)
  ;;
  (if (or (free-id=? tag1 tag2)
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
		       ((free-id=? ($car anc1) ($car anc2))
			($car anc1))
		       (else
			(inner ($cdr anc2))))))
	      (else
	       (outer ($cdr anc1))))))))


;;;; tagged identifiers: expand-time binding type tagging

(define-constant *EXPAND-TIME-BINDING-TAG-COOKIE*
  'vicare:expander:binding-type-tagging)

;;; --------------------------------------------------------------------

(define* (set-identifier-tag! {binding-id identifier-bound?} {tag tag-identifier?})
  ;;Given a  syntactic binding identifier:  add TAG to  its property list  as binding
  ;;type  tagging.  This  tag  should represent  the object  type  referenced by  the
  ;;binding.  When this call succeeds: BINDING-ID becomes a tagged identifier.
  ;;
  (let ((label (id->label/or-error __who__ binding-id binding-id)))
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

(define* (override-identifier-tag! {binding-id identifier-bound?} {tag tag-identifier?})
  ;;Given a  syntactic binding identifier:  add TAG to  its property list  as binding
  ;;type  tagging,  silently  overriding  the previous  property.   This  tag  should
  ;;represent the object type referenced by the binding.
  ;;
  ($syntactic-binding-putprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE* tag))

(module (identifier-tag
	 tagged-identifier?)

  (define* (identifier-tag {binding-id identifier-bound?})
    ;;Given  a  syntactic binding  identifier:  retrieve  from  its property  list  the
    ;;identifier representing  the binding  type tagging.   This tag  identifier should
    ;;represent the object type referenced by the binding.
    ;;
    ($identifier-tag binding-id))

  (define* (tagged-identifier? {id identifier-bound?})
    ;;Return #t  if ID is an  identifier having a type  tagging; otherwise
    ;;return false.  If the return value is true: ID is a bound identifier
    ;;created by some binding syntaxes (define, let, letrec, ...).
    ;;
    (and ($identifier-tag id)
	 #t))

  (define ($identifier-tag binding-id)
    ($syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE*))

  #| end of module |# )

(define* (identifier-tag-retvals-signature {id identifier-bound?})
  (cond ((identifier-tag id)
	 => (lambda (tag)
	      (make-retvals-signature-single-value tag)))
	(else
	 (make-retvals-signature-single-top))))

;;; --------------------------------------------------------------------

(define* (set-label-tag! {label symbol?} {tag tag-identifier?})
  ;;Given a  syntactic binding LABEL:  add TAG  to its property  list as
  ;;binding type  tagging.  This  tag should  represent the  object type
  ;;referenced by the binding.
  ;;
  (cond (($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*)
	 => (lambda (old-tag)
	      (syntax-violation __who__
		"label binding tag already defined" label old-tag tag)))
	(($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	 => (lambda (spec)
	      (assertion-violation __who__
		"tag identifier cannot become a tagged identifier"
		label spec tag)))
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
      (let ((spec  (make-object-type-spec tag (procedure-tag-id) (procedure-pred-id))))
	(set-identifier-object-type-spec! tag spec)
	($putprop lab *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE* callable-signature))
      tag))

  (define* (%fabricate-bound-identifier {sym symbol?})
    ;;Build an  identifier having SYM as  name.  Return 2 values:  the identifier and
    ;;its label gensym.
    ;;
    ;;The returned  identifier is bound  in the sense  that applying ID->LABEL  to it
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
      (values (make-<stx> sym TOP-MARK*
			  (list (make-<rib> (list sym) TOP-MARK** (list lab) #f))
			  '())
	      lab)))

  (define-constant *EXPAND-TIME-TAG-CALLABLE-SIGNATURE-COOKIE*
    'vicare:expander:tag-callable-signature)

  (define* (set-tag-identifier-callable-signature! {tag-id tag-identifier?} {callable-signature callable-signature?})
    (let ((label (id->label/or-error __who__ tag-id tag-id)))
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
  (cond ((id->label id)
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


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; End:
