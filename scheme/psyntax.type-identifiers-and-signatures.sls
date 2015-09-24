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


(library (psyntax.type-identifiers-and-signatures)
  (export
    type-identifier?					all-type-identifiers?
    type-identifier-sub-and-super?			type-identifier-is-procedure-sub-type?
    type-identifier-common-ancestor
    typed-procedure-variable.unsafe-variant		typed-procedure-variable.unsafe-variant-set!
    fabricate-closure-type-identifier
    syntax-object.standard-formals?
    syntax-object.formals-signature?
    syntax-object.retvals-signature?
    syntax-object.retvals-signature-super-and-sub?	syntax-object.formals-signature-super-and-sub?
    retvals-signature-syntax-common-ancestor
    syntax-object.typed-argument?			syntax-object.parse-typed-argument
    syntax-object.list-of-typed-bindings?		syntax-object.parse-list-of-typed-bindings
    syntax-object.parse-formals-signature
    syntax-object.lambda-clause-signature?		syntax-object.parse-lambda-clause-signature

;;; --------------------------------------------------------------------
;;; signatures internal representation

    <retvals-signature>
    make-retvals-signature				retvals-signature?
    retvals-signature.tags
    make-retvals-signature/single-top			make-retvals-signature/single-void
    make-retvals-signature/single-boolean		make-retvals-signature/single-procedure
    make-retvals-signature/standalone-list		make-retvals-signature/fully-unspecified
    make-retvals-signature/single-value
    retvals-signature=?
    retvals-signature.fully-unspecified?
    retvals-signature.partially-unspecified?
    retvals-signature.super-and-sub?
    retvals-signature.single-tag?
    retvals-signature.single-top-tag?
    retvals-signature.single-tag-or-fully-unspecified?

    retvals-signatures-common-ancestor			datum-retvals-signature

    <formals-signature>
    make-formals-signature				formals-signature?
    formals-signature.tags
    formals-signature=?
    formals-signature.super-and-sub?
    formals-signature.fully-unspecified?

    <callable-signature>
    callable-signature?
    callable-signature.retvals

    <lambda-signature>
    make-lambda-signature				lambda-signature?
    lambda-signature.retvals				lambda-signature.formals
    lambda-signature=?
    lambda-signature.formals.tags			lambda-signature.retvals.tags
    lambda-signature.fully-unspecified?
    list-of-lambda-signatures?

    <clambda-compound>
    make-clambda-compound				clambda-compound?
    clambda-compound.retvals				clambda-compound.lambda-signature*
    )
  (import (except (rnrs)
		  generate-temporaries)
    (prefix (rnrs syntax-case) sys.)
    (psyntax.compat)
    (prefix (psyntax.config) config.)
    (prefix (psyntax.lexical-environment) lex.)
    (only (psyntax.syntax-match)
	  syntax-match
	  improper-list->list-and-rest
	  proper-list->head-and-last)
    (psyntax.syntactic-binding-properties)
    (only (psyntax.syntax-utilities)
	  syntax-unwrap))


;;;; helpers

(include "psyntax.helpers.scm" #t)


;;;; type identifier utilities

(case-define type-identifier?
  ((id)
   (type-identifier? id (lex.current-inferior-lexenv)))
  ((id lexenv)
   (and (identifier? id)
	(cond ((lex.id->label id)
	       => (lambda (label)
		    (let* ((descr (lex.label->syntactic-binding-descriptor label lexenv))
			   (value (lex.syntactic-binding-descriptor.value descr)))
		      (lex.object-type-spec? value))))
	      (else #f)))))

(define (assert-type-identifier who input-form.stx id)
  (define (%synner message)
    (syntax-violation who message input-form.stx id))
  (unless (identifier? id)
    (%synner "expected syntactic identifier as type identifier"))
  (cond ((lex.id->label id)
	 => (lambda (label)
	      (let ((descr (lex.label->syntactic-binding-descriptor label (lex.current-inferior-lexenv))))
		(case (lex.syntactic-binding-descriptor.type descr)
		  ((displaced-lexical)
		   (%synner "out of context syntactic identifier given as type identifier"))
		  (else
		   (unless (lex.object-type-spec? (lex.syntactic-binding-descriptor.value descr))
		     (%synner "syntactic identifier has no <object-type-spex> as descriptor value")))))))
	(else
	 (%synner "expected bound syntactic identifier as type identifier"))))

(case-define* type-identifier-sub-and-super?
  ((sub-type.id super-type.id)
   (type-identifier-sub-and-super? #f (lex.current-inferior-lexenv) sub-type.id super-type.id))
  ((input-form.stx lexenv sub-type.id super-type.id)
   ;;Given two  syntactic identifiers having  an instance of  "<object-type-spec>" as
   ;;value in  the syntactic  binding's descriptor: return  true if  SUPER-TYPE.ID is
   ;;FREE-IDENTIFIER=?  to SUB-TYPE.ID or one of its ancestors.
   ;;
   (define (%error-non-type-identifier id)
     (syntax-violation #f
       "the syntactic identifier is not a type identifier"
       input-form.stx id))
   (or (free-identifier=? sub-type.id super-type.id)
       (lex.$top-tag-id? super-type.id)
       (let* ((super-label (lex.id->label/or-error #f input-form.stx super-type.id))
	      (super-descr (lex.label->syntactic-binding-descriptor super-label lexenv))
	      (super-value (lex.syntactic-binding-descriptor.value super-descr)))
	 (if (lex.object-type-spec? super-value)
	     (let loop ((sub-type.id sub-type.id))
	       (let* ((sub-label (lex.id->label/or-error #f input-form.stx sub-type.id))
		      (sub-descr (lex.label->syntactic-binding-descriptor sub-label lexenv))
		      (sub-value (lex.syntactic-binding-descriptor.value sub-descr)))
		 (if (lex.object-type-spec? sub-value)
		     (cond ((lex.object-type-spec.parent-id sub-value)
			    => (lambda (parent.id)
				 (cond ((lex.$top-tag-id? parent.id)
					#f)
				       ((lex.~free-identifier=? parent.id super-type.id)
					#t)
				       (else
					(loop parent.id)))))
			   (else #f))
		   (%error-non-type-identifier sub-type.id))))
	   (%error-non-type-identifier super-type.id))))))

(case-define* type-identifier-common-ancestor
  ((id1 id2)
   (type-identifier-common-ancestor id1 id2 (lex.current-inferior-lexenv)))
  ((id1 id2 lexenv)
  ;;Visit the hierarchy of parents of the given type identifiers, determine the first
  ;;common ancestor and return its identifier.
  ;;
  (cond ((lex.~free-identifier=? id1 id2)
	 id1)
	((or (lex.$top-tag-id? id1)
	     (lex.$top-tag-id? id2))
	 (lex.top-tag-id))
	(else
	 (let outer-loop ((type1 id1))
	   (let inner-loop ((type2 id2))
	     (if (lex.~free-identifier=? type1 type2)
		 type1
	       (let* ((label2 (lex.id->label/or-error #f #f type2))
		      (descr2 (lex.label->syntactic-binding-descriptor label2 lexenv))
		      (value2 (lex.syntactic-binding-descriptor.value descr2)))
		 (cond ((lex.object-type-spec.parent-id value2)
			=> inner-loop)
		       (else
			(let* ((label1 (lex.id->label/or-error #f #f type1))
			       (descr1 (lex.label->syntactic-binding-descriptor label1 lexenv))
			       (value1 (lex.syntactic-binding-descriptor.value descr1)))
			  (cond ((lex.object-type-spec.parent-id value1)
				 => outer-loop)
				(else
				 (lex.top-tag-id))))))))))))))

(case-define all-type-identifiers?
  ((stx)
   (all-type-identifiers? stx (lex.current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx ()
     ((?id . ?rest)
      (and (type-identifier? ?id lexenv)
	   (all-type-identifiers? ?rest lexenv)))
     (() #t)
     (_  #f))))

(case-define type-identifier-is-procedure-sub-type?
  ((id)
   (type-identifier-is-procedure-sub-type? id (lex.current-inferior-lexenv)))
  ((id lexenv)
   (type-identifier-sub-and-super? #f lexenv id (lex.procedure-tag-id))))


;;;; typed variable with procedure sub-type type utilities
;;
;;The following  functions are used  to deal with  lexical variables, both  local and
;;global,  that  are  typed  with  a  sub-type of  "<procedure>".   It  is  known  at
;;expand-time that such  lexical variables are bound to a  closure object; this means
;;their syntactic binding descriptor has one of the formats:
;;
;;   (lexical-typed . ?lexical-typed-spec)
;;   (global-typed  . ?global-typed-spec)
;;
;;where   ?LEXICAL-TYPED-SPEC   is   an  instance   of   "<lexical-typed-spec>"   and
;;?GLOBAL-TYPED-SPEC is an  instance of "<global-typed-spec>".  These  two spec types
;;are  sub-types  of  "<typed-variable-spec>",  which  has  some  special  fields  to
;;represent expand-time properties of closure object's syntactic bindings.
;;

(case-define* typed-procedure-variable.unsafe-variant
  ;;Given  an identifier  representing  a typed,  imported  or non-imported,  lexical
  ;;variable which  is meant to  be bound  to a closure  object: return false  or the
  ;;symbolic expression representing its unsafe variant.
  ;;
  (({typed-variable.id identifier?})
   (typed-procedure-variable.unsafe-variant typed-variable.id (lex.current-inferior-lexenv)))
  (({typed-variable.id identifier?} lexenv)
   (let* ((label (lex.id->label/or-error __who__ #f typed-variable.id))
	  (descr (lex.label->syntactic-binding-descriptor label lexenv))
	  (spec  (lex.syntactic-binding-descriptor.value descr)))
     (if (lex.typed-variable-spec? spec)
	 (lex.typed-variable-spec.unsafe-variant-sexp spec)
       (assertion-violation __who__
	 "given identifier is not a typed lexical variable"
	 typed-variable.id)))))

(case-define* typed-procedure-variable.unsafe-variant-set!
  ;;Given an identifier representing a typed, non-imported, lexical variable which is
  ;;meant to be  bound to a closure object: set  the symbolic expression representing
  ;;its unsafe variant.
  ;;
  (({typed-variable.id identifier?} unsafe-variant.sexp)
   (typed-procedure-variable.unsafe-variant-set! typed-variable.id unsafe-variant.sexp (lex.current-inferior-lexenv)))
  (({typed-variable.id identifier?} unsafe-variant.sexp lexenv)
   (let* ((label (lex.id->label/or-error __who__ #f typed-variable.id))
	  (descr (lex.label->syntactic-binding-descriptor label lexenv))
	  (spec  (lex.syntactic-binding-descriptor.value descr)))
     (if (lex.lexical-typed-spec? spec)
	 (lex.typed-variable-spec.unsafe-variant-sexp-set! spec unsafe-variant.sexp)
       (assertion-violation __who__
	 "given identifier is not a typed lexical variable"
	 typed-variable.id unsafe-variant.sexp)))))


;;;; fabricated procedure type identifiers
;;
;;Let's consider the following code in the C language:
;;
;;   int func (double a, char * b) { ... }
;;   typedef int func_t (double a, char * b);
;;   func_t * the_func = do_something;
;;
;;we need a pointer to the function "func()",  so we define the type "func_t" as type
;;of the pointer to function "the_func".  We do something similar in Vicare.
;;
;;When the use of a core macro INTERNAL-DEFINE is expanded from the input form:
;;
;;   (internal-define ?attributes (?who . ?formals) . ?body)
;;
;;a QRHS is created, then the syntactic identifier ?WHO is bound, finally the QRHS is
;;expanded.  When the syntactic  binding for ?WHO is created: what  is its type?  For
;;sure it must be a sub-type of "<procedure>", but we would like to keep informations
;;about the signature of the closure object definition.
;;
;;When the use of a core macro LAMBDA or CASE-LAMBDA is expanded: a signature for the
;;resulting closure object is built; it is either an instance of "<lambda-signature>"
;;or an  instance of  "<clambda-compound>".  For example,  when the  following LAMBDA
;;syntax is expanded:
;;
;;   (lambda ({_ <exact-integer>} {a <fixnum>} {b <fixnum>})
;;     (+ 1 a b))
;;
;;the LAMBDA proto parser builds the following "lambda-signature" struct:
;;
;;   #[<lambda-signature>
;;       retvals=#[<retvals-signature> tags=(#'<exact-integer>)]
;;       formals=#[<formals-signature> tags=(#'<fixnum> #'<fixnum>)]]
;;
;;To represent  the type of  the closure object: we  create a fresh  type identifier,
;;bound in  the top-level rib with  the syntactic binding's descriptor  stored in the
;;VALUE field of the label gensym; the descriptor is of type "$closure-type-spec" and
;;it has an instance of "<closure-type-spec>" as value.
;;
(module (fabricate-closure-type-identifier)

  (define* ({fabricate-closure-type-identifier type-identifier?} {who false-or-symbol?} {signature callable-signature?})
    ;;WHO must be false  or a symbol representing the name of  the closure object; it
    ;;can be a random gensym when no name is given.  SIGNATURE must be an instance of
    ;;"<callable-signature>" or one of its sub-types.
    ;;
    (let* ((type.sym  (%make-closure-type-name who))
	   (type.lab  (gensym type.sym)))
      (receive-and-return (type.id)
	  (lex.make-top-level-syntactic-identifier-from-source-name-and-label type.sym type.lab)
	(set-symbol-value! type.lab (lex.make-syntactic-binding-descriptor/closure-type-name type.id signature)))))

  (define (false-or-symbol? obj)
    (or (not     obj)
	(symbol? obj)))

  (define (%make-closure-type-name who)
    (gensym (string-append "<"
			   (if who
			       (symbol->string who)
			     "anonymous")
			   "/closure-signature>")))

  #| end of module |# )


;;;; helpers and utilities

(define (datum-retvals-signature datum)
  (make-retvals-signature
   (list (cond ((boolean? datum)	(lex.core-prim-id '<boolean>))
	       ((char?    datum)	(lex.core-prim-id '<char>))
	       ((symbol?  datum)	(lex.core-prim-id '<symbol>))
	       ((keyword? datum)	(lex.core-prim-id '<keyword>))

	       ((fixnum?  datum)	(lex.core-prim-id '<fixnum>))
	       ((flonum?  datum)	(lex.core-prim-id '<flonum>))
	       ((ratnum?  datum)	(lex.core-prim-id '<ratnum>))
	       ((bignum?  datum)	(lex.core-prim-id '<bignum>))
	       ((compnum? datum)	(lex.core-prim-id '<compnum>))
	       ((cflonum? datum)	(lex.core-prim-id '<cflonum>))

	       ((string?  datum)	(lex.core-prim-id '<string>))
	       ((vector?  datum)	(lex.core-prim-id '<vector>))
	       ((list?    datum)	(lex.core-prim-id '<list>))
	       ((pair?    datum)	(lex.core-prim-id '<pair>))
	       ((bytevector? datum)	(lex.core-prim-id '<bytevector>))

	       ((eq? datum (void))	(lex.core-prim-id '<void>))
	       (else			(lex.top-tag-id))))))


;;;; formals, retvals, signature syntaxes predicates

(define (syntax-object.standard-formals? stx)
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
     (syntax-object.standard-formals? ?rest))
    (?rest
     (identifier? ?rest))
    ))

(case-define syntax-object.formals-signature?
  ((stx)
   (syntax-object.formals-signature? stx (lex.current-inferior-lexenv)))
  ((stx lexenv)
   ;;Return true if STX is a syntax object representing the tag signature of a tagged
   ;;formals syntax; otherwise  return false.  The return  value is true if  STX is a
   ;;proper  or improper  list of  tag identifiers,  with null  and a  standalone tag
   ;;identifier being acceptable.  Examples:
   ;;
   ;;   (formals-signature-syntax #'<list>)				=> #t
   ;;   (formals-signature-syntax #'())					=> #t
   ;;   (formals-signature-syntax #'(<fixnum> <string>))		=> #t
   ;;   (formals-signature-syntax #'(<fixnum> <string> . <fixnum>))	=> #t
   ;;
   ;;A standalone "<list>" identifier means: any number of values of any type.
   ;;
   (syntax-match stx ()
     (() #t)
     ((?id . ?rest)
      (type-identifier? ?id lexenv)
      (syntax-object.formals-signature? ?rest lexenv))
     (?rest
      (type-identifier? ?rest lexenv))
     )))

(case-define syntax-object.retvals-signature?
  ((stx)
   (syntax-object.retvals-signature? stx (lex.current-inferior-lexenv)))
  ((stx lexenv)
   ;;Return true if STX  is a syntax object representing the  tag signature of tagged
   ;;return values; otherwise return false.  The return  value is true if STX is null
   ;;or  a  proper  or improper  list  of  tag  identifiers,  with a  standalone  tag
   ;;identifier being acceptable.  Examples:
   ;;
   ;;   (retvals-signature-syntax #'<list>)				=> #t
   ;;   (retvals-signature-syntax #'())					=> #t
   ;;   (retvals-signature-syntax #'(<fixnum> <string>))		=> #t
   ;;   (retvals-signature-syntax #'(<fixnum> <string> . <fixnum>))	=> #t
   ;;
   ;;A standalone "<list>" identifier means: any number of values of any type.
   ;;
   (syntax-match stx ()
     (() #t)
     ((?id . ?rest)
      (type-identifier? ?id lexenv)
      (syntax-object.retvals-signature? ?rest lexenv))
     (?rest
      (type-identifier? ?rest lexenv))
     )))

;;; --------------------------------------------------------------------

(define* (syntax-object.formals-signature-partially-untyped? {stx syntax-object.formals-signature?})
  ;;The argument STX must be a  syntax object representing a formals signature syntax
  ;;according  to SYNTAX-OBJECT.FORMALS-SIGNATURE?.   Return true  if STX  has at  least one
  ;;"<top>" tag identifier; otherwise return false.
  ;;
  ($syntax-object.formals-signature-partially-untyped? stx))

(define ($syntax-object.formals-signature-partially-untyped? stx)
  (syntax-match stx ()
    (() #f)
    ((?tag . ?rest-tags)
     (or (lex.top-tag-id? ?tag)
	 ($syntax-object.formals-signature-partially-untyped? ?rest-tags)))
    (?rest
     #t)))

(define* (syntax-object.retvals-signature-partially-unspecified? {stx syntax-object.retvals-signature?})
  ;;The argument STX must be a  syntax object representing a retvals signature syntax
  ;;according to SYNTAX-OBJECT.RETVALS-SIGNATURE?.  Return true  if STX if the signature has
  ;;at least one "<top>" tag identifier; otherwise return false.
  ;;
  ($syntax-object.retvals-signature-partially-unspecified? stx))

(define $syntax-object.retvals-signature-partially-unspecified?
  $syntax-object.formals-signature-partially-untyped?)

;;; --------------------------------------------------------------------

(define* (syntax-object.formals-signature-super-and-sub? {super-signature syntax-object.formals-signature?}
							 {sub-signature   syntax-object.formals-signature?})
  ;;Return  true if  the super  signature syntax  and the  sub signature  syntax have
  ;;compatible structure and  the tags from the super signature  are supertags of the
  ;;tags from the sub signature; otherwise return false.
  ;;
  ;;This function can  be used to determine: if a  tuple of arguments (sub-signature)
  ;;matches  a lambda  formals's signature  (super-signature); if  a tuple  or return
  ;;values (sub-signature) matches the receiver signature (super-signature).
  ;;
  ($syntax-object.formals-signature-super-and-sub? super-signature sub-signature))

(define ($syntax-object.formals-signature-super-and-sub? super-signature sub-signature)
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
	(and (or (lex.top-tag-id? ?super-tag)
		 (type-identifier-sub-and-super? ?sub-tag ?super-tag))
	     ($syntax-object.formals-signature-super-and-sub? ?super-rest-tags ?sub-rest-tags)))
       (_ #f)))

    (?super-rest-tag
     (syntax-match sub-signature ()
       ;;The super signature is an improper list with rest item and the sub signature
       ;;is finished.  We want the following signatures to match:
       ;;
       ;;  super-singature == #'(<number>  <fixnum> . <top>)
       ;;  sub-signature   == #'(<complex> <fixnum>)
       ;;
       ;;because "<top>" in rest position means any number of objects of any type.
       (()
	(lex.top-tag-id? ?super-rest-tag))

       ;;The super signature is an improper  list shorter than the sub signature.  We
       ;;want the following signatures to match:
       ;;
       ;;  super-signature == #'(<number>  . <top>)
       ;;  sub-signature   == #'(<complex> <fixnum> <fixnum>)
       ;;
       ;;because "<top>"  in rest position means  any number of objects  of any type.
       ((?sub-tag . ?sub-rest-tags)
	(lex.top-tag-id? ?super-rest-tag))

       ;;Both the  signatures are improper lists  with the same number  of items, and
       ;;all the items are  correct super and sub; if the rest  tags are proper super
       ;;and subs: success!
       ;;
       ;;For example, we want the following signatures to match:
       ;;
       ;;  super-signature == #'(<string> <string> . <number>)
       ;;  sub-signature   == #'(<string> <string> . <fixnum>)
       ;;
       (?sub-rest-tag
	(type-identifier-sub-and-super? ?sub-rest-tag ?super-rest-tag))
       ))
    ))

(define* (syntax-object.retvals-signature-super-and-sub? {super-signature syntax-object.retvals-signature?}
							 {sub-signature   syntax-object.retvals-signature?})
  ;;Return  true  if the  super  signature  and  the  sub signature  have  compatible
  ;;structure and the  tags from the super  signature are supertags of  the tags from
  ;;the sub signature; otherwise return false.
  ;;
  ;;This function can be used to determine:  if a tuple of arguments matches a lambda
  ;;retvals's signaure; if a tuple or return values matches the receiver signature.
  ;;
  ($syntax-object.retvals-signature-super-and-sub? super-signature sub-signature))

(define-syntax-rule ($syntax-object.retvals-signature-super-and-sub? ?super-signature ?sub-signature)
  ($syntax-object.formals-signature-super-and-sub? ?super-signature ?sub-signature))

;;; --------------------------------------------------------------------

(module (retvals-signature-syntax-common-ancestor)

  (define* (retvals-signature-syntax-common-ancestor {sig1 syntax-object.retvals-signature?}
						     {sig2 syntax-object.retvals-signature?})
    ;;Given two signatures syntaxes: return a new signature syntax representing their
    ;;common ancestor.  Examples:
    ;;
    ;;  (retvals-signature-syntax-common-ancestor #'(<fixnum>) #'(<fixnum>))
    ;;  => #'(<fixnum>)
    ;;
    ;;  (retvals-signature-syntax-common-ancestor #'(<fixnum>) #'(<flonum>))
    ;;  => #'(<real>)
    ;;
    ;;  (retvals-signature-syntax-common-ancestor #'(<fixnum>) #'(<string>))
    ;;  => #'(<top>)
    ;;
    ($retvals-signature-syntax-common-ancestor sig1 sig2))

  (define ($retvals-signature-syntax-common-ancestor sig1 sig2)
    ;;
    ;;We assume that  both SIG1 and SIG2 are retvals  signature syntaxes according to
    ;;SYNTAX-OBJECT.RETVALS-SIGNATURE?.
    ;;
    (if (or (lex.list-tag-id? sig1)
	    (lex.list-tag-id? sig2))
	(lex.list-tag-id)
      (syntax-match sig1 ()
	(()
	 (syntax-match sig2 ()
	   (() '())
	   ((?tag2 . ?rest-tags2)
	    (lex.top-tag-id))
	   (_
	    (lex.top-tag-id))))

	((?tag1 . ?rest-tags1)
	 (syntax-match sig2 ()
	   (()
	    (lex.top-tag-id))
	   ((?tag2 . ?rest-tags2)
	    (cons (type-identifier-common-ancestor ?tag1 ?tag2)
		  ($retvals-signature-syntax-common-ancestor ?rest-tags1 ?rest-tags2)))
	   (?rest-tag2
	    (lex.top-tag-id))))

	(?rest-tag1
	 (syntax-match sig2 ()
	   (()
	    (lex.top-tag-id))
	   ((?tag2 . ?rest-tags2)
	    (lex.top-tag-id))
	   (?rest-tag2
	    (type-identifier-common-ancestor ?rest-tag1 ?rest-tag2))))
	)))

  #| end of module |# )


;;;; type definition: return values signature

(define-record-type (<retvals-signature> make-retvals-signature retvals-signature?)
  (nongenerative vicare:expander:<retvals-signature>)
  (fields (immutable tags	retvals-signature.tags))
		;A syntax object representing a retvals signature syntax according to
		;SYNTAX-OBJECT.RETVALS-SIGNATURE?.
  (protocol
    (lambda (make-record)
      (define* (make-retvals-signature {tags syntax-object.retvals-signature?})
	(make-record (syntax-unwrap tags)))
      make-retvals-signature)))

;;; special constructors

(define-syntax define-cached-retvals-maker
  (syntax-rules ()
    ((_ ?who ?stx-maker)
     (define ?who
       (let ((rvs #f))
	 (lambda ()
	   (or rvs
	       (receive-and-return (S)
		   (make-retvals-signature ?stx-maker)
		 (set! rvs S)))))))
    ))

(define-syntax define-single-tag-retvals-maker
  (syntax-rules ()
    ((_ ?who ?type-id-maker)
     (define-cached-retvals-maker ?who (list (?type-id-maker))))
    ))

(define-single-tag-retvals-maker make-retvals-signature/single-top		lex.top-tag-id)
(define-single-tag-retvals-maker make-retvals-signature/single-void		lex.void-tag-id)
(define-single-tag-retvals-maker make-retvals-signature/single-boolean		lex.boolean-tag-id)
(define-single-tag-retvals-maker make-retvals-signature/single-procedure	lex.procedure-tag-id)

(define-cached-retvals-maker make-retvals-signature/standalone-list		(lex.list-tag-id))

(define-syntax-rule (make-retvals-signature/fully-unspecified)
  (make-retvals-signature/standalone-list))

(define* (make-retvals-signature/single-value {tag type-identifier?})
  (make-retvals-signature (list tag)))


;;;; type definition: formals signature

(define-record-type (<formals-signature> make-formals-signature formals-signature?)
  (nongenerative vicare:expander:<formals-signature>)
  (fields (immutable tags	formals-signature.tags))
		;A syntax object representing a formals signature syntax according to
		;SYNTAX-OBJECT.FORMALS-SIGNATURE?.
  (protocol
    (lambda (make-record)
      (define* (make-formals-signature {tags syntax-object.formals-signature?})
	(make-record (syntax-unwrap tags)))
      make-formals-signature)))


;;;; type definition: callable signature

(define-record-type (<callable-signature> dummy-make-callable-signature callable-signature?)
  (nongenerative vicare:expander:<callable-signature>)
  (fields
   (immutable retvals	callable-signature.retvals)
		;An instance of "<retvals-signature>".
		;
		;For the "<clambda-compound>" sub-type it represents the signature of
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
   #| end of FIELDS |# ))


;;;; type definition: LAMBDA signature

(define-record-type (<lambda-signature> make-lambda-signature lambda-signature?)
  (nongenerative vicare:expander:<lambda-signature>)
  (parent <callable-signature>)
  (fields
   (immutable formals	lambda-signature.formals)
		;An instance of "<formals-signature>".
   #| end of FIELDS |# )
  (protocol
    (lambda (make-callable-signature)
      (define* (make-lambda-signature {retvals retvals-signature?} {formals formals-signature?})
	((make-callable-signature retvals) formals))
      make-lambda-signature)))

(define* (lambda-signature.retvals {sig lambda-signature?})
  (callable-signature.retvals sig))


;;;; type definition: CLAMBDA signature

(define-record-type (<clambda-compound> make-clambda-compound clambda-compound?)
  (nongenerative vicare:expander:<clambda-compound>)
  (parent <callable-signature>)
  (fields
   (immutable lambda-signature*	clambda-compound.lambda-signature*)
		;A  proper list  of "<lambda-signature>"  instances representing  the
		;signatures of the CASE-LAMBDA clauses.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-callable-signature)
      (define* (make-clambda-compound {signature* list-of-lambda-signatures?})
	((make-callable-signature (if (pair? signature*)
				      (apply retvals-signatures-common-ancestor
					     (map callable-signature.retvals signature*))
				    (make-retvals-signature/standalone-list)))
	 signature*))
      make-clambda-compound)))

(define* (clambda-compound.retvals {sig clambda-compound?})
  (callable-signature.retvals sig))


;;;; lambda-signature stuff

(define* (lambda-signature=? {signature1 lambda-signature?} {signature2 lambda-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (and (syntax=? (formals-signature.tags (lambda-signature.formals signature1))
		 (formals-signature.tags (lambda-signature.formals signature2)))
       (syntax=? (retvals-signature.tags (callable-signature.retvals signature1))
		 (retvals-signature.tags (callable-signature.retvals signature2)))))

;;; --------------------------------------------------------------------

(define* (lambda-signature.formals.tags {signature lambda-signature?})
  (formals-signature.tags (lambda-signature.formals signature)))

(define* (lambda-signature.retvals.tags {signature lambda-signature?})
  (retvals-signature.tags (callable-signature.retvals signature)))

;;; --------------------------------------------------------------------

(define (list-of-lambda-signatures? obj)
  (and (list? obj)
       (for-all lambda-signature? obj)))

(define* (lambda-signature.fully-unspecified? {signature lambda-signature?})
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
  (and (formals-signature.fully-unspecified? (lambda-signature.formals signature))
       (retvals-signature.fully-unspecified? (callable-signature.retvals signature))))


;;;; formals-signature stuff

(define* (formals-signature=? {signature1 formals-signature?} {signature2 formals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? (formals-signature.tags signature1)
	    (formals-signature.tags signature2)))

;;; --------------------------------------------------------------------

(define* (formals-signature.super-and-sub? {super-signature formals-signature?}
					   {sub-signature   formals-signature?})
  ($syntax-object.formals-signature-super-and-sub? (formals-signature.tags super-signature)
						   (formals-signature.tags sub-signature)))

(define* (formals-signature.fully-unspecified? {signature formals-signature?})
  ;;Return  true  if the  formals  signature  does  not  specify any  argument  type,
  ;;otherwise  return false;  in other  words, return  true if  the signature  is one
  ;;among:
  ;;
  ;;   (<top> ...)
  ;;   (<top> ... . <list>)
  ;;
  (let ((formals.stx (formals-signature.tags signature)))
    (if (list? formals.stx)
	(for-all lex.top-tag-id? formals.stx)
      (receive (head tail)
	  (improper-list->list-and-rest formals.stx)
	(and (for-all lex.top-tag-id? head)
	     (lex.list-tag-id? tail))))))


;;;; retvals-signature stuff

(define* (retvals-signature.fully-unspecified? {signature retvals-signature?})
  (lex.list-tag-id? (retvals-signature.tags signature)))

(define* (retvals-signature.partially-unspecified? {signature retvals-signature?})
  ($syntax-object.retvals-signature-partially-unspecified? (retvals-signature.tags signature)))

(define* (retvals-signature.super-and-sub? {super-signature retvals-signature?}
					   {sub-signature   retvals-signature?})
  ($syntax-object.retvals-signature-super-and-sub? (retvals-signature.tags super-signature)
						   (retvals-signature.tags sub-signature)))

(define* (retvals-signature.single-tag? {signature retvals-signature?})
  ;;Return  true if  SIGNATURE represents  a  single return  value, otherwise  return
  ;;false.  We have to remember that a signature syntax can be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (syntax-match (retvals-signature.tags signature) ()
    ((?tag)	#t)
    (_		#f)))

(define* (retvals-signature.single-top-tag? {signature retvals-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.   We have to remember that, after  parsing syntax objects
  ;;with SYNTAX-MATCH, a signature syntax can result to be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (syntax-match (retvals-signature.tags signature) ()
    ((?tag)
     (lex.top-tag-id? ?tag))
    (_ #f)))

(define* (retvals-signature.single-tag-or-fully-unspecified? {signature retvals-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier,  otherwise return  false.  We have  to remember  that, after
  ;;parsing syntax objects with SYNTAX-MATCH, a signature syntax can result to be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (let ((tags (retvals-signature.tags signature)))
    (or (lex.list-tag-id? tags)
	(syntax-match tags ()
	  ((?tag)	#t)
	  (_		#f)))))

;;; --------------------------------------------------------------------

(define* (retvals-signature=? {signature1 retvals-signature?} {signature2 retvals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? (retvals-signature.tags signature1)
	    (retvals-signature.tags signature2)))

;;; --------------------------------------------------------------------

(case-define* retvals-signatures-common-ancestor
  (({sig retvals-signature?})
   sig)

  (({sig1 retvals-signature?} {sig2 retvals-signature?})
   (make-retvals-signature
    (retvals-signature-syntax-common-ancestor (retvals-signature.tags sig1)
					      (retvals-signature.tags sig2))))
  (({sig1 retvals-signature?} {sig2 retvals-signature?} . sig*)
   (fold-left (lambda (sig-a sig-b)
		(retvals-signatures-common-ancestor sig-a sig-b))
     (retvals-signatures-common-ancestor sig1 sig2)
     sig*)))


;;;; tagged binding parsing: standalone identifiers

(define (syntax-object.typed-argument? stx)
  ;;Return  true  if  STX is  a  syntax  object  representing  a tagged  or  untagged
  ;;identifier, otherwise return false.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (type-identifier? ?tag)))
    (?id
     (identifier? ?id))))

(define* (syntax-object.parse-typed-argument stx)
  ;;If  STX is  a tagged  or  untagged identifier,  return 2  values: the  identifier
  ;;representing the binding name and  the identifier representing the tag; otherwise
  ;;raise  an exception.   When no  tag is  present: the  tag identifier  defaults to
  ;;"<top>".
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (begin
       (assert-type-identifier __who__ stx ?tag)
       (values ?id ?tag)))
    (?id
     (identifier? ?id)
     (values ?id (lex.top-tag-id)))))


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

(case-define* syntax-object.parse-list-of-typed-bindings
  ((stx)
   (syntax-object.parse-list-of-typed-bindings stx stx))
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
	      (assert-type-identifier __who__ input-form.stx ?tag)
	      (receive (id* tag*)
		  (recur ?other-id*)
		(values (cons ?id id*) (cons ?tag tag*)))))
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (receive (id* tag*)
		(recur ?other-id*)
	      (values (cons ?id id*) (cons (lex.top-tag-id) tag*))))
	   (_
	    (if input-form.stx
		(%invalid-tagged-bindings-syntax input-form.stx stx)
	      (%invalid-tagged-bindings-syntax stx #f)))
	   ))
     (unless (lex.distinct-bound-ids? id*)
       (if input-form.stx
	   (%duplicate-identifiers-in-bindings-specification input-form.stx stx)
	 (%duplicate-identifiers-in-bindings-specification stx #f))))))

(define* (syntax-object.list-of-typed-bindings? lhs*)
  ;;Return true  if LHS* is a  list of possibly tagged  identifiers; otherwise return
  ;;false.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (id* tag*)
	(syntax-object.parse-list-of-typed-bindings lhs*)
      #t)))


;;;; tagged binding parsing: let-values formals

(module (syntax-object.parse-formals-signature)
  ;;Given a syntax object representing  tagged LET-VALUES formals: split formals from
  ;;tags.  Do test for duplicate bindings.
  ;;
  ;;Return 2 values:
  ;;
  ;;1..A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2..An object representing the LET-VALUES tagging signature.
  ;;
  (define-module-who syntax-object.parse-formals-signature)

  (case-define* syntax-object.parse-formals-signature
    (({_ syntax-object.standard-formals? formals-signature?} original-formals.stx)
     (syntax-object.parse-formals-signature original-formals.stx #f))
    (({_ syntax-object.standard-formals? formals-signature?} original-formals.stx input-form.stx)
     (receive (standard-formals formals-tags)
     	 (%parse-formals input-form.stx original-formals.stx original-formals.stx)
       (values standard-formals (make-formals-signature formals-tags)))))

  (define (%parse-formals input-form.stx original-formals.stx formals.stx)
    (syntax-match formals.stx (brace)

      ;;Tagged args, as in:
      ;;
      ;;   (let-values (({args <fixnum>} ?expr)) . ?body)
      ;;
      ;;the given tag is the type of each item in ARGS.
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (values ?args-id ?args-tag))

      ;;Possibly tagged identifiers with tagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . {rest <fixnum>}) ?expr)) . ?body)
      ;;
      ;;the given tag is the type of each item in REST.
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (begin
	 (unless (and (identifier? ?rest-id)
		      (identifier? ?rest-tag))
	   (syntax-violation __module_who__
	     "invalid rest argument specification" original-formals.stx (list 'brace ?rest-id ?rest-tag)))
	 (assert-type-identifier __module_who__ input-form.stx ?rest-tag)
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
	       (values ?rest-id (lex.top-tag-id))))
	 (%validate-formals input-form.stx original-formals.stx standard-formals.stx)))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (begin
	 (%validate-formals input-form.stx original-formals.stx ?id*)
	 (values ?id* (map (lambda (id) (lex.top-tag-id)) ?id*))))

      ;;Standard formals: UNtagged identifiers with UNtagged rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-formals input-form.stx original-formals.stx (append ?id* ?rest-id))
	 (values formals.stx (cons* (map (lambda (id) (lex.top-tag-id)) ?id*) (lex.top-tag-id)))))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (lex.top-tag-id)))

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
	   (values (cons ?id standard-formals) (cons (lex.top-tag-id) tags)))
	  ;;Tagged argument.
	  ((brace ?id ?tag)
	   (and (identifier? ?id)
		(identifier? ?tag))
	   (begin
	     (assert-type-identifier __module_who__ input-form.stx ?tag)
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


;;;; tagged binding parsing: callable signature

(case-define* syntax-object.parse-lambda-clause-signature
  ;;Given a  syntax object representing  a tagged  callable spec: split  the standard
  ;;formals from the tags; do test for duplicate bindings.  Return 2 values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2. An instance of "lambda-signature".
  ;;
  ;;This function *does*  enforce the constraint: the identifiers  in type identifier
  ;;positions must  actually be type  identifiers (with syntactic  binding descriptor
  ;;already added to the LEXENV).
  ;;
  (({_ syntax-object.standard-formals? lambda-signature?} {callable-signature.stx lex.syntax-object?})
   (syntax-object.parse-lambda-clause-signature callable-signature.stx #f))
  (({_ syntax-object.standard-formals? lambda-signature?} {callable-signature.stx lex.syntax-object?} {input-form.stx lex.syntax-object?})
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
	(unless (syntax-object.retvals-signature? retvals.stx)
	  (syntax-violation __who__
	    "invalid return values signature syntax" input-form.stx retvals.stx))
	(receive (standard-formals.stx formals-signature)
	      (syntax-object.parse-formals-signature ?formals input-form.stx)
	  (values standard-formals.stx
		  (make-lambda-signature (make-retvals-signature retvals.stx) formals-signature)))))
     ;;Without return values tagging.
     (?formals
      (receive (standard-formals.stx formals-signature)
	  (syntax-object.parse-formals-signature ?formals input-form.stx)
	(values standard-formals.stx
		(make-lambda-signature (make-retvals-signature/standalone-list) formals-signature)))))))

(define* (syntax-object.lambda-clause-signature? formals-stx)
  ;;Return true if  FORMALS-STX is a syntax object representing  valid tagged formals
  ;;for a LAMBDA syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature-tags)
	(syntax-object.parse-lambda-clause-signature formals-stx)
      #t)))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'set-identifier-tag-type-spec!		'scheme-indent-function 1)
;; eval: (put 'sys.syntax-case				'scheme-indent-function 2)
;; eval: (put 'sys.with-syntax				'scheme-indent-function 1)
;; End:
