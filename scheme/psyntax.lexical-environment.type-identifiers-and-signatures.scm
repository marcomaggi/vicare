;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module PSYNTAX-TYPE-IDENTIFIERS-AND-SIGNATURES
  (type-identifier?
   all-type-identifiers?
   type-identifier=?
   type-identifier-super-and-sub?			type-identifier-super-and-sub?/matching
   type-identifier-is-procedure-sub-type?		type-identifier-is-procedure-or-procedure-sub-type?
   type-identifier-is-list-sub-type?			type-identifier-is-list-or-list-sub-type?
   type-identifier-is-vector-sub-type?			type-identifier-is-vector-or-vector-sub-type?
   type-identifier-common-ancestor
   typed-procedure-variable.unsafe-variant		typed-procedure-variable.unsafe-variant-set!

   fabricate-closure-type-identifier			fabricate-list-type-identifier

;;; --------------------------------------------------------------------

   syntax-object.typed-argument?			syntax-object.parse-typed-argument

   syntax-object.type-signature?			syntax-object.type-signature.single-identifier?
   syntax-object.type-signature.fully-untyped?		syntax-object.type-signature.partially-untyped?
   syntax-object.type-signature.untyped?		syntax-object.type-signature.no-return?
   syntax-object.type-signature.super-and-sub?		syntax-object.type-signature.super-and-sub?/matching
   syntax-object.type-signature.common-ancestor
   syntax-object.type-signature.min-and-max-count

   syntax-object.parse-standard-formals			syntax-object.parse-typed-formals
   syntax-object.parse-standard-list-of-bindings	syntax-object.parse-typed-list-of-bindings
   syntax-object.parse-standard-clambda-clause-formals	syntax-object.parse-typed-clambda-clause-formals
   syntax-object.parse-standard-clambda-multi-clauses-formals syntax-object.parse-typed-clambda-multi-clauses-formals

   syntax-object.standard-formals?			syntax-object.typed-formals?
   syntax-object.standard-clambda-clause-formals?	syntax-object.typed-clambda-clause-formals?

;;; --------------------------------------------------------------------
;;; signatures internal representation

   <type-signature>
   <type-signature>-rtd					<type-signature>-rcd
   make-type-signature					type-signature?
   type-signature-tags
   make-type-signature/single-top			make-type-signature/single-void
   make-type-signature/single-boolean			make-type-signature/single-procedure
   make-type-signature/single-stx			make-type-signature/single-syntactic-identifier
   make-type-signature/standalone-list			make-type-signature/fully-untyped
   make-type-signature/single-untyped			make-type-signature/single-value
   type-signature=?
   type-signature.fully-untyped?			type-signature.partially-untyped?
   type-signature.untyped?
   type-signature.super-and-sub?			type-signature.super-and-sub?/matching
   type-signature.single-type?				type-signature.single-top-tag?
   type-signature.single-type-or-fully-untyped?
   type-signature.min-count				type-signature.max-count

   type-signature.common-ancestor			datum-type-signature

   <callable-signature>
   callable-signature?
   callable-signature.retvals

   <clambda-clause-signature>
   make-clambda-clause-signature			clambda-clause-signature?
   clambda-clause-signature=?
   clambda-clause-signature.retvals			clambda-clause-signature.retvals.tags
   clambda-clause-signature.argvals			clambda-clause-signature.argvals.tags
   clambda-clause-signature.fully-untyped?		clambda-clause-signature.untyped?
   clambda-signature.min-and-max-argvals

   <clambda-signature>
   make-clambda-signature				clambda-signature?
   clambda-signature.retvals				clambda-signature.clause-signature*)


;;;; helpers

#;(include "psyntax.helpers.scm" #t)


;;;; type identifier utilities

(case-define* type-identifier?
  ;;Return true if the argument ID is  a type identifier; otherwise return false.  If
  ;;ID is  not an  identifier or it  is unbound: return  false.  If  ID is an  out of
  ;;context identifier: raise an exception.
  ;;
  ((id)
   (type-identifier? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(cond ((id->label id)
	       => (lambda (label)
		    (let ((descr (label->syntactic-binding-descriptor label lexenv)))
		      (case (syntactic-binding-descriptor.type descr)
			((displaced-lexical)
			 (raise
			  (condition (make-who-condition __who__)
				     (make-message-condition "identifier out of context (identifier's label not in LEXENV)")
				     (make-syntax-violation input-form.stx id)
				     (make-syntactic-binding-descriptor-condition descr))))
			((core-object-type-name local-object-type-name global-object-type-name)
			 #t)
			(else #f)))))
	      (else #f)))))

;;; --------------------------------------------------------------------

(case-define* type-identifier=?
  ;;The arguments ID1 and ID2 must be type identifiers according to TYPE-IDENTIFIER?,
  ;;otherwise an exception is raised.  Return true  if ID1 and ID2 represent the same
  ;;type; otherwise return false.
  ;;
  ((id1 id2)
   (type-identifier=? id1 id2 (current-inferior-lexenv) #f))
  ((id1 id2 lexenv)
   (type-identifier=? id1 id2 lexenv #f))
  ((id1 id2 lexenv input-form.stx)
   ;;These calls to ID->OBJECT-TYPE-SPECIFICATION serve two purposes: to validate the
   ;;arguments  as  type   identifiers;  to  retrieve  the   associated  instance  of
   ;;"<object-type-spec>".
   (let ((ots1 (id->object-type-specification __who__ input-form.stx id1 lexenv))
	 (ots2 (id->object-type-specification __who__ input-form.stx id2 lexenv)))
     (or (eq? ots1 ots2)
	 (cond ((list-type-spec? ots1)
		(and (list-type-spec? ots2)
		     (type-identifier=? (list-type-spec.type-id ots1)
					(list-type-spec.type-id ots2)
					lexenv)))
	       ((vector-type-spec? ots1)
		(and (vector-type-spec? ots2)
		     (type-identifier=? (vector-type-spec.type-id ots1)
					(vector-type-spec.type-id ots2)
					lexenv)))
	       (else #f))))))

(case-define* type-identifier-super-and-sub?
  ((super-type.id sub-type.id)
   (type-identifier-super-and-sub? super-type.id sub-type.id (current-inferior-lexenv) #f))
  ((super-type.id sub-type.id lexenv)
   (type-identifier-super-and-sub? super-type.id sub-type.id lexenv #f))
  ((super-type.id sub-type.id lexenv input-form.stx)
   ;;The arguments SUPER-TYPE.ID  and SUB-TYPE.ID must be  type identifiers according
   ;;to TYPE-IDENTIFIER?,  otherwise the behaviour  of this function  is unspecified.
   ;;Return true  if SUPER-TYPE.ID is  a super-type of SUB-TYPE.ID;  otherwise return
   ;;false.
   ;;
   (cond ((~free-identifier=? super-type.id sub-type.id)
	  #t)
	 (($top-tag-id? super-type.id)
	  #t)
	 ((or ($untyped-tag-id? super-type.id)
	      ($untyped-tag-id? sub-type.id)
	      ($top-tag-id?     sub-type.id))
	  #f)
	 ((procedure-tag-id? super-type.id)
	  (or (predicate-tag-id? sub-type.id)
	      (closure-type-spec? (id->object-type-specification __who__ input-form.stx sub-type.id lexenv))))
	 (else
	  (let ((super-ots (id->object-type-specification __who__ input-form.stx super-type.id lexenv))
		(sub-ots   (id->object-type-specification __who__ input-form.stx sub-type.id   lexenv)))
	    (cond ((list-type-spec? super-ots)
		   (and (list-type-spec? sub-ots)
			(type-identifier-super-and-sub? (list-type-spec.type-id super-ots)
							(list-type-spec.type-id sub-ots)
							lexenv input-form.stx)))
		  ((vector-type-spec? super-ots)
		   (and (vector-type-spec? sub-ots)
			(type-identifier-super-and-sub? (vector-type-spec.type-id super-ots)
							(vector-type-spec.type-id sub-ots)
							lexenv input-form.stx)))
		  (else
		   (let loop ((sub-ots sub-ots))
		     (cond ((object-type-spec.parent-id sub-ots)
			    => (lambda (parent.id)
				 (if ($top-tag-id? parent.id)
				     #f
				   (let ((parent-ots (id->object-type-specification __who__ input-form.stx parent.id lexenv)))
				     (or (eq? super-ots parent-ots)
					 (loop parent-ots))))))
			   (else #f))))))))))

(case-define* type-identifier-super-and-sub?/matching
  ((super-type.id sub-type.id)
   (type-identifier-super-and-sub?/matching super-type.id sub-type.id (current-inferior-lexenv) #f))
  ((super-type.id sub-type.id lexenv)
   (type-identifier-super-and-sub?/matching super-type.id sub-type.id lexenv #f))
  ((super-type.id sub-type.id lexenv input-form.stx)
   ;;The arguments SUPER-TYPE.ID  and SUB-TYPE.ID must be  type identifiers according
   ;;to TYPE-IDENTIFIER?,  otherwise the behaviour  of this function  is unspecified.
   ;;Return true if  SUPER-TYPE.ID is a super-type of SUB-TYPE.ID  or an operand with
   ;;type SUB-TYPE.ID matches  an argument with type  SUPER-TYPE.ID; otherwise return
   ;;false.
   ;;
   (cond ((~free-identifier=? super-type.id sub-type.id)
	  #t)
	 ((or ($top-tag-id?     super-type.id)
	      ($untyped-tag-id? super-type.id))
	  #t)
	 ((or ($top-tag-id?     sub-type.id)
	      ($untyped-tag-id? sub-type.id))
	  #f)
	 ((procedure-tag-id? super-type.id)
	  (or (predicate-tag-id? sub-type.id)
	      (closure-type-spec? (id->object-type-specification __who__ input-form.stx sub-type.id lexenv))))
	 (else
	  (let ((super-ots (id->object-type-specification __who__ input-form.stx super-type.id lexenv))
		(sub-ots   (id->object-type-specification __who__ input-form.stx sub-type.id   lexenv)))
	    (cond ((list-type-spec? super-ots)
		   (and (list-type-spec? sub-ots)
			(type-identifier-super-and-sub? (list-type-spec.type-id super-ots)
							(list-type-spec.type-id sub-ots)
							lexenv input-form.stx)))
		  ((vector-type-spec? super-ots)
		   (and (vector-type-spec? sub-ots)
			(type-identifier-super-and-sub? (vector-type-spec.type-id super-ots)
							(vector-type-spec.type-id sub-ots)
							lexenv input-form.stx)))
		  (else
		   (let loop ((sub-ots sub-ots))
		     (cond ((object-type-spec.parent-id sub-ots)
			    => (lambda (parent.id)
				 (if ($top-tag-id? parent.id)
				     #f
				   (let ((parent-ots (id->object-type-specification __who__ input-form.stx parent.id lexenv)))
				     (or (eq? super-ots parent-ots)
					 (loop parent-ots))))))
			   (else #f))))))))))

(case-define* type-identifier-common-ancestor
  ;;The arguments ID1 and ID2 must be type identifiers according to TYPE-IDENTIFIER?,
  ;;otherwise the behaviour of this function  is unspecified.  Visit the hierarchy of
  ;;parents of  the given type identifiers,  determine the first common  ancestor and
  ;;return its identifier.
  ;;
  ;;Examples:
  ;;
  ;;   (type-identifier-common-ancestor #'<fixnum> #'<bignum>) => #'<exact-integer>
  ;;   (type-identifier-common-ancestor #'<string> #'<vector>) => #'<top>
  ;;
  ((id1 id2)
   (type-identifier-common-ancestor id1 id2 (current-inferior-lexenv)))
  ((id1 id2 lexenv)
   (type-identifier-common-ancestor id1 id2 lexenv #f))
  ((id1 id2 lexenv input-form.stx)
   (cond ((type-identifier=? id1 id2 lexenv)
	  id1)
	 ((or ($top-tag-id? id1)
	      ($top-tag-id? id2))
	  (top-tag-id))
	 ((or ($untyped-tag-id? id1)
	      ($untyped-tag-id? id2))
	  (untyped-tag-id))
	 (($no-return-tag-id? id1)
	  id2)
	 (($no-return-tag-id? id2)
	  id1)
	 (else
	  (let outer-loop ((type1 id1))
	    (let inner-loop ((type2 id2))
	      (if (type-identifier=? type1 type2 lexenv)
		  type1
		(let ((ots2 (id->object-type-specification __who__ input-form.stx type2 lexenv)))
		  (cond ((object-type-spec.parent-id ots2)
			 => inner-loop)
			(else
			 (let ((ots1 (id->object-type-specification __who__ input-form.stx type1 lexenv)))
			   (cond ((object-type-spec.parent-id ots1)
				  => outer-loop)
				 (else
				  (top-tag-id))))))))))))))

(case-define all-type-identifiers?
  ;;Inspect the argument STX and return true  if it is a syntax object representing a
  ;;proper list of type identifiers; otherwise return false.
  ;;
  ((stx)
   (all-type-identifiers? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx ()
     ((?id . ?rest)
      (and (type-identifier? ?id lexenv)
	   (all-type-identifiers? ?rest lexenv)))
     (() #t)
     (_  #f))))

;;; --------------------------------------------------------------------

(case-define* type-identifier-is-procedure-sub-type?
  ;;The  argument  ID  must  be  a type  identifier  according  to  TYPE-IDENTIFIER?,
  ;;otherwise the behaviour of this function is  unspecified.  Return true if ID is a
  ;;type identifier representing a sub-type of "<procedure>"; otherwise return false.
  ;;If ID is "<procedure>" itself: the return value is false.
  ;;
  ((id)
   (type-identifier-is-procedure-sub-type? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier-is-procedure-sub-type? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(closure-type-spec? (id->object-type-specification __who__ input-form.stx id lexenv)))))

(case-define* type-identifier-is-procedure-or-procedure-sub-type?
  ;;The  argument  ID  must  be  a type  identifier  according  to  TYPE-IDENTIFIER?,
  ;;otherwise the  behaviour of this function  is unspecified.  Return true  if ID is
  ;;the type identifier "<procedure>" or a type identifier representing a sub-type of
  ;;"<procedure>"; otherwise return false.
  ;;
  ((id)
   (type-identifier-is-procedure-or-procedure-sub-type? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier-is-procedure-or-procedure-sub-type? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(or (procedure-tag-id? id)
	    (predicate-tag-id? id)
	    (type-identifier-is-procedure-sub-type? id lexenv input-form.stx)))))

;;; --------------------------------------------------------------------

(case-define* type-identifier-is-list-sub-type?
  ;;The  argument  ID  must  be  a type  identifier  according  to  TYPE-IDENTIFIER?,
  ;;otherwise the behaviour of this function is  unspecified.  Return true if ID is a
  ;;type identifier representing a sub-type  of "<list>"; otherwise return false.  If
  ;;ID is "<list>" itself: the return value is false.
  ;;
  ((id)
   (type-identifier-is-list-sub-type? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier-is-list-sub-type? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(list-type-spec? (id->object-type-specification __who__ input-form.stx id lexenv)))))

(case-define* type-identifier-is-list-or-list-sub-type?
  ;;The  argument  ID  must  be  a type  identifier  according  to  TYPE-IDENTIFIER?,
  ;;otherwise the  behaviour of this function  is unspecified.  Return true  if ID is
  ;;the type  identifier "<list>"  or a  type identifier  representing a  sub-type of
  ;;"<list>"; otherwise return false.
  ;;
  ((id)
   (type-identifier-is-list-or-list-sub-type? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier-is-list-or-list-sub-type? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(or (list-tag-id? id)
	    (type-identifier-is-list-sub-type? id lexenv input-form.stx)))))

;;; --------------------------------------------------------------------

(case-define* type-identifier-is-vector-sub-type?
  ;;The  argument  ID  must  be  a type  identifier  according  to  TYPE-IDENTIFIER?,
  ;;otherwise the behaviour of this function is  unspecified.  Return true if ID is a
  ;;type identifier  representing a sub-type  of "<vector>"; otherwise  return false.
  ;;If ID is "<vector>" itself: the return value is false.
  ;;
  ((id)
   (type-identifier-is-vector-sub-type? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier-is-vector-sub-type? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(vector-type-spec? (id->object-type-specification __who__ input-form.stx id lexenv)))))

(case-define* type-identifier-is-vector-or-vector-sub-type?
  ;;The  argument  ID  must  be  a type  identifier  according  to  TYPE-IDENTIFIER?,
  ;;otherwise the  behaviour of this function  is unspecified.  Return true  if ID is
  ;;the type  identifier "<vector>" or a  type identifier representing a  sub-type of
  ;;"<vector>"; otherwise return false.
  ;;
  ((id)
   (type-identifier-is-vector-or-vector-sub-type? id (current-inferior-lexenv) #f))
  ((id lexenv)
   (type-identifier-is-vector-or-vector-sub-type? id lexenv #f))
  ((id lexenv input-form.stx)
   (and (identifier? id)
	(or (vector-tag-id? id)
	    (type-identifier-is-vector-sub-type? id lexenv input-form.stx)))))


;;;; typed variable with procedure sub-type type utilities
;;
;;The following  functions are used  to deal with  lexical variables, both  local and
;;global,  that  are  typed  with  a  sub-type of  "<procedure>".   It  is  known  at
;;expand-time that such  lexical variables are bound to a  closure object; this means
;;their syntactic binding descriptor has one of the formats:
;;
;;   (lexical-typed         . (#<lexical-typed-variable-spec> . ?expanded-expr))
;;   (global-typed          . (#<library> . ?loc))
;;   (global-typed-mutable  . (#<library> . ?loc))
;;
;;and  ?LOC  is  a  loc  gensym  containing,  in  its  VALUE  slot,  an  instance  of
;;"<global-typed-variable-spec>".
;;
;;The two spec types are sub-types of "<typed-variable-spec>", which has some special
;;fields to represent expand-time properties of closure object's syntactic bindings.
;;

(case-define* typed-procedure-variable.unsafe-variant
  ;;Given  an identifier  representing  a typed,  imported  or non-imported,  lexical
  ;;variable which  is meant to  be bound  to a closure  object: return false  or the
  ;;symbolic expression representing its unsafe variant.
  ;;
  ((id)
   (typed-procedure-variable.unsafe-variant id (current-inferior-lexenv) #f))
  ((id lexenv)
   (typed-procedure-variable.unsafe-variant id lexenv #f))
  ((id lexenv input-form.stx)
   (let ((tvs (id->typed-variable-spec __who__ input-form.stx id lexenv)))
     (if (type-identifier-is-procedure-sub-type? (typed-variable-spec.type-id tvs))
	 (typed-variable-spec.unsafe-variant-sexp tvs)
       (assertion-violation __who__
	 "the type of typed variable is not a sub-type of \"<procedure>\""
	 id tvs)))))

(case-define* typed-procedure-variable.unsafe-variant-set!
  ;;Given an  identifier representing a typed  lexical variable which is  meant to be
  ;;bound to  a closure object: set  the symbolic expression representing  its unsafe
  ;;variant.
  ;;
  ;;When this  function is  called while expanding  code: the ID  is a  local lexical
  ;;typed variable.   When this function is  called while evaluating the  visit code:
  ;;the ID is a global lexical typed variable.
  ;;
  ((id unsafe-variant.sexp)
   (typed-procedure-variable.unsafe-variant-set! id unsafe-variant.sexp (current-inferior-lexenv) #f))
  ((id unsafe-variant.sexp lexenv)
   (typed-procedure-variable.unsafe-variant-set! id unsafe-variant.sexp lexenv #f))
  ((id unsafe-variant.sexp lexenv input-form.stx)
   (let ((tvs (id->typed-variable-spec __who__ input-form.stx id lexenv)))
     (if (type-identifier-is-procedure-sub-type? (typed-variable-spec.type-id tvs))
	 (typed-variable-spec.unsafe-variant-sexp-set! tvs unsafe-variant.sexp)
       (assertion-violation __who__
	 "the type of typed variable is not a sub-type of \"<procedure>\""
	 id tvs)))))


;;;; fabricated procedure type identifiers
;;
;;Let's consider the following code in the C language:
;;
;;   int func (double a, char * b) { ... }
;;   typedef int func_t (double a, char * b);
;;   func_t * the_func = func;
;;
;;we need a pointer to the function "func()",  so we define the type "func_t" as type
;;of the pointer to function "the_func".  We do something similar in Vicare.
;;
;;When the use of a core macro DEFINE/TYPED is expanded from the input form:
;;
;;   (define/typed (?who . ?formals) . ?body)
;;
;;a QRHS is created, then the syntactic identifier ?WHO is bound, finally the QRHS is
;;expanded.  When the syntactic  binding for ?WHO is created: what  is its type?  For
;;sure it must be a sub-type of "<procedure>", but we would like to keep informations
;;about the signature of the closure object definition.
;;
;;Similarly,  when the  use of  a core  macro LAMBDA  or CASE-LAMBDA  is expanded:  a
;;signature  for  the  resulting closure  object  is  built;  it  is an  instance  of
;;"<clambda-signature>".  For example, when the following LAMBDA syntax is expanded:
;;
;;   (lambda ({_ <exact-integer>} {a <fixnum>} {b <fixnum>})
;;     (+ 1 a b))
;;
;;the LAMBDA parser builds the following "<clambda-clause-signature>" struct:
;;
;;   #[<clambda-clause-signature>
;;       retvals=#[<type-signature> tags=(#'<exact-integer>)]
;;       argvals=#[<type-signature> tags=(#'<fixnum> #'<fixnum>)]]
;;
;;To represent  the type of  the closure object: we  create a fresh  type identifier,
;;bound in  the top-level rib with  the syntactic binding's descriptor  stored in the
;;VALUE field of the label gensym.
;;
(define* ({fabricate-closure-type-identifier type-identifier?} {who false-or-symbol?} {signature callable-signature?})
  ;;WHO must be false or a symbol representing the name of the closure object; it can
  ;;be a  random gensym  when no  name is given.   SIGNATURE must  be an  instance of
  ;;"<callable-signature>" or one of its sub-types.
  ;;
  (let* ((type-id.sym  (make-fabricated-closure-type-name who))
	 (type-id.lab  (generate-label-gensym type-id.sym)))
    (receive-and-return (type-id)
	(make-top-level-syntactic-identifier-from-source-name-and-label type-id.sym type-id.lab)
      (let ((spec          (make-closure-type-spec signature))
	    (expanded-expr #f))
	(set-symbol-value! type-id.lab (make-syntactic-binding-descriptor/closure-type-name spec expanded-expr))))))


;;;; list type identifiers

(case-define* fabricate-list-type-identifier
  ;;Given a type identifier return a  type identifier representing a (possibly empty)
  ;;list of items having that type.
  ;;
  ((item-type.id)
   (fabricate-list-type-identifier item-type.id (current-inferior-lexenv) #f))
  ((item-type.id lexenv)
   (fabricate-list-type-identifier item-type.id lexenv #f))
  ((item-type.id lexenv input-form.stx)
   (define (make-fabricated-list-type-name id)
     (gensym (string-append (symbol->string (identifier->symbol id)) "*")))
   (syntax-match item-type.id (<char> <string> <pointer> <symbol>)
     (<char>		(core-prim-id '<char*>))
     (<string>		(core-prim-id '<string*>))
     (<symbol>		(core-prim-id '<symbol*>))
     (<pointer>		(core-prim-id '<pointer*>))
     (else
      (let ((item-ots (id->object-type-specification __who__ input-form.stx item-type.id lexenv)))
	(or (object-type-spec.memoised-list-id item-ots)
	    (cond ((or (top-tag-id?     item-type.id)
		       (untyped-tag-id? item-type.id))
		   (receive-and-return (list-type.id)
		       (list-tag-id)
		     (object-type-spec.memoised-list-id-set! item-ots list-type.id)))
		  (else
		   (let* ((list-type.sym	(make-fabricated-list-type-name item-type.id))
			  (list-type.lab	(generate-label-gensym list-type.sym))
			  (list-ots	(make-list-type-spec item-type.id)))
		     (receive-and-return (list-type.id)
			 (make-top-level-syntactic-identifier-from-source-name-and-label list-type.sym list-type.lab)
		       (set-symbol-value! list-type.lab (make-syntactic-binding-descriptor/list-sub-type-name list-ots #f))
		       (object-type-spec.memoised-list-id-set! item-ots list-type.id)))))))))))


;;;; helpers and utilities

(define (datum-type-signature datum)
  ;;Build and  return a new instance  of "<type-signature>" representing the  type of
  ;;the single value returned by the expression  DATUM, which must be a Scheme object
  ;;extracted from a syntax object representing a literal expression.
  ;;
  (make-type-signature
   (list (cond ((boolean? datum)	(cond (datum
					       (core-prim-id '<true>))
					      (else
					       (core-prim-id '<false>))))
	       ((char?    datum)	(core-prim-id '<char>))
	       ((symbol?  datum)	(core-prim-id '<symbol>))
	       ((keyword? datum)	(core-prim-id '<keyword>))

	       ((fixnum?  datum)	(cond ((fxpositive? datum)
					       (core-prim-id '<positive-fixnum>))
					      ((fxzero? datum)
					       (core-prim-id '<zero-fixnum>))
					      (else
					       (core-prim-id '<fixnum>))))
	       ((flonum?  datum)	(cond ((flpositive? datum)
					       (core-prim-id '<positive-flonum>))
					      ((flzero?/positive datum)
					       (core-prim-id '<positive-zero-flonum>))
					      ((flzero?/negative datum)
					       (core-prim-id '<negative-zero-flonum>))
					      (else
					       (core-prim-id '<flonum>))))
	       ((ratnum?  datum)	(core-prim-id '<ratnum>))
	       ((bignum?  datum)	(core-prim-id '<bignum>))
	       ((compnum? datum)	(cond ((exact-compnum? datum)
					       (core-prim-id '<exact-compnum>))
					      (else
					       (core-prim-id '<compnum>))))
	       ((cflonum? datum)	(core-prim-id '<cflonum>))

	       ((string?  datum)	(core-prim-id '<string>))
	       ((vector?  datum)	(core-prim-id '<vector>))
	       ((bytevector? datum)	(core-prim-id '<bytevector>))

	       ((list?    datum)	(cond ((null? datum)		(core-prim-id '<null>))
					      ((for-all char?   datum)	(core-prim-id '<char*>))
					      ((for-all string? datum)	(core-prim-id '<string*>))
					      ((for-all symbol? datum)	(core-prim-id '<symbol*>))
					      (else			(core-prim-id '<nlist>))))
	       ((pair?    datum)	(cond ((standalone-pair? datum)
					       (core-prim-id '<standalone-pair>))
					      (else
					       (core-prim-id '<pair>))))

	       ((eq? datum (void))	(core-prim-id '<void>))
	       (else			(top-tag-id))))))


;;;; type signature syntaxes predicates

(case-define syntax-object.type-signature?
  ;;Return true  if STX  is a  syntax object  representing the  type signature  of an
  ;;expression's return values; otherwise return false.   The return value is true if
  ;;STX is null or  a proper or improper list of type  identifiers, with a standalone
  ;;type identifier being acceptable if it is "<list>" or one of its sub-types.
  ;;
  ;;Examples:
  ;;
  ;;   (syntax-object.type-signature? #'<list>)				=> #t
  ;;   (syntax-object.type-signature? #'())				=> #t
  ;;   (syntax-object.type-signature? #'(<fixnum> <string>))		=> #t
  ;;   (syntax-object.type-signature? #'(<fixnum> <string> . <list>))	=> #t
  ;;
  ;;A standalone "<list>" identifier means: any number of values of any type.
  ;;
  ((stx)
   (syntax-object.type-signature? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx (<list> <no-return>)
     (() #t)
     ((?id . ?rest)
      (and (type-identifier? ?id lexenv)
	   (syntax-object.type-signature? ?rest lexenv)))
     (<list>
      #t)
     (<no-return>
      #t)
     (?rest
      (identifier? ?rest)
      (type-identifier-is-list-sub-type? ?rest lexenv))
     (_ #f))))

;;; --------------------------------------------------------------------

(define (syntax-object.type-signature.single-identifier? stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature of an expression returning a single value; otherwise return false.
  ;;
  (syntax-match stx ()
    ((?type)	#t)
    (else	#f)))

(define* (syntax-object.type-signature.fully-untyped? stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature of an expression returning any number of values of any types; otherwise
  ;;return false.
  ;;
  ;;In other words, return true if STX is a standalone "<list>".
  ;;
  (list-tag-id? stx))

(define* (syntax-object.type-signature.no-return? stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature  of   an  expression  that,   rather  than  returning,  will   raise  a
  ;;non-continuable exception.
  ;;
  ;;In other words, return true if STX is a standalone "<no-return>".
  ;;
  (no-return-tag-id? stx))

;;; --------------------------------------------------------------------

(case-define* syntax-object.type-signature.partially-untyped?
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature of  an expression's  return values, and  at least one  of the  types is
  ;;unspecified; otherwise return false.
  ;;
  ;;In other words, return true if at least  one type identifier is "<top>" or STX is
  ;;an improper list with "<list>" or one of its sub-types in "improper" position.
  ;;
  ((stx)
   (syntax-object.type-signature.partially-untyped? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (let loop ((stx stx)
	      (rv  #f))
     (syntax-match stx (<top>)
       (() rv)
       ((<top> . ?rest)
	(loop ?rest #t))
       ((?id . ?rest)
	(type-identifier? ?id lexenv)
	(loop ?rest rv))
       (<list>
	#t)
       (?rest
	(and (type-identifier-is-list-sub-type? ?rest lexenv)
	     rv))
       ))))

(case-define* syntax-object.type-signature.untyped?
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.  Return  true if STX  is an "untyped"  type signature: all  the type
  ;;identifiers are either "<top>" or "<list>"; otherwise return false.
  ;;
  ((stx)
   (syntax-object.type-signature.untyped? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (define-syntax-rule (recur ?stx)
     (syntax-object.type-signature.untyped? ?stx lexenv))
   (syntax-match stx (<top> <untyped> <list>)
     (()			#t)
     ((<top>     . ?rest)	(recur ?rest))
     ((<untyped> . ?rest)	(recur ?rest))
     ((?id   . ?rest)		#f)
     (<list>			#t)
     (?rest			#f))))

;;; --------------------------------------------------------------------

(define* (syntax-object.type-signature.min-and-max-count stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.  Return two non-negative real  numbers: the minimum number of values
  ;;that can match  the type signature; the  maximum number of values  that can match
  ;;the type signature, possibly infinite.
  ;;
  (let recur ((stx  stx)
	      (min  0)
	      (max  0))
    (syntax-match stx ()
      (()
       (values min max))
      ((?type . ?rest)
       (recur ?rest (fxadd1 min) (fxadd1 max)))
      (?type
       (values min +inf.0)))))

;;; --------------------------------------------------------------------

(case-define* syntax-object.type-signature.super-and-sub?
  ;;The  arguments   SUPER-SIGNATURE  and   SUB-SIGNATURE  must  be   syntax  objects
  ;;representing   type   signatures  according   to   SYNTAX-OBJECT.TYPE-SIGNATURE?,
  ;;otherwise the behaviour of this function is unspecified.
  ;;
  ;;Return true if: SUPER-SIGNATURE and  SUB-SIGNATURE have compatible structure; the
  ;;type identifiers from  SUPER-SIGNATURE are super-types of  the corresponding type
  ;;identifiers from SUB-SIGNATURE.  Otherwise return false.
  ;;
  ((super-signature sub-signature)
   (syntax-object.type-signature.super-and-sub? super-signature sub-signature (current-inferior-lexenv)))
  ((super-signature sub-signature lexenv)
   (define-syntax-rule (recur ?super ?sub)
     (syntax-object.type-signature.super-and-sub? ?super ?sub lexenv))
   (syntax-match super-signature (<top> <list>)
     (()
      (syntax-match sub-signature ()
	;;Both the signatures are proper lists with the same number of items, and all
	;;the items are correct super and sub: success!
	(() #t)
	;;The signatures do not match.
	(_  #f)))

     ((<top> . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     ((?super-type . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (type-identifier-super-and-sub? ?super-type ?sub-type lexenv)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     (<list>
      ;;The super signature is an improper list accepting any object as rest.
      #t)

     (?super-rest-type
      (type-identifier-is-list-sub-type? ?super-rest-type lexenv)
      (let ((item-id (list-type-spec.type-id (id->object-type-specification __who__ #f ?super-rest-type lexenv))))
	(or (top-tag-id? item-id)
	    (syntax-match sub-signature (<list>)
	      ;;The super  signature is an improper  list with rest item  and the sub
	      ;;signature is finished.  We want the following signatures to match:
	      ;;
	      ;;  super-signature == #'(<number>  <fixnum> . <list>)
	      ;;  sub-signature   == #'(<complex> <fixnum>)
	      ;;
	      ;;because "<list>" in rest position means  any number of objects of any
	      ;;type.
	      (() #t)

	      ;;The  super  signature  is  an  improper list  shorter  than  the  sub
	      ;;signature.  We want the following signatures to match:
	      ;;
	      ;;  super-signature == #'(<number>  . <list-of-fixnums>)
	      ;;  sub-signature   == #'(<complex> <fixnum> <fixnum>)
	      ;;
	      ((?sub-type . ?sub-rest-types)
	       (and (type-identifier-super-and-sub? item-id ?sub-type lexenv)
		    (recur ?super-rest-type ?sub-rest-types)))

	      (<list>
	       ;;Both  the signatures  are improper  lists  with the  same number  of
	       ;;items, and all the items are  correct super and sub.  The rest types
	       ;;are mismatching.
	       #f)

	      ;;Both the signatures are improper lists with the same number of items,
	      ;;and all the  items are correct super  and sub; if the  rest types are
	      ;;proper super and  subs: success!  For example, we  want the following
	      ;;signatures to match:
	      ;;
	      ;;  super-signature == #'(<string> <string> . <list-of-numbers>)
	      ;;  sub-signature   == #'(<string> <string> . <list-of-fixnums>)
	      ;;
	      (?sub-rest-type
	       (type-identifier-is-list-sub-type? ?sub-rest-type lexenv)
	       (type-identifier-super-and-sub? ?super-rest-type ?sub-rest-type lexenv))
	      ))))
     )))

(case-define* syntax-object.type-signature.super-and-sub?/matching
  ;;The  arguments   SUPER-SIGNATURE  and   SUB-SIGNATURE  must  be   syntax  objects
  ;;representing   type   signatures  according   to   SYNTAX-OBJECT.TYPE-SIGNATURE?,
  ;;otherwise the behaviour of this function is unspecified.
  ;;
  ;;Return true if: SUPER-SIGNATURE and  SUB-SIGNATURE have compatible structure; the
  ;;type identifiers  from SUPER-SIGNATURE  match the corresponding  type identifiers
  ;;from SUB-SIGNATURE.  Otherwise return false.
  ;;
  ;;This function can  be used to determine:
  ;;
  ;;*  If the  signature of  a tuple  of arguments  (SUB-SIGNATURE) matches  a LAMBDA
  ;;argvals's signature (SUPER-SIGNATURE).
  ;;
  ;;*  If the  signature of  a  tuple or  return values  (SUB-SIGNATURE) matches  the
  ;;receiver's signature (SUPER-SIGNATURE).
  ;;
  ((super-signature sub-signature)
   (syntax-object.type-signature.super-and-sub?/matching super-signature sub-signature (current-inferior-lexenv)))
  ((super-signature sub-signature lexenv)
   (define-syntax-rule (recur ?super ?sub)
     (syntax-object.type-signature.super-and-sub?/matching ?super ?sub lexenv))
   (syntax-match super-signature (<top> <untyped> <list>)
     (()
      (syntax-match sub-signature ()
	;;Both the signatures are proper lists with the same number of items, and all
	;;the items are correct super and sub: success!
	(() #t)
	;;The signatures do not match.
	(_  #f)))

     ((<top> . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     ((<untyped> . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     ((?super-type . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (type-identifier-super-and-sub? ?super-type ?sub-type lexenv)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     (<list>
      ;;The super signature is an improper list accepting any object as rest.
      #t)

     (?super-rest-type
      (type-identifier-is-list-sub-type? ?super-rest-type lexenv)
      (let ((item-id (list-type-spec.type-id (id->object-type-specification __who__ #f ?super-rest-type lexenv))))
	(or (top-tag-id?     item-id)
	    (untyped-tag-id? item-id)
	    (syntax-match sub-signature (<list>)
	      ;;The super  signature is an improper  list with rest item  and the sub
	      ;;signature is finished.  We want the following signatures to match:
	      ;;
	      ;;  super-signature == #'(<number>  <fixnum> . <list>)
	      ;;  sub-signature   == #'(<complex> <fixnum>)
	      ;;
	      ;;because "<list>" in rest position means  any number of objects of any
	      ;;type.
	      (() #t)

	      ;;The  super  signature  is  an  improper list  shorter  than  the  sub
	      ;;signature.  We want the following signatures to match:
	      ;;
	      ;;  super-signature == #'(<number>  . <list-of-fixnums>)
	      ;;  sub-signature   == #'(<complex> <fixnum> <fixnum>)
	      ;;
	      ((?sub-type . ?sub-rest-types)
	       (and (type-identifier-super-and-sub? item-id ?sub-type lexenv)
		    (recur ?super-rest-type ?sub-rest-types)))

	      (<list>
	       ;;Both  the signatures  are improper  lists  with the  same number  of
	       ;;items, and all the items are  correct super and sub.  The rest types
	       ;;are mismatching.
	       #f)

	      ;;Both the signatures are improper lists with the same number of items,
	      ;;and all the  items are correct super  and sub; if the  rest types are
	      ;;proper super and  subs: success!  For example, we  want the following
	      ;;signatures to match:
	      ;;
	      ;;  super-signature == #'(<string> <string> . <list-of-numbers>)
	      ;;  sub-signature   == #'(<string> <string> . <list-of-fixnums>)
	      ;;
	      (?sub-rest-type
	       (type-identifier-is-list-sub-type? ?sub-rest-type lexenv)
	       (type-identifier-super-and-sub? ?super-rest-type ?sub-rest-type lexenv))
	      ))))
     )))

;;; --------------------------------------------------------------------

(case-define syntax-object.type-signature.common-ancestor
  ;;Given two syntax objects representing type signatures: return a new syntax object
  ;;representing the type signature being their common ancestor.  Examples:
  ;;
  ;;  (syntax-object.type-signature.common-ancestor #'(<fixnum>) #'(<fixnum>))
  ;;  => #'(<fixnum>)
  ;;
  ;;  (syntax-object.type-signature.common-ancestor #'(<fixnum>) #'(<flonum>))
  ;;  => #'(<real>)
  ;;
  ;;  (syntax-object.type-signature.common-ancestor #'(<fixnum>) #'(<string>))
  ;;  => #'(<top>)
  ;;
  ;;NOTE The arguments  SIG1 and SIG2 are  *not* validated here: they  must have been
  ;;previously validated.
  ;;
  ((sig1 sig2)
   (syntax-object.type-signature.common-ancestor sig1 sig2 (current-inferior-lexenv)))
  ((sig1 sig2 lexenv)
   (define-syntax-rule (recur ?sig1 ?sig2)
     (syntax-object.type-signature.common-ancestor ?sig1 ?sig2 lexenv))
   (syntax-match sig1 ()
     (()
      (syntax-match sig2 ()
	;;Both  the signatures  are  proper  lists with  the  same  number of  items:
	;;success!
	(() '())
	;;SIG1 is a proper list shorter that SIG2.
	(_
	 (list-tag-id))))

     ((?type1 . ?rest1)
      (syntax-match sig2 ()
	;;SIG2 is a proper list shorter that SIG1.
	(()
	 (list-tag-id))
	;;SIG1 and SIG2 have matching type identifiers ?TYPE1 and ?TYPE2.
	((?type2 . ?rest2)
	 (cons (type-identifier-common-ancestor ?type1 ?type2 lexenv)
	       (recur ?rest1 ?rest2)))
	;;SIG2 is an improper list shorter that SIG1.
	(?rest2
	 (list-tag-id))))

     (?rest1
      (syntax-match sig2 ()
	;;Both SIG1 and SIG2 are improper lists with the same number of items.
	(?rest2
	 (identifier? ?rest2)
	 (type-identifier-common-ancestor ?rest1 ?rest2 lexenv))
	(_
	 (list-tag-id))))
     )))


;;;; type definition: return values signature

(define-record-type (<type-signature> make-type-signature type-signature?)
  (nongenerative vicare:expander:<type-signature>)
  (fields
   (immutable tags	type-signature-tags)
		;A  fully  unwrapped  syntax  object representing  a  type  signature
		;according to SYNTAX-OBJECT.TYPE-SIGNATURE?.
   (mutable memoised-fully-untyped?	type-signature.memoised-fully-untyped?		type-signature.memoised-fully-untyped?-set!)
   (mutable memoised-partially-untyped?	type-signature.memoised-partially-untyped?	type-signature.memoised-partially-untyped?-set!)
   (mutable memoised-untyped?		type-signature.memoised-untyped?		type-signature.memoised-untyped?-set!)
   (mutable memoised-min-count		type-signature.memoised-min-count		type-signature.memoised-min-count-set!)
		;Memoised minimum number of values matching this signature.
   (mutable memoised-max-count		type-signature.memoised-max-count		type-signature.memoised-max-count-set!)
		;Memoised maximum number of values matching this signature.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-type-signature {tags syntax-object.type-signature?})
	(make-record (syntax-unwrap tags) (void) (void) (void) #f #f))
      make-type-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (sub-printer `(<type-signature> ,(type-signature-tags S))))))

(define <type-signature>-rtd
  (record-type-descriptor <type-signature>))

(define <type-signature>-rcd
  (record-constructor-descriptor <type-signature>))

;;; --------------------------------------------------------------------
;;; special constructors

(let*-syntax
    ((define-cached-signature-maker
       (syntax-rules ()
	 ((_ ?who ?stx-maker)
	  (define ?who
	    (let ((rvs #f))
	      (lambda ()
		(or rvs
		    (receive-and-return (S)
			(make-type-signature ?stx-maker)
		      (set! rvs S)))))))))
     (define-single-type-signature-maker
       (syntax-rules ()
	 ((_ ?who ?type-id-maker)
	  (define-cached-signature-maker ?who (list (?type-id-maker)))))))
  (define-single-type-signature-maker make-type-signature/single-top			top-tag-id)
  (define-single-type-signature-maker make-type-signature/single-void			void-tag-id)
  (define-single-type-signature-maker make-type-signature/single-boolean		boolean-tag-id)
  (define-single-type-signature-maker make-type-signature/single-procedure		procedure-tag-id)
  (define-single-type-signature-maker make-type-signature/single-predicate		predicate-tag-id)
  (define-single-type-signature-maker make-type-signature/single-stx			stx-tag-id)
  (define-single-type-signature-maker make-type-signature/single-syntactic-identifier	syntactic-identifier-tag-id)
  (define-single-type-signature-maker make-type-signature/single-untyped		untyped-tag-id)
  (define-cached-signature-maker make-type-signature/standalone-list			(list-tag-id))
  #| end of LET-SYNTAX |# )

(define-syntax-rule (make-type-signature/fully-untyped)
  (make-type-signature/standalone-list))

(define* (make-type-signature/single-value {type type-identifier?})
  (make-type-signature (list type)))

;;; --------------------------------------------------------------------

(define* (type-signature.fully-untyped? {signature type-signature?})
  ;;Return true  if the type  signature specifies  neither object types,  nor objects
  ;;count; otherwise return false.
  ;;
  (let ((obj (type-signature.memoised-fully-untyped? signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (syntax-object.type-signature.fully-untyped? (type-signature-tags signature))
	  (type-signature.memoised-fully-untyped?-set! signature bool))
      obj)))

(define* (type-signature.partially-untyped? {signature type-signature?})
  ;;Return true if the type signature as at least one untyped item, either "<top>" or
  ;;"<list>"; otherwise return false.
  ;;
  (let ((obj (type-signature.memoised-partially-untyped? signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (syntax-object.type-signature.partially-untyped? (type-signature-tags signature))
	  (type-signature.memoised-partially-untyped?-set! signature bool))
      obj)))

(define* (type-signature.untyped? {signature type-signature?})
  ;;Return  true if  the type  signature  as only  untyped items,  either "<top>"  or
  ;;"<list>"; otherwise return false.
  ;;
  (let ((obj (type-signature.memoised-untyped? signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (syntax-object.type-signature.untyped? (type-signature-tags signature))
	  (type-signature.memoised-untyped?-set! signature bool))
      obj)))

(define* (type-signature.super-and-sub? {super-signature type-signature?}
					{sub-signature   type-signature?})
  ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE  have the same structure and the
  ;;identifiers in  the homologous  position are  super-type and  sub-type; otherwise
  ;;return false.
  ;;
  (syntax-object.type-signature.super-and-sub? (type-signature-tags super-signature)
					       (type-signature-tags sub-signature)))

(define* (type-signature.super-and-sub?/matching {super-signature type-signature?}
						 {sub-signature   type-signature?})
  ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE  have the same structure and the
  ;;identifiers in the homologous position match each other as argument and operands;
  ;;otherwise return false.
  ;;
  (syntax-object.type-signature.super-and-sub?/matching (type-signature-tags super-signature)
							(type-signature-tags sub-signature)))

(define* (type-signature.single-type? {signature type-signature?})
  ;;Return  true if  SIGNATURE represents  a  single return  value; otherwise  return
  ;;false.
  ;;
  (syntax-object.type-signature.single-identifier? (type-signature-tags signature)))

(define* (type-signature.single-top-tag? {signature type-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.
  ;;
  (syntax-match (type-signature-tags signature) (<top>)
    ((<top>)  #t)
    (_        #f)))

(define* (type-signature.single-type-or-fully-untyped? {signature type-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier, otherwise return false.
  ;;
  (syntax-match (type-signature-tags signature) (<list>)
    ((?tag)	#t)
    (<list>	#t)
    (_		#f)))

;;; --------------------------------------------------------------------

(module (type-signature.min-count
	 type-signature.max-count
	 type-signature.min-and-max-counts)

  (define* (type-signature.min-count {signature type-signature?})
    ;;Return a non-negative fixnum representing the minimum number of values that can
    ;;match the signature.
    ;;
    (or (type-signature.memoised-min-count signature)
	(receive (min-count max-count)
	    (type-signature.min-and-max-counts signature)
	  min-count)))

  (define* (type-signature.max-count {signature type-signature?})
    ;;Return a non-negative fixnum representing the maximum number of values that can
    ;;match the signature; if the number of values is infinite: return +inf.0.
    ;;
    (or (type-signature.memoised-max-count signature)
	(receive (min-count max-count)
	    (type-signature.min-and-max-counts signature)
	  max-count)))

  (define (type-signature.min-and-max-counts signature)
    (receive-and-return (min-count max-count)
	(syntax-object.type-signature.min-and-max-count (type-signature-tags signature))
      (type-signature.memoised-min-count-set! signature min-count)
      (type-signature.memoised-max-count-set! signature max-count)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (type-signature=? {signature1 type-signature?} {signature2 type-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (define (%syntax=? stx1 stx2)
    (cond ((and (identifier? stx1)
		(identifier? stx2))
	   (type-identifier=? stx1 stx2))
	  ((and (pair? stx1)
		(pair? stx2))
	   (and (type-identifier=? (car stx1) (car stx2))
		(%syntax=?         (cdr stx1) (cdr stx2))))
	  (else
	   (and (null? stx1)
		(null? stx2)))))
  (%syntax=? (type-signature-tags signature1)
	     (type-signature-tags signature2)))

;;; --------------------------------------------------------------------

(case-define* type-signature.common-ancestor
  ;;Given a  multitude of type signatures:  return a new type  signature representing
  ;;their common ancestor.
  ;;
  (({sig type-signature?})
   sig)

  (({sig1 type-signature?} {sig2 type-signature?})
   (make-type-signature
    (syntax-object.type-signature.common-ancestor (type-signature-tags sig1)
						  (type-signature-tags sig2))))
  (({sig1 type-signature?} {sig2 type-signature?} . sig*)
   (fold-left (lambda (sig-a sig-b)
		(type-signature.common-ancestor sig-a sig-b))
     (type-signature.common-ancestor sig1 sig2)
     sig*)))


;;;; type definition: callable signature

;;This is the  base type of every object  that can be in operator position  in a form
;;like:
;;
;;   (?rator ?rand ...)
;;
;;representing a function application.
;;
(define-record-type (<callable-signature> dummy-make-callable-signature callable-signature?)
  (nongenerative vicare:expander:<callable-signature>)
  (fields
   (immutable retvals	callable-signature.retvals)
		;An instance of "<type-signature>".
		;
		;For the "<clambda-signature>" sub-type it represents the signature of
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


;;;; type definition: CASE-LAMBDA clause signature

(define-record-type (<clambda-clause-signature> make-clambda-clause-signature clambda-clause-signature?)
  (nongenerative vicare:expander:<clambda-clause-signature>)
  (fields
   (immutable retvals	clambda-clause-signature.retvals)
		;An instance of "<type-signature>"  representing the signature of the
		;return values.
   (immutable argvals	clambda-clause-signature.argvals)
		;An instance of "<type-signature>"  representing the signature of the
		;argument values.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-clambda-clause-signature {retvals type-signature?} {argvals type-signature?})
	(make-record retvals argvals))
      make-clambda-clause-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (sub-printer `(<clambda-clause-signature>
		     (:retvals ,(type-signature-tags (clambda-clause-signature.retvals S)))
		     (:argvals ,(type-signature-tags (clambda-clause-signature.argvals S))))))))

(define (not-empty-list-of-clambda-clause-signatures? obj)
  (and (pair? obj)
       (and (clambda-clause-signature? (car obj))
	    (let loop ((obj (cdr obj)))
	      (if (pair? obj)
		  (and (clambda-clause-signature? (car obj))
		       (loop (cdr obj)))
		(null? obj))))))

(define* (clambda-clause-signature.argvals.tags {signature clambda-clause-signature?})
  (type-signature-tags (clambda-clause-signature.argvals signature)))

(define* (clambda-clause-signature.retvals.tags {signature clambda-clause-signature?})
  (type-signature-tags (clambda-clause-signature.retvals signature)))

;;; --------------------------------------------------------------------

(define* (clambda-clause-signature=? {signature1 clambda-clause-signature?} {signature2 clambda-clause-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (and (type-signature=? (clambda-clause-signature.argvals signature1)
			 (clambda-clause-signature.argvals signature2))
       (type-signature=? (clambda-clause-signature.retvals signature1)
			 (clambda-clause-signature.retvals signature2))))

;;; --------------------------------------------------------------------

(define* (clambda-clause-signature.fully-untyped? {clause-signature clambda-clause-signature?})
  ;;A  clambda clause  signature  has fully  unspecified types  if  its retvals  type
  ;;signature  is the  standalone  "<list>" and  its argvals  type  signature is  the
  ;;standalone "<list>".
  ;;
  (and (type-signature.fully-untyped? (clambda-clause-signature.argvals clause-signature))
       (type-signature.fully-untyped? (clambda-clause-signature.retvals clause-signature))))

(define* (clambda-clause-signature.untyped? {clause-signature clambda-clause-signature?})
  ;;A  clambda  clause has  "untyped"  signature  if  both  its argvals  and  retvals
  ;;signatures only use "<top>" and "<list>" as type identifiers.
  ;;
  (and (type-signature.untyped? (clambda-clause-signature.argvals clause-signature))
       (type-signature.untyped? (clambda-clause-signature.retvals clause-signature))))


;;;; type definition: CLAMBDA signature

;;Type representing the full type signature of closure objects.
;;
(define-record-type (<clambda-signature> make-clambda-signature clambda-signature?)
  (nongenerative vicare:expander:<clambda-signature>)
  (parent <callable-signature>)
  (fields
   (immutable clause-signature* clambda-signature.clause-signature*)
		;A proper list of "<clambda-clause-signature>" instances representing
		;the signatures of the CASE-LAMBDA clauses.
   (mutable memoised-min-count	clambda-signature.memoised-min-count	clambda-signature.memoised-min-count-set!)
   (mutable memoised-max-count	clambda-signature.memoised-max-count	clambda-signature.memoised-max-count-set!)
   #| end of FIELDS |# )
  (protocol
    (lambda (make-callable-signature)
      (define* (make-clambda-signature {signature* not-empty-list-of-clambda-clause-signatures?})
	((make-callable-signature (apply type-signature.common-ancestor (map clambda-clause-signature.retvals signature*)))
	 signature* #f #f))
      make-clambda-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (sub-printer `(<clambda-signature>
		     (:common-retvals ,(callable-signature.retvals S))
		     (:clause-signatures . ,(clambda-signature.clause-signature* S)))))))

(define* (clambda-signature.retvals {sig clambda-signature?})
  (callable-signature.retvals sig))

(define* (clambda-signature.min-and-max-argvals {sig clambda-signature?})
  ;;Return two non-negative real numbers  representing the minimum and maximum number
  ;;of values that can match the argvals type signatures of all the clauses.
  ;;
  (cond ((clambda-signature.memoised-min-count sig)
	 => (lambda (min-count)
	      (values min-count (clambda-signature.memoised-max-count sig))))
	(else
	 (let ((clause-signature* (clambda-signature.clause-signature* sig)))
	   (receive (min-count max-count)
	       (if (and (pair? clause-signature*)
			(null? (cdr clause-signature*)))
		   ;;There is only one clause signature.
		   (type-signature.min-and-max-counts (clambda-clause-signature.argvals (car clause-signature*)))
		 ;;There are two or more clause signatures.
		 (let ((cnts (fold-left
				 (lambda (knil clause-signature)
				   (receive (min-count max-count)
				       (type-signature.min-and-max-counts (clambda-clause-signature.argvals clause-signature))
				     (cons (min (car knil) min-count)
					   (max (cdr knil) max-count))))
			       (receive (min-count max-count)
				   (type-signature.min-and-max-counts (clambda-clause-signature.argvals (car clause-signature*)))
				 (cons min-count max-count))
			       (cdr clause-signature*))))
		   (values (car cnts) (cdr cnts))))
	     (clambda-signature.memoised-min-count-set! sig min-count)
	     (clambda-signature.memoised-max-count-set! sig max-count)
	     (values min-count max-count))))))


;;;; tagged binding parsing: standalone identifiers

(define (syntax-object.typed-argument? stx)
  ;;Return true if STX is a syntax object representing a typed or untyped identifier;
  ;;otherwise return false.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (type-identifier? ?tag)))
    (?id
     (identifier? ?id))))

(case-define* syntax-object.parse-typed-argument
  ;;If  STX  is a  typed  or  untyped identifier,  return  2  values: the  identifier
  ;;representing the syntactic binding name and the identifier representing the type;
  ;;otherwise  raise an  exception.  When  no type  is present:  the type  identifier
  ;;defaults to "<top>".
  ;;
  ((stx)
   (syntax-object.parse-typed-argument stx (current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx (brace)
     ((brace ?id ?tag)
      (begin
	;;We retrieve the object-type specification to  validate ?TAG: if ?TAG is not
	;;a type identifier, an exception is raised.
	(id->object-type-specification __who__ stx ?tag lexenv)
	(values ?id ?tag)))
     (?id
      (identifier? ?id)
      (values ?id (untyped-tag-id))))))


;;;; standard binding parsing: proper lists of bindings left-hand sides

(case-define* syntax-object.parse-standard-list-of-bindings
  ;;Parser function  for lists of standard  syntactic bindings.  It is  used to parse
  ;;bindings from  LET, DO  and similar  syntaxes.  For  example, when  expanding the
  ;;syntax:
  ;;
  ;;   (let ((a 1)
  ;;         (b "b")
  ;;         (c #t))
  ;;     . ?body)
  ;;
  ;;the argument BINDING* is:
  ;;
  ;;   (#'a #'b #'c)
  ;;
  ;;and the return value is:
  ;;
  ;;   (#'a #'b #'c)
  ;;
  ;;Assume BINDING* is a syntax object representing a proper list of standard binding
  ;;identifiers; parse  the list and a  list of identifiers representing  the binding
  ;;identifiers.  The identifiers must be distinct.
  ;;
  ((binding*)
   (syntax-object.parse-standard-list-of-bindings binding* #f))
  ((binding* input-form.stx)
   (define (%error message)
     (syntax-violation __who__ message (or input-form.stx binding*) (if input-form.stx binding* #f)))
   (define lexenv
     (current-inferior-lexenv))
   (receive-and-return (id*)
       (let recur ((bind* binding*))
	 (syntax-match bind* (brace)
	   (() '())
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (cons ?id (recur ?other-id*)))
	   (_
	    (%error "invalid standard syntactic bindings syntax"))))
     (unless (distinct-bound-ids? id*)
       (%error "duplicate identifiers in syntactic bindings specification")))))


;;;; tagged binding parsing: proper lists of bindings left-hand sides

(case-define* syntax-object.parse-typed-list-of-bindings
  ;;Parser  function for  lists of  typed syntactic  bindings.  It  is used  to parse
  ;;bindings from  LET, DO  and similar  syntaxes.  For  example, when  expanding the
  ;;syntax:
  ;;
  ;;   (let (({a <fixnum>} 1)
  ;;         ({b <string>} "b")
  ;;         (c            #t))
  ;;     . ?body)
  ;;
  ;;the argument BINDING* is:
  ;;
  ;;   (#'(brace a <fixnum>) #'(brace b <string>) #'c)
  ;;
  ;;and the return values are:
  ;;
  ;;   (#'a #'b #'c) (#'<fixnum> #'<string> #'<top>)
  ;;
  ;;Assume BINDING* is a syntax object  representing a proper list of possibly tagged
  ;;binding identifiers;  parse the list and  return 2 values: a  list of identifiers
  ;;representing the binding identifiers, a list of identifiers representing the type
  ;;tags; "<top>" is used when no tag is present.  The identifiers must be distinct.
  ;;
  ((binding*)
   (syntax-object.parse-typed-list-of-bindings binding* #f))
  ((binding* input-form.stx)
   (define (%error message)
     (syntax-violation __who__ message (or input-form.stx binding*) (if input-form.stx binding* #f)))
   (define lexenv
     (current-inferior-lexenv))
   (receive-and-return (id* tag*)
       (let recur ((bind* binding*))
	 (syntax-match bind* (brace)
	   (()
	    (values '() '()))
	   (((brace ?id ?tag) . ?other-id*)
	    (begin
	      (id->object-type-specification __who__ input-form.stx ?tag lexenv)
	      (receive (id* tag*)
		  (recur ?other-id*)
		(values (cons ?id id*) (cons ?tag tag*)))))
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (receive (id* tag*)
		(recur ?other-id*)
	      (values (cons ?id id*) (cons (untyped-tag-id) tag*))))
	   (_
	    (%error "invalid tagged bindings syntax"))))
     (unless (distinct-bound-ids? id*)
       (%error "duplicate identifiers in bindings specification")))))


;;;; standard binding parsing: standard LAMBDA formals

(define* (syntax-object.parse-standard-formals formals.stx input-form.stx)
  ;;Parse the  given syntax  object as standard  (untyped) LET-VALUES  formals (these
  ;;formals are  equal to the ones  of standard lambda clauses).   Test for duplicate
  ;;bindings.  If the syntax is invalid: raise an exception.
  ;;
  ;;When successful return the following values:
  ;;
  ;;1. A  proper or improper list  of identifiers representing the  standard formals.
  ;;It is the argument FORMALS.STX itself, but fully unwrapped.
  ;;
  ;;2.   A  syntax   object   representing   the  type   signature   as  defined   by
  ;;SYNTAX-OBJECT.TYPE-SIGNATURE?.
  ;;
  ;;NOTE We return two values (including FORMALS.STX  itself) to make the API of this
  ;;function equal to  the one of SYNTAX-OBJECT.PARSE-TYPED-FORMALS, so  that the two
  ;;can be used as:
  ;;
  ;;   (if (options::strict-r6rs)
  ;;       (syntax-object.parse-standard-formals formals.stx input-form.stx)
  ;;     (syntax-object.parse-typed-formals formals.stx input-form.stx))
  ;;
  ;;it makes the code simpler to read.  (Marco Maggi; Wed Feb  3, 2016)
  ;;
  (define-syntax __func_who__
    (identifier-syntax (quote syntax-object.parse-standard-formals)))
  (define (%synner message subform)
    (syntax-violation __func_who__ message input-form.stx subform))
  (define (%one-untyped-for-each item*)
    (map (lambda (x) (untyped-tag-id)) item*))
  (define (%validate-standard-formals standard-formals.stx %synner)
    (cond ((duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(%synner "duplicate identifiers in formals specification" duplicate-id)))))
  (syntax-match formals.stx (brace)
    (?args-id
     (identifier? ?args-id)
     (values ?args-id (list-tag-id)))

    ((?arg* ...)
     (for-all identifier? ?arg*)
     (begin
       (%validate-standard-formals ?arg* %synner)
       (values ?arg* (%one-untyped-for-each ?arg*))))

    ((?arg* ... . ?rest-id)
     (and (for-all identifier? ?arg*)
	  (identifier? ?rest-id))
     (begin
       (%validate-standard-formals (append ?arg* ?rest-id) %synner)
       ;;These APPEND applications return an improper list.
       (values (append ?arg* ?rest-id)
	       (append (%one-untyped-for-each ?arg*) (list-tag-id)))))

    (_
     (%synner "invalid standard formals specification" formals.stx))))

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


;;;; tagged binding parsing: typed LAMBDA formals

(module (syntax-object.parse-typed-formals)
  ;;Parse a  syntax object as  possibly typed  LET-VALUES formals (these  formals are
  ;;different from  the one  of lambda  clauses because they  have no  return values'
  ;;types).   Test for  duplicate  bindings.   If the  syntax  is  invalid: raise  an
  ;;exception.
  ;;
  ;;When successful return the following values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2.   A  syntax   object   representing   the  type   signature   as  defined   by
  ;;SYNTAX-OBJECT.TYPE-SIGNATURE?.
  ;;
  (define-module-who syntax-object.parse-typed-formals)

  (define (syntax-object.parse-typed-formals formals.stx input-form.stx)
    (define (%synner message subform)
      (syntax-violation __module_who__ message input-form.stx subform))
    (syntax-match formals.stx (brace)

      ;;Non-standard formals: typed args, as in: (lambda (brace args <list>) ---)
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (if (type-identifier-is-list-or-list-sub-type? ?args-tag)
	   (values ?args-id ?args-tag)
	 (%synner "expected \"<list>\" or its sub-type as type identifier for the args argument" formals.stx)))

      ;;Standard formals, UNtyped args as in: (lambda args ---)
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (list-tag-id)))

      ;;Non-standard formals: possibly typed arguments with typed rest argument.
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (begin
		 (unless (and (identifier? ?rest-id)
			      (identifier? ?rest-tag))
		   (%synner "invalid rest argument specification" (list (brace-id) ?rest-id ?rest-tag)))
		 (unless (type-identifier-is-list-or-list-sub-type? ?rest-tag)
		   (%synner "expected \"<list>\" or its sub-type as type identifier for the rest argument"
			    (list (brace-id) ?rest-id ?rest-tag)))
		 (values ?rest-id ?rest-tag))))
	 (%validate-standard-formals standard-formals.stx %synner)))

      ;;Standard formals: UNtyped identifiers without rest argument.
      ((?arg* ...)
       (for-all identifier? ?arg*)
       (begin
	 (%validate-standard-formals ?arg* %synner)
	 (values ?arg* (%one-untyped-for-each ?arg*))))

      ;;Standard formals: UNtyped identifiers with UNtyped rest argument.
      ((?arg* ... . ?rest-id)
       (and (for-all identifier? ?arg*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-standard-formals (append ?arg* ?rest-id) %synner)
	 (values formals.stx (append (%one-untyped-for-each ?arg*) (list-tag-id)))))

      ;;Non-standard formals: possibly typed identifiers with UNtyped rest argument.
      ((?arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (if (identifier? ?rest-id)
		   (values ?rest-id (list-tag-id))
		 (%synner "invalid rest argument specification" ?rest-id))))
	 (%validate-standard-formals standard-formals.stx %synner)))

      ;;Non-standard formals: possibly typed identifiers without rest argument.
      ;;
      ((?arg* ...)
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (values '() '())))
	 (%validate-standard-formals standard-formals.stx %synner)))

      (_
       (%synner "invalid formals specification" formals.stx))))

  (define (%process-arg* arg*.stx process-next-arg input-form.stx %synner)
    (receive (standard-formals.stx type-signature.stx)
	(process-next-arg (cdr arg*.stx))
      (let ((arg.stx (car arg*.stx)))
	(syntax-match arg.stx (brace)
	  ;;Untyped argument.
	  (?id
	   (identifier? ?id)
	   (values (cons ?id standard-formals.stx) (cons (untyped-tag-id) type-signature.stx)))
	  ;;Typed argument.
	  ((brace ?id ?tag)
	   (and (identifier? ?id)
		(identifier? ?tag))
	   (begin
	     (id->object-type-specification __module_who__ input-form.stx ?tag (current-inferior-lexenv))
	     (values (cons ?id standard-formals.stx) (cons ?tag type-signature.stx))))
	  (else
	   (%synner "invalid argument specification" arg.stx))))))

  (define (%validate-standard-formals standard-formals.stx %synner)
    (cond ((duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(%synner "duplicate identifiers in formals specification" duplicate-id)))))

  (define (%one-untyped-for-each item*)
    (map (lambda (x) (untyped-tag-id)) item*))

  #| end of module |# )

(define (syntax-object.typed-formals? formals.stx)
  ;;Return true  if FORMALS.STX is a  syntax object representing valid  typed formals
  ;;for a LAMBDA or LET-VALUES syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals.stx formals-signature.stx)
	(syntax-object.parse-typed-formals formals.stx #f)
      #t)))


;;;; standard formals parsing

(define* (syntax-object.parse-standard-clambda-clause-formals formals.stx input-form.stx)
  ;;Given a syntax object parse it as  standard lambda formals; do test for duplicate
  ;;bindings.   Return   the  argument   FORMALS.STX  itself   and  an   instance  of
  ;;"<clambda-clause-signature>".
  ;;
  (receive (standard-formals.stx clause-signature.stx)
      (syntax-object.parse-standard-formals formals.stx input-form.stx)
    (values standard-formals.stx
	    (make-clambda-clause-signature (make-type-signature/fully-untyped)
					   (make-type-signature clause-signature.stx)))))

(define (syntax-object.parse-standard-clambda-multi-clauses-formals input-formals*.stx input-form.stx)
  ;;Given a list of syntax objects  INPUT-FORMALS*.STX: parse them as clambda clauses
  ;;standard  formals;  do   test  for  duplicate  bindings.    Return  the  argument
  ;;INPUT-FORMALS*.STX itself and a list of "<clambda-clause-signature>" instances.
  ;;
  (let recur ((input-formals*.stx input-formals*.stx))
    (if (pair? input-formals*.stx)
	(receive (standard-formals.stx clause-signature)
	    (syntax-object.parse-standard-clambda-clause-formals (car input-formals*.stx) input-form.stx)
	  (receive (standard-formals*.stx clause-signature*)
	      (recur (cdr input-formals*.stx))
	    (values (cons standard-formals.stx standard-formals*.stx)
		    (cons clause-signature     clause-signature*))))
      (values '() '()))))

(define* (syntax-object.standard-clambda-clause-formals? formals.stx)
  ;;Return true if FORMALS.STX is a syntax object representing valid standard formals
  ;;for a LAMBDA or LET-VALUES syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (syntax-object.parse-standard-formals formals.stx #f)
    #t))


;;;; tagged binding parsing: callable signature

(define (syntax-object.parse-typed-clambda-clause-formals callable-signature.stx input-form.stx)
  ;;Given a  syntax object  representing a  typed callable  spec: split  the standard
  ;;formals  from the  type  signature; do  test for  duplicate  bindings.  Return  2
  ;;values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2. An instance of "<clambda-clause-signature>".
  ;;
  ;;This function *does*  enforce the constraint: the identifiers  in type identifier
  ;;positions must  actually be type  identifiers (with syntactic  binding descriptor
  ;;already added to the LEXENV).
  ;;
  (define-syntax __func_who__
    (identifier-syntax (quote syntax-object.parse-typed-clambda-clause-formals)))
  (define (%synner message subform)
    (syntax-violation __func_who__ message input-form.stx subform))
  (syntax-match callable-signature.stx (brace)
    ;;With return values tagging.
    (((brace ?who ?rv-tag* ... . ?rv-rest-tag) . ?formals)
     (underscore-id? ?who)
     (let ((retvals-signature.stx (append ?rv-tag*
					  ;;We want a proper  list when possible, not
					  ;;an improper  list with the  syntax object
					  ;;#'() as tail.
					  (syntax-match ?rv-rest-tag ()
					    (() '())
					    (_  ?rv-rest-tag)))))
       (unless (syntax-object.type-signature? retvals-signature.stx)
	 (%synner "invalid syntax for return values' signature" retvals-signature.stx))
       (receive (standard-formals.stx formals-signature.stx)
	   (syntax-object.parse-typed-formals ?formals input-form.stx)
	 (values standard-formals.stx
		 (make-clambda-clause-signature (make-type-signature retvals-signature.stx)
						(make-type-signature formals-signature.stx))))))
    ;;Without return values tagging.
    (?formals
     (receive (standard-formals.stx formals-signature.stx)
	 (syntax-object.parse-typed-formals ?formals input-form.stx)
       (values standard-formals.stx
	       (make-clambda-clause-signature (make-type-signature/fully-untyped)
					      (make-type-signature formals-signature.stx)))))))

(define (syntax-object.parse-typed-clambda-multi-clauses-formals input-formals*.stx input-form.stx)
  ;;Given a list of syntax objects  INPUT-FORMALS*.STX: parse them as clambda clauses
  ;;typed formals;  do test for duplicate  bindings.  Return a list  of syntax object
  ;;representing  the standard  formals  and a  list of  "<clambda-clause-signature>"
  ;;instances.
  ;;
  (let recur ((input-formals*.stx input-formals*.stx))
    (if (pair? input-formals*.stx)
	(receive (standard-formals.stx clause-signature)
	    (syntax-object.parse-typed-clambda-clause-formals (car input-formals*.stx) input-form.stx)
	  (receive (standard-formals*.stx clause-signature*)
	      (recur (cdr input-formals*.stx))
	    (values (cons standard-formals.stx standard-formals*.stx)
		    (cons clause-signature     clause-signature*))))
      (values '() '()))))

(define* (syntax-object.typed-clambda-clause-formals? formals.stx)
  ;;Return true if  FORMALS.STX is a syntax object representing  valid tagged formals
  ;;for a LAMBDA syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature-tags)
	(syntax-object.parse-typed-clambda-clause-formals formals.stx #f)
      #t)))


;;;; done

#| end of module: PSYNTAX-TYPE-IDENTIFIERS-AND-SIGNATURES |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'set-identifier-tag-type-spec!		'scheme-indent-function 1)
;; eval: (put '$record-type-printer-set!		'scheme-indent-function 1)
;; End:
