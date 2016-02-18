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


(module PSYNTAX-TYPE-IDENTIFIERS
    (
;;; predicates
     type-identifier?					all-type-identifiers?
     type-identifier-is-procedure-sub-type?		type-identifier-is-procedure-or-procedure-sub-type?
     type-identifier-is-list-sub-type?			type-identifier-is-list-or-list-sub-type?
     type-identifier-is-vector-sub-type?		type-identifier-is-vector-or-vector-sub-type?

;;; comparison
     type-identifier=?					type-identifier-super-and-sub?

;;; inspection
     type-identifier-common-ancestor
     typed-procedure-variable.unsafe-variant		typed-procedure-variable.unsafe-variant-set!

     #| end of exports |# )


;;;; type identifiers: predicates

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
			((core-object-type-name local-object-type-name global-object-type-name)
			 #t)
			((displaced-lexical)
			 (raise
			  (condition (make-who-condition __who__)
				     (make-message-condition "identifier out of context (identifier's label not in LEXENV)")
				     (make-syntax-violation input-form.stx id)
				     (make-syntactic-binding-descriptor-condition descr))))
			(else #f)))))
	      (else #f)))))

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


;;;; type identifiers: comparison

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
	 (($top-tag-id? sub-type.id)
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


;;;; type identifiers: inspection

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


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
