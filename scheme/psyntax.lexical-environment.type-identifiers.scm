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
	(or (procedure-type-id? id)
	    (predicate-type-id? id)
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
	(typed-list-type-spec? (id->object-type-specification __who__ input-form.stx id lexenv)))))

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
	(or (list-type-id? id)
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
	(or (vector-type-id? id)
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
	 (cond ((typed-list-type-spec? ots1)
		(and (typed-list-type-spec? ots2)
		     (type-identifier=? (typed-list-type-spec.type-id ots1)
					(typed-list-type-spec.type-id ots2)
					lexenv)))
	       ((typed-vector-type-spec? ots1)
		(and (typed-vector-type-spec? ots2)
		     (type-identifier=? (typed-vector-type-spec.type-id ots1)
					(typed-vector-type-spec.type-id ots2)
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
	 (($top-type-id? super-type.id)
	  #t)
	 (($top-type-id? sub-type.id)
	  #f)
	 ((procedure-type-id? super-type.id)
	  (or (predicate-type-id? sub-type.id)
	      (closure-type-spec? (id->object-type-specification __who__ input-form.stx sub-type.id lexenv))))
	 (else
	  (let ((super-ots (id->object-type-specification __who__ input-form.stx super-type.id lexenv))
		(sub-ots   (id->object-type-specification __who__ input-form.stx sub-type.id   lexenv)))
	    (cond ((typed-list-type-spec? super-ots)
		   (and (typed-list-type-spec? sub-ots)
			(type-identifier-super-and-sub? (typed-list-type-spec.type-id super-ots)
							(typed-list-type-spec.type-id sub-ots)
							lexenv input-form.stx)))
		  ((typed-vector-type-spec? super-ots)
		   (and (typed-vector-type-spec? sub-ots)
			(type-identifier-super-and-sub? (typed-vector-type-spec.type-id super-ots)
							(typed-vector-type-spec.type-id sub-ots)
							lexenv input-form.stx)))
		  (else
		   (let loop ((sub-ots sub-ots))
		     (cond ((object-type-spec.parent-id sub-ots)
			    => (lambda (parent.id)
				 (if ($top-type-id? parent.id)
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
	 ((or ($top-type-id? id1)
	      ($top-type-id? id2))
	  (top-type-id))
	 (($no-return-type-id? id1)
	  id2)
	 (($no-return-type-id? id2)
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
				  (top-type-id))))))))))))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
