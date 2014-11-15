;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tables of core primitive properties
;;;Date: Sat Sep 20, 2014
;;;
;;;Abstract
;;;
;;;   This file  contains a  table of  core primitive  properties for  both primitive
;;;   functions and primitive operations.
;;;
;;;
;;;Core type signatures
;;;--------------------
;;;
;;;Used by  the compiler  passes "core type  inference", "introduce  unsafe primitive
;;;references" and "specify representation" to:
;;;
;;;*  Validate the  input  code, detecting  operands  of invalid  core  type in  core
;;;primitive  applications.
;;;
;;;*  Substitute  the  application  of   safe  primitives  with  application  of  the
;;;corresponding unsafe  primitives, whenever  it is known  at compile-time  that the
;;;operands are of the correct type.
;;;
;;;*  Generate  optimised  integrated  code  to implement  the  application  of  core
;;;primitive operations.
;;;
;;;Core  type  signatures  are   specified  via  the  ?CORE-SIGNATURES-SPEC  symbolic
;;;expression:
;;;
;;;   ?core-signatures-spec  = (signatures ?sign-spec ...)
;;;   ?sign-spec             = (?arguments-spec => ?return-values-spec)
;;;   ?arguments-spec        = (?core-type-tag ... . ())
;;;                          | (?core-type-tag ... . ?core-type-tag)
;;;   ?return-values-spec    = (?core-type-tag ... . ())
;;;                          | (?core-type-tag ... . ?core-type-tag)
;;;
;;;where SIGNATURES is an auxiliary syntax and ?CORE-TYPE-TAG is an identifier among:
;;;"T:object", "T:fixnum", ..., with "_" acting as wildcard equivalent to "T:object".
;;;
;;;Whenever a  core type signature  is examined by  the compiler: the  ?SIGN-SPEC are
;;;considered in the order  in which they appear in the  declaration (left to right),
;;;stopping at the first that matches the operands.
;;;
;;;Example:
;;;
;;;   (signatures
;;;	((T:fixnum)	=> (T:true))
;;;	((_)		=> (T:boolean)))
;;;
;;;this ?CORE-SIGNATURES-SPEC specifies that:
;;;
;;;* When the core primitive is applied  to a single operand of type "T:fixnum" there
;;;is a single return value of type "T:true".
;;;
;;;* When the core primitive is applied  to a single operand of unspecified type (but
;;;not "T:fixnum") there is a single return value of type "T:boolean".
;;;
;;;
;;;Core primitive attributes
;;;-------------------------
;;;
;;;Used  by  the  source  optimiser  to  precompute  the  result  of  core  primitive
;;;applications.   A tuple  of  attributes  can be  specified  for  a core  primitive
;;;invocation with  selected number  of operands;  if a primitive  can be  applied to
;;;different  numbers  of  operands,  each  arity  can  have  a  different  tuple  of
;;;attributes.
;;;
;;;Attributes are specified via the ?ATTRIBUTES-SPEC symbolic expression:
;;;
;;;   ?attributes-spec  = (attributes ?attr-spec)
;;;   ?attr-spec        = (?attr-signature . ?attr-tuple)
;;;   ?attr-signature   = (?operand-spec ... . ?operand-spec)
;;;   ?operand-spec     = _ | 0 | #f | ()
;;;   ?attr-tuple       = (?attr-symbol ...)
;;;   ?attr-symbol      = effect-free | foldable | result-true | result-false
;;;
;;;where ATTRIBUTES is an auxiliary syntax and the attributes are the symbols:
;;;
;;;   effect-free -  The application produces no side effects.
;;;
;;;   foldable -     The application can be precomputed at compile time.
;;;
;;;   result-true -  The application always has non-#f result.
;;;
;;;   result-false - The application always has #f result.
;;;
;;;The ?OPERAND-SPEC can be the symbol "_"  to represent any operand or one among: 0,
;;;#f, () to represent an operand that is known at compile-time to be such datum.
;;;
;;;Example:
;;;
;;;   (attributes
;;;     (()		foldable)
;;;     ((_)		effect-free)
;;;     ((_ _)		result-true)
;;;     ((_ _ _ . _)    result-false))
;;;
;;;this ?ATTRIBUTES-SPEC specifies that:
;;;
;;;* The core primitive can be applied to 0, 1, 2, 3 or more operands.
;;;
;;;* When the number of operands is 0: the application is foldable.
;;;
;;;* When the number of operands is 1: the application has no side effects.
;;;
;;;* When the number of operands is 2: the application always returns non-false.
;;;
;;;* When the number of operands is 3 or more: the application always returns false.
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; syntax helpers

(define-syntax /section
  (syntax-rules ()))

(define-syntax section
  ;;By enclosing code in:
  ;;
  ;;   (section ?body ... /section)
  ;;
  ;;we can comment out a section by just commenting out the form:
  ;;
  ;;   #;(section ?body ... /section)
  ;;
  ;;This is sometimes useful when debugging.
  ;;
  (syntax-rules (/section)
    ((section ?body ... /section)
     (begin ?body ...))))


;;;; syntax helpers: predicates

(define-syntax declare-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate fixnum? T:fixnum)
  ;;   (declare-type-predicate vector? T:vector)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((?obj-tag)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ))

;;; --------------------------------------------------------------------

(module (define-object-predicate-declarer)

  (define-syntax define-object-predicate-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-predicate-declarer declare-number-predicate T:number)
    ;;   (declare-number-predicate zero?)
    ;;   (declare-number-predicate positive?)
    ;;   (declare-number-predicate negative?)
    ;;
    (syntax-rules ()
      ((_ ?declarer ?obj-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-predicate ?who ?obj-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-predicate ?who ?obj-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-predicate ?who ?obj-tag unsafe (replacements)))

	   ((_ ?who		(replacements . ?replacements))	(%define-predicate ?who ?obj-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe	(replacements . ?replacements))	(%define-predicate ?who ?obj-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe	(replacements . ?replacements))	(%define-predicate ?who ?obj-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-predicate
    (syntax-rules (replacements)
      ((_ ?who ?obj-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?obj-tag)		=> (T:true))
	  ((_)			=> (T:boolean)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-PREDICATE-DECLARER |# )

(define-object-predicate-declarer declare-object-predicate T:object)
(define-object-predicate-declarer declare-number-predicate T:number)
(define-object-predicate-declarer declare-fixnum-predicate T:fixnum)
(define-object-predicate-declarer declare-ratnum-predicate T:ratnum)
(define-object-predicate-declarer declare-bignum-predicate T:bignum)
(define-object-predicate-declarer declare-flonum-predicate T:flonum)


;;;; syntax helpers: comparison functions

(module (define-object-binary-comparison-declarer)

  (define-syntax define-object-binary-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-comparison-declarer declare-object-binary-comparison _)
    ;;   (declare-object-binary-comparison eq?)
    ;;   (declare-object-binary-comparison eqv?)
    ;;   (declare-object-binary-comparison equal?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)	=> (T:boolean)))
	 (attributes
	  ((_ _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-COMPARISON-DECLARER |# )

(define-object-binary-comparison-declarer declare-object-binary-comparison T:object)
(define-object-binary-comparison-declarer declare-fixnum-binary-comparison T:fixnum)
(define-object-binary-comparison-declarer declare-flonum-binary-comparison T:flonum)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-comparison-declarer)

  (define-syntax define-object-unary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison T:flonum)
    ;;   (declare-flonum-unary/multi-comparison fl=?)
    ;;   (declare-flonum-unary/multi-comparison fl<?)
    ;;   (declare-flonum-unary/multi-comparison fl>?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)				=> (T:boolean))
	  ((?type-tag ?type-tag)		=> (T:boolean))
	  ((?type-tag ?type-tag . ?type-tag)	=> (T:boolean)))
	 (attributes
	  ((_)				foldable effect-free)
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-COMPARISON-DECLARER |# )

(define-object-unary/multi-comparison-declarer declare-number-unary/multi-comparison T:number)
(define-object-unary/multi-comparison-declarer declare-fixnum-unary/multi-comparison T:fixnum)
(define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison T:flonum)


;;;; syntax helpers: math operations

(define-syntax (define-object-unary-operation-declarer stx)
  ;;Usage example:
  ;;
  ;;   (define-object-unary-operation-declarer declare-flonum-unary T:flonum)
  ;;   (declare-flonum-unary flsin)
  ;;   (declare-flonum-unary flcos)
  ;;   (declare-flonum-unary fltan)
  ;;
  (syntax-case stx ()
    ((_ ?declarer ?type-tag)
     #'(define-syntax ?declarer
	 (syntax-rules ()
	   ((_ ?who)
	    (?declarer ?who safe))
	   ((_ ?who ?safety)
	    #'(declare-core-primitive ?who
		  (?safety)
		(signatures
		 ((?type-tag)	=> (?type-tag)))
		(attributes
		 ((_)		foldable effect-free result-true))))
	   )))
    ))

(define-object-unary-operation-declarer declare-number-unary T:number)
(define-object-unary-operation-declarer declare-fixnum-unary T:fixnum)
(define-object-unary-operation-declarer declare-flonum-unary T:flonum)

;;; --------------------------------------------------------------------

(define-syntax (define-object-binary-operation-declarer stx)
  ;;Usage examples:
  ;;
  ;;   (define-object-binary-operation-declarer declare-flonum-binary T:flonum)
  ;;   (declare-flonum-binary flexpt)
  ;;
  (syntax-case stx ()
    ((_ ?declarer ?type-tag)
    #'(define-syntax ?declarer
	(syntax-rules ()
	  ((_ ?who)
	   (?declarer ?who safe))
	  ((_ ?who ?safety)
	   #'(declare-core-primitive ?who
		 (?safety)
	       (signatures
		((?type-tag ?type-tag)	=> (?type-tag)))
	       (attributes
		((_ _)			foldable effect-free result-true))))
	  )))
    ))

(define-object-binary-operation-declarer declare-number-binary T:number)
(define-object-binary-operation-declarer declare-fixnum-binary T:fixnum)
(define-object-binary-operation-declarer declare-flonum-binary T:flonum)

;;; --------------------------------------------------------------------

(define-syntax (define-object-unary/binary-operation-declarer stx)
  ;;Usage examples:
  ;;
  ;;   (define-object-unary/binary-operation-declarer declare-flonum-unary/binary T:flonum)
  ;;   (declare-flonum-unary/binary fllog)
  ;;
  (syntax-case stx ()
    ((_ ?declarer ?type-tag)
    #'(define-syntax ?declarer
	(syntax-rules ()
	  ((_ ?who)
	   (?declarer ?who safe))
	  ((_ ?who ?safety)
	   #'(declare-core-primitive ?who
		 (?safety)
	       (signatures
		((?type-tag)		=> (?type-tag))
		((?type-tag ?type-tag)	=> (?type-tag)))
	       (attributes
		((_)		foldable effect-free result-true)
		((_ _)		foldable effect-free result-true))))
	  )))
    ))

(define-object-unary/binary-operation-declarer declare-number-unary/binary T:number)
(define-object-unary/binary-operation-declarer declare-fixnum-unary/binary T:fixnum)
(define-object-unary/binary-operation-declarer declare-flonum-unary/binary T:flonum)

;;; --------------------------------------------------------------------

(define-syntax (define-object-unary/multi-operation-declarer stx)
  ;;Usage examples:
  ;;
  ;;   (define-object-unary/multi-operation-declarer declare-flonum-unary/multi T:flonum)
  ;;   (declare-flonum-unary/multi fl+)
  ;;   (declare-flonum-unary/multi fl-)
  ;;   (declare-flonum-unary/multi fl*)
  ;;   (declare-flonum-unary/multi fl/)
  ;;
  (syntax-case stx ()
    ((_ ?declarer ?type-tag)
     #'(define-syntax ?declarer
	 (syntax-rules ()
	   ((_ ?who)
	    (?declarer ?who safe))
	   ((_ ?who ?safety)
	    #'(declare-core-primitive ?who
		  (?safety)
		(signatures
		 ((?type-tag)				=> (?type-tag))
		 ((?type-tag ?type-tag)			=> (?type-tag))
		 ((?type-tag ?type-tag . ?type-tag)	=> (?type-tag)))
		(attributes
		 ((_)			foldable effect-free result-true)
		 ((_ _)			foldable effect-free result-true)
		 ((_ _ . _)		foldable effect-free result-true))))
	   )))
    ))

(define-object-unary/multi-operation-declarer declare-number-unary/multi T:number)
(define-object-unary/multi-operation-declarer declare-fixnum-unary/multi T:fixnum)
(define-object-unary/multi-operation-declarer declare-flonum-unary/multi T:flonum)

;;; --------------------------------------------------------------------

(define-syntax (define-object-multi-operation-declarer stx)
  ;;Usage examples:
  ;;
  ;;   (define-object-multi-operation-declarer declare-fixnum-multi T:fixnum)
  ;;   (declare-fixnum-multi fxior)
  ;;
  (syntax-case stx ()
    ((_ ?declarer ?type-tag)
     #'(define-syntax ?declarer
	 (syntax-rules ()
	   ((_ ?who)
	    (?declarer ?who safe))
	   ((_ ?who ?safety)
	    #'(declare-core-primitive ?who
		  (?safety)
		(signatures
		 (()					=> (?type-tag))
		 (?type-tag				=> (?type-tag)))
		(attributes
		 (()			foldable effect-free result-true)
		 ((_ . _)		foldable effect-free result-true))))
	   )))
    ))

(define-object-multi-operation-declarer declare-number-multi T:number)
(define-object-multi-operation-declarer declare-fixnum-multi T:fixnum)
(define-object-multi-operation-declarer declare-flonum-multi T:flonum)


;;;; syntax helpers: pairs, lists, alists

(module (declare-list-accessor)

  (define-syntax declare-list-accessor
    ;;This is for: CAR, CDR, CAAR, CADR, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-list-accessor ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-list-accessor ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-list-accessor ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-list-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-list-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-list-accessor ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-list-accessor
    (syntax-rules (replacements)
      ((_ ?who ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((T:pair)		=> (_)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (declare-list-mutator)

  (define-syntax declare-list-mutator
    ;;This is for: SET-CAR!, SET-CDR!, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-list-mutator ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-list-mutator ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-list-mutator ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-list-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-list-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-list-mutator ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-list-mutator
    (syntax-rules (replacements)
      ((_ ?who ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((T:pair _)		=> (T:void)))
	 (attributes
	  ((_ _)		result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DECLARE-LIST-MUTATOR |# )

;;; --------------------------------------------------------------------

(module (declare-alist-accessor)

  (define-syntax declare-alist-accessor
    ;;This is for: ASSQ, ASSV, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who ?obj-tag)					(%declare-alist-accessor ?who ?obj-tag safe   (replacements)))
      ((_ ?who ?obj-tag safe)					(%declare-alist-accessor ?who ?obj-tag safe   (replacements)))
      ((_ ?who ?obj-tag unsafe)					(%declare-alist-accessor ?who ?obj-tag unsafe (replacements)))
      ((_ ?who ?obj-tag        (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag safe   (replacements . ?replacements)))
      ((_ ?who ?obj-tag safe   (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag safe   (replacements . ?replacements)))
      ((_ ?who ?obj-tag unsafe (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-alist-accessor
    (syntax-rules (replacements)
      ((_ ?who ?obj-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?obj-tag T:pair)	=> (_)))
	 (attributes
	  ((_ _)		foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DECLARE-ALIST-ACCESSOR |# )

;;; --------------------------------------------------------------------

(define-syntax declare-list-finder
  ;;This is for: MEMQ, MEMV, ...
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag)
     (declare-list-finder ?who ?obj-tag safe))
    ((_ ?who ?obj-tag ?safety)
     (declare-core-primitive ?who
	 (?safety)
       (signatures
	((?obj-tag T:pair)	=> (_))
	((?obj-tag T:null)	=> (T:false)))
       (attributes
	((_ ())			foldable effect-free result-false)
	((_ _)			foldable effect-free))))
    ))


;;;; misc functions

(declare-object-binary-comparison eq?)
(declare-object-binary-comparison eqv?)
(declare-object-binary-comparison equal?)

(declare-object-predicate not)

(declare-core-primitive void
    (safe)
  (signatures
   (()				=> (T:void)))
  (attributes
   (()				foldable effect-free result-true)))


;;;; pairs and lists, safe functions

;;; predicates

(declare-core-primitive null?
    (safe)
  (signatures
   ((T:null)		=> (T:true))
   ((T:pair)		=> (T:false))
   ((_)			=> (T:boolean)))
  (attributes
   ((())		foldable effect-free result-true)
   ((_)			foldable effect-free)))

(declare-core-primitive pair?
    (safe)
  (signatures
   ((T:null)		=> (T:false))
   ((T:pair)		=> (T:true))
   ((_)			=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive list?
    (safe)
  (signatures
   ((T:null)		=> (T:true))
   ((T:pair)		=> (T:true))
   ((_)			=> (T:boolean)))
  (attributes
   ((())		foldable effect-free result-true)
   ((_)			foldable effect-free)))

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive cons
    (safe)
  (signatures
   ((_ _)		=> (T:pair)))
  (attributes
   ;;This is not foldable because it must return a newly allocated pair every time.
   ((_ _)		effect-free result-true)))

(declare-core-primitive cons*
    (safe)
  (signatures
   ((_)			=> (_))
   ((_ _ . _)		=> (T:pair)))
  (attributes
   ;;This will return the operand itself, so it is foldable.
   ((_)			foldable effect-free)
   ;;This is not foldable because it must return a newly allocated list every time.
   ((_ _ . _)		effect-free result-true)))

(declare-core-primitive list
    (safe)
  (signatures
   (()			=> (T:null))
   ((_ . _)		=> (T:pair)))
  (attributes
   ;;Foldable because it returns null.
   (()			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_ . _)		         effect-free result-true)))

(declare-core-primitive reverse
    (safe)
  (signatures
   ((T:null)		=> (T:null))
   ((T:pair)		=> (T:pair)))
  (attributes
   ;;This is foldable because it returns null itself.
   ((())		foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-list-finder memq T:object)
(declare-list-finder memv T:object)
(declare-list-finder member T:object)
(declare-list-finder memp T:procedure)

(declare-core-primitive length
    (safe)
  (signatures
   ((T:pair)			=> (_))
   ((T:null)			=> (T:zero)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-list-accessor car
  (replacements $car))

(declare-list-accessor cdr
  (replacements $cdr))

(declare-list-accessor caar)
(declare-list-accessor cadr)
(declare-list-accessor cdar)
(declare-list-accessor cddr)
(declare-list-accessor caaar)
(declare-list-accessor caadr)
(declare-list-accessor cadar)
(declare-list-accessor caddr)
(declare-list-accessor cdaar)
(declare-list-accessor cdadr)
(declare-list-accessor cddar)
(declare-list-accessor cdddr)
(declare-list-accessor caaaar)
(declare-list-accessor caaadr)
(declare-list-accessor caadar)
(declare-list-accessor caaddr)
(declare-list-accessor cadaar)
(declare-list-accessor cadadr)
(declare-list-accessor caddar)
(declare-list-accessor cadddr)
(declare-list-accessor cdaaar)
(declare-list-accessor cdaadr)
(declare-list-accessor cdadar)
(declare-list-accessor cdaddr)
(declare-list-accessor cddaar)
(declare-list-accessor cddadr)
(declare-list-accessor cdddar)
(declare-list-accessor cddddr)

;;; --------------------------------------------------------------------
;;; mutators

(declare-list-mutator set-car!
  (replacements $set-car!))

(declare-list-mutator set-cdr!
  (replacements $set-cdr!))

;;; --------------------------------------------------------------------
;;; associative lists

(declare-alist-accessor assq T:object)
(declare-alist-accessor assv T:object)
(declare-alist-accessor assoc T:object)
(declare-alist-accessor assp T:procedure)


;;;; pairs and lists, unsafe functions

(declare-list-accessor $car unsafe)
(declare-list-accessor $cdr unsafe)

(declare-list-mutator $set-car! unsafe)
(declare-list-mutator $set-cdr! unsafe)


;;;; fixnums safe operations

(declare-core-primitive greatest-fixnum
    (unsafe)
  (signatures
   (()				=> (T:positive-fixnum)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive least-fixnum
    (unsafe)
  (signatures
   (()				=> (T:negative-fixnum)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive fixnum-width
    (unsafe)
  (signatures
   (()				=> (T:positive-fixnum)))
  (attributes
   (()				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate fixnum? T:fixnum)

(declare-fixnum-predicate fxzero?		(replacements $fxzero?))
(declare-fixnum-predicate fxpositive?		(replacements $fxpositive?))
(declare-fixnum-predicate fxnegative?		(replacements $fxnegative?))
(declare-fixnum-predicate fxnonpositive?	(replacements $fxnonpositive?))
(declare-fixnum-predicate fxnonnegative?	(replacements $fxnonnegative?))
(declare-fixnum-predicate fxeven?		(replacements $fxeven?))
(declare-fixnum-predicate fxodd?		(replacements $fxodd?))

;;; --------------------------------------------------------------------
;;; arithmetics

(declare-fixnum-binary fx+)
(declare-fixnum-unary/binary fx-)
(declare-fixnum-binary fx*)
(declare-fixnum-binary fxdiv)
(declare-fixnum-binary fxmod)
(declare-fixnum-binary fxdiv0)
(declare-fixnum-binary fxmod0)
(declare-fixnum-unary fxadd1)
(declare-fixnum-unary fxsub1)

(declare-fixnum-unary fxabs)
(declare-fixnum-unary fxsign)
(declare-fixnum-binary fxremainder)
(declare-fixnum-binary fxquotient)
(declare-fixnum-binary fxmodulo)

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive fx+/carry
;;     (safe)
;;   (signatures
;;    ((T:fixnum T:fixnum T:fixnum) => (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _ _)			foldable effect-free result-true)))

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive fx-/carry
;;     (safe)
;;   (signatures
;;    ((T:fixnum T:fixnum T:fixnum) => (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _ _)			foldable effect-free result-true)))

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive fx*/carry
;;     (safe)
;;   (signatures
;;    ((T:fixnum T:fixnum T:fixnum) => (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _ _)			foldable effect-free result-true)))

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive fxdiv-and-mod
;;     (safe)
;;   (signatures
;;    ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _)			foldable effect-free result-true)))

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive fxdiv-and-mod0
;;     (safe)
;;   (signatures
;;    ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; bitwise operations

(declare-fixnum-unary fxnot		(replacements $fxlognot))
(declare-fixnum-multi fxand		(replacements $fxlogand))
(declare-fixnum-multi fxior		(replacements $fxlogor))
(declare-fixnum-multi fxxor		(replacements $fxlogxor))
(declare-fixnum-multi fxlogor		(replacements $fxlogor))
(declare-fixnum-multi fxlogand		(replacements $fxlogand))
(declare-fixnum-multi fxlogxor		(replacements $fxlogxor))

(declare-fixnum-binary fxsll)
(declare-fixnum-binary fxsra)
(declare-fixnum-binary fxarithmetic-shift-left)
(declare-fixnum-binary fxarithmetic-shift-right)
(declare-fixnum-binary fxarithmetic-shift)

;;; --------------------------------------------------------------------
;;; comparison

(declare-fixnum-unary/multi-comparison fx=?	(replacements $fx=))
(declare-fixnum-unary/multi-comparison fx!=?	(replacements $fx!=))
(declare-fixnum-unary/multi-comparison fx<?	(replacements $fx<))
(declare-fixnum-unary/multi-comparison fx>?	(replacements $fx>))
(declare-fixnum-unary/multi-comparison fx<=?	(replacements $fx<=))
(declare-fixnum-unary/multi-comparison fx>=?	(replacements $fx>=))

;;;

(declare-fixnum-unary/multi-comparison fx=	(replacements $fx=))
(declare-fixnum-unary/multi-comparison fx!=	(replacements $fx!=))
(declare-fixnum-unary/multi-comparison fx<	(replacements $fx<))
(declare-fixnum-unary/multi-comparison fx>	(replacements $fx>))
(declare-fixnum-unary/multi-comparison fx<=	(replacements $fx<=))
(declare-fixnum-unary/multi-comparison fx>=	(replacements $fx>=))

(declare-fixnum-unary/multi-comparison fxmax	(replacements $fxmax))
(declare-fixnum-unary/multi-comparison fxmin	(replacements $fxmin))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive fixnum->flonum
    (safe)
  (signatures
   ((T:fixnum)		=> (T:flonum)))
  (attributes
   ((_)			foldable effect-free result-true)))


;;;; fixnums unsafe operations

;;; predicates

(declare-fixnum-predicate $fxzero? unsafe)
(declare-fixnum-predicate $fxpositive? unsafe)
(declare-fixnum-predicate $fxnegative? unsafe)
(declare-fixnum-predicate $fxnonpositive? unsafe)
(declare-fixnum-predicate $fxnonnegative? unsafe)
(declare-fixnum-predicate $fxeven? unsafe)
(declare-fixnum-predicate $fxodd? unsafe)

;;; --------------------------------------------------------------------
;;; arithmetics

(declare-fixnum-binary $fx+ unsafe)
(declare-fixnum-unary/binary $fx- unsafe)
(declare-fixnum-binary $fx* unsafe)
(declare-fixnum-binary $fxdiv unsafe)
(declare-fixnum-binary $fxmod unsafe)
(declare-fixnum-binary $fxdiv0 unsafe)
(declare-fixnum-binary $fxmod0 unsafe)

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive $fxdiv-and-mod
;;     (unsafe)
;;   (signatures
;;    ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _)			foldable effect-free result-true)))

;;FIXME We  do not  support multiple return  value, yet.  (Marco  Maggi; Mon  Nov 10,
;;2014)
;;
;; (declare-core-primitive $fxdiv-and-mod0
;;     (unsafe)
;;   (signatures
;;    ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
;;   (attributes
;;    ((_ _)			foldable effect-free result-true)))

(declare-fixnum-unary $fxadd1 unsafe)
(declare-fixnum-unary $fxsub1 unsafe)
(declare-fixnum-unary $fxabs unsafe)
(declare-fixnum-unary $fxsign unsafe)
(declare-fixnum-unary $fxremainder unsafe)
(declare-fixnum-unary $fxquotient unsafe)
(declare-fixnum-unary $fxmodulo unsafe)

;;; --------------------------------------------------------------------
;;; bitwise operations

(declare-fixnum-binary $fxlogor unsafe)
(declare-fixnum-binary $fxlogand unsafe)
(declare-fixnum-binary $fxlogxor unsafe)
(declare-fixnum-unary $fxnot unsafe)
(declare-fixnum-binary $fxsll unsafe)
(declare-fixnum-binary $fxsra unsafe)

;;; --------------------------------------------------------------------
;;; comparison

(declare-fixnum-binary-comparison $fx= unsafe)
(declare-fixnum-binary-comparison $fx!= unsafe)
(declare-fixnum-binary-comparison $fx< unsafe)
(declare-fixnum-binary-comparison $fx> unsafe)
(declare-fixnum-binary-comparison $fx<= unsafe)
(declare-fixnum-binary-comparison $fx>= unsafe)

(declare-fixnum-binary-comparison $fxmax unsafe)
(declare-fixnum-binary-comparison $fxmin unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $fixnum->flonum
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $fixnum->char
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:char)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; bignums, safe operations

(declare-core-primitive least-positive-bignum
    (safe)
  (signatures
   (()				=> (T:bignum)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive greatest-negative-bignum
    (safe)
  (signatures
   (()				=> (T:bignum)))
  (attributes
   (()				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate bignum? T:bignum)

(declare-bignum-predicate bignum-positive?	(replacements $bignum-positive?))
(declare-bignum-predicate bignum-negative?	(replacements $bignum-negative?))
(declare-bignum-predicate bignum-non-positive?	(replacements $bignum-non-positive?))
(declare-bignum-predicate bignum-non-negative?	(replacements $bignum-non-negative?))

(declare-bignum-predicate bignum-even?		(replacements $bignum-even?))
(declare-bignum-predicate bignum-odd?		(replacements $bignum-odd?))


;;;; bignums, unsafe operations

(declare-bignum-predicate $bignum-positive? unsafe)
(declare-bignum-predicate $bignum-negative? unsafe)
(declare-bignum-predicate $bignum-non-positive? unsafe)
(declare-bignum-predicate $bignum-non-negative? unsafe)

(declare-bignum-predicate $bignum-even? unsafe)
(declare-bignum-predicate $bignum-odd? unsafe)

;;; --------------------------------------------------------------------

(declare-core-primitive $bignum-byte-ref
    (unsafe)
  (signatures
   ((T:bignum T:fixnum)		=> (T:fixnum)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive $bignum-size
    (unsafe)
  (signatures
   ((T:bignum)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $bignum->flonum
    (unsafe)
  (signatures
   ((T:bignum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; ratnums, safe operations

(declare-type-predicate ratnum? T:ratnum)


;;;; ratnums, unsafe operations

(declare-core-primitive $make-ratnum
    (unsafe)
  (signatures
   ((T:exact-integer T:exact-integer)		=> (T:ratnum)))
  (attributes
   ((_ _)					foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive $ratnum-n
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-d
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-num
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-den
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-ratnum-predicate $ratnum-positive? unsafe)
(declare-ratnum-predicate $ratnum-negative? unsafe)
(declare-ratnum-predicate $ratnum-non-positive? unsafe)
(declare-ratnum-predicate $ratnum-non-negative? unsafe)

;;; --------------------------------------------------------------------

(declare-core-primitive $ratnum->flonum
    (unsafe)
  (signatures
   ((T:ratnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; flonums, safe functions

(section

;;; predicates

 (declare-type-predicate flonum? T:flonum)

 (declare-flonum-predicate flzero?		(replacements $flzero?))
 (declare-flonum-predicate flzero?/negative	(replacements $flzero?/negative))
 (declare-flonum-predicate flzero?/positive	(replacements $flzero?/positive))
 (declare-flonum-predicate flpositive?		(replacements $flpositive?))
 (declare-flonum-predicate flnegative?		(replacements $flnegative?))
 (declare-flonum-predicate flnonpositive?	(replacements $flnonpositive?))
 (declare-flonum-predicate flnonnegative?	(replacements $flnonnegative?))
 (declare-flonum-predicate fleven?		(replacements $fleven?))
 (declare-flonum-predicate flodd?		(replacements $flodd?))

 (declare-flonum-predicate flnan?		(replacements $flnan?))
 (declare-flonum-predicate flfinite?		(replacements $flfinite?))
 (declare-flonum-predicate flinfinite?		(replacements $flinfinite?))
 (declare-flonum-predicate flinteger?		(replacements $flinteger?))

;;; --------------------------------------------------------------------
;;; rounding

 (declare-flonum-unary flround		(replacements $flround))
 (declare-flonum-unary flfloor		(replacements $flfloor))
 (declare-flonum-unary flceiling	(replacements $flceiling))
 (declare-flonum-unary fltruncate	(replacements $fltruncate))

;;; --------------------------------------------------------------------
;;; parts

 (declare-flonum-unary flnumerator	(replacements $flnumerator))
 (declare-flonum-unary fldenominator	(replacements $fldenominator))
 (declare-flonum-unary flabs		(replacements $flabs))

 ;;FIXME We do not do multiple return values, yet.  (Marco Maggi; Thu Nov 13, 2014)
 ;;
 ;; (declare-flonum-unary flonum-bytes)
 ;; (declare-flonum-unary flonum-parts)

;;; --------------------------------------------------------------------
;;; trigonometric

 (declare-flonum-unary flsin		(replacements $flsin))
 (declare-flonum-unary flcos		(replacements $flcos))
 (declare-flonum-unary fltan		(replacements $fltan))
 (declare-flonum-unary flasin		(replacements $flasin))
 (declare-flonum-unary flacos		(replacements $flacos))

 (declare-core-primitive flatan
     (safe)
   (signatures
    ((T:flonum)			=> (T:flonum))
    ((T:flonum T:flonum)		=> (T:flonum)))
   (attributes
    ((_)			foldable effect-free result-true)
    ((_ _)		foldable effect-free result-true))
   (replacements $flatan $flatan2))

;;; --------------------------------------------------------------------
;;; hyperbolic

 (declare-flonum-unary flsinh		(replacements $flsinh))
 (declare-flonum-unary flcosh		(replacements $flcosh))
 (declare-flonum-unary fltanh		(replacements $fltanh))
 (declare-flonum-unary flasinh		(replacements $flasinh))
 (declare-flonum-unary flacosh		(replacements $flacosh))
 (declare-flonum-unary flatanh		(replacements $flatanh))

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials, logarithms

 (declare-flonum-unary flexp		(replacements $flexp))
 (declare-flonum-unary/binary fllog	(replacements $fllog $fllog2))
 (declare-flonum-unary flexpm1		(replacements $flexpm1))
 (declare-flonum-unary fllog1p		(replacements $fllog1p))
 (declare-flonum-binary flexpt		(replacements $flexpt))
 (declare-flonum-unary flsqrt		(replacements $flsqrt))
 (declare-flonum-unary flsquare		(replacements $flsquare))
 (declare-flonum-unary flcube		(replacements $flcube))
 (declare-flonum-unary flcbrt		(replacements $flcbrt))
 (declare-flonum-binary flhypot		(replacements $flhypot))

;;; --------------------------------------------------------------------
;;; comparison

 (declare-flonum-unary/multi-comparison fl=?	(replacements $fl=))
 (declare-flonum-unary/multi-comparison fl!=?	(replacements $fl!=))
 (declare-flonum-unary/multi-comparison fl<?	(replacements $fl<))
 (declare-flonum-unary/multi-comparison fl>?	(replacements $fl>))
 (declare-flonum-unary/multi-comparison fl<=?	(replacements $fl<=))
 (declare-flonum-unary/multi-comparison fl>=?	(replacements $fl>=))

;;; --------------------------------------------------------------------
;;; arithmetics

 (declare-flonum-unary/multi fl+		(replacements $fl+))
 (declare-flonum-unary/multi fl-		(replacements $fl-))
 (declare-flonum-unary/multi fl*		(replacements $fl*))
 (declare-flonum-unary/multi fl/		(replacements $fl/))

 (declare-flonum-unary/multi flmin	(replacements $flmin))
 (declare-flonum-unary/multi flmax	(replacements $flmax))

 (declare-flonum-binary fldiv		(replacements $fldiv))
 (declare-flonum-binary fldiv0		(replacements $fldiv0))
 (declare-flonum-binary flmod		(replacements $flmod))
 (declare-flonum-binary flmod0		(replacements $flmod0))

 ;;FIXME We do not do multiple return values, yet.  (Marco Maggi; Wed Nov 12, 2014)
 ;;
 ;; (declare-flonum-binary fldiv-and-mod		(replacements $fldiv-and-mod))
 ;; (declare-flonum-binary fldiv0-and-mod0	(replacements $fldiv0-and-mod0))

;;; --------------------------------------------------------------------
;;; conversion

 (declare-core-primitive flonum->string
     (safe)
   (signatures
    ((T:flonum)		=> (T:string)))
   (attributes
    ((_)			foldable effect-free result-true)))

 (declare-core-primitive flonum->bytevector
     (safe)
   (signatures
    ((T:flonum)		=> (T:bytevector)))
   (attributes
    ((_)			foldable effect-free result-true)))

 (declare-core-primitive real->flonum
     (safe)
   (signatures
    ((T:real)		=> (T:flonum)))
   (attributes
    ((_)			foldable effect-free result-true)))

 /section)


;;;; flonums, unsafe functions

(section

 (declare-core-primitive $make-flonum
     (unsafe)
   (signatures
    (()				=> (T:flonum)))
   ;;Not foldable because $MAKE-FLONUM must return a new flonum every time.
   (attributes
    (()				effect-free result-true)))

 (declare-core-primitive $flonum->exact
     (unsafe)
   (signatures
    ((T:flonum)			=> (T:exact-real)))
   (attributes
    ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

 (declare-flonum-predicate $flzero? unsafe)
 (declare-flonum-predicate $flzero?/positive unsafe)
 (declare-flonum-predicate $flzero?/negative unsafe)
 (declare-flonum-predicate $flpositive? unsafe)
 (declare-flonum-predicate $flnegative? unsafe)
 (declare-flonum-predicate $flnonpositive? unsafe)
 (declare-flonum-predicate $flnonnegative? unsafe)

 (declare-flonum-predicate $fleven? unsafe)
 (declare-flonum-predicate $flodd? unsafe)

 (declare-flonum-predicate $flnan? unsafe)
 (declare-flonum-predicate $flfinite? unsafe)
 (declare-flonum-predicate $flinfinite? unsafe)
 (declare-flonum-predicate $flonum-integer? unsafe)
 (declare-flonum-predicate $flonum-rational? unsafe)

;;; --------------------------------------------------------------------
;;; rounding

 (declare-flonum-unary $flround unsafe)
 (declare-flonum-unary $flfloor unsafe)
 (declare-flonum-unary $flceiling unsafe)
 (declare-flonum-unary $fltruncate unsafe)

;;; --------------------------------------------------------------------
;;; parts

 (declare-flonum-unary $flnumerator unsafe)
 (declare-flonum-unary $fldenominator unsafe)
 (declare-flonum-unary $flabs unsafe)

 (declare-core-primitive $flonum-u8-ref
     (unsafe)
   (signatures
    ((T:flonum T:fixnum)		=> (T:fixnum)))
   (attributes
    ((_ _)			effect-free result-true)))

 (declare-core-primitive $flonum-sbe
     (unsafe)
   (signatures
    ((T:flonum)			=> (T:fixnum)))
   (attributes
    ((_)				effect-free result-true)))

 (declare-core-primitive $flonum-set!
     (unsafe)
   (signatures
    ((T:flonum T:fixnum T:fixnum)	=> (T:void)))
   (attributes
    ((_ _ _)				result-true)))

;;; --------------------------------------------------------------------
;;; trigonometric

 (declare-flonum-unary $flsin unsafe)
 (declare-flonum-unary $flcos unsafe)
 (declare-flonum-unary $fltan unsafe)
 (declare-flonum-unary $flasin unsafe)
 (declare-flonum-unary $flacos unsafe)
 (declare-flonum-unary $flatan unsafe)
 (declare-flonum-binary $flatan2 unsafe)

;;; --------------------------------------------------------------------
;;; hyperbolic

 (declare-flonum-unary $flsinh unsafe)
 (declare-flonum-unary $flcosh unsafe)
 (declare-flonum-unary $fltanh unsafe)
 (declare-flonum-unary $flasinh unsafe)
 (declare-flonum-unary $flacosh unsafe)
 (declare-flonum-unary $flatanh unsafe)

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials, logarithms

 (declare-flonum-unary $flexp unsafe)
 (declare-flonum-unary $fllog unsafe)
 (declare-flonum-binary $fllog2 unsafe)
 (declare-flonum-unary $flexpm1 unsafe)
 (declare-flonum-unary $fllog1p unsafe)
 (declare-flonum-unary $flexpt unsafe)
 (declare-flonum-unary $flsqrt unsafe)
 (declare-flonum-unary $flsquare unsafe)
 (declare-flonum-unary $flsquare unsafe)
 (declare-flonum-unary $flcube unsafe)
 (declare-flonum-unary $flcbrt unsafe)
 (declare-flonum-binary $flhypot unsafe)

;;; --------------------------------------------------------------------
;;; comparison

 (declare-flonum-binary-comparison $fl= unsafe)
 (declare-flonum-binary-comparison $fl< unsafe)
 (declare-flonum-binary-comparison $fl> unsafe)
 (declare-flonum-binary-comparison $fl<= unsafe)
 (declare-flonum-binary-comparison $fl>= unsafe)

 ;;FIXME To be implemented.  (Marco Maggi; Thu Nov 13, 2014)
 ;;
 ;;(declare-flonum-binary-comparison $fl!=? unsafe)

;;; --------------------------------------------------------------------
;;; arithmetics

 (declare-flonum-binary $fl+ unsafe)
 (declare-flonum-unary/binary $fl- unsafe)
 (declare-flonum-binary $fl* unsafe)
 (declare-flonum-binary $fl/ unsafe)

 (declare-flonum-binary $fldiv unsafe)
 (declare-flonum-binary $flmod unsafe)
 (declare-flonum-binary $fldiv0 unsafe)
 (declare-flonum-binary $flmod0 unsafe)

 (declare-flonum-binary $flmax unsafe)
 (declare-flonum-binary $flmin unsafe)

 ;;FIXME We do not do multiple return values, yet.  (Marco Maggi; Wed Nov 12, 2014)
 ;;
 ;;(($fldiv-and-mod _ _)		   foldable effect-free result-true)
 ;;(($fldiv0-and-mod0 _ _)	   foldable effect-free result-true)

 /section)


;;;; cflonums, safe functions

(declare-type-predicate cflonum? T:cflonum)

(declare-core-primitive $cflonum-real
    (unsafe)
  (signatures
   ((T:cflonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $cflonum-imag
    (unsafe)
  (signatures
   ((T:cflonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; compnums, safe functions

(declare-type-predicate compnum? T:compnum)

(declare-core-primitive $compnum-real
    (unsafe)
  (signatures
   ((T:compnum)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $compnum-imag
    (unsafe)
  (signatures
   ((T:compnum)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; symbols, safe functions

(declare-type-predicate symbol? T:symbol)

;;; --------------------------------------------------------------------
;;; property lists

(declare-core-primitive putprop
    (safe)
  (signatures
   ((T:symbol T:symbol _) => (_)))
  (attributes
   ((_ _ _)		result-true))
  (replacements $putprop))

(declare-core-primitive getprop
    (safe)
  (signatures
   ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		effect-free))
  (replacements $getprop))

(declare-core-primitive remprop
    (safe)
  (signatures
   ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		result-true))
  (replacements $remprop))

(declare-core-primitive property-list
    (safe)
  ;;The return value can be null or a pair.
  (signatures
   ((T:symbol) => (_)))
  (attributes
   ((_)			effect-free result-true))
  (replacements $property-list))


;;;; symbols, unsafe functions

;;; property lists

(declare-core-primitive $putprop
    (unsafe)
  (signatures
   ((T:symbol T:symbol _) => (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive $getprop
    (unsafe)
  (signatures
   ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive $remprop
    (unsafe)
  (signatures
   ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive $property-list
    (unsafe)
  (signatures
   ((T:symbol) => (_)))
  (attributes
   ((_)			effect-free	result-true)))


;;;; characters safe operations

;;; predicates

(declare-type-predicate char? T:char)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive char->integer
    (safe)
  (signatures
   ((T:char)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; characters unsafe operations

;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive $char=
    (unsafe)
  (signatures
   ((T:char T:char)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

;;FIXME Not  implemented.  To  be implemented  before the  next boot  image rotation.
;;(Marco Maggi; Mon Nov 10, 2014)
;;
;; (declare-core-primitive $char!=
;;     (unsafe)
;;   (signatures
;;    ((T:char T:char)		=> (T:boolean)))
;;   (attributes
;;    ((_ _)			foldable effect-free)))

(declare-core-primitive $char<
    (unsafe)
  (signatures
   ((T:char T:char)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $char>
    (unsafe)
  (signatures
   ((T:char T:char)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $char<=
    (unsafe)
  (signatures
   ((T:char T:char)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $char>=
    (unsafe)
  (signatures
   ((T:char T:char)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $char->fixnum
    (unsafe)
  (signatures
   ((T:char)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; pointers, safe functions

(declare-type-predicate pointer?)


;;;; strings, safe functions
;;
;;According to R6RS:  STRING and MAKE-STRING must return a  newly allocated string at
;;every invocation;  if we want the  same string we  just use the double  quotes.  So
;;STRING and MAKE-STRING are not FOLDABLE.
;;

(declare-core-primitive string
    (safe)
  (signatures
   (()		=> (T:string))
   (T:char	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   (()			effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive make-string
    (safe)
  (signatures
   ((T:fixnum)		=> (T:string))
   ((T:fixnum T:char)	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((0)			effect-free result-true)
   ((0 . _)		effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive string?
    (safe)
  (signatures
   ((_)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive string-length
    (safe)
  (signatures
   ((T:string)	=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

;;FIXME  This cannot  have $STRING-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive string-ref
    (safe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;FIXME This  cannot have  $STRING-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive string-set!
    (safe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive string->number
    (safe)
  (signatures
   ((T:string)		=> (_))
   ((T:string T:fixnum)	=> (_)))
  (attributes
   ((_)			foldable effect-free)
   ((_ _)		foldable effect-free)))

(declare-core-primitive string->utf8
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf16be
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf16le
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf16n
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->utf32
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->ascii
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->latin1
    (safe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->symbol
    (safe)
  (signatures
   ((T:string)		=> (T:symbol)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->keyword
    (safe)
  (signatures
   ((T:string)		=> (T:other-object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string->flonum
    (safe)
  (signatures
   ((T:string)		=> (T:flonum)))
  (attributes
   ((_)			foldable effect-free result-true)))


;;;; strings, unsafe functions

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((T:string)	=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-ref
    (safe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (safe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))


;;;; vectors
;;
;;According to R6RS:  VECTOR and MAKE-VECTOR must return a  newly allocated string at
;;every invocation; so they are not foldable.
;;

(declare-core-primitive vector?
    (safe)
  (signatures
   ((_)				=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive vector
    (safe)
  (signatures
   (()				=> (T:vector))
   (_				=> (T:vector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   (()				effect-free result-true)
   (_				effect-free result-true)))

(declare-core-primitive make-vector
    (safe)
  (signatures
   ((T:fixnum)			=> (T:vector))
   ((T:fixnum _)		=> (T:vector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive vector-length
    (safe)
  (signatures
   ((T:vector)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $vector-length))

;;FIXME  This cannot  have $VECTOR-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-ref
    (safe)
  (signatures
   ((T:vector T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free)))

;;FIXME This  cannot have  $VECTOR-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-set!
    (safe)
  (signatures
   ((T:vector T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $vector-length
    (unsafe)
  (signatures
   ((T:vector)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $vector-ref
    (safe)
  (signatures
   ((T:vector T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive $vector-set!
    (safe)
  (signatures
   ((T:vector T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))


;;;; bytevectors, safe functions
;;
;;According to  R6RS: MAKE-BYTEVECTOR must return  a newly allocated string  at every
;;invocation; so it is not foldable.
;;

(declare-core-primitive make-bytevector
    (safe)
  (signatures
   ((T:fixnum)		=> (T:bytevector))
   ((T:fixnum T:fixnum)	=> (T:bytevector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bytevector-length
    (safe)
  (signatures
   ((T:bytevector)	=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bytevector-u8-ref
    (safe)
  (signatures
   ((T:bytevector T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bytevector-s8-ref
    (safe)
  (signatures
   ((T:bytevector T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _)			foldable effect-free result-true)))


;;;; bytevectors, unsafe functions

(declare-core-primitive $make-bytevector
    (safe)
  (signatures
   ((T:fixnum)		=> (T:bytevector))
   ((T:fixnum T:fixnum)	=> (T:bytevector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $bytevector-length
    (safe)
  (signatures
   ((T:bytevector)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $bytevector-u8-ref
    (safe)
  (signatures
   ((T:bytevector T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive $bytevector-s8-ref
    (safe)
  (signatures
   ((T:bytevector T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _)			foldable effect-free result-true)))


;;;; hashtables, safe primitives

(declare-core-primitive make-eq-hashtable
    (safe)
  (signatures
   (()				=> (T:hashtable))
   ((T:exact-integer)		=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive make-eqv-hashtable
    (safe)
  (signatures
   (()				=> (T:hashtable))
   ((T:exact-integer)		=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive make-hashtable
    (safe)
  (signatures
   ((T:procedure T:procedure)			=> (T:hashtable))
   ((T:procedure T:procedure T:exact-integer)	=> (T:hashtable)))
  (attributes
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))


;;;; structs, safe primitives

(declare-core-primitive struct?
    (unsafe)
  (signatures
   ((T:struct)			=> (T:true))
   ((_)				=> (T:boolean))
   ((T:struct T:struct-rtd)	=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)
   ((_ _)			foldable effect-free)))

(declare-type-predicate struct-type-descriptor? T:struct-rtd)



;;;; structs, unsafe primitives

;;; constructors

(declare-core-primitive base-rtd
    (unsafe)
  (signatures
   (()				=> (T:struct-rtd)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive $struct
    (unsafe)
  (signatures
   ((T:struct-rtd . _)		=> (T:struct)))
  (attributes
   ;;It must return a new struct every time.
   ((_ . _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-core-primitive $struct/rtd?
    (unsafe)
  (signatures
   ((_ T:struct-rtd)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive $struct-ref
    (unsafe)
  (signatures
   ((T:struct T:fixnum)		=> (_)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $struct-set!
    (unsafe)
  (signatures
   ((T:struct T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _)			foldable result-true)))



;;;; annotations

(declare-core-primitive annotation?
    (safe)
  (signatures
   ((T:other-struct)		=> (T:boolean)))
  (attributes
   ((#f)			foldable effect-free result-false)
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive get-annotated-datum
    (safe)
  (signatures
   ((T:input-port)		=> (_)))
  (attributes))

(declare-core-primitive annotation-expression
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes))

(declare-core-primitive annotation-stripped
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes
   ((#f)			foldable effect-free result-false)))

(declare-core-primitive annotation-textual-position
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes))

(declare-core-primitive annotation-source
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes))


;;;; input/output

(declare-core-primitive current-input-port
    (safe)
  (signatures
   (()					=> (T:textual-input-port))
   ((T:textual-input-port)		=> (T:void))
   ((T:textual-input-port T:boolean)	=> (T:void)))
  (attributes
   (()				effect-free result-true)
   ((_)				result-true)
   ((_ _)			result-true)))

(declare-core-primitive current-output-port
    (safe)
  (signatures
   (()					=> (T:textual-output-port))
   ((T:textual-output-port)		=> (T:void))
   ((T:textual-output-port T:boolean)	=> (T:void)))
  (attributes
   (()				effect-free result-true)
   ((_)				result-true)
   ((_ _)			result-true)))

(declare-core-primitive current-error-port
    (safe)
  (signatures
   (()					=> (T:textual-output-port))
   ((T:textual-output-port)		=> (T:void))
   ((T:textual-output-port T:boolean)	=> (T:void)))
  (attributes
   (()				effect-free result-true)
   ((_)				result-true)
   ((_ _)			result-true)))


;;;; generic functions

(declare-type-predicate procedure? T:procedure)

;;; --------------------------------------------------------------------

(declare-core-primitive eof-object
    (safe)
  (signatures
   (()				=> (T:object)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive eof-object?
    (safe)
  (signatures
   ((_)				=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive interrupt-handler
    (safe)
  (signatures
   (()				=> (T:procedure))
   ((T:procedure)		=> (T:void))
   ((T:procedure T:boolean)	=> (T:void)))
  (attributes))

(declare-core-primitive engine-handler
    (safe)
  (signatures
   (()				=> (T:procedure))
   ((T:procedure)		=> (T:void))
   ((T:procedure T:boolean)	=> (T:void)))
  (attributes))


;;;; numerics, general arithmetics, addition

(declare-core-primitive +
    (safe)
  (signatures
   (T:number => (T:number)))
  (attributes
   (_			foldable effect-free result-true))
  (replacements
   $add-fixnum-fixnum		$add-fixnum-bignum	$add-fixnum-flonum
   $add-fixnum-ratnum		$add-fixnum-compnum	$add-fixnum-cflonum

   $add-bignum-fixnum		$add-bignum-bignum	$add-bignum-flonum
   $add-bignum-ratnum		$add-bignum-compnum	$add-bignum-cflonum

   $add-flonum-fixnum		$add-flonum-bignum	$add-flonum-flonum
   $add-flonum-ratnum		$add-flonum-compnum	$add-flonum-cflonum

   $add-ratnum-fixnum		$add-ratnum-bignum	$add-ratnum-flonum
   $add-ratnum-ratnum		$add-ratnum-compnum	$add-ratnum-cflonum

   $add-compnum-fixnum		$add-compnum-bignum	$add-compnum-ratnum
   $add-compnum-compnum		$add-compnum-flonum	$add-compnum-cflonum

   $add-cflonum-fixnum		$add-cflonum-bignum	$add-cflonum-ratnum
   $add-cflonum-flonum		$add-cflonum-compnum	$add-cflonum-cflonum

   $add-fixnum-number		$add-bignum-number	$add-flonum-number
   $add-ratnum-number		$add-compnum-number	$add-cflonum-number

   $add-number-fixnum		$add-number-bignum	$add-number-flonum
   $add-number-ratnum		$add-number-compnum	$add-number-cflonum

   $add-number-number))

;;; --------------------------------------------------------------------

(declare-core-primitive $add-fixnum-fixnum
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-bignum
    (unsafe)
  (signatures
   ((T:fixnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-flonum
    (unsafe)
  (signatures
   ((T:fixnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-ratnum
    (unsafe)
  (signatures
   ((T:fixnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-compnum
    (unsafe)
  (signatures
   ((T:fixnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-fixnum-cflonum
    (unsafe)
  (signatures
   ((T:fixnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-bignum-fixnum
    (unsafe)
  (signatures
   ((T:bignum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-bignum
    (unsafe)
  (signatures
   ((T:bignum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-flonum
    (unsafe)
  (signatures
   ((T:bignum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-ratnum
    (unsafe)
  (signatures
   ((T:bignum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-compnum
    (unsafe)
  (signatures
   ((T:bignum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-cflonum
    (unsafe)
  (signatures
   ((T:bignum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-flonum-fixnum
    (unsafe)
  (signatures
   ((T:flonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-bignum
    (unsafe)
  (signatures
   ((T:flonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-flonum
    (unsafe)
  (signatures
   ((T:flonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-ratnum
    (unsafe)
  (signatures
   ((T:flonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-compnum
    (unsafe)
  (signatures
   ((T:flonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-cflonum
    (unsafe)
  (signatures
   ((T:flonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-ratnum-fixnum
    (unsafe)
  (signatures
   ((T:ratnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-bignum
    (unsafe)
  (signatures
   ((T:ratnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-flonum
    (unsafe)
  (signatures ((T:ratnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-ratnum
    (unsafe)
  (signatures
   ((T:ratnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-compnum
    (unsafe)
  (signatures
   ((T:ratnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-cflonum
    (unsafe)
  (signatures
   ((T:ratnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-compnum-fixnum
    (unsafe)
  (signatures
   ((T:compnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-bignum
    (unsafe)
  (signatures
   ((T:compnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-ratnum
    (unsafe)
  (signatures
   ((T:compnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-compnum
    (unsafe)
  (signatures
   ((T:compnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-flonum
    (unsafe)
  (signatures
   ((T:compnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-cflonum
    (unsafe)
  (signatures
   ((T:compnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-cflonum-fixnum
    (unsafe)
  (signatures
   ((T:cflonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-bignum
    (unsafe)
  (signatures
   ((T:cflonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-ratnum
    (unsafe)
  (signatures
   ((T:cflonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-flonum
    (unsafe)
  (signatures
   ((T:cflonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-compnum
    (unsafe)
  (signatures
   ((T:cflonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-cflonum
    (unsafe)
  (signatures
   ((T:cflonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-fixnum-number
    (unsafe)
  (signatures
   ((T:fixnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-bignum-number
    (unsafe)
  (signatures
   ((T:bignum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-flonum-number
    (unsafe)
  (signatures
   ((T:flonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-ratnum-number
    (unsafe)
  (signatures
   ((T:ratnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-compnum-number
    (unsafe)
  (signatures
   ((T:compnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-cflonum-number
    (unsafe)
  (signatures
   ((T:cflonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $add-number-fixnum
    (unsafe)
  (signatures
   ((T:number T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-bignum
    (unsafe)
  (signatures
   ((T:number T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-flonum
    (unsafe)
  (signatures
   ((T:number T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-ratnum
    (unsafe)
  (signatures
   ((T:number T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-compnum
    (unsafe)
  (signatures
   ((T:number T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-cflonum
    (unsafe)
  (signatures
   ((T:number T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $add-number-number
    (unsafe)
  (signatures
   ((T:number T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))


;;;; numerics, general arithmetics, subtraction

(declare-core-primitive -
    (safe)
  (signatures
   ((T:number . T:number)	=> (T:number)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $sub-fixnum-fixnum		$sub-fixnum-bignum	$sub-fixnum-flonum
   $sub-fixnum-ratnum		$sub-fixnum-compnum	$sub-fixnum-cflonum

   $sub-bignum-fixnum		$sub-bignum-bignum	$sub-bignum-flonum
   $sub-bignum-ratnum		$sub-bignum-compnum	$sub-bignum-cflonum

   $sub-flonum-fixnum		$sub-flonum-bignum	$sub-flonum-flonum
   $sub-flonum-ratnum		$sub-flonum-compnum	$sub-flonum-cflonum

   $sub-ratnum-fixnum		$sub-ratnum-bignum	$sub-ratnum-flonum
   $sub-ratnum-ratnum		$sub-ratnum-compnum	$sub-ratnum-cflonum

   $sub-compnum-fixnum		$sub-compnum-bignum	$sub-compnum-ratnum
   $sub-compnum-compnum		$sub-compnum-flonum	$sub-compnum-cflonum

   $sub-cflonum-fixnum		$sub-cflonum-bignum	$sub-cflonum-ratnum
   $sub-cflonum-flonum		$sub-cflonum-compnum	$sub-cflonum-cflonum

   $sub-fixnum-number		$sub-bignum-number	$sub-flonum-number
   $sub-ratnum-number		$sub-compnum-number	$sub-cflonum-number

   $sub-number-fixnum		$sub-number-bignum	$sub-number-flonum
   $sub-number-ratnum		$sub-number-compnum	$sub-number-cflonum

   $sub-number-number))

;;; --------------------------------------------------------------------

(declare-core-primitive $sub-fixnum-fixnum
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-fixnum-bignum
    (unsafe)
  (signatures
   ((T:fixnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-fixnum-flonum
    (unsafe)
  (signatures
   ((T:fixnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-fixnum-ratnum
    (unsafe)
  (signatures
   ((T:fixnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-fixnum-compnum
    (unsafe)
  (signatures
   ((T:fixnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-fixnum-cflonum
    (unsafe)
  (signatures
   ((T:fixnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-bignum-fixnum
    (unsafe)
  (signatures
   ((T:bignum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-bignum-bignum
    (unsafe)
  (signatures
   ((T:bignum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-bignum-flonum
    (unsafe)
  (signatures
   ((T:bignum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-bignum-ratnum
    (unsafe)
  (signatures
   ((T:bignum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-bignum-compnum
    (unsafe)
  (signatures
   ((T:bignum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-bignum-cflonum
    (unsafe)
  (signatures
   ((T:bignum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-flonum-fixnum
    (unsafe)
  (signatures
   ((T:flonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-flonum-bignum
    (unsafe)
  (signatures
   ((T:flonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-flonum-flonum
    (unsafe)
  (signatures
   ((T:flonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-flonum-ratnum
    (unsafe)
  (signatures
   ((T:flonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-flonum-compnum
    (unsafe)
  (signatures
   ((T:flonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-flonum-cflonum
    (unsafe)
  (signatures
   ((T:flonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-ratnum-fixnum
    (unsafe)
  (signatures
   ((T:ratnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-ratnum-bignum
    (unsafe)
  (signatures
   ((T:ratnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-ratnum-flonum
    (unsafe)
  (signatures ((T:ratnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-ratnum-ratnum
    (unsafe)
  (signatures
   ((T:ratnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-ratnum-compnum
    (unsafe)
  (signatures
   ((T:ratnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-ratnum-cflonum
    (unsafe)
  (signatures
   ((T:ratnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-compnum-fixnum
    (unsafe)
  (signatures
   ((T:compnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-compnum-bignum
    (unsafe)
  (signatures
   ((T:compnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-compnum-ratnum
    (unsafe)
  (signatures
   ((T:compnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-compnum-compnum
    (unsafe)
  (signatures
   ((T:compnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-compnum-flonum
    (unsafe)
  (signatures
   ((T:compnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-compnum-cflonum
    (unsafe)
  (signatures
   ((T:compnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-cflonum-fixnum
    (unsafe)
  (signatures
   ((T:cflonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-cflonum-bignum
    (unsafe)
  (signatures
   ((T:cflonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-cflonum-ratnum
    (unsafe)
  (signatures
   ((T:cflonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-cflonum-flonum
    (unsafe)
  (signatures
   ((T:cflonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-cflonum-compnum
    (unsafe)
  (signatures
   ((T:cflonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-cflonum-cflonum
    (unsafe)
  (signatures
   ((T:cflonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-fixnum-number
    (unsafe)
  (signatures
   ((T:fixnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-bignum-number
    (unsafe)
  (signatures
   ((T:bignum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-flonum-number
    (unsafe)
  (signatures
   ((T:flonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-ratnum-number
    (unsafe)
  (signatures
   ((T:ratnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-compnum-number
    (unsafe)
  (signatures
   ((T:compnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-cflonum-number
    (unsafe)
  (signatures
   ((T:cflonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $sub-number-fixnum
    (unsafe)
  (signatures
   ((T:number T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-number-bignum
    (unsafe)
  (signatures
   ((T:number T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-number-flonum
    (unsafe)
  (signatures
   ((T:number T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-number-ratnum
    (unsafe)
  (signatures
   ((T:number T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-number-compnum
    (unsafe)
  (signatures
   ((T:number T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-number-cflonum
    (unsafe)
  (signatures
   ((T:number T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $sub-number-number
    (unsafe)
  (signatures
   ((T:number T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))


;;;; numerics, general arithmetics, multiplication

(declare-core-primitive *
    (safe)
  (signatures (T:number => (T:number)))
  (attributes
   (_			foldable effect-free result-true))
  (replacements
   $mul-fixnum-fixnum		$mul-fixnum-bignum	$mul-fixnum-flonum
   $mul-fixnum-ratnum		$mul-fixnum-compnum	$mul-fixnum-cflonum

   $mul-bignum-fixnum		$mul-bignum-bignum	$mul-bignum-flonum
   $mul-bignum-ratnum		$mul-bignum-compnum	$mul-bignum-cflonum

   $mul-flonum-fixnum		$mul-flonum-bignum	$mul-flonum-flonum
   $mul-flonum-ratnum		$mul-flonum-compnum	$mul-flonum-cflonum

   $mul-ratnum-fixnum		$mul-ratnum-bignum	$mul-ratnum-flonum
   $mul-ratnum-ratnum		$mul-ratnum-compnum	$mul-ratnum-cflonum

   $mul-compnum-fixnum		$mul-compnum-bignum	$mul-compnum-ratnum
   $mul-compnum-compnum		$mul-compnum-flonum	$mul-compnum-cflonum

   $mul-cflonum-fixnum		$mul-cflonum-bignum	$mul-cflonum-ratnum
   $mul-cflonum-flonum		$mul-cflonum-compnum	$mul-cflonum-cflonum

   $mul-fixnum-number		$mul-bignum-number	$mul-flonum-number
   $mul-ratnum-number		$mul-compnum-number	$mul-cflonum-number

   $mul-number-fixnum		$mul-number-bignum	$mul-number-flonum
   $mul-number-ratnum		$mul-number-compnum	$mul-number-cflonum

   $mul-number-number))

;;; --------------------------------------------------------------------

(declare-core-primitive $mul-fixnum-fixnum
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-fixnum-bignum
    (unsafe)
  (signatures
   ((T:fixnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-fixnum-flonum
    (unsafe)
  (signatures
   ((T:fixnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-fixnum-ratnum
    (unsafe)
  (signatures
   ((T:fixnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-fixnum-compnum
    (unsafe)
  (signatures
   ((T:fixnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-fixnum-cflonum
    (unsafe)
  (signatures
   ((T:fixnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-bignum-fixnum
    (unsafe)
  (signatures
   ((T:bignum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-bignum-bignum
    (unsafe)
  (signatures
   ((T:bignum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-bignum-flonum
    (unsafe)
  (signatures
   ((T:bignum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-bignum-ratnum
    (unsafe)
  (signatures
   ((T:bignum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-bignum-compnum
    (unsafe)
  (signatures
   ((T:bignum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-bignum-cflonum
    (unsafe)
  (signatures
   ((T:bignum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-flonum-fixnum
    (unsafe)
  (signatures
   ((T:flonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-flonum-bignum
    (unsafe)
  (signatures
   ((T:flonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-flonum-flonum
    (unsafe)
  (signatures
   ((T:flonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-flonum-ratnum
    (unsafe)
  (signatures
   ((T:flonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-flonum-compnum
    (unsafe)
  (signatures
   ((T:flonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-flonum-cflonum
    (unsafe)
  (signatures
   ((T:flonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-ratnum-fixnum
    (unsafe)
  (signatures
   ((T:ratnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-ratnum-bignum
    (unsafe)
  (signatures
   ((T:ratnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-ratnum-flonum
    (unsafe)
  (signatures ((T:ratnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-ratnum-ratnum
    (unsafe)
  (signatures
   ((T:ratnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-ratnum-compnum
    (unsafe)
  (signatures
   ((T:ratnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-ratnum-cflonum
    (unsafe)
  (signatures
   ((T:ratnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-compnum-fixnum
    (unsafe)
  (signatures
   ((T:compnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-compnum-bignum
    (unsafe)
  (signatures
   ((T:compnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-compnum-ratnum
    (unsafe)
  (signatures
   ((T:compnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-compnum-compnum
    (unsafe)
  (signatures
   ((T:compnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-compnum-flonum
    (unsafe)
  (signatures
   ((T:compnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-compnum-cflonum
    (unsafe)
  (signatures
   ((T:compnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-cflonum-fixnum
    (unsafe)
  (signatures
   ((T:cflonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-cflonum-bignum
    (unsafe)
  (signatures
   ((T:cflonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-cflonum-ratnum
    (unsafe)
  (signatures
   ((T:cflonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-cflonum-flonum
    (unsafe)
  (signatures
   ((T:cflonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-cflonum-compnum
    (unsafe)
  (signatures
   ((T:cflonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-cflonum-cflonum
    (unsafe)
  (signatures
   ((T:cflonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-fixnum-number
    (unsafe)
  (signatures
   ((T:fixnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-bignum-number
    (unsafe)
  (signatures
   ((T:bignum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-flonum-number
    (unsafe)
  (signatures
   ((T:flonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-ratnum-number
    (unsafe)
  (signatures
   ((T:ratnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-compnum-number
    (unsafe)
  (signatures
   ((T:compnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-cflonum-number
    (unsafe)
  (signatures
   ((T:cflonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $mul-number-fixnum
    (unsafe)
  (signatures
   ((T:number T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-number-bignum
    (unsafe)
  (signatures
   ((T:number T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-number-flonum
    (unsafe)
  (signatures
   ((T:number T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-number-ratnum
    (unsafe)
  (signatures
   ((T:number T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-number-compnum
    (unsafe)
  (signatures
   ((T:number T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-number-cflonum
    (unsafe)
  (signatures
   ((T:number T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $mul-number-number
    (unsafe)
  (signatures
   ((T:number T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))


;;;; numerics, general arithmetics, division

(declare-core-primitive /
    (safe)
  (signatures
   ((T:number . T:number)	=> (T:number)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $div-fixnum-fixnum		$div-fixnum-bignum	$div-fixnum-flonum
   $div-fixnum-ratnum		$div-fixnum-compnum	$div-fixnum-cflonum

   $div-bignum-fixnum		$div-bignum-bignum	$div-bignum-flonum
   $div-bignum-ratnum		$div-bignum-compnum	$div-bignum-cflonum

   $div-flonum-fixnum		$div-flonum-bignum	$div-flonum-flonum
   $div-flonum-ratnum		$div-flonum-compnum	$div-flonum-cflonum

   $div-ratnum-fixnum		$div-ratnum-bignum	$div-ratnum-flonum
   $div-ratnum-ratnum		$div-ratnum-compnum	$div-ratnum-cflonum

   $div-compnum-fixnum		$div-compnum-bignum	$div-compnum-ratnum
   $div-compnum-compnum		$div-compnum-flonum	$div-compnum-cflonum

   $div-cflonum-fixnum		$div-cflonum-bignum	$div-cflonum-ratnum
   $div-cflonum-flonum		$div-cflonum-compnum	$div-cflonum-cflonum

   $div-fixnum-number		$div-bignum-number	$div-flonum-number
   $div-ratnum-number		$div-compnum-number	$div-cflonum-number

   $div-number-fixnum		$div-number-bignum	$div-number-flonum
   $div-number-ratnum		$div-number-compnum	$div-number-cflonum

   $div-number-number))

;;; --------------------------------------------------------------------

(declare-core-primitive $div-fixnum-fixnum
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-fixnum-bignum
    (unsafe)
  (signatures
   ((T:fixnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-fixnum-flonum
    (unsafe)
  (signatures
   ((T:fixnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-fixnum-ratnum
    (unsafe)
  (signatures
   ((T:fixnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-fixnum-compnum
    (unsafe)
  (signatures
   ((T:fixnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-fixnum-cflonum
    (unsafe)
  (signatures
   ((T:fixnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-bignum-fixnum
    (unsafe)
  (signatures
   ((T:bignum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-bignum-bignum
    (unsafe)
  (signatures
   ((T:bignum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-bignum-flonum
    (unsafe)
  (signatures
   ((T:bignum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-bignum-ratnum
    (unsafe)
  (signatures
   ((T:bignum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-bignum-compnum
    (unsafe)
  (signatures
   ((T:bignum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-bignum-cflonum
    (unsafe)
  (signatures
   ((T:bignum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-flonum-fixnum
    (unsafe)
  (signatures
   ((T:flonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-flonum-bignum
    (unsafe)
  (signatures
   ((T:flonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-flonum-flonum
    (unsafe)
  (signatures
   ((T:flonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-flonum-ratnum
    (unsafe)
  (signatures
   ((T:flonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-flonum-compnum
    (unsafe)
  (signatures
   ((T:flonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-flonum-cflonum
    (unsafe)
  (signatures
   ((T:flonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-ratnum-fixnum
    (unsafe)
  (signatures
   ((T:ratnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-ratnum-bignum
    (unsafe)
  (signatures
   ((T:ratnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-ratnum-flonum
    (unsafe)
  (signatures ((T:ratnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-ratnum-ratnum
    (unsafe)
  (signatures
   ((T:ratnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-ratnum-compnum
    (unsafe)
  (signatures
   ((T:ratnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-ratnum-cflonum
    (unsafe)
  (signatures
   ((T:ratnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-compnum-fixnum
    (unsafe)
  (signatures
   ((T:compnum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-compnum-bignum
    (unsafe)
  (signatures
   ((T:compnum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-compnum-ratnum
    (unsafe)
  (signatures
   ((T:compnum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-compnum-compnum
    (unsafe)
  (signatures
   ((T:compnum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-compnum-flonum
    (unsafe)
  (signatures
   ((T:compnum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-compnum-cflonum
    (unsafe)
  (signatures
   ((T:compnum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-cflonum-fixnum
    (unsafe)
  (signatures
   ((T:cflonum T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-cflonum-bignum
    (unsafe)
  (signatures
   ((T:cflonum T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-cflonum-ratnum
    (unsafe)
  (signatures
   ((T:cflonum T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-cflonum-flonum
    (unsafe)
  (signatures
   ((T:cflonum T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-cflonum-compnum
    (unsafe)
  (signatures
   ((T:cflonum T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-cflonum-cflonum
    (unsafe)
  (signatures
   ((T:cflonum T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-fixnum-number
    (unsafe)
  (signatures
   ((T:fixnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-bignum-number
    (unsafe)
  (signatures
   ((T:bignum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-flonum-number
    (unsafe)
  (signatures
   ((T:flonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-ratnum-number
    (unsafe)
  (signatures
   ((T:ratnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-compnum-number
    (unsafe)
  (signatures
   ((T:compnum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-cflonum-number
    (unsafe)
  (signatures
   ((T:cflonum T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;;

(declare-core-primitive $div-number-fixnum
    (unsafe)
  (signatures
   ((T:number T:fixnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-number-bignum
    (unsafe)
  (signatures
   ((T:number T:bignum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-number-flonum
    (unsafe)
  (signatures
   ((T:number T:flonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-number-ratnum
    (unsafe)
  (signatures
   ((T:number T:ratnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-number-compnum
    (unsafe)
  (signatures
   ((T:number T:compnum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-number-cflonum
    (unsafe)
  (signatures
   ((T:number T:cflonum) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $div-number-number
    (unsafe)
  (signatures
   ((T:number T:number) => (T:number)))
  (attributes
   ((_ _)		foldable effect-free result-true)))


;;;; numerics, general arithmetics, add1

(declare-core-primitive add1
    (safe)
  (signatures
   ((T:number)			=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $add1-integer		$add1-bignum	$add1-fixnum))

(declare-core-primitive $add1-integer
    (safe)
  (signatures
   ;;FIXME This should be T:integer.  (Marco Maggi; Wed Nov 12, 2014)
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $add1-bignum
    (safe)
  (signatures
   ((T:bignum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $add1-fixnum
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; numerics, general arithmetics, sub1

(declare-core-primitive sub1
    (safe)
  (signatures
   ((T:number)			=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $sub1-integer		$sub1-bignum	$sub1-fixnum))

(declare-core-primitive $sub1-integer
    (safe)
  (signatures
   ;;FIXME This should be T:integer.  (Marco Maggi; Wed Nov 12, 2014)
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $sub1-bignum
    (safe)
  (signatures
   ((T:bignum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $sub1-fixnum
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; numerics, general arithmetics, misc functions

(declare-type-predicate number? T:number)
(declare-type-predicate complex? T:number)
(declare-type-predicate real? T:real)
(declare-type-predicate rational?)
(declare-type-predicate integer?)

(declare-type-predicate real-valued?)
(declare-type-predicate rational-valued?)
(declare-type-predicate integer-valued?)

;;; --------------------------------------------------------------------
;;; exactness

(declare-core-primitive inexact->exact
    (safe)
  (signatures
   ((T:number)		=> (T:exact)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive exact
    (safe)
  (signatures
   ((T:number)		=> (T:exact)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive inexact
    (safe)
  (signatures
   ((T:number)		=> (T:inexact)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; parts

(declare-core-primitive make-rectangular
    (safe)
  (signatures
   ((T:real T:real)		=> (T:complex)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive make-polar
    (safe)
  (signatures
   ((T:real T:real)		=> (T:complex)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-number-unary real-part)
(declare-number-unary imag-part)

;;; --------------------------------------------------------------------
;;; predicates

(declare-number-predicate zero?)
(declare-number-predicate positive?)
(declare-number-predicate negative?)
(declare-number-predicate odd?)
(declare-number-predicate even?)
(declare-number-predicate finite?)
(declare-number-predicate infinite?)
(declare-number-predicate nan?)

;;; --------------------------------------------------------------------
;;; comparison

(let-syntax
    ((declare-real-comparison-unary/multi (syntax-rules ()
					    ((_ ?who)
					     (declare-core-primitive ?who
						 (safe)
					       (signatures
						((T:real . T:real)	=> (T:boolean)))
					       (attributes
						((_ . _)		foldable effect-free)))))))
  (declare-real-comparison-unary/multi =)
  (declare-real-comparison-unary/multi !=)
  (declare-real-comparison-unary/multi <)
  (declare-real-comparison-unary/multi >=)
  (declare-real-comparison-unary/multi <)
  (declare-real-comparison-unary/multi >=)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials and logarithms

(declare-number-binary expt)
(declare-number-unary/binary log)

;;; --------------------------------------------------------------------
;;; bitwise

(declare-core-primitive sll
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:exact-integer))
   ((T:bignum T:fixnum)		=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive sra
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:exact-integer))
   ((T:bignum T:fixnum)		=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-and
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; trigonometric

(declare-number-unary sin)
(declare-number-unary cos)
(declare-number-unary tan)
(declare-number-unary asin)
(declare-number-unary acos)
(declare-number-unary/binary atan)


;;;; numerics

(declare-core-primitive integer->char
    (safe)
  (signatures
   ((T:fixnum)		=> (T:char)))
  (attributes
   ((_)			foldable effect-free result-true)))



#|


      ((condition . _))
      ((top-level-value . _))
      ((make-message-condition . _))
      ((make-lexical-violation . _))
      ((make-who-condition . _))
      ((make-error . _))
      ((make-i/o-error . _))
      ((make-i/o-write-error . _))
      ((make-i/o-read-error . _))
      ((make-i/o-file-already-exists-error . _))
      ((make-i/o-file-is-read-only-error . _))
      ((make-i/o-file-protection-error . _))
      ((make-i/o-file-does-not-exist-error . _))
      ((make-undefined-violation . _))
      ((die . _))
      ;;This must return a new gensym every time.
      ((gensym . _)				effect-free result-true)
      ((values . _))
      ((error . _))
      ((assertion-violation . _))
      ;;FIXME Reduce to display.  (Abdulaziz Ghuloum)
      ((printf . _))
      ((newline . _))
      ((native-transcoder . _))
      ((open-string-output-port . _))
      ((open-string-input-port . _))
      ((environment . _))
      ((print-gensym . _))
      ((exit . _))
      ((display . _))
      ((write-char . _))
      ((current-input-port . _))
      ((current-output-port . _))
      ((current-error-port . _))
      ((standard-input-port . _))
      ((standard-output-port . _))
      ((standard-error-port . _))

      ((standard-input-port . _)		 effect-free result-true)
      ((standard-output-port . _)		 effect-free result-true)
      ((standard-error-port . _)		 effect-free result-true)
      ((console-input-port . _)			 effect-free result-true)
      ((console-output-port . _)		 effect-free result-true)
      ((console-error-port . _)			 effect-free result-true)
      (($current-frame . _))
      ((pretty-width . _))
      (($fp-at-base . _))
      (($collect-key . _))
      ((make-non-continuable-violation . _))

      ;;FIXME Reduce to string-copy (Abdulaziz Ghuloum).
      ((format . _))
      ((uuid . _))
      ((print-graph . _))
      ((interaction-environment . _))
      ((make-guardian)					 effect-free result-true)
      ((command-line-arguments))
      ;;FIXME (Abdulaziz Ghuloum)
      ((make-record-type-descriptor . _))
      ((record-constructor _)				 effect-free result-true)
      ((record-predicate _)				 effect-free result-true)
      ((record-accessor . _)				 effect-free result-true)
      ((record-mutator . _)				 effect-free result-true)
      ((make-assertion-violation . _))
      ((new-cafe . _))
      ((getenv . _))
      ((gensym-prefix . _))
      (($arg-list . _))
      (($make-symbol . _)				 effect-free result-true)
      (($make-call-with-values-procedure . _))
      (($make-values-procedure . _))
      (($unset-interrupted! . _))
      ((make-interrupted-condition . _))
      (($interrupted? . _))
      (($symbol-value . _))
      ((library-extensions . _))
      ;;The base struct type descriptor is a constant created at process
      ;;boot time.
      (($data->transcoder . _)			foldable effect-free result-true)
      ((current-time . _))

|#


;;;; done



;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'declare-core-primitive	'scheme-indent-function 2)
;; eval: (put 'declare-list-accessor	'scheme-indent-function 1)
;; eval: (put 'declare-list-mutator	'scheme-indent-function 1)
;; End:
