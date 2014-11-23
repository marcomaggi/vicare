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
	  ((?obj-tag)		=> (T:boolean)))
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
(define-object-predicate-declarer declare-char-predicate T:char)
(define-object-predicate-declarer declare-string-predicate T:string)
(define-object-predicate-declarer declare-keyword-predicate T:keyword)
(define-object-predicate-declarer declare-vector-predicate T:vector)


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
(define-object-binary-comparison-declarer declare-pointer-binary-comparison T:pointer)
(define-object-binary-comparison-declarer declare-char-binary-comparison T:char)
(define-object-binary-comparison-declarer declare-string-binary-comparison T:string)
(define-object-binary-comparison-declarer declare-keyword-binary-comparison T:keyword)

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
(define-object-unary/multi-comparison-declarer declare-string-unary/multi-comparison T:string)

;;; --------------------------------------------------------------------

(module (define-object-binary/multi-comparison-declarer)

  (define-syntax define-object-binary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison T:char)
    ;;   (declare-char-binary/multi-comparison char=?)
    ;;   (declare-char-binary/multi-comparison char<?)
    ;;   (declare-char-binary/multi-comparison char>?)
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
	  ((?type-tag ?type-tag)		=> (T:boolean))
	  ((?type-tag ?type-tag . ?type-tag)	=> (T:boolean)))
	 (attributes
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY/MULTI-COMPARISON-DECLARER |# )

(define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison T:char)
(define-object-binary/multi-comparison-declarer declare-string-binary/multi-comparison T:string)


;;;; syntax helpers: math operations

(module (define-object-unary-operation-declarer)

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
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
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
	  ((?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY-OPERATION-DECLARER |# )

(define-object-unary-operation-declarer declare-number-unary T:number)
(define-object-unary-operation-declarer declare-fixnum-unary T:fixnum)
(define-object-unary-operation-declarer declare-flonum-unary T:flonum)
(define-object-unary-operation-declarer declare-exact-integer-unary T:exact-integer)
(define-object-unary-operation-declarer declare-char-unary T:char)
(define-object-unary-operation-declarer declare-string-unary T:string)

;;; --------------------------------------------------------------------

(module (define-object-binary-operation-declarer)

  (define-syntax (define-object-binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-operation-declarer declare-flonum-binary T:flonum)
    ;;   (declare-flonum-binary flexpt)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
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
	  ((?type-tag ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_ _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-OPERATION-DECLARER |# )

(define-object-binary-operation-declarer declare-number-binary T:number)
(define-object-binary-operation-declarer declare-fixnum-binary T:fixnum)
(define-object-binary-operation-declarer declare-flonum-binary T:flonum)
(define-object-binary-operation-declarer declare-string-binary T:string)

;;; --------------------------------------------------------------------

(module (define-object-unary/binary-operation-declarer)

  (define-syntax (define-object-unary/binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/binary-operation-declarer declare-flonum-unary/binary T:flonum)
    ;;   (declare-flonum-unary/binary fllog)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
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
	  ((?type-tag)			=> (?type-tag))
	  ((?type-tag ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)			foldable effect-free result-true)
	  ((_ _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/BINARY-OPERATION-DECLARER |# )

(define-object-unary/binary-operation-declarer declare-number-unary/binary T:number)
(define-object-unary/binary-operation-declarer declare-fixnum-unary/binary T:fixnum)
(define-object-unary/binary-operation-declarer declare-flonum-unary/binary T:flonum)
(define-object-unary/binary-operation-declarer declare-string-unary/binary T:string)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-operation-declarer)

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
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
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
	  ((?type-tag)			=> (?type-tag))
	  ((?type-tag . ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)				foldable effect-free result-true)
	  ((_ . _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-OPERATION-DECLARER |# )

(define-object-unary/multi-operation-declarer declare-number-unary/multi T:number)
(define-object-unary/multi-operation-declarer declare-fixnum-unary/multi T:fixnum)
(define-object-unary/multi-operation-declarer declare-flonum-unary/multi T:flonum)
(define-object-unary/multi-operation-declarer declare-string-unary/multi T:string)

;;; --------------------------------------------------------------------

(module (define-object-multi-operation-declarer)

  (define-syntax (define-object-multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-multi-operation-declarer declare-fixnum-multi T:fixnum)
    ;;   (declare-fixnum-multi fxior)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
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
	  (()			=> (?type-tag))
	  (?type-tag		=> (?type-tag)))
	 (attributes
	  (()			foldable effect-free result-true)
	  ((_ . _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-MULTI-OPERATION-DECLARER |# )

(define-object-multi-operation-declarer declare-number-multi T:number)
(define-object-multi-operation-declarer declare-fixnum-multi T:fixnum)
(define-object-multi-operation-declarer declare-flonum-multi T:flonum)
(define-object-multi-operation-declarer declare-string-multi T:string)


;;;; syntax helpers: pairs, lists, alists

(module (declare-pair-accessor)

  (define-syntax declare-pair-accessor
    ;;This is for: CAR, CDR, CAAR, CADR, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-pair-accessor ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-pair-accessor ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-pair-accessor ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-pair-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-pair-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-pair-accessor ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-pair-accessor
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

(module (declare-pair-mutator)

  (define-syntax declare-pair-mutator
    ;;This is for: SET-CAR!, SET-CDR!, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-pair-mutator ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-pair-mutator ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-pair-mutator ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-pair-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-pair-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-pair-mutator ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-pair-mutator
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

  #| end of module: DECLARE-PAIR-MUTATOR |# )

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
	  ((?obj-tag T:proper-list)	=> (_)))
	 (attributes
	  ((_ _)			foldable effect-free))
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
	((?obj-tag T:null)			=> (T:false))
	((?obj-tag T:non-empty-proper-list)	=> (_)))
       (attributes
	((_ ())			foldable effect-free result-false)
	((_ _)			foldable effect-free))))
    ))


;;;; misc functions

(declare-object-binary-comparison eq?)
(declare-object-binary-comparison neq?)
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
   ((T:proper-list)	=> (T:true))
   ((T:pair)		=> (T:boolean))
   ((_)			=> (T:false)))
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
   ((_ . _)		=> (T:non-empty-proper-list)))
  (attributes
   ;;Foldable because it returns null.
   (()			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_ . _)		effect-free result-true)))

(declare-core-primitive make-list
    (safe)
  (signatures
   ((T:non-negative-fixnum)		=> (T:proper-list))
   ((T:non-negative-fixnum T:object)	=> (T:proper-list)))
  (attributes
   ;;Foldable because it returns null.
   ((0)				foldable effect-free result-true)
   ((0 _)			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive reverse
    (safe)
  (signatures
   ((T:null)			=> (T:null))
   ((T:non-empty-proper-list)	=> (T:non-empty-proper-list)))
  (attributes
   ;;This is foldable because it returns null itself.
   ((())		foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)			effect-free result-true)))

(declare-core-primitive append
    (safe)
  (signatures
   (()				=> (T:null))
   ((T:object . T:object)	=> (T:improper-list)))
  (attributes
   ;;This is foldable because it returns null itself.
   (()				foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated improper list every time.
   ((_ . _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive length
    (safe)
  (signatures
   ((T:null)			=> (T:zero))
   ((T:non-empty-proper-list)	=> (_)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;;

(declare-list-finder memq T:object)
(declare-list-finder memv T:object)
(declare-list-finder member T:object)

(declare-core-primitive memp
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (_)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

;;;

(declare-core-primitive remp
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:null))
   ((T:procedure T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ _)				result-true)))

(declare-core-primitive remq
    (safe)
  (signatures
   ((T:object T:null)			=> (T:null))
   ((T:object T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remv
    (safe)
  (signatures
   ((T:object T:null)			=> (T:null))
   ((T:object T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remove
    (safe)
  (signatures
   ((T:object T:null)			=> (T:null))
   ((T:object T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

;;;

(declare-core-primitive last-pair
    (safe)
  (signatures
   ((T:non-empty-proper-list)		=> (T:pair))
   ((T:standalone-pair)			=> (T:standalone-pair)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive list-tail
    (safe)
  (signatures
   ((T:non-empty-proper-list T:exact-integer)	=> (T:proper-list)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive list-ref
    (safe)
  (signatures
   ((T:non-empty-proper-list T:exact-integer)	=> (T:object)))
  (attributes
   ((_ _)				foldable effect-free)))

;;;

(declare-core-primitive map
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:null))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:non-empty-proper-list)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ () . ())				foldable effect-free result-true)
   ((_ _ . _)				result-true)))

(declare-core-primitive for-each
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:void))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:void)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ () . ())				foldable effect-free result-true)
   ((_ _ . _)				result-true)))

;;;

(declare-core-primitive find
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive exists
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:false))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ () . ())				foldable effect-free result-false)))

(declare-core-primitive for-all
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:true))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ () . ())				foldable effect-free result-true)))

(declare-core-primitive filter
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:null))
   ((T:procedure T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)))

(declare-core-primitive partition
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:null T:null))
   ((T:procedure T:non-empty-proper-list)	=> (T:proper-list T:proper-list)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

(declare-core-primitive fold-left
    (safe)
  (signatures
   ((T:procedure T:object T:proper-list . T:proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ _ () . ())			foldable effect-free)))

(declare-core-primitive fold-right
    (safe)
  (signatures
   ((T:procedure T:object T:proper-list . T:proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ _ () . ())			foldable effect-free)))

(declare-core-primitive andmap
    (safe)
  (signatures
   ((T:procedure T:null)						=> (T:true))
   ((T:procedure T:non-empty-proper-list)				=> (T:object))
   ((T:procedure T:null T:null)						=> (T:true))
   ((T:procedure T:non-empty-proper-list T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ () ())				foldable effect-free result-true)))

(declare-core-primitive ormap
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-pair-accessor car
  (replacements $car))

(declare-pair-accessor cdr
  (replacements $cdr))

(declare-pair-accessor caar)
(declare-pair-accessor cadr)
(declare-pair-accessor cdar)
(declare-pair-accessor cddr)
(declare-pair-accessor caaar)
(declare-pair-accessor caadr)
(declare-pair-accessor cadar)
(declare-pair-accessor caddr)
(declare-pair-accessor cdaar)
(declare-pair-accessor cdadr)
(declare-pair-accessor cddar)
(declare-pair-accessor cdddr)
(declare-pair-accessor caaaar)
(declare-pair-accessor caaadr)
(declare-pair-accessor caadar)
(declare-pair-accessor caaddr)
(declare-pair-accessor cadaar)
(declare-pair-accessor cadadr)
(declare-pair-accessor caddar)
(declare-pair-accessor cadddr)
(declare-pair-accessor cdaaar)
(declare-pair-accessor cdaadr)
(declare-pair-accessor cdadar)
(declare-pair-accessor cdaddr)
(declare-pair-accessor cddaar)
(declare-pair-accessor cddadr)
(declare-pair-accessor cdddar)
(declare-pair-accessor cddddr)

;;; --------------------------------------------------------------------
;;; mutators

(declare-pair-mutator set-car!
  (replacements $set-car!))

(declare-pair-mutator set-cdr!
  (replacements $set-cdr!))

;;; --------------------------------------------------------------------
;;; associative lists

(declare-alist-accessor assq T:object)
(declare-alist-accessor assv T:object)
(declare-alist-accessor assoc T:object)

(declare-core-primitive assp
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (_)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; weak pairs

(declare-core-primitive weak-cons
    (safe)
  (signatures
   ((_ _)		=> (T:pair)))
  (attributes
   ;;This is not foldable because it must return a newly allocated pair every time.
   ((_ _)		effect-free result-true)))

(declare-core-primitive weak-pair?
    (safe)
  (signatures
   ((T:null)		=> (T:false))
   ((T:pair)		=> (T:boolean))
   ((_)			=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive list->string
    (safe)
  (signatures
   ((T:proper-list)		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)				effect-free result-true)))

(declare-core-primitive list->vector
    (safe)
  (signatures
   ((T:proper-list)		=> (T:vector)))
  (attributes
   ;;Not foldable because it must return a new vector every time.
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive make-queue-procs
    (safe)
  (signatures
   (()				=> (T:procedure T:procedure T:procedure))
   ((T:proper-list)		=> (T:procedure T:procedure T:procedure)))
  (attributes
   (()				effect-free)
   ((_)				effect-free)))


;;;; pairs and lists, unsafe functions

(declare-pair-accessor $car unsafe)
(declare-pair-accessor $cdr unsafe)

(declare-pair-mutator $set-car! unsafe)
(declare-pair-mutator $set-cdr! unsafe)


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

(declare-fixnum-predicate fixnum-in-character-range?)

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

(declare-core-primitive fx+/carry
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum) => (T:fixnum T:fixnum)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive fx-/carry
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum) => (T:fixnum T:fixnum)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive fx*/carry
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum) => (T:fixnum T:fixnum)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive fxdiv-and-mod
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive fxdiv0-and-mod0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------
;;; bitwise operations

(declare-fixnum-unary fxnot		(replacements $fxlognot))
(declare-fixnum-multi fxand		(replacements $fxlogand))
(declare-fixnum-multi fxior		(replacements $fxlogor))
(declare-fixnum-multi fxxor		(replacements $fxlogxor))
(declare-fixnum-unary fxlognot		(replacements $fxlognot))
(declare-fixnum-multi fxlogor		(replacements $fxlogor))
(declare-fixnum-multi fxlogand		(replacements $fxlogand))
(declare-fixnum-multi fxlogxor		(replacements $fxlogxor))

(declare-fixnum-unary fxlength)
(declare-fixnum-binary fxsll)
(declare-fixnum-binary fxsra)
(declare-fixnum-binary fxarithmetic-shift-left)
(declare-fixnum-binary fxarithmetic-shift-right)
(declare-fixnum-binary fxarithmetic-shift)

(declare-fixnum-unary fxbit-count)

(declare-core-primitive fxbit-field
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxbit-set?
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:boolean)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive fxreverse-bit-field
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxcopy-bit
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxcopy-bit-field
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _ _)					foldable effect-free result-true)))

(declare-core-primitive fxrotate-bit-field
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxif
    (safe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-fixnum-unary fxfirst-bit-set)

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

(declare-core-primitive fixnum->string
    (safe)
  (signatures
   ((T:fixnum)		=> (T:string))
   ((T:fixnum T:fixnum)	=> (T:string)))
  (attributes
   ((_)			foldable effect-free result-true)
   ((_ _)		foldable effect-free result-true)))


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

(declare-core-primitive $fxdiv-and-mod
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive $fxdiv0-and-mod0
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:fixnum T:fixnum)))
  (attributes
   ((_ _)			effect-free)))

(declare-fixnum-unary $fxadd1 unsafe)
(declare-fixnum-unary $fxsub1 unsafe)
(declare-fixnum-unary $fxabs unsafe)
(declare-fixnum-unary $fxsign unsafe)
(declare-fixnum-binary $fxremainder unsafe)
(declare-fixnum-binary $fxquotient unsafe)
(declare-fixnum-binary $fxmodulo unsafe)

(declare-fixnum-binary $int-quotient unsafe)
(declare-fixnum-binary $int-remainder unsafe)

;;; --------------------------------------------------------------------
;;; bitwise operations

(declare-fixnum-binary $fxlogor unsafe)
(declare-fixnum-binary $fxlogand unsafe)
(declare-fixnum-binary $fxlogxor unsafe)
(declare-fixnum-unary $fxnot unsafe)
(declare-fixnum-binary $fxsll unsafe)
(declare-fixnum-binary $fxsra unsafe)

(declare-core-primitive $fxcopy-bit
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive $fxcopy-bit-field
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _ _)					foldable effect-free result-true)))

(declare-core-primitive $fxrotate-bit-field
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _ _)					foldable effect-free result-true)))

(declare-core-primitive $fxbit-field
    (unsafe)
  (signatures
   ((T:fixnum T:fixnum T:fixnum)	=> (T:fixnum)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

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

(declare-core-primitive $fixnum->string
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:string))
   ((T:fixnum T:fixnum)		=> (T:string)))
  (attributes
   ((_)				foldable effect-free result-true)
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $fxinthash
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:fixnum)))
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

 (declare-core-primitive flinteger?
     (safe)
   (signatures
    ((T:flonum-integer)		=> (T:true))
    ((T:flonum-fractional)	=> (T:false))
    ((T:flonum-infinite)	=> (T:false))
    ((T:flonum-nan)		=> (T:false))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flonum-integer?))

 (declare-core-primitive flfinite?
     (safe)
   (signatures
    ((T:flonum-integer)		=> (T:true))
    ((T:flonum-fractional)	=> (T:true))
    ((T:flonum-infinite)	=> (T:false))
    ((T:flonum-nan)		=> (T:false))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flonum-rational?))

 (declare-core-primitive flinfinite?
     (safe)
   (signatures
    ((T:flonum-integer)		=> (T:false))
    ((T:flonum-fractional)	=> (T:false))
    ((T:flonum-infinite)	=> (T:true))
    ((T:flonum-nan)		=> (T:false))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flinfinite?))

 (declare-core-primitive flnan?
     (safe)
   (signatures
    ((T:flonum-integer)		=> (T:false))
    ((T:flonum-fractional)	=> (T:false))
    ((T:flonum-infinite)	=> (T:false))
    ((T:flonum-nan)		=> (T:true))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flnan?))

;;; --------------------------------------------------------------------
;;; rounding

 (let-syntax
     ((declare-flonum-rounding (syntax-rules ()
				 ((_ ?who ?replacement)
				  (declare-core-primitive ?who
				      (safe)
				    (signatures
				     ((T:flonum)	=> (T:flonum-integer)))
				    (attributes
				     ((_)		foldable effect-free result-true))
				    (replacements ?replacement))))))
   (declare-flonum-rounding flround	 $flround)
   (declare-flonum-rounding flfloor	 $flfloor)
   (declare-flonum-rounding flceiling	 $flceiling)
   (declare-flonum-rounding fltruncate	 $fltruncate)
   #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; parts

 (declare-flonum-unary flnumerator	(replacements $flnumerator))
 (declare-flonum-unary fldenominator	(replacements $fldenominator))
 (declare-flonum-unary flabs		(replacements $flabs))

 (declare-core-primitive flonum-bytes
     (safe)
   (signatures
    ((T:flonum)				=> (T:flonum T:flonum T:flonum T:flonum
						     T:flonum T:flonum T:flonum T:flonum)))
   (attributes
    ((_)				effect-free)))

 (declare-core-primitive flonum-parts
     (safe)
   (signatures
    ((T:flonum)				=> (T:boolean T:fixnum T:exact-integer)))
   (attributes
    ((_)				effect-free)))

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
    ((T:flonum T:flonum)	=> (T:flonum)))
   (attributes
    ((_)			foldable effect-free result-true)
    ((_ _)			foldable effect-free result-true))
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

 (declare-core-primitive fldiv-and-mod
     (safe)
   (signatures
    ((T:flonum T:flonum)		=> (T:flonum T:flonum)))
   (attributes
    ((_ _)				effect-free)))

 (declare-core-primitive fldiv0-and-mod0
     (safe)
   (signatures
    ((T:flonum T:flonum)		=> (T:flonum T:flonum)))
   (attributes
    ((_ _)				effect-free)))

;;; --------------------------------------------------------------------
;;; conversion

 (declare-core-primitive flonum->string
     (safe)
   (signatures
    ((T:flonum)		=> (T:string)))
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
    ((_)			foldable effect-free result-true)))

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

 (declare-core-primitive $flonum-integer?
     (unsafe)
   (signatures
    ((T:flonum-integer)		=> (T:true))
    ((T:flonum-fractional)	=> (T:false))
    ((T:flonum-infinite)	=> (T:false))
    ((T:flonum-nan)		=> (T:false))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free)))

 (declare-core-primitive $flonum-rational?
     (unsafe)
   (signatures
    ((T:flonum-integer)		=> (T:true))
    ((T:flonum-fractional)	=> (T:true))
    ((T:flonum-infinite)	=> (T:false))
    ((T:flonum-nan)		=> (T:false))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free)))

 (declare-core-primitive $flinfinite?
     (unsafe)
   (signatures
    ((T:flonum-integer)		=> (T:false))
    ((T:flonum-fractional)	=> (T:false))
    ((T:flonum-infinite)	=> (T:true))
    ((T:flonum-nan)		=> (T:false))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free)))

 (declare-core-primitive $flnan?
     (unsafe)
   (signatures
    ((T:flonum-integer)		=> (T:false))
    ((T:flonum-fractional)	=> (T:false))
    ((T:flonum-infinite)	=> (T:false))
    ((T:flonum-nan)		=> (T:true))
    ((T:flonum)			=> (T:boolean)))
   (attributes
    ((_)			foldable effect-free)))

;;; --------------------------------------------------------------------
;;; rounding

 (let-syntax
     ((declare-flonum-rounding (syntax-rules ()
				 ((_ ?who)
				  (declare-core-primitive ?who
				      (unsafe)
				    (signatures
				     ((T:flonum)	=> (T:flonum-integer)))
				    (attributes
				     ((_)		foldable effect-free result-true)))))))
   (declare-flonum-rounding $flround)
   (declare-flonum-rounding $flfloor)
   (declare-flonum-rounding $flceiling)
   (declare-flonum-rounding $fltruncate)
   #| end of LET-SYNTAX |# )

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
 (declare-flonum-binary $flexpt unsafe)
 (declare-flonum-unary $flsqrt unsafe)
 (declare-flonum-unary $flsquare unsafe)
 (declare-flonum-unary $flcube unsafe)
 (declare-flonum-unary $flcbrt unsafe)
 (declare-flonum-binary $flhypot unsafe)

;;; --------------------------------------------------------------------
;;; comparison

 (declare-flonum-binary-comparison $fl=  unsafe)
 (declare-flonum-binary-comparison $fl!= unsafe)
 (declare-flonum-binary-comparison $fl<  unsafe)
 (declare-flonum-binary-comparison $fl>  unsafe)
 (declare-flonum-binary-comparison $fl<= unsafe)
 (declare-flonum-binary-comparison $fl>= unsafe)

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

 (declare-core-primitive $fldiv-and-mod
     (unsafe)
   (signatures
    ((T:flonum T:flonum)	=> (T:flonum T:flonum)))
   (attributes
    ((_ _)			effect-free)))

 (declare-core-primitive $fldiv0-and-mod0
     (unsafe)
   (signatures
    ((T:flonum T:flonum)	=> (T:flonum T:flonum)))
   (attributes
    ((_ _)			effect-free)))

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


;;;; symbols, safe primitives

(declare-type-predicate symbol? T:symbol)

(declare-core-primitive symbol->string
    (safe)
  (signatures
   ((T:symbol)			=> (T:string)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive symbol=?
    (safe)
  (signatures
   ((T:symbol T:symbol)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; gensyms

(declare-type-predicate gensym?)

(declare-core-primitive gensym
    (safe)
  (signatures
   (()				=> (T:symbol))
   ((T:symbol)			=> (T:symbol))
   ((T:string)			=> (T:symbol)))
  (attributes
   ;;It must return a new gensym every time.
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive gensym->unique-string
    (safe)
  (signatures
   ((T:symbol)			=> (T:string)))
  (attributes
   ;;Once a  gensym has been  created, its unique  string is determined  forever.  So
   ;;this is foldable.
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive symbol-bound?
    (safe)
  (signatures
   ((T:symbol)			=> (T:boolean)))
  (attributes
   ;;Being bound or not is a run-time property; this is *not* foldable.
   ((_)				effect-free)))

(declare-core-primitive top-level-value
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive reset-symbol-proc!
    (safe)
  (signatures
   ((T:symbol)			=> (T:void)))
  (attributes
   ((_)				result-true)))

(declare-core-primitive set-symbol-value!
    (safe)
  (signatures
   ((T:symbol T:object)		=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive symbol-value
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-value
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-label
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-id
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

;;FIXME These  are variable  bindings.  We  should convert  them into  functions, for
;;uniformity of syntax.  (Marco Maggi; Thu Nov 20, 2014)
;;
;; (declare-core-primitive system-value-gensym
;;     (safe)
;;   (signatures
;;    (()				=> (T:symbol)))
;;   (attributes
;;    (()				effect-free result-true)))
;;
;; (declare-core-primitive system-label-gensym
;;     (safe)
;;   (signatures
;;    (()				=> (T:symbol)))
;;   (attributes
;;    (()				effect-free result-true)))
;;
;; (declare-core-primitive system-id-gensym
;;     (safe)
;;   (signatures
;;    (()				=> (T:symbol)))
;;   (attributes
;;    (()				effect-free result-true)))

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

;;; --------------------------------------------------------------------
;;; printing gensyms

(declare-core-primitive print-gensym
    (safe)
  (signatures
   (()				=> (T:object))
   ((T:object)			=> (T:void))
   ((T:object T:object)		=> (T:void)))
  (attributes
   (()				effect-free)
   ((_)				result-true)
   ((_ _)			result-true)))

(declare-core-primitive gensym-count
    (safe)
  (signatures
   (()				=> (T:exact-integer))
   ((T:exact-integer)		=> (T:void))
   ((T:exact-integer T:object)	=> (T:void)))
  (attributes
   (()				effect-free result-true)
   ((_)				result-true)
   ((_ _)			result-true)))

(declare-core-primitive gensym-prefix
    (safe)
  (signatures
   (()				=> (T:string))
   ((T:string)			=> (T:void))
   ((T:string T:object)		=> (T:void)))
  (attributes
   (()				effect-free result-true)
   ((_)				result-true)
   ((_ _)			result-true)))


;;;; symbols, unsafe primitives

(declare-core-primitive $make-symbol
    (unsafe)
  (signatures
   ((T:false)			=> (T:symbol))
   ((T:string)			=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
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


;;;; keywords, safe functions

(declare-type-predicate keyword? T:keyword)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive symbol->keyword
    (safe)
  (signatures
   ((T:symbol)			=> (T:keyword)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-keyword-binary-comparison keyword=?)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive keyword->symbol
    (safe)
  (signatures
   ((T:keyword)			=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive keyword->string
    (safe)
  (signatures
   ((T:keyword)			=> (T:string)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive keyword-hash
    (safe)
  (signatures
   ((T:keyword)			=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))



;;;; keywords, unsafe functions

;;; constructors

(declare-core-primitive $symbol->keyword
    (unsafe)
  (signatures
   ((T:symbol)			=> (T:keyword)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-keyword-binary-comparison $keyword=? unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $keyword->symbol
    (unsafe)
  (signatures
   ((T:keyword)			=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $keyword->string
    (unsafe)
  (signatures
   ((T:keyword)			=> (T:string)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $keyword-hash
    (unsafe)
  (signatures
   ((T:keyword)			=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))


;;;; characters safe operations

;;; predicates

(declare-type-predicate char? T:char)

(declare-char-predicate char-in-ascii-range?)
(declare-char-predicate char-alphabetic?)
(declare-char-predicate char-lower-case?)
(declare-char-predicate char-numeric?)
(declare-char-predicate char-title-case?)
(declare-char-predicate char-upper-case?)
(declare-char-predicate char-whitespace?)
(declare-char-predicate unicode-printable-char?)

;;; --------------------------------------------------------------------
;;; comparison

(declare-char-binary/multi-comparison char=?		(replacements $char=))
(declare-char-binary/multi-comparison char!=?		(replacements $char!=))
(declare-char-binary/multi-comparison char<?		(replacements $char<))
(declare-char-binary/multi-comparison char>?		(replacements $char>))
(declare-char-binary/multi-comparison char<=?		(replacements $char<=))
(declare-char-binary/multi-comparison char>=?		(replacements $char>=))

(declare-char-binary/multi-comparison char-ci=?)
(declare-char-binary/multi-comparison char-ci!=?)
(declare-char-binary/multi-comparison char-ci<?)
(declare-char-binary/multi-comparison char-ci>?)
(declare-char-binary/multi-comparison char-ci<=?)
(declare-char-binary/multi-comparison char-ci>=?)

;;; --------------------------------------------------------------------
;;; transformations

(declare-char-unary char-downcase)
(declare-char-unary char-foldcase)
(declare-char-unary char-titlecase)
(declare-char-unary char-upcase)
(declare-char-unary char-general-category)

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

(declare-char-binary-comparison $char=		unsafe)
(declare-char-binary-comparison $char!=		unsafe)
(declare-char-binary-comparison $char>		unsafe)
(declare-char-binary-comparison $char<		unsafe)
(declare-char-binary-comparison $char>=		unsafe)
(declare-char-binary-comparison $char<=		unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $char->fixnum
    (unsafe)
  (signatures
   ((T:char)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; pointers, safe functions

(declare-type-predicate pointer? T:pointer)

;;; --------------------------------------------------------------------

(declare-core-primitive pointer-null?
    (safe)
  (signatures
   ((T:pointer)			=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive null-pointer
    (safe)
  (signatures
   (()				=> (T:pointer)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive set-pointer-null!
    (safe)
  (signatures
   ((T:pointer)			=> (T:void)))
  (attributes
   ((_)				result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive pointer-clone
    (safe)
  (signatures
   ((T:pointer)			=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive pointer-diff
    (safe)
  (signatures
   ((T:pointer T:pointer)	=> (T:exact-integer)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive pointer-add
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive pointer-and-offset?
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:boolean)))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------

(declare-pointer-binary-comparison pointer=?)
(declare-pointer-binary-comparison pointer!=?)
(declare-pointer-binary-comparison pointer<?)
(declare-pointer-binary-comparison pointer>?)
(declare-pointer-binary-comparison pointer<=?)
(declare-pointer-binary-comparison pointer>=?)

;;; --------------------------------------------------------------------

(declare-core-primitive pointer->integer
    (safe)
  (signatures
   ((T:pointer)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive integer->pointer
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; pointers, unsafe functions

(declare-pointer-binary-comparison $pointer= unsafe)


;;;; strings, safe functions
;;
;;According to R6RS:  STRING and MAKE-STRING must return a  newly allocated string at
;;every invocation;  if we want the  same string we  just use the double  quotes.  So
;;STRING and MAKE-STRING are not FOLDABLE.
;;

;;; predicates

(declare-type-predicate string? T:string)

(declare-string-predicate string-empty?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive string
    (safe)
  (signatures
   (()			=> (T:string))
   (T:char		=> (T:string)))
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

(declare-core-primitive substring
    (safe)
  (signatures
   ((T:string T:fixnum T:fixnum)	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive string-copy
    (safe)
  (signatures
   ((T:string)		=> (T:void)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string-copy!
    (safe)
  (signatures
   ((T:string T:fixnum T:string T:fixnum T:fixnum)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive string-append
    (safe)
  (signatures
   (T:string			=> (T:string)))
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive string-reverse-and-concatenate
    (safe)
  (signatures
   ((T:proper-list)		=> (T:string)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive string-length
    (safe)
  (signatures
   ((T:string)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

(declare-core-primitive string-for-each
    (safe)
  (signatures
   ((T:procedure T:string . T:string)		=> (T:void)))
  (attributes
   ;;Not foldable and not effect-free because it applies an unknown procedure.
   ((_ _ . _)					result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

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

(declare-core-primitive string-fill!
    (safe)
  (signatures
   ((T:string T:char)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-string-binary/multi-comparison string<=?)
(declare-string-binary/multi-comparison string<?)
(declare-string-binary/multi-comparison string=?)
(declare-string-binary/multi-comparison string>=?)
(declare-string-binary/multi-comparison string>?)

(declare-string-binary/multi-comparison string-ci<=?)
(declare-string-binary/multi-comparison string-ci<?)
(declare-string-binary/multi-comparison string-ci=?)
(declare-string-binary/multi-comparison string-ci>=?)
(declare-string-binary/multi-comparison string-ci>?)

;;; --------------------------------------------------------------------
;;; transformation

(declare-string-unary string-titlecase)
(declare-string-unary string-upcase)
(declare-string-unary string-downcase)
(declare-string-unary string-foldcase)

(declare-string-unary string-normalize-nfc)
(declare-string-unary string-normalize-nfd)
(declare-string-unary string-normalize-nfkc)
(declare-string-unary string-normalize-nfkd)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive string->flonum
    (safe)
  (signatures
   ((T:string)		=> (T:flonum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->number
    (safe)
  (signatures
   ((T:string)		=> (T:number/false))
   ((T:string T:fixnum)	=> (T:number/false)))
  (attributes
   ((_)			foldable effect-free)
   ((_ _)		foldable effect-free)))

(declare-core-primitive string->utf8
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf16
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector))
   ((T:string T:symbol)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->utf16be
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf16le
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf16n
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf32
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->ascii
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->latin1
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->bytevector
    (safe)
  (signatures
   ((T:string T:transcoder)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->symbol
    (safe)
  (signatures
   ((T:string)			=> (T:symbol)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string-or-symbol->string
    (safe)
  (signatures
   ((T:string)			=> (T:string))
   ((T:symbol)			=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string-or-symbol->symbol
    (safe)
  (signatures
   ((T:string)			=> (T:symbol))
   ((T:symbol)			=> (T:symbol)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string->keyword
    (safe)
  (signatures
   ((T:string)			=> (T:other-object)))
  (attributes
   ;;Not foldable because keywords cannot be serialised in fasl files.
   ((_)				effect-free result-true)))

(declare-core-primitive string->list
    (safe)
  (signatures
   ((T:string)			=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))


;;;; strings, unsafe functions

;;; predicates

(declare-string-predicate $string-empty?)
(declare-string-predicate $octets-encoded-string?)
(declare-string-predicate $ascii-encoded-string?)
(declare-string-predicate $latin1-encoded-string?)
(declare-string-predicate $uri-encoded-string?)
(declare-string-predicate $percent-encoded-string?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive $make-string
    (unsafe)
  (signatures
   ((T:fixnum)		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string
    (unsafe)
  (signatures
   (T:char		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   (_			effect-free result-true)))

(declare-core-primitive $string-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:string)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $string-reverse-and-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:string)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((T:string)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-total-length
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $string-ref
    (unsafe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (unsafe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-string-binary-comparison $string=)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $string->ascii
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->octets
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->latin1
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string-base64->bytevector
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->symbol
    (unsafe)
  (signatures
   ((T:string)		=> (T:symbol)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $interned-strings
    (unsafe)
  (signatures
   (()			=> (T:vector)))
  (attributes
   ((_)			effect-free result-true)))


;;;; vectors, safe functions
;;
;;According to R6RS:  VECTOR and MAKE-VECTOR must return a  newly allocated string at
;;every invocation; so they are not foldable.
;;

;;; predicates

(declare-type-predicate vector? T:vector)

(declare-vector-predicate vector-empty?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive vector
    (safe)
  (signatures
   (()				=> (T:vector))
   (_				=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (()				effect-free result-true)
   (_				effect-free result-true)))

(declare-core-primitive subvector
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive make-vector
    (safe)
  (signatures
   ((T:fixnum)			=> (T:vector))
   ((T:fixnum _)		=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive vector-resize
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum)		=> (T:vector))
   ((T:vector T:non-negative-fixnum T:object)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive vector-append
    (safe)
  (signatures
   (T:vector			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive vector-copy
    (safe)
  (signatures
   ((T:vector)			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive vector-length
    (safe)
  (signatures
   ((T:vector)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $vector-length))

;;; --------------------------------------------------------------------
;;; accessors and mutators

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

(declare-core-primitive vector-copy!
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum T:vector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  ;;Not foldable  because it must return  a newly allocated vector.   Not effect free
  ;;because it mutates the operand.
  (attributes
   ((_ _ _ _ _)			result-true)))

(declare-core-primitive vector-fill!
    (safe)
  (signatures
   ((T:vector T:object)		=> (T:void)))
  ;;Not effect free because it mutates the operand.
  (attributes
   ((_ _)			foldable result-true)))

;;; --------------------------------------------------------------------
;;; sorting

(declare-core-primitive vector-sort
    (safe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free because it invokes an unknown procedure.
   ((_ _)			result-true)))

(declare-core-primitive vector-sort!
    (safe)
  (signatures
   ((T:procedure T:vector)	=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free because it invokes an unknown procedure and it mutates the operand.
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive vector-for-each
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-map
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:vector)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-all
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-exists
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive vector->list
    (safe)
  (signatures
   ((T:vector)			=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))


;;;; vectors, unsafe functions

;;; constructors

(declare-core-primitive $make-vector
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-vector-predicate $vector-empty? unsafe)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $vector-length
    (unsafe)
  (signatures
   ((T:vector)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $vector-ref
    (unsafe)
  (signatures
   ((T:vector T:fixnum)		=> (T:char)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $vector-set!
    (unsafe)
  (signatures
   ((T:vector T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive $vector-map1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-each1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-all1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:object))))

(declare-core-primitive $vector-exists1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector))))


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


;;;; structs, safe primitives

(declare-core-primitive struct?
    (unsafe)
  (signatures
   ((T:struct)				=> (T:true))
   ((_)					=> (T:boolean))
   ((T:struct T:struct-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)					foldable effect-free)
   ((_ _)				foldable effect-free)))

(declare-type-predicate struct-type-descriptor? T:struct-type-descriptor)



;;;; structs, unsafe primitives

;;; constructors

;;The base struct type descriptor is a constant created at process boot time.
(declare-core-primitive base-rtd
    (unsafe)
  (signatures
   (()					=> (T:struct-type-descriptor)))
  (attributes
   (()					effect-free result-true)))

(declare-core-primitive $struct
    (unsafe)
  (signatures
   ((T:struct-type-descriptor . _)	=> (T:struct)))
  (attributes
   ;;It must return a new struct every time.
   ((_ . _)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-core-primitive $struct/rtd?
    (unsafe)
  (signatures
   ((_ T:struct-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_ _)				foldable effect-free)))

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
   ((T:input-port)		=> (_))))

(declare-core-primitive annotation-expression
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))

(declare-core-primitive annotation-stripped
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes
   ((#f)			foldable effect-free result-false)))

(declare-core-primitive annotation-textual-position
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))

(declare-core-primitive annotation-source
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))


;;;; R6RS records

(declare-type-predicate record? T:record)

;;; --------------------------------------------------------------------

(declare-core-primitive record-constructor
    (safe)
  (signatures
   ((_)				=> (T:procedure)))
  (attributes
   ((_)				effect-free result-false)))

(declare-core-primitive record-predicate
    (safe)
  (signatures
   ((_)				=> (T:procedure)))
  (attributes
   ((_)				effect-free result-false)))

(declare-core-primitive record-accessor
    (safe)
  (signatures
   ((_ T:fixnum)		=> (T:procedure))
   ((_ T:fixnum T:false)	=> (T:procedure))
   ((_ T:fixnum T:symbol)	=> (T:procedure))
   ((_ T:symbol)		=> (T:procedure))
   ((_ T:symbol T:false)	=> (T:procedure))
   ((_ T:symbol T:symbol)	=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-false)
   ((_ _ _)			effect-free result-false)))

(declare-core-primitive record-mutator
    (safe)
  (signatures
   ((_ T:fixnum)		=> (T:procedure))
   ((_ T:fixnum T:false)	=> (T:procedure))
   ((_ T:fixnum T:symbol)	=> (T:procedure))
   ((_ T:symbol)		=> (T:procedure))
   ((_ T:symbol T:false)	=> (T:procedure))
   ((_ T:symbol T:symbol)	=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-false)
   ((_ _ _)			effect-free result-false)))


;;;; hashtables, safe procedures

;;; predicates

(declare-type-predicate hashtable? T:hashtable)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-hashtable
    (safe)
  (signatures
   ((T:procedure T:procedure)			=> (T:hashtable))
   ((T:procedure T:procedure T:exact-integer)	=> (T:hashtable)))
  (attributes
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive make-eq-hashtable
    (safe)
  (signatures
   (()					=> (T:hashtable))
   ((T:exact-integer)			=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive make-eqv-hashtable
    (safe)
  (signatures
   (()					=> (T:hashtable))
   ((T:exact-integer)			=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-copy
    (safe)
  (signatures
   ((T:hashtable)			=> (T:hashtable))
   ((T:hashtable T:object)		=> (T:hashtable)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive hashtable-ref
    (safe)
  (signatures
   ((T:hashtable T:object T:object)	=> (T:object)))
  (attributes
   ((_ _ _)			effect-free)))

(declare-core-primitive hashtable-set!
    (safe)
  (signatures
   ((T:hashtable T:object T:object)	=> (T:void)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive hashtable-delete!
    (safe)
  (signatures
   ((T:hashtable T:object)		=> (T:void)))
  (attributes
   ((_ _) 				result-true)))

(declare-core-primitive hashtable-clear!
    (safe)
  (signatures
   ((T:hashtable)			=> (T:void))
   ((T:hashtable T:exact-integer)	=> (T:void)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)))

(declare-core-primitive hashtable-update!
    (safe)
  (signatures
   ((T:hashtable T:object T:procedure T:object)	=> (T:void)))
  (attributes
   ((_ _ _ _)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive hashtable-contains?
    (safe)
  (signatures
   ((T:hashtable T:object)	=> (T:boolean)))
  (attributes
   ((_ _)			effect-free)))


(declare-core-primitive hashtable-entries
    (safe)
  (signatures
   ((T:hashtable)		=> (T:vector T:vector)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-keys
    (safe)
  (signatures
   ((T:hashtable)		=> (T:vector)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-mutable?
    (safe)
  (signatures
   ((T:hashtable)		=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-size
    (safe)
  (signatures
   ((T:hashtable)		=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-hash-function
    (safe)
  (signatures
   ((T:hashtable)		=> (T:procedure)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-equivalence-function
    (safe)
  (signatures
   ((T:hashtable)		=> (T:object)))
  (attributes
   ;;This returns false for EQ? and EQV? hashtables!!!
   ((_)				effect-free)))

;;; --------------------------------------------------------------------
;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag . ?replacements)
			       (declare-core-primitive ?who
				   (safe)
				 (signatures
				  ((?obj-tag)		=> (T:fixnum)))
				 (attributes
				  ((_)			foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function equal-hash		T:object)
  (declare-hash-function string-hash		T:string	(replacements $string-hash))
  (declare-hash-function string-ci-hash		T:string	(replacements $string-ci-hash))
  (declare-hash-function symbol-hash		T:symbol	(replacements $symbol-hash))
  (declare-hash-function bytevector-hash	T:bytevector	(replacements $bytevector-hash))
  (declare-hash-function port-hash		T:port)
  #| end of LET-SYNTAX |# )


;;;; hashtables, unsafe procedures

;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag)
			       (declare-core-primitive ?who
				   (unsafe)
				 (signatures
				  ((?obj-tag)		=> (T:fixnum)))
				 (attributes
				  ((_)			foldable effect-free result-true))))
			      )))
  (declare-hash-function $string-hash		T:string)
  (declare-hash-function $string-ci-hash	T:string)
  (declare-hash-function $symbol-hash		T:symbol)
  (declare-hash-function $bytevector-hash	T:bytevector)
  #| end of LET-SYNTAX |# )


;;;; input/output, safe primitives

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

;;; --------------------------------------------------------------------

(declare-core-primitive standard-input-port
    (safe)
  (signatures
   (()				=> (T:binary-input-port)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive standard-output-port
    (safe)
  (signatures
   (()				=> (T:binary-output-port)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive standard-error-port
    (safe)
  (signatures
   (()				=> (T:binary-output-port)))
  (attributes
   (()				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive console-input-port
    (safe)
  (signatures
   (()				=> (T:textual-input-port)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive console-output-port
    (safe)
  (signatures
   (()				=> (T:textual-output-port)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive console-error-port
    (safe)
  (signatures
   (()				=> (T:textual-output-port)))
  (attributes
   (()				effect-free result-true)))


;;;; input/output, safe primitives

;;; transcoders

(declare-core-primitive $data->transcoder
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:transcoder)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $transcoder->data
    (unsafe)
  (signatures
   ((T:transcoder)		=> (T:fixnum)))
  (attributes
   ((_)				effect-free result-true)))


;;;; generic functions

(declare-type-predicate procedure? T:procedure)

;;; --------------------------------------------------------------------

(declare-core-primitive eof-object
    (safe)
  (signatures
   (()				=> (T:object)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive eof-object?
    (safe)
  (signatures
   ((_)				=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive bwp-object
    (safe)
  (signatures
   (()				=> (T:object)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive bwp-object?
    (safe)
  (signatures
   ((_)				=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive unbound-object
    (safe)
  (signatures
   (()				=> (T:object)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive unbound-object?
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
   ((T:procedure T:boolean)	=> (T:void))))

(declare-core-primitive engine-handler
    (safe)
  (signatures
   (()				=> (T:procedure))
   ((T:procedure)		=> (T:void))
   ((T:procedure T:boolean)	=> (T:void))))

;;; --------------------------------------------------------------------

(declare-core-primitive make-guardian
    (safe)
  (signatures
   (()				=> (T:procedure)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive make-parameter
    (safe)
  (signatures
   ((T:object)			=> (T:procedure))
   ((T:object T:procedure)	=> (T:procedure)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive always-true
    (safe)
  (signatures
   (_				=> (T:true)))
  (attributes
   (_				foldable effect-free result-true)))

(declare-core-primitive always-false
    (safe)
  (signatures
   (_				=> (T:false)))
  (attributes
   (_				foldable effect-free result-false)))

;;; --------------------------------------------------------------------

(declare-core-primitive uuid
    (safe)
  (signatures
   (()				=> (T:string)))
  (attributes
   (()				effect-free result-false)))


;;;; invocation and termination procedures

(declare-core-primitive command-line
    (safe)
  (signatures
   (()				=> (T:non-empty-proper-list)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive exit
    (safe)
  (signatures
   (()				=> (T:void))
   ((T:fixnum)			=> (T:void))))

(declare-core-primitive exit-hooks
    (safe)
  (signatures
   (()				=> (T:proper-list))
   ((T:proper-list)		=> (T:void))
   ((T:proper-list T:object)	=> (T:void)))
  (attributes
   (()				effect-free result-true)
   ((_)				result-true)
   ((_ _)			result-true)))


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


;;;; system interface and foreign functions

(declare-core-primitive errno
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:void)))
  (attributes
   (()			effect-free result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive malloc
    (safe)
  (signatures
   ((T:exact-integer)			=> (T:pointer/false))))

(declare-core-primitive realloc
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer/false))))

(declare-core-primitive calloc
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer/false))))

(declare-core-primitive free
    (safe)
  (signatures
   ((T:pointer/memory-block)		=> (T:void))))

;;; --------------------------------------------------------------------

(declare-core-primitive malloc*
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer))))

(declare-core-primitive realloc*
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer))))

(declare-core-primitive calloc*
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer))))

;;; --------------------------------------------------------------------

(declare-core-primitive guarded-malloc
    (safe)
  (signatures
   ((T:exact-integer)			=> (T:pointer/false))))

(declare-core-primitive guarded-realloc
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer/false))))

(declare-core-primitive guarded-calloc
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer/false))))

;;; --------------------------------------------------------------------

(declare-core-primitive guarded-malloc*
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer))))

(declare-core-primitive guarded-realloc*
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer))))

(declare-core-primitive guarded-calloc*
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer))))


;;; --------------------------------------------------------------------

(declare-core-primitive memcpy
    (safe)
  (signatures
   ((T:pointer T:pointer T:exact-integer)	=> (T:void))))

(declare-core-primitive memmove
    (safe)
  (signatures
   ((T:pointer T:pointer T:exact-integer)	=> (T:void))))

(declare-core-primitive memcmp
    (safe)
  (signatures
   ((T:pointer T:pointer T:exact-integer)	=> (T:fixnum))))

(declare-core-primitive memset
    (safe)
  (signatures
   ((T:pointer T:exact-integer T:exact-integer)	=> (T:void))))

;;; --------------------------------------------------------------------

(declare-core-primitive make-memory-block
    (safe)
  (signatures
   ((T:pointer T:exact-integer)			=> (T:memory-block))))

(declare-core-primitive make-memory-block/guarded
    (safe)
  (signatures
   ((T:pointer T:exact-integer)			=> (T:memory-block))))

(declare-core-primitive null-memory-block
    (safe)
  (signatures
   (()						=> (T:memory-block)))
  (attributes
   (()						effect-free result-true)))

(declare-core-primitive memory-block?
    (safe)
  (signatures
   ((T:memory-block)				=> (T:boolean))
   ((T:object)					=> (T:false)))
  (attributes
   ((_)						effect-free)))

(declare-core-primitive memory-block?/not-null
    (safe)
  (signatures
   ((T:memory-block)			=> (T:boolean))
   ((T:object)				=> (T:false)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive memory-block?/non-null
    (safe)
  (signatures
   ((T:memory-block)			=> (T:boolean))
   ((T:object)				=> (T:false)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive memory-block-pointer
    (safe)
  (signatures
   ((T:memory-block)			=> (T:pointer)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive memory-block-size
    (safe)
  (signatures
   ((T:memory-block)			=> (T:exact-integer)))
  (attributes
   ((_)					effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-out-of-memory-error
    (safe)
  (signatures
   (()					=> (T:record)))
  (attributes
   (()					effect-free result-true)))

(declare-core-primitive out-of-memory-error?
    (safe)
  (signatures
   ((T:record)				=> (T:boolean))
   ((T:object)				=> (T:false)))
  (attributes
   ((_)					effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive memory-copy
    (safe)
  (signatures
   ((T:pointer/bytevector T:fixnum T:pointer/bytevector T:fixnum T:fixnum)	=> (T:void))))

(declare-core-primitive memory->bytevector
    (safe)
  (signatures
   ((T:pointer T:fixnum)		=> (T:bytevector)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive bytevector->memory
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer/false T:fixnum/false)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer/false T:fixnum/false)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->memory*
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer T:fixnum)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory*
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer T:fixnum)))
  (attributes
   ((_ _)				effect-free)))


#|


 bytevector->memory
 bytevector->guarded-memory
 bytevector->memory*
 bytevector->guarded-memory*

|#


;;;; debugging helpers

(declare-exact-integer-unary integer->machine-word)
(declare-exact-integer-unary machine-word->integer)

;;; --------------------------------------------------------------------

(declare-core-primitive flonum->bytevector
    (safe)
  (signatures
   ((T:flonum)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bytevector->flonum
    (safe)
  (signatures
   ((T:flonum)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bignum->bytevector
    (safe)
  (signatures
   ((T:bignum)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bytevector->bignum
    (safe)
  (signatures
   ((T:bytevector)	=> (T:bignum)))
  (attributes
   ((_)			foldable effect-free result-true)))


;;;; done

#| list of core primitives to declare

 sra
 sll
 add1
 sub1
 quotient+remainder
 random

 port-mode
 set-port-mode!
 with-input-from-string
 get-output-string
 with-output-to-string
 console-input-port
 console-error-port
 console-output-port
 stdin
 stdout
 stderr
 reset-input-port!
 reset-output-port!
 printf
 fprintf
 format

 print-graph
 print-unicode
 printer-integer-radix



 call/cf
 print-error
 assembler-output
 optimizer-output
 assembler-property-key
 new-cafe
 waiter-prompt-string
 readline-enabled?
 readline
 make-readline-input-port
 expand-form-to-core-language
 expand-library
 expand-library->sexp
 expand-top-level
 expand-top-level->sexp
;;;
 environment?
 environment-symbols
 environment-libraries
 environment-labels
 environment-binding
;;;
 time-and-gather
 stats?
 stats-user-secs
 stats-user-usecs
 stats-sys-secs
 stats-sys-usecs
 stats-real-secs
 stats-real-usecs
 stats-collection-id
 stats-gc-user-secs
 stats-gc-user-usecs
 stats-gc-sys-secs
 stats-gc-sys-usecs
 stats-gc-real-secs
 stats-gc-real-usecs
 stats-bytes-minor
 stats-bytes-major
 time-it
 verbose-timer
;;;
 current-time
 time-from-now
 time?
 time-second
 time-nanosecond
 time-gmt-offset
 date-string
 make-time
 time-addition
 time-difference
 time=?
 time<?
 time<=?
 time>?
 time>=?
;;;
 set-rtd-printer!
 set-rtd-destructor!
 struct?
 make-struct-type
 struct-type-descriptor?
 struct-type-name
 struct-type-symbol
 struct-type-field-names
 struct-type-destructor
 default-struct-printer
 struct-constructor
 struct-predicate
 struct-field-accessor
 struct-field-mutator
 struct-length
 struct-ref
 struct-set!
 struct-printer
 struct-destructor
 struct-name
 struct-rtd
 struct=?
 struct-reset
 struct-guardian-logger
 struct-guardian-log
 code?
 immediate?
 pointer-value
;;;
 apropos
 current-primitive-locations
 boot-library-expand
 current-library-collection
 library-name
 find-library-by-name

;;; --------------------------------------------------------------------

;;;
 $length
 $map1
 $for-each1
 $for-all1
 $exists1
 $memq
 $memv
;;;

 $make-bytevector
 $bytevector-length
 $bytevector-empty?
 $bytevector-s8-ref
 $bytevector-u8-ref
 $bytevector-set!
 $bytevector-ieee-double-native-ref
 $bytevector-ieee-double-native-set!
 $bytevector-ieee-double-nonnative-ref
 $bytevector-ieee-double-nonnative-set!
 $bytevector-ieee-single-native-ref
 $bytevector-ieee-single-native-set!
 $bytevector-ieee-single-nonnative-ref
 $bytevector-ieee-single-nonnative-set!
 $bytevector=
 $bytevector-total-length
 $bytevector-concatenate
 $bytevector-reverse-and-concatenate
 $bytevector-copy
 $uri-encode
 $uri-decode
 $uri-encoded-bytevector?
 $uri-normalise-encoding
 $octets-encoded-bytevector?
 $ascii-encoded-bytevector?
 $latin1-encoded-bytevector?
 $percent-encode
 $percent-decode
 $percent-encoded-bytevector?
 $percent-normalise-encoding
 $bytevector->base64
 $base64->bytevector
 $ascii->string
 $octets->string
 $latin1->string
 $bytevector->string-base64

;;;
;;;
 $make-bignum
 $bignum-positive?
 $bignum-negative?
 $bignum-non-positive?
 $bignum-non-negative?
 $bignum-size
 $bignum-byte-ref
 $bignum-byte-set!
 $bignum-even?
 $bignum-odd?
 $bignum->flonum
;;;
 $make-ratnum
 $ratnum-n
 $ratnum-num
 $ratnum-d
 $ratnum-den
 $ratnum->flonum
 $ratnum-positive?
 $ratnum-negative?
 $ratnum-non-positive?
 $ratnum-non-negative?
;;;
 $make-rectangular

 $make-compnum
 $compnum-real
 $compnum-imag

 $make-cflonum
 $cflonum-real
 $cflonum-imag

 $complex-conjugate-compnum
 $complex-conjugate-cflonum

 $angle-fixnum
 $angle-bignum
 $angle-ratnum
 $angle-flonum
 $angle-compnum
 $angle-cflonum

 $magnitude-fixnum
 $magnitude-bignum
 $magnitude-ratnum
 $magnitude-flonum
 $magnitude-compnum
 $magnitude-cflonum
;;;

;;;
 $make-symbol
 $symbol->string
 $symbol-unique-string
 $symbol-value
 $symbol-proc
 $symbol-string
 $symbol-plist
 $set-symbol-value!
 $set-symbol-proc!
 $set-symbol-string!
 $set-symbol-unique-string!
 $set-symbol-plist!
 $unintern-gensym
 $init-symbol-value!)
 $unbound-object?
 $symbol-table-size
 $log-symbol-table-status
 $getprop
 $putprop
 $remprop
 $property-list
;;;

;;; --------------------------------------------------------------------

  (let ((P (C base-rtd)))
    (register-lambda-signature P (S (list (C <struct-type-descriptor>))
				    '())))

  (let ((P (C $struct)))
    (register-lambda-signature P (S (list (C <struct>))
				    (cons (C <struct-type-descriptor>) (C <list>)))))

  (let ((P (C $make-struct)))
    (register-lambda-signature P (S (list (C <struct>))
				    (list (C <struct-type-descriptor>) (C <fixnum>)))))

  (let ((P (C $struct?)))
    (register-lambda-signature P (S (list (C <boolean>))
				    (list (C <top>)))))

  (let ((P (C $struct/rtd?)))
    (register-lambda-signature P (S (list (C <boolean>))
				    (list (C <top>) (C <struct-type-descriptor>)))))

  (let ((P (C $struct-guardian)))
    (register-lambda-signature P (S (list (C <top>))
				    (list (C <struct>)))))

  (let ((P (C $struct-rtd)))
    (register-lambda-signature P (S (list (C <struct-type-descriptor>))
				    (list (C <struct>)))))

  (let ((P (C $struct-set!)))
    (register-lambda-signature P (S (list (C <void>))
				    (list (C <struct>) (C <fixnum>) (C <top>)))))

  (let ((P (C $struct-ref)))
    (register-lambda-signature P (S (list (C <top>))
				    (list (C <struct>) (C <fixnum>)))))

 $record-guardian

 $std-std
 $std-name
 $std-length
 $std-fields
 $std-printer
 $std-symbol
 $std-destructor

 $set-std-std!
 $set-std-name!
 $set-std-length!
 $set-std-fields!
 $set-std-printer!
 $set-std-symbol!
 $set-std-destructor!

;;; --------------------------------------------------------------------
;;; (ikarus system $pointers)

;;;
 $closure-code
 $code->closure
 $code-reloc-vector
 $code-freevars
 $code-size
 $code-annotation
 $code-ref
 $code-set!
 $set-code-annotation!
 procedure-annotation
 $make-annotated-procedure
 $annotated-procedure-annotation
 $cpref
 $make-tcbucket
 $tcbucket-key
 $tcbucket-val
 $tcbucket-next
 $set-tcbucket-val!
 $set-tcbucket-next!
 $set-tcbucket-tconc!
 $arg-list
 $collect-key
 $$apply
 $fp-at-base
 $primitive-call/cc
 $frame->continuation
 $current-frame
 $seal-frame-and-call
 $make-call-with-values-procedure
 $make-values-procedure
 $interrupted?
 $unset-interrupted!
 $swap-engine-counter!
;;;
 interrupted-condition?
 make-interrupted-condition
 source-position-condition?
 make-source-position-condition
 source-position-port-id
 source-position-byte
 source-position-character
 source-position-line
 source-position-column

 $apply-nonprocedure-error-handler
 $incorrect-args-error-handler
 $multiple-values-error
 $debug
 $underflow-misaligned-error
 top-level-value-error
 car-error
 cdr-error
 fxadd1-error
 fxsub1-error
 cadr-error
 fx+-type-error
 fx+-types-error
 fx+-overflow-error
 $do-event
 do-overflow
 do-overflow-words
 do-vararg-overflow
 collect
 collect-key
 post-gc-hooks
 register-to-avoid-collecting
 forget-to-avoid-collecting
 replace-to-avoid-collecting
 retrieve-to-avoid-collecting
 collection-avoidance-list
 purge-collection-avoidance-list
 do-stack-overflow
 make-promise
 make-traced-procedure
 make-traced-macro
 error@fx+
 error@fxarithmetic-shift-left
 error@fxarithmetic-shift-right
 error@fx*
 error@fx-
 error@add1
 error@sub1
 error@fxadd1
 error@fxsub1
 fasl-write
 fasl-read
 syntax-parameter-value
 <
 <=
 =
 !=
 >
 >=
 +
 -
 *
 /
 abs
 sign
 asin
 acos
 atan
 sinh
 cosh
 tanh
 asinh
 acosh
 atanh
 angle
 apply
 assert
 assertion-error
 assertion-violation
 boolean=?
 boolean?

 call-with-current-continuation
 call/cc
 call-with-values
 ceiling
 ;;
 complex?
 cons
 cos
 denominator
 div
 mod
 div-and-mod
 div0
 mod0
 div0-and-mod0
 error
 warning
 die
 even?
 exact
 exact-integer-sqrt
 exact?
 exp
 expt
 finite?
 floor
 gcd
 imag-part
 inexact
 inexact?
 infinite?
 integer->char
 integer-valued?
 integer?
 exact-integer?
 zero-exact-integer?
 negative-exact-integer?
 positive-exact-integer?
 non-negative-exact-integer?
 non-positive-exact-integer?
 lcm
 log
 magnitude
 make-polar
 make-rectangular
 complex-conjugate
 max
 min
 nan?
 negative?
 non-negative?
 not
 null?
 number->string
 number?
 numerator
 odd?
 pair?
 positive?
 non-positive?
 procedure?
 rational-valued?
 rational?
 rationalize
 real-part
 real-valued?
 real?
 reverse
 round
 sin
 sqrt
 cbrt
 square
 cube
 string->latin1
 latin1->string
 latin1-encoded-bytevector?
 ascii-encoded-string?
 latin1-encoded-string?
 string->ascii
 string->octets
 octets-encoded-bytevector?
 octets-encoded-string?
 octets->string
 ascii->string
 ascii-encoded-bytevector?
 bytevector->hex
 hex->bytevector
 string-hex->bytevector
 bytevector->string-hex
 bytevector->base64
 base64->bytevector
 string-base64->bytevector
 bytevector->string-base64
 string->uri-encoding
 uri-encoding->string
 string->percent-encoding
 percent-encoding->string
 uri-encode
 uri-decode
 normalise-uri-encoding
 uri-encoded-bytevector?
 uri-encoded-string?
 percent-encoded-bytevector?
 percent-encoded-string?
 percent-encode
 percent-decode
 normalise-percent-encoding
 tan
 truncate
 values
 values->list
 zero?
 ...
 =>
 _
 else
 bitwise-arithmetic-shift
 bitwise-arithmetic-shift-left
 bitwise-arithmetic-shift-right
 bitwise-not
 bitwise-and
 bitwise-ior
 bitwise-xor
 bitwise-bit-count
 bitwise-bit-field
 bitwise-bit-set?
 bitwise-copy-bit
 bitwise-copy-bit-field
 bitwise-first-bit-set
 bitwise-if
 bitwise-length
 bitwise-reverse-bit-field
 bitwise-rotate-bit-field
;;;
 bignum-positive?
 bignum-negative?
 bignum-non-negative?
 bignum-non-positive?
 bignum-odd?
 bignum-even?
 least-positive-bignum
 greatest-negative-bignum
;;;
 make-no-infinities-violation
 make-no-nans-violation
 &no-infinities
 no-infinities-violation?
 &no-nans
 no-nans-violation?
 bytevector->sint-list
 bytevector->u8-list
 bytevector->s8-list
 bytevector->u16l-list
 bytevector->u16b-list
 bytevector->u16n-list
 bytevector->s16l-list
 bytevector->s16b-list
 bytevector->s16n-list
 bytevector->u32l-list
 bytevector->u32b-list
 bytevector->u32n-list
 bytevector->s32l-list
 bytevector->s32b-list
 bytevector->s32n-list
 bytevector->u64l-list
 bytevector->u64b-list
 bytevector->u64n-list
 bytevector->s64l-list
 bytevector->s64b-list
 bytevector->s64n-list
 bytevector->uint-list
 bytevector->f4l-list
 bytevector->f4b-list
 bytevector->f4n-list
 bytevector->f8l-list
 bytevector->f8b-list
 bytevector->f8n-list
 bytevector->c4l-list
 bytevector->c4b-list
 bytevector->c4n-list
 bytevector->c8l-list
 bytevector->c8b-list
 bytevector->c8n-list
 bytevector-copy
 bytevector-copy!
 bytevector-fill!
 bytevector-ieee-double-native-ref
 bytevector-ieee-double-native-set!
 bytevector-ieee-double-ref
 bytevector-ieee-double-set!
 bytevector-ieee-single-native-ref
 bytevector-ieee-single-native-set!
 bytevector-ieee-single-ref
 bytevector-ieee-single-set!
 bytevector-length
 bytevector-length?
 bytevector-index?
 bytevector-word-size?
 bytevector-word-count?
 bytevector-index-for-word?
 bytevector-index-for-word8?
 bytevector-index-for-word16?
 bytevector-index-for-word32?
 bytevector-index-for-word64?
 bytevector-start-index-and-count-for-word?
 bytevector-start-index-and-count-for-word8?
 bytevector-start-index-and-count-for-word16?
 bytevector-start-index-and-count-for-word32?
 bytevector-start-index-and-count-for-word64?
 list-of-bytevectors?
 bytevector-empty?
 bytevector-s16-native-ref
 bytevector-s16-native-set!
 bytevector-s16-ref
 bytevector-s16-set!
 bytevector-s32-native-ref
 bytevector-s32-native-set!
 bytevector-s32-ref
 bytevector-s32-set!
 bytevector-s64-native-ref
 bytevector-s64-native-set!
 bytevector-s64-ref
 bytevector-s64-set!
 bytevector-s8-ref
 bytevector-s8-set!
 bytevector-sint-ref
 bytevector-sint-set!
 bytevector-u16-native-ref
 bytevector-u16-native-set!
 bytevector-u16-ref
 bytevector-u16-set!
 bytevector-u32-native-ref
 bytevector-u32-native-set!
 bytevector-u32-ref
 bytevector-u32-set!
 bytevector-u64-native-ref
 bytevector-u64-native-set!
 bytevector-u64-ref
 bytevector-u64-set!
 bytevector-u8-ref
 bytevector-u8-set!
 bytevector-uint-ref
 bytevector-uint-set!
 f4l-list->bytevector
 f4b-list->bytevector
 f4n-list->bytevector
 f8l-list->bytevector
 f8b-list->bytevector
 f8n-list->bytevector
 c4l-list->bytevector
 c4b-list->bytevector
 c4n-list->bytevector
 c8l-list->bytevector
 c8b-list->bytevector
 c8n-list->bytevector
 bytevector=?
 bytevector?
 subbytevector-u8
 subbytevector-u8/count
 subbytevector-s8
 subbytevector-s8/count
 bytevector-append
 bytevector-reverse-and-concatenate
 native-endianness
 sint-list->bytevector
 u8-list->bytevector
 s8-list->bytevector
 u16l-list->bytevector
 u16b-list->bytevector
 u16n-list->bytevector
 s16l-list->bytevector
 s16b-list->bytevector
 s16n-list->bytevector
 u32l-list->bytevector
 u32b-list->bytevector
 u32n-list->bytevector
 s32l-list->bytevector
 s32b-list->bytevector
 s32n-list->bytevector
 u64l-list->bytevector
 u64b-list->bytevector
 u64n-list->bytevector
 s64l-list->bytevector
 s64b-list->bytevector
 s64n-list->bytevector
 uint-list->bytevector
 utf8->string
 utf16->string
 utf16le->string
 utf16n->string
 utf16be->string
 utf32->string
 print-condition
 condition?
 &assertion
 assertion-violation?
 &condition
 condition
 condition-accessor
 condition-irritants
 condition-message
 condition-predicate
 condition-who
 define-condition-type
 &error
 error?
 &implementation-restriction
 implementation-restriction-violation?
 &irritants
 irritants-condition?
 &lexical
 lexical-violation?
 make-assertion-violation
 make-error
 make-implementation-restriction-violation
 make-irritants-condition
 make-lexical-violation
 make-message-condition
 make-non-continuable-violation
 make-serious-condition
 make-syntax-violation
 make-undefined-violation
 make-violation
 make-warning
 make-who-condition
 &message
 message-condition?
 &non-continuable
 non-continuable-violation?
 &serious
 serious-condition?
 simple-conditions
 &syntax
 syntax-violation-form
 syntax-violation-subform
 syntax-violation?
 &undefined
 undefined-violation?
 &violation
 violation?
 &warning
 warning?
 &who
 who-condition?
 case-lambda
 do
 unless
 when
 define-enumeration
 enum-set->list
 enum-set-complement
 enum-set-constructor
 enum-set-difference
 enum-set-indexer
 enum-set-intersection
 enum-set-member?
 enum-set-projection
 enum-set-subset?
 enum-set-union
 enum-set-universe
 enum-set=?
 make-enumeration
 enum-set?
 environment
 eval
 raise
 raise-continuable
 with-exception-handler
 guard
 binary-port?
 buffer-mode
 buffer-mode?
 bytevector->string
 call-with-bytevector-output-port
 call-with-port
 call-with-string-output-port

 delay
 exact->inexact
 force
 inexact->exact
 modulo
 remainder
 null-environment
 promise?
 quotient
 scheme-report-environment
 interaction-environment
 new-interaction-environment
 close-port
 eol-style
 error-handling-mode
 file-options
 flush-output-port
 get-bytevector-all
 get-bytevector-n
 get-bytevector-n!
 get-bytevector-some
 get-char
 get-datum
 get-line
 read-line
 get-string-all
 get-string-n
 get-string-n!
 get-string-some
 get-u8
 &i/o
 &i/o-decoding
 i/o-decoding-error?
 &i/o-encoding
 i/o-encoding-error-char
 i/o-encoding-error?
 i/o-error-filename
 i/o-error-port
 i/o-error-position
 i/o-error?
 &i/o-file-already-exists
 i/o-file-already-exists-error?
 &i/o-file-does-not-exist
 i/o-file-does-not-exist-error?
 &i/o-file-is-read-only
 i/o-file-is-read-only-error?
 &i/o-file-protection
 i/o-file-protection-error?
 &i/o-filename
 i/o-filename-error?
 &i/o-invalid-position
 i/o-invalid-position-error?
 &i/o-port
 i/o-port-error?
 &i/o-read
 i/o-read-error?
 &i/o-write
 i/o-write-error?
 &i/o-eagain
 i/o-eagain-error?
 &errno
 errno-condition?
 &h_errno
 h_errno-condition?
 &procedure-argument-violation
 procedure-argument-violation?
 &expression-return-value-violation
 expression-return-value-violation?
 lookahead-char
 lookahead-u8
 lookahead-two-u8
 make-bytevector
 make-custom-binary-input-port
 make-custom-binary-output-port
 make-custom-textual-input-port
 make-custom-textual-output-port
 make-custom-binary-input/output-port
 make-custom-textual-input/output-port
 make-binary-file-descriptor-input-port
 make-binary-file-descriptor-input-port*
 make-binary-file-descriptor-output-port
 make-binary-file-descriptor-output-port*
 make-binary-file-descriptor-input/output-port
 make-binary-file-descriptor-input/output-port*
 make-binary-socket-input-port
 make-binary-socket-input-port*
 make-binary-socket-output-port
 make-binary-socket-output-port*
 make-binary-socket-input/output-port
 make-binary-socket-input/output-port*
 make-textual-file-descriptor-input-port
 make-textual-file-descriptor-input-port*
 make-textual-file-descriptor-output-port
 make-textual-file-descriptor-output-port*
 make-textual-file-descriptor-input/output-port
 make-textual-file-descriptor-input/output-port*
 make-textual-socket-input-port
 make-textual-socket-input-port*
 make-textual-socket-output-port
 make-textual-socket-output-port*
 make-textual-socket-input/output-port
 make-textual-socket-input/output-port*
 make-i/o-decoding-error
 make-i/o-encoding-error
 make-i/o-error
 make-i/o-file-already-exists-error
 make-i/o-file-does-not-exist-error
 make-i/o-file-is-read-only-error
 make-i/o-file-protection-error
 make-i/o-filename-error
 make-i/o-invalid-position-error
 make-i/o-port-error
 make-i/o-read-error
 make-i/o-write-error
 make-i/o-eagain
 make-errno-condition
 condition-errno
 make-h_errno-condition
 condition-h_errno
 make-procedure-argument-violation
 procedure-argument-violation
 make-expression-return-value-violation
 expression-return-value-violation
 latin-1-codec
 make-transcoder
 native-eol-style
 native-transcoder
 transcoder?
 open-bytevector-input-port
 open-bytevector-output-port
 open-file-input-port
 open-file-input/output-port
 open-file-output-port
 open-string-input-port
 open-string-output-port
 bytevector-port-buffer-size
 string-port-buffer-size
 input-file-buffer-size
 output-file-buffer-size
 input/output-file-buffer-size
 input/output-socket-buffer-size
 output-port-buffer-mode
 set-port-buffer-mode!
 port-eof?
 port-has-port-position?
 port-has-set-port-position!?
 port-position
 get-char-and-track-textual-position
 port-textual-position
 port-transcoder
 port?
 put-bytevector
 put-char
 put-datum
 put-string
 put-u8
 set-port-position!
 standard-error-port
 standard-input-port
 standard-output-port
 textual-port?
 transcoded-port
 transcoder-codec
 transcoder-eol-style
 transcoder-error-handling-mode
 utf-8-codec
 utf-16-codec
 utf-16le-codec
 utf-16be-codec
 utf-16n-codec
 utf-bom-codec
 would-block-object
 would-block-object?
 input-port?
 output-port?
 input/output-port?
 binary-input-port?
 textual-input-port?
 binary-output-port?
 textual-output-port?
 binary-input/output-port?
 textual-input/output-port?
 current-input-port
 current-output-port
 current-error-port
 eof-object
 eof-object?
 close-input-port
 close-output-port
 display
 newline
 open-input-file
 open-output-file
 peek-char

  (let ((P (C read)))
    (register-lambda-signature P (make-clambda-compound (list (S (list (C <top>)) '())
							      (S (list (C <top>))
								 (list (C <textual-input-port>)))))))

 read-char

  (let ((P (C write)))
    (register-lambda-signature P (make-clambda-compound (list (S (list (C <top>)) (list (C <top>)))
							      (S (list (C <top>)) (list (C <top>) (C <textual-input-port>)))))))

 write-char
 call-with-input-file
 call-with-output-file
 list-sort
 file-exists?
 directory-exists?
 delete-file

;;; --------------------------------------------------------------------

 record-field-mutable?
 record-rtd
 record-type-field-names
 record-type-generative?
 record-type-name
 record-type-opaque?
 record-type-parent
 record-type-sealed?
 record-type-uid
 record?
 make-record-constructor-descriptor
 make-record-type-descriptor
 record-constructor

  (let ((P (C record-predicate)))
    (register-lambda-signature P (S (list (C <predicate>))
				    (list (C <record-type-descriptor>)))))

 record-type-descriptor?
 record-destructor-set!
 record-destructor
 record-guardian-logger
 record-guardian-log
 record-reset
 record-and-rtd?
 record-accessor
 record-mutator
 unsafe-record-accessor
 unsafe-record-mutator

;;; --------------------------------------------------------------------

 syntax-violation
 bound-identifier=?
 datum->syntax
 syntax->datum
 free-identifier=?
 generate-temporaries
 identifier?
 identifier-bound?
 make-variable-transformer
 variable-transformer?
 variable-transformer-procedure
 make-synonym-transformer
 synonym-transformer?
 synonym-transformer-identifier
 make-compile-time-value
 compile-time-value?
 compile-time-value-object
 syntactic-binding-putprop
 syntactic-binding-getprop
 syntactic-binding-remprop
 syntactic-binding-property-list
 syntax-object?
 syntax-object-expression
 syntax-object-marks
 syntax-object-ribs
 syntax-object-source-objects
 load
 void
 eval-core
 current-core-eval
 pretty-print
 pretty-print*
 debug-print
 debug-print-enabled?
 debug-print*
 pretty-format
 pretty-width
 library
 $transcoder->data
 $data->transcoder
 make-file-options
;;;
 set-identifier-unsafe-variant!
;;;
 set-predicate-procedure-argument-validation!
 set-predicate-return-value-validation!
;;;
 push-compensation
 run-compensations
 compensations
 run-compensations-store
 push-compensation-thunk
;;;
 port-id
 port-uid
 port-fd
 port-set-non-blocking-mode!
 port-unset-non-blocking-mode!
 port-in-non-blocking-mode?
 port-putprop
 port-getprop
 port-remprop
 port-property-list
 string->filename-func
 filename->string-func
 string->pathname-func
 pathname->string-func
 port-dump-status
 port-closed?
;;; (ikarus system $io)
 $make-port
 $port-tag
 $port-id
 $port-cookie
 $port-transcoder
 $port-index
 $port-size
 $port-buffer
 $port-get-position
 $port-set-position!
 $port-close
 $port-read!
 $port-write!
 $set-port-index!
 $set-port-size!
 $port-attrs
 $set-port-attrs!
;;;
 get-annotated-datum
 annotation?
 annotation-expression
 annotation-source
 annotation-stripped
 annotation-textual-position


;;; --------------------------------------------------------------------
;;; configuration options

 vicare-built-with-ffi-enabled
 vicare-built-with-iconv-enabled
 vicare-built-with-posix-enabled
 vicare-built-with-glibc-enabled
 vicare-built-with-linux-enabled
 vicare-built-with-srfi-enabled

 vicare-built-with-arguments-validation-enabled

;;; --------------------------------------------------------------------
;;; POSIX functions

 strerror
 errno->string
 getenv
 environ
 mkdir
 mkdir/parents
 real-pathname
 file-pathname?
 file-string-pathname?
 file-bytevector-pathname?
 file-absolute-pathname?
 file-relative-pathname?
 file-colon-search-path?
 file-string-colon-search-path?
 file-bytevector-colon-search-path?
 file-modification-time
 split-pathname-root-and-tail
 search-file-in-environment-path
 search-file-in-list-path
 split-pathname
 split-pathname-bytevector
 split-pathname-string
 split-search-path
 split-search-path-bytevector
 split-search-path-string
 vicare-argv0
 vicare-argv0-string

;;; --------------------------------------------------------------------
;;; environment inquiry

 uname
 utsname?
 utsname-sysname
 utsname-nodename
 utsname-release
 utsname-version
 utsname-machine

 implementation-name
 implementation-version
 cpu-architecture
 machine-name
 os-name
 os-version

 host-info

;;;
 bytevector->cstring
 bytevector->guarded-cstring
 cstring->bytevector
 cstring16->bytevector
 cstring16n->string
 cstring16le->string
 cstring16be->string
 string->cstring
 string->guarded-cstring
 bytevector->cstring*
 bytevector->guarded-cstring*
 cstring->bytevector*
 string->cstring*
 string->guarded-cstring*
 cstring->string
 strlen
 strcmp
 strncmp
 strdup
 strndup
 guarded-strdup
 guarded-strndup
 strdup*
 strndup*
 guarded-strdup*
 guarded-strndup*

 argv->bytevectors
 argv-length
 argv->strings
 bytevectors->argv
 bytevectors->argv*
 bytevectors->guarded-argv
 bytevectors->guarded-argv*
 strings->argv
 strings->argv*
 strings->guarded-argv
 strings->guarded-argv*

;;;
 pointer-ref-c-uint8
 pointer-ref-c-sint8
 pointer-ref-c-uint16
 pointer-ref-c-sint16
 pointer-ref-c-uint32
 pointer-ref-c-sint32
 pointer-ref-c-uint64
 pointer-ref-c-sint64
;;;
 pointer-ref-c-signed-char
 pointer-ref-c-signed-short
 pointer-ref-c-signed-int
 pointer-ref-c-signed-long
 pointer-ref-c-signed-long-long
 pointer-ref-c-unsigned-char
 pointer-ref-c-unsigned-short
 pointer-ref-c-unsigned-int
 pointer-ref-c-unsigned-long
 pointer-ref-c-unsigned-long-long
;;;
 pointer-ref-c-float
 pointer-ref-c-double
 pointer-ref-c-pointer
;;;
 pointer-ref-c-size_t
 pointer-ref-c-ssize_t
 pointer-ref-c-off_t
 pointer-ref-c-ptrdiff_t
;;;
 pointer-set-c-uint8!
 pointer-set-c-sint8!
 pointer-set-c-uint16!
 pointer-set-c-sint16!
 pointer-set-c-uint32!
 pointer-set-c-sint32!
 pointer-set-c-uint64!
 pointer-set-c-sint64!
;;;
 pointer-set-c-signed-char!
 pointer-set-c-signed-short!
 pointer-set-c-signed-int!
 pointer-set-c-signed-long!
 pointer-set-c-signed-long-long!
 pointer-set-c-unsigned-char!
 pointer-set-c-unsigned-short!
 pointer-set-c-unsigned-int!
 pointer-set-c-unsigned-long!
 pointer-set-c-unsigned-long-long!
;;;
 pointer-set-c-float!
 pointer-set-c-double!
 pointer-set-c-pointer!
;;;
 pointer-set-c-size_t!
 pointer-set-c-ssize_t!
 pointer-set-c-off_t!
 pointer-set-c-ptrdiff_t!
;;;
 array-ref-c-uint8
 array-ref-c-sint8
 array-ref-c-uint16
 array-ref-c-sint16
 array-ref-c-uint32
 array-ref-c-sint32
 array-ref-c-uint64
 array-ref-c-sint64
;;;
 array-ref-c-signed-char
 array-ref-c-unsigned-char
 array-ref-c-signed-short
 array-ref-c-unsigned-short
 array-ref-c-signed-int
 array-ref-c-unsigned-int
 array-ref-c-signed-long
 array-ref-c-unsigned-long
 array-ref-c-signed-long-long
 array-ref-c-unsigned-long-long
;;;
 array-ref-c-float
 array-ref-c-double
 array-ref-c-pointer
;;;
 array-ref-c-size_t
 array-ref-c-ssize_t
 array-ref-c-off_t
 array-ref-c-ptrdiff_t
;;;
 array-set-c-uint8!
 array-set-c-sint8!
 array-set-c-uint16!
 array-set-c-sint16!
 array-set-c-uint32!
 array-set-c-sint32!
 array-set-c-uint64!
 array-set-c-sint64!
;;;
 array-set-c-signed-char!
 array-set-c-unsigned-char!
 array-set-c-signed-short!
 array-set-c-unsigned-short!
 array-set-c-signed-int!
 array-set-c-unsigned-int!
 array-set-c-signed-long!
 array-set-c-unsigned-long!
 array-set-c-signed-long-long!
 array-set-c-unsigned-long-long!
;;;
 array-set-c-float!
 array-set-c-double!
 array-set-c-pointer!
;;;
 array-set-c-size_t!
 array-set-c-ssize_t!
 array-set-c-off_t!
 array-set-c-ptrdiff_t!
;;;
 dlopen
 dlerror
 dlclose
 dlsym
;;;
 make-c-callout-maker
 make-c-callout-maker/with-errno
 make-c-callback-maker
 free-c-callback

;;; --------------------------------------------------------------------
;;; syntax utilities

 identifier->string
 string->identifier
 identifier-prefix
 identifier-suffix
 identifier-append
 identifier-format
 duplicate-identifiers?
 delete-duplicate-identifiers
 identifier-memq

 identifier-record-constructor
 identifier-record-predicate
 identifier-record-field-accessor
 identifier-record-field-mutator

 identifier-struct-constructor
 identifier-struct-predicate
 identifier-struct-field-accessor
 identifier-struct-field-mutator

 syntax-car
 syntax-cdr
 syntax->list
 identifiers->list
 all-identifiers?

 syntax->vector
 syntax-unwrap
 syntax=?
 identifier=symbol?
 #;quoted-syntax-object?

 syntax-clauses-unwrap
 syntax-clauses-filter
 syntax-clauses-remove
 syntax-clauses-partition
 syntax-clauses-collapse
 syntax-clauses-verify-at-least-once
 syntax-clauses-verify-at-most-once
 syntax-clauses-verify-exactly-once
 syntax-clauses-verify-mutually-inclusive
 syntax-clauses-verify-mutually-exclusive

    clause specification structs
 make-syntax-clause-spec
 syntax-clause-spec?
 syntax-clause-spec-keyword
 syntax-clause-spec-min-number-of-occurrences
 syntax-clause-spec-max-number-of-occurrences
 syntax-clause-spec-min-number-of-arguments
 syntax-clause-spec-max-number-of-arguments
 syntax-clause-spec-mutually-inclusive
 syntax-clause-spec-mutually-exclusive
 syntax-clause-spec-custom-data
 syntax-clauses-single-spec
 syntax-clauses-fold-specs
 syntax-clauses-validate-specs

;;; --------------------------------------------------------------------
;;; library names

 library-name?
 library-version-numbers?
 library-version-number?
 library-name-decompose
 library-name->identifiers
 library-name->version
 library-name-identifiers=?
 library-name=?
 library-name<?
 library-name<=?
 library-version=?
 library-version<?
 library-version<=?

;;; --------------------------------------------------------------------
;;; library references and conformity

 library-reference?
 library-version-reference?
 library-sub-version-reference?
 library-sub-version?
 library-reference-decompose
 library-reference->identifiers
 library-reference->version-reference
 library-reference-identifiers=?
 conforming-sub-version-and-sub-version-reference?
 conforming-version-and-version-reference?
 conforming-library-name-and-library-reference?

;;; --------------------------------------------------------------------
;;; library infrastructure

 library?
 library-uid
 library-imp-lib*
 library-vis-lib*
 library-inv-lib*
 library-export-subst
 library-export-env
 library-visit-state
 library-invoke-state
 library-visit-code
 library-invoke-code
 library-guard-code
 library-guard-lib*
 library-visible?
 library-source-file-name
 library-option*

 library-path
 library-extensions
 fasl-directory
 fasl-search-path
 fasl-path
 fasl-stem+extension

 current-library-locator
 run-time-library-locator
 compile-time-library-locator
 source-library-locator
 current-source-library-file-locator
 current-binary-library-file-locator
 default-source-library-file-locator
 default-binary-library-file-locator
 installed-libraries
 uninstall-library

;;; --------------------------------------------------------------------

 $compnum->cflonum

 $neg-number
 $neg-fixnum
 $neg-bignum
 $neg-flonum
 $neg-ratnum
 $neg-compnum
 $neg-cflonum

 $inv-number
 $inv-fixnum
 $inv-bignum
 $inv-flonum
 $inv-ratnum
 $inv-compnum
 $inv-cflonum

 $add1-integer
 $add1-fixnum
 $add1-bignum

 $sub1-integer
 $sub1-fixnum
 $sub1-bignum

 $add-number-number
 $add-fixnum-number
 $add-bignum-number
 $add-flonum-number
 $add-ratnum-number
 $add-compnum-number
 $add-cflonum-number
 $add-number-fixnum
 $add-number-bignum
 $add-number-flonum
 $add-number-ratnum
 $add-number-compnum
 $add-number-cflonum
 $add-fixnum-fixnum
 $add-fixnum-bignum
 $add-fixnum-flonum
 $add-fixnum-ratnum
 $add-fixnum-compnum
 $add-fixnum-cflonum
 $add-bignum-fixnum
 $add-bignum-bignum
 $add-bignum-flonum
 $add-bignum-ratnum
 $add-bignum-compnum
 $add-bignum-cflonum
 $add-flonum-fixnum
 $add-flonum-bignum
 $add-flonum-flonum
 $add-flonum-ratnum
 $add-flonum-compnum
 $add-flonum-cflonum
 $add-ratnum-fixnum
 $add-ratnum-bignum
 $add-ratnum-flonum
 $add-ratnum-ratnum
 $add-ratnum-compnum
 $add-ratnum-cflonum
 $add-compnum-fixnum
 $add-compnum-bignum
 $add-compnum-ratnum
 $add-compnum-compnum
 $add-compnum-flonum
 $add-compnum-cflonum
 $add-cflonum-fixnum
 $add-cflonum-bignum
 $add-cflonum-ratnum
 $add-cflonum-flonum
 $add-cflonum-compnum
 $add-cflonum-cflonum

 $sub-number-number
 $sub-fixnum-number
 $sub-bignum-number
 $sub-flonum-number
 $sub-ratnum-number
 $sub-compnum-number
 $sub-cflonum-number
 $sub-number-fixnum
 $sub-number-bignum
 $sub-number-flonum
 $sub-number-ratnum
 $sub-number-compnum
 $sub-number-cflonum
 $sub-fixnum-fixnum
 $sub-fixnum-bignum
 $sub-fixnum-flonum
 $sub-fixnum-ratnum
 $sub-fixnum-compnum
 $sub-fixnum-cflonum
 $sub-bignum-fixnum
 $sub-bignum-bignum
 $sub-bignum-flonum
 $sub-bignum-ratnum
 $sub-bignum-compnum
 $sub-bignum-cflonum
 $sub-flonum-fixnum
 $sub-flonum-bignum
 $sub-flonum-ratnum
 $sub-flonum-flonum
 $sub-flonum-compnum
 $sub-flonum-cflonum
 $sub-ratnum-fixnum
 $sub-ratnum-bignum
 $sub-ratnum-flonum
 $sub-ratnum-ratnum
 $sub-ratnum-compnum
 $sub-ratnum-cflonum
 $sub-compnum-fixnum
 $sub-compnum-bignum
 $sub-compnum-ratnum
 $sub-compnum-compnum
 $sub-compnum-flonum
 $sub-compnum-cflonum
 $sub-cflonum-fixnum
 $sub-cflonum-bignum
 $sub-cflonum-ratnum
 $sub-cflonum-flonum
 $sub-cflonum-compnum
 $sub-cflonum-cflonum

 $mul-number-number
 $mul-fixnum-number
 $mul-bignum-number
 $mul-flonum-number
 $mul-ratnum-number
 $mul-compnum-number
 $mul-cflonum-number
 $mul-number-fixnum
 $mul-number-bignum
 $mul-number-flonum
 $mul-number-ratnum
 $mul-number-compnum
 $mul-number-cflonum
 $mul-fixnum-fixnum
 $mul-fixnum-bignum
 $mul-fixnum-flonum
 $mul-fixnum-ratnum
 $mul-fixnum-compnum
 $mul-fixnum-cflonum
 $mul-bignum-fixnum
 $mul-bignum-bignum
 $mul-bignum-flonum
 $mul-bignum-ratnum
 $mul-bignum-compnum
 $mul-bignum-cflonum
 $mul-flonum-flonum
 $mul-flonum-cflonum
 $mul-flonum-fixnum
 $mul-flonum-bignum
 $mul-flonum-ratnum
 $mul-flonum-compnum
 $mul-ratnum-fixnum
 $mul-ratnum-bignum
 $mul-ratnum-flonum
 $mul-ratnum-ratnum
 $mul-ratnum-compnum
 $mul-ratnum-cflonum
 $mul-compnum-fixnum
 $mul-compnum-bignum
 $mul-compnum-ratnum
 $mul-compnum-flonum
 $mul-compnum-compnum
 $mul-compnum-cflonum
 $mul-cflonum-fixnum
 $mul-cflonum-bignum
 $mul-cflonum-ratnum
 $mul-cflonum-flonum
 $mul-cflonum-compnum
 $mul-cflonum-cflonum

 $div-number-number
 $div-flonum-number
 $div-fixnum-number
 $div-bignum-number
 $div-ratnum-number
 $div-compnum-number
 $div-cflonum-number
 $div-number-flonum
 $div-number-fixnum
 $div-number-bignum
 $div-number-ratnum
 $div-number-compnum
 $div-number-cflonum
 $div-fixnum-flonum
 $div-fixnum-fixnum
 $div-fixnum-bignum
 $div-fixnum-ratnum
 $div-fixnum-compnum
 $div-fixnum-cflonum
 $div-bignum-fixnum
 $div-bignum-bignum
 $div-bignum-flonum
 $div-bignum-ratnum
 $div-bignum-compnum
 $div-bignum-cflonum
 $div-ratnum-fixnum
 $div-ratnum-bignum
 $div-ratnum-ratnum
 $div-ratnum-flonum
 $div-ratnum-compnum
 $div-ratnum-cflonum
 $div-flonum-flonum
 $div-flonum-cflonum
 $div-flonum-fixnum
 $div-flonum-bignum
 $div-flonum-ratnum
 $div-flonum-compnum
 $div-compnum-fixnum
 $div-compnum-bignum
 $div-compnum-ratnum
 $div-compnum-flonum
 $div-compnum-compnum
 $div-compnum-cflonum
 $div-cflonum-fixnum
 $div-cflonum-bignum
 $div-cflonum-ratnum
 $div-cflonum-flonum
 $div-cflonum-compnum
 $div-cflonum-cflonum

 $square-fixnum
 $square-bignum
 $square-ratnum
 $square-compnum
 $square-cflonum

 $cube-fixnum
 $cube-bignum
 $cube-ratnum
 $cube-compnum
 $cube-cflonum

 $gcd-number
 $gcd-number-number
 $gcd-fixnum-number
 $gcd-bignum-number
 $gcd-flonum-number
 $gcd-number-fixnum
 $gcd-number-bignum
 $gcd-number-flonum
 $gcd-fixnum-fixnum
 $gcd-fixnum-bignum
 $gcd-fixnum-flonum
 $gcd-bignum-fixnum
 $gcd-bignum-bignum
 $gcd-bignum-flonum
 $gcd-flonum-fixnum
 $gcd-flonum-bignum
 $gcd-flonum-flonum

 $lcm-number
 $lcm-number-number
 $lcm-fixnum-number
 $lcm-bignum-number
 $lcm-flonum-number
 $lcm-number-fixnum
 $lcm-number-bignum
 $lcm-number-flonum
 $lcm-fixnum-fixnum
 $lcm-fixnum-bignum
 $lcm-fixnum-flonum
 $lcm-bignum-fixnum
 $lcm-bignum-bignum
 $lcm-bignum-flonum
 $lcm-flonum-fixnum
 $lcm-flonum-bignum
 $lcm-flonum-flonum

 $quotient+remainder-fixnum-number
 $quotient+remainder-number-fixnum
 $quotient+remainder-bignum-number
 $quotient+remainder-number-bignum
 $quotient+remainder-flonum-number
 $quotient+remainder-number-flonum
 $quotient+remainder-fixnum-fixnum
 $quotient+remainder-bignum-fixnum
 $quotient+remainder-fixnum-bignum
 $quotient+remainder-bignum-bignum
 $quotient+remainder-fixnum-flonum
 $quotient+remainder-bignum-flonum
 $quotient+remainder-flonum-fixnum
 $quotient+remainder-flonum-bignum
 $quotient+remainder-flonum-flonum

 $quotient-fixnum-number
 $quotient-number-fixnum
 $quotient-bignum-number
 $quotient-number-bignum
 $quotient-flonum-number
 $quotient-number-flonum
 $quotient-fixnum-fixnum
 $quotient-fixnum-bignum
 $quotient-fixnum-flonum
 $quotient-bignum-fixnum
 $quotient-bignum-bignum
 $quotient-bignum-flonum
 $quotient-flonum-fixnum
 $quotient-flonum-bignum
 $quotient-flonum-flonum

 $remainder-fixnum-number
 $remainder-number-fixnum
 $remainder-bignum-number
 $remainder-number-bignum
 $remainder-flonum-number
 $remainder-number-flonum
 $remainder-fixnum-fixnum
 $remainder-fixnum-bignum
 $remainder-fixnum-flonum
 $remainder-bignum-fixnum
 $remainder-bignum-bignum
 $remainder-bignum-flonum
 $remainder-flonum-fixnum
 $remainder-flonum-bignum
 $remainder-flonum-flonum

 $modulo-fixnum-number
 $modulo-bignum-number
 $modulo-flonum-number
 $modulo-number-fixnum
 $modulo-number-bignum
 $modulo-number-flonum
 $modulo-fixnum-fixnum
 $modulo-fixnum-bignum
 $modulo-fixnum-flonum
 $modulo-bignum-fixnum
 $modulo-bignum-bignum
 $modulo-bignum-flonum
 $modulo-flonum-fixnum
 $modulo-flonum-bignum
 $modulo-flonum-flonum

 $max-fixnum-number
 $max-bignum-number
 $max-flonum-number
 $max-ratnum-number
 $max-number-fixnum
 $max-number-bignum
 $max-number-flonum
 $max-number-ratnum
 $max-fixnum-fixnum
 $max-fixnum-bignum
 $max-fixnum-flonum
 $max-fixnum-ratnum
 $max-bignum-fixnum
 $max-bignum-bignum
 $max-bignum-flonum
 $max-bignum-ratnum
 $max-flonum-flonum
 $max-flonum-fixnum
 $max-flonum-bignum
 $max-flonum-ratnum
 $max-ratnum-fixnum
 $max-ratnum-bignum
 $max-ratnum-ratnum
 $max-ratnum-flonum

 $min-fixnum-number
 $min-bignum-number
 $min-flonum-number
 $min-ratnum-number
 $min-number-fixnum
 $min-number-bignum
 $min-number-flonum
 $min-number-ratnum
 $min-fixnum-fixnum
 $min-fixnum-bignum
 $min-fixnum-flonum
 $min-fixnum-ratnum
 $min-bignum-fixnum
 $min-bignum-bignum
 $min-bignum-flonum
 $min-bignum-ratnum
 $min-flonum-flonum
 $min-flonum-fixnum
 $min-flonum-bignum
 $min-flonum-ratnum
 $min-ratnum-fixnum
 $min-ratnum-bignum
 $min-ratnum-ratnum
 $min-ratnum-flonum

 $abs-fixnum
 $abs-bignum
 $abs-flonum
 $abs-ratnum

 $sign-fixnum
 $sign-bignum
 $sign-flonum
 $sign-ratnum
;;;
 $expt-number-fixnum

 $expt-number-zero-fixnum
 $expt-fixnum-zero-fixnum
 $expt-flonum-zero-fixnum
 $expt-compnum-zero-fixnum
 $expt-cflonum-zero-fixnum

 $expt-number-negative-fixnum
 $expt-fixnum-negative-fixnum
 $expt-bignum-negative-fixnum
 $expt-ratnum-negative-fixnum
 $expt-flonum-negative-fixnum
 $expt-compnum-negative-fixnum
 $expt-cflonum-negative-fixnum

 $expt-number-positive-fixnum
 $expt-fixnum-positive-fixnum
 $expt-bignum-positive-fixnum
 $expt-flonum-positive-fixnum
 $expt-ratnum-positive-fixnum
 $expt-compnum-positive-fixnum
 $expt-cflonum-positive-fixnum

 $expt-fixnum-fixnum
 $expt-bignum-fixnum
 $expt-ratnum-fixnum
 $expt-flonum-fixnum
 $expt-compnum-fixnum
 $expt-cflonum-fixnum

 $expt-number-bignum
 $expt-fixnum-bignum
 $expt-bignum-bignum
 $expt-ratnum-bignum
 $expt-flonum-bignum
 $expt-compnum-bignum
 $expt-cflonum-bignum

 $expt-number-flonum
 $expt-number-ratnum
 $expt-number-compnum
 $expt-number-cflonum

 $expt-fixnum-flonum
 $expt-bignum-flonum
 $expt-ratnum-flonum
 $expt-flonum-flonum
 $expt-compnum-flonum
 $expt-cflonum-flonum

 $expt-fixnum-ratnum
 $expt-bignum-ratnum
 $expt-ratnum-ratnum
 $expt-flonum-ratnum
 $expt-compnum-ratnum
 $expt-cflonum-ratnum

 $expt-fixnum-cflonum
 $expt-bignum-cflonum
 $expt-ratnum-cflonum
 $expt-flonum-cflonum
 $expt-compnum-cflonum
 $expt-cflonum-cflonum

 $expt-fixnum-compnum
 $expt-bignum-compnum
 $expt-ratnum-compnum
 $expt-flonum-compnum
 $expt-compnum-compnum
 $expt-cflonum-compnum
;;;
 $sqrt-fixnum
 $sqrt-flonum
 $sqrt-bignum
 $sqrt-ratnum
 $sqrt-compnum
 $sqrt-cflonum

 $exact-integer-sqrt-fixnum
 $exact-integer-sqrt-bignum

 $cbrt-fixnum
 $cbrt-flonum
 $cbrt-bignum
 $cbrt-ratnum
 $cbrt-compnum
 $cbrt-cflonum

 $log-fixnum
 $log-flonum
 $log-bignum
 $log-ratnum
 $log-compnum
 $log-cflonum

 $exp-fixnum
 $exp-bignum
 $exp-ratnum
 $exp-flonum
 $exp-compnum
 $exp-cflonum

 $sin-fixnum
 $sin-bignum
 $sin-ratnum
 $sin-flonum
 $sin-cflonum
 $sin-compnum

 $cos-fixnum
 $cos-bignum
 $cos-ratnum
 $cos-flonum
 $cos-cflonum
 $cos-compnum

 $tan-fixnum
 $tan-bignum
 $tan-ratnum
 $tan-flonum
 $tan-compnum
 $tan-cflonum

 $asin-fixnum
 $asin-bignum
 $asin-ratnum
 $asin-flonum
 $asin-cflonum
 $asin-compnum

 $acos-fixnum
 $acos-bignum
 $acos-ratnum
 $acos-flonum
 $acos-cflonum
 $acos-compnum

 $atan2-real-real

 $atan-fixnum
 $atan-ratnum
 $atan-bignum
 $atan-flonum
 $atan-cflonum
 $atan-compnum

 $sinh-fixnum
 $sinh-bignum
 $sinh-ratnum
 $sinh-flonum
 $sinh-compnum
 $sinh-cflonum

 $cosh-fixnum
 $cosh-bignum
 $cosh-ratnum
 $cosh-flonum
 $cosh-compnum
 $cosh-cflonum

 $tanh-fixnum
 $tanh-bignum
 $tanh-ratnum
 $tanh-flonum
 $tanh-compnum
 $tanh-cflonum

 $asinh-fixnum
 $asinh-bignum
 $asinh-ratnum
 $asinh-flonum
 $asinh-cflonum
 $asinh-compnum

 $acosh-fixnum
 $acosh-bignum
 $acosh-ratnum
 $acosh-flonum
 $acosh-cflonum
 $acosh-compnum

 $atanh-fixnum
 $atanh-bignum
 $atanh-ratnum
 $atanh-flonum
 $atanh-cflonum
 $atanh-compnum

 $bitwise-not-fixnum
 $bitwise-not-bignum

 $bitwise-and-fixnum-number
 $bitwise-and-bignum-number
 $bitwise-and-fixnum-fixnum
 $bitwise-and-fixnum-bignum
 $bitwise-and-bignum-fixnum
 $bitwise-and-bignum-bignum

 $bitwise-ior-fixnum-number
 $bitwise-ior-bignum-number
 $bitwise-ior-fixnum-fixnum
 $bitwise-ior-fixnum-bignum
 $bitwise-ior-bignum-fixnum
 $bitwise-ior-bignum-bignum

 $bitwise-xor-fixnum-number
 $bitwise-xor-bignum-number
 $bitwise-xor-fixnum-fixnum
 $bitwise-xor-fixnum-bignum
 $bitwise-xor-bignum-fixnum
 $bitwise-xor-bignum-bignum

;;; --------------------------------------------------------------------

 tagged-identifier-syntax?
 list-of-tagged-bindings?
 tagged-lambda-proto-syntax?
 tagged-formals-syntax?
 standard-formals-syntax?
 formals-signature-syntax?
 retvals-signature-syntax?
 parse-tagged-identifier-syntax
 parse-list-of-tagged-bindings
 parse-tagged-lambda-proto-syntax
 parse-tagged-formals-syntax

 make-clambda-compound
 clambda-compound?
 clambda-compound-common-retvals-signature
 clambda-compound-lambda-signatures

 make-lambda-signature
 lambda-signature?
 lambda-signature-formals
 lambda-signature-retvals
 lambda-signature-formals-tags
 lambda-signature-retvals-tags
 lambda-signature=?

 make-formals-signature
 formals-signature?
 formals-signature-tags
 formals-signature=?

 make-retvals-signature
 retvals-signature?
 retvals-signature-tags
 retvals-signature=?
 retvals-signature-common-ancestor

 tag-identifier?
 all-tag-identifiers?
 tag-identifier-callable-signature
 tag-super-and-sub?
 tag-identifier-ancestry
 tag-common-ancestor
 formals-signature-super-and-sub-syntax?

 set-identifier-object-type-spec!
 identifier-object-type-spec
 set-label-object-type-spec!
 label-object-type-spec
 make-object-type-spec
 object-type-spec?
 object-type-spec-uids
 object-type-spec-type-id
 object-type-spec-parent-spec
 object-type-spec-pred-stx
 object-type-spec-constructor-maker
 object-type-spec-accessor-maker
 object-type-spec-mutator-maker
 object-type-spec-getter-maker
 object-type-spec-setter-maker
 object-type-spec-dispatcher
 object-type-spec-ancestry

 tagged-identifier?
 set-identifier-tag!
 identifier-tag
 set-label-tag!
 label-tag

 expand-time-retvals-signature-violation?
 expand-time-retvals-signature-violation-expected-signature
 expand-time-retvals-signature-violation-returned-signature

 top-tag-id
 void-tag-id
 procedure-tag-id
 list-tag-id
 boolean-tag-id

|#

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'declare-core-primitive	'scheme-indent-function 2)
;; eval: (put 'declare-pair-accessor	'scheme-indent-function 1)
;; eval: (put 'declare-pair-mutator	'scheme-indent-function 1)
;; End:
