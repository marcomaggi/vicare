;;; -*- coding: utf-8-unix -*-
;;;
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


(module
    (&syntactic-identifier
     &syntactic-identifier-rtd
     &syntactic-identifier-rcd
     make-syntactic-identifier-condition
     syntactic-identifier-condition?
     condition-syntactic-identifier

     &syntactic-binding-descriptor
     &syntactic-binding-descriptor-rtd
     &syntactic-binding-descriptor-rcd
     make-syntactic-binding-descriptor-condition
     syntactic-binding-descriptor-condition?
     condition-syntactic-binding-descriptor

     &object-type-spec
     &object-type-spec-rtd
     &object-type-spec-rcd
     make-object-type-spec-condition
     object-type-spec-condition?
     condition-object-type-spec
;;;
     &syntactic-identifier-resolution-rtd
     &syntactic-identifier-resolution-rcd
     &syntactic-identifier-resolution
     make-syntactic-identifier-resolution-violation
     syntactic-identifier-resolution-violation?

     &syntactic-identifier-unbound-rtd
     &syntactic-identifier-unbound-rcd
     &syntactic-identifier-unbound
     make-syntactic-identifier-unbound-condition
     syntactic-identifier-unbound-condition?

     &syntactic-identifier-out-of-context
     &syntactic-identifier-out-of-context-rtd
     &syntactic-identifier-out-of-context-rcd
     make-syntactic-identifier-out-of-context-condition
     syntactic-identifier-out-of-context-condition?

     &syntactic-identifier-not-type-identifier
     &syntactic-identifier-not-type-identifier-rtd
     &syntactic-identifier-not-type-identifier-rcd
     make-syntactic-identifier-not-type-identifier-condition
     syntactic-identifier-not-type-identifier-condition?
;;;
     &syntax-definition-expanded-rhs
     &syntax-definition-expanded-rhs-rtd
     &syntax-definition-expanded-rhs-rcd
     make-syntax-definition-expanded-rhs-condition
     syntax-definition-expanded-rhs-condition?
     condition-syntax-definition-expanded-rhs

     &syntax-definition-expression-return-value
     &syntax-definition-expression-return-value-rtd
     &syntax-definition-expression-return-value-rcd
     make-syntax-definition-expression-return-value-condition
     syntax-definition-expression-return-value-condition?
     condition-syntax-definition-expression-return-value

     &macro-expansion-trace
     &macro-expansion-trace-rtd
     &macro-expansion-trace-rcd
     make-macro-expansion-trace
     macro-expansion-trace?
     macro-expansion-trace-form

     &type-signature
     &type-signature-rtd
     &type-signature-rcd
     make-type-signature-condition
     type-signature-condition?
     condition-type-signature

;;; --------------------------------------------------------------------

     &application-argument-type-name
     &application-argument-type-name-rtd
     &application-argument-type-name-rcd
     make-application-argument-type-name-condition
     application-argument-type-name-condition?
     condition-application-argument-type-name

     &application-argument-index
     &application-argument-index-rtd
     &application-argument-index-rcd
     make-application-argument-index-condition
     application-argument-index-condition?
     condition-application-argument-index

     &application-operator-expression
     &application-operator-expression-rtd
     &application-operator-expression-rcd
     make-application-operator-expression-condition
     application-operator-expression-condition?
     condition-application-operator-expression

     &application-operands-expressions
     &application-operands-expressions-rtd
     &application-operands-expressions-rcd
     make-application-operands-expressions-condition
     application-operands-expressions-condition?
     condition-application-operands-expressions

     &application-operator-signature
     &application-operator-signature-rtd
     &application-operator-signature-rcd
     make-application-operator-signature-condition
     application-operator-signature-condition?
     condition-application-operator-signature

     &application-operand-signature
     &application-operand-signature-rtd
     &application-operand-signature-rcd
     make-application-operand-signature-condition
     application-operand-signature-condition?
     condition-application-operand-signature

;;; --------------------------------------------------------------------

     &expected-type-signature
     &expected-type-signature-rtd
     &expected-type-signature-rcd
     make-expected-type-signature-condition
     expected-type-signature-condition?
     condition-expected-type-signature

     &returned-type-signature
     &returned-type-signature-rtd
     &returned-type-signature-rcd
     make-returned-type-signature-condition
     returned-type-signature-condition?
     condition-returned-type-signature

;;; --------------------------------------------------------------------

     &type-method-name
     &type-method-name-rtd
     &type-method-name-rcd
     make-type-method-name-condition
     condition-type-method-name?
     condition-type-method-name

;;; --------------------------------------------------------------------

     &expand-time-type-signature-violation
     &expand-time-type-signature-violation-rtd
     &expand-time-type-signature-violation-rcd
     make-expand-time-type-signature-violation
     expand-time-type-signature-violation?

     &expand-time-type-signature-warning
     &expand-time-type-signature-warning-rtd
     &expand-time-type-signature-warning-rcd
     make-expand-time-type-signature-warning
     expand-time-type-signature-warning?

;;; --------------------------------------------------------------------

     assertion-error
     syntax-violation/internal-error
     assertion-violation/internal-error
     syntax-violation
     error-unbound-identifier
     error-identifier-out-of-context
     raise-compound-condition-object
     raise-compound-condition-object/continuable

     #| end of exports |# )


;;;; condition object types: descriptive objects, expansion process

;;This is used to represent the succession  of transformations a macro use input form
;;undergoes while  expanded; there is  an instance of  this condition type  for every
;;transformation.
;;
(define-condition-type &macro-expansion-trace
    &condition
  make-macro-expansion-trace macro-expansion-trace?
  (form macro-expansion-trace-form))
(define &macro-expansion-trace-rtd
  (record-type-descriptor &macro-expansion-trace))
(define &macro-expansion-trace-rcd
  (record-constructor-descriptor &macro-expansion-trace))

;;; --------------------------------------------------------------------

(define-condition-type &syntactic-identifier
    &condition
  make-syntactic-identifier-condition
  syntactic-identifier-condition?
  (syntactic-identifier		condition-syntactic-identifier))
(define &syntactic-identifier-rtd
  (record-type-descriptor &syntactic-identifier))
(define &syntactic-identifier-rcd
  (record-constructor-descriptor &syntactic-identifier))

(define-condition-type &syntactic-binding-descriptor
    &condition
  make-syntactic-binding-descriptor-condition
  syntactic-binding-descriptor-condition?
  (syntactic-binding-descriptor	condition-syntactic-binding-descriptor))
(define &syntactic-binding-descriptor-rtd
  (record-type-descriptor &syntactic-binding-descriptor))
(define &syntactic-binding-descriptor-rcd
  (record-constructor-descriptor &syntactic-binding-descriptor))

;;; --------------------------------------------------------------------

(define-condition-type &syntactic-identifier-resolution
    &violation
  make-syntactic-identifier-resolution-violation
  syntactic-identifier-resolution-violation?)
(define &syntactic-identifier-resolution-rtd
  (record-type-descriptor &syntactic-identifier-resolution))
(define &syntactic-identifier-resolution-rcd
  (record-constructor-descriptor &syntactic-identifier-resolution))

(define-condition-type &syntactic-identifier-unbound
    &syntactic-identifier-resolution
  make-syntactic-identifier-unbound-condition
  syntactic-identifier-unbound-condition?)
(define &syntactic-identifier-unbound-rtd
  (record-type-descriptor &syntactic-identifier-unbound))
(define &syntactic-identifier-unbound-rcd
  (record-constructor-descriptor &syntactic-identifier-unbound))

(define-condition-type &syntactic-identifier-out-of-context
    &syntactic-identifier-resolution
  make-syntactic-identifier-out-of-context-condition
  syntactic-identifier-out-of-context-condition?)
(define &syntactic-identifier-out-of-context-rtd
  (record-type-descriptor &syntactic-identifier-out-of-context))
(define &syntactic-identifier-out-of-context-rcd
  (record-constructor-descriptor &syntactic-identifier-out-of-context))

(define-condition-type &syntactic-identifier-not-type-identifier
    &syntactic-identifier-resolution
  make-syntactic-identifier-not-type-identifier-condition
  syntactic-identifier-not-type-identifier-condition?)
(define &syntactic-identifier-not-type-identifier-rtd
  (record-type-descriptor &syntactic-identifier-not-type-identifier))
(define &syntactic-identifier-not-type-identifier-rcd
  (record-constructor-descriptor &syntactic-identifier-not-type-identifier))

(define-condition-type &object-type-spec
    &condition
  make-object-type-spec-condition
  object-type-spec-condition?
  (object-type-spec		condition-object-type-spec))
(define &object-type-spec-rtd
  (record-type-descriptor &object-type-spec))
(define &object-type-spec-rcd
  (record-constructor-descriptor &object-type-spec))

;;; --------------------------------------------------------------------

;;This  is used  to describe  exceptions raised  while expanding  and evaluating  the
;;right-hand side (RHS) expression of a syntax definition (DEFINE-SYNTAX, LET-SYNTAX,
;;LETREC-SYNTAX, DEFINE-FLUID-SYNTAX, FLUID-LET-SYNTAX, et cetera).
;;
;;The value in the field SYNTAX-DEFINITION-EXPANDED-RHS must be a symbolic expression
;;representing the a core language expression.
;;
(define-condition-type &syntax-definition-expanded-rhs
    &condition
  make-syntax-definition-expanded-rhs-condition
  syntax-definition-expanded-rhs-condition?
  (syntax-definition-expanded-rhs	condition-syntax-definition-expanded-rhs))
(define &syntax-definition-expanded-rhs-rtd
  (record-type-descriptor &syntax-definition-expanded-rhs))
(define &syntax-definition-expanded-rhs-rcd
  (record-constructor-descriptor &syntax-definition-expanded-rhs))

;;This  is used  to describe  exceptions raised  while expanding  and evaluating  the
;;right-hand side (RHS) expression of a syntax definition (DEFINE-SYNTAX, LET-SYNTAX,
;;LETREC-SYNTAX, DEFINE-FLUID-SYNTAX, FLUID-LET-SYNTAX, etc.).
;;
;;The  value in  field  SYNTAX-DEFINITION-EXPRESSION-RETURN-VALUE must  be the  value
;;returned by the evaluation of the RHS expression.
;;
(define-condition-type &syntax-definition-expression-return-value
    &condition
  make-syntax-definition-expression-return-value-condition
  syntax-definition-expression-return-value-condition?
  (syntax-definition-expression-return-value	condition-syntax-definition-expression-return-value))
(define &syntax-definition-expression-return-value-rtd
  (record-type-descriptor &syntax-definition-expression-return-value))
(define &syntax-definition-expression-return-value-rcd
  (record-constructor-descriptor &syntax-definition-expression-return-value))


;;;; condition object types: descriptive objects, application forms

;;This used to describe the syntax object  acting as operator in an application form.
;;The single  argument to the  constructor must be  a syntax object  representing the
;;operator.
;;
(define-condition-type &application-operator-expression
    &condition
  make-application-operator-expression-condition
  application-operator-expression-condition?
  (operator	condition-application-operator-expression))
(define &application-operator-expression-rtd
  (record-type-descriptor &application-operator-expression))
(define &application-operator-expression-rcd
  (record-constructor-descriptor &application-operator-expression))

;;This is used  to describe the syntax  objects acting as operands  in an application
;;form.  The  single argument  to the constructor  must be a  list of  syntax objects
;;representing the operands.
(define-condition-type &application-operands-expressions
    &condition
  make-application-operands-expressions-condition
  application-operands-expressions-condition?
  (operands	condition-application-operands-expressions))
(define &application-operands-expressions-rtd
  (record-type-descriptor &application-operands-expressions))
(define &application-operands-expressions-rcd
  (record-constructor-descriptor &application-operands-expressions))

;;This is used to hold a "<type-signature>" instance representing the types of values
;;returned  by an  expression  used  as operator  in  a  procedure application  form.
;;Examples: if  the operator  returns zero, two  or more values;  if the  operator is
;;marked as no-return; if the operator does not return a procedure.
;;
(define-condition-type &application-operator-signature
    &condition
  make-application-operator-signature-condition
  application-operator-signature-condition?
  (signature	condition-application-operator-signature))
(define &application-operator-signature-rtd
  (record-type-descriptor &application-operator-signature))
(define &application-operator-signature-rcd
  (record-constructor-descriptor &application-operator-signature))

;;This is used to hold a "<type-signature>" instance representing the types of values
;;returned by an expression used as operand in procedure application form.  Examples:
;;if  the operand  returns zero,  two or  more values;  if the  operand is  marked as
;;no-return.
;;
(define-condition-type &application-operand-signature
    &condition
  make-application-operand-signature-condition
  application-operand-signature-condition?
  (signature	condition-application-operand-signature))
(define &application-operand-signature-rtd
  (record-type-descriptor &application-operand-signature))
(define &application-operand-signature-rcd
  (record-constructor-descriptor &application-operand-signature))

;;This  is used  to describe  the type  identifier of  an argument  to function,  the
;;expected type of object used in the function call.
;;
(define-condition-type &application-argument-type-name
    &condition
  make-application-argument-type-name-condition
  application-argument-type-name-condition?
  (argument-type-name		condition-application-argument-type-name))
(define &application-argument-type-name-rtd
  (record-type-descriptor &application-argument-type-name))
(define &application-argument-type-name-rcd
  (record-constructor-descriptor &application-argument-type-name))

(define-condition-type &application-argument-index
    &condition
  make-application-argument-index-condition
  application-argument-index-condition?
  (application-argument-index	condition-application-argument-index))
(define &application-argument-index-rtd
  (record-type-descriptor &application-argument-index))
(define &application-argument-index-rcd
  (record-constructor-descriptor &application-argument-index))


;;;; condition object types: descriptive objects, type signatures

(define-condition-type &type-signature
    &condition
  make-type-signature-condition
  type-signature-condition?
  (type-signature		condition-type-signature))
(define &type-signature-rtd
  (record-type-descriptor &type-signature))
(define &type-signature-rcd
  (record-constructor-descriptor &type-signature))

(define-condition-type &expected-type-signature
    &condition
  make-expected-type-signature-condition
  expected-type-signature-condition?
  (expected-type-signature	condition-expected-type-signature))
(define &expected-type-signature-rtd
  (record-type-descriptor &expected-type-signature))
(define &expected-type-signature-rcd
  (record-constructor-descriptor &expected-type-signature))

(define-condition-type &returned-type-signature
    &condition
  make-returned-type-signature-condition
  returned-type-signature-condition?
  (returned-type-signature	condition-returned-type-signature))
(define &returned-type-signature-rtd
  (record-type-descriptor &returned-type-signature))
(define &returned-type-signature-rcd
  (record-constructor-descriptor &returned-type-signature))


;;;; condition object types: descriptive objects

;;This is used to describe a type's  method name involved in an exception.  The field
;;must be a symbol representing the method name.
(define-condition-type &type-method-name
    &condition
  make-type-method-name-condition
  condition-type-method-name?
  (method-name condition-type-method-name))
(define &type-method-name-rtd
  (record-type-descriptor &type-method-name))
(define &type-method-name-rcd
  (record-constructor-descriptor &type-method-name))


;;;; condition object types: error objects

;;This  is used  to  describe exceptions  in  which  there is  a  mismatch between  a
;;resulting expression type signature and an expected one; this condition type should
;;be the root of all the condition types in this category.
(define-condition-type &expand-time-type-signature-violation
    &violation
  make-expand-time-type-signature-violation
  expand-time-type-signature-violation?)
(define &expand-time-type-signature-violation-rtd
  (record-type-descriptor &expand-time-type-signature-violation))
(define &expand-time-type-signature-violation-rcd
  (record-constructor-descriptor &expand-time-type-signature-violation))

(define-condition-type &expand-time-type-signature-warning
    &warning
  make-expand-time-type-signature-warning
  expand-time-type-signature-warning?)
(define &expand-time-type-signature-warning-rtd
  (record-type-descriptor &expand-time-type-signature-warning))
(define &expand-time-type-signature-warning-rcd
  (record-constructor-descriptor &expand-time-type-signature-warning))

;;; --------------------------------------------------------------------

(define-condition-type &vicare-scheme-internal-error
    &error
  make-vicare-scheme-internal-error
  vicare-scheme-internal-error?)


;;;; exception raising functions

(define (assertion-error expr source-identifier
			 byte-offset character-offset
			 line-number column-number)
  ;;Invoked by the expansion of the ASSERT macro to raise an assertion violation.
  ;;
  (raise
   (condition (make-assertion-violation)
	      (make-who-condition 'assert)
	      (make-message-condition "assertion failed")
	      (make-irritants-condition (list expr))
	      (make-source-position-condition source-identifier
	      				      byte-offset character-offset
	      				      line-number column-number))))

(case-define* syntax-violation/internal-error
  ((who msg form)
   (syntax-violation/internal-error who msg form #f))
  ((who {msg string?} form subform)
   (raise
    (condition (make-who-condition who)
	       (make-message-condition (string-append "Vicare Scheme: internal error: " msg))
	       (make-syntax-violation form subform)
	       (make-vicare-scheme-internal-error)))))

(define* (assertion-violation/internal-error who {msg string?} . irritants)
  (raise
   (condition (make-vicare-scheme-internal-error)
	      (make-assertion-violation)
	      (make-who-condition who)
	      (make-message-condition (string-append "Vicare Scheme: internal error: " msg))
	      (make-irritants-condition irritants))))

(module (error-unbound-identifier
	 error-identifier-out-of-context
	 syntax-violation
	 raise-compound-condition-object
	 raise-compound-condition-object/continuable)

  (case-define error-unbound-identifier
    ((source-who id)
     (error-unbound-identifier source-who id (condition)))
    ((source-who id cnd)
     ;;Raise an  "unbound identifier" exception.   This is  to be used  when applying
     ;;ID->LABEL  to the  identifier ID  returns false,  and such  result is  invalid
     ;;because we were expecting ID to be bound.
     ;;
     (raise
      (condition (make-undefined-violation)
		 (make-syntactic-identifier-unbound-condition)
		 (make-who-condition source-who)
		 (make-message-condition "unbound syntactic identifier")
		 (make-syntactic-identifier-condition id)
		 (%expression->source-position-condition id)
		 cnd
		 (%extract-macro-expansion-trace id)))))

  (case-define error-identifier-out-of-context
    ((caller-who id)
     (error-identifier-out-of-context caller-who id (condition)))
    ((caller-who id cnd)
     (raise
      (condition (make-syntactic-identifier-out-of-context-condition)
		 (make-who-condition caller-who)
		 (make-message-condition "identifier out of context (identifier's label not in LEXENV)")
		 (make-syntactic-identifier-condition id)
		 (%expression->source-position-condition id)
		 cnd
		 (%extract-macro-expansion-trace id)))))

  (module (raise-compound-condition-object raise-compound-condition-object/continuable syntax-violation)

    (case-define* syntax-violation
      ;;Defined by R6RS.  WHO must be false or a string or a symbol.  MESSAGE must be
      ;;a string.  FORM must be a syntax object  or a datum value.  SUBFORM must be a
      ;;syntax object or a datum value.
      ;;
      ;;The  SYNTAX-VIOLATION  procedure  raises  an exception,  reporting  a  syntax
      ;;violation.   WHO should  describe  the macro  transformer  that detected  the
      ;;exception.  The MESSAGE argument should  describe the violation.  FORM should
      ;;be the erroneous  source syntax object or a datum  value representing a form.
      ;;The  optional SUBFORM  argument  should be  a syntax  object  or datum  value
      ;;representing a form that more precisely locates the violation.
      ;;
      ;;If WHO is false, SYNTAX-VIOLATION attempts  to infer an appropriate value for
      ;;the  condition  object  (see  below)  as follows:  when  FORM  is  either  an
      ;;identifier or a list-structured syntax object containing an identifier as its
      ;;first  element,  then   the  inferred  value  is   the  identifier's  symbol.
      ;;Otherwise, no value for WHO is provided as part of the condition object.
      ;;
      ;;The condition object provided with  the exception has the following condition
      ;;types:
      ;;
      ;;* If WHO  is not false or  can be inferred, the condition  has condition type
      ;;  "&who",  with WHO  as the  value of its  field.  In  that case,  WHO should
      ;;  identify  the procedure or  entity that detected  the exception.  If  it is
      ;;  false, the condition does not have condition type "&who".
      ;;
      ;;* The condition  has condition type "&message", with MESSAGE  as the value of
      ;;  its field.
      ;;
      ;;* The  condition has condition  type "&syntax" with  FORM and SUBFORM  as the
      ;;  value of its fields.  If SUBFORM  is not provided, the value of the subform
      ;;  field is false.
      ;;
      ((who {msg string?} form)
       (syntax-violation who msg form #f))
      ((who {msg string?} form subform)
       (%raise-compound-condition-object __who__ #f who msg form subform (make-syntax-violation form subform))))

    (define* (raise-compound-condition-object source-who {msg string?} input-form.stx condition-object)
      (%raise-compound-condition-object __who__ #f source-who msg input-form.stx #f condition-object))

    (define* (raise-compound-condition-object/continuable source-who {msg string?} input-form.stx condition-object)
      (%raise-compound-condition-object __who__ #t source-who msg input-form.stx #f condition-object))

    (define* (%raise-compound-condition-object caller-who continuable? source-who msg input-form.stx sub-form.stx condition-object)
      ;;Raise a compound condition object.
      ;;
      ;;SOURCE-WHO can be a string, symbol or  false; it is used as value for "&who".
      ;;When false:  INPUT-FORM.STX is  inspected to determine  a possible  value for
      ;;"&who".
      ;;
      ;;MSG must be a string; it is used as value for "&message".
      ;;
      ;;INPUT-FORM.STX must  be a (wrapped  or unwrapped) syntax  object representing
      ;;the subject of the  raised exception.  It is used for  both inferring a value
      ;;for "&who" and retrieving source location informations.
      ;;
      ;;SUB-FORM.STX  must  be  false  or  a (wrapped  or  unwrapped)  syntax  object
      ;;representing the  subject of the  raised exception.   It is used  to retrieve
      ;;source location informations.
      ;;
      ;;CONDITION-OBJECT is  an already built condition  object that is added  to the
      ;;raised compound.
      ;;
      (define the-who
	(cond ((or (string? source-who)
		   (symbol? source-who))
	       source-who)
	      ((not source-who)
	       (syntax-case input-form.stx ()
		 (?id
		  (identifier? #'?id)
		  (syntax->datum #'?id))
		 ((?id . ?rest)
		  (identifier? #'?id)
		  (syntax->datum #'?id))
		 (_  #f)))
	      (else
	       (assertion-violation caller-who "invalid who argument" source-who))))
      (define C1
	(condition (make-message-condition msg)
		   condition-object
		   (%expression->source-position-condition (or sub-form.stx input-form.stx))
		   (%extract-macro-expansion-trace         (or sub-form.stx input-form.stx))))
      (define C2
	(if the-who
	    (condition (make-who-condition the-who) C1)
	  C1))
      (if continuable?
	  (raise-continuable C2)
	(raise C2)))

    #| end of module |# )

  (define (%extract-macro-expansion-trace X)
    ;;Extract from the  (wrapped or unwrapped) syntax object X  the sequence of macro
    ;;expansion traces from the AE* field of wrapped syntax-object records and return
    ;;a   compound   condition   object    representing   them,   as   instances   of
    ;;"&macro-expansion-trace".
    ;;
    ;;NOTE Unfortunately it  does not always go  as we would like.   For example, the
    ;;program:
    ;;
    ;;   (import (vicare))
    ;;   (define-syntax (one stx)
    ;;     (syntax-case stx ()
    ;;       ((_)
    ;;        (syntax-violation 'one "demo" stx #f))))
    ;;   (define-syntax (two stx)
    ;;     (syntax-case stx ()
    ;;       ((_)
    ;;        #'(one))))
    ;;   (two)
    ;;
    ;;raises the error:
    ;;
    ;;*** Vicare: unhandled exception:
    ;;  Condition components:
    ;;    1. &who: one
    ;;    2. &message: "demo"
    ;;    3. &syntax:
    ;;        form: #<syntax expr=(one) mark*=(#f "" src) ...>
    ;;        subform: #f
    ;;    4. &source-position:
    ;;        port-id: "../tests/test-demo.sps"
    ;;        byte: 516
    ;;        character: 514
    ;;        line: 31
    ;;        column: 8
    ;;    5. &macro-expansion-trace: #<syntax expr=(one) mark*=(#f "" src) ...>
    ;;    6. &macro-expansion-trace: #<syntax expr=(two) mark*=(src) ...>
    ;;
    ;;and we can see  the expansion's trace.  But if we compose  the output form with
    ;;pieces of different origin:
    ;;
    ;;   (import (vicare))
    ;;   (define-syntax (one stx)
    ;;     (syntax-case stx ()
    ;;       ((_ . ?stuff)
    ;;        (syntax-violation 'one "demo" stx #f))))
    ;;   (define-syntax (two stx)
    ;;     (syntax-case stx ()
    ;;       ((_ ?id)
    ;;        #`(one ?id))))
    ;;   (two display)
    ;;
    ;;the error is:
    ;;
    ;;   *** Vicare: unhandled exception:
    ;;    Condition components:
    ;;      1. &who: one
    ;;      2. &message: "demo"
    ;;      3. &syntax:
    ;;          form: (#<syntax expr=one mark*=(#f "" src) ...>
    ;;                 #<syntax expr=display mark*=(#f src) ...>
    ;;                 . #<syntax expr=() mark*=(#f "" src)>)
    ;;          subform: #f
    ;;
    ;;and there is no trace.  This is  because the syntax object used as "form" value
    ;;in "&syntax" is  not wrapped, and SYNTAX-VIOLATION cannot decide  from which of
    ;;its components it makes sense to extract  the trace.
    ;;
    ;;To solve this problem,  we could try to wrap the output form  of macros into an
    ;;instance of "<stx>" in DO-MACRO-CALL as follows:
    ;;
    ;;   (let ((return-form.stx (add-new-mark rib output-form.stx input-form.stx)))
    ;;     (if (options::debug-mode-enabled?)
    ;;         (stx-push-annotated-expr return-form.stx input-form.stx)
    ;;       return-form.stx))
    ;;
    ;;and to  modify %MERGE-ANNOTATED-EXPR* to  use %APPEND-CANCEL-FACING; but  it is
    ;;not so simple.  (Marco Maggi; Sat Apr 12, 2014)
    ;;
    (if (options::debug-mode-enabled?)
	(cond ((stx? X)
	       ;;X is a wrapped syntax object: so the items in its stack of annotated
	       ;;expressions are all wrapped syntax objects.
	       (let* ((ae*    (stx-annotated-expr* X))
		      (trace* (map make-macro-expansion-trace ae*)))
		 (apply condition (if (syntax=? X (car ae*))
				      trace*
				    (cons (make-macro-expansion-trace X) trace*)))))

	      ((reader-annotation? X)
	       ;;Here we only  want to wrap X into an  syntax object, we
	       ;;do not care about the context.
	       (make-macro-expansion-trace (make-stx-or-syntactic-identifier X '() '() '())))

	      (else
	       (condition)))
      (condition)))

  (define (%expression->source-position-condition x)
    (expression-position x))

  #| end of module |# )


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.lexical-environment.condition-objects end")))

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
