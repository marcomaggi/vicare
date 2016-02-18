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
    (&type-syntactic-identifier
     &type-syntactic-identifier-rtd
     &type-syntactic-identifier-rcd
     make-type-syntactic-identifier-condition
     type-syntactic-identifier-condition?
     condition-type-syntactic-identifier

     &argument-type-syntactic-identifier
     &argument-type-syntactic-identifier-rtd
     &argument-type-syntactic-identifier-rcd
     make-argument-type-syntactic-identifier-condition
     argument-type-syntactic-identifier-condition?
     condition-argument-type-syntactic-identifier

     &operand-type-syntactic-identifier
     &operand-type-syntactic-identifier-rtd
     &operand-type-syntactic-identifier-rcd
     make-operand-type-syntactic-identifier-condition
     operand-type-syntactic-identifier-condition?
     condition-operand-type-syntactic-identifier

     &argument-index
     &argument-index-rtd
     &argument-index-rcd
     make-argument-index-condition
     argument-index-condition?
     condition-argument-index

     &type-method-name
     make-type-method-name-condition
     condition-type-method-name?
     condition-type-method-name

     &syntactic-binding-descriptor
     make-syntactic-binding-descriptor-condition
     syntactic-binding-descriptor-condition?
     condition-syntactic-binding-descriptor

     &syntax-definition-expanded-rhs-condition
     make-syntax-definition-expanded-rhs-condition
     syntax-definition-expanded-rhs-condition?
     condition-syntax-definition-expanded-rhs

     &syntax-definition-expression-return-value
     make-syntax-definition-expression-return-value-condition
     syntax-definition-expression-return-value-condition?
     condition-syntax-definition-expression-return-value

     &application-operator
     make-application-operator-condition
     application-operator-condition?
     application-operator-condition.operator

     &application-operands
     make-application-operands-condition
     application-operands-condition?
     application-operands-condition.operands

     &retvals-signature-condition
     make-retvals-signature-condition
     retvals-signature-condition?
     retvals-signature-condition-signature

     &macro-expansion-trace
     make-macro-expansion-trace macro-expansion-trace?
     macro-expansion-trace-form

     &expand-time-type-signature-violation
     &expand-time-type-signature-violation-rtd
     &expand-time-type-signature-violation-rcd
     make-expand-time-type-signature-violation
     expand-time-type-signature-violation?

     &expand-time-retvals-signature-violation
     &expand-time-retvals-signature-violation-rtd
     &expand-time-retvals-signature-violation-rcd
     make-expand-time-retvals-signature-violation
     expand-time-retvals-signature-violation?
     expand-time-retvals-signature-violation-expected-signature
     expand-time-retvals-signature-violation-returned-signature

     &expand-time-type-signature-warning
     &expand-time-type-signature-warning-rtd
     &expand-time-type-signature-warning-rcd
     make-expand-time-type-signature-warning
     expand-time-type-signature-warning?

     &syntax-warning
     &syntax-warning-rtd
     &syntax-warning-rcd
     make-syntax-warning syntax-warning?
     syntax-warning-form
     syntax-warning-subform

     assertion-error
     syntax-violation/internal-error
     assertion-violation/internal-error
     syntax-violation
     expand-time-retvals-signature-violation
     raise-unbound-error
     raise-compound-condition-object
     raise-compound-condition-object/continuable

     #| end of exports |# )


;;;; condition object types: descriptive objects

;;This used to describe the syntax object  acting as operator in an application form.
;;The single  argument to the  constructor must be  a syntax object  representing the
;;operator.
;;
(define-condition-type &application-operator
    &condition
  make-application-operator-condition
  application-operator-condition?
  (operator application-operator-condition.operator))

;;This used to describe the syntax objects acting as operands in an application form.
;;The  single  argument  to  the  constructor  must  be  a  list  of  syntax  objects
;;representing the operands.
(define-condition-type &application-operands
    &condition
  make-application-operands-condition
  application-operands-condition?
  (operands application-operands-condition.operands))

;;This is used to describe a type involved in an exception, for example when the type
;;identifier is  used by  the syntax  IS-A? or  METHOD-CALL.  The  field must  be the
;;syntactic identifier  bound to the type  specification (for example the  name of an
;;R6RS record type).
;;
(define-condition-type &type-syntactic-identifier
    &condition
  make-type-syntactic-identifier-condition
  type-syntactic-identifier-condition?
  (type-identifier	condition-type-syntactic-identifier))
(define &type-syntactic-identifier-rtd
  (record-type-descriptor &type-syntactic-identifier))
(define &type-syntactic-identifier-rcd
  (record-constructor-descriptor &type-syntactic-identifier))

;;This  is used  to describe  the type  identifier of  an argument  to function,  the
;;expected type of object used in the function call.
;;
(define-condition-type &argument-type-syntactic-identifier
    &condition
  make-argument-type-syntactic-identifier-condition
  argument-type-syntactic-identifier-condition?
  (argument-type-identifier	condition-argument-type-syntactic-identifier))
(define &argument-type-syntactic-identifier-rtd
  (record-type-descriptor &argument-type-syntactic-identifier))
(define &argument-type-syntactic-identifier-rcd
  (record-constructor-descriptor &argument-type-syntactic-identifier))

;;This is used to describe the type identifier of an operand to function, the type of
;;the actual object used as operand in a specific function call.
;;
(define-condition-type &operand-type-syntactic-identifier
    &condition
  make-operand-type-syntactic-identifier-condition
  operand-type-syntactic-identifier-condition?
  (operand-type-identifier	condition-operand-type-syntactic-identifier))
(define &operand-type-syntactic-identifier-rtd
  (record-type-descriptor &operand-type-syntactic-identifier))
(define &operand-type-syntactic-identifier-rcd
  (record-constructor-descriptor &operand-type-syntactic-identifier))

(define-condition-type &argument-index
    &condition
  make-argument-index-condition
  argument-index-condition?
  (argument-index	condition-argument-index))
(define &argument-index-rtd
  (record-type-descriptor &argument-index))
(define &argument-index-rcd
  (record-constructor-descriptor &argument-index))

;;This is used to describe a type's  method name involved in an exception.  The field
;;must be a symbol representing the method name.
(define-condition-type &type-method-name
    &condition
  make-type-method-name-condition
  condition-type-method-name?
  (method-name condition-type-method-name))

(define-condition-type &syntactic-binding-descriptor
    &condition
  make-syntactic-binding-descriptor-condition
  syntactic-binding-descriptor-condition?
  (descr	condition-syntactic-binding-descriptor))

;;This  is  used  to describe  exceptions  in  which  the  expanded expression  of  a
;;right-hand side (RHS) syntax  definition (DEFINE-SYNTAX, LET-SYNTAX, LETREC-SYNTAX,
;;DEFINE-FLUID-SYNTAX,  FLUID-LET-SYNTAX,  etc.)   has  a role.   The  value  in  the
;;CORE-EXPR slot must be a symbolic expression representing the a core expression.
(define-condition-type &syntax-definition-expanded-rhs-condition
    &condition
  make-syntax-definition-expanded-rhs-condition
  syntax-definition-expanded-rhs-condition?
  (core-expr condition-syntax-definition-expanded-rhs))

;;This is used to describe exceptions in  which the return value of the evaluation of
;;a   right-hand   side   (RHS)   syntax   definition   (DEFINE-SYNTAX,   LET-SYNTAX,
;;LETREC-SYNTAX, DEFINE-FLUID-SYNTAX, FLUID-LET-SYNTAX, etc.)  has a role.  The value
;;in the  RETVAL slot  must be  the value returned  by the  evaluation of  the syntax
;;definition RHS expression.
(define-condition-type &syntax-definition-expression-return-value
    &condition
  make-syntax-definition-expression-return-value-condition
  syntax-definition-expression-return-value-condition?
  (retval condition-syntax-definition-expression-return-value))

;;This  is used  to include  a retvals  signature specification  in generic  compound
;;objects, for example because we were expecting a signature with some properties and
;;the one we got does not have them.
(define-condition-type &retvals-signature-condition
    &condition
  %make-retvals-signature-condition
  retvals-signature-condition?
  (signature retvals-signature-condition-signature))

(define* (make-retvals-signature-condition {sig type-signature?})
  (%make-retvals-signature-condition sig))

;;This is used to represent the succession  of transformations a macro use input form
;;undergoes while  expanded; there is  an instance of  this condition type  for every
;;transformation.
;;
(define-condition-type &macro-expansion-trace
    &condition
  make-macro-expansion-trace macro-expansion-trace?
  (form macro-expansion-trace-form))


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

(define-condition-type &syntax-warning
    &warning
  make-syntax-warning syntax-warning?
  (form syntax-warning-form)
  (subform syntax-warning-subform))

(define &syntax-warning-rtd
  (record-type-descriptor &syntax-warning))

(define &syntax-warning-rcd
  (record-constructor-descriptor &syntax-warning))

;;; --------------------------------------------------------------------

;;This is  used to describe  exceptions in which:  after expanding an  expression, we
;;were expecting it to  have a "retvals-signature" matching a given  one, but the one
;;we got does not match.
;;
;;See the function EXPAND-TIME-RETVALS-SIGNATURE-VIOLATION for details.
(define-condition-type &expand-time-retvals-signature-violation
    &expand-time-type-signature-violation
  %make-expand-time-retvals-signature-violation
  expand-time-retvals-signature-violation?
  (expected-signature expand-time-retvals-signature-violation-expected-signature)
  (returned-signature expand-time-retvals-signature-violation-returned-signature))

(define &expand-time-retvals-signature-violation-rtd
  (record-type-descriptor &expand-time-retvals-signature-violation))

(define &expand-time-retvals-signature-violation-rcd
  (record-constructor-descriptor &expand-time-retvals-signature-violation))

(define* (make-expand-time-retvals-signature-violation {expected-signature type-signature?}
						       {returned-signature type-signature?})
  (%make-expand-time-retvals-signature-violation expected-signature returned-signature))

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

(module (raise-unbound-error
	 syntax-violation
	 expand-time-retvals-signature-violation
	 raise-compound-condition-object
	 raise-compound-condition-object/continuable)

  (case-define* syntax-violation
    ;;Defined by R6RS.  WHO must be false or a string or a symbol.  MESSAGE must be a
    ;;string.  FORM  must be a  syntax object  or a datum  value.  SUBFORM must  be a
    ;;syntax object or a datum value.
    ;;
    ;;The  SYNTAX-VIOLATION  procedure  raises   an  exception,  reporting  a  syntax
    ;;violation.   WHO  should  describe  the macro  transformer  that  detected  the
    ;;exception.  The MESSAGE argument should describe the violation.  FORM should be
    ;;the erroneous source  syntax object or a datum value  representing a form.  The
    ;;optional SUBFORM argument should be a syntax object or datum value representing
    ;;a form that more precisely locates the violation.
    ;;
    ;;If WHO  is false, SYNTAX-VIOLATION attempts  to infer an appropriate  value for
    ;;the condition object (see below) as  follows: when FORM is either an identifier
    ;;or  a list-structured  syntax  object  containing an  identifier  as its  first
    ;;element, then  the inferred  value is the  identifier's symbol.   Otherwise, no
    ;;value for WHO is provided as part of the condition object.
    ;;
    ;;The condition  object provided with  the exception has the  following condition
    ;;types:
    ;;
    ;;* If  WHO is not  false or  can be inferred,  the condition has  condition type
    ;;   "&who", with  WHO as  the value  of  its field.   In that  case, WHO  should
    ;;   identify the  procedure or  entity that  detected the  exception.  If  it is
    ;;  false, the condition does not have condition type "&who".
    ;;
    ;;* The condition has condition type "&message", with MESSAGE as the value of its
    ;;  field.
    ;;
    ;;* The condition has condition type "&syntax" with FORM and SUBFORM as the value
    ;;  of its fields.  If SUBFORM is not provided, the value of the subform field is
    ;;  false.
    ;;
    ((who {msg string?} form)
     (syntax-violation who msg form #f))
    ((who {msg string?} form subform)
     (raise-compound-condition-object who msg form (make-syntax-violation form subform))))

  (define* (expand-time-retvals-signature-violation source-who form subform
						    {expected-retvals-signature type-signature?}
						    {returned-retvals-signature type-signature?})
    ;;To be used at  expand-time when we were expecting a  signature from an expanded
    ;;expression  and we  received  an incompatible  one, for  example  in the  macro
    ;;transformers of TAG-ASSERT and TAG-ASSERT-AND-RETURN.
    ;;
    (raise-compound-condition-object source-who "expand-time return values signature mismatch" form
				     (condition
				      (%make-expand-time-retvals-signature-violation expected-retvals-signature
										     returned-retvals-signature)
				      (make-syntax-violation form subform))))

  (define (raise-unbound-error source-who input-form.stx id)
    ;;Raise an  "unbound identifier"  exception.  This  is to  be used  when applying
    ;;ID->LABEL  to the  identifier  ID returns  false, and  such  result is  invalid
    ;;because we were expecting ID to be bound.
    ;;
    ;;Often INPUT-FORM.STX is ID itself, and we can do nothing about it.
    ;;
    (raise-compound-condition-object source-who "unbound identifier" input-form.stx
				     (condition
				      (make-undefined-violation)
				      (make-syntax-violation input-form.stx id))))

  (module (raise-compound-condition-object raise-compound-condition-object/continuable)

    (define* (raise-compound-condition-object source-who {msg string?} input-form.stx condition-object)
      (%raise-compound-condition-object #f source-who msg input-form.stx condition-object))

    (define* (raise-compound-condition-object/continuable source-who {msg string?} input-form.stx condition-object)
      (%raise-compound-condition-object #t source-who msg input-form.stx condition-object))

    (define (%raise-compound-condition-object continuable? source-who msg input-form.stx condition-object)
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
	       (assertion-violation __who__ "invalid who argument" source-who))))
      (define C1
	(condition (make-message-condition msg)
		   condition-object
		   (%expression->source-position-condition input-form.stx)
		   (%extract-macro-expansion-trace input-form.stx)))
      (define C2
	(if the-who
	    (condition (make-who-condition the-who) C1)
	  C1))
      (if continuable?
	  (raise-continuable C2)
	(raise C2)))

    #| end of module |# )

  (define (%extract-macro-expansion-trace stx)
    ;;Extract from the (wrapped or unwrapped) syntax object STX the sequence of macro
    ;;expansion traces  from the AE* field  of "stx" records and  return a compound
    ;;condition object representing them, as instances of "&macro-expansion-trace".
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
    ;;its components it makes sense to extract  the trace.  (Marco Maggi; Sat Apr 12,
    ;;2014)
    ;;
    (let loop ((X stx))
      (cond ((stx? X)
	     (apply condition
		    (make-macro-expansion-trace X)
		    (map loop (stx-annotated-expr* X))))
	    ((reader-annotation? X)
	     ;;Here we  only want to wrap  X into an  "stx" object, we do  not care
	     ;;about the context.  (Marco Maggi; Sat Apr 11, 2015)
	     (make-macro-expansion-trace (make-stx-or-syntactic-identifier X '() '() '())))
	    (else
	     (condition)))))

  (define (%expression->source-position-condition x)
    (expression-position x))

  #| end of module |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
