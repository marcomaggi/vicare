;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (psyntax.errors-and-conditions)
  (export

    &syntax-definition-expanded-rhs-condition
    make-syntax-definition-expanded-rhs-condition
    syntax-definition-expanded-rhs-condition?
    condition-syntax-definition-expanded-rhs

    &syntax-definition-expression-return-value
    make-syntax-definition-expression-return-value-condition
    syntax-definition-expression-return-value-condition?
    condition-syntax-definition-expression-return-value

    &retvals-signature-condition
    make-retvals-signature-condition
    retvals-signature-condition?
    retvals-signature-condition-signature

    &macro-use-input-form-condition
    ;;%make-macro-use-input-form-condition
    macro-use-input-form-condition?
    condition-macro-use-input-form

    &macro-expansion-trace
    make-macro-expansion-trace macro-expansion-trace?
    macro-expansion-trace-form

    &expand-time-type-signature-violation
    make-expand-time-type-signature-violation
    expand-time-type-signature-violation?

    &expand-time-retvals-signature-violation
    ;;%make-expand-time-retvals-signature-violation
    expand-time-retvals-signature-violation?
    expand-time-retvals-signature-violation-expected-signature
    expand-time-retvals-signature-violation-returned-signature

    with-exception-handler/input-form
    assertion-error
    syntax-violation/internal-error
    assertion-violation/internal-error
    syntax-violation
    expand-time-retvals-signature-violation
    raise-unbound-error
    make-macro-use-input-form-condition
    raise-compound-condition-object

    ;; helpers
    expression-position
    stx-error)
  (import (except (rnrs)
		  syntax-violation)
    (only (vicare)
	  syntax-object?
	  syntax-object-expression
	  syntax-object-source-objects)
    (psyntax.compat))


;;;; helpers

(define-syntax stx-error
  ;;Convenience wrapper for raising syntax violations.
  ;;
  (syntax-rules (quote)
    ((_ ?expr-stx)
     (syntax-violation #f "syntax error" ?expr-stx))
    ((_ ?expr-stx ?msg)
     (syntax-violation #f ?msg ?expr-stx))
    ((_ ?expr-stx ?msg ?who)
     (syntax-violation ?who ?msg ?expr-stx))
    ))

(define (retvals-signature? x)
  ;;FIXME This  is defined  here to  nothing to avoid  sharing contexts  between this
  ;;library and the  ones that defines the  correct version.  It is to  be removed in
  ;;some future.  Maybe at  the next boot image rotation.  (Marco  Maggi; Sat Apr 11,
  ;;2015)
  ;;
  #t)

(define (expression-position x)
  (if (syntax-object? x)
      (let ((x (syntax-object-expression x)))
	(if (annotation? x)
	    (annotation-textual-position x)
	  (condition)))
    (condition)))


;;;; condition object types: descriptive objects

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

(define* (make-retvals-signature-condition {sig retvals-signature?})
  (%make-retvals-signature-condition sig))

;;This is used  to describe exceptions in which  the input form of a macro  use has a
;;role.  The value  in the FORM slot  must be a syntax object  representing the macro
;;use input form.
;;
;;See MAKE-MACRO-USE-INPUT-FORM-CONDITION for details.
(define-condition-type &macro-use-input-form-condition
    &condition
  %make-macro-use-input-form-condition
  macro-use-input-form-condition?
  (form condition-macro-use-input-form))

;;This is used to represent the succession  of transformations a macro use input form
;;undergoes while  expanded; there is  an instance of  this condition type  for every
;;transformation.
;;
;;See MAKE-MACRO-USE-INPUT-FORM-CONDITION for details.
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

(define* (make-expand-time-retvals-signature-violation {expected-signature retvals-signature?}
						       {returned-signature retvals-signature?})
  (%make-expand-time-retvals-signature-violation expected-signature returned-signature))


;;;; exception raising functions

(define-syntax with-exception-handler/input-form
  ;;This macro is typically used as follows:
  ;;
  ;;   (with-exception-handler/input-form
  ;;       input-form.stx
  ;;     (%eval-macro-transformer
  ;;        (%expand-macro-transformer input-form.stx lexenv.expand)
  ;;        lexenv.run))
  ;;
  (syntax-rules ()
    ((_ ?input-form . ?body)
     (with-exception-handler
	 (lambda (E)
	   (raise (condition E (make-macro-use-input-form-condition ?input-form))))
       (lambda () . ?body)))
    ))

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

(module (syntax-violation/internal-error assertion-violation/internal-error)

  (case-define* syntax-violation/internal-error
    ((who {msg string?} form)
     (syntax-violation who (string-append PREFIX msg) form #f))
    ((who {msg string?} form subform)
     (syntax-violation who (string-append PREFIX msg) form subform)))

  (case-define* assertion-violation/internal-error
    ((who {msg string?} . irritants)
     (apply assertion-violation who (string-append PREFIX msg) irritants))
    ((who {msg string?} . irritants)
     (apply assertion-violation who (string-append PREFIX msg) irritants)))

  (define-constant PREFIX
    "Vicare Scheme: internal error: ")

  #| end of module |# )

(module (raise-unbound-error
	 syntax-violation
	 expand-time-retvals-signature-violation
	 make-macro-use-input-form-condition
	 raise-compound-condition-object)

  (case-define syntax-violation
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
    ((who msg form)
     (syntax-violation who msg form #f))
    ((who msg form subform)
     (raise-compound-condition-object who msg form (make-syntax-violation form subform))))

  (define* (expand-time-retvals-signature-violation source-who form subform
						    {expected-retvals-signature retvals-signature?}
						    {returned-retvals-signature retvals-signature?})
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

  (define* (raise-compound-condition-object source-who {msg string?} input-form.stx condition-object)
    ;;Raise a compound condition object.
    ;;
    ;;SOURCE-WHO can be  a string, symbol or  false; it is used as  value for "&who".
    ;;When  false: INPUT-FORM.STX  is inspected  to  determine a  possible value  for
    ;;"&who".
    ;;
    ;;MSG must be a string; it is used as value for "&message".
    ;;
    ;;INPUT-FORM.STX must be a (wrapped  or unwrapped) syntax object representing the
    ;;subject of  the raised exception.   It is used for  both inferring a  value for
    ;;"&who" and retrieving source location informations.
    ;;
    ;;CONDITION-OBJECT is  an already  built condition  object that  is added  to the
    ;;raised compound.
    ;;
    (let ((source-who (cond ((or (string? source-who)
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
			     (assertion-violation __who__ "invalid who argument" source-who)))))
      (raise
       (condition (if source-who
		      (make-who-condition source-who)
		    (condition))
		  (make-message-condition msg)
		  condition-object
		  (%expression->source-position-condition input-form.stx)
		  (%extract-macro-expansion-trace input-form.stx)))))

  (define* (make-macro-use-input-form-condition stx)
    (condition
     (%make-macro-use-input-form-condition stx)
     (%extract-macro-expansion-trace stx)))

  (define (%extract-macro-expansion-trace stx)
    ;;Extraxt from the (wrapped or unwrapped) syntax object STX the sequence of macro
    ;;expansion traces  from the AE* field  of "<stx>" records and  return a compound
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
    ;;        form: #<syntax expr=(one) mark*=(#f "" top) ...>
    ;;        subform: #f
    ;;    4. &source-position:
    ;;        port-id: "../tests/test-demo.sps"
    ;;        byte: 516
    ;;        character: 514
    ;;        line: 31
    ;;        column: 8
    ;;    5. &macro-expansion-trace: #<syntax expr=(one) mark*=(#f "" top) ...>
    ;;    6. &macro-expansion-trace: #<syntax expr=(two) mark*=(top) ...>
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
    ;;          form: (#<syntax expr=one mark*=(#f "" top) ...>
    ;;                 #<syntax expr=display mark*=(#f top) ...>
    ;;                 . #<syntax expr=() mark*=(#f "" top)>)
    ;;          subform: #f
    ;;
    ;;and there is no trace.  This is  because the syntax object used as "form" value
    ;;in "&syntax" is  not wrapped, and SYNTAX-VIOLATION cannot decide  from which of
    ;;its components it makes sense to extract  the trace.  (Marco Maggi; Sat Apr 12,
    ;;2014)
    ;;
    (let loop ((X stx))
      (cond ((syntax-object? X)
	     (apply condition
		    (make-macro-expansion-trace X)
		    (map loop (syntax-object-source-objects X))))
	    ((annotation? X)
	     ;;FIXME Here we  only want to wrap  X into an "<stx>" object,  we do not
	     ;;care about the context.  (Marco Maggi; Sat Apr 11, 2015)
	     #;(make-macro-expansion-trace (make-<stx> X '() '() '()))
	     (make-macro-expansion-trace X))
	    (else
	     (condition)))))

  (define (%expression->source-position-condition x)
    (expression-position x))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
