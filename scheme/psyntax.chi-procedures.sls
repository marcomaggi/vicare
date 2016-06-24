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


;;;; introduction
;;
;;The CHI functions are the ones visiting syntax objects and performing the expansion
;;process.  The main facilities are:
;;
;;CHI-INTERACTION-EXPR: fully expands a standalone  expression in a specified lexical
;;context, in an interaction environment; the result is a core language expression.
;;
;;CHI-BODY*:  partially expands  the  forms in  a  top-level or  internal  body in  a
;;specified lexical context; it performs the first of the two passes needed to expand
;;body forms.   The results of  this first pass are:  a list of  qualified right-hand
;;side objects  (QDEF) describing the body's  variable definitions; a list  of syntax
;;objects representing trailing expressions.
;;
;;CHI-QDEF: fully expands  a qualified right-hand side object in  a specified lexical
;;context.  The  result is a PSI  object representing the right-hand  side expression
;;evaluation; the code representing the handling  of the resulting value is generated
;;somewhere else.
;;
;;CHI-INTERACTION-QDEF:  fully  expands  a  qualified right-hand  side  object  in  a
;;specified  lexical context  in an  interaction environment.   The result  is a  PSI
;;object   representing  the   right-hand  side   expression  evaluation;   the  code
;;representing the handling of the resulting value is generated somewhere else.
;;
;;CHI-EXPR: fully expands an expression in a specified lexical context; the result is
;;a  PSI  object.   It  is  used   for:  trailing  expressions  in  bodies;  function
;;application's  operators  and  operands;  SET!  right-hand  side  expressions;  LET
;;right-hand side expressions; et cetera.
;;
;;
;;NOTE Beware of the difference between evaluating  forms with EVAL in the context of
;;an interaction  environment and  in the context  of a  non-interaction environment.
;;Examples:
;;
;;  (eval '(begin (define a 1) a) (environment '(rnrs)))
;;  error --> a definition was found where an expression was expected
;;
;;  (eval '(begin (define a 1) a) (interaction-environment))
;;  => 1
;;
;;  (eval '(define a 1) (environment '(rnrs)))
;;  error --> a definition was found where an expression was expected
;;
;;  (eval '(define a 1) (interaction-environment))
;;  => #<void>
;;
;;* When  the environment is  non-interaction: the input  expression must be  a valid
;;standalone expression, expandable with CHI-EXPR.
;;
;;*  When the  environment  is interaction:  the  input expression  must  be a  valid
;;definition   or  expression   at  the   top-level  of   a  body,   expandable  with
;;CHI-INTERACTION-EXPR which in turn calls CHI-BODY*.
;;


(library (psyntax.chi-procedures)
  (export
    psi.core-expr
    chi-interaction-expr
    chi-expr			chi-expr*
    chi-body*			chi-qdef*
    ;;;chi-application/psi-first-operand
    qdef-generate-loc		qdef.lex
    SPLICE-FIRST-ENVELOPE)
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer)
    (prefix (rnrs syntax-case) sys::)
    (psyntax.compat)
    (prefix (only (psyntax.config)
		  typed-language-enabled?
		  strict-r6rs-enabled?
		  warn-about-logic-constants
		  warn-about-not-returning-expressions
		  warn-about-compatible-operands-signature-in-procedure-application
		  warn-about-unused-lexical-variables
		  warn-about-overloaded-function-late-binding)
	    options::)
    (psyntax.builders)
    (psyntax.lexical-environment)
    (only (psyntax.import-spec-parser)
	  parse-import-spec*)
    (psyntax.library-collectors)
    (only (psyntax.special-transformers)
	  variable-transformer?		variable-transformer-procedure
	  synonym-transformer?		synonym-transformer-identifier
	  expand-time-value?		expand-time-value-object)
    (psyntax.non-core-macro-transformers)
    (psyntax.library-manager)
    (psyntax.internal))

;; module interfaces
(import PSYNTAX-SYNTAX-MATCH
  PSYNTAX-SYNTAX-UTILITIES
  PSYNTAX-TYPE-SYNTAX-OBJECTS
  PSYNTAX-TYPE-SIGNATURES
  PSYNTAX-LAMBDA-SIGNATURES
  PSYNTAX-SYNTACTIC-BINDINGS)


;;;; helpers

(include "psyntax.helpers.scm" #t)

(define-syntax stx-error
  (syntax-rules ()
    ((_ ?stx ?msg)
     (syntax-violation #f ?msg ?stx))
    ((_ ?stx)
     (syntax-violation #f "syntax error" ?stx))
    ))

;;; --------------------------------------------------------------------

(define* (proper-list->last-item ell)
  (syntax-match ell ()
    (()
     (assertion-violation __who__ "expected non-empty list" ell))
    ((?last)
     ?last)
    ((?car . ?cdr)
     (proper-list->last-item ?cdr))
    ))


;;;; core expressions struct

(define-record-type (<psi> make-psi psi?)
  (nongenerative vicare:expander:<psi>)
  (fields
    (immutable input-form		psi.input-form)
		;The syntax object that originated this struct instance.  In the case
		;of internal  body: it is a  list of syntax objects,  but this should
		;not be a problem.  It is kept here for debugging purposes: it can be
		;used  as  "form"  or  "subform"  argument  for  "&syntax"  condition
		;objects.

    (immutable core-expr		psi.core-expr)
		;Either:
		;
		;* A symbolic expression in the core language representing the result
		;of fully expanding a syntax object.
		;
		;* An  instance of  "splice-first-envelope".  This happens  only when
		;this   PSI  struct   is  the   return  value   of  the   core  macro
		;SPLICE-FIRST-EXPAND.

    (immutable retvals-signature	psi.retvals-signature)
		;An  instance  of "<type-signature>"  representing  the  type of  the
		;values returned by this expression.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (case-define* make-psi
	((stx core-expr)
	 (make-record stx core-expr (make-type-signature/fully-unspecified)))
	((stx core-expr {retvals-signature type-signature?})
	 (make-record stx core-expr retvals-signature)))
      make-psi))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-psi/single-false input-form.stx)
  (make-psi input-form.stx
    (build-data no-source #f)
    (make-type-signature/single-false)))

(define (make-psi/single-true input-form.stx)
  (make-psi input-form.stx
    (build-data no-source #t)
    (make-type-signature/single-true)))

(define (make-psi/single-null input-form.stx)
  (make-psi input-form.stx
    (build-data no-source '())
    (make-type-signature/single-null)))


;;;; chi procedures: syntax object type inspection

(define* (syntactic-form-type caller-who expr.stx lexenv)
  ;;Determine the  syntax type of a  syntactic form representing an  expression.  The
  ;;type of an expression is determined by two things:
  ;;
  ;;* The shape of the expression (identifier, pair, or datum).
  ;;
  ;;* The binding of the identifier or the type of car of the pair.
  ;;
  ;;The argument EXPR.STX must be a syntax object representing an expression.
  ;;
  ;;Return 3 values:
  ;;
  ;;* If the syntactic form is a macro application:
  ;;
  ;;   1. A symbol representing the syntax type.
  ;;
  ;;   2. The value of the syntactic binding associated to the macro identifier.
  ;;
  ;;   3. The syntactic identifier representing the macro keyword.
  ;;
  ;;* If the syntactic form is not a macro application:
  ;;
  ;;   1. A symbol representing the syntax type.
  ;;
  ;;   2. False.
  ;;
  ;;   3. False.
  ;;
  ;;Special cases of return values and exceptional conditions:
  ;;
  ;;* If EXPR.STX is  a standalone syntactic identifier and it  is unbound: the first
  ;;return value is the symbol "standalone-unbound-identifier".
  ;;
  ;;* If  EXPR.STX is  a pair  whose car is  an unbound  identifier: an  exception is
  ;;raised.
  ;;
  (syntax-match expr.stx ()
    (?id
     (identifier? ?id)
     (cond ((id->label ?id)
	    => (lambda (label)
		 (let* ((descr (label->syntactic-binding-descriptor label lexenv))
			(type  (syntactic-binding-descriptor.type descr)))
		   (case type
		     (( ;;
		       core-prim core-prim-typed lexical lexical-typed
		       global global-mutable global-typed global-typed-mutable
		       integrated-macro macro macro! global-macro global-macro! local-macro local-macro!
		       local-etv global-etv pattern-variable
		       $core-rtd $core-rcd $module $core-type-descriptor
		       displaced-lexical)
		      (values type descr ?id))
		     ((local-overloaded-function)
		      (values 'local-overloaded-function-reference  descr ?id))
		     ((global-overloaded-function)
		      (values 'global-overloaded-function-reference descr ?id))
		     (else
		      ;;This will cause an error to be raised later.
		      (values 'other #f #f))))))
	   (else
	    (values 'standalone-unbound-identifier #f #f))))

    ((?car . ?cdr)
     (identifier? ?car)
     ;;Here we know that EXPR.STX has the format:
     ;;
     ;;   (?car ?form ...)
     ;;
     (cond ((id->label ?car)
	    => (lambda (label)
		 (let* ((descr (label->syntactic-binding-descriptor label lexenv))
			(type  (syntactic-binding-descriptor.type descr)))
		   (case type
		     (( ;;
		       core-macro integrated-macro
		       global-macro global-macro! local-macro local-macro! macro
		       module local-etv global-etv
		       displaced-lexical)
		      (values type descr ?car))
		     ((core-object-type-name local-object-type-name global-object-type-name)
		      (let ((ots (syntactic-binding-descriptor/object-type-spec.ots descr)))
			(cond ((enumeration-type-spec? ots)
			       (values 'enumeration descr ?car))
			      (else
			       (values 'other #f #f)))))
		     ((local-overloaded-function)
		      (values 'local-overloaded-function-application descr ?car))
		     ((global-overloaded-function)
		      (values 'global-overloaded-function-application descr ?car))
		     (else
		      ;;This  case includes  TYPE being:  core-prim, core-prim-typed,
		      ;;lexical, lexical-typed, global, global-mutable, global-typed,
		      ;;global-typed-mutable,          $core-rtd,          $core-rcd,
		      ;;$core-type-descriptor.
		      (values 'call #f #f))))))
	   (else
	    (error-unbound-identifier caller-who ?car))))

    ((?car . ?cdr)
     ;;Here we know that EXPR.STX has the format:
     ;;
     ;;   (?non-id ?form ...)
     ;;
     ;;where ?NON-ID  can be anything but  not an identifier.  In  practice the valid
     ;;syntaxes for this case are:
     ;;
     ;;   ((?first-subform ?subform ...) ?form ...)
     ;;
     (values 'call #f #f))

    (_
     (let ((datum (syntax->datum expr.stx)))
       (if (self-evaluating? datum)
	   (values 'constant datum #f)
	 ;;This will cause an error to be raised later.
	 (values 'other #f #f))))
    ))


;;;; facilities for the implementation of SPLICE-FIRST-EXPAND

(module SPLICE-FIRST-ENVELOPE
  (make-splice-first-envelope
   splice-first-envelope?
   splice-first-envelope-form)

  (define-record-type splice-first-envelope
    (nongenerative vicare:expander:splice-first-envelope)
    (fields form))

  #| end of module |# )

;;Set to true whenever  we are expanding the first suborm  in a function application.
;;This is where the syntax SPLICE-FIRST-EXPAND must  be used; in every other place it
;;must be discarded.
;;
(define expanding-application-first-subform?
  (make-parameter #f))

(define-syntax while-expanding-application-first-subform
  ;;Evaluate a  body while the parameter  is true.  This  syntax is used in  a single
  ;;place: the function CHI-APPLICATION applied to a syntax object:
  ;;
  ;;   ((?nested-rator ?nested-rand ...) ?rand ...)
  ;;
  ;;when expanding the first form.  It allows forms like:
  ;;
  ;;   ((splice-first-expand (?nested-rator ?nested-rand ...)) ?rand ...)
  ;;
  ;;to be transformed into:
  ;;
  ;;   (?nested-rator ?nested-rand ... ?rand ...)
  ;;
  ;;and then expanded.
  ;;
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (parametrise ((expanding-application-first-subform? #t))
       ?body0 ?body ...))))

(define-syntax while-not-expanding-application-first-subform
  ;;Evaluate a body while the parameter is false.
  ;;
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (parametrise ((expanding-application-first-subform? #f))
       ?body0 ?body ...))))


;;;; facilities for fluid syntaxes

(define* (fluid-syntax-push-redefinition-on-lexenvs input-form.stx lexenv.run lexenv.expand
						    caller-who lhs.id rhs.stx)
  ;;Push on the LEXENVs the result of redefining the single syntactic binding for the
  ;;identifier LHS.ID, having  RHS.STX as right-hand side expression;  LHS.ID must be
  ;;already bound to  a fluid syntax.  Return two values:  the updated LEXENV.RUN and
  ;;LEXENV.EXPAND.  Raise an exception if something goes wrong.
  ;;
  (let* ((fluid-label  (fluid-syntax-lookup-fluid-label caller-who lhs.id lexenv.run))
	 (descriptor   (eval-macro-transformer (expand-macro-transformer rhs.stx lexenv.expand) lexenv.run))
	 (entry        (cons fluid-label descriptor)))
    (values (cons entry lexenv.run)
	    (cons entry lexenv.expand))))

(define* (fluid-syntax-lookup-fluid-label caller-who lhs.id lexenv)
  ;;Search the lexical environment for the syntactic binding capturing the identifier
  ;;LHS.ID  and retrieve  its label;  if  such label  is present  and the  associated
  ;;syntactic binding  descriptor from LEXENV is  of type "fluid syntax":  return the
  ;;associated  fluid label  that can  be used  to rebind  the identifier.   Raise an
  ;;exception if something goes wrong.
  ;;
  (case-identifier-syntactic-binding-descriptor/no-indirection (__who__ lhs.id lexenv)
    (($fluid)
     (syntactic-binding-descriptor/fluid-syntax.fluid-label __descr__))
    (else
     (raise
      (condition (make-who-condition caller-who)
		 (make-message-condition "expected the keyword identifier of a fluid identifier")
		 (make-syntactic-identifier-condition lhs.id)
		 (make-syntactic-binding-descriptor-condition __descr__))))))

;; (define (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand
;; 					   caller-who who.id)
;;   ;;Redefine on  the LEXENVs the  syntactic binding  for the fluid  syntax "__who__",
;;   ;;expanding  to the  quoted  identifier  WHO.ID.  Return  two  values: the  updated
;;   ;;LEXENV.RUN and LEXENV.EXPAND.
;;   ;;
;;   (fluid-syntax-push-redefinition-on-lexenvs input-form.stx lexenv.run lexenv.expand
;; 					     caller-who (core-prim-id '__who__)
;; 					     (bless `(identifier-syntax (quote ,who.id)))))

(define (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand
					  caller-who who.id)
  ;;Redefine on  the LEXENVs the  syntactic binding  for the fluid  syntax "__who__",
  ;;expanding  to the  quoted  identifier  WHO.ID.  Return  two  values: the  updated
  ;;LEXENV.RUN and LEXENV.EXPAND.
  ;;
  ;;NOTE This  is a  specialised version of  the code that  redefines a  fluid syntax
  ;;binding,  since it  is used  often.  There  is no  need to  expand every  time an
  ;;IDENTIFIER-SYNTAX use in the syntax object:
  ;;
  ;;   (bless `(identifier-syntax (quote ,who.id)))
  ;;
  ;;we just create a transformer function here.
  ;;
  (let* ((lhs.id	(core-prim-id '__who__))
	 (fluid-label	(fluid-syntax-lookup-fluid-label caller-who lhs.id lexenv.run))
	 (who.sym	(identifier->symbol who.id))
	 (rhs.func	(let ((out.stx (bless `(quote ,who.sym))))
			  (lambda (stx)
			    (if (identifier? stx)
				out.stx
			      (syntax-violation '__who__ "invalid fluid syntax use" stx lhs.id)))))
	 (rhs.psi	(chi-expr (let ((stx.sym (gensym "stx")))
				    (bless
				     `(lambda/std (,stx.sym)
					(if (identifier? ,stx.sym)
					    (quote ,who.sym)
					  (syntax-violation '__who__ "invalid fluid syntax use" ,stx.sym (syntax ,lhs.id))))))
				  lexenv.expand lexenv.expand))
	 (rhs.core	(psi.core-expr rhs.psi))
	 (descriptor	(make-syntactic-binding-descriptor/local-macro/non-variable-transformer rhs.func rhs.core))
	 (fluid-entry	(cons fluid-label descriptor)))
    (values (cons fluid-entry lexenv.run)
	    (cons fluid-entry lexenv.expand))))


;;;; macro transformers helpers

(define (expand-macro-transformer rhs.stx lexenv.expand)
  ;;Given  a  syntax object  representing  the  right-hand  side  (RHS) of  a  syntax
  ;;definition   (DEFINE-SYNTAX,   LET-SYNTAX,  LETREC-SYNTAX,   DEFINE-FLUID-SYNTAX,
  ;;FLUID-LET-SYNTAX):  expand  it, invoking  libraries  as  needed, and  return  the
  ;;expanded language sexp representing the  expression.  Usually the return value of
  ;;this function is handed to EVAL-MACRO-TRANSFORMER.
  ;;
  ;;For:
  ;;
  ;;   (define-syntax ?lhs ?rhs)
  ;;
  ;;this function is called as:
  ;;
  ;;   (expand-macro-transformer #'?rhs lexenv.expand)
  ;;
  ;;For:
  ;;
  ;;   (let-syntax ((?lhs ?rhs)) ?body0 ?body ...)
  ;;
  ;;this function is called as:
  ;;
  ;;   (expand-macro-transformer #'?rhs lexenv.expand)
  ;;
  (let* ((rtc     (make-collector))
	 (rhs.psi (parametrise ((inv-collector rtc)
				(vis-collector (lambda (x) (void))))
		    (chi-expr rhs.stx lexenv.expand lexenv.expand))))
    ;;We invoke all the libraries needed to evaluate the right-hand side.
    (for-each
	(let ((register-visited-library (vis-collector)))
	  (lambda (lib)
	    ;;LIB is a record of type "<library>".  Here we invoke the library, which
	    ;;means  we evaluate  its run-time  code.  Then  we mark  the library  as
	    ;;visited.
	    (invoke-library lib)
	    (register-visited-library lib)))
      (rtc))
    (psi.core-expr rhs.psi)))

(define* (eval-macro-transformer rhs.core lexenv.run)
  ;;Given a  core language sexp  representing the right-hand  side (RHS) of  a syntax
  ;;definition   (DEFINE-SYNTAX,   LET-SYNTAX,  LETREC-SYNTAX,   DEFINE-FLUID-SYNTAX,
  ;;FLUID-LET-SYNTAX):  compile  it, evaluate  it,  then  return a  proper  syntactic
  ;;binding's descriptor for the resulting  object.  Usually this function is applied
  ;;to the return value of EXPAND-MACRO-TRANSFORMER.
  ;;
  (let ((rv (compiler::eval-core (expanded->core rhs.core))))
    (cond ((procedure? rv)
	   (make-syntactic-binding-descriptor/local-macro/non-variable-transformer rv rhs.core))
	  ((variable-transformer? rv)
	   (make-syntactic-binding-descriptor/local-macro/variable-transformer (variable-transformer-procedure rv) rhs.core))
	  ((object-type-spec? rv)
	   (make-syntactic-binding-descriptor/object-type-name rv rhs.core))
	  ((expand-time-value? rv)
	   (make-syntactic-binding-descriptor/local-macro/expand-time-value (expand-time-value-object rv) rhs.core))
	  ((synonym-transformer? rv)
	   (make-syntactic-binding-descriptor/local-global-macro/synonym-syntax (synonym-transformer-identifier rv)))
	  (else
	   (raise
	    (condition
	     (make-assertion-violation)
	     (make-who-condition __who__)
	     (make-message-condition "invalid return value from syntax definition's right-hand side")
	     (make-syntax-definition-expanded-rhs-condition rhs.core)
	     (make-syntax-definition-expression-return-value-condition rv)))))))


;;;; chi procedures: macro calls

(module (chi-non-core-macro
	 chi-local-macro
	 chi-global-macro)

  (define* (chi-non-core-macro {procname symbol?} input-form.stx lexenv.run {rib false-or-rib?})
    ;;Expand an expression representing the use  of a non-core macro; the transformer
    ;;function is  integrated in the  expander.  Return a syntax  object representing
    ;;the macro output form.
    ;;
    ;;PROCNAME  is a  symbol representing  the  name of  the non-core  macro; we  can
    ;;retrieve   the  transformer   procedure   from  PROCNAME   with  the   function
    ;;NON-CORE-MACRO-TRANSFORMER.
    ;;
    ;;INPUT-FORM.STX is the syntax object representing the expression to be expanded.
    ;;
    ;;LEXENV.RUN is the run-time lexical environment  in which the expression must be
    ;;expanded.
    ;;
    ;;RIB is false or a struct of type "rib".
    ;;
    (do-macro-call ($symbol-value procname)
		   input-form.stx lexenv.run rib))

  (define* (chi-local-macro bind-val input-form.stx lexenv.run {rib false-or-rib?})
    ;;This function is used to expand macro uses for macros defined by the code being
    ;;expanded; these  are the lexical  environment entries with  types "local-macro"
    ;;and "local-macro!".  Return a syntax object representing the macro output form.
    ;;
    ;;BIND-VAL is  the value of the  local macro's syntactic binding  descriptor; the
    ;;format of the descriptors is:
    ;;
    ;;     (local-macro  . (?transformer . ?expanded-expr))
    ;;     (local-macro! . (?transformer . ?expanded-expr))
    ;;
    ;;and the argument BIND-VAL is:
    ;;
    ;;     (?transformer . ?expanded-expr)
    ;;
    ;;INPUT-FORM.STX is the syntax object representing the expression to be expanded.
    ;;
    ;;LEXENV.RUN is the run-time lexical environment  in which the expression must be
    ;;expanded.
    ;;
    ;;RIB is false or a struct of type "rib".
    ;;
    (do-macro-call (car bind-val) input-form.stx lexenv.run rib))

  (define* (chi-global-macro bind-val input-form.stx lexenv.run {rib false-or-rib?})
    ;;This  function is  used to  expand macro  uses for:
    ;;
    ;;* Macros imported from other libraries.
    ;;
    ;;* Macros  defined by expressions  previously evaluated in the  same interaction
    ;;  environment.
    ;;
    ;;these  are  the  lexical  environment entries  with  types  "global-macro"  and
    ;;"global-macro!".  Return a syntax object representing the macro output form.
    ;;
    ;;BIND-VAL is the  value of the global macro's syntactic  binding descriptor; the
    ;;format of the descriptors is:
    ;;
    ;;     (global-macro  . (?library . ?loc))
    ;;     (global-macro! . (?library . ?loc))
    ;;
    ;;and the argument BIND-VAL is:
    ;;
    ;;     (?library . ?loc)
    ;;
    ;;INPUT-FORM.STX is the syntax object representing the expression to be expanded.
    ;;
    ;;LEXENV.RUN is the run-time lexical environment  in which the expression must be
    ;;expanded.
    ;;
    ;;RIB is false or a struct of type "rib".
    ;;
    (let ((lib (car bind-val))
	  (loc (cdr bind-val)))
      ;;If this  global binding use  is the  first time a  binding from LIB  is used:
      ;;visit the library.
      (visit-library lib)
      (let ((transformer (let ((x (symbol-value loc)))
			   (cond ((procedure? x)
				  x)
				 ((variable-transformer? x)
				  (variable-transformer-procedure x))
				 (else
				  (assertion-violation/internal-error __who__
				    "invalid object in \"value\" slot of global macro's loc gensym" x))))))
	(do-macro-call transformer input-form.stx lexenv.run rib))))

;;; --------------------------------------------------------------------

  (define* (do-macro-call transformer input-form.stx lexenv.run {rib false-or-rib?})
    ;;Apply the transformer  to the syntax object representing the  macro input form.
    ;;Return a syntax object representing the output form.
    ;;
    ;;The gist of the macro call is:
    ;;
    ;;   (add-new-mark rib
    ;;                 (transformer (add-anti-mark input-form.stx))
    ;;                 input-form.stx)
    ;;
    ;;in this function we do more than this to allows for: invalid transformer output
    ;;detection;   implementation  of   compile-time   values;  expander   inspection
    ;;facilities.
    ;;
    ;;NOTE ON TRACING WITH ANNOTATED EXPRESSIONS The function ADD-NEW-MARK takes care
    ;;of  pushing   INPUT-FORM.STX  on   the  stack   of  annotated   expressions  of
    ;;OUTPUT-FORM.STX, to trace the transformations  a form undergoes while expanding
    ;;it.
    ;;
    (import PSYNTAX-ADD-MARK)
    (let ((output-form.stx (transformer (add-anti-mark input-form.stx))))
      (let assert-no-raw-symbols-in-output-form ((x output-form.stx))
	(unless (stx? x)
	  (cond ((pair? x)
		 (assert-no-raw-symbols-in-output-form (car x))
		 (assert-no-raw-symbols-in-output-form (cdr x)))
		((vector? x)
		 (vector-for-each assert-no-raw-symbols-in-output-form x))
		((symbol? x)
		 (syntax-violation __who__
		   "raw symbol encountered in output of macro" input-form.stx x)))))
      ;;Put a new mark  on the output form.  For all  the identifiers already present
      ;;in the input form: this new mark  will be annihilated by the anti-mark we put
      ;;before.  For all the identifiers introduced by the transformer: this new mark
      ;;will stay there.
      (add-new-mark rib output-form.stx input-form.stx)))

  #| end of module |# )


;;;; chi procedures: expressions

(define* (chi-expr* expr*.stx lexenv.run lexenv.expand)
  ;;Recursive function.  Expand the expressions in EXPR*.STX left to right.  Return a
  ;;list of PSI structs.
  ;;
  (if (pair? expr*.stx)
      ;;ORDER MATTERS!!!  Make sure that first we do the car, then the rest.
      (let ((expr0.psi (while-not-expanding-application-first-subform
			(chi-expr (car expr*.stx) lexenv.run lexenv.expand))))
	(cons expr0.psi
	      (chi-expr* (cdr expr*.stx) lexenv.run lexenv.expand)))
    '()))

(module (chi-expr)
  (define-module-who chi-expr)

  (define* (chi-expr expr.stx lexenv.run lexenv.expand)
    ;;Expand a single expression form.  Return a PSI struct.
    ;;
    (%drop-splice-first-envelope-maybe
      (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	(receive (type descr kwd)
	    (syntactic-form-type __module_who__ expr.stx lexenv.run)
	  ;; (begin
	  ;;   (debug-print __who__ 'input type (syntax->datum expr.stx))
	  ;;   (receive-and-return (out)
	  ;; 	(%do-expansion expr.stx lexenv.run lexenv.expand type descr kwd)
	  ;;     #;(debug-print __who__ 'output out)
	  ;;     (debug-print __who__ 'output (syntax->datum expr.stx))))
	  (%do-expansion expr.stx lexenv.run lexenv.expand type descr kwd)))
      lexenv.run lexenv.expand))

  (define (%do-expansion expr.stx lexenv.run lexenv.expand type descr kwd)
    (case type
      ((core-macro)
       ;;Core macro use.  We expect the syntactic binding's descriptor to be:
       ;;
       ;;   (core-macro . ?core-macro-name)
       ;;
       ;;The core macro transformers are integrated in the expander; they perform the
       ;;full expansion of their input forms and return a PSI struct.
       ;;
       ;;Only when  debugging mode  is on:  we push the  input form  on the  stack of
       ;;annotated expressions of  the output form, to trace  the transformations the
       ;;form undergoes.  For  non-core macros: the function  ADD-NEW-MARK pushes the
       ;;input-form syntax object on the stack of the output-form syntax object.
       (let* ((transformer	($symbol-value (syntactic-binding-descriptor.value descr)))
	      (input-form.stx	(if (options::debug-mode-enabled?)
				    (stx-push-annotated-expr expr.stx expr.stx)
				  expr.stx)))
	 (transformer input-form.stx lexenv.run lexenv.expand)))

      ((global)
       ;;Reference to  global imported  lexical variable; this  means EXPR.STX  is an
       ;;identifier.  We expect the syntactic binding's descriptor to be:
       ;;
       ;;   (global . (#<library> . ?loc))
       ;;
       (let* ((descr.value	(syntactic-binding-descriptor.value descr))
	      (lib		(car descr.value))
	      (loc		(cdr descr.value)))
	 ((inv-collector) lib)
	 (make-psi expr.stx
	   (build-global-reference no-source loc)
	   (make-type-signature/single-top))))

      ((global-typed)
       ;;Reference to global imported typed  lexical variable; this means EXPR.STX is
       ;;an identifier.  We expect the syntactic binding's descriptor to be:
       ;;
       ;;   (global-typed . (#<library> . ?loc))
       ;;
       ;;We visit  the library so that  the ?LOC actually references  the instance of
       ;;"<global-typed-variable-spec>".
       (let* ((descr.value	(syntactic-binding-descriptor.value descr))
	      (lib		(car descr.value))
	      (globvar.type-loc	(cdr descr.value)))
	 ((inv-collector) lib)
	 (if (symbol-bound? globvar.type-loc)
	     (let ((gts (symbol-value globvar.type-loc)))
	       (if (global-typed-variable-spec? gts)
		   (let ((globvar.value-ots	(typed-variable-spec.ots		 gts))
			 (globvar.value-loc	(global-typed-variable-spec.variable-loc gts)))
		     (make-psi expr.stx
		       (build-global-reference no-source globvar.value-loc)
		       (make-type-signature/single-value globvar.value-ots)))
		 (assertion-violation __module_who__
		   "invalid object in loc gensym's \"value\" slot of \"global-typed\" syntactic binding's descriptor"
		   expr.stx descr)))
	   (assertion-violation __module_who__
	     "unbound loc gensym of \"global-typed\" syntactic binding's descriptor"
	     expr.stx descr))))

      ((core-prim-typed)
       ;;Core  primitive  with  type  signatures  specification;  it  is  a  built-in
       ;;procedure.  We expect the syntactic binding's descriptor DESCR to be:
       ;;
       ;;   (core-prim-typed . (#<core-prim-type-spec> . ?hard-coded-sexp))
       ;;
       (let* ((cpts		(syntactic-binding-descriptor/core-prim-typed.core-prim-type-spec descr))
	      (prim.name	(core-prim-type-spec.name cpts))
	      (prim.ots		(typed-variable-spec.ots cpts)))
	 (make-psi expr.stx
	   (build-primref no-source prim.name)
	   (make-type-signature/single-value prim.ots))))

      ((core-prim)
       ;;Core  primitive; it  is  either a  built-in procedure  (like  DISPLAY) or  a
       ;;constant (like the R6RS record-type descriptor for "&condition").  We expect
       ;;the syntactic binding's descriptor DESCR to be:
       ;;
       ;;   (core-prim . ?prim-name)
       ;;
       (let ((name (syntactic-binding-descriptor/core-prim.public-name descr)))
	 (make-psi expr.stx
	   (build-primref no-source name)
	   (make-type-signature/single-top))))

      (($core-rtd)
       ;;Core  record-type  descriptor reference;  it  is  a built-in  constant  like
       ;;"&condition-rtd",  the R6RS  record-type  descriptor  for "&condition".   We
       ;;expect the syntactic binding's descriptor DESCR to be:
       ;;
       ;;   ($core-rtd . ?prim-name)
       ;;
       (let ((name (syntactic-binding-descriptor.value descr)))
	 (make-psi expr.stx
	   (build-primref no-source name)
	   (make-type-signature/single-value (core-prim-spec '<record-type-descriptor> lexenv.run)))))

      (($core-rcd)
       ;;Core record-constructor descriptor reference; it is a built-in constant like
       ;;"&condition-rcd", the  R6RS record-constructor descriptor  for "&condition".
       ;;We expect the syntactic binding's descriptor DESCR to be:
       ;;
       ;;   ($core-rcd . ?prim-name)
       ;;
       (let ((name (syntactic-binding-descriptor.value descr)))
	 (make-psi expr.stx
	   (build-primref no-source name)
	   (make-type-signature/single-value (core-prim-spec '<record-constructor-descriptor> lexenv.run)))))

      (($core-type-descriptor)
       ;;Core Scheme object-type descriptor reference; it is a built-in constant like
       ;;"<fixnum>-ctd",  the  run-time descriptor  for  "<fixnum>".   We expect  the
       ;;syntactic binding's descriptor DESCR to be:
       ;;
       ;;   ($core-type-descriptor . ?prim-name)
       ;;
       (let ((name (syntactic-binding-descriptor.value descr)))
	 (make-psi expr.stx
	   (build-primref no-source name)
	   (make-type-signature/single-value (core-prim-spec '<core-type-descriptor> lexenv.run)))))

      ((call)
       ;;A function call; this means EXPR.STX has one of the formats:
       ;;
       ;;   (?id ?form ...)
       ;;   ((?first-subform ?subform ...) ?form ...)
       ;;
       (chi-application expr.stx lexenv.run lexenv.expand))

      ((lexical)
       ;;Reference to  lexical variable; this  means EXPR.STX is an  identifier.  The
       ;;syntactic binding's descriptor DESCR has format:
       ;;
       ;;   (lexical . #<untyped-lexical-var>)
       ;;
       (let* ((descr.value	(syntactic-binding-descriptor.value descr))
	      (lex		(syntactic-binding-descriptor/lexical-var/value.lex-name descr.value)))
	 (syntactic-binding-descriptor/lexical-var/value.referenced? descr.value #t)
	 (make-psi expr.stx
	   (build-lexical-reference no-source lex)
	   (make-type-signature/single-top))))

      ((lexical-typed)
       ;;Reference to typed  lexical variable; this means EXPR.STX  is an identifier.
       ;;The syntactic binding's descriptor has format:
       ;;
       ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
       ;;
       (let* ((lts		(syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec descr))
	      (variable.lex	(lexical-typed-variable-spec.lex lts))
	      (variable.ots	(typed-variable-spec.ots         lts)))
	 (lexical-typed-variable-spec.referenced?-set! lts #t)
	 (make-psi expr.stx
	   (build-lexical-reference no-source variable.lex)
	   (make-type-signature/single-value variable.ots))))

      ((global-macro global-macro!)
       ;;Macro uses  of macros imported from  other libraries or defined  by previous
       ;;expressions in  the same  interaction environment.  The  syntactic binding's
       ;;descriptor DESCR has format:
       ;;
       ;;   (global-macro  . (#<library> . ?loc))
       ;;   (global-macro! . (#<library> . ?loc))
       ;;
       (let ((exp-e (while-not-expanding-application-first-subform
		     (chi-global-macro (syntactic-binding-descriptor.value descr)
				       expr.stx lexenv.run #f))))
	 (chi-expr exp-e lexenv.run lexenv.expand)))

      ((local-macro local-macro!)
       ;;Macro uses  of macros  defined in  the code  being expanded.   The syntactic
       ;;binding's descriptor DESCR has format:
       ;;
       ;;   (local-macro  . (?transformer . ?expanded-expr))
       ;;   (local-macro! . (?transformer . ?expanded-expr))
       ;;
       (let ((exp-e (while-not-expanding-application-first-subform
		     (chi-local-macro (syntactic-binding-descriptor.value descr)
				      expr.stx lexenv.run #f))))
	 (chi-expr exp-e lexenv.run lexenv.expand)))

      ((macro macro!)
       ;;Macro uses of  non-core macros; such macros are integrated  in the expander.
       ;;The syntactic binding's descriptor DESCR has format:
       ;;
       ;;   (macro  . ?macro-name)
       ;;   (macro! . ?macro-name)
       ;;
       (let ((exp-e (while-not-expanding-application-first-subform
		     (chi-non-core-macro (syntactic-binding-descriptor.value descr)
					 expr.stx lexenv.run #f))))
	 (chi-expr exp-e lexenv.run lexenv.expand)))

      ((constant)
       ;;Constant; it means EXPR.STX is a self-evaluating datum.
       (let ((datum descr))
	 (make-psi expr.stx
	   (build-data no-source datum)
	   (datum-type-signature datum lexenv.run))))

      ((integrated-macro)
       (case (syntactic-binding-descriptor.value descr)
	 ((set!)
	  ;;Macro use of SET!; it means EXPR.STX has the format:
	  ;;
	  ;;   (set! ?lhs ?rhs)
	  ;;
	  (chi-set! expr.stx lexenv.run lexenv.expand))

	 ((set!/initialise)
	  ;;Macro use of SET!/INITIALISE; it means EXPR.STX has the format:
	  ;;
	  ;;   (set!/initialise ?lhs ?rhs)
	  ;;
	  (chi-set!/initialise expr.stx lexenv.run lexenv.expand))

	 ((begin)
	  ;;R6RS BEGIN  core macro use.   First we  check with SYNTAX-MATCH  that the
	  ;;syntax is correct, then we build the core language expression.
	  ;;
	  (syntax-match expr.stx ()
	    ((_ ?body ?body* ...)
	     (let ((body*.psi (chi-expr* (cons ?body ?body*) lexenv.run lexenv.expand)))
	       (make-psi expr.stx
		 (build-sequence no-source
		   (map psi.core-expr body*.psi))
		 (psi.retvals-signature (proper-list->last-item body*.psi)))))
	    ))

	 ((stale-when)
	  ;;STALE-WHEN macro use.  STALE-WHEN acts like BEGIN, but in addition causes
	  ;;an expression to be registered in the current stale-when collector.  When
	  ;;such expression  evaluates to false:  the compiled library is  stale with
	  ;;respect to some source file.  See for example the INCLUDE syntax.
	  (syntax-match expr.stx ()
	    ((_ ?guard ?body ?body* ...)
	     (begin
	       (handle-stale-when ?guard lexenv.expand)
	       (let ((body*.psi (chi-expr* (cons ?body ?body*) lexenv.run lexenv.expand)))
		 (make-psi expr.stx
		   (build-sequence no-source
		     (map psi.core-expr body*.psi))
		   (psi.retvals-signature (proper-list->last-item body*.psi))))))
	    ))

	 ((let-syntax letrec-syntax)
	  ;;LET-SYNTAX or LETREC-SYNTAX core macro uses.
	  (syntax-match expr.stx ()
	    ((?key ((?xlhs* ?xrhs*) ...) ?xbody ?xbody* ...)
	     (unless (valid-bound-ids? ?xlhs*)
	       (syntax-violation __module_who__ "invalid identifiers" expr.stx))
	     (let* ((xlab* (map generate-label-gensym ?xlhs*))
		    (xrib  (make-rib/from-identifiers-and-labels ?xlhs* xlab*))
		    (xb*   (map (lambda (x)
				  (let ((in-form (if (eq? type 'let-syntax)
						     x
						   (push-lexical-contour xrib x))))
				    (eval-macro-transformer (expand-macro-transformer in-form lexenv.expand)
							    lexenv.run)))
			     ?xrhs*)))
	       (let ((body*.psi (chi-expr* (map (lambda (x)
						  (push-lexical-contour xrib x))
					     (cons ?xbody ?xbody*))
					   (append (map cons xlab* xb*) lexenv.run)
					   (append (map cons xlab* xb*) lexenv.expand))))
		 (make-psi expr.stx
		   (build-sequence no-source
		     (map psi.core-expr body*.psi))
		   (psi.retvals-signature (proper-list->last-item body*.psi))))))
	    ))

	 ((define/std define/typed define/checked define-syntax define-fluid-syntax define-alias module import library)
	  (stx-error expr.stx
		     (string-append
		      (case type
			((define/std define/typed define/checked)	"a definition")
			((define-syntax)				"a define-syntax")
			((define-fluid-syntax)				"a define-fluid-syntax")
			((define-alias)					"a define-alias")
			((module)					"a module definition")
			((library)					"a library definition")
			((import)					"an import declaration")
			((export)					"an export declaration")
			(else						"a non-expression"))
		      " was found where an expression was expected")))

	 (else
	  (assertion-violation __module_who__
	    "internal error, invalid integrated-macro descriptor" expr.stx descr))))

      ((local-overloaded-function-application)
       ;;Local overloaded  function application.  The syntactic  binding's descriptor
       ;;DESCR has format:
       ;;
       ;;   (local-overloaded-function . #<overloaded-function-spec>)
       ;;
       (let ((over.ofs (syntactic-binding-descriptor/local-overloaded-function.ofs descr)))
	 (chi-overloaded-function-application expr.stx lexenv.run lexenv.expand over.ofs)))

      ((global-overloaded-function-application)
       ;;Global overloaded function application.   The syntactic binding's descriptor
       ;;DESCR has format:
       ;;
       ;;   (global-overloaded-function . (#<library> . ?loc))
       ;;
       (let ((over.ofs (syntactic-binding-descriptor/global-overloaded-function.ofs descr)))
	 (chi-overloaded-function-application expr.stx lexenv.run lexenv.expand over.ofs)))

      ((local-overloaded-function-reference)
       ;;Local  overloaded-function reference.   The  syntactic binding's  descriptor
       ;;DESCR has format:
       ;;
       ;;   (local-overloaded-function . #<overloaded-function-spec>)
       ;;
       (let ((over.ofs (syntactic-binding-descriptor/local-overloaded-function.ofs descr)))
	 (chi-expr (overloaded-function-spec.late-binding-function-id over.ofs) lexenv.run lexenv.expand)))

      ((global-overloaded-function-reference)
       ;;Global  overloaded-function reference.   The syntactic  binding's descriptor
       ;;DESCR has format:
       ;;
       ;;   (global-overloaded-function . (#<library> . ?loc))
       ;;
       (let ((over.ofs (syntactic-binding-descriptor/global-overloaded-function.ofs descr)))
	 (chi-expr (overloaded-function-spec.late-binding-function-id over.ofs) lexenv.run lexenv.expand)))

      ((enumeration)
       ;;Syntax  use  of  a  previously  defined  enumeration  type.   The  syntactic
       ;;binding's descriptor has one of the formats:
       ;;
       ;;   (core-object-type-name   . (#<enumeration-type-spec> . ?symbolic-expr))
       ;;   (local-object-type-name  . (#<enumeration-type-spec> . ?expanded-expr))
       ;;   (global-object-type-name . (#<library> . ?loc))
       ;;
       ;;where ?LOC is  a loc gensym containing  in its VALUE slot a  reference to an
       ;;instance of "<enumeration-type-spec>".
       (syntax-match expr.stx ()
	 ((_ ?symbol)
	  (let ((sym (syntax->datum ?symbol)))
	    (if (symbol? sym)
		(let ((ots (syntactic-binding-descriptor/object-type-spec.ots descr)))
		  (if (enumeration-type-spec.member? ots sym)
		      (make-psi expr.stx
			(build-data no-source sym)
			(make-type-signature/single-value (make-enumeration-type-spec (list sym) (list (enumeration-id) ?symbol))))
		    (stx-error expr.stx "expected symbol in enumeration as argument to enumeration validator" sym)))
	      (stx-error expr.stx "expected symbol as argument to enumeration validator" sym))))
	 (_
	  (stx-error expr.stx "invalid enumeration validator form"))))

      ((displaced-lexical)
       (stx-error expr.stx "identifier out of context"))

      ((pattern-variable)
       (stx-error expr.stx "reference to pattern variable outside a syntax form"))

      ((global-mutable global-typed-mutable)
       ;;Imported variable in reference position,  whose binding is assigned at least
       ;;once  in  the  code  of  the  imported library;  it  means  EXPR.STX  is  an
       ;;identifier.
       (stx-error expr.stx "attempt to reference a variable that is assigned in an imported library"))

      ((standalone-unbound-identifier)
       (error-unbound-identifier __module_who__ expr.stx))

      (else
       (stx-error expr.stx "invalid expression"))))

;;; --------------------------------------------------------------------

  (define* (%drop-splice-first-envelope-maybe {expr.psi psi?} lexenv.run lexenv.expand)
    ;;If we are expanding the first  subform of an application: just return EXPR.PSI;
    ;;otherwise if EXPR.PSI  is a splice-first envelope: extract its  form, expand it
    ;;and return the result.
    ;;
    (import SPLICE-FIRST-ENVELOPE)
    (let ((expr (psi.core-expr expr.psi)))
      (if (splice-first-envelope? expr)
	  (if (expanding-application-first-subform?)
	      expr.psi
	    (%drop-splice-first-envelope-maybe (chi-expr (splice-first-envelope-form expr) lexenv.run lexenv.expand)
					       lexenv.run lexenv.expand))
	expr.psi)))

  (define-syntax stx-error
    (syntax-rules ()
      ((_ ?stx ?msg . ?args)
       (syntax-violation __module_who__ ?msg ?stx . ?args))
      ))

  #| end of module: CHI-EXPR |# )

;;; --------------------------------------------------------------------

(define* (chi-expr-for-core-expr expr.stx lexenv.run)
  ;;This   function    is   used    when   converting   expand-time    instances   of
  ;;"<object-type-spec>" to run-time instances of "<object-type-descr>".
  ;;
  (psi.core-expr (chi-expr expr.stx lexenv.run (make-empty-lexenv))))

(define* (chi-expr-for-type-annotation expr.stx lexenv.run)
  ;;This  function  is used  when  converting  type  annotation syntax  objects  into
  ;;instances  of   "<object-type-spec>".   For  example:  when   processing  TYPE-OF
  ;;annotations.
  ;;
  (let ((expr.sig (psi.retvals-signature (chi-expr expr.stx lexenv.run (make-empty-lexenv)))))
    (case-type-signature-full-structure expr.sig
      (<list>/<list-of-spec>
       (syntax-violation __who__
	 "the expression in the TYPE-OF annotation is typed as returning an unspecified number of values"
	 expr.stx))

      (<bottom>
       (syntax-violation __who__
	 "the expression in the TYPE-OF annotation is typed as not returning"
	 expr.stx))

      ((<void>)
       ;;The expression is marked as returning void.
       (<void>-ots))

      ((single-value)
       ;;The expression returns a single value.
       => (lambda (obj.ots) obj.ots))

      (else
       (syntax-violation __who__
	 "the expression in the TYPE-OF annotation is typed as returning zero, two or more values"
	 expr.stx)))))


;;;; chi procedures: expressions in the interaction environment

(module (chi-interaction-expr)
  ;;Expand an expression in the context of an interaction environment.  If successful
  ;;return two values: the  result of the expansion as output  expression in the core
  ;;language;  the LEXENV  updated  with  the top-level  definitions  from the  input
  ;;expression.
  ;;
  ;;Note about SHADOW/REDEFINE-BINDINGS?
  ;;------------------------------------
  ;;
  ;;We are about to expand syntactic forms in the context of an interaction top-level
  ;;environment.     When     calling    CHI-BODY*,     we    set     the    argument
  ;;SHADOW/REDEFINE-BINDINGS? to true because:
  ;;
  ;;* Syntactic  binding definitions  at the  top-level of this  body are  allowed to
  ;;shadow  imported  syntactic bindings  established  by  the top-level  interaction
  ;;environment.  That is, at the REPL:
  ;;
  ;;   vicare> (import (rnrs))
  ;;   vicare> (define display 123)
  ;;
  ;;is a valid definition;  a DEFINE use expanded at the  top-level of an interaction
  ;;environment  can  shadow  the  binding imported  from  "(rnrs)".   The  following
  ;;definition is also valid:
  ;;
  ;;   vicare> (define-syntax let (identifier-syntax 1))
  ;;
  ;;* Syntactic binding  definitions at the top-level of this  body can be redefined.
  ;;That is, at the REPL:
  ;;
  ;;   vicare> (define a 1)
  ;;   vicare> (define a 2)
  ;;
  ;;is valid; a  DEFINE use can redefine a binding.   The following redefinitions are
  ;;also valid:
  ;;
  ;;   vicare> (begin (define a 1) (define a 2))
  ;;
  ;;   vicare> (define-syntax b (identifier-syntax 1))
  ;;   vicare> (define-syntax b (identifier-syntax 2))
  ;;
  ;;   vicare> (define-fluid-syntax b (identifier-syntax 1))
  ;;   vicare> (define-fluid-syntax b (identifier-syntax 2))
  ;;
  (define-module-who chi-interaction-expr)

  (define (chi-interaction-expr expr.stx rib lexenv.all)
    (receive (trailing-init-form*.stx
	      lexenv.run^ lexenv.expand^
	      rev-qdef* module-trailing-form**.stx
	      kwd*.unused internal-export*.unused)
	(let ((rev-qdef*				'())
	      (trailing-module-expression**		'())
	      (defined-names*				'())
	      (export-spec*				'())
	      (mixed-definitions-and-expressions?	#t)
	      (shadow/redefine-bindings?		#t))
	  (chi-body* (list expr.stx) lexenv.all lexenv.all
		     rev-qdef* trailing-module-expression** defined-names* export-spec* rib
		     mixed-definitions-and-expressions? shadow/redefine-bindings?))
      (values (build-sequence no-source
		(let ((init*.stx (reverse-and-append-with-tail module-trailing-form**.stx trailing-init-form*.stx)))
		  (%expand-qdef*-then-init* (reverse rev-qdef*) init*.stx lexenv.run^ lexenv.expand^)))
	      lexenv.run^)))

  (define (%expand-qdef*-then-init* qdef* init*.stx lexenv.run lexenv.expand)
    ;;Recursive  function.   Return  a  list   of  core  language  expressions,  some
    ;;representing global assignments  and some discarding the  return values; notice
    ;;that we do not care if there are no trailing expressions (INIT*.STX is null).
    ;;
    ;;NOTE Remember that the QDEF* are  accumulated in reverse order in CHI-BODY* but
    ;;they are handed to this function already reordered!
    ;;
    (define-syntax-rule (%recurse ?qdef*)
      (%expand-qdef*-then-init* ?qdef* init*.stx lexenv.run lexenv.expand))
    (if (pair? qdef*)
	(cons (let ((rhs.core (psi.core-expr (chi-interaction-qdef (car qdef*) lexenv.run lexenv.expand))))
		(if (qdef-top-expr? (car qdef*))
		    ;;Notice how here we do *not*  use the lex gensym argument; for a
		    ;;top-level expression it is useless and inefficient to store the
		    ;;returned value.
		    rhs.core
		  (build-global-assignment no-source
		    (qdef.lex (car qdef*)) rhs.core)))
	      (%recurse (cdr qdef*)))
      (map (lambda (init.stx)
	     (psi.core-expr (chi-expr init.stx lexenv.run lexenv.expand)))
	init*.stx)))

  #| end of module: CHI-INTERACTION-EXPR |# )


;;;; chi procedures: SET! syntax

(module (chi-set! chi-set!/initialise)

  (define* (chi-set! input-form.stx lexenv.run lexenv.expand)
    (while-not-expanding-application-first-subform
     (syntax-match input-form.stx (method-call)
       ((_ (method-call ?field-name ?expr) ?new-value)
	(identifier? ?field-name)
	(chi-expr (bless
		   `(slot-set! ,?expr ,?field-name ,?new-value))
		  lexenv.run lexenv.expand))

       ((_ ?lhs ?rhs)
	(identifier? ?lhs)
	(%chi-set-identifier input-form.stx lexenv.run lexenv.expand
			     __who__ ?lhs ?rhs #f))

       (_
	(syntax-violation __who__ "invalid setter syntax" input-form.stx)))))

  (define* (chi-set!/initialise input-form.stx lexenv.run lexenv.expand)
    ;;This  is  meant only  for  internal  use.  When  the  LHS  is a  typed  lexical
    ;;identifier with type  "<void>": override its type  with the one of  the RHS, if
    ;;possible.
    ;;
    (while-not-expanding-application-first-subform
     (syntax-match input-form.stx (method-call)
       ((_ (method-call ?field-name ?expr) ?new-value)
	(identifier? ?field-name)
	(chi-expr (bless
		   `(slot-set! ,?expr ,?field-name ,?new-value))
		  lexenv.run lexenv.expand))

       ((_ ?lhs ?rhs)
	(identifier? ?lhs)
	(%chi-set-identifier input-form.stx lexenv.run lexenv.expand
			     __who__ ?lhs ?rhs #t))

       (_
	(syntax-violation __who__ "invalid setter syntax" input-form.stx)))))

  (define (%chi-set-identifier input-form.stx lexenv.run lexenv.expand
			       caller-who lhs.id rhs.stx initialise-and-override-void?)
    (define-values (type descr kwd)
      (syntactic-form-type caller-who lhs.id lexenv.run))
    (case type
      ((lexical)
       ;;A  lexical binding  used  as LHS  of  SET! is  mutable  and so  unexportable
       ;;according to R6RS.  Example:
       ;;
       ;;   (library (demo)
       ;;     (export var)		;error!!!
       ;;     (import (rnrs))
       ;;     (define var 1)
       ;;     (set! var 2))
       ;;
       ;;The syntactic binding's descriptor DESCR has format:
       ;;
       ;;   (lexical . #<untyped-lexical-var>)
       ;;
       (let ((descr.value (syntactic-binding-descriptor.value descr)))
	 (syntactic-binding-descriptor/lexical-var/value.referenced? descr.value #t)
	 (syntactic-binding-descriptor/lexical-var/value.assigned?   descr.value #t)
	 (let ((rhs.psi (chi-expr rhs.stx lexenv.run lexenv.expand)))
	   (make-psi input-form.stx
		     (build-lexical-assignment no-source
		       (syntactic-binding-descriptor/lexical-var/value.lex-name descr.value)
		       (psi.core-expr rhs.psi))
		     (make-type-signature/single-void)))))

      ((lexical-typed)
       ;;The syntactic binding's descriptor has format:
       ;;
       ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
       ;;
       (let* ((lts		(syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec descr))
	      (variable.lex	(lexical-typed-variable-spec.lex lts))
	      (lts.ots		(typed-variable-spec.ots lts))
	      (rhs.psi		(chi-expr (bless rhs.stx) lexenv.run lexenv.expand))
	      (rhs.ots		(%rhs-signature->rhs-object-type-spec caller-who rhs.stx rhs.psi)))
	 (define rhs.code
	   (if (and initialise-and-override-void? (<void>-ots? lts.ots))
	       (begin
		 ;;Override "<void>" with the type of the RHS.
		 (typed-variable-spec.ots-set! lts rhs.ots)
		 ;;A typed  lexical binding used as  LHS of SET!  is  assigned and so
		 ;;unexportable, but this is a special  case.  We use this variant of
		 ;;the SET!  syntax only to  initialise syntactic bindings (as in the
		 ;;current implementation  of DEFINE-VALUES, [Sat May  14, 2016]), so
		 ;;we do *not* mark this variable as assigned.
		 (psi.core-expr rhs.psi))
	     (begin
	       ;;A typed  lexical binding  used as  LHS of SET!   is assigned  and so
	       ;;unexportable.  We mark it as such.
	       (lexical-typed-variable-spec.referenced?-set! lts #t)
	       (lexical-typed-variable-spec.assigned?-set!   lts #t)
	       (%generate-rhs-core-expr input-form.stx lexenv.run lexenv.expand
					caller-who lts.ots rhs.ots lhs.id rhs.psi))))
	 (make-psi input-form.stx
	   (build-lexical-assignment no-source
	       variable.lex
	     rhs.code)
	   (make-type-signature/single-void))))

      ((core-prim core-prim-typed)
       (syntax-violation caller-who "cannot modify imported core primitive" input-form.stx lhs.id))

      ((global global-typed)
       (syntax-violation caller-who "attempt to assign a variable that is imported from a library" input-form.stx lhs.id))

      ((global-macro!)
       ;;The syntactic binding's descriptor DESCR has format:
       ;;
       ;;   (global-macro! . (#<library> . ?loc))
       ;;
       (chi-expr (chi-global-macro (syntactic-binding-descriptor.value descr)
				   input-form.stx lexenv.run #f)
		 lexenv.run lexenv.expand))

      ((local-macro!)
       ;;The syntactic binding's descriptor DESCR has format:
       ;;
       ;;   (local-macro! . (?transformer . ?expanded-expr))
       ;;
       (chi-expr (chi-local-macro (syntactic-binding-descriptor.value descr)
				  input-form.stx lexenv.run #f)
		 lexenv.run lexenv.expand))

      ((global-mutable global-typed-mutable)
       ;;Imported variable in reference position,  whose binding is assigned at least
       ;;once in the code of the imported library.
       ;;
       ;;Let's consider this library:
       ;;
       ;;   (library (demo)
       ;;     (export macro)
       ;;     (import (vicare))
       ;;     (define-syntax macro
       ;;       (syntax-rules ()
       ;;         ((_)
       ;;          (set! var 5))))
       ;;     (define var 123)
       ;;     (set! var 8))
       ;;
       ;;in which  VAR is  assigned and so  not exportable according  to R6RS;  if we
       ;;import the library in a program and use MACRO:
       ;;
       ;;   (import (vicare) (demo))
       ;;   (macro)
       ;;
       ;;we  are attempting  to mutate  an  unexportable binding  in another  lexical
       ;;context.  This is forbidden by R6RS.
       (syntax-violation caller-who
	 "attempt to assign a variable that is imported from a library"
	 input-form.stx lhs.id))

      ((standalone-unbound-identifier)
       ;;The identifier  LHS.ID is unbound: raise  an exception.
       ;;
       ;;NOTE In  the original  Ikarus' code,  and for years  in Vicare's  code: SET!
       ;;could create  a new syntactic binding  when LHS.ID was unbound  and the SET!
       ;;syntactic form was expanded at  the top-level of an interaction environment.
       ;;I nuked this feature.  (Marco Maggi; Fri Apr 24, 2015)
       ;;
       (error-unbound-identifier caller-who lhs.id))

      ((displaced-lexical)
       (syntax-violation caller-who
	 "identifier out of context in assignment syntax" input-form.stx lhs.id))

      (else
       (syntax-violation caller-who
	 "invalid left-hand side in assignment syntax" input-form.stx lhs.id))))

;;; --------------------------------------------------------------------

  (define (%generate-rhs-core-expr input-form.stx lexenv.run lexenv.expand
				   caller-who lts.ots rhs.ots lhs.id rhs.psi)
    (cond ((object-type-spec.matching-super-and-sub? lts.ots rhs.ots)
	   (psi.core-expr rhs.psi))
	  ((object-type-spec.compatible-super-and-sub? lts.ots rhs.ots)
	   (let* ((validator.stx (object-type-spec.single-value-validator-lambda-stx lts.ots #t))
		  (validator.psi (chi-expr validator.stx lexenv.run lexenv.expand)))
	     (build-application no-source
		 (psi.core-expr validator.psi)
	       (list (psi.core-expr rhs.psi)			       ;value
		     (build-data no-source 1)			       ;value-index
		     (build-data no-source (syntax->datum lhs.id)))))) ;caller-who
	  (else
	   (raise
	    (condition (make-expand-time-type-signature-violation)
		       (make-who-condition caller-who)
		       (make-message-condition
			"expression used as right-hand side for SET! has type not matching the variable type")
		       (make-syntax-violation input-form.stx (psi.input-form rhs.psi))
		       (make-expected-type-signature-condition (make-type-signature/single-value lts.ots))
		       (make-returned-type-signature-condition (psi.retvals-signature rhs.psi)))))))

;;; --------------------------------------------------------------------

  (module (%rhs-signature->rhs-object-type-spec)

    (define (%rhs-signature->rhs-object-type-spec caller-who input-form.stx rhs.psi)
      (define (common message)
	(condition
	  (make-who-condition caller-who)
	  (make-message-condition message)
	  (make-syntax-violation input-form.stx (psi.input-form rhs.psi))
	  (make-type-signature-condition (psi.retvals-signature rhs.psi))))
      (case-type-signature-full-structure (psi.retvals-signature rhs.psi)
	(<bottom>
	 ;;The expression is marked as not-returning.
	 (when (options::warn-about-not-returning-expressions)
	   (%handle-error common "expression used as right-hand side in SET! is typed as not returning")))

	((<void>)
	 ;;The expression is marked as returning void.
	 (%handle-error common "expression used as right-hand side in SET! is typed as returning void"))

	((single-value)
	 ;;The expression returns a single value.  Good this OTS will become the type
	 ;;of the syntactic binding.
	 => (lambda (rhs.ots) rhs.ots))

	(<list-of-spec>
	 ;;The RHS expression returns an unspecified number of values, of known type.
	 ;;RHS.OTS holds a "<list-of-type-spec>" OTS.
	 => (lambda (rhs.ots)
	      ;;We delegate  to the  run-time code  the validation  of the  number of
	      ;;returned values.
	      (list-of-type-spec.item-ots rhs.ots)))

	(<list>
	 ;;The RHS expression returns an unspecified number of values, of unspecified
	 ;;type; RHS.OTS holds a "<list>" OTS.
	 (<top>-ots))

	(else
	 ;;The expression returns zero, two or more values.
	 (%handle-error common "expression used as right-hand side in SET! is typed as returning zero, two or more values"))))

    (define (%handle-error common message)
      (case-expander-language
	((typed)
	 (raise			(condition (make-expand-time-type-signature-violation)	(common message))))
	((default)
	 (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common message)))
	 (<top>-ots))
	((strict-r6rs)
	 (<top>-ots))))

    #| end of module: %RHS-SIGNATURE->RHS-OBJECT-TYPE-SPEC |# )

  #| end of module: CHI-SET |# )


;;;; chi procedures: internal body

(define* (chi-internal-body body-form*.stx lexenv.run lexenv.expand)
  ;;This function is used to expand the internal bodies:
  ;;
  ;;*  The  LET-like  syntaxes  are  converted to  LETREC*  syntaxes:  this  function
  ;;processes the internal bodies of LETREC*.
  ;;
  ;;*  The  LAMBDA syntaxes  are  processed  as  CASE-LAMBDA clauses:  this  function
  ;;processes the internal body of CASE-LAMBDA clauses.
  ;;
  ;;Return  a   PSI  struct  containing  an   expanded-language  symbolic  expression
  ;;representing the expanded body.
  ;;
  ;;About internal bodies
  ;;---------------------
  ;;
  ;;An internal body must satisfy the following constraints:
  ;;
  ;;* There must be at least one trailing expression, otherwise an error is raised.
  ;;
  ;;* Mixed definitions and expressions are forbidden.  All the definitions must come
  ;;first and the expressions last.
  ;;
  ;;* It is impossible to export an internal binding from the enclosing library.  The
  ;;EXPORT syntaxes present in the internal body are discarded.
  ;;
  ;;An internal body having internal definitions:
  ;;
  ;;   (define ?lhs ?rhs)
  ;;   ...
  ;;   ?trailing-expr
  ;;   ...
  ;;
  ;;is equivalent to:
  ;;
  ;;   (letrec* ((?lhs ?rhs) ...)
  ;;     ?trailing-expr ...)
  ;;
  ;;so we create  a rib to describe  the lexical contour of the  implicit LETREC* and
  ;;push it on the BODY-FORM*.STX.
  ;;
  ;;NOTE  We are  about to  expand syntactic  forms in  an internal  body defining  a
  ;;lexical contour; it  does not matter if the top-level  environment is interaction
  ;;or not.   When calling CHI-BODY*,  we set the  argument SHADOW/REDEFINE-BINDINGS?
  ;;to false because syntactic binding definitions  in this body cannot be redefined.
  ;;That is:
  ;;
  ;;   (internal-body
  ;;     (define a 1)
  ;;     (define a 2)
  ;;     a)
  ;;
  ;;is  a syntax  violation because  the use  of DEFINE  cannot redefine  the
  ;;binding for "a".
  (define input-form.stx
    (cons (core-prim-id 'internal-body) body-form*.stx))
  (let*-values
      (((rib) (make-rib/empty))
       ((trailing-expr-stx* lexenv.run lexenv.expand rev-qdef* trailing-mod-expr-stx** unused-kwd* unused-export-spec*)
	(let ((rev-qdef*			'())
	      (trailing-module-expression**	'())
	      (defined-keywords*		'())
	      (export-spec*			'())
	      (mix-definitions-and-expressions?	#f)
	      (shadow/redefine-bindings?	#f))
	  (chi-body* (map (lambda (x)
			    (push-lexical-contour rib x))
		       (syntax->list body-form*.stx))
		     lexenv.run lexenv.expand
		     rev-qdef* trailing-module-expression** defined-keywords* export-spec* rib
		     mix-definitions-and-expressions? shadow/redefine-bindings?)))
       ;;Upon  arriving  here:  RIB,   LEXENV.RUN  and  LEXENV.EXPAND  contain  the
       ;;syntactic bindings associated to the REV-QDEF*.
       ((init*.stx) (reverse-and-append-with-tail trailing-mod-expr-stx** trailing-expr-stx*)))
    (when (null? init*.stx)
      (syntax-violation __who__ "no expressions in body" input-form.stx))
    ;;We want order here!   First we expand the QDEFs, then we  expande the INITs; so
    ;;that the QDEF bindings are typed when the INITs are expanded.
    (let* ((qdef*		(reverse rev-qdef*))
	   (rhs*.psi		(chi-qdef* qdef* lexenv.run lexenv.expand))
	   (init*.psi		(chi-expr* init*.stx lexenv.run lexenv.expand))
	   (lhs*.lex		(map qdef.lex qdef*))
	   (rhs*.core		(map psi.core-expr rhs*.psi))
	   (init*.core		(map psi.core-expr init*.psi))
	   (last-init.psi	(proper-list->last-item init*.psi)))
      (begin0
	  (make-psi input-form.stx
	    (build-letrec* (syntax-annotation input-form.stx)
		lhs*.lex rhs*.core
	      (build-sequence no-source
		init*.core))
	    (psi.retvals-signature last-init.psi))
	;;Warn about unused  variables.  Let's check the  parameter before performing
	;;costly operations.
	(when (options::warn-about-unused-lexical-variables)
	  (let* ((lhs*.id	(map qdef.var-id qdef*))
		 (lhs*.lab	(map id->label lhs*.id)))
	    (%warn-about-unused-lexical-variables __who__ lhs*.id lhs*.lab lexenv.run)))))))


;;;; chi procedures: stale-when handling

(define (handle-stale-when guard-expr.stx lexenv.expand)
  (let* ((stc            (make-collector))
	 (guard-expr.psi (parametrise ((inv-collector stc))
			   (chi-expr guard-expr.stx lexenv.expand lexenv.expand))))
    (cond ((stale-when-collector)
	   => (lambda (c)
		(c (psi.core-expr guard-expr.psi) (stc)))))))


(define (%establish-typed-syntactic-bindings-lhs* all-lhs*.id lhs*.ots lexenv)
  ;;This function is meant to be used by syntaxes that create new syntactic bindings:
  ;;LAMBDA, NAMED-LAMBDA,  CASE-LAMBDA, NAMED-CASE-LAMBDA,  LET, LETREC,  LETREC*, et
  ;;cetera.  These syntaxes need to create both typed and untyped syntactic bindings.
  ;;
  ;;LHS*.ID must be a proper list  of syntactic identifiers representing the names of
  ;;the syntactic  bindings.  LHS*.OTS must  be a proper list  of #f or  instances of
  ;;"<object-type-spec>" representing the types of the bindings.
  ;;
  ;;Process the  LHS specifications  generating the typed  lexical vars  when needed.
  ;;Create a  new rib mapping  identifiers to labels.   Update the given  LEXENV with
  ;;entries mapping labels to syntactic binding descriptors.
  ;;
  ;;Return:  the  new  rib;  the  updated  LEXENV;  a  proper  list  of  lex  gensyms
  ;;representing the  core language  names of  the bindings; a  proper list  of label
  ;;gensyms.
  ;;
  ;;
  ;;Example, for the LET syntax:
  ;;
  ;;   (let (({A <fixnum>} 1)
  ;;         ({B <string>} "ciao"))
  ;;     ?body)
  ;;
  ;;this function must be called as:
  ;;
  ;;   (%establish-typed-syntactic-bindings-lhs*
  ;;      (list #'A #'B)
  ;;      (list (<fixnum>-ots) (<string>-ots))
  ;;      lexenv.run)
  ;;
  ;;Example, for the LAMBDA syntax:
  ;;
  ;;   (lambda ({A <fixnum>} {B <string>} . C)
  ;;     ?body)
  ;;
  ;;this function must be called as:
  ;;
  ;;   (%establish-typed-syntactic-bindings-lhs*
  ;;      (list #'C #'A #'B)
  ;;      (list (<list>-ots) (<fixnum>-ots) (<string>-ots))
  ;;      lexenv.run)
  ;;
  (let loop ((lhs*.id		all-lhs*.id)
	     (lhs*.ots		lhs*.ots)
	     (lhs*.rev-lex	'())
	     (lhs*.rev-lab	'())
	     (lexenv		lexenv))
    (if (pair? lhs*.id)
	(let* ((lhs.id  (car lhs*.id))
	       (lhs.ots (car lhs*.ots))
	       (lhs.lex (generate-lexical-gensym lhs.id))
	       (lhs.lab (generate-label-gensym   lhs.id))
	       (lexenv  (let ((lhs.des (if (and lhs.ots (not (<untyped>-ots? lhs.ots)))
					   ;;Typed binding.
					   (make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots lhs.lex)
					 ;;Untyped binding.
					 (make-syntactic-binding-descriptor/lexical-var lhs.lex))))
			  (push-entry-on-lexenv lhs.lab lhs.des lexenv))))
	  (loop (cdr lhs*.id) (cdr lhs*.ots)
		(cons lhs.lex lhs*.rev-lex)
		(cons lhs.lab lhs*.rev-lab)
		lexenv))
      ;;Beware of the order of the lex and lab gensyms!!!  It must match the order of
      ;;the identifiers in LHS*.ID.
      (let* ((lhs*.lex	(reverse lhs*.rev-lex))
	     (lhs*.lab	(reverse lhs*.rev-lab))
	     (rib		(make-rib/from-identifiers-and-labels all-lhs*.id lhs*.lab)))
	(values rib lexenv lhs*.lex lhs*.lab)))))


;;;; unused variable validation

(define (%warn-about-unused-lexical-variables/untyped caller-who lhs*.id lhs*.des)
  ;;Given a list of untyped lexical variables' identifiers LHS*.ID, and a list of the
  ;;correspondig  syntactic bindings'  descriptors LHS*.DES:  check if  the variables
  ;;where  referenced at  least once.   When the  variable was  never referenced  nor
  ;;assigned: raise a warning.
  ;;
  (when (options::warn-about-unused-lexical-variables)
    (for-each (lambda (lhs.des lhs.id)
		(unless (syntactic-binding-descriptor/lexical-var/value.referenced?
			 (syntactic-binding-descriptor.value lhs.des))
		  (raise-continuable
		   (condition (make-warning-unused-lexical-variable)
			      (make-who-condition caller-who)
			      (make-message-condition "unused lexical syntactic binding")
			      (make-syntactic-identifier-condition lhs.id)))))
      lhs*.des lhs*.id)))

(define* (%warn-about-unused-lexical-variables caller-who lhs*.id lhs*.lab lexenv)
  ;;Given  a  list  of  typed  lexical variables'  identifiers  LHS*.ID,  a  list  of
  ;;associated label gensyms LHS*.LAB, and the lexenv in which the syntactic bindings
  ;;are established: check if the variables where referenced at least once.  When the
  ;;variable was never referenced nor assigned: raise a warning.
  ;;
  (define (%warn lhs.id)
    (raise-continuable
     (condition (make-warning-unused-lexical-variable)
		(make-who-condition caller-who)
		(make-message-condition "unused lexical syntactic binding")
		(make-syntactic-identifier-condition lhs.id))))
  (when (options::warn-about-unused-lexical-variables)
    (for-each (lambda (lhs.id lhs.lab)
		(let ((lhs.des (label->syntactic-binding-descriptor lhs.lab lexenv)))
		  (case (syntactic-binding-descriptor.type lhs.des)
		    ((lexical-typed)
		     (let ((lhs.lts (syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec lhs.des)))
		       (unless (lexical-typed-variable-spec.referenced? lhs.lts)
			 (%warn lhs.id))))
		    ((lexical)
		     (unless (syntactic-binding-descriptor/lexical-var/value.referenced?
			      (syntactic-binding-descriptor.value lhs.des))
		       (%warn lhs.id)))
		    (else
		     (assertion-violation __who__
		       "internal error, cannot resolve label" lhs.id lhs.lab lhs.des)))))
      lhs*.id lhs*.lab)))


;;;; chi procedures: external modules

(include "psyntax.chi-procedures.qdef.scm"		#t)
(include "psyntax.chi-procedures.lambda.scm"		#t)
(include "psyntax.chi-procedures.application.scm"	#t)
(include "psyntax.chi-procedures.body.scm"		#t)
(include "psyntax.chi-procedures.core-macro-transformers.scm" #t)


;;;; done

(expression-expander-for-type-annotations chi-expr-for-type-annotation)
(expression-expander-for-core-expressions chi-expr-for-core-expr)

#| end of library |# )

;;; end of file
