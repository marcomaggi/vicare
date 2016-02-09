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
;;side objects  (QRHS) describing the body's  variable definitions; a list  of syntax
;;objects representing trailing expressions.
;;
;;CHI-QRHS: fully expands  a qualified right-hand side object in  a specified lexical
;;context.  The  result is a PSI  object representing the right-hand  side expression
;;evaluation; the code representing the handling  of the resulting value is generated
;;somewhere else.
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
    chi-body*			chi-qrhs*
    ;;;chi-application/psi-first-operand
    qrhs-generate-loc
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
    (psyntax.builders)
    (psyntax.lexical-environment)
    (only (psyntax.import-spec-parser)
	  parse-import-spec*)
    (psyntax.library-collectors)
    (only (psyntax.special-transformers)
	  variable-transformer?		variable-transformer-procedure
	  synonym-transformer?		synonym-transformer-identifier
	  expand-time-value?		expand-time-value-object)
    (only (psyntax.syntactic-binding-properties)
	  predicate-assertion-procedure-argument-validation
	  predicate-assertion-return-value-validation)
    (psyntax.non-core-macro-transformers)
    (psyntax.library-manager)
    (psyntax.internal))

;; module interfaces
(import PSYNTAX-SYNTAX-MATCH)
(import PSYNTAX-SYNTAX-UTILITIES)
(import PSYNTAX-TYPE-IDENTIFIERS-AND-SIGNATURES)


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

(module (<psi>
	 make-psi psi?
	 psi.input-form
	 psi.core-expr
	 psi.retvals-signature
	 psi-application-retvals-signature)

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
		;An instance of "<type-signature>".
		;
		;When  this  PSI is  a  callable  object:  we  expect this  field  to
		;represent a signature like:
		;
		;   (?tag)
		;
		;where ?TAG is "<procedure>" or a sub-type of it.
     #| end of FIELDS |# )

    (protocol
      (lambda (make-record)
	(case-define* make-psi
	  ((stx core-expr)
	   (make-record stx core-expr (make-type-signature/fully-untyped)))
	  ((stx core-expr {retvals-signature type-signature?})
	   (make-record stx core-expr retvals-signature)))
	make-psi))

    #| end of DEFINE-RECORD-TYPE |# )

  (define* ({psi-application-retvals-signature type-signature?} input-form.stx lexenv {rator.psi psi?})
    ;;We  assume  RATOR.PSI is  a  PSI  representing the  first  form  in a  callable
    ;;application:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;we need  to establish the retvals  signature of the application  and return it.
    ;;We can return a meaningful value if RATOR.PSI has a type which is a sub-type of
    ;;"<procedure>".
    ;;
    (syntax-match (type-signature-tags (psi.retvals-signature rator.psi)) (<predicate>)
      ((<predicate>)
       (make-type-signature/single-boolean))
      ((?type-id)
       (let ((ots (id->object-type-specification __who__ input-form.stx ?type-id lexenv)))
	 (if (closure-type-spec? ots)
	     (callable-signature.retvals (closure-type-spec.signature ots))
	   (make-type-signature/fully-untyped))))
      (_
       (make-type-signature/fully-untyped))))

  #| end of module |# )


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
		     ((core-prim core-prim-typed
				 lexical lexical-typed
				 global global-mutable global-typed global-typed-mutable
				 macro macro! global-macro global-macro! local-macro local-macro!
				 import export library $module pattern-variable
				 local-etv global-etv
				 displaced-lexical)
		      (values type descr ?id))
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
		     ((core-macro
		       define/standard define/typed
		       case-define/standard case-define/typed
		       define-syntax define-alias
		       define-fluid-syntax
		       let-syntax letrec-syntax begin-for-syntax
		       begin set! stale-when
		       local-etv global-etv
		       global-macro global-macro! local-macro local-macro! macro
		       import export library module
		       displaced-lexical)
		      (values type descr ?car))
		     (else
		      ;;This  case includes  TYPE being:  CORE-PRIM, CORE-PRIM-TYPED,
		      ;;LEXICAL, LEXICAL-TYPED, GLOBAL, GLOBAL-MUTABLE, GLOBAL-TYPED,
		      ;;GLOBAL-TYPED-MUTABLE.
		      (values 'call #f #f))))))
	   (else
	    (raise-unbound-error caller-who expr.stx ?car))))

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
  (let* ((fluid-label  (fluid-syntax-lookup-fluid-label caller-who input-form.stx lhs.id lexenv.run))
	 (descriptor   (eval-macro-transformer (expand-macro-transformer rhs.stx lexenv.expand) lexenv.run))
	 (entry        (cons fluid-label descriptor)))
    (values (cons entry lexenv.run)
	    (cons entry lexenv.expand))))

(define (fluid-syntax-lookup-fluid-label caller-who input-form.stx lhs.id lexenv)
  ;;Search the lexical environment for the syntactic binding capturing the identifier
  ;;LHS.ID  and retrieve  its label;  if  such label  is present  and the  associated
  ;;syntactic binding  descriptor from LEXENV is  of type "fluid syntax":  return the
  ;;associated  fluid label  that can  be used  to rebind  the identifier.   Raise an
  ;;exception if something goes wrong.
  ;;
  (case-identifier-syntactic-binding-descriptor/no-indirection (caller-who input-form.stx lhs.id lexenv)
    (($fluid)
     (fluid-syntax-binding-descriptor.fluid-label __descr__))
    (else
     (syntax-violation caller-who
       "expected the keyword identifier of a fluid identifier"
       input-form.stx lhs.id))))

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
	 (fluid-label	(fluid-syntax-lookup-fluid-label caller-who input-form.stx lhs.id lexenv.run))
	 (who.sym	(identifier->symbol who.id))
	 (rhs.func	(let ((out.stx (bless `(quote ,who.sym))))
			  (lambda (stx)
			    (if (identifier? stx)
				out.stx
			      (syntax-violation '__who__ "invalid fluid syntax use" stx lhs.id)))))
	 (rhs.psi	(chi-expr (bless
				   `(lambda/standard (stx)
				      (if (identifier? stx)
					  (quote ,who.sym)
					(syntax-violation '__who__ "invalid fluid syntax use" stx (syntax ,lhs.id)))))
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
	  ((record-type-spec? rv)
	   (make-syntactic-binding-descriptor/record-type-name rv rhs.core))
	  ((struct-type-spec? rv)
	   (make-syntactic-binding-descriptor/struct-type-name rv rhs.core))
	  ((closure-type-spec? rv)
	   (make-syntactic-binding-descriptor/closure-type-name rv rhs.core))
	  ((list-type-spec? rv)
	   (make-syntactic-binding-descriptor/list-sub-type-name rv rhs.core))
	  ((vector-type-spec? rv)
	   (make-syntactic-binding-descriptor/vector-sub-type-name rv rhs.core))
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
    (do-macro-call (non-core-macro-transformer procname)
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
    (import ADD-MARK)
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
       (let ((transformer (core-macro-transformer (syntactic-binding-descriptor.value descr))))
	 (transformer (if (options::debug-mode-enabled?)
			  ;;Here we  push the  input form on  the stack  of annotated
			  ;;expressions, to improve error  messages in case of syntax
			  ;;violations.  When expanding  non-core macros the function
			  ;;%DO-MACRO-CALL  takes  care of  doing  it,  but for  core
			  ;;macros we have to do it here.
			  ;;
			  ;;NOTE  Unfortunately, I  have measured  that wrapping  the
			  ;;input form into an additional "stx" record slows down the
			  ;;expansion in a significant  way; when rebuilding the full
			  ;;Vicare source  code, compiling the libraries  and running
			  ;;the test  suite the total  time can be 25%  greater.  For
			  ;;this reason  this step  is performed only  when debugging
			  ;;mode is enabled.  (Marco Maggi; Wed Apr 2, 2014)
			  (stx-push-annotated-expr expr.stx expr.stx)
			expr.stx)
		      lexenv.run lexenv.expand)))

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
		   (make-type-signature/single-untyped))))

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
	      (loc		(cdr descr.value)))
	 ((inv-collector) lib)
	 (if (symbol-bound? loc)
	     (let ((gts (symbol-value loc)))
	       (if (global-typed-variable-spec? gts)
		   (let ((type-id		(global-typed-variable-spec.type-id      gts))
			 (variable-loc	(global-typed-variable-spec.variable-loc gts)))
		     (make-psi expr.stx
			       (build-global-reference no-source variable-loc)
			       (make-type-signature/single-value type-id)))
		 (assertion-violation __module_who__
		   "invalid object in loc gensym's \"value\" slot of \"global-typed\" syntactic binding's descriptor"
		   expr.stx descr)))
	   (assertion-violation __module_who__
	     "unbound loc gensym of \"global-typed\" syntactic binding's descriptor"
	     expr.stx descr))))

      ((core-prim)
       ;;Core  primitive; it  is  either a  built-in procedure  (like  DISPLAY) or  a
       ;;constant  (like  then R6RS  record-type  descriptor  for "&condition").   We
       ;;expect the syntactic binding's descriptor DESCR to be:
       ;;
       ;;   (core-prim . ?prim-name)
       ;;
       (let ((name (syntactic-binding-descriptor.value descr)))
	 (make-psi expr.stx
		   (build-primref no-source name)
		   (make-type-signature/single-untyped))))

      ((core-prim-typed)
       ;;Core  primitive  with  type  signatures  specification;  it  is  a  built-in
       ;;procedure.  We expect the syntactic binding's descriptor DESCR to be:
       ;;
       ;;   (core-prim-typed . (#<core-prim-type-spec> . ?hard-coded-sexp))
       ;;
       (let* ((descr.value	(syntactic-binding-descriptor.value descr))
	      (name		(core-prim-typed-binding-descriptor.value.prim-name descr.value))
	      (type-id	(core-prim-typed-binding-descriptor.value.type-id   descr.value)))
	 (make-psi expr.stx
		   (build-primref no-source name)
		   (make-type-signature/single-value type-id))))

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
       ;;   (lexical . (?lex-gensym . ?assigned-bool))
       ;;
       (let ((lex (lexical-var-binding-descriptor-value.lex-name (syntactic-binding-descriptor.value descr))))
	 (make-psi expr.stx
		   (build-lexical-reference no-source lex)
		   (make-type-signature/single-untyped))))

      ((lexical-typed)
       ;;Reference to typed  lexical variable; this means EXPR.STX  is an identifier.
       ;;The syntactic binding's descriptor has format:
       ;;
       ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
       ;;
       (let* ((descr.value	(syntactic-binding-descriptor.value descr))
	      (lex		(syntactic-binding-descriptor/lexical-typed-var.value.lex     descr.value))
	      (type-id	(syntactic-binding-descriptor/lexical-typed-var.value.type-id descr.value)))
	 (make-psi expr.stx
		   (build-lexical-reference no-source lex)
		   (make-type-signature/single-value type-id))))

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
		   (datum-type-signature datum))))

      ((set!)
       ;;Macro use of SET!; it means EXPR.STX has the format:
       ;;
       ;;   (set! ?lhs ?rhs)
       ;;
       (let ()
	 (import CHI-SET)
	 (chi-set! expr.stx lexenv.run lexenv.expand)))

      ((begin)
       ;;R6RS BEGIN core macro use.  First we check with SYNTAX-MATCH that the syntax
       ;;is correct, then we build the core language expression.
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
       ;;STALE-WHEN macro use.  STALE-WHEN acts like BEGIN, but in addition causes an
       ;;expression to be registered in  the current stale-when collector.  When such
       ;;expression evaluates to false: the compiled library is stale with respect to
       ;;some source file.  See for example the INCLUDE syntax.
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

      ((displaced-lexical)
       (stx-error expr.stx "identifier out of context"))

      ((pattern-variable)
       (stx-error expr.stx "reference to pattern variable outside a syntax form"))

      ((define/standard define/typed define-syntax define-fluid-syntax define-alias module import library)
       (stx-error expr.stx
		  (string-append
		   (case type
		     ((define/standard define/typed)		"a definition")
		     ((define-syntax)				"a define-syntax")
		     ((define-fluid-syntax)			"a define-fluid-syntax")
		     ((define-alias)				"a define-alias")
		     ((module)					"a module definition")
		     ((library)					"a library definition")
		     ((import)					"an import declaration")
		     ((export)					"an export declaration")
		     (else					"a non-expression"))
		   " was found where an expression was expected")))

      ((global-mutable global-typed-mutable)
       ;;Imported variable in reference position,  whose binding is assigned at least
       ;;once  in  the  code  of  the  imported library;  it  means  EXPR.STX  is  an
       ;;identifier.
       (stx-error expr.stx "attempt to reference a variable that is assigned in an imported library"))

      ((standalone-unbound-identifier)
       (raise-unbound-error __module_who__ expr.stx expr.stx))

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
	      lhs*.lex qrhs*
	      module-init-form**.stx
	      kwd*.unused internal-export*.unused)
	(let ((mixed-definitions-and-expressions?	#t)
	      (shadow/redefine-bindings?		#t))
	  (chi-body* (list expr.stx) lexenv.all lexenv.all
		     '() '() '() '() '() rib
		     mixed-definitions-and-expressions? shadow/redefine-bindings?))
      (let ((expr*.core (%expand-qrhs*-then-init*
			 (reverse lhs*.lex) (reverse qrhs*)
			 (reverse-and-append-with-tail module-init-form**.stx trailing-init-form*.stx)
			 lexenv.run^ lexenv.expand^)))
	(let ((expr.core (cond ((null? expr*.core)
				(build-void))
			       ((null? (cdr expr*.core))
				(car expr*.core))
			       (else
				(build-sequence no-source expr*.core)))))
	  (values expr.core lexenv.run^)))))

  (define (%expand-qrhs*-then-init* lhs*.lex qrhs* init*.stx lexenv.run lexenv.expand)
    ;;Recursive  function.   Return  a  list   of  core  language  expressions,  some
    ;;representing global assignments  and some discarding the  return values; notice
    ;;that we do not care if there are no trailing expressions (INIT*.STX is null).
    ;;
    ;;There must be a LHS*.LEX for every QRHS*.
    ;;
    (if (pair? lhs*.lex)
	(cons (let ((rhs.core (psi.core-expr (chi-interaction-qrhs (car qrhs*) lexenv.run lexenv.expand))))
		(if (qualified-rhs/top-expr? (car qrhs*))
		    ;;Notice how  here we do  *not* use  the LHS.LEX argument;  for a
		    ;;top-level expression it is useless and inefficient to store the
		    ;;returned value.
		    rhs.core
		  (build-global-assignment no-source (car lhs*.lex) rhs.core)))
	      (%expand-qrhs*-then-init* (cdr lhs*.lex) (cdr qrhs*) init*.stx lexenv.run lexenv.expand))
      (map (lambda (init.stx)
	     (psi.core-expr (chi-expr init.stx lexenv.run lexenv.expand)))
	init*.stx)))

  #| end of module: CHI-INTERACTION-EXPR |# )


;;;; chi procedures: SET! syntax

(module CHI-SET
  (chi-set!)

  (define-module-who set!)

  (define (chi-set! input-form.stx lexenv.run lexenv.expand)
    (while-not-expanding-application-first-subform
     (syntax-match input-form.stx ()
       ((_ (?method-call ?field-name ?expr) ?new-value)
	(and (identifier? ?method-call)
	     (~free-identifier=? ?method-call (core-prim-id 'method-call))
	     (identifier? ?field-name))
	(chi-expr (bless
		   `(slot-set! ,?expr ,?field-name ?new-value))
		  lexenv.run lexenv.expand))

       ((_ ?lhs ?rhs)
	(identifier? ?lhs)
	(%chi-set-identifier input-form.stx lexenv.run lexenv.expand
			     ?lhs ?rhs))

       (_
	(syntax-violation __module_who__ "invalid setter syntax" input-form.stx)))))

  (define (%chi-set-identifier input-form.stx lexenv.run lexenv.expand lhs.id rhs.stx)
    (define-values (type descr kwd)
      (syntactic-form-type __module_who__ lhs.id lexenv.run))
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
       ;;   (lexical . (?lex-gensym . ?assigned-bool))
       ;;
       (let ((descr.value (syntactic-binding-descriptor.value descr)))
	 (lexical-var-binding-descriptor-value.assigned? descr.value #t)
	 (let ((rhs.psi (chi-expr rhs.stx lexenv.run lexenv.expand)))
	   (make-psi input-form.stx
		     (build-lexical-assignment no-source
		       (lexical-var-binding-descriptor-value.lex-name descr.value)
		       (psi.core-expr rhs.psi))
		     (make-type-signature/single-void)))))

      ((lexical-typed)
       ;;A typed lexical binding used as LHS of SET!  is mutable and so unexportable.
       ;;The syntactic binding's descriptor has format:
       ;;
       ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
       ;;
       (let* ((descr.value	(syntactic-binding-descriptor.value descr))
	      (lhs.lex		(syntactic-binding-descriptor/lexical-typed-var.value.lex     descr.value))
	      (lhs.tag		(syntactic-binding-descriptor/lexical-typed-var.value.type-id descr.value))
	      (rhs.psi		(chi-expr (bless
					   `(assert-signature-and-return (,lhs.tag) ,rhs.stx))
					  lexenv.run lexenv.expand)))
	 (syntactic-binding-descriptor/lexical-typed-var.value.assigned? descr.value #t)
	 (make-psi input-form.stx
		   (build-lexical-assignment no-source
		     lhs.lex
		     (psi.core-expr rhs.psi))
		   (make-type-signature/single-void))))

      ((core-prim core-prim-typed)
       (syntax-violation __module_who__ "cannot modify imported core primitive" input-form.stx lhs.id))

      ((global global-typed)
       (syntax-violation __module_who__ "attempt to assign a variable that is imported from a library" input-form.stx lhs.id))

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
       (syntax-violation __module_who__
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
       (raise-unbound-error __module_who__ input-form.stx lhs.id))

      ((displaced-lexical)
       (syntax-violation __module_who__
	 "identifier out of context in assignment syntax" input-form.stx lhs.id))

      (else
       (syntax-violation __module_who__
	 "invalid left-hand side in assignment syntax" input-form.stx lhs.id))))

  #| end of module: CHI-SET |# )


;;;; chi procedures: BEGIN-FOR-SYNTAX

(module CHI-BEGIN-FOR-SYNTAX
  (chi-begin-for-syntax)
  ;;This module  is used only by  the function CHI-BODY*, because  a BEGIN-FOR-SYNTAX
  ;;macro use can appear only in a body.
  ;;
  ;;The input form is a BEGIN-FOR-SYNTAX syntax use.  We expand the expressions using
  ;;LEXENV.EXPAND as  LEXENV for run-time, much  like what we do  when evaluating the
  ;;right-hand side of a DEFINE-SYNTAX, but handling the sequence of expressions as a
  ;;body; then we  build a special core language expression  with global assignments;
  ;;finally we evaluate the result.
  ;;
  ;;When calling CHI-BODY*: the argument SHADOW/REDEFINE-BINDINGS? is honoured.
  ;;
  ;;* If SHADOW/REDEFINE-BINDINGS? is set to false:
  ;;
  ;;** Syntactic binding definitions in the body of a BEGIN-FOR-SYNTAX use must *not*
  ;;shadow syntactic bindings established by the top-level environment.  For example:
  ;;
  ;;  (library (demo)
  ;;    (export)
  ;;    (import (vicare))
  ;;    (begin-for-syntax
  ;;      (define display 1)
  ;;      (debug-print display)))
  ;;
  ;;is  a syntax  violation  because the  use  of DEFINE  cannot  shadow the  binding
  ;;imported from "(vicare)".
  ;;
  ;;** Syntactic  bindings established  in the  body of  a BEGIN-FOR-SYNTAX  use must
  ;;*not* be redefined.  For example:
  ;;
  ;;   (import (vicare))
  ;;   (begin-for-syntax
  ;;     (define a 1)
  ;;     (define a 2)
  ;;     (debug-print a))
  ;;
  ;;is a syntax violation  because the use of DEFINE cannot  redefine the binding for
  ;;"a".
  ;;
  ;;* If the argument SHADOW/REDEFINE-BINDINGS? is set to true:
  ;;
  ;;**  Syntactic binding  definitions  in the  body of  a  BEGIN-FOR-SYNTAX use  are
  ;;allowed  to  shadow imported  syntactic  bindings  established by  the  top-level
  ;;interaction environment.  For example, at the REPL:
  ;;
  ;;  vicare> (begin-for-syntax
  ;;            (define display 1)
  ;;            (debug-print display))
  ;;
  ;;is fine, because DEFINE can shadow the binding imported from "(vicare)".
  ;;
  ;;** Syntactic  binding definitions in  the body of  a BEGIN-FOR-SYNTAX use  can be
  ;;redefined.  For example, at the REPL:
  ;;
  ;;  vicare> (begin-for-syntax
  ;;            (define a 1)
  ;;            (define a 2)
  ;;            (debug-print a))
  ;;
  ;;is fine, because DEFINE can redefine the binding for "a".
  ;;
  (define (chi-begin-for-syntax input-form.stx body-form*.stx lexenv.run lexenv.expand
				lex* qrhs* mod** kwd* export-spec* rib mix?
				shadow/redefine-bindings?)
    (receive (lhs*.lex init*.core rhs*.core lexenv.expand^)
	(%expand input-form.stx lexenv.expand rib shadow/redefine-bindings?)
      ;;Build an expanded  code expression and evaluate it.
      ;;
      ;;NOTE This variable is set to #f (which is invalid core code) when there is no
      ;;code after the expansion.  This can happen, for example, when doing:
      ;;
      ;;   (begin-for-syntax
      ;;     (define-syntax ?lhs ?rhs))
      ;;
      ;;because the expansion of a DEFINE-SYNTAX use is nothing.
      (define visit-code.core
	(let ((rhs*.out  (if (null? rhs*.core)
			     #f
			   (build-sequence no-source
			     (map (lambda (lhs.lex rhs.core)
				    (build-global-assignment no-source
				      lhs.lex rhs.core))
			       lhs*.lex rhs*.core))))
	      (init*.out (if (null? init*.core)
			     #f
			   (build-sequence no-source
			     init*.core))))
	  (cond ((and rhs*.out init*.out)
		 (build-sequence no-source
		   (list rhs*.out init*.out)))
		(rhs*.out)
		(init*.out)
		(else #f))))
      (when visit-code.core
	(compiler::eval-core (expanded->core visit-code.core)))
      ;;Done!  Push on the LEXENV an entry like:
      ;;
      ;;   (?unused-label . (begin-for-syntax . ?core-code))
      ;;
      ;;then go on with the next body forms.
      (let ((lexenv.run^ (if visit-code.core
			     (let ((entry (cons (gensym "begin-for-syntax-label")
						(cons 'begin-for-syntax visit-code.core))))
			       (cons entry lexenv.run))
			   lexenv.run)))
	(chi-body* (cdr body-form*.stx)
		   lexenv.run^ lexenv.expand^
		   lex* qrhs* mod** kwd* export-spec* rib
		   mix? shadow/redefine-bindings?))))

  (define (%expand input-form.stx lexenv.expand rib shadow/redefine-bindings?)
    (define rtc
      (make-collector))
    (parametrise ((inv-collector rtc)
		  (vis-collector (lambda (x) (void))))
      (receive (empty
		lexenv.expand^ lexenv.super^
		lex* qrhs* module-init** kwd* export-spec*)
	  ;;Expand  the sequence  of  forms  as a  top-level  body, accumulating  the
	  ;;definitions.
	  (let ((lexenv.super                     lexenv.expand)
		(lex*                             '())
		(qrhs*                            '())
		(mod**                            '())
		(kwd*                             '())
		(export-spec*                     '())
		(mix-definitions-and-expressions? #t))
	    (syntax-match input-form.stx ()
	      ((_ ?expr ?expr* ...)
	       (chi-body* (cons ?expr ?expr*)
			  lexenv.expand lexenv.super
			  lex* qrhs* mod** kwd* export-spec* rib
			  mix-definitions-and-expressions? shadow/redefine-bindings?))))
	;;There must be no trailing expressions because we allowed mixing definitions
	;;and expressions as in a top-level program.
	(assert (null? empty))
	;;Expand the definitions and the module trailing expressions.
	(let* ((lhs*.lex  (reverse lex*))
	       (rhs*.psi  (chi-qrhs* (reverse qrhs*)                    lexenv.expand^ lexenv.super^))
	       (init*.psi (chi-expr* (reverse-and-append module-init**) lexenv.expand^ lexenv.super^)))
	  ;;Now that  we have fully expanded  the forms: we invoke  all the libraries
	  ;;needed to evaluate them.
	  (for-each
	      (let ((register-visited-library (vis-collector)))
		(lambda (lib)
		  (invoke-library lib)
		  (register-visited-library lib)))
	    (rtc))
	  (values lhs*.lex
		  (map psi.core-expr init*.psi)
		  (map psi.core-expr rhs*.psi)
		  lexenv.expand^)))))

  #| end of module: BEGIN-FOR-SYNTAX |# )


;;;; chi procedures: internal body

(define* (chi-internal-body input-form.stx lexenv.run lexenv.expand
			    body-form*.stx)
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
  (let*-values
      (((rib) (make-rib/empty))
       ((trailing-expr-stx* lexenv.run lexenv.expand lex* qrhs* trailing-mod-expr-stx** unused-kwd* unused-export-spec*)
	(let ((lex*                              '())
	      (qrhs*                             '())
	      (mod**                             '())
	      (kwd*                              '())
	      (export-spec*                      '())
	      (mix-definitions-and-expressions?  #f)
	      (shadow/redefine-bindings?         #f))
	  (chi-body* (map (lambda (x)
			    (push-lexical-contour rib x))
		       (syntax->list body-form*.stx))
		     lexenv.run lexenv.expand
		     lex* qrhs* mod** kwd* export-spec* rib
		     mix-definitions-and-expressions? shadow/redefine-bindings?)))
       ;;Upon  arriving  here:  RIB,   LEXENV.RUN  and  LEXENV.EXPAND  contain  the
       ;;syntactic bindings associated to the QRHS*.
       ((init*.stx) (reverse-and-append-with-tail trailing-mod-expr-stx** trailing-expr-stx*)))
    (when (null? init*.stx)
      (syntax-violation __who__ "no expression in body" input-form.stx body-form*.stx))
    ;;We want order here!   First we expand the QRHSs, then we  expande the INITs; so
    ;;that the QRHS bindings are typed when the INITs are expanded.
    (let* ((rhs*.psi       (chi-qrhs* (reverse qrhs*) lexenv.run lexenv.expand))
	   (init*.psi      (chi-expr* init*.stx lexenv.run lexenv.expand))
	   (rhs*.core      (map psi.core-expr rhs*.psi))
	   (init*.core     (map psi.core-expr init*.psi))
	   (last-init.psi  (proper-list->last-item init*.psi)))
      (make-psi (or input-form.stx body-form*.stx)
		(build-letrec* (syntax-annotation input-form.stx)
		  (reverse lex*)
		  rhs*.core
		  (build-sequence no-source
		    init*.core))
		(psi.retvals-signature last-init.psi)))))


;;;; chi procedures: MODULE processing

(module (chi-internal-module
	 module-interface-exp-id*
	 module-interface-exp-lab-vec)

  (define-record-type module-interface
    (nongenerative vicare:expander:module-interface)
    (fields first-mark
		;The first mark in the lexical context of the MODULE form.
	    exp-id-vec
		;A vector of identifiers exported by the module.
	    exp-lab-vec
		;A vector  of gensyms  acting as  labels for  the identifiers  in the
		;field EXP-ID-VEC.
	    ))

  (define (chi-internal-module module-form-stx lexenv.run lexenv.expand lex* qrhs* mod** kwd*)
    ;;Expand  the syntax  object  MODULE-FORM-STX which  represents  a core  langauge
    ;;MODULE syntax use.
    ;;
    ;;LEXENV.RUN and  LEXENV.EXPAND must  be lists  representing the  current lexical
    ;;environment for run and expand times.
    ;;
    (receive (name export-id* internal-body-form*)
	(%parse-module module-form-stx)
      (let* ((module-rib               (make-rib/empty))
	     (internal-body-form*/rib  (map (lambda (form)
					      (push-lexical-contour module-rib form))
					 (syntax->list internal-body-form*))))
	(receive (leftover-body-expr* lexenv.run lexenv.expand lex* qrhs* mod** kwd* _export-spec*)
	    ;;In a module: we do not want the trailing expressions to be converted to
	    ;;dummy definitions; rather  we want them to be accumulated  in the MOD**
	    ;;argument, for later expansion and evaluation.  So we set MIX? to false.
	    (let ((empty-export-spec*	'())
		  (mix?			#f)
		  ;;In calling  CHI-BODY* we set the  argument REDEFINE-BINDINGS?  to
		  ;;false because definitions  at the top level of  the module's body
		  ;;cannot be redefined.  That is:
		  ;;
		  ;; (import (vicare))
		  ;; (module ()
		  ;;   (define a 1)
		  ;;   (define a 2))
		  ;;
		  ;;is  a  syntax  violation  because   the  binding  "a"  cannot  be
		  ;;redefined.
		  (redefine-bindings?	#f))
	      (chi-body* internal-body-form*/rib
			 lexenv.run lexenv.expand
			 lex* qrhs* mod** kwd* empty-export-spec*
			 module-rib mix? redefine-bindings?))
	  ;;The list  of exported  identifiers is  not only the  one from  the MODULE
	  ;;argument, but  also the  one from  all the EXPORT  forms in  the MODULE's
	  ;;body.
	  (let* ((all-export-id*  (vector-append export-id* (list->vector _export-spec*)))
		 (all-export-lab* (vector-map
				      (lambda (id)
					;;For every exported identifier there must be
					;;a label already in the rib.
					(or (id->label (make-syntactic-identifier (identifier->symbol id)
										  (stx-mark* id)
										  (list module-rib)
										  '()))
					    (stx-error id "cannot find module export")))
				    all-export-id*))
		 (mod**           (cons leftover-body-expr* mod**)))
	    (if (not name)
		;;The module has no name.  All the exported identifier will go in the
		;;enclosing lexical environment.
		(values lex* qrhs* all-export-id* all-export-lab* lexenv.run lexenv.expand mod** kwd*)
	      ;;The module has a name.  Only the name itself will go in the enclosing
	      ;;lexical environment.
	      (let* ((name-label (generate-label-gensym 'module))
		     (iface      (make-module-interface
				  (car (stx-mark* name))
				  (vector-map
				      (lambda (x)
					;;This   is  a   syntax  object   holding  an
					;;identifier.
					(let ((rib* '())
					      (ae*  '()))
					  (make-syntactic-identifier (stx-expr x) (stx-mark* x) rib* ae*)))
				    all-export-id*)
				  all-export-lab*))
		     (binding    (make-syntactic-binding-descriptor/local-global-macro/module-interface iface))
		     (entry      (cons name-label binding)))
		(values lex* qrhs*
			;;FIXME: module cannot export itself yet.  Abdulaziz Ghuloum.
			(vector name)
			(vector name-label)
			(cons entry lexenv.run)
			(cons entry lexenv.expand)
			mod** kwd*))))))))

  (define (%parse-module module-form-stx)
    ;;Parse  a syntax  object representing  a core  language MODULE  form.  Return  3
    ;;values:  false  or an  identifier  representing  the  module  name; a  list  of
    ;;identifiers selecting the  exported bindings from the first  MODULE argument; a
    ;;list of syntax objects representing the internal body forms.
    ;;
    (syntax-match module-form-stx ()
      ((_ (?export* ...) ?body* ...)
       (begin
	 (unless (for-all identifier? ?export*)
	   (stx-error module-form-stx "module exports must be identifiers"))
	 (values #f (list->vector ?export*) ?body*)))
      ((_ ?name (?export* ...) ?body* ...)
       (begin
	 (unless (identifier? ?name)
	   (stx-error module-form-stx "module name must be an identifier"))
	 (unless (for-all identifier? ?export*)
	   (stx-error module-form-stx "module exports must be identifiers"))
	 (values ?name (list->vector ?export*) ?body*)))
      ))

  (module (module-interface-exp-id*)

    (define (module-interface-exp-id* iface id-for-marks)
      (let ((diff   (%diff-marks (stx-mark* id-for-marks)
				 (module-interface-first-mark iface)))
	    (id-vec (module-interface-exp-id-vec iface)))
	(if (null? diff)
	    id-vec
	  (vector-map
	      (lambda (x)
		(let ((rib* '())
		      (ae*  '()))
		  (make-syntactic-identifier (stx-expr x)
					     (append diff (stx-mark* x))
					     rib* ae*)))
	    id-vec))))

    (define (%diff-marks mark* the-mark)
      ;;MARK* must be  a non-empty list of  marks; THE-MARK must be a  mark in MARK*.
      ;;Return a list of the elements of MARK* up to and not including THE-MARK.
      ;;
      (when (null? mark*)
	(error '%diff-marks "BUG: should not happen"))
      (let ((a (car mark*)))
	(if (eq? a the-mark)
	    '()
	  (cons a (%diff-marks (cdr mark*) the-mark)))))

    #| end of module: MODULE-INTERFACE-EXP-ID* |# )

  #| end of module |# )


;;;; chi procedures: stale-when handling

(define (handle-stale-when guard-expr.stx lexenv.expand)
  (let* ((stc            (make-collector))
	 (guard-expr.psi (parametrise ((inv-collector stc))
			   (chi-expr guard-expr.stx lexenv.expand lexenv.expand))))
    (cond ((stale-when-collector)
	   => (lambda (c)
		(c (psi.core-expr guard-expr.psi) (stc)))))))


(module (%process-typed-syntactic-bindings-lhs*)
  ;;This function is meant to be used by syntaxes that create new syntactic bindings:
  ;;LAMBDA/TYPED,  NAMED-LAMBDA/TYPED,   CASE-LAMBDA/TYPED,  NAMED-CASE-LAMBDA/TYPED,
  ;;LET,  LETREC, LETREC*.   These syntaxes  need to  create both  typed and  untyped
  ;;syntactic bindings.
  ;;
  ;;LHS*.ID must be a proper list  of syntactic identifiers representing the names of
  ;;the  syntactic bindings.   LHS*.TAG must  be  a proper  list of  #f or  syntactic
  ;;identifiers representing the types of the bindings.
  ;;
  ;;Process the  LHS specifications  generating the typed  lexical vars  when needed.
  ;;Create a  new rib mapping  identifiers to labels.   Update the given  LEXENV with
  ;;entries mapping labels to syntactic binding descriptors.
  ;;
  ;;Return:  the  new  rib;  the  updated  LEXENV;  a  proper  list  of  lex  gensyms
  ;;representing the core language names of the bindings.
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
  ;;   (%process-typed-syntactic-bindings-lhs*
  ;;      (list #'A #'B)
  ;;      (list #'<fixnum> #'<string>)
  ;;      lexenv.run)
  ;;
  ;;Example, for the LAMBDA syntax:
  ;;
  ;;   (lambda ({A <fixnum>} {B <string>} . C)
  ;;     ?body)
  ;;
  ;;this function must be called as:
  ;;
  ;;   (%process-typed-syntactic-bindings-lhs*
  ;;      (list #'C #'A #'B)
  ;;      (list #'<list> #'<fixnum> #'<string>)
  ;;      lexenv.run)
  ;;
  (define (%process-typed-syntactic-bindings-lhs* lhs*.id lhs*.tag lexenv)
    (receive (typed-var*.id typed-var*.tag typed-var*.lex untyped-var*.id untyped-var*.lex lhs*.lex)
	(%partition-typed-and-untyped-lhs* lhs*.id lhs*.tag)
      ;;Prepare the UNtyped lexical variables.
      (let* ((untyped-var*.lab	(map generate-label-gensym   untyped-var*.id))
	     (lexenv		(lexenv-add-lexical-var-bindings untyped-var*.lab untyped-var*.lex lexenv)))
	;;Prepare the typed lexical variables.
	(let* ((typed-var*.lab	  (map generate-label-gensym   typed-var*.id))
	       (typed-var*.descr  (map make-syntactic-binding-descriptor/lexical-typed-var/from-data
				    typed-var*.tag typed-var*.lex))
	       (lexenv            (fold-left (lambda (lexenv lab descr)
					       (push-entry-on-lexenv lab descr lexenv))
				    lexenv typed-var*.lab typed-var*.descr))
	       (rib               (make-rib/from-identifiers-and-labels (append typed-var*.id  untyped-var*.id)
									(append typed-var*.lab untyped-var*.lab))))
	  ;;Beware of  the order of  the lex gensyms in  the return values!   It must
	  ;;match the order of the identifiers in LHS*.ID.
	  (values rib lexenv lhs*.lex)))))

  (define (%partition-typed-and-untyped-lhs* lhs*.id lhs*.tag)
    ;;Partition the  syntactic bindings into typed  and untyped.  Those having  #f or
    ;;"<top>" or "<untyped>" as tag are untyped.
    ;;
    (let loop ((lhs*.id			lhs*.id)
	       (lhs*.tag		lhs*.tag)
	       (typed-var*.id		'())
	       (typed-var*.tag		'())
	       (typed-var*.lex		'())
	       (untyped-var*.id		'())
	       (untyped-var*.lex	'())
	       (lhs*.lex		'()))
      (if (pair? lhs*.id)
	  (let ((lhs.id  (car lhs*.id))
		(lhs.tag (car lhs*.tag)))
	    (if (and lhs.tag
		     (not (top-tag-id?     lhs.tag))
		     (not (untyped-tag-id? lhs.tag)))
		;;Add a typed lexical variable.
		(let ((lhs.lex (generate-lexical-gensym lhs.id)))
		  (loop (cdr lhs*.id)
			(cdr lhs*.tag)
			(cons lhs.id  typed-var*.id)
			(cons lhs.tag typed-var*.tag)
			(cons lhs.lex typed-var*.lex)
			untyped-var*.id
			untyped-var*.lex
			(cons lhs.lex lhs*.lex)))
	      ;;Add an UNtyped lexical variable.
	      (let ((lhs.lex (generate-lexical-gensym lhs.id)))
		(loop (cdr lhs*.id)
		      (cdr lhs*.tag)
		      typed-var*.id
		      typed-var*.tag
		      typed-var*.lex
		      (cons lhs.id  untyped-var*.id)
		      (cons lhs.lex untyped-var*.lex)
		      (cons lhs.lex lhs*.lex)))))
	(values (reverse typed-var*.id) (reverse typed-var*.tag) (reverse typed-var*.lex)
		(reverse untyped-var*.id) (reverse untyped-var*.lex)
		(reverse lhs*.lex)))))

  #| end of module |# )


;;;; chi procedures: external modules

(include "psyntax.chi-procedures.qrhs.scm"		#t)
(include "psyntax.chi-procedures.lambda.scm"		#t)
(include "psyntax.chi-procedures.application.scm"	#t)
(include "psyntax.chi-procedures.body.scm"		#t)
(include "psyntax.chi-procedures.core-macro-transformers.scm" #t)


;;;; done

#| end of library |# )

;;; end of file
;;Local Variables:
;;fill-column: 85
;;eval: (put '%drop-splice-first-envelope-maybe		'scheme-indent-function 0)
;;eval: (put 'assertion-violation/internal-error	'scheme-indent-function 1)
;;eval: (put 'with-who					'scheme-indent-function 1)
;;eval: (put 'expand-time-retvals-signature-violation	'scheme-indent-function 1)
;;eval: (put 'case-qrhs-category			'scheme-indent-function 1)
;;eval: (put 'sys::syntax-case				'scheme-indent-function 2)
;;End:
