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


;;;; expansion of internal bodies
;;
;;The recursive function CHI-BODY* expands the forms of a body.  Here we expand:
;;
;;* User-defined the macro definitions and uses.
;;
;;* Non-core macro uses.
;;
;;* Basic language  syntaxes: LIBRARY, MODULE, BEGIN, STALE-WHEN,  IMPORT, EXPORT, et
;;cetera.
;;
;;but expand  neither the core  macros nor the  right-hand sides of  lexical variable
;;definitions (DEFINE forms)  and trailing expressions: the  variable definitions are
;;accumulated in  the argument and  return value  REV-QDEF* for later  expansion; the
;;trailing   expressions  are   accumulated  in   the  argument   and  return   value
;;BODY-FORM*.STX for later expansion.
;;
;;Here is a description of the arguments.
;;
;;BODY-FORM*.STX must be null or a list of syntax objects representing the forms.
;;
;;LEXENV.RUN  and LEXENV.EXPAND  must  be lists  representing  the current  lexical
;;environment for run and expand times.
;;
;;About the MOD** argument.  We know that module definitions have the syntax:
;;
;;   (module (?export-id ...) ?definition ... ?expression ...)
;;
;;and the trailing ?EXPRESSION forms must  be evaluated after the right-hand sides of
;;the DEFINE syntaxes of  the module but also of the  enclosing lexical context.  So,
;;when expanding a MODULE, we accumulate  such expression syntax objects in the MOD**
;;argument as:
;;
;;   MOD** == ((?expression ...) ...)
;;
;;KWD*  is a  list of  identifiers  representing the  names of  syntaxes and  lexical
;;variables defined in this body.  It is used to test for duplicate definitions.
;;
;;EXPORT-SPEC*  is  null  or  a  list  of  syntax  objects  representing  the  export
;;specifications from this body.  It is to be processed later.
;;
;;RIB is the current lexical environment's rib.
;;
;;MIX? is interpreted  as boolean.  When false: the expansion  process visits all the
;;definition forms and  stops at the first expression form;  the expression forms are
;;returned to the caller.  When true: the expansion process visits all the definition
;;and expression  forms, accepting a  mixed sequence of  them; an expression  form is
;;handled as a dummy definition form.
;;
;;When the argument SHADOW/REDEFINE-BINDINGS? is set to false:
;;
;;* Syntactic binding  definitions in this body must *not*  shadow syntactic bindings
;;established by the top-level environment.  For example, in the program:
;;
;;   (import (vicare))
;;   (define display 1)
;;   (debug-print display)
;;
;;the DEFINE use  is a syntax violation  because the use of DEFINE  cannot shadow the
;;binding imported from "(vicare)".
;;
;;**  Syntactic bindings  established  in  the body  must  *not*  be redefined.   For
;;example, in the program:
;;
;;   (import (vicare))
;;   (define a 1)
;;   (define a 2)
;;   (debug-print a)
;;
;;the  second DEFINE  use is  a syntax  violation because  the use  of DEFINE  cannot
;;redefine the binding for "a".
;;
;;When the argument SHADOW/REDEFINE-BINDINGS? is set to true:
;;
;;**  Syntactic binding  definitions in  this body  are allowed  to shadow  syntactic
;;bindings established by the top-level environment.  For example, at the REPL:
;;
;;   vicare> (import (vicare))
;;   vicare> (define display 1)
;;   vicare> (debug-print display)
;;
;;the  DEFINE  use is  fine:  it  is allowed  to  shadow  the binding  imported  from
;;"(vicare)".
;;
;;* Syntactic binding definitions in the body  can be redefined.  For example, at the
;;REPL:
;;
;;  vicare> (begin
;;            (define a 1)
;;            (define a 2)
;;            (debug-print a))
;;
;;the second DEFINE use is fine: it can redefine the binding for "a".

(module (chi-body*)

  (define-module-who chi-body*)


(define* (chi-body* body-form*.stx lexenv.run lexenv.expand
		    rev-qdef* mod** kwd* export-spec* rib
		    mix? shadow/redefine-bindings?)
  (if (pair? body-form*.stx)
      (let*-values
	  (((body-form.stx)	(car body-form*.stx))
	   ((type descr kwd)	(syntactic-form-type __module_who__ body-form.stx lexenv.run))
	   ((kwd*)		(if (identifier? kwd)
				    (cons kwd kwd*)
				  kwd*)))
	;;(debug-print __module_who__ body-form.stx)
	(parametrise ((current-run-lexenv (lambda () lexenv.run)))
	  (case type
	    ((integrated-macro)
	     (%chi-integrated-macro body-form*.stx lexenv.run lexenv.expand
				    rev-qdef* mod** kwd* export-spec* rib
				    mix? shadow/redefine-bindings?
				    body-form.stx descr kwd))

	    ((global-macro global-macro!)
	     ;;The body  form is  a macro  use, where  the macro  is imported  from a
	     ;;library.   We  perform  the  macro  expansion,  then  recurse  on  the
	     ;;resulting syntax object.  The syntactic binding's descriptor DESCR has
	     ;;format:
	     ;;
	     ;;   (global-macro  . (#<library> . ?loc))
	     ;;   (global-macro! . (#<library> . ?loc))
	     ;;
	     (let ((body-form.stx^ (chi-global-macro (syntactic-binding-descriptor.value descr)
						     body-form.stx lexenv.run rib)))
	       (chi-body* (cons body-form.stx^ (cdr body-form*.stx))
			  lexenv.run lexenv.expand
			  rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?)))

	    ((local-macro local-macro!)
	     ;;The body form is a macro use,  where the macro is locally defined.  We
	     ;;perform  the macro  expansion, then  recurse on  the resulting  syntax
	     ;;object.  The syntactic binding's descriptor DESCR has format:
	     ;;
	     ;;   (local-macro  . (?transformer . ?expanded-expr))
	     ;;   (local-macro! . (?transformer . ?expanded-expr))
	     ;;
	     (let ((body-form.stx^ (chi-local-macro (syntactic-binding-descriptor.value descr)
						    body-form.stx lexenv.run rib)))
	       (chi-body* (cons body-form.stx^ (cdr body-form*.stx))
			  lexenv.run lexenv.expand
			  rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?)))

	    ((macro macro!)
	     ;;The body  form is  a macro use,  where the macro  is a  non-core macro
	     ;;integrated  in the  expander.  We  perform the  macro expansion,  then
	     ;;recurse  on  the resulting  syntax  object.   The syntactic  binding's
	     ;;descriptor DESCR has format:
	     ;;
	     ;;   (macro  . ?macro-name)
	     ;;   (macro! . ?macro-name)
	     ;;
	     (let ((body-form.stx^ (chi-non-core-macro (syntactic-binding-descriptor.value descr)
						       body-form.stx lexenv.run rib)))
	       (chi-body* (cons body-form.stx^ (cdr body-form*.stx))
			  lexenv.run lexenv.expand
			  rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?)))

	    ((standalone-unbound-identifier)
	     (error-unbound-identifier __module_who__ body-form.stx))

	    ((displaced-lexical)
	     (syntax-violation __module_who__
	       "identifier out of context" body-form.stx
	       (syntax-match body-form.stx ()
		 ((?car . ?cdr)
		  ?car)
		 (?id
		  (identifier? ?id)
		  ?id)
		 (_ #f))))

	    (else
	     ;;Any other expression.
	     (%chi-trailing-expressions body-form*.stx lexenv.run lexenv.expand rev-qdef* mod** kwd* export-spec* rib
					mix? shadow/redefine-bindings?)))))
    (values body-form*.stx lexenv.run lexenv.expand rev-qdef* mod** kwd* export-spec*)))


(define (%chi-integrated-macro body-form*.stx lexenv.run lexenv.expand
			       rev-qdef* mod** kwd* export-spec* rib
			       mix? shadow/redefine-bindings?
			       body-form.stx descr kwd)
  ;;BODY-FORM.STX is the first item in BODY-FORM*.STX.
  ;;
  (import CHI-DEFINE)

  (define-syntax-rule (%expand-single-internal-definition ?expander-who)
    ;;Used to  process a single  internal definition core  macro.  We parse  the body
    ;;form  and generate  a  qualified right-hand  side (QDEF)  object  that will  be
    ;;expanded later.
    ;;
    (receive (qdef lexenv.run)
	#;(?expander-who body-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
	(?expander-who (if (options::debug-mode-enabled?)
			   (stx-push-annotated-expr body-form.stx body-form.stx)
			 body-form.stx)
		       rib lexenv.run kwd* shadow/redefine-bindings?)
      (chi-body* (cdr body-form*.stx)
		 lexenv.run lexenv.expand
		 (cons qdef rev-qdef*)
		 mod** kwd* export-spec* rib mix? shadow/redefine-bindings?)))

  (define-syntax-rule (%expand-multiple-internal-definitions ?expander-who)
    ;;Used to  process an internal definition  core macro that expands  into multiple
    ;;internal definitions.  We parse the body  form and generate a list of qualified
    ;;right-hand side (QDEF) objects that will be expanded later.
    ;;
    (receive (qdef* lexenv.run)
	#;(?expander-who body-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
	(?expander-who (if (options::debug-mode-enabled?)
			   (stx-push-annotated-expr body-form.stx body-form.stx)
			 body-form.stx)
		       rib lexenv.run kwd* shadow/redefine-bindings?)
      (chi-body* (cdr body-form*.stx)
		 lexenv.run lexenv.expand
		 (append (reverse qdef*) rev-qdef*)
		 mod** kwd* export-spec* rib mix? shadow/redefine-bindings?)))

  (case (syntactic-binding-descriptor.value descr)
    ((define/std)
     ;;The body form is a DEFINE/STD core macro use, one among:
     ;;
     ;;   (define/std ?lhs ?rhs)
     ;;   (define/std (?lhs . ?formals) . ?body)
     ;;
     ;;it is meant to be the implementation of R6RS's DEFINE syntax.
     (%expand-single-internal-definition chi-define/std))

    ((define/typed)
     ;;The body form is a DEFINE/TYPED core macro use, one among:
     ;;
     ;;   (define/typed ?lhs ?rhs)
     ;;   (define/typed (?lhs . ?formals) . ?body)
     ;;
     ;;it  is meant  to  be an  extension  to R6RS's  DEFINE  syntax supporting  type
     ;;specifications.
     (%expand-single-internal-definition chi-define/typed))

    ((define/checked)
     ;;The body form is a DEFINE/TYPED core macro use, one among:
     ;;
     ;;   (define/checked ?lhs ?rhs)
     ;;   (define/checked (?lhs . ?formals) . ?body)
     ;;
     ;;it  is meant  to  be an  extension  to R6RS's  DEFINE  syntax supporting  type
     ;;specifications.
     (%expand-multiple-internal-definitions chi-define/checked))

    ((case-define/std)
     ;;The body form is a CASE-DEFINE/STD core macro use:
     ;;
     ;;   (case-define/std ?lhs ?case-lambda-clause ...)
     ;;
     ;;it is meant to be equivalent to:
     ;;
     ;;   (define/std ?lhs (case-lambda/std ?case-lambda-clause ...))
     ;;
     (%expand-single-internal-definition chi-case-define/std))

    ((case-define/typed)
     ;;The body form is a CASE-DEFINE/TYPED core macro use:
     ;;
     ;;   (case-define/typed ?lhs ?case-lambda-clause ...)
     ;;
     ;;it is meant  to be an extension to the  CASE-DEFINE/STD syntax supporting type
     ;;specifications.
     (%expand-single-internal-definition chi-case-define/typed))

    ((case-define/checked)
     ;;The body form is a CASE-DEFINE/CHECKED core macro use:
     ;;
     ;;   (case-define/checked ?lhs ?case-lambda-clause ...)
     ;;
     ;;it is meant  to be an extension to the  CASE-DEFINE/STD syntax supporting type
     ;;specifications.
     (%expand-multiple-internal-definitions chi-case-define/checked))

    ((define/overload)
     ;;The body form is a DEFINE/OVERLOAD core macro use, one among:
     ;;
     ;;   (define/overload (?lhs . ?formals) . ?body)
     ;;
     ;;it  is meant  to  be an  extension  to R6RS's  DEFINE  syntax supporting  type
     ;;specifications and overloading.
     (%expand-multiple-internal-definitions chi-define/overload))

    ((define-syntax)
     ;;The body form is a built-in DEFINE-SYNTAX macro use.  This is what happens:
     ;;
     ;;1. Parse the syntactic form.
     ;;
     ;;2. Expand the right-hand side expression.  This expansion happens in a lexical
     ;;context in which the syntactic binding of  the macro itself has *not* yet been
     ;;established.
     ;;
     ;;3.  Add to  the  rib the  syntactic binding's  association  between the  macro
     ;;keyword and the label.
     ;;
     ;;4.  Evaluate the right-hand side expression, which is meant to return: a macro
     ;;transformer function, a compile-time value, a synonym transformer or whatever.
     ;;
     ;;5. Add to the lexenv the syntactic binding's association between the label and
     ;;the binding's descriptor.
     ;;
     ;;6. Finally recurse to expand the rest of the body.
     ;;
     (receive (id rhs.stx)
	 (%parse-define-syntax body-form.stx)
       (when (bound-id-member? id kwd*)
	 (stx-error body-form.stx "cannot redefine keyword"))
       (let ((rhs.core (expand-macro-transformer rhs.stx lexenv.expand))
	     (lab      (generate-label-gensym id)))
	 ;;This call will raise an exception if it represents an attempt to illegally
	 ;;redefine a binding.
	 (extend-rib! rib id lab shadow/redefine-bindings?)
	 (let ((entry (cons lab (eval-macro-transformer rhs.core))))
	   (chi-body* (cdr body-form*.stx)
		      (cons entry lexenv.run)
		      (cons entry lexenv.expand)
		      rev-qdef* mod** kwd* export-spec* rib
		      mix? shadow/redefine-bindings?)))))

    ((define-type)
     ;;The body form is a built-in DEFINE-TYPE macro use.  This is what happens:
     ;;
     (chi-define-type body-form.stx lexenv.run lexenv.expand
		      (cdr body-form*.stx)
		      rev-qdef* mod** kwd* export-spec* rib
		      mix? shadow/redefine-bindings?))

    ((define-fluid-syntax)
     ;;The  body form  is a  built-in DEFINE-FLUID-SYNTAX  macro use.   This is  what
     ;;happens:
     ;;
     ;;1. Parse the syntactic form.
     ;;
     ;;2. Expand the right-hand side expression.  This expansion happens in a lexical
     ;;context in which the syntactic binding of  the macro itself has *not* yet been
     ;;established.
     ;;
     ;;3.   Add to  the rib  the syntactic  binding's association  between the  macro
     ;;keyword and the label.
     ;;
     ;;4.  Evaluate  the right-hand  side expression (which  usually returns  a macro
     ;;transformer function).
     ;;
     ;;5. Add to the lexenv the syntactic binding's association between the label and
     ;;the binding's descriptor.
     ;;
     ;;6. Finally recurse to expand the rest of the body.
     ;;
     (receive (lhs.id rhs.stx)
	 (%parse-define-syntax body-form.stx)
       (when (bound-id-member? lhs.id kwd*)
	 (stx-error body-form.stx "cannot redefine keyword"))
       (let ((rhs.core (expand-macro-transformer rhs.stx lexenv.expand))
	     (lhs.lab  (generate-label-gensym lhs.id)))
	 ;;This call will raise an exception if it represents an attempt to illegally
	 ;;redefine a binding.
	 (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	 ;;The  LEXENV entry  KEYWORD-ENTRY represents  the definition  of the  fluid
	 ;;syntax's  keyword   syntactic  binding.   The  LEXENV   entry  FLUID-ENTRY
	 ;;represents the  definition of the current  value of the fluid  syntax, the
	 ;;one   that  later   can   be  shadowed   with   internal  definitions   by
	 ;;FLUID-LET-SYNTAX.
	 (let* ((descriptor	(eval-macro-transformer rhs.core))
		(fluid-label	(generate-label-gensym lhs.id))
		(fluid-entry	(cons fluid-label descriptor))
		(keyword-entry	(cons lhs.lab (make-syntactic-binding-descriptor/local-global-macro/fluid-syntax fluid-label))))
	   (chi-body* (cdr body-form*.stx)
		      (cons* keyword-entry fluid-entry lexenv.run)
		      (cons* keyword-entry fluid-entry lexenv.expand)
		      rev-qdef* mod** kwd* export-spec* rib
		      mix? shadow/redefine-bindings?)))))

    ((define-alias)
     ;;The  body form  is a  core  language DEFINE-ALIAS  macro  use.  We  add a  new
     ;;association identifier/label  to the current  rib.  Finally we recurse  on the
     ;;rest of the body.
     ;;
     (receive (alias-id old-id)
	 (%parse-define-alias body-form.stx)
       (when (bound-id-member? old-id kwd*)
	 (stx-error body-form.stx "cannot redefine keyword"))
       (cond ((id->label old-id)
	      => (lambda (label)
		   ;;This call will raise an exception if it represents an attempt to
		   ;;illegally redefine a binding.
		   (extend-rib! rib alias-id label shadow/redefine-bindings?)
		   (chi-body* (cdr body-form*.stx)
			      lexenv.run lexenv.expand
			      rev-qdef* mod** kwd* export-spec* rib
			      mix? shadow/redefine-bindings?)))
	     (else
	      (stx-error body-form.stx "unbound source identifier")))))

    ((let-syntax letrec-syntax)
     ;;The body  form is a core  language LET-SYNTAX or LETREC-SYNTAX  macro use.  We
     ;;expand and evaluate the transformer  expressions, build syntactic bindings for
     ;;them, register their labels in a new  rib because they are visible only in the
     ;;internal body.  The  internal forms are spliced in the  external body but with
     ;;the rib added to them.
     ;;
     (syntax-match body-form.stx ()
       ((_ ((?xlhs* ?xrhs*) ...) ?xbody* ...)
	(begin
	  (unless (valid-bound-ids? ?xlhs*)
	    (stx-error body-form.stx "invalid identifiers"))
	  (let* ((xlab*  (map generate-label-gensym ?xlhs*))
		 (xrib   (make-rib/from-identifiers-and-labels ?xlhs* xlab*))
		 ;;We evaluate  the transformers  for LET-SYNTAX without  pushing the
		 ;;XRIB: the syntax bindings do not exist in the environment in which
		 ;;the transformer is evaluated.
		 ;;
		 ;;We evaluate  the transformers for LETREC-SYNTAX  after pushing the
		 ;;XRIB: the syntax bindings do exist in the environment in which the
		 ;;transformer is evaluated.
		 (xbind* (map (lambda (x)
				(let ((in-form (case (syntactic-binding-descriptor.value descr)
						 ((let-syntax)
						  x)
						 ((letrec-syntax)
						  (push-lexical-contour xrib x))
						 (else
						  (assertion-violation __module_who__ "internal error" body-form.stx)))))
				  (eval-macro-transformer (expand-macro-transformer in-form lexenv.expand))))
			   ?xrhs*)))
	    (chi-body*
	     ;;Splice the internal body forms but add a lexical contour to them.
	     (append (map (lambda (internal-body-form)
			    (push-lexical-contour xrib
			      internal-body-form))
		       ?xbody*)
		     (cdr body-form*.stx))
	     ;;Push on the  lexical environment entries corresponding  to the defined
	     ;;syntaxes.  Such entries  will stay there even after  we have processed
	     ;;the internal  body forms;  this is  not a  problem because  the labels
	     ;;cannot be seen by the rest of the body.
	     (append (map cons xlab* xbind*) lexenv.run)
	     (append (map cons xlab* xbind*) lexenv.expand)
	     rev-qdef* mod** kwd* export-spec* rib
	     mix? shadow/redefine-bindings?))))))

    ((begin-for-syntax)
     (chi-begin-for-syntax body-form.stx body-form*.stx lexenv.run lexenv.expand
			   rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))

    ((begin)
     ;;The body form is a BEGIN syntax  use.  Just splice the expressions and recurse
     ;;on them.
     ;;
     (syntax-match body-form.stx ()
       ((_ ?expr* ...)
	(chi-body* (append ?expr* (cdr body-form*.stx))
		   lexenv.run lexenv.expand
		   rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))))

    ((stale-when)
     ;;The  body form  is  a STALE-WHEN  syntax use.   Process  the stale-when  guard
     ;;expression, then just  splice the internal expressions as we  do for BEGIN and
     ;;recurse.
     ;;
     (syntax-match body-form.stx ()
       ((_ ?guard ?expr* ...)
	(begin
	  (handle-stale-when ?guard lexenv.expand)
	  (chi-body* (append ?expr* (cdr body-form*.stx))
		     lexenv.run lexenv.expand
		     rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?)))))

    ((module)
     ;;The body form  is an internal module definition.  We  process the module, then
     ;;recurse on the rest of the body.
     ;;
     (receive (rev-qdef* m-exp-id* m-exp-lab* lexenv.run lexenv.expand mod** kwd*)
	 (chi-internal-module body-form.stx lexenv.run lexenv.expand rev-qdef* mod** kwd*)
       ;;Extend the rib with the syntactic bindings exported by the module.
       (vector-for-each (lambda (id lab)
			  ;;This call  will raise  an exception  if it  represents an
			  ;;attempt to illegally redefine a binding.
			  (extend-rib! rib id lab shadow/redefine-bindings?))
	 m-exp-id* m-exp-lab*)
       (chi-body* (cdr body-form*.stx) lexenv.run lexenv.expand
		  rev-qdef* mod** kwd* export-spec*
		  rib mix? shadow/redefine-bindings?)))

    ((library)
     ;;The body form  is a library definition.  We process  the library, then recurse
     ;;on the rest of the body.
     ;;
     (expand-library (syntax->datum body-form.stx))
     (chi-body* (cdr body-form*.stx)
		lexenv.run lexenv.expand
		rev-qdef* mod** kwd* export-spec*
		rib mix? shadow/redefine-bindings?))

    ((export)
     ;;The  body   form  is  an   EXPORT  form.    We  just  accumulate   the  export
     ;;specifications, to be processed later, and we recurse on the rest of the body.
     ;;
     (syntax-match body-form.stx ()
       ((_ ?export-spec* ...)
	(chi-body* (cdr body-form*.stx)
		   lexenv.run lexenv.expand
		   rev-qdef* mod** kwd*
		   (append ?export-spec* export-spec*)
		   rib mix? shadow/redefine-bindings?))))

    ((import)
     ;;The body form  is an IMPORT form.   We just process the form  which results in
     ;;extending  the RIB  with  more identifier-to-label  associations.  Finally  we
     ;;recurse on the rest of the body.
     ;;
     (chi-import body-form.stx lexenv.run rib shadow/redefine-bindings?)
     (chi-body* (cdr body-form*.stx) lexenv.run lexenv.expand
		rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))

    ((set! set!/initialise)
     (%chi-trailing-expressions body-form*.stx lexenv.run lexenv.expand rev-qdef* mod** kwd* export-spec* rib
				mix? shadow/redefine-bindings?))

    ((expand-time-expr)
     ;;Vicare's  EXPAND-TIME-EXPR   integrated-macro  use.    First  we   check  with
     ;;SYNTAX-MATCH  that the  syntax is  correct, then  we build  the core  language
     ;;expression.
     ;;
     (syntax-match body-form.stx ()
       ((_ ?expand-time-expr)
	(let ((new-body-form.stx (compiler::eval-core (expanded->core (expand-macro-transformer ?expand-time-expr lexenv.expand)))))
	  (chi-body* (cons new-body-form.stx (cdr body-form*.stx))
		     lexenv.run lexenv.expand
		     rev-qdef* mod** kwd* export-spec*
		     rib mix? shadow/redefine-bindings?)))
       ))

    (else
     (assertion-violation __module_who__
       "internal error, invalid integrated-macro descriptor" (car body-form*.stx) descr))))


(define (%chi-trailing-expressions body-form*.stx lexenv.run lexenv.expand
				   rev-qdef* mod** kwd* export-spec* rib
				   mix? shadow/redefine-bindings?)
  ;;If mixed definitions and expressions are allowed, we handle this expression as an
  ;;implicit definition:
  ;;
  ;;   (define/std dummy ?body-form)
  ;;
  ;;which is not really part of the lexical environment: we only generate a lex and a
  ;;qdef for it,  without adding entries to  LEXENV.RUN; then we move on  to the next
  ;;body form.
  ;;
  ;;If mixed  definitions and expressions are  forbidden: we have reached  the end of
  ;;definitions  and expect  this and  all the  other forms  in BODY-FORM*.STX  to be
  ;;expressions.  So BODY-FORM*.STX becomes the "trailing expressions" return value.
  ;;
  (if mix?
      ;;There must be a LEX for every QDEF, really.
      (let ((qdef (make-qdef-top-expr (car body-form*.stx))))
	(chi-body* (cdr body-form*.stx)
		   lexenv.run lexenv.expand
		   (cons qdef rev-qdef*)
		   mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))
    (values body-form*.stx lexenv.run lexenv.expand rev-qdef* mod** kwd* export-spec*)))



;;;; helpers

(define (%parse-define-syntax stx)
  ;;Syntax parser  for R6RS's DEFINE-SYNTAX,  extended with Vicare's  syntax.  Accept
  ;;both:
  ;;
  ;;  (define-syntax ?name ?transformer-expr)
  ;;  (define-syntax (?name ?arg) ?body0 ?body ...)
  ;;
  (syntax-match stx ()
    ((_ (?id ?arg) ?body0 ?body* ...)
     (and (identifier? ?id)
	  (identifier? ?arg))
     (values ?id (bless
		  (let ((synner.id	(make-syntactic-identifier-for-temporary-variable "synner"))
			(message.id	(make-syntactic-identifier-for-temporary-variable "message"))
			(subform.id	(make-syntactic-identifier-for-temporary-variable "subform")))
		    `(named-lambda/std ,?id (,?arg)
		       (letrec/checked
			   ((,synner.id (case-lambda/checked
					  (({,message.id <string>})
					   (syntax-violation (quote ,?id) ,message.id ,?arg #f))
					  (({,message.id <string>} ,subform.id)
					   (syntax-violation (quote ,?id) ,message.id ,?arg ,subform.id)))))
			 (fluid-let-syntax
			     ((__synner__ (identifier-syntax ,synner.id)))
			   ,?body0 . ,?body*)))))))
    ((_ ?id)
     (identifier? ?id)
     (values ?id (bless
		  `(lambda/std (stx)
		     (syntax-violation (quote ,?id)' "invalid syntax use" stx #f)))))
    ((_ ?id ?transformer-expr)
     (identifier? ?id)
     (values ?id ?transformer-expr))
    ))

(define (%parse-define-alias body-form.stx)
  ;;Syntax parser for Vicares's DEFINE-ALIAS.
  ;;
  (syntax-match body-form.stx ()
    ((_ ?alias-id ?old-id)
     (and (identifier? ?alias-id)
	  (identifier? ?old-id))
     (values ?alias-id ?old-id))
    ))


(module (chi-import)
  ;;Process  an  IMPORT  form.  The  purpose  of  such  forms  is to  push  some  new
  ;;identifier-to-label association on the current RIB.
  ;;
  (define-module-who import)

  (define (chi-import body-form.stx lexenv.run rib shadow/redefine-bindings?)
    (receive (id.vec lab.vec)
	(%any-import*-checked body-form.stx lexenv.run)
      (vector-for-each (lambda (id lab)
			 ;;This  call will  raise an  exception if  it represents  an
			 ;;attempt to illegally redefine a binding.
			 (extend-rib! rib id lab shadow/redefine-bindings?))
	id.vec lab.vec)))

  (define (%any-import*-checked import-form.stx lexenv.run)
    (syntax-match import-form.stx ()
      ((?ctxt ?import-spec* ...)
       (%any-import* ?ctxt ?import-spec* lexenv.run))
      (_
       (stx-error import-form.stx "invalid import form"))))

  (define (%any-import* ctxt import-spec* lexenv.run)
    (if (null? import-spec*)
	(values '#() '#())
      (let-values
	  (((t1 t2) (%any-import  ctxt (car import-spec*) lexenv.run))
	   ((t3 t4) (%any-import* ctxt (cdr import-spec*) lexenv.run)))
	(values (vector-append t1 t3)
		(vector-append t2 t4)))))

  (define (%any-import ctxt import-spec lexenv.run)
    (if (identifier? import-spec)
	(%module-import (list ctxt import-spec) lexenv.run)
      (%library-or-module-import (list ctxt import-spec) lexenv.run)))

  (define (%module-import import-form.stx lexenv.run)
    (syntax-match import-form.stx ()
      ((_ ?module-name)
       (identifier? ?module-name)
       (module-id->id-and-lab-vecs import-form.stx lexenv.run ?module-name))
      ))

  (define (module-id->id-and-lab-vecs import-form.stx lexenv.run module-name.id)
    ;;Given a syntactic identifier representing a module's name, return two values: a
    ;;vector holding the syntactic identifiers  representing the exported bindings; a
    ;;vector holding the corresponding label gensyms.
    ;;
    (receive (type descr kwd)
	(syntactic-form-type __module_who__ module-name.id lexenv.run)
      (case type
	(($module)
	 (let ((iface (syntactic-binding-descriptor.value descr)))
	   (values (module-interface-exp-id*     iface (stx-mark* module-name.id))
		   (module-interface-exp-lab-vec iface))))
	((standalone-unbound-identifier)
	 (error-unbound-identifier __module_who__ module-name.id))
	(else
	 (stx-error import-form.stx "invalid import")))))

  (define (module-id->module-subst import-form.stx lexenv.run module-name.id)
    ;;Given a syntactic identifier representing  a module's name, return the module's
    ;;export  subst: an  a  list whose  cars are  symbols  representing the  exported
    ;;bindings' names and whose cdrs are the associated label gensyms.
    ;;
    (receive (exported-id-vec exported-lab-vec)
	(module-id->id-and-lab-vecs import-form.stx lexenv.run module-name.id)
      (vector-fold-right (lambda (id lab knil)
			   (cons (cons (identifier->symbol id) lab) knil))
	'() exported-id-vec exported-lab-vec)))

  (define (%library-or-module-import import-form.stx lexenv.run)
    (syntax-match import-form.stx ()
      ((?ctxt ?imp* ...)
       ;;NAME-VEC is  a vector of  symbols representing  the external names  of the
       ;;imported  bindings.   LABEL-VEC is  a  vector  of label  gensyms  uniquely
       ;;associated to the imported bindings.
       (receive (name-vec label-vec)
	   (parse-import-spec* ?imp*
			       (lambda (module-name.id)
				 (module-id->module-subst import-form.stx lexenv.run module-name.id)))
	 (values (vector-map (lambda (name)
			       (~datum->syntax ?ctxt name))
		   name-vec)
		 label-vec)))
      (_
       (stx-error import-form.stx "invalid import form"))))

  #| end of module: CHI-IMPORT |# )


;;;; chi procedures: BEGIN-FOR-SYNTAX

(module (chi-begin-for-syntax)
  ;;This module  is used only by  the function CHI-BODY*, because  a BEGIN-FOR-SYNTAX
  ;;macro use can appear only in a body.
  ;;
  ;;The input form is a BEGIN-FOR-SYNTAX syntax use:
  ;;
  ;;   (begin-for-syntax ?expr0 ?expr ...)
  ;;
  ;;We expand the  expressions using LEXENV.EXPAND as LEXENV for  run-time, much like
  ;;what we do  when evaluating the right-hand side of  a DEFINE-SYNTAX, but handling
  ;;the sequence  of expressions  as a body;  then we build  a special  core language
  ;;expression with global assignments; finally we evaluate the result.
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
				qdef* mod** kwd* export-spec* rib mix?
				shadow/redefine-bindings?)
    ;;BODY-FORM.STX is the first item in BODY-FORM*.STX.
    ;;
    (define (%go-on lexenv.run lexenv.expand)
      (chi-body* (cdr body-form*.stx)
		 lexenv.run lexenv.expand
		 qdef* mod** kwd* export-spec* rib
		 mix? shadow/redefine-bindings?))
    (receive (lhs*.lex init*.core rhs*.core lexenv.expand^)
	(%expand input-form.stx lexenv.expand rib shadow/redefine-bindings?)
      ;;Build an expanded  code expression and evaluate it.
      (let ((visit-code.core (build-sequence no-source
			       (append (map (lambda (lhs.lex rhs.core)
					      (build-global-assignment no-source
						lhs.lex rhs.core))
					 lhs*.lex rhs*.core)
				       init*.core))))
	;;Done!  Push on the LEXENV an entry like:
	;;
	;;   (?unused-label . (begin-for-syntax . ?core-code))
	;;
	;;then go on with the next body forms.
	(if (void-core-expression? visit-code.core)
	    (%go-on lexenv.run lexenv.expand^)
	  (begin
	    (compiler::eval-core (expanded->core visit-code.core))
	    (let* ((bfs.des	(make-syntactic-binding-descriptor/begin-for-syntax visit-code.core))
		   (bfs.lab	(generate-label-gensym "begin-for-syntax-label"))
		   (lexenv.run^	(push-entry-on-lexenv bfs.lab bfs.des lexenv.run)))
	      (%go-on lexenv.run^ lexenv.expand^)))))))

  (define (%expand input-form.stx lexenv.expand rib shadow/redefine-bindings?)
    (define rtc
      (make-collector))
    (parametrise ((inv-collector rtc)
		  (vis-collector (lambda (x) (void))))
      (receive (empty
		lexenv.expand^ lexenv.super^
		qdef* module-init** kwd* export-spec*)
	  ;;Expand  the sequence  of  forms  as a  top-level  body, accumulating  the
	  ;;definitions.
	  (let ((lexenv.super                     lexenv.expand)
		(qdef*                            '())
		(mod**                            '())
		(kwd*                             '())
		(export-spec*                     '())
		(mix-definitions-and-expressions? #t))
	    (syntax-match input-form.stx ()
	      ((_ ?expr ?expr* ...)
	       (chi-body* (cons ?expr ?expr*)
			  lexenv.expand lexenv.super
			  qdef* mod** kwd* export-spec* rib
			  mix-definitions-and-expressions? shadow/redefine-bindings?))))
	;;There must be no trailing expressions because we allowed mixing definitions
	;;and expressions as in a top-level program.
	(assert (null? empty))
	;;Expand the definitions and the module trailing expressions.
	(let* ((qdef*		(reverse qdef*))
	       (lhs*.lex	(map qdef.lex qdef*))
	       (rhs*.psi	(chi-qdef* qdef* lexenv.expand^ lexenv.super^))
	       (init*.psi	(chi-expr* (reverse-and-append module-init**) lexenv.expand^ lexenv.super^)))
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


;;;; chi procedures: MODULE processing

(module (chi-internal-module
	 module-interface-exp-id*
	 module-interface-exp-lab-vec)

  (define-core-record-type <module-interface>
    (nongenerative vicare:expander:<module-interface>)
    (define-type-descriptors)
    (strip-angular-parentheses)
    (fields (immutable first-mark	module-interface-first-mark)
		;The first mark in the lexical context of the MODULE form.
	    (immutable exp-id-vec	module-interface-exp-id-vec)
		;A vector of identifiers exported by the module.
	    (immutable exp-lab-vec	module-interface-exp-lab-vec)
		;A vector  of gensyms  acting as  labels for  the identifiers  in the
		;field EXP-ID-VEC.
	    ))

  (define (chi-internal-module module-form.stx lexenv.run lexenv.expand
			       rev-qdef* module-trailing-expression** kwd*)
    ;;Expand  the syntax  object  MODULE-FORM.STX which  represents  a core  language
    ;;MODULE syntax use.
    ;;
    ;;LEXENV.RUN and  LEXENV.EXPAND must  be lists  representing the  current lexical
    ;;environment for run and expand times.
    ;;
    ;;Return the  following values:  REV-QDEF*, a list  of QDEF  objects representing
    ;;internal definitions;  M-EXP-ID*, a list of  syntactic identifiers representing
    ;;the exported  syntactic bindings (this can  be only the name  identifier of the
    ;;module);  M-EXP-LAB*,  a list  of  label  gensyms  for the  exported  syntactic
    ;;identifiers;  LEXENV.RUN and  LEXENV.EXPAND; MOD**  a list  of lists  of syntax
    ;;objects  representing the  trailing  module expressions;  KEYWORD*,  a list  of
    ;;identifiers representing the defined syntactic bindings.
    ;;
    (receive (name export-id* internal-body-form*)
	(%parse-module module-form.stx)
      (let* ((module-rib               (make-rib/empty))
	     (internal-body-form*/rib  (map (lambda (form)
					      (push-lexical-contour module-rib form))
					 (syntax->list internal-body-form*))))
	(receive (leftover-body-expr* lexenv.run lexenv.expand rev-qdef* module-trailing-expression** kwd* _export-spec*)
	    ;;About the argument REDEFINE-BINDINGS?
	    ;;-------------------------------------
	    ;;
	    ;;In calling CHI-BODY*  we set the argument  REDEFINE-BINDINGS?  to false
	    ;;because definitions  at the top  level of  the module's body  cannot be
	    ;;redefined.  That is:
	    ;;
	    ;; (import (vicare))
	    ;; (module ()
	    ;;   (define a 1)
	    ;;   (define a 2))
	    ;;
	    ;;is a syntax violation because the binding "a" cannot be redefined.
	    ;;
	    (let ((mix-definitions-and-expressions?	#t)
		  (empty-export-spec*			'())
		  (redefine-bindings?			#f))
	      (chi-body* internal-body-form*/rib
			 lexenv.run lexenv.expand
			 rev-qdef* module-trailing-expression** kwd* empty-export-spec*
			 module-rib mix-definitions-and-expressions? redefine-bindings?))
	  ;;All of  these top-level expression  become dummy definitions, so  they go
	  ;;into REV-QDEF*.
	  (assert (null? leftover-body-expr*))
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
		 (module-trailing-expression** (if (pair? leftover-body-expr*)
						   (cons leftover-body-expr* module-trailing-expression**)
						 module-trailing-expression**)))
	    (if (not name)
		;;The module  has no name.  All  the exported identifiers will  go in
		;;the enclosing lexical environment.
		(values rev-qdef* all-export-id* all-export-lab* lexenv.run lexenv.expand module-trailing-expression** kwd*)
	      ;;The module has a name.  Only the name itself will go in the enclosing
	      ;;lexical environment.
	      (let* ((name-label	(generate-label-gensym 'module))
		     (iface		(make-module-interface
					 (car (stx-mark* name))
					 (vector-map
					     (lambda (x)
					       (let ((rib* '())
						     (ae*  '()))
						 (make-syntactic-identifier (stx-expr x) (stx-mark* x) rib* ae*)))
					   all-export-id*)
					 all-export-lab*))
		     (descr		(make-syntactic-binding-descriptor/local-global-macro/module-interface iface))
		     (entry		(cons name-label descr)))
		(values rev-qdef*
			;;FIXME: module cannot export itself yet.  Abdulaziz Ghuloum.
			(vector name)
			(vector name-label)
			(cons entry lexenv.run)
			(cons entry lexenv.expand)
			module-trailing-expression** kwd*))))))))

  (define (%parse-module module-form.stx)
    ;;Parse  a syntax  object representing  a core  language MODULE  form.  Return  3
    ;;values:  false  or an  identifier  representing  the  module  name; a  list  of
    ;;identifiers selecting the  exported bindings from the first  MODULE argument; a
    ;;list of syntax objects representing the internal body forms.
    ;;
    (syntax-match module-form.stx ()
      ((_ (?export* ...) ?body* ...)
       (begin
	 (unless (for-all identifier? ?export*)
	   (stx-error module-form.stx "module exports must be identifiers"))
	 (values #f (list->vector ?export*) ?body*)))
      ((_ ?name (?export* ...) ?body* ...)
       (begin
	 (unless (identifier? ?name)
	   (stx-error module-form.stx "module name must be an identifier"))
	 (unless (for-all identifier? ?export*)
	   (stx-error module-form.stx "module exports must be identifiers"))
	 (values ?name (list->vector ?export*) ?body*)))
      ))

  (module (module-interface-exp-id*)

    (define (module-interface-exp-id* iface import-mark*)
      ;;IFACE is  an instance of MODULE-INTERFACE.   IMPORT-MARK* is a list  of marks
      ;;representing the lexical  context of the IMPORT syntax use  that is importing
      ;;the module.
      ;;
      ;;Return  a vector  of  syntactic identifiers  representing syntactic  bindings
      ;;exported by the module; each identifier  has the marks of the lexical context
      ;;in which the module is imported.
      ;;
      (let ((mark-diff*	(%diff-marks import-mark* (module-interface-first-mark iface)))
	    (id-vec	(module-interface-exp-id-vec iface)))
	(if (null? mark-diff*)
	    id-vec
	  (vector-map
	      (lambda (x)
		(let ((rib* '())
		      (ae*  '()))
		  (make-syntactic-identifier (stx-expr x)
					     (append mark-diff* (stx-mark* x))
					     rib* ae*)))
	    id-vec))))

    (define* (%diff-marks mark* the-mark)
      ;;MARK* must be  a non-empty list of  marks; THE-MARK must be a  mark in MARK*.
      ;;Return a  sublist of  the elements  of MARK* from  the head  up to  and *not*
      ;;including THE-MARK.
      ;;
      ;;   (%diff-marks '(m1 m2 m3 m4) 'm3) => (m1 m2)
      ;;
      (if (pair? mark*)
	  (let ((a (car mark*)))
	    (if (eq? a the-mark)
		'()
	      (cons a (%diff-marks (cdr mark*) the-mark))))
	(assertion-violation __who__ "internal error: null marks should not happen")))

    #| end of module: MODULE-INTERFACE-EXP-ID* |# )

  #| end of module |# )


;;;; parsing DEFINE-TYPE forms

(module (chi-define-type)
  ;;Transformer  function  used  to  expand  Vicare's  DEFINE-TYPE  macros  from  the
  ;;top-level built in environment.
  ;;
  ;;On recursive types: how do recursive types work?
  ;;------------------------------------------------
  ;;
  ;;We can imagine the DEFINE-TYPE form:
  ;;
  ;;  (define-type <it>
  ;;    (or <vector> (list-of <it>)))
  ;;
  ;;as expanding into the following pseudo-code:
  ;;
  ;;  (define-type/forward-definition <it>)
  ;;
  ;;  (define-type/concrete-definition <it>
  ;;    (or <vector> (list-of <it>)))
  ;;
  ;;The syntax use of  DEFINE-TYPE/FORWARD-DEFINITION establishes a syntactic binding
  ;;for "<it>" whose type descriptor represents a "<reference-type-spec>" object-type
  ;;specification.   Such  reference  is  "dangling":  it  references  the  syntactic
  ;;identifier "<it>", but has no concrete object-type specification.
  ;;
  ;;The syntax use of DEFINE-TYPE/CONCRETE-DEFINITION does the following:
  ;;
  ;;1.  Expand  the right-hand side,  building a concrete  object-type specification;
  ;;nested "<it>" type annotations are represented by the "<reference-type-spec>".
  ;;
  ;;2.   Mutate  the   syntactic  binding's  descriptor  for   "<it>"  replacing  the
  ;;"<reference-type-spec>" with the concrete "<object-type-spec>".
  ;;
  ;;3.   Register   the  concrete   object-type  specification   of  "<it>"   in  the
  ;;"<reference-types-spec>", so that the reference is no more dangling.
  ;;
  ;;
  ;;On forward definitions: how do forward type definitions work?
  ;;-------------------------------------------------------------
  ;;
  ;;We can imagine the DEFINE-TYPE forms:
  ;;
  ;;  (define-type <it>)
  ;;  (define-type <that>
  ;;    (list-of <it>))
  ;;  (define-type <it>
  ;;    <fixnum>)
  ;;
  ;;as expanding into the following pseudo-code:
  ;;
  ;;  (define-type/forward-definition <it>)
  ;;
  ;;  (define-type/forward-definition <that>)
  ;;  (define-type/concrete-definition <that>
  ;;    (list-of <it>))
  ;;
  ;;  (define-type/concrete-definition <it>
  ;;    <fixnum>)
  ;;
  ;;which do the following:
  ;;
  ;;1.   The form  "(define-type/forward-definition  <it>)"  establishes a  syntactic
  ;;binding  for "<it>"  whose type  descriptor represents  a forward  definition for
  ;;"<it>"; the descriptor holds a "<reference-type-spec>" referencing the identifier
  ;;"<it>" but having no concrete object-type specification.
  ;;
  ;;2.   The forms  acting on  "<that>" establish  a syntactic  binding for  "<that>"
  ;;representing an  object-type specification.   The nested type  annotations "<it>"
  ;;are represented by the associated "<reference-type-spec>".
  ;;
  ;;3. The  syntax use "(define-type/concrete-definition <it>  <fixnum>)" mutates the
  ;;syntactic binding for "<it>" to reference the concrete object-type specification.
  ;;
  (define-module-who chi-define-type)

  (define (chi-define-type input-form.stx lexenv.run lexenv.expand
			   rest-body-form*.stx rev-qdef* mod** kwd* export-spec* rib
			   mix? shadow/redefine-bindings?)
    (case-define synner
      ((message)
       (synner message #f))
      ((message subform)
       (syntax-violation __module_who__ message input-form.stx subform)))
    (receive (type-name.id type-annotation.stx)
	(%parse-define-type input-form.stx synner)
      (cond ((id->label/local type-name.id rib)
	     ;;A syntactic binding with identifier TYPE-NAME.ID already exists.
	     => (lambda (type-name.lab)
		  (let ((type-name.des (label->syntactic-binding-descriptor type-name.lab lexenv.run)))
		    (case (syntactic-binding-descriptor.type type-name.des)
		      ((local-object-type-name)
		       ;;It is a local type definition: fine.
		       (let ((type-name.ots (syntactic-binding-descriptor/local-object-type.object-type-spec type-name.des)))
			 (if (reference-type-spec? type-name.ots)
			     ;;It is a local, forward type definition: fine.
			     (if type-annotation.stx
				 ;;There  is  a  type  annotation,  so  this  is  the
				 ;;redefinition   of   a  previously   defined   type
				 ;;definition: fine.
				 (receive (new-type.des rev-qdef* kwd* lexenv.run)
				     (let ((reference.entry (assq type-name.lab lexenv.run)))
				       (%establish-concrete-object-type-syntactic-binding input-form.stx lexenv.run lexenv.expand
											  type-name.id type-annotation.stx reference.entry
											  rev-qdef* kwd* rib shadow/redefine-bindings?
											  synner))
				   (chi-body* rest-body-form*.stx lexenv.run lexenv.expand
					      rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))
			       ;;There is  no type  annotation, so  this is  a double
			       ;;forward definition, as in:
			       ;;
			       ;;   (define-type <it>)
			       ;;   (define-type <it>)
			       ;;
			       ;;let's do nothing.
			       (chi-body* rest-body-form*.stx lexenv.run lexenv.expand
					  rev-qdef* mod** kwd* export-spec* rib
					  mix? shadow/redefine-bindings?))
			   ;;It  is  a  fully  defined type  definition,  as  in  the
			   ;;following example:
			   ;;
			   ;;   (define-type <it> <fixnum>)
			   ;;   (define-type <it>)	;this one!
			   ;;
			   ;;error.
			   (synner "cannot redefine keyword bound to an object-type specification which is not a forward definition"
				   type-name.id))))

		      (else
		       ;;The syntactic binding is not a local type definition.
		       (synner "cannot redefine keyword not bound to an object-type specification" type-name.id))))))

	    (type-annotation.stx
	     ;;No  syntactic binding  with  identifier TYPE-NAME.ID  exists.  A  type
	     ;;annotation is provided.  This is a new, full type definition.
	     (receive (reference.entry rev-qdef* kwd* lexenv.run lexenv.expand)
		 (%establish-forward-definition-syntactic-binding input-form.stx lexenv.run lexenv.expand
								  type-name.id
								  rev-qdef* kwd* rib shadow/redefine-bindings?
								  synner)
	       (receive (new-type.des rev-qdef* kwd* lexenv.run)
		   (%establish-concrete-object-type-syntactic-binding input-form.stx lexenv.run lexenv.expand
								       type-name.id type-annotation.stx reference.entry
								       rev-qdef* kwd* rib shadow/redefine-bindings?
								       synner)
		 (chi-body* rest-body-form*.stx lexenv.run lexenv.expand
			    rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))))

	    (else
	     ;;No  syntactic binding  with identifier  TYPE-NAME.ID exists.   No type
	     ;;annotation is provided.  This is just a forward type definition.
	     (receive (reference.entry rev-qdef* kwd* lexenv.run lexenv.expand)
		 (%establish-forward-definition-syntactic-binding input-form.stx lexenv.run lexenv.expand
								  type-name.id
								  rev-qdef* kwd* rib shadow/redefine-bindings?
								  synner)
	       (chi-body* rest-body-form*.stx lexenv.run lexenv.expand
			  rev-qdef* mod** kwd* export-spec* rib mix? shadow/redefine-bindings?))))))

;;; --------------------------------------------------------------------

  (define (%parse-define-type input-form.stx synner)
    (define (%error-not-an-identifier type-name.stx)
      (synner "expected identifier as type annotation name" type-name.stx))
    (syntax-match input-form.stx ()
      ((_ ?type-name ?type-annotation)
       (begin
	 (unless (identifier? ?type-name)
	   (%error-not-an-identifier ?type-name))
	 (values ?type-name ?type-annotation)))
      ((_ ?type-name)
       (begin
	 (unless (identifier? ?type-name)
	   (%error-not-an-identifier ?type-name))
	 (values ?type-name #f)))
      (_
       (synner "invalid syntax in macro use" #f))))

;;; --------------------------------------------------------------------

  (define (%establish-forward-definition-syntactic-binding input-form.stx lexenv.run lexenv.expand
							   type-name.id
							   rev-qdef* kwd* rib shadow/redefine-bindings?
							   synner)
    ;;Establish a new syntactic binding  for TYPE-NAME.ID representing a forward type
    ;;definition.  Establish  a new syntactic  binding representing a  "forward" type
    ;;predicate for the type.
    ;;
    (when (bound-id-member? type-name.id kwd*)
      (synner "cannot redefine keyword to forward type-definition" type-name.id))
    ;;Establish the forward type definition's syntactic binding.
    (let* ((name-lhs.id		type-name.id)
	   (name-lhs.lab	(generate-label-gensym name-lhs.id))
	   (name-rhs.ots	(make-reference-type-spec name-lhs.id))
	   ;;(name-rhs.core	(build-application no-source
	   ;; 			    (build-primref no-source 'make-reference-type-spec)
	   ;; 			  (list (build-data no-source name-lhs.id))))
	   ;;(name-lhs.des	(make-syntactic-binding-descriptor/local-object-type-name name-rhs.ots name-rhs.core))
	   (name-rhs.core	(build-data no-source name-rhs.ots))
	   (name-lhs.des	(make-syntactic-binding-descriptor/local-object-type-name name-rhs.ots name-rhs.core)))
      (extend-rib! rib name-lhs.id name-lhs.lab shadow/redefine-bindings?)
      ;;Notice that there is a single entry for both the lexenvs.
      (let* ((reference.entry	(make-lexenv-entry name-lhs.lab name-lhs.des))
	     (lexenv.run	(cons reference.entry lexenv.run))
	     (lexenv.expand	(cons reference.entry lexenv.expand)))
	;;Establish the forward type predicate's syntactic binding.
	(let* ((pred-lhs.id	(reference-type-spec.predicate-id-forward-definition name-rhs.ots))
	       (pred-rhs.stx	(reference-type-spec.forward-type-predicate-stx      name-rhs.ots)))
	  (when (bound-id-member? pred-lhs.id kwd*)
	    (synner "cannot redefine keyword to forward type-definition type-predicate" pred-lhs.id))
	  (let* ((pred-lhs.lab	(generate-label-gensym pred-lhs.id))
		 (pred-lhs.ots	(make-type-predicate-spec))
		 (pred-qdef	(make-qdef-typed-defvar input-form.stx pred-lhs.id pred-lhs.ots #t pred-rhs.stx))
		 (pred-lhs.des	(make-syntactic-binding-descriptor/lexical-typed-var/from-data pred-lhs.ots (qdef.lex pred-qdef)))
		 (lexenv.run	(push-entry-on-lexenv pred-lhs.lab pred-lhs.des lexenv.run)))
	    (extend-rib! rib pred-lhs.id pred-lhs.lab shadow/redefine-bindings?)
	    (values reference.entry
		    (cons pred-qdef rev-qdef*)
		    (cons* name-lhs.id pred-lhs.id kwd*)
		    lexenv.run lexenv.expand))))))

;;; --------------------------------------------------------------------

  (module (%establish-concrete-object-type-syntactic-binding)
    ;;Build  and  return  a  new  syntactic  binding's  descriptor  representing  the
    ;;definition of a new object-type specification.
    ;;
    ;;This new descriptor is meant to be inserted in the lexical environment in place
    ;;of a previously defined forward type definition.
    ;;
    (define* (%establish-concrete-object-type-syntactic-binding input-form.stx lexenv.run lexenv.expand
								type-name.id type-annotation.stx reference.entry
								rev-qdef* kwd* rib shadow/redefine-bindings?
								synner)
      (let ((new-type.des (%make-concrete-object-type-syntactic-binding-descriptor type-name.id type-annotation.stx
										   lexenv.run lexenv.expand synner)))
	(receive (rev-qdef* kwd* lexenv.run)
	    (%establish-concrete-type-predicate input-form.stx lexenv.run lexenv.expand
						reference.entry new-type.des
						rev-qdef* kwd* rib shadow/redefine-bindings?
						synner)
	  (%update-syntactic-binding! reference.entry new-type.des)
	  (%perform-validations reference.entry new-type.des lexenv.run (lambda (message)
									  (synner message type-annotation.stx)))
	  (values new-type.des rev-qdef* kwd* lexenv.run))))

;;; --------------------------------------------------------------------

    (define (%make-concrete-object-type-syntactic-binding-descriptor type-name.id type-annotation.stx lexenv.run lexenv.expand
								     synner)
      (syntax-match type-annotation.stx (constructor)
	((constructor ?constructor-expr)
	 (let* ((new.core	(expand-macro-transformer ?constructor-expr lexenv.expand))
		(new.ots	(compiler::eval-core (expanded->core new.core))))
	   (unless (object-type-spec? new.ots)
	     (assertion-violation __module_who__
	       "expected instance of <object-type-spec> as return value of type constructor expression"
	       type-annotation.stx new.ots))
	   ;;(make-syntactic-binding-descriptor/local-object-type-name new.ots new.core)
	   (make-syntactic-binding-descriptor/local-object-type-name new.ots (build-data no-source new.ots))))

	(_
	 (let* ((new.ots	(with-exception-handler
				    (lambda (E)
				      (if (dangling-reference-type-spec? E)
					  (synner (condition-message E)
						  (dangling-reference-type-spec.name E))
					(raise-continuable E)))
				  (lambda ()
				    (type-annotation->object-type-spec type-annotation.stx lexenv.run type-name.id))))
		(new.core	(build-data no-source new.ots)))
	   ;;(make-syntactic-binding-descriptor/local-object-type-name new.ots new.core)
	   (make-syntactic-binding-descriptor/local-object-type-name new.ots (build-data no-source new.ots))))
	))

;;; --------------------------------------------------------------------

    (define (%establish-concrete-type-predicate input-form.stx lexenv.run lexenv.expand
						reference.entry new-type.des
						rev-qdef* kwd* rib shadow/redefine-bindings?
						synner)
      ;;Every  type specification  must be  able to  generate, upon  request, a  type
      ;;predicate.  This function  establishes the concrete syntactic  binding of the
      ;;type predicate.
      ;;
      ;;For  the  "floating"  type  annotations  used in  binding  syntaxes:  a  type
      ;;predicate expression is generated and inserted in the code upon request.  For
      ;;example, in the form:
      ;;
      ;;   (let (({obj (list-of <fixnum>)} (read)))
      ;;     ---)
      ;;
      ;;the  return value  of "(read)"  is validated  at run-time;  a type  predicate
      ;;expression is generated  for "(list-of <fixnum>)" and inserted  in the output
      ;;code.
      ;;
      ;;For type annotations defined by DEFINE-TYPE:  we need a proper type predicate
      ;;definition, otherwise  we will  not be  able to  support recursive  types and
      ;;forward type definitions.  So DEFINE-TYPE must define a type predicate in the
      ;;form of a properly named function.  Let's consider:
      ;;
      ;;  (define-type <list-of-finxums>)
      ;;  (define-type <list-of-finxums>
      ;;    (list-of <fixnum>))
      ;;
      ;;we can imagine these for to expand into the following pseudo-code:
      ;;
      ;;  (define-type/forward-definition <list-of-fixnums>)
      ;;
      ;;  (define (#{forward-<list-of-fixnums>? |abcd|} obj)
      ;;    (#{concrete-<list-of-fixnums>? |efgh|} obj))
      ;;
      ;;  (define-type/concrete-definition <list-of-fixnums>
      ;;    (list-of <fixnum>))
      ;;
      ;;  (define #{concrete-<list-of-fixnums>? |efgh|}
      ;;    (letrec ((item-pred fixnum?)
      ;;             (type-pred (lambda (obj)
      ;;                          (if (pair? obj)
      ;;                              (and (item-pred (car obj))
      ;;                                   (type-pred (cdr obj)))
      ;;                            (null? obj)))))
      ;;      type-pred)
      ;;
      ;;In the case of struct-types,  record-types and label-types: the definition of
      ;;the  type predicate  may come  after the  DEFINE-TYPE form.   So we  wrap the
      ;;predicate into a lambda to avoid an illegal forward identifier reference.  We
      ;;can imagine the forms:
      ;;
      ;;   (define-type <duo>)
      ;;   (define-struct <duo> (one two))
      ;;
      ;;to expand to the following pseudo-code:
      ;;
      ;;  (define-type/forward-definition <duo>)
      ;;
      ;;  (define (#{forward-<duo>? |abcd|} obj)
      ;;    (#{concrete-<duo>? |efgh|} obj))
      ;;
      ;;  (define-type/concrete-definition <duo>
      ;;    (constructor (make-struct-type-spec ---)))
      ;;
      ;;  (define #{concrete-<duo>? |efgh|}
      ;;    (lambda (obj)
      ;;      (<duo>? obj)))
      ;;
      ;;  (define (<duo>? obj)
      ;;    ($struct/rtd? obj <duo>-std))
      ;;
      (let* ((new-type.ots	(syntactic-binding-descriptor/local-object-type.object-type-spec new-type.des))
	     (reference.des	(lexenv-entry.binding-descriptor reference.entry))
	     (reference.ots	(syntactic-binding-descriptor/local-object-type.object-type-spec reference.des))
	     (lhs.id		(reference-type-spec.predicate-id-concrete-definition reference.ots)))
	(when (bound-id-member? lhs.id kwd*)
	  (synner "cannot redefine keyword to concrete type-definition type-predicate" lhs.id))
	(let* ((lhs.lab		(generate-label-gensym lhs.id))
	       (rhs.stx		(let ((pred.stx	(object-type-spec.type-predicate-stx new-type.ots)))
				  (if (or (record-type-spec? new-type.ots)
					  (struct-type-spec? new-type.ots)
					  (label-type-spec?  new-type.ots))
				      (let ((obj.id (make-syntactic-identifier-for-temporary-variable "obj")))
					(bless
					 `(lambda/typed ({_ <boolean>} ,obj.id)
					    (,pred.stx ,obj.id))))
				    pred.stx)))
	       (lhs.ots		(make-type-predicate-spec))
	       (qdef		(make-qdef-typed-defvar input-form.stx lhs.id lhs.ots #t rhs.stx))
	       (lhs.des		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab lhs.des lexenv.run)))
	  ;;This  call  will raise  an  exception  if  it  represents an  attempt  to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values (cons qdef rev-qdef*) (cons lhs.id kwd*) lexenv.run))))

;;; --------------------------------------------------------------------

    (define (%perform-validations reference.entry new-type.des lexenv.run syn)
      ;;Let's do some validation.
      ;;
      (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	(let* ((new-type.ots	(syntactic-binding-descriptor/local-object-type.object-type-spec new-type.des))
	       (reference.des	(lexenv-entry.binding-descriptor reference.entry))
	       (reference.ots	(syntactic-binding-descriptor/local-object-type.object-type-spec reference.des)))
	  (define (%same? item.ots)
	    (or (eq? item.ots reference.ots)
		(eq? item.ots new-type.ots)
		(object-type-spec=? item.ots reference.ots)
		(object-type-spec=? item.ots new-type.ots)))
	  (cond ((union-type-spec? new-type.ots)
		 (for-each (lambda (item.ots)
			     (when (%same? item.ots)
			       (syn "invalid recursive type: union cannot hold the type itself")))
		   (union-type-spec.item-ots* new-type.ots)))

		((intersection-type-spec? new-type.ots)
		 (for-each (lambda (item.ots)
			     (when (%same? item.ots)
			       (syn "invalid recursive type: intersection cannot hold the type itself")))
		   (intersection-type-spec.item-ots* new-type.ots)))

		((complement-type-spec? new-type.ots)
		 (when (%same? (complement-type-spec.item-ots new-type.ots))
		   (syn "invalid recursive type: complement cannot hold the type itself")))

		((reference-type-spec? new-type.ots)
		 (when (object-type-spec=? reference.ots new-type.ots)
		   (syn "invalid recursive type: the type is an alias for itself")))))))

;;; --------------------------------------------------------------------

    (define* (%update-syntactic-binding! reference.entry new-type.des)
      ;;Register  the syntactic  binding descriptor  NEW-TYPE.DES as  the object-type
      ;;specification referenced by the lexenv entry REFERENCE.ENTRY.
      ;;
      (let* ((reference.des	(lexenv-entry.binding-descriptor reference.entry))
	     (reference.ots	(syntactic-binding-descriptor/local-object-type.object-type-spec reference.des))
	     (new-type.ots	(syntactic-binding-descriptor/local-object-type.object-type-spec new-type.des)))
	(reference-type-spec.object-type-spec-set! reference.ots   new-type.ots)
	(lexenv-entry.binding-descriptor-set!      reference.entry new-type.des)
	(void)))

    #| end of module: %ESTABLISH-CONCRETE-OBJECT-TYPE-SYNTACTIC-BINDING |# )

  #| end of module: CHI-DEFINE-TYPE |# )


;;;; parsing DEFINE forms

(module CHI-DEFINE
  (chi-define/std
   chi-define/typed
   chi-define/checked
   chi-case-define/std
   chi-case-define/typed
   chi-case-define/checked
   chi-define/overload)
  ;;The facilities of this module are used when the BODY-FORM.STX parsed by CHI-BODY*
  ;;is a  syntax object  representing the  macro use of  a DEFINE  variant; something
  ;;like:
  ;;
  ;;   (define ?lhs)
  ;;   (define ?lhs ?rhs)
  ;;   (define (?lhs . ?formals) . ?body)
  ;;
  ;;The functions  parse the body form  and build a qualified  right-hand side (QDEF)
  ;;object that will be expanded later.
  ;;
  ;;Here we  establish a new syntactic  binding representing lexical variable  in the
  ;;lexical context represented by the arguments RIB and LEXENV.RUN:
  ;;
  ;;* We generate a  label gensym uniquely associated to the  syntactic binding and a
  ;;lex gensym as name of the syntactic binding in the expanded code.
  ;;
  ;;* We register the association identifier/label in the rib.
  ;;
  ;;* We push an entry on LEXENV.RUN to represent the association label/lex.
  ;;
  ;;Finally we return the lex, the QDEF  and the updated LEXENV.RUN to recurse on the
  ;;rest of the body.
  ;;
  ;;
  ;;About the lex gensym
  ;;--------------------
  ;;
  ;;Notice that:
  ;;
  ;;* If the syntactic  binding is at the top-level of a program  or library body: we
  ;;need a  loc gensym to store  the result of evaluating  the RHS in QDEF;  this loc
  ;;will be generated later.
  ;;
  ;;* If the  syntactic binding is at  the top-level of an expression  expanded in an
  ;;interaction environment: we  need a loc gensym to store  the result of evaluating
  ;;the RHS  in QDEF; in  this case the  lex gensym generated  here also acts  as loc
  ;;gensym.
  ;;
  ;;
  ;;About redefining an existing syntactic binding
  ;;----------------------------------------------
  ;;
  ;;Special case:  if this  definition is  at the top-level  and a  syntactic binding
  ;;capturing the  syntactic identifier  ?LHS already  exists in the  top rib  of the
  ;;interaction environment: we  allow the binding to be redefined;  this is a Vicare
  ;;extension.   Normally,  in  a  non-interaction environment,  we  would  raise  an
  ;;exception as mandated by R6RS.
  ;;


(module DEFINE/CHECKED-HELPERS
  (%make-unsafe-name %make-unsafe-application-stx)

  (define (%make-unsafe-name safe-who.stx synner)
    ;;The argument  SAFE-WHO.STX is a  syntax object representing a  possibly typed
    ;;name for the safe function.  For example, it can be:
    ;;
    ;;   ?lhs
    ;;   (brace ?lhs ?rv-type0 ?rv-type ...)
    ;;   (brace ?lhs . ?rv-types)
    ;;
    ;;where ?LHS is the syntactic identifier representing the name of the function.
    ;;
    ;;This function builds and returns the following values:
    ;;
    ;;1. The  syntactic identifier  representing the name  of the  unsafe function.
    ;;This is the name of the safe function with a "~" prepended.
    ;;
    ;;2.   A syntax  object  representing  a possibly  typed  name  for the  unsafe
    ;;function.  This is the syntax object SAFE-WHO.STX with the safe name replaced
    ;;by the unsafe name.
    ;;
    (define (%mkname safe.id)
      (datum->syntax safe.id (string->symbol
			     (string-append "~" (symbol->string (syntax->datum safe.id))))))
    (syntax-match safe-who.stx (brace)
      (?name
       (identifier? ?name)
       (%mkname ?name))
      ((brace ?name . ?rv-types)
       (identifier? ?name)
       (%mkname ?name))
      (_
       (synner "invalid syntax in function definition formals, wrong function name specification" safe-who.stx))))

  (define (%make-unsafe-application-stx unsafe-name.id standard-formals.stx)
    ;;Build and return a syntax object representing the unsafe function application
    ;;in the body of the safe function.  For example, if the standard formals are a
    ;;proper list:
    ;;
    ;;   #'(a b c)
    ;;
    ;;the return value is:
    ;;
    ;;   (#'~fun #'a #'b #'c)
    ;;
    ;;otherwise, if the standard formals are an improper list:
    ;;
    ;;   #'(a b c . rest)
    ;;
    ;;the return value is:
    ;;
    ;;   (#'apply #'~fun #'a #'b #'c #'rest)
    ;;
    (if (list? standard-formals.stx)
	(cons unsafe-name.id standard-formals.stx)
      (cons* (core-prim-id 'apply) unsafe-name.id (%properise standard-formals.stx))))

  (define (%properise formals.stx)
    ;;Given  a syntax  object representing  an improper  list of  standard formals:
    ;;build  and return  a  syntax  object representing  a  proper  list of  formal
    ;;arguments.  Examples:
    ;;
    ;;   (%properise #'(a b . rest))	=> (#'a #'b #'rest)
    ;;   (%properise #'rest)		=> (#'rest)
    ;;
    (syntax-match formals.stx ()
      ((?car . ?cdr)
       (cons ?car (%properise ?cdr)))
      (_
       (list formals.stx))))

  #| end of module: DEFINE/CHECKED-HELPERS |# )


;;;; core macros: DEFINE/STD

(module (chi-define/std)
  ;;The  BODY-FORM.STX  parsed  by  CHI-BODY*  is  a  syntax  object  representing  a
  ;;DEFINE/STD core macro use; for example, one among:
  ;;
  ;;   (define/std ?lhs)
  ;;   (define/std ?lhs ?rhs)
  ;;   (define/std (?lhs . ?formals) . ?body)
  ;;
  ;;we parse  the form and  generate a qualified  right-hand side (QDEF)  object that
  ;;will be expanded later.  Here we establish a new syntactic binding representing a
  ;;fully R6RS compliant  lexical variable in the lexical context  represented by the
  ;;arguments RIB and LEXENV.RUN.
  ;;
  (define-module-who define/std)

  (define (chi-define/std input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (define-synner %synner __module_who__ input-form.stx)
    (receive (lhs.id lhs.ots qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the lexical variable's syntactic binding's identifier; LHS.OTS, an instance
	;;of "<object-type-spec>"  representing the type  of this binding;  QDEF, the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define* (%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? synner)
      ;;Syntax  parser for  Vicare's DEFINE/STD  syntax uses;  this is  like the
      ;;standard DEFINE described by R6RS.  Return the following values:
      ;;
      ;;1. The syntactic identifier of the lexical syntactic binding.
      ;;
      ;;2.  A syntactic  identifier representing the lexical variable's  type.  It is
      ;;false when no  type is specified.  It  is a sub-type of  "<procedure>" when a
      ;;procedure is defined.
      ;;
      ;;3.   An object  representing a  qualified right-hand  side expression  (QDEF)
      ;;fully representing the syntactic binding to create.
      ;;
      ;;4. A possibly updated LEXENV.RUN.
      ;;
      (syntax-match input-form.stx ()
	((_ (?lhs . ?formals) ?body0 ?body* ...)
	 (%process-function-definition input-form.stx lexenv.run
				       ?lhs ?formals `(,?body0 . ,?body*) synner))

	((_ ?lhs ?rhs)
	 (%process-variable-definition input-form.stx lexenv.run ?lhs #t ?rhs synner))

	((_ ?lhs)
	 (%process-variable-definition input-form.stx lexenv.run ?lhs #f #f synner))

	(_
	 (synner "invalid syntax"))))

    (define (%process-function-definition input-form.stx lexenv.run
					  lhs.id input-formals.stx body*.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as function name" lhs.id))
      ;;From parsing the standard formals we get  2 values: a proper or improper list
      ;;of   identifiers  representing   the   standard  formals;   an  instance   of
      ;;"<lambda-signature>".   An exception  is  raised if  an error  occurs
      ;;while parsing.
      (receive (standard-formals.stx clause-signature)
	  (syntax-object.parse-standard-clambda-clause-formals input-formals.stx)
	;;This is a standard variable syntactic  binding, so the arguments and return
	;;values are untyped.   However, we still want to generate  a typed syntactic
	;;binding so that we can validate the number of operands and return values.
	(let* ((lhs.ots	(make-closure-type-spec (make-case-lambda-signature (list clause-signature))))
	       (qdef	(make-qdef-standard-defun input-form.stx lhs.id standard-formals.stx body*.stx lhs.ots)))
	  (values lhs.id lhs.ots qdef lexenv.run))))

    (define (%process-variable-definition input-form.stx lexenv.run lhs.id init-expr? rhs.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      (let ((lhs.ots	(<top>-ots))
	    (qdef	(make-qdef-standard-defvar input-form.stx lhs.id init-expr? rhs.stx)))
	(values lhs.id lhs.ots qdef lexenv.run)))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-DEFINE/STD |# )


;;;; core macros: CASE-DEFINE/STD

(module (chi-case-define/std)

  (define-constant __module_who__ 'case-define/std)

  (define* (chi-case-define/std input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (define-synner %synner __module_who__ input-form.stx)
    (receive (lhs.id lhs.ots qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the lexical variable's syntactic binding's identifier; LHS.OTS, an instance
	;;of "<object-type-spec>"  representing the type  of this binding;  QDEF, the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx lexenv.run %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define (%parse-macro-use input-form.stx lexenv.run synner)
      (syntax-match input-form.stx ()
	((_ ?who ?cl-clause ?cl-clause* ...)
	 (begin
	   (unless (identifier? ?who)
	     (synner "expected identifier as function name" ?who))
	   (receive (standard-formals*.stx clause-signature* body**.stx)
	       (%parse-clauses input-form.stx (cons ?cl-clause ?cl-clause*) '() '() '())
	     ;;This is  a standard variable  syntactic binding, so the  arguments and
	     ;;return values are untyped.  However, we still want to generate a typed
	     ;;syntactic binding so  that we can validate the number  of operands and
	     ;;return values.
	     (let* ((lhs.ots	(make-closure-type-spec (make-case-lambda-signature clause-signature*)))
		    (qdef	(make-qdef-standard-case-defun input-form.stx ?who standard-formals*.stx body**.stx lhs.ots)))
	       (values ?who lhs.ots qdef lexenv.run)))))
	))

    (define (%parse-clauses input-form.stx clause*.stx
			    standard-formals*.stx clause-signature* body**.stx)
      ;;Recursive function.
      ;;
      (if (pair? clause*.stx)
	  (receive (standard-formals.stx clause-signature body*.stx)
	      (%parse-single-clause input-form.stx (car clause*.stx))
	    (receive (standard-formals*.stx clause-signature* body**.stx)
		(%parse-clauses input-form.stx (cdr clause*.stx) standard-formals*.stx clause-signature* body**.stx)
	      (values (cons standard-formals.stx	standard-formals*.stx)
		      (cons clause-signature		clause-signature*)
		      (cons body*.stx			body**.stx))))
	(values standard-formals*.stx clause-signature* body**.stx)))

    (define (%parse-single-clause input-form.stx clause.stx)
      (syntax-match clause.stx ()
	((?formals ?body ?body* ...)
	 ;;From parsing  the standard formals we  get 2 values: a  proper or improper
	 ;;list  of identifiers  representing the  standard formals;  an instance  of
	 ;;"<lambda-signature>".  An  exception is raised if  an error occurs
	 ;;while parsing.
	 (receive (standard-formals.stx clause-signature)
	     (syntax-object.parse-standard-clambda-clause-formals ?formals)
	   (values standard-formals.stx clause-signature (cons ?body ?body*))))
	))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-CASE-DEFINE/STD |# )


;;;; core macros: DEFINE/TYPED

(module (chi-define/typed)
  ;;The  BODY-FORM.STX  parsed  by  CHI-BODY*  is  a  syntax  object  representing  a
  ;;DEFINE/TYPED core macro use; for example, one among:
  ;;
  ;;   (define/typed ?lhs.id)
  ;;   (define/typed ?lhs.id ?rhs)
  ;;   (define/typed (?lhs.id . ?formals) . ?body)
  ;;   (define/typed (brace ?lhs.id ?lhs.type)
  ;;   (define/typed (brace ?lhs.id ?lhs.type) ?rhs)
  ;;   (define/typed ((brace ?lhs.id ?lhs.type) . ?formals) . ?body)
  ;;
  ;;we parse  the form and  generate a qualified  right-hand side (QDEF)  object that
  ;;will be expanded later.  Here we establish a new syntactic binding representing a
  ;;typed lexical  variable in the lexical  context represented by the  arguments RIB
  ;;and LEXENV.RUN.
  ;;
  (define-module-who define/typed)

  (define (chi-define/typed input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (define-synner %synner __module_who__ input-form.stx)
    (receive (lhs.id lhs.ots qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values:
	;;
	;;* LHS.ID: the lexical variable's syntactic binding's identifier.
	;;
	;;* LHS.OTS: when the type was specified, an instance of "<object-type-spec>"
	;;representing the type of this binding; otherwise false.
	;;
	;;* QDEF: the qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym lhs.id))
	       ;;It  is important,  here, to  generate an  untyped lexical  variable;
	       ;;later we will try to convert it into a typed lexical variable by RHS
	       ;;type propagation.
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define* (%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? synner)
      ;;Syntax  parser for  Vicare's DEFINE/TYPED  syntax uses;  this is  a like  the
      ;;standard DEFINE described  by R6RS but extended to  support type annotations.
      ;;Return the following values:
      ;;
      ;;1. The syntactic identifier of the lexical syntactic binding.
      ;;
      ;;2.  A syntactic  identifier representing the lexical variable's  type.  It is
      ;;false when no  type is specified.  It  is a sub-type of  "<procedure>" when a
      ;;procedure is defined.
      ;;
      ;;3.   An object  representing a  qualified right-hand  side expression  (QDEF)
      ;;fully representing the syntactic binding to create.
      ;;
      ;;4. A possibly updated LEXENV.RUN.
      ;;
      (syntax-match input-form.stx (brace)
	((_ ((brace ?lhs . ?rv-types) . ?formals) ?body0 ?body* ...)
	 (%process-function-definition input-form.stx lexenv.run
				       ?lhs `((,(brace-id) ,(underscore-id) . ,?rv-types) . ,?formals)
				       `(,?body0 . ,?body*) synner))

	((_ (brace ?lhs ?type) ?rhs)
	 (%process-variable-definition input-form.stx lexenv.run ?lhs ?type #t ?rhs synner))

	((_ (brace ?lhs ?type))
	 (%process-variable-definition input-form.stx lexenv.run ?lhs ?type #f #f synner))

	((_ (?lhs . ?formals) ?body0 ?body* ...)
	 (%process-function-definition input-form.stx lexenv.run
				       ?lhs ?formals `(,?body0 . ,?body*) synner))

	((_ ?lhs ?rhs)
	 (%process-variable-definition input-form.stx lexenv.run ?lhs #f #t ?rhs synner))

	((_ ?lhs)
	 (%process-variable-definition input-form.stx lexenv.run ?lhs #f #f #f synner))

	(_
	 (synner "invalid DEFINE/TYPED syntax use"))))

    (define (%process-function-definition input-form.stx lexenv.run
					  lhs.id input-formals.stx body*.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      ;;From parsing the typed formals we get  2 values: a proper or improper list of
      ;;identifiers   representing    the   standard   formals;   an    instance   of
      ;;"<lambda-signature>".   An exception  is  raised if  an error  occurs
      ;;while parsing.
      (receive (standard-formals.stx clause-signature)
	  ;;This call will use "<top>" for formals without type annotation.
	  (syntax-object.parse-typed-clambda-clause-formals input-formals.stx)
	(let* ((lhs.ots			(make-closure-type-spec (make-case-lambda-signature (list clause-signature))))
	       (qdef			(make-qdef-typed-defun input-form.stx lhs.id standard-formals.stx body*.stx lhs.ots)))
	  (values lhs.id lhs.ots qdef lexenv.run))))

    (define (%process-variable-definition input-form.stx lexenv.run
					  lhs.id lhs.type-ann init-expr? rhs.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      (let* ((lhs.ots	(if lhs.type-ann
			    (try
				(type-annotation->object-type-spec lhs.type-ann lexenv.run)
			      (catch E
				(else
				 (synner "invalid type annotation" lhs.type-ann))))
			  (<untyped>-ots)))
	     (qdef	(make-qdef-typed-defvar input-form.stx lhs.id lhs.ots init-expr? rhs.stx)))
	(values lhs.id lhs.ots qdef lexenv.run)))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-DEFINE/TYPED |# )


;;;; core macros: DEFINE/CHECKED

(module (chi-define/checked)
  ;;The  BODY-FORM.STX  parsed  by  CHI-BODY*  is  a  syntax  object  representing  a
  ;;DEFINE/CHECKED core macro use; for example, one among:
  ;;
  ;;   (define/checked ?lhs.id)
  ;;   (define/checked ?lhs.id ?rhs)
  ;;   (define/checked (?lhs.id . ?formals) . ?body)
  ;;   (define/checked (brace ?lhs.id ?lhs.type)
  ;;   (define/checked (brace ?lhs.id ?lhs.type) ?rhs)
  ;;   (define/checked ((brace ?lhs.id ?lhs.type) . ?formals) . ?body)
  ;;
  ;;we parse  the form and  generate a qualified  right-hand side (QDEF)  object that
  ;;will be expanded later.  Here we establish a new syntactic binding representing a
  ;;typed lexical  variable in the lexical  context represented by the  arguments RIB
  ;;and LEXENV.RUN.
  ;;
  ;;The cases of  typed functions are special  because we want to define  both a safe
  ;;variant and an unsafe variant.  We want to expand:
  ;;
  ;;   (define ({fun <fixnum>} {a <fixnum>})
  ;;     a)
  ;;
  ;;into an equivalent of:
  ;;
  ;;   (begin
  ;;     (define/checked ({fun  <fixnum>} {a <fixnum>})
  ;;       (~fun a))
  ;;     (define/typed   ({~fun <fixnum>} {a <fixnum>})
  ;;       a))
  ;;
  ;;and:
  ;;
  ;;   (define ({fun <fixnum>} {a <fixnum>} . {rest (list-of <fixnum>)})
  ;;     a)
  ;;
  ;;into an equivalent of:
  ;;
  ;;   (begin
  ;;     (define/checked ({fun  <fixnum>} {a <fixnum>} . {rest (list-of <fixnum>)})
  ;;       (apply ~fun a rest))
  ;;     (define/typed   ({~fun <fixnum>} {a <fixnum>} . {rest (list-of <fixnum>)})
  ;;       a))
  ;;
  ;;Functions  defined  with  DEFINE/CHECKED:  will  check  their  operands  both  at
  ;;expand-time and at  run-time; they will check their return  values at expand-time
  ;;and, only  when needed, at  run-time.  Functions defined with  DEFINE/TYPED: will
  ;;check their  operands at expand-time and  not at run-time; they  will check their
  ;;return values at expand-time and, only when needed, at run-time.
  ;;
  (define-module-who define/checked)

  (define (chi-define/checked input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    ;;Return the following values:
    ;;
    ;;1.  An object representing a  qualified right-hand side expression (QDEF) fully
    ;;representing the syntactic binding to create.
    ;;
    ;;2. A possibly updated LEXENV.RUN.
    ;;
    (define-synner %synner __module_who__ input-form.stx)
    (syntax-match input-form.stx (brace)
      ((_ (brace ?lhs ?type) ?rhs)
       (%process-variable-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
				     ?lhs ?type #t ?rhs %synner))

      ((_ (brace ?lhs ?type))
       (%process-variable-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
				     ?lhs ?type #f #f %synner))

      ((_ ((brace ?lhs . ?rv-types) . ?formals) ?body0 ?body* ...)
       (receive (standard-formals.stx argvals.sig)
	   ;;This call will use "<top>" for untyped arguments.
	   (syntax-object.parse-typed-formals ?formals)
	 (let* ((retvals.sig		(let ((synner (lambda (message subform)
							(syntax-violation __module_who__ message input-form.stx subform))))
					  (make-type-signature (syntax-object->type-signature-specs ?rv-types lexenv.run synner))))
		(clause-signature	(make-lambda-signature retvals.sig argvals.sig)))
	   (%process-function-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					 ?lhs `(,(brace-id) ,?lhs . ,?rv-types) standard-formals.stx clause-signature
					 `(,?body0 . ,?body*) %synner))))

      ((_ (?lhs . ?formals) ?body0 ?body* ...)
       (receive (standard-formals.stx argvals.sig)
	   ;;This call will use "<top>" for untyped arguments.
	   (syntax-object.parse-typed-formals ?formals)
	 (let ((clause-signature (make-lambda-signature (make-type-signature/fully-unspecified) argvals.sig)))
	   (%process-function-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					 ?lhs ?lhs standard-formals.stx clause-signature
					 `(,?body0 . ,?body*) %synner))))

      ((_ ?lhs ?rhs)
       (%process-variable-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
				     ?lhs #f #t ?rhs %synner))

      ((_ ?lhs)
       (%process-variable-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
				     ?lhs #f #f #f %synner))

      (_
       (%synner "invalid DEFINE/CHECKED syntax use"))))

  (define (%process-variable-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					lhs.id lhs.type-ann init-expr? rhs.stx synner)
    (unless (identifier? lhs.id)
      (synner "expected identifier as variable name" lhs.id))
    (when (bound-id-member? lhs.id kwd*)
      (synner "cannot redefine keyword"))
    (let* ((lhs.ots	(if lhs.type-ann
			    (try
				(type-annotation->object-type-spec lhs.type-ann lexenv.run)
			      (catch E
				(else
				 (synner "invalid type annotation" lhs.type-ann))))
			  (<untyped>-ots)))
	   (qdef	(make-qdef-checked-defvar input-form.stx lhs.id lhs.ots init-expr? rhs.stx))
	   (lhs.lab	(generate-label-gensym lhs.id))
	   (lhs.descr	(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots (qdef.lex qdef)))
	   (lexenv.run	(push-entry-on-lexenv lhs.lab lhs.descr lexenv.run)))
      ;;This rib  extension will raise  an exception if  it represents an  attempt to
      ;;illegally redefine a binding.
      (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
      (values (list qdef) lexenv.run)))

  (module (%process-function-definition)
    (import DEFINE/CHECKED-HELPERS)

    (define (%process-function-definition input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					  lhs.id safe-who.stx standard-formals.stx clause-signature
					  body*.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      (if (type-signature.only-<untyped>-and-<top>-and-<list>? (lambda-signature.argvals clause-signature))
	  ;;Only generic type annotations: there is no need to validate the operands,
	  ;;so generate a single checked function.
	  (receive (qdef lexenv.run)
	      (%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
				  make-qdef-checked-defun
				  lhs.id standard-formals.stx clause-signature
				  body*.stx #f synner)
	    (values (list qdef) lexenv.run))
	;;Type arguments are present.  Generate two functions: the safe (which checks
	;;the  arguments  also at  run-time)  and  the  unsafe (which  validates  the
	;;arguments only at expand-time).
	(let* ((unsafe-name.id	(%make-unsafe-name safe-who.stx synner))
	       (safe-body*.stx	(list (%make-unsafe-application-stx unsafe-name.id standard-formals.stx))))
	  (let*-values
	      (((qdef-safe   lexenv.run)	(%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
								    make-qdef-checked-defun
								    lhs.id standard-formals.stx clause-signature
								    safe-body*.stx (vector unsafe-name.id) synner))
	       ((qdef-unsafe lexenv.run)	(%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
								    make-qdef-typed-defun
								    unsafe-name.id standard-formals.stx clause-signature
								    body*.stx #f synner)))
	    ;;If we put QDEF-UNSAFE first  and QDEF-SAFE last: type propagation works
	    ;;better.
	    (values (list qdef-unsafe qdef-safe) lexenv.run)))))

    (define* (%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
				 qdef-maker
				 lhs.id standard-formals.stx clause-signature body*.stx
				 {replacements (or not vector?)} synner)
      (when (bound-id-member? lhs.id kwd*)
	(synner "cannot redefine keyword"))
      (let* ((lhs.ots		(make-closure-type-spec (make-case-lambda-signature (list clause-signature))))
	     (qdef		(qdef-maker input-form.stx lhs.id standard-formals.stx body*.stx lhs.ots))
	     (lhs.lab		(generate-label-gensym lhs.id))
	     (lhs.descr		(make-syntactic-binding-descriptor/lexical-closure-var/from-data
				 lhs.ots (qdef.lex qdef) replacements))
	     (lexenv.run	(push-entry-on-lexenv lhs.lab lhs.descr lexenv.run)))
	;;This rib extension  will raise an exception if it  represents an attempt to
	;;illegally redefine a binding.
	(extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	(values qdef lexenv.run)))

    #| end of module: %PROCESS-FUNCTION-DEFINITION |# )

  #| end of module: CHI-DEFINE/CHECKED |# )


;;;; core macros: CASE-DEFINE/TYPED

(module (chi-case-define/typed)

  (define-constant __module_who__ 'case-define/typed)

  (define* (chi-case-define/typed input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (define-synner %synner __module_who__ input-form.stx)
    (receive (lhs.id lhs.ots qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the lexical variable's syntactic  binding's identifier; LHS.OTS an instance
	;;of "<object-type-spec>"  representing the type  of this binding;  QDEF, the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx lexenv.run %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.ots (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define (%parse-macro-use input-form.stx lexenv.run synner)
      (syntax-match input-form.stx ()
	((_ ?who ?cl-clause ?cl-clause* ...)
	 (begin
	   (unless (identifier? ?who)
	     (synner "expected identifier as function name" ?who))
	   (receive (standard-formals*.stx clause-signature* body**.stx)
	       (%parse-clauses (cons ?cl-clause ?cl-clause*) '() '() '())
	     (let* ((lhs.ots	(make-closure-type-spec (make-case-lambda-signature clause-signature*)))
		    (qdef-safe	(make-qdef-typed-case-defun input-form.stx ?who standard-formals*.stx body**.stx lhs.ots)))
		 (values ?who lhs.ots qdef-safe lexenv.run)))))
	))

    (define (%parse-clauses clause*.stx standard-formals*.stx clause-signature* body**.stx)
      ;;Recursive function.
      ;;
      (if (pair? clause*.stx)
	  (receive (standard-formals.stx clause-signature body*.stx)
	      (%parse-single-clause (car clause*.stx))
	    (receive (standard-formals*.stx clause-signature* body**.stx)
		(%parse-clauses (cdr clause*.stx) standard-formals*.stx clause-signature* body**.stx)
	      (values (cons standard-formals.stx	standard-formals*.stx)
		      (cons clause-signature		clause-signature*)
		      (cons body*.stx			body**.stx))))
	(values standard-formals*.stx clause-signature* body**.stx)))

    (define (%parse-single-clause clause.stx)
      (syntax-match clause.stx ()
	((?formals ?body ?body* ...)
	 ;;From parsing  the standard formals we  get 2 values: a  proper or improper
	 ;;list  of identifiers  representing the  standard formals;  an instance  of
	 ;;"<lambda-signature>".  An  exception is raised if  an error occurs
	 ;;while parsing.
	 (receive (standard-formals.stx clause-signature)
	     ;;This call will use "<top>" for formals without type annotation.
	     (syntax-object.parse-typed-clambda-clause-formals ?formals)
	   (values standard-formals.stx clause-signature (cons ?body ?body*))))
	))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-CASE-DEFINE/TYPED |# )


;;;; core macros: CASE-DEFINE/CHECKED

(module (chi-case-define/checked)
  (import DEFINE/CHECKED-HELPERS)
  (define-constant __module_who__ 'case-define/checked)

  (define* (chi-case-define/checked input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (define-synner %synner __module_who__ input-form.stx)
    (syntax-match input-form.stx ()
      ((_ ?who ?cl-clause ?cl-clause* ...)
       (begin
	 (unless (identifier? ?who)
	   (%synner "expected identifier as function name" ?who))
	 (receive (standard-formals*.stx clause-signature* body**.stx)
	     (%parse-clauses input-form.stx (cons ?cl-clause ?cl-clause*) '() '() '())
	   (if (for-all (lambda (clause-signature)
			  (type-signature.only-<untyped>-and-<top>-and-<list>? (lambda-signature.argvals clause-signature)))
		 clause-signature*)
	       ;;Only generic type annotations:  no operands validation is necessary,
	       ;;so generate a single checked function.
	       (let ((lhs.ots (make-closure-type-spec (make-case-lambda-signature clause-signature*))))
		 (receive (qdef lexenv.run)
		     (%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					 make-qdef-checked-case-defun
					 ?who lhs.ots standard-formals*.stx body**.stx
					 #f %synner)
		   (values (list qdef) lexenv.run)))
	     ;;Type arguments are  present.  Generate two functions:  the safe (which
	     ;;checks the arguments also at run-time) and the unsafe (which validates
	     ;;the arguments only at expand-time).
	     (let* ((lhs.ots		(make-closure-type-spec (make-case-lambda-signature clause-signature*)))
		    (unsafe-name.id	(%make-unsafe-name ?who %synner))
		    (safe-body**.stx	(map (lambda (standard-formals.stx)
					       (list (%make-unsafe-application-stx unsafe-name.id standard-formals.stx)))
					  standard-formals*.stx)))
	       (let*-values
		   (((safe-qdef lexenv.run)
		     (%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					 make-qdef-checked-case-defun
					 ?who       lhs.ots standard-formals*.stx safe-body**.stx
					 (vector unsafe-name.id) %synner))
		    ((unsafe-qdef lexenv.run)
		     (%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
					 make-qdef-typed-case-defun
					 unsafe-name.id lhs.ots standard-formals*.stx body**.stx
					 #f %synner)))
(debug-print ?who unsafe-name.id)
		 ;;If  we put  first the  UNSAFE-QDEF  and last  the SAFE-QDEF:  type
		 ;;propagation will work better.
		 (values (list unsafe-qdef safe-qdef) lexenv.run)))))))
      ))

  (define (%generate-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings? qdef-maker
			      lhs.id lhs.ots standard-formals*.stx body**.stx replacements %synner)
    (if (bound-id-member? lhs.id kwd*)
	(%synner "cannot redefine keyword")
      (let* ((qdef		(qdef-maker input-form.stx lhs.id standard-formals*.stx body**.stx lhs.ots))
	     (lhs.lab		(generate-label-gensym lhs.id))
	     (descr		(make-syntactic-binding-descriptor/lexical-closure-var/from-data
				 lhs.ots (qdef.lex qdef) replacements))
	     (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	;;This rib extension  will raise an exception if it  represents an attempt to
	;;illegally redefine a binding.
	(extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	(values qdef lexenv.run))))

  (define (%parse-clauses input-form.stx clause*.stx
			  standard-formals*.stx clause-signature* body**.stx)
    ;;Recursive function.
    ;;
    (if (pair? clause*.stx)
	(receive (standard-formals.stx clause-signature body*.stx)
	    (%parse-single-clause input-form.stx (car clause*.stx))
	  (receive (standard-formals*.stx clause-signature* body**.stx)
	      (%parse-clauses input-form.stx (cdr clause*.stx) standard-formals*.stx clause-signature* body**.stx)
	    (values (cons standard-formals.stx	standard-formals*.stx)
		    (cons clause-signature	clause-signature*)
		    (cons body*.stx		body**.stx))))
      (values standard-formals*.stx clause-signature* body**.stx)))

  (define (%parse-single-clause input-form.stx clause.stx)
    (syntax-match clause.stx ()
      ((?formals ?body ?body* ...)
       ;;From parsing the standard formals we get 2 values: a proper or improper list
       ;;of   identifiers  representing   the  standard   formals;  an   instance  of
       ;;"<lambda-signature>".   An exception  is  raised if  an  error occurs  while
       ;;parsing.
       (receive (standard-formals.stx clause-signature)
	   ;;This call will use "<top>" for formals without type annotation.
	   (syntax-object.parse-typed-clambda-clause-formals ?formals)
	 (values standard-formals.stx clause-signature (cons ?body ?body*))))
      ))

  #| end of module: CHI-CASE-DEFINE/CHECKED |# )


;;;; core macros: DEFINE/OVERLOAD

(module (chi-define/overload)
  ;;The  BODY-FORM.STX  parsed  by  CHI-BODY*  is  a  syntax  object  representing  a
  ;;DEFINE/OVERLOAD core macro use; one among:
  ;;
  ;;   (define/overload (?lhs.id . ?formals) . ?body)
  ;;   (define/overload ((brace ?lhs.id ?lhs.type) . ?formals) . ?body)
  ;;
  ;;When ?LHS.ID  is not  bound in  the local lexical  context rib:  we define  a new
  ;;overloaded function; otherwise we extend the existent one.
  ;;
  ;;We parse the form and generate  two qualified right-hand side (QDEF) objects that
  ;;will  be expanded  later; these  QDEFs represent  a specialised  function in  the
  ;;overloaded function.  Here we establish:
  ;;
  ;;* A  new syntactic binding representing  a typed lexical variable  in the lexical
  ;;context represented  by the arguments  RIB and  LEXENV.RUN; this variable  is the
  ;;specialised function.
  ;;
  ;;* When defining  a new overloaded function: a new  syntactic binding representing
  ;;the  overloaded function.   This  is similar  to  a macro,  but  its handling  is
  ;;integrated in the expander.
  ;;
  ;;* When defining  a new overloaded function: a new  syntactic binding representing
  ;;the overloaded function descriptor, used for late binding.
  ;;
  ;;* When extending  an existent overloaded function: a new  dummy syntactic binding
  ;;representing  the registration  of  the specialised  function  in the  overloaded
  ;;function descriptor.
  ;;
  (define-module-who define/overload)

  (define (chi-define/overload input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (define-synner %synner __module_who__ input-form.stx)
    (receive (lhs.id input-formals.stx body*.stx)
	;;From parsing the  syntactic form, we receive the  following values:
	;;
	;;* LHS.ID: the overload function's syntactic binding's identifier.
	;;
	;;*  INPUT-FORMALS.STX:  a syntax  object  representing  the formals  of  the
	;;specialised function.
	;;
	;;* BODY*.STX:  a list of syntax  objects representing the body  forms of the
	;;specialised function.
	(%parse-macro-use input-form.stx %synner)
      (cond ((id->label/local lhs.id rib)
	     ;;A syntactic binding with identifier LHS.ID already exists.
	     => (lambda (lhs.lab)
		  (let ((lhs.des (label->syntactic-binding-descriptor lhs.lab lexenv.run)))
		    (case (syntactic-binding-descriptor.type lhs.des)
		      ((local-overloaded-function)
		       ;;Let's  extend the  existent overloaded  function with  a new
		       ;;specialised function.
		       (let ((lhs.ofs (syntactic-binding-descriptor.value lhs.des)))
			 (%extend-existent-overloaded-function input-form.stx rib lexenv.run shadow/redefine-bindings?
							       lhs.id lhs.ofs input-formals.stx body*.stx
							       %synner)))
		      ((global-overloaded-function)
		       (let ((lhs.ofs (syntactic-binding-descriptor/global-overloaded-function.ofs lhs.des)))
			 (%extend-existent-overloaded-function input-form.stx rib lexenv.run shadow/redefine-bindings?
							       lhs.id lhs.ofs input-formals.stx body*.stx
							       %synner)))
		      (else
		       ;;The syntactic binding is not an overloaded function.
		       (%synner "cannot redefine keyword" lhs.id))))))
	    (else
	     ;;No syntactic  binding with identifier  LHS.ID exists.  Let's  define a
	     ;;new overloaded function.
	     (%establish-new-overloaded-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
						 lhs.id input-formals.stx body*.stx
						 %synner)))))

  (define* (%parse-macro-use input-form.stx synner)
    ;;Syntax parser for Vicare's DEFINE/OVERLOAD syntax uses.
    ;;
    (syntax-match input-form.stx (brace)
      ((_ ((brace ?lhs . ?rv-types) . ?formals) ?body0 ?body* ...)
       (begin
	 (unless (identifier? ?lhs)
	   (synner "expected identifiers as overloaded function name" ?lhs))
	 (values ?lhs
		 `((,(brace-id) ,(underscore-id) . ,?rv-types) . ,?formals) ;input-formals.stx
		 `(,?body0 . ,?body*))))

      ((_ (?lhs . ?formals) ?body0 ?body* ...)
       (begin
	 (unless (identifier? ?lhs)
	   (synner "expected identifiers as overloaded function name" ?lhs))
	 (values ?lhs ?formals `(,?body0 . ,?body*))))

      (_
       (synner "invalid DEFINE/OVERLOAD syntax use"))))

;;; --------------------------------------------------------------------

  (module (%establish-new-overloaded-function)

    (define (%establish-new-overloaded-function input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?
						lhs.id input-formals.stx body*.stx
						synner)
      (receive (spec.id spec.lambda-sig spec.qdef lexenv.run)
	  ;;Establish the syntactic binding of the specialised function.
	  (%establish-specialised-implementation input-form.stx rib lexenv.run shadow/redefine-bindings?
						 lhs.id input-formals.stx body*.stx)
	;;Establish the syntactic binding of the overloaded function.
	(let* ((lhs.sym		(syntax->datum lhs.id))
	       ;;OFD.ID is the syntactic  identifier bound to the Overloaded-Function
	       ;;Descriptor used at run-time for late binding.
	       (ofd.id		(datum->syntax lhs.id (gensym lhs.sym)))
	       ;;LBF.ID  is  the  syntactic  identifier bound  to  the  Late  Binding
	       ;;Function  used at  run-time for  late binding,  when the  overloaded
	       ;;function is referenced.
	       (lbf.id		(datum->syntax lhs.id (gensym lhs.sym)))
	       (lhs.ofs		(make-overloaded-function-spec lhs.id ofd.id lbf.id))
	       (lhs.lab		(generate-label-gensym lhs.id))
	       (lhs.des		(make-syntactic-binding-descriptor/overloaded-function/from-data lhs.ofs))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab lhs.des lexenv.run)))
	  ;;Register the specialised  function in the overloaded  function.  This may
	  ;;fail raising an expand-time exception.
	  (overloaded-function-spec.add-specialised-implementation! input-form.stx lhs.ofs spec.lambda-sig spec.id)
	  ;;Generate the  visit code that  registers the specialised function  in the
	  ;;overloaded function.
	  (let ((lexenv.run (%generate-registration-visit-code lhs.id spec.id spec.lambda-sig lexenv.run)))
	    ;;This rib extension will raise an  exception if it represents an attempt
	    ;;to illegally redefine a binding.
	    (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	    (receive (ofd.qdef lexenv.run)
		(%establish-overloaded-function-descriptor ofd.id spec.id spec.lambda-sig rib lexenv.run kwd* shadow/redefine-bindings?)
	      (receive (lbf.qdef lexenv.run)
		  (%establish-overloaded-function-late-binding-function lbf.id ofd.id rib lexenv.run kwd* shadow/redefine-bindings?)
		(values (list spec.qdef ofd.qdef lbf.qdef) lexenv.run)))))))

    (define (%establish-overloaded-function-descriptor ofd.id spec.id spec.lambda-sig rib lexenv.run kwd* shadow/redefine-bindings?)
      (let* ((spec.ann		(object-type-spec.type-annotation
				 (make-closure-type-spec (make-case-lambda-signature (list spec.lambda-sig)))))
	     (input-form.stx	(bless `(define/typed {,ofd.id <overloaded-function-descriptor>}
					  (make-overloaded-function-descriptor
					   (quote ,ofd.id)
					   (list (cons (car (case-lambda-descriptors.clause-signature*
							     (closure-type-descr.signature (type-descriptor ,spec.ann))))
						       ,spec.id)))))))
	(chi-define/typed input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)))

    (define (%establish-overloaded-function-late-binding-function lbf.id ofd.id rib lexenv.run kwd* shadow/redefine-bindings?)
      (let* ((args.id		(make-syntactic-identifier-for-temporary-variable "args"))
	     (input-form.stx	(bless
				 `(define/std (,lbf.id . ,args.id)
				    (apply overloaded-function-late-binding ,ofd.id ,args.id)))))
	(chi-define/std input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)))

    #| end of module: %ESTABLISH-NEW-OVERLOADED-FUNCTION |# )

;;; --------------------------------------------------------------------

  (module (%extend-existent-overloaded-function)

    (define (%extend-existent-overloaded-function input-form.stx rib lexenv.run shadow/redefine-bindings?
						  lhs.id lhs.ofs input-formals.stx body*.stx
						  synner)
      (receive (spec.id spec.lambda-sig spec.qdef lexenv.run)
	  ;;Establish the syntactic binding of the specialised function.
	  (%establish-specialised-implementation input-form.stx rib lexenv.run shadow/redefine-bindings?
						 lhs.id input-formals.stx body*.stx)
	;;Register the  specialised function  in the  overloaded function.   This may
	;;fail raising an expand-time exception.
	(overloaded-function-spec.add-specialised-implementation! input-form.stx lhs.ofs spec.lambda-sig spec.id)
	;;Generate  the visit  code that  registers the  specialised function  in the
	;;expand-time overloaded function specification (OFS).
	(let ((lexenv.run (%generate-registration-visit-code lhs.id spec.id spec.lambda-sig lexenv.run)))
	  ;;Generate the init  expression that registers the  specialised function in
	  ;;the run-time overloaded function descriptor (OFD).
	  (let ((regis.qdef (%generate-registration-invoke-code lhs.ofs spec.id spec.lambda-sig)))
	    (values (list spec.qdef regis.qdef) lexenv.run)))))

    (define* (%generate-registration-invoke-code {lhs.ofs		overloaded-function-spec?}
						 {spec.id		identifier?}
						 {spec.lambda-sig	lambda-signature?})
      (let* ((spec.ann	(object-type-spec.type-annotation
			 (make-closure-type-spec (make-case-lambda-signature (list spec.lambda-sig)))))
	     (ofd.id	(overloaded-function-spec.ofd-id lhs.ofs))
	     (regis.stx	(bless `(overloaded-function-descriptor.register!
				 ,ofd.id
				 (car (case-lambda-descriptors.clause-signature*
				       (closure-type-descr.signature (type-descriptor ,spec.ann))))
				 ,spec.id))))
	(make-qdef-top-expr regis.stx)))

    #| end of module: %EXTEND-EXISTENT-OVERLOADED-FUNCTION |# )

;;; --------------------------------------------------------------------

  (define (%establish-specialised-implementation input-form.stx rib lexenv.run shadow/redefine-bindings?
						 lhs.id input-formals.stx body*.stx)
    ;;From parsing the  typed formals we get  2 values: a proper or  improper list of
    ;;identifiers    representing   the    standard   formals;    an   instance    of
    ;;"<lambda-signature>".  An exception is raised if an error occurs while parsing.
    (receive (standard-formals.stx spec.lambda-sig)
	;;This call will use "<top>" for formals without type annotation.
	(syntax-object.parse-typed-clambda-clause-formals input-formals.stx)
      (let* ((spec.id		(datum->syntax lhs.id (gensym (syntax->datum lhs.id))))
	     (spec.ots		(make-closure-type-spec (make-case-lambda-signature (list spec.lambda-sig))))
	     (spec.qdef		(make-qdef-typed-defun input-form.stx spec.id standard-formals.stx body*.stx spec.ots))
	     (spec.lab		(generate-label-gensym spec.id))
	     (spec.des		(make-syntactic-binding-descriptor/lexical-typed-var/from-data spec.ots (qdef.lex spec.qdef)))
	     (lexenv.run	(push-entry-on-lexenv spec.lab spec.des lexenv.run)))
	;;This rib extension  will raise an exception if it  represents an attempt to
	;;illegally redefine a binding.
	(extend-rib! rib spec.id spec.lab shadow/redefine-bindings?)
	(values spec.id spec.lambda-sig spec.qdef lexenv.run))))

;;; --------------------------------------------------------------------

  (define (%generate-registration-visit-code lhs.id spec.id spec.lambda-sig lexenv.run)
    (let* ((regis.des	(make-syntactic-binding-descriptor/begin-for-syntax
			 (build-application no-source
			     (build-primref no-source 'overloaded-function-spec.register-specialisation!)
			   (list (build-data no-source lhs.id)
				 (build-data no-source spec.id)
				 (build-data no-source spec.lambda-sig)))))
	   (regis.lab	(generate-label-gensym spec.id))
	   (lexenv.run	(push-entry-on-lexenv regis.lab regis.des lexenv.run)))
      lexenv.run))

  #| end of module: CHI-DEFINE/OVERLOAD |# )


;;;; parsing DEFINE forms: end of module

#| end of module: CHI-DEFINE |# )


;;;; done

#| end of module: CHI-BODY* |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
