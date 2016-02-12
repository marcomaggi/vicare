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


(define* (chi-body* body-form*.stx lexenv.run lexenv.expand rev-qdef* mod** kwd* export-spec* rib
		    mix? shadow/redefine-bindings?)
  (import CHI-DEFINE)
  (if (pair? body-form*.stx)
      (let*-values
	  (((body-form.stx)	(car body-form*.stx))
	   ((type descr kwd)	(syntactic-form-type __who__ body-form.stx lexenv.run)))

	(define keyword*
	  (if (identifier? kwd)
	      (cons kwd kwd*)
	    kwd*))

	(define-syntax-rule (%expand-internal-definition ?expander-who)
	  ;;Used  to  process an  internal  definition  core macro:  DEFINE/STANDARD,
	  ;;DEFINE/TYPED, CASE-DEFINE/STANDARD, CASE-DEFINE/TYPED.  We parse the body
	  ;;form and generate a qualified right-hand  side (QDEF) object that will be
	  ;;expanded later.
	  ;;
	  (receive (qdef lexenv.run)
	      (?expander-who body-form.stx rib lexenv.run keyword* shadow/redefine-bindings?)
	    (chi-body* (cdr body-form*.stx)
		       lexenv.run lexenv.expand
		       (cons qdef rev-qdef*)
		       mod** keyword* export-spec* rib mix? shadow/redefine-bindings?)))

	(parametrise ((current-run-lexenv (lambda () lexenv.run)))
	  (case type

	    ((define/standard)
	     ;;The body form is a DEFINE/STANDARD core macro use, one among:
	     ;;
	     ;;   (define/standard ?lhs ?rhs)
	     ;;   (define/standard (?lhs . ?formals) . ?body)
	     ;;
	     ;;it is meant to be the implementation of R6RS's DEFINE syntax.
	     (%expand-internal-definition chi-define/standard))

	    ((case-define/standard)
	     ;;The body form is a CASE-DEFINE/STANDARD core macro use:
	     ;;
	     ;;   (case-define/standard ?lhs ?case-lambda-clause ...)
	     ;;
	     ;;it is meant to be equivalent to:
	     ;;
	     ;;   (define/standard ?lhs (case-lambda/standard ?case-lambda-clause ...))
	     ;;
	     (%expand-internal-definition chi-case-define/standard))

	    ((define/typed)
	     ;;The body form is a DEFINE/TYPED core macro use, one among:
	     ;;
	     ;;   (define/typed ?lhs ?rhs)
	     ;;   (define/typed (?lhs . ?formals) . ?body)
	     ;;
	     ;;it is meant to be an extension to R6RS's DEFINE syntax supporting type
	     ;;specifications.
	     (%expand-internal-definition chi-define/typed))

	    ((case-define/typed)
	     ;;The body form is a CASE-DEFINE/TYPED core macro use:
	     ;;
	     ;;   (case-define/typed ?lhs ?case-lambda-clause ...)
	     ;;
	     ;;it  is meant  to be  an extension  to the  CASE-DEFINE/STANDARD syntax
	     ;;supporting type specifications.
	     (%expand-internal-definition chi-case-define/typed))

	    ((define-syntax)
	     ;;The body  form is a  built-in DEFINE-SYNTAX  macro use.  This  is what
	     ;;happens:
	     ;;
	     ;;1. Parse the syntactic form.
	     ;;
	     ;;2. Expand the right-hand side expression.  This expansion happens in a
	     ;;lexical context in which the syntactic binding of the macro itself has
	     ;;*not* yet been established.
	     ;;
	     ;;3. Add  to the rib  the syntactic binding's association  between the
	     ;;macro keyword and the label.
	     ;;
	     ;;4.   Evaluate the  right-hand  side expression,  which  is meant  to
	     ;;return:  a  macro  transformer  function, a  compile-time  value,  a
	     ;;synonym transformer or whatever.
	     ;;
	     ;;5. Add to the lexenv the syntactic binding's association between the
	     ;;label and the binding's descriptor.
	     ;;
	     ;;6. Finally recurse to expand the rest of the body.
	     ;;
	     (receive (id rhs.stx)
		 (%parse-define-syntax body-form.stx)
	       (when (bound-id-member? id keyword*)
		 (stx-error body-form.stx "cannot redefine keyword"))
	       (let ((rhs.core (expand-macro-transformer rhs.stx lexenv.expand))
		     (lab      (generate-label-gensym id)))
		 ;;This call will raise an exception if it represents an attempt to
		 ;;illegally redefine a binding.
		 (extend-rib! rib id lab shadow/redefine-bindings?)
		 (let ((entry (cons lab (eval-macro-transformer rhs.core lexenv.run))))
		   (chi-body* (cdr body-form*.stx)
			      (cons entry lexenv.run)
			      (cons entry lexenv.expand)
			      rev-qdef* mod** keyword* export-spec* rib
			      mix? shadow/redefine-bindings?)))))

	    ((define-fluid-syntax)
	     ;;The body  form is a  built-in DEFINE-FLUID-SYNTAX macro use.   This is
	     ;;what happens:
	     ;;
	     ;;1. Parse the syntactic form.
	     ;;
	     ;;2. Expand the right-hand side expression.  This expansion happens in a
	     ;;lexical context in which the syntactic binding of the macro itself has
	     ;;*not* yet been established.
	     ;;
	     ;;3.  Add to  the rib  the syntactic  binding's association  between the
	     ;;macro keyword and the label.
	     ;;
	     ;;4.  Evaluate the  right-hand side expression (which  usually returns a
	     ;;macro transformer function).
	     ;;
	     ;;5. Add to  the lexenv the syntactic binding's  association between the
	     ;;label and the binding's descriptor.
	     ;;
	     ;;6. Finally recurse to expand the rest of the body.
	     ;;
	     (receive (lhs.id rhs.stx)
		 (%parse-define-syntax body-form.stx)
	       (when (bound-id-member? lhs.id keyword*)
		 (stx-error body-form.stx "cannot redefine keyword"))
	       (let ((rhs.core (expand-macro-transformer rhs.stx lexenv.expand))
		     (lhs.lab  (generate-label-gensym lhs.id)))
		 ;;This call will  raise an exception if it represents  an attempt to
		 ;;illegally redefine a binding.
		 (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
		 ;;The LEXENV  entry KEYWORD-ENTRY  represents the definition  of the
		 ;;fluid  syntax's  keyword  syntactic  binding.   The  LEXENV  entry
		 ;;FLUID-ENTRY represents the definition of  the current value of the
		 ;;fluid syntax,  the one  that later can  be shadowed  with internal
		 ;;definitions by FLUID-LET-SYNTAX.
		 (let* ((descriptor	(eval-macro-transformer rhs.core lexenv.run))
			(fluid-label	(generate-label-gensym lhs.id))
			(fluid-entry	(cons fluid-label descriptor))
			(keyword-entry	(cons lhs.lab (make-syntactic-binding-descriptor/local-global-macro/fluid-syntax fluid-label))))
		   (chi-body* (cdr body-form*.stx)
			      (cons* keyword-entry fluid-entry lexenv.run)
			      (cons* keyword-entry fluid-entry lexenv.expand)
			      rev-qdef* mod** keyword* export-spec* rib
			      mix? shadow/redefine-bindings?)))))

	    ((define-alias)
	     ;;The body form  is a core language DEFINE-ALIAS macro  use.  We add a
	     ;;new  association identifier/label  to the  current rib.   Finally we
	     ;;recurse on the rest of the body.
	     ;;
	     (receive (alias-id old-id)
		 (%parse-define-alias body-form.stx)
	       (when (bound-id-member? old-id keyword*)
		 (stx-error body-form.stx "cannot redefine keyword"))
	       (cond ((id->label old-id)
		      => (lambda (label)
			   ;;This call will raise an  exception if it represents an
			   ;;attempt to illegally redefine a binding.
			   (extend-rib! rib alias-id label shadow/redefine-bindings?)
			   (chi-body* (cdr body-form*.stx)
				      lexenv.run lexenv.expand
				      rev-qdef* mod** keyword* export-spec* rib
				      mix? shadow/redefine-bindings?)))
		     (else
		      (stx-error body-form.stx "unbound source identifier")))))

	    ((let-syntax letrec-syntax)
	     ;;The body form is a  core language LET-SYNTAX or LETREC-SYNTAX macro
	     ;;use.   We expand  and evaluate  the transformer  expressions, build
	     ;;syntactic bindings  for them,  register their labels  in a  new rib
	     ;;because they are  visible only in the internal  body.  The internal
	     ;;forms are  spliced in the external  body but with the  rib added to
	     ;;them.
	     ;;
	     (syntax-match body-form.stx ()
	       ((_ ((?xlhs* ?xrhs*) ...) ?xbody* ...)
		(unless (valid-bound-ids? ?xlhs*)
		  (stx-error body-form.stx "invalid identifiers"))
		(let* ((xlab*  (map generate-label-gensym ?xlhs*))
		       (xrib   (make-rib/from-identifiers-and-labels ?xlhs* xlab*))
		       ;;We  evaluate  the  transformers  for  LET-SYNTAX  without
		       ;;pushing the XRIB: the syntax bindings do not exist in the
		       ;;environment in which the transformer is evaluated.
		       ;;
		       ;;We  evaluate  the  transformers for  LETREC-SYNTAX  after
		       ;;pushing the  XRIB: the  syntax bindings  do exist  in the
		       ;;environment in which the transformer is evaluated.
		       (xbind* (map (lambda (x)
				      (let ((in-form (if (eq? type 'let-syntax)
							 x
						       (push-lexical-contour xrib x))))
					(eval-macro-transformer (expand-macro-transformer in-form lexenv.expand)
								lexenv.run)))
				 ?xrhs*)))
		  (chi-body*
		   ;;Splice the internal  body forms but add a  lexical contour to
		   ;;them.
		   (append (map (lambda (internal-body-form)
				  (push-lexical-contour xrib
				    internal-body-form))
			     ?xbody*)
			   (cdr body-form*.stx))
		   ;;Push on the lexical  environment entries corresponding to the
		   ;;defined syntaxes.  Such entries will stay there even after we
		   ;;have processed the internal body forms; this is not a problem
		   ;;because the labels cannot be seen by the rest of the body.
		   (append (map cons xlab* xbind*) lexenv.run)
		   (append (map cons xlab* xbind*) lexenv.expand)
		   rev-qdef* mod** keyword* export-spec* rib
		   mix? shadow/redefine-bindings?)))))

	    ((begin-for-syntax)
	     (chi-begin-for-syntax body-form.stx body-form*.stx lexenv.run lexenv.expand
				   rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?))

	    ((begin)
	     ;;The body form  is a BEGIN syntax use.  Just  splice the expressions
	     ;;and recurse on them.
	     ;;
	     (syntax-match body-form.stx ()
	       ((_ ?expr* ...)
		(chi-body* (append ?expr* (cdr body-form*.stx))
			   lexenv.run lexenv.expand
			   rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?))))

	    ((stale-when)
	     ;;The  body form  is a  STALE-WHEN syntax  use.  Process  the stale-when
	     ;;guard expression, then  just splice the internal expressions  as we do
	     ;;for BEGIN and recurse.
	     ;;
	     (syntax-match body-form.stx ()
	       ((_ ?guard ?expr* ...)
		(begin
		  (handle-stale-when ?guard lexenv.expand)
		  (chi-body* (append ?expr* (cdr body-form*.stx))
			     lexenv.run lexenv.expand
			     rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?)))))

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
			  rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?)))

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
			  rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?)))

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
			  rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?)))

	    ((module)
	     ;;The body  form is  an internal module  definition.  We  process the
	     ;;module, then recurse on the rest of the body.
	     ;;
	     (receive (rev-qdef* m-exp-id* m-exp-lab* lexenv.run lexenv.expand mod** keyword*)
		 (chi-internal-module body-form.stx lexenv.run lexenv.expand rev-qdef* mod** keyword*)
	       ;;Extend the rib with the syntactic bindings exported by the module.
	       (vector-for-each (lambda (id lab)
				  ;;This  call  will  raise   an  exception  if  it
				  ;;represents an  attempt to illegally  redefine a
				  ;;binding.
				  (extend-rib! rib id lab shadow/redefine-bindings?))
		 m-exp-id* m-exp-lab*)
	       (chi-body* (cdr body-form*.stx) lexenv.run lexenv.expand
			  rev-qdef* mod** keyword* export-spec*
			  rib mix? shadow/redefine-bindings?)))

	    ((library)
	     ;;The body  form is  a library definition.   We process  the library,
	     ;;then recurse on the rest of the body.
	     ;;
	     (expand-library (syntax->datum body-form.stx))
	     (chi-body* (cdr body-form*.stx)
			lexenv.run lexenv.expand
			rev-qdef* mod** keyword* export-spec*
			rib mix? shadow/redefine-bindings?))

	    ((export)
	     ;;The body  form is an  EXPORT form.   We just accumulate  the export
	     ;;specifications, to be  processed later, and we recurse  on the rest
	     ;;of the body.
	     ;;
	     (syntax-match body-form.stx ()
	       ((_ ?export-spec* ...)
		(chi-body* (cdr body-form*.stx)
			   lexenv.run lexenv.expand
			   rev-qdef* mod** keyword*
			   (append ?export-spec* export-spec*)
			   rib mix? shadow/redefine-bindings?))))

	    ((import)
	     ;;The body  form is an  IMPORT form.  We  just process the  form which
	     ;;results  in   extending  the   RIB  with   more  identifier-to-label
	     ;;associations.  Finally we recurse on the rest of the body.
	     ;;
	     (chi-import body-form.stx lexenv.run rib shadow/redefine-bindings?)
	     (chi-body* (cdr body-form*.stx) lexenv.run lexenv.expand
			rev-qdef* mod** keyword* export-spec* rib mix? shadow/redefine-bindings?))

	    ((standalone-unbound-identifier)
	     (raise-unbound-error __who__ body-form.stx body-form.stx))

	    ((displaced-lexical)
	     (syntax-violation __who__ "identifier out of context" body-form.stx
			       (syntax-match body-form.stx ()
				 ((?car . ?cdr)
				  ?car)
				 (?id
				  (identifier? ?id)
				  ?id)
				 (_ #f))))

	    (else
	     ;;Any other expression.
	     ;;
	     ;;If  mixed definitions  and  expressions are  allowed,  we handle  this
	     ;;expression as an implicit definition:
	     ;;
	     ;;   (define/standard dummy ?body-form)
	     ;;
	     ;;which is not really part of  the lexical environment: we only generate
	     ;;a lex and a qdef for it, without adding entries to LEXENV.RUN; then we
	     ;;move on to the next body form.
	     ;;
	     ;;If mixed  definitions and expressions  are forbidden: we  have reached
	     ;;the end  of definitions  and expect  this and all  the other  forms in
	     ;;BODY-FORM*.STX  to  be  expressions.  So  BODY-FORM*.STX  becomes  the
	     ;;"trailing expressions" return value.
	     ;;
	     (if mix?
		 ;;There must be a LEX for every QDEF, really.
		 (let ((qdef (make-qdef-top-expr body-form.stx)))
		   (chi-body* (cdr body-form*.stx)
			      lexenv.run lexenv.expand
			      (cons qdef rev-qdef*)
			      mod** keyword* export-spec* rib mix? shadow/redefine-bindings?))
	       (values body-form*.stx lexenv.run lexenv.expand rev-qdef* mod** keyword* export-spec*))))))
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
     (values ?id (bless `(lambda (,?arg) ,?body0 ,@?body*))))
    ((_ ?id)
     (identifier? ?id)
     (values ?id (bless '(syntax-rules ()))))
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
	 (raise-unbound-error __module_who__ import-form.stx module-name.id))
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
				qdef* mod** kwd* export-spec* rib mix?
				shadow/redefine-bindings?)
    (receive (lhs*.lex init*.core rhs*.core lexenv.expand^)
	(%expand input-form.stx lexenv.expand rib shadow/redefine-bindings?)
      ;;Build an expanded  code expression and evaluate it.
      (let ((visit-code.core (build-sequence no-source
			       (append (map (lambda (lhs.lex rhs.core)
					      (build-global-assignment no-source
						lhs.lex rhs.core))
					 lhs*.lex rhs*.core)
				       init*.core))))
	(unless (void-core-expression? visit-code.core)
	  (compiler::eval-core (expanded->core visit-code.core)))
	;;Done!  Push on the LEXENV an entry like:
	;;
	;;   (?unused-label . (begin-for-syntax . ?core-code))
	;;
	;;then go on with the next body forms.
	(let ((lexenv.run^ (if (void-core-expression? visit-code.core)
			       lexenv.run
			     (cons (cons (gensym "begin-for-syntax-label")
					 (cons 'begin-for-syntax visit-code.core))
				   lexenv.run))))
	  (chi-body* (cdr body-form*.stx)
		     lexenv.run^ lexenv.expand^
		     qdef* mod** kwd* export-spec* rib
		     mix? shadow/redefine-bindings?)))))

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

  (define-record-type (<module-interface> make-module-interface module-interface?)
    (nongenerative vicare:expander:<module-interface>)
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


;;;; parsing DEFINE forms

(module CHI-DEFINE
  (chi-define/standard
   chi-define/typed
   chi-case-define/standard
   chi-case-define/typed)
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


;;;; core macros: DEFINE/STANDARD

(module (chi-define/standard)
  ;;The  BODY-FORM.STX  parsed  by  CHI-BODY*  is  a  syntax  object  representing  a
  ;;DEFINE/STANDARD core macro use; for example, one among:
  ;;
  ;;   (define/standard ?lhs)
  ;;   (define/standard ?lhs ?rhs)
  ;;   (define/standard (?lhs . ?formals) . ?body)
  ;;
  ;;we parse  the form and  generate a qualified  right-hand side (QDEF)  object that
  ;;will be expanded later.  Here we establish a new syntactic binding representing a
  ;;fully R6RS compliant  lexical variable in the lexical context  represented by the
  ;;arguments RIB and LEXENV.RUN.
  ;;
  (define-module-who define/standard)

  (define (chi-define/standard input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (case-define %synner
      ((message)
       (syntax-violation __module_who__ message input-form.stx))
      ((message subform)
       (syntax-violation __module_who__ message input-form.stx subform)))
    (receive (lhs.id lhs.type qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the  lexical   variable's  syntactic  binding's  identifier;   LHS.TYPE,  a
	;;syntactic  identifier representing  the  type of  this  binding; QDEF,  the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.type (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define* (%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? synner)
      ;;Syntax  parser for  Vicare's DEFINE/STANDARD  syntax uses;  this is  like the
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
	 (%process-standard-function-definition input-form.stx rib lexenv.run shadow/redefine-bindings?
						?lhs ?formals `(,?body0 . ,?body*) synner))

	((_ ?lhs ?rhs)
	 (%process-standard-variable-definition-with-init-expr	input-form.stx lexenv.run ?lhs ?rhs synner))

	((_ ?lhs)
	 (%process-standard-variable-definition-with-init-expr	input-form.stx lexenv.run ?lhs (bless '(void)) synner))

	(_
	 (synner "invalid syntax"))))

    (define (%process-standard-function-definition input-form.stx rib lexenv.run shadow/redefine-bindings?
						   lhs.id input-formals.stx body*.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as function name" lhs.id))
      ;;From parsing the standard formals we get  2 values: a proper or improper list
      ;;of   identifiers  representing   the   standard  formals;   an  instance   of
      ;;"<clambda-clause-signature>".   An exception  is  raised if  an error  occurs
      ;;while parsing.
      (receive (standard-formals.stx clause-signature)
	  (syntax-object.parse-standard-clambda-clause-formals input-formals.stx input-form.stx)
	;;This is a standard variable syntactic  binding, so the arguments and return
	;;values are untyped.   However, we still want to generate  a typed syntactic
	;;binding so that  we can validate the number of  operands and return values.
	;;So we fabricate a type identifier and put it on the lexenv to represent the
	;;type of this function.
	(let* ((lhs.type	(datum->syntax lhs.id (make-fabricated-closure-type-name (identifier->symbol lhs.id))))
	       (signature	(make-clambda-signature (list clause-signature)))
	       (lexenv.run	(make-syntactic-binding/closure-type-name lhs.type signature rib lexenv.run shadow/redefine-bindings?))
	       (qdef		(make-qdef-standard-defun input-form.stx lhs.id standard-formals.stx body*.stx
								   lhs.type signature)))
	  (values lhs.id lhs.type qdef lexenv.run))))

    (define (%process-standard-variable-definition-with-init-expr input-form.stx lexenv.run lhs.id rhs.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      (let ((qdef (make-qdef-standard-defvar input-form.stx lhs.id rhs.stx)))
	(values lhs.id (top-tag-id) qdef lexenv.run)))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-DEFINE/STANDARD |# )


;;;; core macros: CASE-DEFINE/STANDARD

(module (chi-case-define/standard)

  (define-constant __module_who__ 'case-define/standard)

  (define* (chi-case-define/standard input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (case-define %synner
      ((message)
       (syntax-violation __module_who__ message input-form.stx))
      ((message subform)
       (syntax-violation __module_who__ message input-form.stx subform)))
    (receive (lhs.id lhs.type qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the  lexical   variable's  syntactic  binding's  identifier;   LHS.TYPE,  a
	;;syntactic  identifier representing  the  type of  this  binding; QDEF,  the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.type (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define (%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? synner)
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
	     ;;return values.   So we fabricate a  type identifier and put  it on the
	     ;;lexenv to represent the type of this function.
	     (let* ((lhs.type	(datum->syntax ?who (make-fabricated-closure-type-name (identifier->symbol ?who))))
		    (signature	(make-clambda-signature clause-signature*))
		    (lexenv.run	(make-syntactic-binding/closure-type-name lhs.type signature rib lexenv.run shadow/redefine-bindings?))
		    (qdef		(make-qdef-standard-case-defun input-form.stx ?who standard-formals*.stx body**.stx
										lhs.type signature)))
	       (values ?who lhs.type qdef lexenv.run)))))
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
	 ;;"<clambda-clause-signature>".  An  exception is raised if  an error occurs
	 ;;while parsing.
	 (receive (standard-formals.stx clause-signature)
	     (syntax-object.parse-standard-clambda-clause-formals ?formals input-form.stx)
	   (values standard-formals.stx clause-signature (cons ?body ?body*))))
	))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-CASE-DEFINE/STANDARD |# )


;;;; core macros: DEFINE/TYPED

(module (chi-define/typed)
  ;;The  BODY-FORM.STX  parsed  by  CHI-BODY*  is  a  syntax  object  representing  a
  ;;DEFINE/TYPED core macro use; for example, one among:
  ;;
  ;;   (define/typed ?lhs)
  ;;   (define/typed ?lhs ?rhs)
  ;;   (define/typed (?lhs . ?formals) . ?body)
  ;;   (define/typed (brace ?lhs ?lhs.type)
  ;;   (define/typed (brace ?lhs ?lhs.type) ?rhs)
  ;;   (define/typed ((brace ?lhs ?lhs.type) . ?formals) . ?body)
  ;;
  ;;we parse  the form and  generate a qualified  right-hand side (QDEF)  object that
  ;;will be expanded later.  Here we establish a new syntactic binding representing a
  ;;typed lexical  variable in the lexical  context represented by the  arguments RIB
  ;;and LEXENV.RUN.
  ;;
  (define-module-who define/typed)

  (define (chi-define/typed input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (case-define %synner
      ((message)
       (syntax-violation __module_who__ message input-form.stx))
      ((message subform)
       (syntax-violation __module_who__ message input-form.stx subform)))
    (receive (lhs.id lhs.type qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the  lexical   variable's  syntactic  binding's  identifier;   LHS.TYPE,  a
	;;syntactic  identifier representing  the  type of  this  binding; QDEF,  the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.type (qdef.lex qdef)))
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
	((_ ((brace ?lhs ?rv-type* ... . ?rv-rest-type) . ?formals) ?body0 ?body* ...)
	 (%process-typed-function-definition input-form.stx rib lexenv.run shadow/redefine-bindings?
					     ?lhs (bless `((brace _ ,@?rv-type* . ,?rv-rest-type) . ,?formals))
					     `(,?body0 . ,?body*) synner))

	((_ (brace ?lhs ?type) ?rhs)
	 (%process-typed-variable-definition-with-init-expr	input-form.stx lexenv.run ?lhs ?rhs ?type synner))

	((_ (brace ?lhs ?type))
	 (%process-typed-variable-definition-with-init-expr	input-form.stx lexenv.run ?lhs (bless '(void)) ?type synner))

	((_ (?lhs . ?formals) ?body0 ?body* ...)
	 (%process-typed-function-definition input-form.stx rib lexenv.run shadow/redefine-bindings?
					     ?lhs ?formals `(,?body0 . ,?body*) synner))

	((_ ?lhs ?rhs)
	 (%process-standard-variable-definition-with-init-expr	input-form.stx lexenv.run ?lhs ?rhs synner))

	((_ ?lhs)
	 (%process-standard-variable-definition-with-init-expr	input-form.stx lexenv.run ?lhs (bless '(void)) synner))

	(_
	 (synner "invalid DEFINE/TYPED syntax use"))))

    (define (%process-typed-function-definition input-form.stx rib lexenv.run shadow/redefine-bindings?
						lhs.id input-formals.stx body*.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      ;;From parsing the typed formals we get  2 values: a proper or improper list of
      ;;identifiers   representing    the   standard   formals;   an    instance   of
      ;;"<clambda-clause-signature>".   An exception  is  raised if  an error  occurs
      ;;while parsing.
      (receive (standard-formals.stx clause-signature)
	  (syntax-object.parse-typed-clambda-clause-formals input-formals.stx input-form.stx)
	(let* ((lhs.type	(datum->syntax lhs.id (make-fabricated-closure-type-name (identifier->symbol lhs.id))))
	       (signature	(make-clambda-signature (list clause-signature)))
	       (lexenv.run	(make-syntactic-binding/closure-type-name lhs.type signature rib lexenv.run shadow/redefine-bindings?))
	       (qdef		(make-qdef-typed-defun input-form.stx lhs.id standard-formals.stx body*.stx
								lhs.type signature)))
	  (values lhs.id lhs.type qdef lexenv.run))))

    (define (%process-typed-variable-definition-with-init-expr input-form.stx lexenv.run
							       lhs.id rhs.stx lhs.type synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      (unless (type-identifier? lhs.type)
	(synner "expected type identifier as type annotation for variable name" lhs.type))
      (let ((qdef (make-qdef-typed-defvar input-form.stx lhs.id rhs.stx lhs.type)))
	(values lhs.id lhs.type qdef lexenv.run)))

    (define (%process-standard-variable-definition-with-init-expr input-form.stx lexenv.run
								  lhs.id rhs.stx synner)
      (unless (identifier? lhs.id)
	(synner "expected identifier as variable name" lhs.id))
      (let ((qdef (make-qdef-standard-defvar input-form.stx lhs.id rhs.stx)))
	(values lhs.id (top-tag-id) qdef lexenv.run)))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-DEFINE/TYPED |# )


;;;; core macros: CASE-DEFINE/TYPED

(module (chi-case-define/typed)

  (define-constant __module_who__ 'case-define/typed)

  (define* (chi-case-define/typed input-form.stx rib lexenv.run kwd* shadow/redefine-bindings?)
    (case-define %synner
      ((message)
       (syntax-violation __module_who__ message input-form.stx))
      ((message subform)
       (syntax-violation __module_who__ message input-form.stx subform)))
    (receive (lhs.id lhs.type qdef lexenv.run)
	;;From parsing the  syntactic form, we receive the  following values: LHS.ID,
	;;the  lexical   variable's  syntactic  binding's  identifier;   LHS.TYPE,  a
	;;syntactic  identifier representing  the  type of  this  binding; QDEF,  the
	;;qualified RHS object to be expanded later.
	(%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? %synner)
      (if (bound-id-member? lhs.id kwd*)
	  (%synner "cannot redefine keyword")
	(let* ((lhs.lab		(generate-label-gensym   lhs.id))
	       (descr		(make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.type (qdef.lex qdef)))
	       (lexenv.run	(push-entry-on-lexenv lhs.lab descr lexenv.run)))
	  ;;This rib extension will raise an exception if it represents an attempt to
	  ;;illegally redefine a binding.
	  (extend-rib! rib lhs.id lhs.lab shadow/redefine-bindings?)
	  (values qdef lexenv.run)))))

  (module (%parse-macro-use)

    (define (%parse-macro-use input-form.stx rib lexenv.run shadow/redefine-bindings? synner)
      (syntax-match input-form.stx ()
	((_ ?who ?cl-clause ?cl-clause* ...)
	 (begin
	   (unless (identifier? ?who)
	     (synner "expected identifier as function name" ?who))
	   (receive (standard-formals*.stx clause-signature* body**.stx)
	       (%parse-clauses input-form.stx (cons ?cl-clause ?cl-clause*) '() '() '())
	     (let* ((lhs.type	(datum->syntax ?who (make-fabricated-closure-type-name (identifier->symbol ?who))))
		    (signature	(make-clambda-signature clause-signature*))
		    (lexenv.run	(make-syntactic-binding/closure-type-name lhs.type signature rib lexenv.run shadow/redefine-bindings?))
		    (qdef	(make-qdef-typed-case-defun input-form.stx ?who standard-formals*.stx body**.stx
							    lhs.type signature)))
	       (values ?who lhs.type qdef lexenv.run)))))
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
	 ;;"<clambda-clause-signature>".  An  exception is raised if  an error occurs
	 ;;while parsing.
	 (receive (standard-formals.stx clause-signature)
	     (syntax-object.parse-typed-clambda-clause-formals ?formals input-form.stx)
	   (values standard-formals.stx clause-signature (cons ?body ?body*))))
	))

    #| end of module: %PARSE-MACRO-USE |# )

  #| end of module: CHI-CASE-DEFINE/TYPED |# )


;;;; parsing DEFINE forms: end of module

#| end of module: CHI-DEFINE |# )


;;;; done

#| end of module: CHI-BODY* |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'assertion-violation/internal-error	'scheme-indent-function 1)
;;eval: (put 'with-who					'scheme-indent-function 1)
;;End:
