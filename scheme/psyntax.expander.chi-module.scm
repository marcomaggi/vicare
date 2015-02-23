;;;Copyright (c) 2010-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; core expressions struct

(module (psi
	 make-psi psi?
	 psi-stx
	 psi-core-expr
	 psi-retvals-signature
	 psi-application-retvals-signature)

  (define-record (psi %make-psi psi?)
    (stx
		;The syntax object that originated this struct instance.  In the case
		;of internal  body: it is a  list of syntax objects,  but this should
		;not be a problem.  It is kept here for debugging purposes: it can be
		;used  as  "form"  or  "subform"  argument  for  "&syntax"  condition
		;objects.
     core-expr
		;Either:
		;
		;* A symbolic expression in the core language representing the result
		;  of fully expanding a syntax object.
		;
		;* An  instance of  "splice-first-envelope".  This happens  only when
		;   this  PSI   struct  is  the  return  value  of   the  core  macro
		;  SPLICE-FIRST-EXPAND.
     retvals-signature
		;An instance of "retvals-signature".
		;
		;When  this  PSI is  a  callable  object:  we  expect this  field  to
		;represent a signature like:
		;
		;   (?tag)
		;
		;where ?TAG  is "<procedure>" or a  subtag of it; in  the latter case
		;?TAG has an associated callable spec object in its property list.
     ))

  (case-define* make-psi
    ((stx core-expr)
     (%make-psi stx core-expr (make-retvals-signature-fully-unspecified)))
    ((stx core-expr {retvals-signature retvals-signature?})
     (%make-psi stx core-expr retvals-signature)))

  (define* ({psi-application-retvals-signature retvals-signature?} {rator.psi psi?})
    ;;RATOR.PSI is a PSI representing the first form in a callable application:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;we need to establish the retvals signature of the application and return it.
    ;;
    (syntax-match (retvals-signature-tags (psi-retvals-signature rator.psi)) ()
      ((?tag)
       (let ((spec (tag-identifier-callable-signature ?tag)))
	 (cond ((lambda-signature? spec)
		(lambda-signature-retvals spec))
	       ((clambda-compound? spec)
		(clambda-compound-common-retvals-signature spec))
	       (else
		(make-retvals-signature-fully-unspecified)))))
      (_
       (make-retvals-signature-fully-unspecified))))

  #| end of module |# )


;;;; lambda clause attributes

(define (lambda-clause-attributes:safe-formals? attributes.sexp)
  (or (memq 'safe         attributes.sexp)
      (memq 'safe-formals attributes.sexp)))

(define (lambda-clause-attributes:safe-retvals? attributes.sexp)
  (or (memq 'safe         attributes.sexp)
      (memq 'safe-retvals attributes.sexp)))


;;;; chi procedures: syntax object type inspection

(define (expr-syntax-type expr.stx lexenv)
  ;;Determine the  syntax type of  an expression.  EXPR.STX  must be a  syntax object
  ;;representing an expression.  Return 3 values:
  ;;
  ;;1..A symbol representing the syntax type.
  ;;
  ;;2..If  the syntax  is a  macro application:  the value  of the  syntactic binding
  ;;   associated to the macro identifier; otherwise false.
  ;;
  ;;3..If the  syntax is a macro  application: the identifier representing  the macro
  ;;   keyword; otherwise false.
  ;;
  ;;The type of an expression is determined by two things:
  ;;
  ;;* The shape of the expression (identifier, pair, or datum).
  ;;
  ;;* The binding of the identifier or the type of car of the pair.
  ;;
  (syntax-match expr.stx ()
    (?id
     (identifier? expr.stx)
     (let ((label (id->label/intern ?id)))
       (unless label
	 (%raise-unbound-error #f expr.stx ?id))
       (let* ((binding (label->syntactic-binding label lexenv))
	      (type    (syntactic-binding-type binding)))
	 (case type
	   ((core-prim
	     lexical global mutable
	     core-macro! global-macro global-macro! macro macro! local-macro local-macro!
	     import export library $module syntax
	     local-ctv global-ctv
	     displaced-lexical)
	    (values type (syntactic-binding-value binding) ?id))
	   (else
	    ;;This will cause an error to be raised later.
	    (values 'other #f #f))))))

    ((?tag)
     (tag-identifier? ?tag)
     (values 'tag-cast-operator #f ?tag))

    ((?car . ?cdr)
     (identifier? ?car)
     ;;Here we know that EXPR.STX has the format:
     ;;
     ;;   (?car ?form ...)
     ;;
     (cond ((id->label/intern ?car)
	    => (lambda (label)
		 (let* ((binding (label->syntactic-binding label lexenv))
			(type    (syntactic-binding-type binding)))
		   (case type
		     ((core-macro
		       define define-syntax define-alias
		       define-fluid-syntax define-fluid-override
		       let-syntax letrec-syntax begin-for-syntax
		       begin set! stale-when
		       local-ctv global-ctv
		       global-macro global-macro! local-macro local-macro! macro
		       import export library module)
		      (values type (syntactic-binding-value binding) ?car))
		     (($rtd)
		      (values 'tag-maker-application (syntactic-binding-value binding) ?car))
		     (else
		      ;;This case includes TYPE being: CORE-PRIM, LEXICAL, GLOBAL, MUTABLE.
		      (values 'call #f #f))))))
	   (else
	    (%raise-unbound-error #f expr.stx ?car))))

    ((?car . ?cdr)
     ;;Here we know that EXPR.STX has the format:
     ;;
     ;;   (?non-id ?form ...)
     ;;
     ;;where ?NON-ID  can be anything but  not an identifier.  In  practice the valid
     ;;syntaxes for this case are:
     ;;
     ;;   ((?first-subform ?subform ...) ?form ...)
     ;;   (?datum ?form ...)
     ;;
     (values 'call #f #f))

    (_
     (let ((datum (syntax->datum expr.stx)))
       (if (self-evaluating? datum)
	   (values 'constant datum #f)
	 ;;This will cause an error to be raised later.
	 (values 'other #f #f))))
    ))


;;;; chi procedures: helpers for SPLICE-FIRST-EXPAND

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


;;;; chi procedures: macro calls

(module (chi-non-core-macro
	 chi-local-macro
	 chi-global-macro)

  (define* (chi-non-core-macro {procname symbol?} input-form-stx lexenv.run {rib false-or-rib?})
    ;;Expand an expression representing the use  of a non-core macro; the transformer
    ;;function is  integrated in the  expander.  Return a syntax  object representing
    ;;the macro output form.
    ;;
    ;;PROCNAME is a  symbol representing the name  of the non-core macro;  we can map
    ;;from   such  symbol   to  the   transformer   function  with   the  module   of
    ;;NON-CORE-MACRO-TRANSFORMER.
    ;;
    ;;INPUT-FORM-STX is the syntax object representing the expression to be expanded.
    ;;
    ;;LEXENV.RUN is the run-time lexical environment  in which the expression must be
    ;;expanded.
    ;;
    ;;RIB is false or a struct of type "<rib>".
    ;;
    (%do-macro-call (let ()
		      (import NON-CORE-MACRO-TRANSFORMER)
		      (non-core-macro-transformer procname))
		    input-form-stx lexenv.run rib))

  (define* (chi-local-macro bind-val input-form-stx lexenv.run {rib false-or-rib?})
    ;;This function  is used  to expand  macro uses for  macros whose  transformer is
    ;;defined by local user code, but  not identifier syntaxes; these are the lexical
    ;;environment  entries with  types  "local-macro" and  "local-macro!".  Return  a
    ;;syntax object representing the macro output form.
    ;;
    ;;BIND-VAL is the binding value of the global macro.  The format of the syntactic
    ;;binding descriptors is:
    ;;
    ;;     (local-macro  . (?transformer . ?expanded-expr))
    ;;     (local-macro! . (?transformer . ?expanded-expr))
    ;;
    ;;and the argument BIND-VAL is:
    ;;
    ;;     (?transformer . ?expanded-expr)
    ;;
    ;;INPUT-FORM-STX is the syntax object representing the expression to be expanded.
    ;;
    ;;LEXENV.RUN is the run-time lexical environment  in which the expression must be
    ;;expanded.
    ;;
    ;;RIB is false or a struct of type "<rib>".
    ;;
    (%do-macro-call (car bind-val) input-form-stx lexenv.run rib))

  (define* (chi-global-macro bind-val input-form-stx lexenv.run {rib false-or-rib?})
    ;;This function  is used  to expand  macro uses for  macros whose  transformer is
    ;;defined by user  code in imported libraries; these are  the lexical environment
    ;;entries with types "global-macro" and  "global-macro!".  Return a syntax object
    ;;representing the macro output form.
    ;;
    ;;BIND-VAL is the binding value of the global macro.  The format of the syntactic
    ;;binding descriptors is:
    ;;
    ;;     (global-macro  . (?library . ?loc))
    ;;     (global-macro! . (?library . ?loc))
    ;;
    ;;and the argument BIND-VAL is:
    ;;
    ;;     (?library . ?loc)
    ;;
    ;;INPUT-FORM-STX is the syntax object representing the expression to be expanded.
    ;;
    ;;LEXENV.RUN is the run-time lexical environment  in which the expression must be
    ;;expanded.
    ;;
    ;;RIB is false or a struct of type "<rib>".
    ;;
    (let ((lib (car bind-val))
	  (loc (cdr bind-val)))
      ;;If this  global binding use  is the  first time a  binding from LIB  is used:
      ;;visit the library.
      (unless (eq? lib '*interaction*)
	(visit-library lib))
      (let* ((x           (symbol-value loc))
	     (transformer (cond ((procedure? x)
				 x)
				((variable-transformer? x)
				 (variable-transformer-procedure x))
				(else
				 (assertion-violation/internal-error __who__
				   "not a procedure" x)))))
	(%do-macro-call transformer input-form-stx lexenv.run rib))))

;;; --------------------------------------------------------------------

  (define* (%do-macro-call transformer input-form-stx lexenv.run {rib false-or-rib?})
    ;;Apply  the transformer  to the  macro input  form.  Return  the output  form as
    ;;syntax object.
    ;;
    (import ADD-MARK)
    (define (main)
      ;;We parametrise here because we can never know which transformer, for example,
      ;;will query the syntactic binding properties.
      (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	(let ((output-form-expr (transformer
				 ;;Put the anti-mark on the input form.
				 (add-mark anti-mark #f input-form-stx #f))))
	  ;;If  the  transformer returns  a  function:  we  must apply  the  returned
	  ;;function  to a  function acting  as compile-time  value retriever.   Such
	  ;;application must return a value as a transformer would do.
	  (if (procedure? output-form-expr)
	      (%return (output-form-expr %ctv-retriever))
	    (%return output-form-expr)))))

    (define (%return output-form-expr)
      ;;Check  that there  are no  raw symbols  in the  value returned  by the  macro
      ;;transformer.
      (let recur ((x output-form-expr))
	;;Don't feed me cycles.
	(unless (<stx>? x)
	  (cond ((pair? x)
		 (recur (car x))
		 (recur (cdr x)))
		((vector? x)
		 (vector-for-each recur x))
		((symbol? x)
		 (syntax-violation #f
		   "raw symbol encountered in output of macro"
		   input-form-stx x)))))
      ;;Put a new mark  on the output form.  For all  the identifiers already present
      ;;in the input form: this new mark  will be annihilated by the anti-mark we put
      ;;before.  For all the identifiers introduced by the transformer: this new mark
      ;;will stay there.
      (add-mark (gen-mark) rib output-form-expr input-form-stx))

    (define (%ctv-retriever id)
      ;;This is  the compile-time  values retriever  function.  Given  an identifier:
      ;;search an  entry in  the lexical  environment; when  found return  its value,
      ;;otherwise return false.
      ;;
      (unless (identifier? id)
	(assertion-violation 'rho "not an identifier" id))
      (let ((binding (label->syntactic-binding (id->label id) lexenv.run)))
	(case (syntactic-binding-type binding)
	  ;;The given identifier is bound to  a local compile-time value.  The actual
	  ;;object is stored in the binding itself.
	  ((local-ctv)
	   (local-compile-time-value-binding-object binding))

	  ;;The given  identifier is bound  to a  compile-time value imported  from a
	  ;;library or the top-level environment.  The actual object is stored in the
	  ;;"value" field of a loc gensym.
	  ((global-ctv)
	   (global-compile-time-value-binding-object binding))

	  ;;The given identifier is not bound to a compile-time value.
	  (else #f))))

    (main))

  #| end of module |# )


;;;; chi procedures: expressions

(define* (chi-expr* expr*.stx lexenv.run lexenv.expand)
  ;;Recursive function.  Expand the expressions in EXPR*.STX left to right.  Return a
  ;;list of PSI structs.
  ;;
  (if (null? expr*.stx)
      '()
    ;;ORDER MATTERS!!!  Make sure that first we do the car, then the rest.
    (let ((expr0.psi (while-not-expanding-application-first-subform
		      (chi-expr (car expr*.stx) lexenv.run lexenv.expand))))
      (cons expr0.psi
	    (chi-expr* (cdr expr*.stx) lexenv.run lexenv.expand)))))

(module (chi-expr)

  (define (chi-expr expr.stx lexenv.run lexenv.expand)
    ;;Expand a single expression form.  Return a PSI struct.
    ;;
    (%drop-splice-first-envelope-maybe
     (receive (type bind-val kwd)
	 (expr-syntax-type expr.stx lexenv.run)
       (case type
	 ((core-macro)
	  ;;Core  macro use.   The  core  macro transformers  are  integrated in  the
	  ;;expander; they perform the full expansion of their input forms and return
	  ;;a PSI struct.
	  (let ((transformer    (let ()
				  (import CORE-MACRO-TRANSFORMER)
				  (core-macro-transformer bind-val))))
	    (transformer (if (option.debug-mode-enabled?)
			     ;;Here we push the input  form on the stack of annotated
			     ;;expressions,  to improve  error  messages  in case  of
			     ;;syntax violations.  When expanding non-core macros the
			     ;;function %DO-MACRO-CALL  takes care  of doing  it, but
			     ;;for core macros we have to do it here.
			     ;;
			     ;;NOTE Unfortunately, I have  measured that wrapping the
			     ;;input  form into  an additional  "<stx>" record  slows
			     ;;down  the   expansion  in  a  significant   way;  when
			     ;;rebuilding the full Vicare  source code, compiling the
			     ;;libraries and  running the  test suite the  total time
			     ;;can  be 25%  greater.  For  this reason  this step  is
			     ;;performed only when debugging mode is enabled.  (Marco
			     ;;Maggi; Wed Apr 2, 2014)
			     (make-<stx> expr.stx '() '() (list expr.stx))
			   expr.stx)
			 lexenv.run lexenv.expand)))

	 ((global)
	  ;;Reference to global imported lexical  variable; this means EXPR.STX is an
	  ;;identifier.  We expect the syntactic binding descriptor to be:
	  ;;
	  ;;   (global . (?library . ?loc))
	  ;;
	  ;;and BIND-VAL to be:
	  ;;
	  ;;   (?library . ?loc)
	  ;;
	  (let* ((lib (car bind-val))
		 (loc (cdr bind-val)))
	    ((inv-collector) lib)
	    (make-psi expr.stx
		      (build-global-reference no-source loc)
		      (identifier-tag-retvals-signature expr.stx))))

	 ((core-prim)
	  ;;Core primitive (it  is either a procedure or a  constant).  We expect the
	  ;;syntactic binding descriptor to be:
	  ;;
	  ;;   (core-prim . ?prim-name)
	  ;;
	  ;;and BIND-VAL to be the symbol:
	  ;;
	  ;;   ?prim-name
	  ;;
	  (let ((name bind-val))
	    (make-psi expr.stx
		      (build-primref no-source name)
		      (identifier-tag-retvals-signature expr.stx))))

	 ((call)
	  ;;A function call; this means EXPR.STX has one of the formats:
	  ;;
	  ;;   (?id ?form ...)
	  ;;   ((?first-subform ?subform ...) ?form ...)
	  ;;
	  (chi-application expr.stx lexenv.run lexenv.expand))

	 ((lexical)
	  ;;Reference to lexical variable; this means EXPR.STX is an identifier.
	  (make-psi expr.stx
		    (let ((lex (lexical-var bind-val)))
		      (build-lexical-reference no-source lex))
		    (identifier-tag-retvals-signature expr.stx)))

	 ((global-macro global-macro!)
	  ;;Global macro use.
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-global-macro bind-val expr.stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((local-macro local-macro!)
	  ;;Macro uses of macros that are local in a non top-level region.
	  ;;
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-local-macro bind-val expr.stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((macro macro!)
	  ;;Macro  uses  of  non-core  macros;  such macros  are  integrated  in  the
	  ;;expander.
	  ;;
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-non-core-macro bind-val expr.stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((constant)
	  ;;Constant; it means EXPR.STX is a self-evaluating datum.
	  (let ((datum bind-val))
	    (make-psi expr.stx
		      (build-data no-source datum)
		      (retvals-signature-of-datum datum))))

	 ((set!)
	  ;;Macro use of SET!; it means EXPR.STX has the format:
	  ;;
	  ;;   (set! ?lhs ?rhs)
	  ;;
	  (let ()
	    (import CHI-SET)
	    (chi-set! expr.stx lexenv.run lexenv.expand)))

	 ((begin)
	  ;;R6RS BEGIN  core macro use.   First we  check with SYNTAX-MATCH  that the
	  ;;syntax is correct, then we build the core language expression.
	  ;;
	  (syntax-match expr.stx ()
	    ((_ ?body ?body* ...)
	     (let ((body*.psi (chi-expr* (cons ?body ?body*) lexenv.run lexenv.expand)))
	       (make-psi expr.stx
			 (build-sequence no-source
			   (map psi-core-expr body*.psi))
			 (psi-retvals-signature (proper-list->last-item body*.psi)))))
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
			     (map psi-core-expr body*.psi))
			   (psi-retvals-signature (proper-list->last-item body*.psi))))))
	    ))

	 ((let-syntax letrec-syntax)
	  ;;LET-SYNTAX or LETREC-SYNTAX core macro uses.
	  (syntax-match expr.stx ()
	    ((_ ((?xlhs* ?xrhs*) ...) ?xbody ?xbody* ...)
	     (unless (valid-bound-ids? ?xlhs*)
	       (stx-error expr.stx "invalid identifiers"))
	     (let* ((xlab* (map gensym-for-label ?xlhs*))
		    (xrib  (make-filled-rib ?xlhs* xlab*))
		    (xb*   (map (lambda (x)
				  (let ((in-form (if (eq? type 'let-syntax)
						     x
						   (push-lexical-contour xrib x))))
				    (with-exception-handler/input-form
					in-form
				      (%eval-macro-transformer
				       (%expand-macro-transformer in-form lexenv.expand)
				       lexenv.run))))
			     ?xrhs*)))
	       (let ((body*.psi (chi-expr* (map (lambda (x)
						  (push-lexical-contour xrib x))
					     (cons ?xbody ?xbody*))
					   (append (map cons xlab* xb*) lexenv.run)
					   (append (map cons xlab* xb*) lexenv.expand))))
		 (make-psi expr.stx
			   (build-sequence no-source
			     (map psi-core-expr body*.psi))
			   (psi-retvals-signature (proper-list->last-item body*.psi))))))
	    ))

	 ((displaced-lexical)
	  (stx-error expr.stx "identifier out of context"))

	 ((syntax)
	  (stx-error expr.stx "reference to pattern variable outside a syntax form"))

	 ((define define-syntax define-fluid-syntax define-fluid-override define-alias module import library)
	  (stx-error expr.stx (string-append
			       (case type
				 ((define)                 "a definition")
				 ((define-syntax)          "a define-syntax")
				 ((define-fluid-syntax)    "a define-fluid-syntax")
				 ((define-fluid-override)  "a define-fluid-override")
				 ((define-alias)           "a define-alias")
				 ((module)                 "a module definition")
				 ((library)                "a library definition")
				 ((import)                 "an import declaration")
				 ((export)                 "an export declaration")
				 (else                     "a non-expression"))
			       " was found where an expression was expected")))

	 ((mutable)
	  ;;Variable in reference  position, whose binding is a  mutable variable; it
	  ;;means EXPR.STX is an identifier.
	  ;;
	  ;;We expect the syntactic binding descriptor to be:
	  ;;
	  ;;   (mutable . (?library . ?loc))
	  ;;
	  ;;and BIND-VAL to be:
	  ;;
	  ;;   (?library . ?loc)
	  ;;
	  (if (and (pair? bind-val)
		   (let ((lib (car bind-val)))
		     (eq? lib '*interaction*)))
	      (let ((loc (cdr bind-val)))
		(make-psi expr.stx
			  (build-global-reference no-source loc)
			  (identifier-tag-retvals-signature expr.stx)))
	    (stx-error expr.stx "attempt to reference an unexportable variable")))

	 ((tag-maker-application)
	  ;;Here we expand  a form whose car  is a tag identifier.  The  result is an
	  ;;expression  that evaluates  to the  application of  the struct  or record
	  ;;maker.
	  (syntax-match expr.stx ()
	    ((?tag (?ellipsis))
	     (ellipsis-id? ?ellipsis)
	     (let ((constructor.stx (tag-identifier-constructor-maker ?tag expr.stx)))
	       (chi-expr constructor.stx lexenv.run lexenv.expand)))
	    ((?tag (?arg* ...))
	     (let ((constructor.stx (tag-identifier-constructor-maker ?tag expr.stx)))
	       (chi-expr (bless
			  `(,constructor.stx . ,?arg*))
			 lexenv.run lexenv.expand)))
	    ))

	 ((tag-cast-operator)
	  ;;The input form is:
	  ;;
	  ;;   (?tag)
	  ;;
	  ;;and we imagine it is the first form in:
	  ;;
	  ;;   ((?tag) ?object)
	  ;;
	  ;;this is the  syntax of casting a  value from a type to  another.  We want
	  ;;the following chain of expansions:
	  ;;
	  ;;   ((?tag) ?object)
	  ;;   ==> ((splice-first-expand (tag-cast ?tag)) ?object)
	  ;;   ==> (tag-cast ?tag ?object)
	  ;;
	  (chi-expr (bless
		     `(splice-first-expand (tag-cast ,kwd)))
		    lexenv.run lexenv.expand))

	 (else
	  ;;(assertion-violation 'chi-expr "invalid type " type (strip expr.stx '()))
	  (stx-error expr.stx "invalid expression"))))
     lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (define* (%drop-splice-first-envelope-maybe {expr.psi psi?} lexenv.run lexenv.expand)
    ;;If we are expanding the first  subform of an application: just return EXPR.PSI;
    ;;otherwise if EXPR.PSI  is a splice-first envelope: extract its  form, expand it
    ;;and return the result.
    ;;
    (import SPLICE-FIRST-ENVELOPE)
    (let ((expr (psi-core-expr expr.psi)))
      (if (splice-first-envelope? expr)
	  (if (expanding-application-first-subform?)
	      expr.psi
	    (%drop-splice-first-envelope-maybe (chi-expr (splice-first-envelope-form expr) lexenv.run lexenv.expand)
					       lexenv.run lexenv.expand))
	expr.psi)))

  #| end of module: CHI-EXPR |# )


;;;; chi procedures: closure object application processing

(module PROCESS-CLOSURE-OBJECT-APPLICATION
  (%process-closure-object-application)
  ;;This module is  subordinate to the function CHI-APPLICATION.  Here  we handle the
  ;;special case of closure object application to  a given tuple of operands; here we
  ;;know that the application to process is:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;and ?RATOR will evaluate to a closure object with tag identifier RATOR.TAG, which
  ;;is a sub-tag of "<procedure>".
  ;;
  (define-fluid-override __who__
    (identifier-syntax '%process-closure-object-application))

  (define (%process-closure-object-application input-form.stx lexenv.run lexenv.expand
					       rator.tag rator.psi rand*.psi)
    (define rator.callable
      (tag-identifier-callable-signature rator.tag))
    (cond ((lambda-signature? rator.callable)
	   (%process-lambda-application input-form.stx lexenv.run lexenv.expand
					rator.callable rator.psi rand*.psi))
	  ((clambda-compound? rator.callable)
	   (%process-clambda-application input-form.stx rator.callable rator.psi rand*.psi))
	  (else
	   (%build-core-expression input-form.stx rator.psi rand*.psi))))

  (define (%process-lambda-application input-form.stx lexenv.run lexenv.expand
				       rator.lambda-signature rator.psi rand*.psi)
    (if (%match-rator-signature-against-rand-signatures input-form.stx rator.lambda-signature
							rator.psi rand*.psi)
	;;The signatures do match.
	(let ((rator.stx (psi-stx rator.psi)))
	  (cond ((identifier? rator.stx)
		 (cond ((identifier-unsafe-variant rator.stx)
			=> (lambda (unsafe-rator.stx)
			     (let ((unsafe-rator.psi (chi-expr unsafe-rator.stx lexenv.run lexenv.expand)))
			       (%build-core-expression input-form.stx unsafe-rator.psi rand*.psi))))
		       (else
			(%build-core-expression input-form.stx rator.psi rand*.psi))))
	      (else
	       (%build-core-expression input-form.stx rator.psi rand*.psi))))
      ;;The signatures do not match, but we rely on run-time checking.
      (%build-core-expression input-form.stx rator.psi rand*.psi)))

  (define (%process-clambda-application input-form.stx rator.callable rator.psi rand*.psi)
    ;;FIXME Insert here  something special for CLAMBDA-COMPOUNDs.   (Marco Maggi; Thu
    ;;Apr 10, 2014)
    (%build-core-expression input-form.stx rator.psi rand*.psi))

  (define (%build-core-expression input-form.stx rator.psi rand*.psi)
    (let* ((rator.core (psi-core-expr rator.psi))
	   (rand*.core (map psi-core-expr rand*.psi)))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  rator.core
		  rand*.core)
		(psi-application-retvals-signature rator.psi))))

;;; --------------------------------------------------------------------

  (module CLOSURE-APPLICATION-ERRORS
    (%error-more-operands-than-arguments
     %error-more-arguments-than-operands
     %error-mismatch-between-argument-tag-and-operand-retvals-signature)

    (define-condition-type &expected-arguments-count
	&condition
      make-expected-arguments-count-condition
      expected-arguments-count-condition?
      (count expected-arguments-count))

    (define-condition-type &given-operands-count
	&condition
      make-given-operands-count-condition
      given-operands-count-condition?
      (count given-operands-count))

    (define-condition-type &argument-description
	&condition
      make-argument-description-condition
      argument-description-condition?
      (zero-based-argument-index argument-description-index)
      (expected-argument-tag     argument-description-expected-tag))

    (define-condition-type &operand-retvals-signature
	&condition
      make-operand-retvals-signature-condition
      operand-retvals-signature-condition?
      (signature operand-retvals-signature))

    (define (%error-more-operands-than-arguments input-form.stx expected-arguments-count given-operands-count)
      (%raise-compound-condition-object __who__
	"more given operands than expected arguments"
	input-form.stx
	(condition
	 (make-syntax-violation input-form.stx #f)
	 (make-expected-arguments-count-condition expected-arguments-count)
	 (make-given-operands-count-condition given-operands-count))))

    (define (%error-more-arguments-than-operands input-form.stx expected-arguments-count given-operands-count)
      (%raise-compound-condition-object __who__
	"more expected arguments than given operands"
	input-form.stx
	(condition
	 (make-syntax-violation input-form.stx #f)
	 (make-expected-arguments-count-condition expected-arguments-count)
	 (make-given-operands-count-condition given-operands-count))))

    (define (%error-mismatch-between-argument-tag-and-operand-retvals-signature input-form.stx rand.stx
										arg.idx arg.tag rand.retvals-signature)
      (%raise-compound-condition-object __who__
	"expand-time mismatch between expected argument tag and operand retvals signature"
	input-form.stx
	(condition
	 (make-expand-time-type-signature-violation)
	 (make-syntax-violation input-form.stx rand.stx)
	 (make-argument-description-condition arg.idx arg.tag)
	 (make-operand-retvals-signature-condition rand.retvals-signature))))

    #| end of module: CLOSURE-APPLICATION-ERRORS |# )

;;; --------------------------------------------------------------------

  (define (%match-rator-signature-against-rand-signatures input-form.stx rator.lambda-signature
							  rator.psi rand*.psi)
    ;;In a closure object application: compare  the signature tags of the operator to
    ;;the retvals signatures of the operands:
    ;;
    ;;* If they match at expand-time: return true.
    ;;
    ;;* If  they do  *not* match at  expand-time, but they  could match  at run-time:
    ;;  return false.
    ;;
    ;;* If  they do  *not* match at  expand-time, and will  *not* match  at run-time:
    ;;  raise an exception.
    ;;
    (import CLOSURE-APPLICATION-ERRORS)
    (define rator.formals-signature
      (lambda-signature-formals rator.lambda-signature))
    (let loop ((expand-time-match? #t)
	       (arg*.tag           (formals-signature-tags rator.formals-signature))
	       (rand*.psi          rand*.psi)
	       (count              0))
      (syntax-match arg*.tag ()
	(()
	 (if (null? rand*.psi)
	     expand-time-match?
	   (%error-more-operands-than-arguments input-form.stx
						count (+ count (length rand*.psi)))))

	((?arg.tag . ?rest-arg*.tag)
	 (if (null? rand*.psi)
	     (let ((number-of-mandatory-args (if (list? arg*.tag)
						 (length arg*.tag)
					       (receive (proper tail)
						   (improper-list->list-and-rest arg*.tag)
						 (length proper)))))
	       (%error-more-arguments-than-operands input-form.stx (+ count number-of-mandatory-args) count))
	   (let* ((rand.psi               (car rand*.psi))
		  (rand.retvals-signature (psi-retvals-signature rand.psi))
		  (rand.signature-tags    (retvals-signature-tags rand.retvals-signature)))
	     (syntax-match rand.signature-tags ()
	       ((?rand.tag)
		;;Single return value: good, this is what we want.
		(cond ((top-tag-id? ?rand.tag)
		       ;;The  argument expression  returns a  single "<top>"  object;
		       ;;this  is  what  happens   with  standard  (untagged)  Scheme
		       ;;language.  Let's see at run-time what will happen.
		       (loop #f ?rest-arg*.tag (cdr rand*.psi) (fxadd1 count)))
		      ((tag-super-and-sub? ?arg.tag ?rand.tag)
		       ;;Argument and  operand do match at  expand-time.  Notice that
		       ;;this case  includes the one  of expected argument  tagged as
		       ;;"<top>":  this  is  what happens  with  standard  (untagged)
		       ;;Scheme language.
		       (loop expand-time-match? ?rest-arg*.tag (cdr rand*.psi) (fxadd1 count)))
		      (else
		       ;;Argument and operand do *not* match at expand-time.
		       (%error-mismatch-between-argument-tag-and-operand-retvals-signature input-form.stx
											   (psi-stx rand.psi)
											   count ?arg.tag
											   rand.retvals-signature))))
	       (?rand.tag
		(if (list-tag-id? ?rand.tag)
		    ;;The operand expression returns an unspecified number of objects
		    ;;of unspecified  type of return  values: it could be  one value.
		    ;;Let's accept it for now, and  move on to the other operands: we
		    ;;will see at run-time.
		    (loop #f ?rest-arg*.tag (cdr rand*.psi) (fxadd1 count))
		  ;;The operand expression returns zero or more values of a specified
		  ;;type: we consider this invalid even though if the operand returns
		  ;;a single value the application could succeed.
		  ;;
		  ;;FIXME The  validity of this  rejection must be  verified.  (Marco
		  ;;Maggi; Fri Apr 11, 2014)
		  (%error-mismatch-between-argument-tag-and-operand-retvals-signature input-form.stx
										      (psi-stx rand.psi)
										      count ?arg.tag
										      rand.retvals-signature)))
	       (_
		;;The operand returns multiple values for sure.
		(%error-mismatch-between-argument-tag-and-operand-retvals-signature input-form.stx
										    (psi-stx rand.psi)
										    count ?arg.tag
										    rand.retvals-signature))
	       ))))

	(?arg.tag
	 (if (list-tag-id? ?arg.tag)
	     ;;From now on: the rator accepts any number of operands, of any type.
	     expand-time-match?
	   ;;If we are  here: ?ARG.TAG is a  sub-tag of "<list>".  From  now on: the
	   ;;rator accepts any  number of operands, of a specified  type.  Let's see
	   ;;at run-time what happens.
	   #f))

	(_
	 ;;This should never happen.
	 (assertion-violation/internal-error __who__
	   "invalid closure object operator formals"
	   input-form.stx rator.formals-signature))
	)))

  #| end of module: PROCESS-CLOSURE-OBJECT-APPLICATION |# )


;;;; chi procedures: operator application processing, operator already expanded

(module (chi-application/psi-rator)

  (define* (chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				      {rator.psi psi?} rand*.stx)
    ;;Expand an operator  application form; it is called when  INPUT-FORM.STX has the
    ;;format:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;and ?RATOR is neither a macro keyword identifier, nor a VALUES application, nor
    ;;an  APPLY application,  nor a  SPLICE-FIRST-EXPAND syntax.   For example  it is
    ;;called when INPUT-FORM.STX is:
    ;;
    ;;   (?core-prim ?rand ...)
    ;;   (?datum ?rand ...)
    ;;   ((?sub-rator ?sub-rand ...) ?rand ...)
    ;;
    ;;We call this function when the operator has already been expanded.
    ;;
    (define rator.sig (psi-retvals-signature rator.psi))
    (syntax-match (retvals-signature-tags rator.sig) ()
      (?tag
       (list-tag-id? ?tag)
       ;;The rator type  is unknown: evaluating the rator might  return any number of
       ;;values of any  type.  Return a normal  rator application and we  will see at
       ;;run-time what happens; this is standard Scheme behaviour.
       (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					rator.psi rand*.stx))

      ((?tag)
       ;;The rator type is a single value.  Good, this is what it is meant to be.
       (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					 rator.psi ?tag rand*.stx))

      (_
       ;;The rator  is declared to  evaluate to multiple  values; this is  invalid in
       ;;call  context,  so we  raise  an  exception.   This is  non-standard  Scheme
       ;;behaviour:  according  to the  standard  we  should  insert a  normal  rator
       ;;application and raise an exception at run-time.
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "call operator declared to evaluate to multiple values")
		   (syntax-match input-form.stx ()
		     ((?rator . ?rands)
		      (make-syntax-violation input-form.stx ?rator)))
		   (make-retvals-signature-condition rator.sig))))
      ))

  (define* (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    {rator.psi psi?} rand*.stx)
    ;;Build a core language expression to apply  the rator to the rands; return a PSI
    ;;struct.  This  is an application  form in standard (untagged)  Scheme language.
    ;;We do not know  what the retvals signature of the  application is; the returned
    ;;PSI struct will  have "<list>" as retvals signature, which  means any number of
    ;;values of any type.
    ;;
    (let* ((rator.core (psi-core-expr rator.psi))
	   (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	   (rand*.core (map psi-core-expr rand*.psi)))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  rator.core
		  rand*.core))))

  (define* (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					     {rator.psi psi?} {rator.tag tag-identifier?} rand*.stx)
    ;;Build a  core language expression to  apply the rator  to the rands when  it is
    ;;known  that the  rator will  return  a single  value with  specified tag;  this
    ;;function implements implicit dispatching.  Do the following:
    ;;
    ;;*  If the  rator is  a procedure  return a  PSI struct  as per  standard Scheme
    ;;  behaviour  and, when possible,  select the appropriate retvals  signature for
    ;;  the returned PSI.
    ;;
    ;;* If the  rator is not a  procedure and implicit dispatching is  ON: attempt to
    ;;  match the  input form with the  getter syntax, tag method  application or tag
    ;;  field accessor application.
    ;;
    ;;* If  the rator is not  a procedure and  implicit dispatching is OFF:  return a
    ;;  common Scheme application ad we will se at run-time what happens.
    ;;
    ;;* Otherwise raise a syntax violation.
    ;;
    (cond ((tag-super-and-sub? (procedure-tag-id) rator.tag)
	   ;;The rator is  a procedure: very good; return  a procedure application.
	   (let ((rand*.psi (chi-expr* rand*.stx lexenv.run lexenv.expand)))
	     (import PROCESS-CLOSURE-OBJECT-APPLICATION)
	     (%process-closure-object-application input-form.stx lexenv.run lexenv.expand
						  rator.tag rator.psi rand*.psi)))

	  ((top-tag-id? rator.tag)
	   ;;The rator  type is  unknown, we  only know  that it  is a  single value.
	   ;;Return a procedure application and we will see at run-time what happens.
	   (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    rator.psi rand*.stx))

	  (else
	   ;;The rator  has a correct single-value  signature and it has  a specified
	   ;;tag, but it is not a procedure.
	   (if (option.tagged-language.datums-as-operators?)
	       (%process-datum-operator input-form.stx lexenv.run lexenv.expand
					rator.psi rator.tag rand*.stx)
	     (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					      rator.psi rand*.stx)))))

  (define* (%process-datum-operator input-form.stx lexenv.run lexenv.expand
				    {rator.psi psi?} {rator.tag tag-identifier?} rand*.stx)
    ;;When the "datums  as operators" option is on, there  are multiple cases covered
    ;;by this procedure; examples:
    ;;
    ;;(123 positive?)	=> #t
    ;;
    ;;   The  rator is the  fixnum 123  and the expander  determines that its  tag is
    ;;    "<fixnum>";  this  form  matches  the   syntax  of  a  method  or  accessor
    ;;   application.  Here we retrieve the method dispatcher of "<fixnum>" and apply
    ;;   it to the symbol "positive?".
    ;;
    ;;("ciao" [1])		=> #\i
    ;;
    ;;   The rator is  the string "ciao" and the expander determines  that its tag is
    ;;   "<string>"; this  form matches the syntax of a  getter application.  Here we
    ;;   retrieve  the getter  maker of "<string>"  and apply it  to the  keys syntax
    ;;   object "[1]".
    ;;
    (syntax-match rand*.stx ()
      (()
       ;;No operands.  This is an error, because the input form is something like:
       ;;
       ;;   ("ciao mamma")
       ;;
       ;;so we raise an exception.
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "invalid datum call operator with no operands")
		   (make-syntax-violation input-form.stx #f)
		   (make-retvals-signature-condition (psi-retvals-signature rator.psi)))))

      (((?key00 ?key0* ...) (?key11* ?key1** ...) ...)
       ;;There are operands and they match the getter keys syntax.
       (let* ((getter.stx  (tag-identifier-getter rator.tag rand*.stx input-form.stx))
	      (getter.psi  (while-not-expanding-application-first-subform
			    (chi-expr getter.stx lexenv.run lexenv.expand)))
	      (getter.core (psi-core-expr getter.psi))
	      (rator.core  (psi-core-expr rator.psi)))
	 (make-psi input-form.stx
		   (build-application (syntax-annotation input-form.stx)
		     getter.core
		     (list rator.core))
		   (psi-application-retvals-signature getter.psi))))

      ((?member ?arg* ...)
       (identifier? ?member)
       ;;There are  operands and  they match  the syntax for  a tag  dispatcher call,
       ;;that's great.
       (let ((method.stx (tag-identifier-dispatch rator.tag ?member input-form.stx)))
	 (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					    method.stx rator.psi ?arg*)))

      (_
       ;;There are operands, but they do not match any of the supported syntaxes; for
       ;;example the input form may be:
       ;;
       ;;   ("ciao" "mamma")
       ;;
       ;;so we raise an exception.
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "invalid operands for non-procedure operator")
		   (make-syntax-violation input-form.stx #f)
		   (make-retvals-signature-condition (psi-retvals-signature rator.psi)))))
      ))

  #| end of module: CHI-APPLICATION/PSI-RATOR |# )


;;;; chi procedures: operator application processing, first operand already expanded

(module (chi-application/psi-first-operand)

  (define* (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					      rator.stx {first-rand.psi psi?} other-rand*.stx)
    ;;Expand an operator  application form; it is called when  application to process
    ;;has the format:
    ;;
    ;;   (?rator ?first-rand ?other-rand ...)
    ;;
    ;;and ?FIRST-RAND has already been expanded.  Here we know that the input form is
    ;;a special syntax  like IS-A?, SLOT-REF, SLOT-SET! and similar;  so the operator
    ;;will expand into a predicate, accessor, mutator or method object.
    ;;
    (define rator.psi (while-not-expanding-application-first-subform
		       (chi-expr rator.stx lexenv.run lexenv.expand)))
    (define rator.sig (psi-retvals-signature rator.psi))
    (syntax-match (retvals-signature-tags rator.sig) ()
      (?tag
       (list-tag-id? ?tag)
       ;;The rator type  is unknown: evaluating the rator might  return any number of
       ;;values of any  type.  Return a normal  rator application and we  will see at
       ;;run-time what happens; this is standard Scheme behaviour.
       (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					rator.psi first-rand.psi other-rand*.stx))

      ((?tag)
       ;;The rator type is a single value.  Good, this is what it is meant to be.
       (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					 rator.psi ?tag first-rand.psi other-rand*.stx))

      (_
       ;;The rator  is declared to  evaluate to multiple  values; this is  invalid in
       ;;call  context,  so we  raise  an  exception.   This is  non-standard  Scheme
       ;;behaviour:  according  to the  standard  we  should  insert a  normal  rator
       ;;application and raise an exception at run-time.
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "call operator declared to evaluate to multiple values")
		   (syntax-match input-form.stx ()
		     ((?rator . ?rands)
		      (make-syntax-violation input-form.stx ?rator)))
		   (make-retvals-signature-condition rator.sig))))
      ))

  (define* (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    {rator.psi psi?} {first-rand.psi psi?} other-rand*.stx)
    ;;Build a core language expression to apply  the rator to the rands; return a PSI
    ;;struct.  This  is an application  form in standard (untagged)  Scheme language.
    ;;We do not know  what the retvals signature of the  application is; the returned
    ;;PSI struct will  have "<list>" as retvals signature, which  means any number of
    ;;values of any type.
    ;;
    (let* ((rator.core       (psi-core-expr rator.psi))
	   (first-rand.core  (psi-core-expr first-rand.psi))
	   (other-rand*.psi  (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
	   (other-rand*.core (map psi-core-expr other-rand*.psi)))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  rator.core
		  (cons first-rand.core other-rand*.core)))))

  (define* (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					     {rator.psi psi?} {rator.tag tag-identifier?}
					     {first-rand.psi psi?} other-rand*.stx)
    ;;Build a  core language expression to  apply the rator  to the rands when  it is
    ;;known that the rator will return a single value with specified tag.
    ;;
    ;;Notice that it is an error if the rator is a datum: since the first operand has
    ;;already  been  expanded,  it  is   impossible  to  correctly  process  implicit
    ;;dispatching for datums.
    ;;
    (cond ((tag-super-and-sub? (procedure-tag-id) rator.tag)
	   ;;The rator is a procedure: very good; return a procedure application.
	   (let* ((other-rand*.psi (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
		  (rand*.psi       (cons first-rand.psi other-rand*.psi)))
	     (import PROCESS-CLOSURE-OBJECT-APPLICATION)
	     (%process-closure-object-application input-form.stx lexenv.run lexenv.expand
						  rator.tag rator.psi rand*.psi)))

	  ((top-tag-id? rator.tag)
	   ;;The rator  type is  unknown, we  only know  that it  is a  single value.
	   ;;Return a procedure application and we will see at run-time what happens.
	   (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    rator.psi first-rand.psi other-rand*.stx))

	  (else
	   ;;The rator  has a correct single-value  signature and it has  a specified
	   ;;tag, but it is not a procedure.
	   (syntax-violation __who__
	     "expansion results in datum call operator, invalid for this form"
	     input-form.stx (psi-core-expr rator.psi)))))

  #| end of module: CHI-APPLICATION/PSI-FIRST-OPERAND |# )


;;;; chi procedures: general operator application, nothing already expanded

(module (chi-application)
  (define-fluid-override __who__
    (identifier-syntax (quote chi-application)))

  (define (chi-application input-form.stx lexenv.run lexenv.expand)
    ;;Expand an operator  application form; it is called when  INPUT-FORM.STX has the
    ;;format:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;and ?RATOR is *not* a macro keyword  identifier.  For example it is called when
    ;;INPUT-FORM.STX is:
    ;;
    ;;   (?core-prim ?rand ...)
    ;;   (?datum ?rand ...)
    ;;   ((?sub-rator ?sub-rand ...) ?rand ...)
    ;;
    ;;and the sub-application form can be a SPLICE-FIRST-EXPAND syntax.
    ;;
    (syntax-match input-form.stx (values apply map1 for-each1 for-all1 exists1)
      (((?nested-rator ?nested-rand* ...) ?rand* ...)
       ;;Nested application.  We process this specially because the nested rator form
       ;;might be a SPLICE-FIRST-EXPAND syntax.
       (%chi-nested-rator-application input-form.stx lexenv.run lexenv.expand
				      (cons ?nested-rator ?nested-rand*) ?rand*))

      ((values ?rand* ...)
       ;;A  call to  VALUES is  special  because VALUES  does not  have a  predefined
       ;;retvals signature, but the retvals signature equals the arguments signature.
       (%chi-values-application input-form.stx lexenv.run lexenv.expand
				?rand*))

      ((apply ?rator ?rand* ...)
       (%chi-apply-application input-form.stx lexenv.run lexenv.expand
       			       ?rator ?rand*))

      ((map1 ?func ?list)
       (expander-option.integrate-special-list-functions?)
       ;;This is to be considered experimental.  The purpose of this code integration
       ;;it  not  to  integrate  the  list  iteration  function,  but  to  allow  the
       ;;integration of ?FUNC.
       (chi-expr (bless
		  `(let map1 ((L ,?list)
			      (H #f)  ;head
			      (T #f)) ;last pair
		     (cond ((pair? L)
			    (let* ((V (,?func ($car L))) ;value
				   (P (cons V '()))	 ;new last pair
				   (T (if T
					  (begin
					    ($set-cdr! T P)
					    P)
					P))
				   (H (or H P)))
			      (map1 ($cdr L) H T)))
			   ((null? L)
			    (or H '()))
			   (else
			    (procedure-argument-violation 'map1 "expected proper list as argument" L)))))
		 lexenv.run lexenv.expand))

      ((for-each1 ?func ?list)
       (expander-option.integrate-special-list-functions?)
       ;;This is to be considered experimental.  The purpose of this code integration
       ;;it  not  to  integrate  the  list  iteration  function,  but  to  allow  the
       ;;integration of ?FUNC.
       (chi-expr (bless
		  `(let for-each1 ((L ,?list))
		     (cond ((pair? L)
			    (,?func ($car L))
			    (for-each1 ($cdr L)))
			   ((null? L)
			    (void))
			   (else
			    (procedure-argument-violation 'for-each1 "expected proper list as argument" L)))))
		 lexenv.run lexenv.expand))

      ((for-all1 ?func ?list)
       (expander-option.integrate-special-list-functions?)
       ;;This is to be considered experimental.  The purpose of this code integration
       ;;it  not  to  integrate  the  list  iteration  function,  but  to  allow  the
       ;;integration of ?FUNC.
       (chi-expr (bless
		  `(let for-all1 ((L ,?list)
				  (R #t))
		     (if (pair? L)
			 (let ((R (,?func ($car L))))
			   (and R (for-all1 ($cdr L) R)))
		       R)))
		 lexenv.run lexenv.expand))

      ((exists1 ?func ?list)
       (expander-option.integrate-special-list-functions?)
       ;;This is to be considered experimental.  The purpose of this code integration
       ;;it  not  to  integrate  the  list  iteration  function,  but  to  allow  the
       ;;integration of ?FUNC.
       (chi-expr (bless
		  `(let exists1 ((L ,?list))
		     (and (pair? L)
			  (or (,?func ($car L))
			      (exists1 ($cdr L))))))
		 lexenv.run lexenv.expand))

      ((?rator ?rand* ...)
       ;;The input form is either a common function application like:
       ;;
       ;;   (list 1 2 3)
       ;;
       ;;or an expression application like:
       ;;
       ;;   ((lambda (a b) (+ a b)) 1 2)
       ;;
       ;;or a datum operator application like:
       ;;
       ;;   ("ciao" length)
       ;;
       (let ((rator.psi (chi-expr ?rator lexenv.run lexenv.expand)))
	 (chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				    rator.psi ?rand*)))

      (_
       (syntax-violation/internal-error __who__
	 "invalid application syntax" input-form.stx))))

  (define (%chi-nested-rator-application input-form.stx lexenv.run lexenv.expand
					 rator.stx rand*.stx)
    ;;Here the input form is:
    ;;
    ;;  ((?nested-rator ?nested-rand* ...) ?rand* ...)
    ;;
    ;;where the nested rator  can be anything: a closure application,  a macro use, a
    ;;datum  operator  application;  we  expand  it considering  the  case  of  rator
    ;;expanding to a SPLICE-FIRST-EXPAND form.
    ;;
    ;;RATOR.STX is the syntax object:
    ;;
    ;;  #'(?nested-rator ?nested-rand* ...)
    ;;
    ;;and RAND*.STX is the list of syntax objects:
    ;;
    ;;  (#'?rand ...)
    ;;
    (let* ((rator.psi  (while-expanding-application-first-subform
			(chi-expr rator.stx lexenv.run lexenv.expand)))
	   (rator.expr (psi-core-expr rator.psi)))
      ;;Here RATOR.EXPR  is either an  instance of "splice-first-envelope" or  a core
      ;;language sexp.
      (import SPLICE-FIRST-ENVELOPE)
      (if (splice-first-envelope? rator.expr)
	  (syntax-match (splice-first-envelope-form rator.expr) ()
	    ((?nested-rator ?nested-rand* ...)
	     (chi-expr (cons ?nested-rator (append ?nested-rand* rand*.stx))
		       lexenv.run lexenv.expand))
	    (_
	     (syntax-violation __who__
	       "expected list as argument of splice-first-expand"
	       input-form.stx rator.stx)))
	(chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				   rator.psi rand*.stx))))

;;; --------------------------------------------------------------------

  (define (%chi-values-application input-form.stx lexenv.run lexenv.expand
				   rand*.stx)
    ;;The input form has the syntax:
    ;;
    ;;   (values ?rand ...)
    ;;
    ;;and RAND*.STX is the syntax object:
    ;;
    ;;   (#'?rand ...)
    ;;
    ;;A call to VALUES  is special because VALUES does not  have a predefined retvals
    ;;signature, but the retvals signature equals the operands' signature.
    ;;
    (let* ((rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	   (rand*.core (map psi-core-expr rand*.psi))
	   (rand*.sig  (map psi-retvals-signature rand*.psi))
	   (rator.core (build-primref no-source 'values)))
      (define application.sig
	(let loop ((rand*.sig rand*.sig)
		   (rand*.stx rand*.stx)
		   (rand*.tag '()))
	  (if (pair? rand*.sig)
	      ;;To be a valid VALUES argument an expression must have as signature:
	      ;;
	      ;;   (?tag)
	      ;;
	      ;;which  means a  single return  value.  Here  we allow  "<list>", too,
	      ;;which means  we accept an  argument having unknown  retval signature,
	      ;;and we will see what happens at run-time; we accept only "<list>", we
	      ;;reject a signature if it is a standalone sub-tag of "<list>".
	      ;;
	      ;;NOTE We  could reject "<list>"  as argument signature and  demand the
	      ;;caller of  VALUES to cast  the arguments, but  it would be  too much;
	      ;;remember that we still do  some signature validation even when tagged
	      ;;language support is off.  (Marco Maggi; Mon Mar 31, 2014)
	      (syntax-match (retvals-signature-tags (car rand*.sig)) ()
		((?tag)
		 (loop (cdr rand*.sig) (cdr rand*.stx)
		       (cons ?tag rand*.tag)))
		(?tag
		 (list-tag-id? ?tag)
		 (loop (cdr rand*.sig) (cdr rand*.stx)
		       (cons (top-tag-id) rand*.tag)))
		(_
		 (expand-time-retvals-signature-violation 'values
							  input-form.stx (car rand*.stx)
							  (make-retvals-signature-single-top)
							  (car rand*.sig))))
	    (make-retvals-signature (reverse rand*.tag)))))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  rator.core
		  rand*.core)
		application.sig)))

;;; --------------------------------------------------------------------

  (module (%chi-apply-application)

    (define (%chi-apply-application input-form.stx lexenv.run lexenv.expand
				    rator.stx rand*.stx)
      (let* ((rator.psi (chi-expr rator.stx lexenv.run lexenv.expand))
	     (rator.sig (psi-retvals-signature rator.psi)))
	(define (%error-wrong-rator-sig)
	  (expand-time-retvals-signature-violation 'apply input-form.stx rator.stx
						   (make-retvals-signature-single-value (procedure-tag-id))
						   rator.sig))
	(syntax-match (retvals-signature-tags rator.sig) ()
	  ((?rator.tag)
	   (cond ((tag-super-and-sub? (procedure-tag-id) ?rator.tag)
		  ;;Procedure application: good.
		  (let* ((apply.core (build-primref no-source 'apply))
			 (rator.core (psi-core-expr rator.psi))
			 (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
			 (rand*.core (map psi-core-expr rand*.psi)))
		    (make-psi input-form.stx
			      (build-application (syntax-annotation input-form.stx)
				apply.core
				(cons rator.core rand*.core))
			      (psi-application-retvals-signature rator.psi))))
		 ((top-tag-id? ?rator.tag)
		  ;;Let's do it and we will see at run-time what happens.  Notice that,
		  ;;in this case: we do not know the signature of the return values.
		  (%build-application-no-signature input-form.stx lexenv.run lexenv.expand
						   rator.psi rand*.stx))
		 (else
		  ;;Non-procedure: bad.
		  (%error-wrong-rator-sig))))

	  (?rator.tag
	   (list-tag-id? ?rator.tag)
	   ;;Fully  unspecified return  values.   Let's  do it  and  we  will see  at
	   ;;run-time what  happens.  Notice that, in  this case: we do  not know the
	   ;;signature of the return values.
	   (%build-application-no-signature input-form.stx lexenv.run lexenv.expand
					    rator.psi rand*.stx))

	  (_
	   ;;Everything else is wrong.
	   (%error-wrong-rator-sig)))))

    (define (%build-application-no-signature input-form.stx lexenv.run lexenv.expand
					     rator.psi rand*.stx)
      (let* ((apply.core (build-primref no-source 'apply))
	     (rator.core (psi-core-expr rator.psi))
	     (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	     (rand*.core (map psi-core-expr rand*.psi)))
	(make-psi input-form.stx
		  (build-application (syntax-annotation input-form.stx)
		    apply.core
		    (cons rator.core rand*.core)))))

    #| end of module: %CHI-APPLY-APPLICATION |# )

  #| end of module: CHI-APPLICATION |# )


;;;; chi procedures: SET! syntax

(module CHI-SET
  (chi-set!)

  (define (chi-set! input-form.stx lexenv.run lexenv.expand)
    (while-not-expanding-application-first-subform
     (syntax-match input-form.stx ()
       ((_ (?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ...) ?new-value)
	(option.tagged-language.setter-forms?)
	(let* ((keys.stx  (cons (cons ?key00 ?key0*)
				(map cons ?key11* ?key1**))))
	  (chi-expr (bless
		     `(tag-setter ,?expr ,keys.stx ,?new-value))
		    lexenv.run lexenv.expand)))

       ((_ ?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ... ?new-value)
	(option.tagged-language.setter-forms?)
	(let* ((keys.stx  (cons (cons ?key00 ?key0*)
				(map cons ?key11* ?key1**))))
	  (chi-expr (bless
		     `(tag-setter ,?expr ,keys.stx ,?new-value))
		    lexenv.run lexenv.expand)))

       ((_ (?expr ?field-name) ?new-value)
	(and (option.tagged-language.setter-forms?)
	     (identifier? ?field-name))
	(chi-expr (bless
		   `(tag-mutator ,?expr ,?field-name ,?new-value))
		  lexenv.run lexenv.expand))

       ((_ ?lhs ?rhs)
	(identifier? ?lhs)
	(%chi-set-identifier input-form.stx lexenv.run lexenv.expand
			     ?lhs ?rhs))

       (_
	(syntax-violation 'set! "invalid setter syntax" input-form.stx)))))

  (define (%chi-set-identifier input-form.stx lexenv.run lexenv.expand lhs.id rhs.stx)
    (receive (type bind-val kwd)
	(expr-syntax-type lhs.id lexenv.run)
      (case type
	((lexical)
	 ;;A  lexical binding  used as  LHS of  SET! is  mutable and  so unexportable
	 ;;according to R6RS.  Example:
	 ;;
	 ;;   (library (demo)
	 ;;     (export var)		;error!!!
	 ;;     (import (rnrs))
	 ;;     (define var 1)
	 ;;     (set! var 2))
	 ;;
	 (set-lexical-mutable! bind-val)
	 (let* ((lhs.tag (or (identifier-tag lhs.id)
			     (top-tag-id)))
		(rhs.psi (chi-expr (bless
				    `(tag-assert-and-return (,lhs.tag) ,rhs.stx))
				   lexenv.run lexenv.expand)))
	   (make-psi input-form.stx
		     (build-lexical-assignment no-source
		       (lexical-var bind-val)
		       (psi-core-expr rhs.psi))
		     (make-retvals-signature-single-top))))

	((core-prim)
	 (syntax-violation 'set! "cannot modify imported core primitive" input-form.stx lhs.id))

	((global)
	 (syntax-violation 'set! "attempt to modify an immutable binding" input-form.stx lhs.id))

	((global-macro!)
	 (chi-expr (chi-global-macro bind-val input-form.stx lexenv.run #f) lexenv.run lexenv.expand))

	((local-macro!)
	 (chi-expr (chi-local-macro bind-val input-form.stx lexenv.run #f) lexenv.run lexenv.expand))

	((mutable)
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
	 ;;in which VAR is mutable and not exportable according to R6RS; if we import
	 ;;the library in a program and use MACRO:
	 ;;
	 ;;   (import (vicare) (demo))
	 ;;   (macro)
	 ;;
	 ;;we are  attempting to  mutate an unexportable  binding in  another lexical
	 ;;context.  This is forbidden by R6RS.
	 ;;
	 (if (and (pair? bind-val)
		  (let ((lib (car bind-val)))
		    (eq? lib '*interaction*)))
	     (let* ((loc     (cdr bind-val))
		    (lhs.tag (or (identifier-tag lhs.id)
				 (top-tag-id)))
		    (rhs.psi (chi-expr (bless
					`(tag-assert-and-return (,lhs.tag) ,rhs.stx))
				       lexenv.run lexenv.expand)))
	       (make-psi input-form.stx
			 (build-global-assignment no-source
			   loc
			   (psi-core-expr rhs.psi))
			 (make-retvals-signature-single-top)))
	   (syntax-violation 'set!
	     "attempt to modify a variable imported from another lexical context"
	     input-form.stx lhs.id)))

	(else
	 (stx-error input-form.stx)))))

  #| end of module: CHI-SET |# )


;;;; chi procedures: lambda clauses

(module CHI-LAMBDA-CLAUSES
  (%chi-lambda-clause
   %chi-lambda-clause*)

  (define* (%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			       attributes.sexp formals.stx body-form*.stx)
    ;;Expand  the components  of  a LAMBDA  syntax or  a  single CASE-LAMBDA  clause.
    ;;Return 3  values: a  proper or  improper list of  lex gensyms  representing the
    ;;formals; an instance  of "lambda-signature" representing the  tag signature for
    ;;this  LAMBDA   clause;  a  PSI   struct  containint  the   language  expression
    ;;representing the body of the clause.
    ;;
    ;;A LAMBDA or CASE-LAMBDA clause defines a lexical contour; so we build a new rib
    ;;for it, initialised with the id/label  associations of the formals; we push new
    ;;lexical bindings on LEXENV.RUN.
    ;;
    ;;NOTE The expander for the internal body will create yet another lexical contour
    ;;to hold the body's internal definitions.
    ;;
    ;;When the formals are tagged, we want to transform:
    ;;
    ;;   (lambda ({_ <symbol>} {a <fixnum>} {b <string>})
    ;;     ?body ... ?last-body)
    ;;
    ;;into:
    ;;
    ;;   (lambda (a b)
    ;;     (tag-procedure-argument-validator <fixnum> a)
    ;;     (tag-procedure-argument-validator <string> b)
    ;;     (internal-body
    ;;       ?body ...
    ;;       (tag-assert-and-return (<symbol>) ?last-body)))
    ;;
    (receive (standard-formals.stx lambda-signature)
	(parse-tagged-lambda-proto-syntax formals.stx input-form.stx)
      (define formals-signature.tags
	(lambda-signature-formals-tags lambda-signature))
      (define (%override-retvals-singature lambda-signature body.psi)
	;;If  unspecified: override  the retvals  signature  of the  lambda with  the
	;;retvals signature of the body.
	(if (retvals-signature-fully-unspecified? (lambda-signature-retvals lambda-signature))
	    (make-lambda-signature (psi-retvals-signature body.psi)
				   (lambda-signature-formals lambda-signature))
	  lambda-signature))
      (syntax-match standard-formals.stx ()
	;;Without rest argument.
	((?arg* ...)
	 (let* ((lex*        (map gensym-for-lexical-var ?arg*))
		(lab*        (map gensym-for-label       ?arg*))
		(lexenv.run^ (add-lexical-bindings lab* lex* lexenv.run)))
	   (define validation*.stx
	     (if (lambda-clause-attributes:safe-formals? attributes.sexp)
		 (%build-formals-validation-form* ?arg* formals-signature.tags #f #f)
	       '()))
	   (define has-arguments-validators?
	     (not (null? validation*.stx)))
	   (define body-form^*.stx
	     (push-lexical-contour
		 (make-filled-rib ?arg* lab*)
	       (append validation*.stx
		       (if (lambda-clause-attributes:safe-retvals? attributes.sexp)
			   (%build-retvals-validation-form has-arguments-validators?
							   (lambda-signature-retvals lambda-signature)
							   body-form*.stx)
			 body-form*.stx))))
	   ;;Here  we  know that  the  formals  signature is  a  proper  list of  tag
	   ;;identifiers with the same structure of FORMALS.STX.
	   (map set-label-tag! lab* formals-signature.tags)
	   (let ((body.psi (chi-internal-body body-form^*.stx lexenv.run^ lexenv.expand)))
	     (values lex* (%override-retvals-singature lambda-signature body.psi) body.psi))))

	;;With rest argument.
	((?arg* ... . ?rest-arg)
	 (let* ((lex*         (map gensym-for-lexical-var ?arg*))
		(lab*         (map gensym-for-label       ?arg*))
		(rest-lex     (gensym-for-lexical-var ?rest-arg))
		(rest-lab     (gensym-for-label       ?rest-arg))
		(lexenv.run^  (add-lexical-bindings (cons rest-lab lab*)
						    (cons rest-lex lex*)
						    lexenv.run)))
	   (receive (arg-tag* rest-tag)
	       (improper-list->list-and-rest formals-signature.tags)
	     (define validation*.stx
	       (if (lambda-clause-attributes:safe-formals? attributes.sexp)
		   (%build-formals-validation-form* ?arg* arg-tag* ?rest-arg rest-tag)
		 '()))
	     (define has-arguments-validators?
	       (not (null? validation*.stx)))
	     (define body-form^*.stx
	       (push-lexical-contour
		   (make-filled-rib (cons ?rest-arg ?arg*)
				    (cons rest-lab  lab*))
		 (append validation*.stx
			 (if (lambda-clause-attributes:safe-retvals? attributes.sexp)
			     (%build-retvals-validation-form has-arguments-validators?
							     (lambda-signature-retvals lambda-signature)
							     body-form*.stx)
			   body-form*.stx))))
	     ;;Here we know  that the formals signature is an  improper list with the
	     ;;same structure of FORMALS.STX.
	     (map set-label-tag! lab* arg-tag*)
	     (set-label-tag! rest-lab rest-tag)
	     (let ((body.psi (chi-internal-body body-form^*.stx lexenv.run^ lexenv.expand)))
	       (values (append lex* rest-lex) ;yes, this builds an improper list
		       (%override-retvals-singature lambda-signature body.psi)
		       body.psi)))))

	(_
	 (syntax-violation __who__
	   "invalid lambda formals syntax" input-form.stx formals.stx)))))

;;; --------------------------------------------------------------------

  (define* (%chi-lambda-clause* input-form.stx lexenv.run lexenv.expand
				attributes.sexp formals*.stx body-form**.stx)
    ;;Expand all the clauses of a CASE-LAMBDA syntax, return 2 values:
    ;;
    ;;1..A list  of subslist,  each sublist being  a proper or  improper list  of lex
    ;;   gensyms representing the formals.
    ;;
    ;;2..A list of  "lambda-signature" instances representing the  signatures of each
    ;;   clause.
    ;;
    ;;3..A  list  of   PSI  structs  each  containing  a   core  language  expression
    ;;   representing the body of a clause.
    ;;
    (if (null? formals*.stx)
	(values '() '() '())
      (receive (formals-lex lambda-signature body.psi)
	  (%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			      attributes.sexp (car formals*.stx) (car body-form**.stx))
	(receive (formals-lex* lambda-signature* body*.psi)
	    (%chi-lambda-clause* input-form.stx lexenv.run lexenv.expand
				 attributes.sexp (cdr formals*.stx) (cdr body-form**.stx))
	  (values (cons formals-lex       formals-lex*)
		  (cons lambda-signature  lambda-signature*)
		  (cons body.psi          body*.psi))))))

;;; --------------------------------------------------------------------

  (define (%build-formals-validation-form* arg* tag* rest-arg rest-tag)
    ;;Build and return a list of syntax objects representing expressions like:
    ;;
    ;;   (tag-procedure-argument-validator ?tag ?arg)
    ;;
    ;;excluding the  formals in which the  tag is "<top>", whose  argument are always
    ;;valid.  When there is no rest argument: REST-ARG and REST-TAG must be #f.
    ;;
    (define-syntax-rule (%recur)
      (%build-formals-validation-form* ($cdr arg*) ($cdr tag*) rest-arg rest-tag))
    (cond ((pair? arg*)
	   (if (top-tag-id? ($car tag*))
	       (%recur)
	     (cons (bless
		    `(tag-procedure-argument-validator ,($car tag*) ,($car arg*)))
		   (%recur))))
	  ;;If there  is no rest  or args  argument or the  rest or args  argument is
	  ;;tagged as "<list>":  there is no need to include  an argument validation.
	  ;;It is obvious that in LAMBDAs like:
	  ;;
	  ;;   (lambda args       123)
	  ;;   (lambda (a . rest) 123)
	  ;;
	  ;;the formals ARGS and REST are tagged  as "<list>" and they will always be
	  ;;lists; so there is no need to validate them.
	  ((or (not rest-tag)
	       (list-tag-id? rest-tag))
	   '())
	  (else
	   (list (bless
		  `(tag-procedure-argument-validator ,rest-tag ,rest-arg))))))

  (define (%build-retvals-validation-form has-arguments-validators? retvals-signature body-form*.stx)
    ;;Add the return values validation to the last form in the body; return a list of
    ;;body forms.
    ;;
    ;;When  there  are  arguments  validators:  the body  forms  are  wrapped  in  an
    ;;INTERNAL-BODY to  create an internal  lexical scope.   This is far  better than
    ;;wrapping into a LET, which would expand into a nested LAMBDA.
    ;;
    ;;The  argument HAS-ARGUMENTS-VALIDATORS?   is  required  to avoid  INTERNAL-BODY
    ;;wrapping when not needed; this gains a bit of speed when expanding the body.
    ;;
    (cond (has-arguments-validators?
	   (if (retvals-signature-fully-unspecified? retvals-signature)
	       ;;The number and type of return values is unknown.
	       (bless
		`((internal-body . ,body-form*.stx)))
	     (receive (head*.stx last.stx)
		 (proper-list->head-and-last body-form*.stx)
	       (bless
		`((internal-body
		    ,@head*.stx
		    (tag-assert-and-return ,(retvals-signature-tags retvals-signature) ,last.stx)))))))
	  (else
	   (if (retvals-signature-fully-unspecified? retvals-signature)
	       ;;The number and type of return values is unknown.
	       body-form*.stx
	     (receive (head*.stx last.stx)
		 (proper-list->head-and-last body-form*.stx)
	       (append head*.stx
		       (bless
			`((tag-assert-and-return ,(retvals-signature-tags retvals-signature) ,last.stx)))))))))

  #| end of module: CHI-LAMBDA-CLAUSES |# )


;;;; chi procedures: function definitions and lambda syntaxes

(module (chi-defun)
  (import CHI-LAMBDA-CLAUSES)

  (define (chi-defun input-form.stx lexenv.run lexenv.expand)
    ;;Expand a  syntax object representing a  INTERNAL-DEFINE syntax for the  case of
    ;;function definition.   Return an expanded language  expression representing the
    ;;expanded definition.
    ;;
    ;;The  returned expression  will  be  coupled (by  the  caller)  with an  already
    ;;generated  lex gensym  serving as  lexical variable  name; for  this reason  we
    ;;return a lambda core form rather than a define core form.
    ;;
    ;;NOTE This function assumes the INPUT-FORM.STX  has already been parsed, and the
    ;;binding for ?CTXT has already been added to LEXENV by the caller.
    ;;
    (syntax-match input-form.stx (brace)
      ((_ ?attributes ((brace ?ctxt ?rv-tag* ... . ?rest-rv-tag) . ?fmls) . ?body-form*)
       (let ((formals.stx (bless
			   `((brace _ ,@?rv-tag* . ,?rest-rv-tag) . ,?fmls)))
	     (body*.stx   (cons (bless
				 `(define-fluid-override __who__
				    (identifier-syntax (quote ,?ctxt))))
				?body-form*)))
	 (%expand input-form.stx lexenv.run lexenv.expand
		  (syntax->datum ?attributes) ?ctxt formals.stx body*.stx)))

      ((_ ?attributes (?ctxt . ?fmls) . ?body-form*)
       (let ((formals.stx ?fmls)
	     (body*.stx   (cons (bless
				 `(define-fluid-override __who__
				    (identifier-syntax (quote ,?ctxt))))
				?body-form*)))
	 (%expand input-form.stx lexenv.run lexenv.expand
		  (syntax->datum ?attributes) ?ctxt formals.stx body*.stx)))
      ))

  (define (%expand input-form.stx lexenv.run lexenv.expand
		   attributes.sexp ctxt.id formals.stx body*.stx)
    ;;This procedure is  like CHI-LAMBDA, but, in addition, it  puts CTXT.ID in the
    ;;core language LAMBDA sexp's annotation.
    (receive (formals.core lambda-signature body.psi)
	(%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			    attributes.sexp formals.stx body*.stx)
      ;;FORMALS.CORE is composed of lex gensyms.
      (make-psi input-form.stx
		(build-lambda (syntax-annotation ctxt.id)
		  formals.core
		  (psi-core-expr body.psi))
		(make-retvals-signature-with-fabricated-procedure-tag (syntax->datum ctxt.id) lambda-signature))))

  #| end of module: CHI-DEFUN |# )

;;; --------------------------------------------------------------------

(define* (chi-lambda input-form.stx lexenv.run lexenv.expand
		     attributes.stx formals.stx body*.stx)
  ;;Expand the contents of CASE syntax and return a "psi" struct.
  ;;
  ;;INPUT-FORM.STX is a syntax object representing the original LAMBDA expression.
  ;;
  ;;FORMALS.STX is a syntax object representing the formals of the LAMBDA syntax.
  ;;
  ;;BODY*.STX is  a list of syntax  objects representing the body  expressions in the
  ;;LAMBDA syntax.
  ;;
  (import CHI-LAMBDA-CLAUSES)
  (define attributes.sexp (syntax->datum attributes.stx))
  (receive (formals.lex lambda-signature body.psi)
      (%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			  attributes.sexp formals.stx body*.stx)
    (make-psi input-form.stx
	      (build-lambda (syntax-annotation input-form.stx)
		formals.lex
		(psi-core-expr body.psi))
	      (make-retvals-signature-with-fabricated-procedure-tag (gensym) lambda-signature))))

(define* (chi-case-lambda input-form.stx lexenv.run lexenv.expand
			  attributes.stx formals*.stx body**.stx)
  ;;Expand the clauses of a CASE-LAMBDA syntax and return a "psi" struct.
  ;;
  ;;INPUT-FORM.STX  is   a  syntax  object  representing   the  original  CASE-LAMBDA
  ;;expression.
  ;;
  ;;FORMALS*.STX is  a list  of syntax  objects whose  items are  the formals  of the
  ;;CASE-LAMBDA clauses.
  ;;
  ;;BODY**.STX  is a  list  of syntax  objects  whose  items are  the  bodies of  the
  ;;CASE-LAMBDA clauses.
  ;;
  ;;Example, for the input form:
  ;;
  ;;   (case-lambda ((a b c) body1) ((d e f) body2))
  ;;
  ;;this function is invoked as:
  ;;
  ;;   (chi-case-lambda
  ;;    #'(case-lambda ((a b c) body1) ((d e f) body2))
  ;;    (list #'(a b c) #'(d e f))
  ;;    (list #'(body1) #'(body2))
  ;;    lexenv.run lexenv.expand)
  ;;
  (import CHI-LAMBDA-CLAUSES)
  (define attributes.sexp (syntax->datum attributes.stx))
  (receive (formals*.lex lambda-signature* body**.psi)
      (%chi-lambda-clause* input-form.stx lexenv.run lexenv.expand
			   attributes.sexp formals*.stx body**.stx)
    (make-psi input-form.stx
	      (build-case-lambda (syntax-annotation input-form.stx)
		formals*.lex
		(map psi-core-expr body**.psi))
	      (make-retvals-signature-with-fabricated-procedure-tag (gensym) (make-clambda-compound lambda-signature*)))))


;;;; chi procedures: lexical bindings qualified right-hand sides
;;
;;A "qualified right-hand side expression" is a pair whose car is a symbol specifying
;;the  type of  the expression  and whose  cdr is  a syntax  object representing  the
;;right-hand side expression of a lexical binding definition.
;;
;;For  example, when  CHI-BODY*  expands a  body that  allows  mixed definitions  and
;;expressions:
;;
;;   (define (fun)
;;     1)
;;   (define name)
;;   (define {var2 tag} (+ 3 4))
;;   (define var1 (+ 3 4))
;;   (display 5)
;;
;;all the forms are parsed and the following QRHS compounds are created:
;;
;;   (defun    . #'(internal-define ?attributes (fun) 1))
;;   (expr     . #'(void))
;;   (expr     . #'(+ 3 4))
;;   (untagged-define-expr . (#'var1 . #'(+ 3 4)))
;;   (top-expr . #'(display 5))
;;
;;The possible types are:
;;
;;DEFUN -
;;   For a function variable definition.  A syntax like:
;;
;;      (internal-define ?attributes (?id . ?formals) ?body ...)
;;
;;UNSAFE-DEFUN -
;;   Like  DEFUN,  but do  not  automatically  include  validation forms  for  tagged
;;   operands and return values.
;;
;;EXPR -
;;  For an non-function variable definition.  A syntax like:
;;
;;      (internal-define ?attributes ?id)
;;      (internal-define ?attributes {?id ?tag} ?val)
;;
;;UNTAGGED-DEFINE-EXPR -
;;  For an non-function variable definition.  A syntax like:
;;
;;      (internal-define ?attributes ?id ?val)
;;
;;TOP-EXPR -
;;  For an expression that is not a  definition; this QRHS is created only when mixed
;;  definitions and expressions are allowed.  A syntax like:
;;
;;     ?expr
;;
;;  in this case the caller implicitly handles such expression as:
;;
;;     (internal-define ?attributes dummy ?expr)
;;
;;It is  responsibility of the  caller to create  the appropriate lexical  binding to
;;represent the DEFINE syntax; when CHI-QRHS and CHI-QRHS* are called the binding has
;;already been created.
;;
(define* (chi-qrhs qrhs lexenv.run lexenv.expand)
  ;;Expand a qualified right-hand side expression and return a PSI struct.
  ;;
  (while-not-expanding-application-first-subform
   (case (car qrhs)
     ((defun)
      (let ((expr.stx (cdr qrhs)))
	;;This returns a PSI struct containing a lambda core expression.
	(chi-defun expr.stx lexenv.run lexenv.expand)))

     ((expr)
      (let ((expr.stx (cdr qrhs)))
	(chi-expr expr.stx lexenv.run lexenv.expand)))

     ((untagged-define-expr)
      ;;We know that here the definition is for an untagged identifier.
      (let ((var.id   (cadr qrhs))
	    (expr.stx (cddr qrhs)))
	(receive-and-return (expr.psi)
	    (chi-expr expr.stx lexenv.run lexenv.expand)
	  (let ((expr.sig (psi-retvals-signature expr.psi))
		(tag.id   (identifier-tag var.id)))
	    (syntax-match (retvals-signature-tags expr.sig) ()
	      ((?tag)
	       ;;A single return value: good.
	       (when (option.tagged-language.rhs-tag-propagation?)
		 (if (top-tag-id? tag.id)
		     (override-identifier-tag! var.id ?tag)
		   (assertion-violation/internal-error __who__
		     "expected variable definition with untagged identifier"
		     expr.stx tag.id))))

	      (?tag
	       (list-tag-id? ?tag)
	       ;;Fully  unspecified return  values: we  accept it  here and  delegate
	       ;;further checks at run-time.
	       (void))

	      (_
	       ;;Multiple return values: syntax violation.
	       (expand-time-retvals-signature-violation __who__
							expr.stx #f
							(make-retvals-signature-single-top)
							expr.sig))
	      )))))

     ((top-expr)
      (let* ((expr.stx  (cdr qrhs))
	     (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	     (expr.core (psi-core-expr expr.psi)))
	(make-psi expr.stx
		  (build-sequence no-source
		    (list expr.core
			  (build-void)))
		  (psi-retvals-signature expr.psi))))

     (else
      (assertion-violation/internal-error __who__
	"invalid qualified right-hand side (QRHS) format" qrhs)))))

(define (chi-qrhs* qrhs* lexenv.run lexenv.expand)
  ;;Expand the qualified right-hand side expressions in QRHS*, left-to-right.  Return
  ;;a list of PSI structs.
  ;;
  (let recur ((ls qrhs*))
    ;; chi-qrhs in order
    (if (null? ls)
	'()
      (let ((psi (chi-qrhs (car ls) lexenv.run lexenv.expand)))
	(cons psi (recur (cdr ls)))))))


;;;; chi procedures: begin-for-syntax

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
  (define (chi-begin-for-syntax input-form.stx body-form*.stx lexenv.run lexenv.expand
				lex* qrhs* mod** kwd* export-spec* rib mix? sd?)
    (receive (lhs*.lex init*.core rhs*.core lexenv.expand^)
	(%expand input-form.stx lexenv.expand rib)
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
	(parametrise ((current-run-lexenv (lambda () lexenv.run)))
	  (compiler.eval-core (expanded->core visit-code.core))))
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
		   mix? sd?))))

  (define (%expand input-form.stx lexenv.expand rib)
    (define rtc
      (make-collector))
    (parametrise ((inv-collector rtc)
		  (vis-collector (lambda (x) (values))))
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
		(mix-definitions-and-expressions? #t)
		(shadowing-definitions?           #t))
	    (syntax-match input-form.stx ()
	      ((_ ?expr ?expr* ...)
	       (chi-body* (cons ?expr ?expr*)
			  lexenv.expand lexenv.super
			  lex* qrhs* mod** kwd* export-spec* rib
			  mix-definitions-and-expressions?
			  shadowing-definitions?))))
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
		  (map psi-core-expr init*.psi)
		  (map psi-core-expr rhs*.psi)
		  lexenv.expand^)))))

  #| end of module: BEGIN-FOR-SYNTAX |# )


;;;; chi procedures: internal body

(define* (chi-internal-body body-form*.stx lexenv.run lexenv.expand)
  ;;This function is used to expand the internal bodies:
  ;;
  ;;*  The  LET-like  syntaxes  are  converted to  LETREC*  syntaxes:  this  function
  ;;  processes the internal bodies of LETREC*.
  ;;
  ;;*  The  LAMBDA syntaxes  are  processed  as  CASE-LAMBDA clauses:  this  function
  ;;  processes the internal body of CASE-LAMBDA clauses.
  ;;
  ;;Return  a  PSI  struct  containing   an  expanded  language  symbolic  expression
  ;;representing the expanded body.
  ;;
  ;;An internal body must satisfy the following constraints:
  ;;
  ;;* There must be at least one trailing expression, otherwise an error is raised.
  ;;
  ;;* Mixed definitions and expressions are forbidden.  All the definitions must come
  ;;  first and the expressions last.
  ;;
  ;;* It is impossible to export an internal binding from the enclosing library.  The
  ;;  EXPORT syntaxes present in the internal body are discarded.
  ;;
  ;;An internal body having internal definitions:
  ;;
  ;;   (define ?id ?rhs)
  ;;   ...
  ;;   ?trailing-expr
  ;;   ...
  ;;
  ;;is equivalent to:
  ;;
  ;;   (letrec* ((?id ?rhs) ...)
  ;;     ?trailing-expr)
  ;;
  ;;so we create  a rib to describe  the lexical contour of the  implicit LETREC* and
  ;;push it on the BODY-FORM*.STX.
  ;;
  (let ((rib (make-empty-rib)))
    (receive (trailing-expr-stx*^
	      lexenv.run^ lexenv.expand^
	      lex*^ qrhs*^
	      trailing-mod-expr-stx**^
	      unused-kwd*^ unused-export-spec*^)
	(let ((lex*                              '())
	      (qrhs*                             '())
	      (mod**                             '())
	      (kwd*                              '())
	      (export-spec*                      '())
	      (mix-definitions-and-expressions?  #f)
	      (shadowing-definitions?            #t))
	  (chi-body* (map (lambda (x)
			    (push-lexical-contour rib x))
		       (syntax->list body-form*.stx))
		     lexenv.run lexenv.expand
		     lex* qrhs* mod** kwd* export-spec* rib
		     mix-definitions-and-expressions?
		     shadowing-definitions?))
      (when (null? trailing-expr-stx*^)
	(stx-error body-form*.stx "no expression in body"))
      ;;We want order here!   First the RHS then the inits, so that  tags are put in
      ;;place when the inits are expanded.
      (let* ((rhs*.psi       (chi-qrhs* (reverse qrhs*^) lexenv.run^ lexenv.expand^))
	     (init*.psi      (chi-expr* (append (reverse-and-append trailing-mod-expr-stx**^)
						trailing-expr-stx*^)
					lexenv.run^ lexenv.expand^))
	     (rhs*.core      (map psi-core-expr rhs*.psi))
	     (init*.core     (map psi-core-expr init*.psi))
	     (last-init.psi  (proper-list->last-item init*.psi)))
	(make-psi body-form*.stx
		  (build-letrec* no-source
		    (reverse lex*^)
		    rhs*.core
		    (build-sequence no-source
		      init*.core))
		  (psi-retvals-signature last-init.psi))))))


;;;; chi procedures: body

(module (chi-body*)
  ;;The recursive function CHI-BODY* expands the forms of a body.  Here we expand:
  ;;
  ;;* User-defined the macro definitions and uses.
  ;;
  ;;* Non-core macro uses.
  ;;
  ;;* Basic language syntaxes: LIBRARY, MODULE, BEGIN, STALE-WHEN, IMPORT, EXPORT.
  ;;
  ;;but expand neither  the core macros nor the lexical  variable definitions (DEFINE
  ;;forms) and trailing expressions: the  variable definitions are accumulated in the
  ;;argument and return value QRHS* for later expansion; the trailing expressions are
  ;;accumulated in the argument and return value BODY-FORM*.STX for later expansion.
  ;;
  ;;Here is a description of the arguments.
  ;;
  ;;BODY-FORM*.STX must be null or a list of syntax objects representing the forms.
  ;;
  ;;LEXENV.RUN  and LEXENV.EXPAND  must  be lists  representing  the current  lexical
  ;;environment for run and expand times.
  ;;
  ;;LEX* must be a  list of gensyms and QRHS* must be a  list of qualified right-hand
  ;;sides representing right-hand  side expressions for DEFINE syntax  uses; they are
  ;;meant to  be processed together  item by item;  they are accumulated  in reversed
  ;;order.  Whenever the QRHS expressions are  expanded: a core language binding will
  ;;be created with a  LEX gensym associate to a QRHS expression.   The QRHS have the
  ;;formats:
  ;;
  ;;   (defun . ?full-form)
  ;;		Represents a DEFINE form which defines a function.  ?FULL-FORM is the
  ;;		syntax object representing the full DEFINE form.
  ;;
  ;;   (expr  . ?val)
  ;;		Represents a DEFINE form which defines a non-function variable.  ?VAL
  ;;		is a syntax object representing the variable's value.
  ;;
  ;;   (top-expr . ?body-form-stx)
  ;;		Represents  a  dummy  DEFINE   form  introduced  when  processing  an
  ;;		expression in a R6RS program.
  ;;
  ;;About the MOD** argument.  We know that module definitions have the syntax:
  ;;
  ;;   (module (?export-id ...) ?definition ... ?expression ...)
  ;;
  ;;and the trailing  ?EXPRESSION forms must be evaluated after  the right-hand sides
  ;;of the DEFINE syntaxes  of the module but also of  the enclosing lexical context.
  ;;So, when expanding a MODULE, we  accumulate such expression syntax objects in the
  ;;MOD** argument as:
  ;;
  ;;   MOD** == ((?expression ...) ...)
  ;;
  ;;KWD* is  a list of  identifiers representing  the syntaxes and  lexical variables
  ;;defined in this body.  It is used to test for duplicate definitions.
  ;;
  ;;EXPORT-SPEC*  is  null or  a  list  of  syntax  objects representing  the  export
  ;;specifications from this body.  It is to be processed later.
  ;;
  ;;RIB is the current lexical environment's rib.
  ;;
  ;;MIX? is interpreted as boolean.  When false: the expansion process visits all the
  ;;definition forms and stops at the first expression form; the expression forms are
  ;;returned  to  the caller.   When  true:  the  expansion  process visits  all  the
  ;;definition  and  expression  forms,  accepting  a  mixed  sequence  of  them;  an
  ;;expression form is handled as a dummy definition form.
  ;;
  ;;When SD? is  false this body is  allowed to redefine bindings  created by DEFINE;
  ;;this happens when  expanding for the Scheme REPL in  the interaction environment.
  ;;When  SD?  is  true: attempting  to  redefine  a  DEFINE  binding will  raise  an
  ;;exception.
  ;;
  (define (chi-body* body-form*.stx lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec* rib mix? sd?)
    (if (null? body-form*.stx)
	(values body-form*.stx lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec*)
      (let ((body-form.stx (car body-form*.stx)))
	(receive (type bind-val kwd)
	    (expr-syntax-type body-form.stx lexenv.run)
	  (let ((kwd* (if (identifier? kwd)
			  (cons kwd kwd*)
			kwd*)))
	    (case type

	      ((define)
	       ;;The body form is a core language DEFINE macro use:
	       ;;
	       ;;   (define ?id ?rhs)
	       ;;
	       ;;we parse the form and generate  a QRHS that will be expanded later.
	       ;;We create a new lexical binding:
	       ;;
	       ;;* We generate a label gensym uniquely associated to the binding and
	       ;;  a lex gensym as name of the binding in the expanded code.
	       ;;
	       ;;* We register the association id/label in the rib.
	       ;;
	       ;;*  We push  an entry  on  LEXENV.RUN to  represent the  association
	       ;;  label/lex.
	       ;;
	       ;;* Finally we recurse on the rest of the body.
	       ;;
	       ;;We  receive  the  following  values:   ID  is  the  tagged  binding
	       ;;identifier; TAG  is the tag  identifier for ID  (possibly "<top>");
	       ;;QRHS.STX the QRHS to be expanded later.
	       (receive (id tag qrhs.stx)
		   (%parse-define body-form.stx)
		 (when (bound-id-member? id kwd*)
		   (stx-error body-form.stx "cannot redefine keyword"))
		 (receive (lab lex)
		     ;;About this call to GEN-DEFINE-LABEL+LEX notice that:
		     ;;
		     ;;* If  the binding is at  the top-level of a  program body: we
		     ;;  need a loc gensym to store the result of evaluating the RHS
		     ;;  in QRHS; this loc will be generated later.
		     ;;
		     ;;* If the binding is at the top-level of a REPL expression: we
		     ;;  need a loc gensym to store the result of evaluating the RHS
		     ;;  in QRHS; in this case the loc is generated here.
		     ;;
		     (gen-define-label+lex id rib sd?)
		   (extend-rib! rib id lab sd?)
		   (set-label-tag! lab tag)
		   (chi-body* (cdr body-form*.stx)
			      (add-lexical-binding lab lex lexenv.run) lexenv.expand
			      (cons lex lex*) (cons qrhs.stx qrhs*)
			      mod** kwd* export-spec* rib mix? sd?))))

	      ((define-syntax)
	       ;;The  body form  is a  core  language DEFINE-SYNTAX  macro use.   We
	       ;;expand and  evaluate the transformer expression,  build a syntactic
	       ;;binding for it, register the label  in the rib.  Finally we recurse
	       ;;on the rest of the body.
	       ;;
	       (receive (id rhs.stx)
		   (%parse-define-syntax body-form.stx)
		 (when (bound-id-member? id kwd*)
		   (stx-error body-form.stx "cannot redefine keyword"))
		 ;;We want order here!?!
		 (let ((lab      (gen-define-syntax-label id rib sd?))
		       (rhs.core (with-exception-handler/input-form
				     rhs.stx
				   (%expand-macro-transformer rhs.stx lexenv.expand))))
		   ;;First map  the identifier to  the label, creating  the binding;
		   ;;then evaluate the macro transformer.
		   (extend-rib! rib id lab sd?)
		   (let ((entry (cons lab (with-exception-handler/input-form
					      rhs.stx
					    (%eval-macro-transformer rhs.core lexenv.run)))))
		     (chi-body* (cdr body-form*.stx)
				(cons entry lexenv.run)
				(cons entry lexenv.expand)
				lex* qrhs* mod** kwd* export-spec* rib
				mix? sd?)))))

	      ((define-fluid-syntax)
	       ;;The body form is a core language DEFINE-FLUID-SYNTAX macro use.  We
	       ;;expand  and evaluate  the transformer  expression, build  syntactic
	       ;;bindings for it, register the label in the rib.  Finally we recurse
	       ;;on the rest of the body.
	       ;;
	       (receive (id rhs.stx)
		   (%parse-define-syntax body-form.stx)
		 (when (bound-id-member? id kwd*)
		   (stx-error body-form.stx "cannot redefine keyword"))
		 ;;We want order here!?!
		 (let* ((lab      (gen-define-syntax-label id rib sd?))
			(flab     (gen-define-syntax-label id rib sd?))
			(rhs.core (with-exception-handler/input-form
				      rhs.stx
				    (%expand-macro-transformer rhs.stx lexenv.expand))))
		   ;;First map  the identifier to  the label,  so that it  is bound;
		   ;;then evaluate the macro transformer.
		   (extend-rib! rib id lab sd?)
		   (let* ((binding  (with-exception-handler/input-form
					rhs.stx
				      (%eval-macro-transformer rhs.core lexenv.run)))
			  ;;This LEXENV entry represents the definition of the fluid
			  ;;syntax.
			  (entry1   (cons lab (make-fluid-syntax-binding flab)))
			  ;;This LEXENV entry represents  the current binding of the
			  ;;fluid  syntax;   the  binding  descriptor  is   of  type
			  ;;LOCAL-MACRO, LOCAL-MACRO!  or  LOCAL-CTV.  Other entries
			  ;;like this one can be pushed to rebind the fluid syntax.
			  (entry2   (cons flab binding)))
		     (chi-body* (cdr body-form*.stx)
				(cons* entry1 entry2 lexenv.run)
				(cons* entry1 entry2 lexenv.expand)
				lex* qrhs* mod** kwd* export-spec* rib
				mix? sd?)))))

	      ((define-fluid-override)
	       ;;The body form  is a core language  DEFINE-FLUID-OVERRIDE macro use.
	       ;;We push new entries  on the LEXENV then recurse on  the rest of the
	       ;;body.
	       ;;
	       ;;For  a  description of  how  to  re-bind  fluid syntaxes:  see  the
	       ;;transformer for FLUID-LET-SYNTAX.
	       ;;
	       (receive (id rhs.stx)
		   (%parse-define-syntax body-form.stx)
		 (when (bound-id-member? id kwd*)
		   (stx-error body-form.stx "cannot redefine keyword"))
		 (let* ((fluid-label (let* ((label    (or (id->label id)
							  (stx-error id "unbound identifier")))
					    (binding  (label->syntactic-binding/no-indirection label lexenv.run)))
				       (cond ((fluid-syntax-binding? binding)
					      (fluid-syntax-binding-fluid-label binding))
					     (else
					      (stx-error id "not a fluid identifier")))))
			(binding      (with-exception-handler/input-form
					  rhs.stx
					(%eval-macro-transformer
					 (%expand-macro-transformer rhs.stx lexenv.expand)
					 lexenv.run)))
			(entry       (cons fluid-label binding)))
		   (chi-body* (cdr body-form*.stx)
			      (cons entry lexenv.run)
			      (cons entry lexenv.expand)
			      lex* qrhs* mod** kwd* export-spec* rib
			      mix? sd?))))

	      ((define-alias)
	       ;;The body form is a core  language DEFINE-ALIAS macro use.  We add a
	       ;;new association  identifier/label to  the current rib.   Finally we
	       ;;recurse on the rest of the body.
	       ;;
	       (receive (alias-id old-id)
		   (%parse-define-alias body-form.stx)
		 (when (bound-id-member? old-id kwd*)
		   (stx-error body-form.stx "cannot redefine keyword"))
		 (cond ((id->label old-id)
			=> (lambda (label)
			     (extend-rib! rib alias-id label sd?)
			     (chi-body* (cdr body-form*.stx)
					lexenv.run lexenv.expand
					lex* qrhs* mod** kwd* export-spec* rib
					mix? sd?)))
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
		  (let* ((xlab*  (map gensym-for-label ?xlhs*))
			 (xrib   (make-filled-rib ?xlhs* xlab*))
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
					  (with-exception-handler/input-form
					      in-form
					    (%eval-macro-transformer
					     (%expand-macro-transformer in-form lexenv.expand)
					     lexenv.run))))
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
		     lex* qrhs* mod** kwd* export-spec* rib
		     mix? sd?)))))

	      ((begin-for-syntax)
	       (let ()
		 (import CHI-BEGIN-FOR-SYNTAX)
		 (chi-begin-for-syntax body-form.stx body-form*.stx lexenv.run lexenv.expand
				       lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	      ((begin)
	       ;;The body form  is a BEGIN syntax use.  Just  splice the expressions
	       ;;and recurse on them.
	       ;;
	       (syntax-match body-form.stx ()
		 ((_ ?expr* ...)
		  (chi-body* (append ?expr* (cdr body-form*.stx))
			     lexenv.run lexenv.expand
			     lex* qrhs* mod** kwd* export-spec* rib mix? sd?))))

	      ((stale-when)
	       ;;The body form  is a STALE-WHEN syntax use.   Process the stale-when
	       ;;guard expression, then  just splice the internal  expressions as we
	       ;;do for BEGIN and recurse.
	       ;;
	       (syntax-match body-form.stx ()
		 ((_ ?guard ?expr* ...)
		  (begin
		    (handle-stale-when ?guard lexenv.expand)
		    (chi-body* (append ?expr* (cdr body-form*.stx))
			       lexenv.run lexenv.expand
			       lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))))

	      ((global-macro global-macro!)
	       ;;The body form  is a macro use,  where the macro is  imported from a
	       ;;library.   We perform  the  macro expansion,  then  recurse on  the
	       ;;resulting syntax object.
	       ;;
	       (let ((body-form.stx^ (chi-global-macro bind-val body-form.stx lexenv.run rib)))
		 (chi-body* (cons body-form.stx^ (cdr body-form*.stx))
			    lexenv.run lexenv.expand
			    lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	      ((local-macro local-macro!)
	       ;;The body form  is a macro use, where the  macro is locally defined.
	       ;;We  perform the  macro  expansion, then  recurse  on the  resulting
	       ;;syntax object.
	       ;;
	       (let ((body-form.stx^ (chi-local-macro bind-val body-form.stx lexenv.run rib)))
		 (chi-body* (cons body-form.stx^ (cdr body-form*.stx))
			    lexenv.run lexenv.expand
			    lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	      ((macro)
	       ;;The body form is  a macro use, where the macro  is a non-core macro
	       ;;integrated in the  expander.  We perform the  macro expansion, then
	       ;;recurse on the resulting syntax object.
	       ;;
	       (let ((body-form.stx^ (chi-non-core-macro bind-val body-form.stx lexenv.run rib)))
		 (chi-body* (cons body-form.stx^ (cdr body-form*.stx))
			    lexenv.run lexenv.expand
			    lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	      ((module)
	       ;;The body  form is  an internal module  definition.  We  process the
	       ;;module, then recurse on the rest of the body.
	       ;;
	       (receive (lex* qrhs* m-exp-id* m-exp-lab* lexenv.run lexenv.expand mod** kwd*)
		   (chi-internal-module body-form.stx lexenv.run lexenv.expand lex* qrhs* mod** kwd*)
		 ;;Extend  the  rib with  the  syntactic  bindings exported  by  the
		 ;;module.
		 (vector-for-each (lambda (id lab)
				    (extend-rib! rib id lab sd?))
		   m-exp-id* m-exp-lab*)
		 (chi-body* (cdr body-form*.stx) lexenv.run lexenv.expand
			    lex* qrhs* mod** kwd* export-spec*
			    rib mix? sd?)))

	      ((library)
	       ;;The body  form is  a library definition.   We process  the library,
	       ;;then recurse on the rest of the body.
	       ;;
	       (expand-library (syntax->datum body-form.stx))
	       (chi-body* (cdr body-form*.stx)
			  lexenv.run lexenv.expand
			  lex* qrhs* mod** kwd* export-spec*
			  rib mix? sd?))

	      ((export)
	       ;;The body  form is an  EXPORT form.   We just accumulate  the export
	       ;;specifications, to be  processed later, and we recurse  on the rest
	       ;;of the body.
	       ;;
	       (syntax-match body-form.stx ()
		 ((_ ?export-spec* ...)
		  (chi-body* (cdr body-form*.stx)
			     lexenv.run lexenv.expand
			     lex* qrhs* mod** kwd*
			     (append ?export-spec* export-spec*)
			     rib mix? sd?))))

	      ((import)
	       ;;The body  form is an IMPORT  form.  We just process  the form which
	       ;;results  in   extending  the  RIB  with   more  identifier-to-label
	       ;;associations.  Finally we recurse on the rest of the body.
	       ;;
	       (%chi-import body-form.stx lexenv.run rib sd?)
	       (chi-body* (cdr body-form*.stx) lexenv.run lexenv.expand
			  lex* qrhs* mod** kwd* export-spec* rib mix? sd?))

	      (else
	       ;;Any other expression.
	       ;;
	       ;;If mixed  definitions and expressions  are allowed, we  handle this
	       ;;expression as an implicit definition:
	       ;;
	       ;;   (define dummy ?body-form)
	       ;;
	       ;;which  is not  really  part  of the  lexical  environment: we  only
	       ;;generate  a lex  and  a  qrhs for  it,  without  adding entries  to
	       ;;LEXENV-RUN; then we move on to the next body form.
	       ;;
	       ;;If mixed definitions and expressions are forbidden: we have reached
	       ;;the end of  definitions and expect this and all  the other forms in
	       ;;BODY-FORM*.STX to  be expressions.   So BODY-FORM*.STX  becomes the
	       ;;"trailing expressions" return value.
	       ;;
	       (if mix?
		   (chi-body* (cdr body-form*.stx)
			      lexenv.run lexenv.expand
			      (cons (gensym-for-lexical-var 'dummy) lex*)
			      (cons (cons 'top-expr body-form.stx) qrhs*)
			      mod** kwd* export-spec* rib #t sd?)
		 (values body-form*.stx lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec*)))))))))

;;; --------------------------------------------------------------------

  (define (%parse-define input-form.stx)
    ;;Syntax  parser for  Vicare's INTERNAL-DEFINE;  this  is like  R6RS DEFINE,  but
    ;;supports extended tagged bindings syntax and an additional first argument being
    ;;a list of attributes.  Return 4 values:
    ;;
    ;;1..The identifier of the binding variable.
    ;;
    ;;2..An identifier representing the binding tag.   "<top>" is used when no tag is
    ;;   specified; a subtag of "<procedure>" is used when a procedure is defined.
    ;;
    ;;3..A qualified  right-hand side expression  (QRHS) representing the  binding to
    ;;   create.
    ;;
    (syntax-match input-form.stx (brace)
      ;;Function definition with tagged return values and possibly tagged formals.
      ;;
      ;;NOTE The following special case matches fine:
      ;;
      ;;   (internal-define ?attributes ({ciao})
      ;;     (values))
      ;;
      ;;NOTE We explicitly decide not to rely  on RHS tag propagation to properly tag
      ;;the defined identifier ?WHO.  We could have considered:
      ;;
      ;;   (internal-define ?attributes (?who . ?formals) . ?body)
      ;;
      ;;as equivalent to:
      ;;
      ;;   (internal-define ?attributes ?who (lambda ?formals . ?body))
      ;;
      ;;expand the LAMBDA  form, take its single-tag retvals signature  and use it to
      ;;tag the identifier  ?WHO.  But to do  it that way: we would  first expand the
      ;;LAMBDA and after  tag ?WHO; so while  the LAMBDA is expanded ?WHO  is not yet
      ;;tagged.  Instead by tagging  ?WHO here we are sure that  it is already tagged
      ;;when LAMBDA is expanded.
      ((_ ?attributes ((brace ?who ?rv-tag* ... . ?rv-rest-tag) . ?fmls) ?body0 ?body* ...)
       (identifier? ?who)
       (let* ((qrhs   (cons 'defun input-form.stx))
      	      (tag.id (receive (standard-formals-stx signature)
      			  (parse-tagged-lambda-proto-syntax (bless
      							     `((brace _ ,@?rv-tag* . ,?rv-rest-tag) . ,?fmls))
      							    input-form.stx)
      			(fabricate-procedure-tag-identifier (syntax->datum ?who) signature))))
      	 (values ?who tag.id qrhs)))

      ;;Variable definition with tagged identifier.
      ((_ ?attributes (brace ?id ?tag) ?expr)
       (identifier? ?id)
       (let* ((rhs.stx (bless `(tag-assert-and-return (,?tag) ,?expr)))
	      (qrhs    (cons 'expr rhs.stx)))
	 (values ?id ?tag qrhs)))

      ;;Variable definition with tagged identifier, no init.
      ((_ ?attributes (brace ?id ?tag))
       (identifier? ?id)
       (let* ((rhs.stx (bless '(void)))
	      (qrhs    (cons 'expr rhs.stx)))
	 (values ?id ?tag qrhs)))

      ;;Function definition with possibly tagged formals.
      ;;
      ;;NOTE We  explicitly decide *not* to  rely on RHS tag  propagation to properly
      ;;tag the defined identifier ?WHO; see above for an explanation.
      ((_ ?attributes (?who . ?fmls) ?body0 ?body ...)
       (identifier? ?who)
       (let* ((qrhs   (cons 'defun input-form.stx))
	      (tag.id (receive (standard-formals-stx signature)
			  (parse-tagged-lambda-proto-syntax ?fmls input-form.stx)
			(fabricate-procedure-tag-identifier (syntax->datum ?who) signature))))
	 (values ?who tag.id qrhs)))

      ;;R6RS variable definition.
      ((_ ?attributes ?id ?expr)
       (identifier? ?id)
       (let ((qrhs (cons* 'untagged-define-expr ?id ?expr)))
	 (values ?id (top-tag-id) qrhs)))

      ;;R6RS variable definition, no init.
      ((_ ?attributes ?id)
       (identifier? ?id)
       (let* ((rhs.stx (bless '(void)))
	      (qrhs    (cons 'expr rhs.stx)))
	 (values ?id (top-tag-id) qrhs)))

      ))

  (define (%parse-define-syntax stx)
    ;;Syntax parser for R6RS's DEFINE-SYNTAX,  extended with Vicare's syntax.  Accept
    ;;both:
    ;;
    ;;  (define-syntax ?name ?transformer-expr)
    ;;  (define-syntax (?name ?arg) ?body0 ?body ...)
    ;;
    (syntax-match stx ()
      ((_ ?id)
       (identifier? ?id)
       (values ?id (bless '(syntax-rules ()))))
      ((_ ?id ?transformer-expr)
       (identifier? ?id)
       (values ?id ?transformer-expr))
      ((_ (?id ?arg) ?body0 ?body* ...)
       (and (identifier? ?id)
	    (identifier? ?arg))
       (values ?id (bless `(lambda (,?arg) ,?body0 ,@?body*))))
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

;;; --------------------------------------------------------------------

  (module (%chi-import)
    ;;Process  an IMPORT  form.   The purpose  of  such  forms is  to  push some  new
    ;;identifier-to-label association on the current RIB.
    ;;
    (define (%chi-import body-form.stx lexenv.run rib sd?)
      (receive (id* lab*)
	  (%any-import*-checked body-form.stx lexenv.run)
	(vector-for-each (lambda (id lab)
			   (extend-rib! rib id lab sd?))
	  id* lab*)))

    (define (%any-import*-checked import-form lexenv.run)
      (syntax-match import-form ()
	((?ctxt ?import-spec* ...)
	 (%any-import* ?ctxt ?import-spec* lexenv.run))
	(_
	 (stx-error import-form "invalid import form"))))

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
	(%library-import (list ctxt import-spec))))

    (define (%module-import import-form lexenv.run)
      (syntax-match import-form ()
	((_ ?id)
	 (identifier? ?id)
	 (receive (type bind-val kwd)
	     (expr-syntax-type ?id lexenv.run)
	   (case type
	     (($module)
	      (let ((iface bind-val))
		(values (module-interface-exp-id*     iface ?id)
			(module-interface-exp-lab-vec iface))))
	     (else
	      (stx-error import-form "invalid import")))))))

    (define (%library-import import-form)
      (syntax-match import-form ()
	((?ctxt ?imp* ...)
	 ;;NAME-VEC is  a vector of  symbols representing  the external names  of the
	 ;;imported  bindings.   LABEL-VEC is  a  vector  of label  gensyms  uniquely
	 ;;associated to the imported bindings.
	 (receive (name-vec label-vec)
	     (let ()
	       (import PARSE-IMPORT-SPEC)
	       (parse-import-spec* (syntax->datum ?imp*)))
	   (values (vector-map (lambda (name)
				 ($datum->syntax ?ctxt name))
		     name-vec)
		   label-vec)))
	(_
	 (stx-error import-form "invalid import form"))))

    #| end of module: %CHI-IMPORT |# )

  #| end of module: CHI-BODY* |# )


;;;; chi procedures: module processing

(module (chi-internal-module
	 module-interface-exp-id*
	 module-interface-exp-lab-vec)

  (define-record module-interface
    (first-mark
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
      (let* ((module-rib               (make-empty-rib))
	     (internal-body-form*/rib  (map (lambda (form)
					      (push-lexical-contour module-rib form))
					 (syntax->list internal-body-form*))))
	(receive (leftover-body-expr* lexenv.run lexenv.expand lex* qrhs* mod** kwd* _export-spec*)
	    ;;In a module: we do not want the trailing expressions to be converted to
	    ;;dummy definitions; rather  we want them to be accumulated  in the MOD**
	    ;;argument, for later expansion and evaluation.  So we set MIX? to false.
	    (let ((empty-export-spec*      '())
		  (mix?                    #f)
		  (shadowing-definitions?  #t))
	      (chi-body* internal-body-form*/rib
			 lexenv.run lexenv.expand
			 lex* qrhs* mod** kwd* empty-export-spec*
			 module-rib mix? shadowing-definitions?))
	  ;;The list  of exported  identifiers is  not only the  one from  the MODULE
	  ;;argument, but  also the  one from  all the EXPORT  forms in  the MODULE's
	  ;;body.
	  (let* ((all-export-id*  (vector-append export-id* (list->vector _export-spec*)))
		 (all-export-lab* (vector-map
				      (lambda (id)
					;;For every exported identifier there must be
					;;a label already in the rib.
					(or (id->label (make-<stx> (identifier->symbol id)
								   (<stx>-mark* id)
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
	      (let* ((name-label (gensym-for-label 'module))
		     (iface      (make-module-interface
				  (car ($<stx>-mark* name))
				  (vector-map
				      (lambda (x)
					;;This   is  a   syntax  object   holding  an
					;;identifier.
					(let ((rib* '())
					      (ae*  '()))
					  (make-<stx> ($<stx>-expr x)
						      ($<stx>-mark* x)
						      rib*
						      ae*)))
				    all-export-id*)
				  all-export-lab*))
		     (binding    (make-module-binding iface))
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
      (let ((diff   (%diff-marks (<stx>-mark* id-for-marks)
				 (module-interface-first-mark iface)))
	    (id-vec (module-interface-exp-id-vec iface)))
	(if (null? diff)
	    id-vec
	  (vector-map
	      (lambda (x)
		(let ((rib* '())
		      (ae*  '()))
		  (make-<stx> ($<stx>-expr x)
			      (append diff ($<stx>-mark* x))
			      rib*
			      ae*)))
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
		(c (psi-core-expr guard-expr.psi) (stc)))))))


;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'with-exception-handler/input-form	'scheme-indent-function 1)
;;eval: (put '%raise-compound-condition-object	'scheme-indent-function 1)
;;End:
