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
	 psi-core-expr
	 psi-retvals-signature
	 psi-callable-spec
	 psi-application-retvals-signature)

  (define-record (psi %make-psi psi?)
    (core-expr
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
     callable-spec
		;An instance of "clambda-compound" or "lambda-signature".
     ))

  (case-define* make-psi
    ((core-expr)
     (make-psi core-expr (make-fully-unspecified-retvals-signature) (make-clambda-compound '())))
    ((core-expr retvals-signature)
     (make-psi core-expr retvals-signature                          (make-clambda-compound '())))
    ((core-expr {retvals-signature retvals-signature?} {callable-spec %callable-spec?})
     (%make-psi core-expr retvals-signature callable-spec)))

  (define (%callable-spec? obj)
    (or (lambda-signature? obj)
	(clambda-compound? obj)))

  (define* (psi-application-retvals-signature {rator psi?})
    ;;RATOR is a PSI representing the first form in a callable application:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;we need to establish its retvals signature and return it.
    ;;
    (let ((spec ($psi-callable-spec psi)))
      (cond ((lambda-signature? spec)
	     ($lambda-signature-retvals spec))
	    ((clambda-compound? spec)
	     ($clambda-compound-common-retvals-signature spec))
	    (else
	     (make-fully-unspecified-retvals-signature)))))

  #| end of module |# )


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
	 (%raise-unbound-error #f ?id ?id))
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
	   (($rtd)
	    (values 'type-maker-reference (syntactic-binding-value binding) ?id))
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
     (let ((label (id->label/intern ?car)))
       (unless label
	 (%raise-unbound-error #f ?car ?car))
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
	    (values 'type-maker-application (syntactic-binding-value binding) ?car))
	   (else
	    ;;This case includes TYPE being: CORE-PRIM, LEXICAL, GLOBAL,
	    ;;MUTABLE.
	    (values 'call #f #f))))))

    ((?car . ?cdr)
     ;;Here we know that EXPR.STX has the format:
     ;;
     ;;   (?non-id ?form ...)
     ;;
     ;;where ?NON-ID  can be anything  but not an  identifier.  In practice  the only
     ;;valid syntax for this case is:
     ;;
     ;;   ((?first-subform ?subform ...) ?form ...)
     ;;
     ;;because ?NON-ID must be an expression evaluating to a closure object.
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
  ;;place: the function %CHI-APPLICATION applied to a syntax object:
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
				 (assertion-violation __who__
				   "Vicare: internal error: not a procedure" x)))))
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

(define (chi-expr* expr*.stx lexenv.run lexenv.expand)
  ;;Recursive function.  Expand the expressions in EXPR*.STX left to right.  Return a
  ;;list of PSI structs.
  ;;
  (if (null? expr*.stx)
      '()
    ;;ORDER MATTERS!!!  Make sure  that first  we do  the car,  then the
    ;;rest.
    (let ((expr0.psi (chi-expr (car expr*.stx) lexenv.run lexenv.expand)))
      (cons expr0.psi
	    (chi-expr* (cdr expr*.stx) lexenv.run lexenv.expand)))))

(module (chi-expr)

  (define (chi-expr expr-stx lexenv.run lexenv.expand)
    ;;Expand a single expression form.  Return a PSI struct.
    ;;
    (%drop-splice-first-envelope-maybe
     (receive (type bind-val kwd)
	 (expr-syntax-type expr-stx lexenv.run)
       (case type
	 ((core-macro)
	  ;;Core  macro use.   The  core  macro transformers  are  integrated in  the
	  ;;expander; they perform the full expansion of their input forms and return
	  ;;a PSI struct.
	  (let ((transformer (let ()
			       (import CORE-MACRO-TRANSFORMER)
			       (core-macro-transformer bind-val))))
	    (transformer expr-stx lexenv.run lexenv.expand)))

	 ((global)
	  ;;Reference to global imported lexical  variable; this means EXPR-STX is an
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
	    (make-psi (build-global-reference no-source loc)
		      (make-retvals-signature (list (if (tagged-identifier? expr-stx)
							(identifier-tag expr-stx)
						      (untagged-tag-id)))))))

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
	    (make-psi (build-primref no-source name)
		      (if (tagged-identifier? expr-stx)
			  (make-retvals-signature (list (identifier-tag expr-stx)))
			(make-fully-unspecified-retvals-signature)))))

	 ((call)
	  ;;A function call; this means EXPR-STX has one of the formats:
	  ;;
	  ;;   (?id ?form ...)
	  ;;   ((?first-subform ?subform ...) ?form ...)
	  ;;
	  (%chi-application expr-stx lexenv.run lexenv.expand))

	 ((lexical)
	  ;;Reference to lexical variable; this means EXPR-STX is an identifier.
	  (make-psi (let ((lex (lexical-var bind-val)))
		      (build-lexical-reference no-source lex))
		    (make-retvals-signature (list (if (tagged-identifier? expr-stx)
						      (identifier-tag expr-stx)
						    (untagged-tag-id))))))

	 ((global-macro global-macro!)
	  ;;Global macro use.
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-global-macro bind-val expr-stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((local-macro local-macro!)
	  ;;Macro uses of macros that are local in a non top-level region.
	  ;;
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-local-macro bind-val expr-stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((macro macro!)
	  ;;Macro  uses  of  non-core  macros;  such macros  are  integrated  in  the
	  ;;expander.
	  ;;
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-non-core-macro bind-val expr-stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((constant)
	  ;;Constant; it means EXPR-STX is a self-evaluating datum.
	  (let ((datum bind-val))
	    (make-psi (build-data no-source datum)
		      (retvals-signature-of-datum datum))))

	 ((set!)
	  ;;Macro use of SET!; it means EXPR-STX has the format:
	  ;;
	  ;;   (set! ?lhs ?rhs)
	  ;;
	  (%chi-set! expr-stx lexenv.run lexenv.expand))

	 ((begin)
	  ;;R6RS BEGIN  core macro use.   First we  check with SYNTAX-MATCH  that the
	  ;;syntax is correct, then we build the core language expression.
	  ;;
	  (syntax-match expr-stx ()
	    ((_ ?body ?body* ...)
	     (let ((body*.psi (while-not-expanding-application-first-subform
			       (chi-expr* (cons ?body ?body*) lexenv.run lexenv.expand))))
	       (make-psi (build-sequence no-source
			   (map psi-core-expr body*.psi))
			 (psi-retvals-signature (proper-list->last-item body*.psi)))))
	    ))

	 ((stale-when)
	  ;;STALE-WHEN macro use.  STALE-WHEN acts like BEGIN, but in addition causes
	  ;;an expression to be registered in the current stale-when collector.  When
	  ;;such expression  evaluates to false:  the compiled library is  stale with
	  ;;respect to some source file.  See for example the INCLUDE syntax.
	  (syntax-match expr-stx ()
	    ((_ ?guard ?body ?body* ...)
	     (begin
	       (handle-stale-when ?guard lexenv.expand)
	       (let ((body*.psi (while-not-expanding-application-first-subform
				 (chi-expr* (cons ?body ?body*) lexenv.run lexenv.expand))))
		 (make-psi (build-sequence no-source
			     (map psi-core-expr body*.psi))
			   (psi-retvals-signature (proper-list->last-item body*.psi))))))
	    ))

	 ((let-syntax letrec-syntax)
	  ;;LET-SYNTAX or LETREC-SYNTAX core macro uses.
	  (syntax-match expr-stx ()
	    ((_ ((?xlhs* ?xrhs*) ...) ?xbody ?xbody* ...)
	     (unless (valid-bound-ids? ?xlhs*)
	       (stx-error expr-stx "invalid identifiers"))
	     (let* ((xlab* (map gensym-for-label ?xlhs*))
		    (xrib  (make-filled-rib ?xlhs* xlab*))
		    (xb*   (map (lambda (x)
				  (let ((in-form (if (eq? type 'let-syntax)
						     x
						   (push-lexical-contour xrib x))))
				    (with-exception-handler
					(lambda (E)
					  (raise
					   (condition E (make-macro-input-form-condition in-form))))
				      (lambda ()
					(%eval-macro-transformer
					 (%expand-macro-transformer in-form lexenv.expand)
					 lexenv.run)))))
			     ?xrhs*)))
	       (let ((body*.psi (while-not-expanding-application-first-subform
				 (chi-expr* (map (lambda (x)
						   (push-lexical-contour xrib x))
					      (cons ?xbody ?xbody*))
					    (append (map cons xlab* xb*) lexenv.run)
					    (append (map cons xlab* xb*) lexenv.expand)))))
		 (make-psi (build-sequence no-source
			     (map psi-core-expr body*.psi))
			   (psi-retvals-signature (proper-list->last-item body*.psi))))))
	    ))

	 ((displaced-lexical)
	  (stx-error expr-stx "identifier out of context"))

	 ((syntax)
	  (stx-error expr-stx "reference to pattern variable outside a syntax form"))

	 ((define define-syntax define-fluid-syntax define-fluid-override define-alias module import library)
	  (stx-error expr-stx (string-append
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
	  ;;means EXPR-STX is an identifier.
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
		(make-psi (build-global-reference no-source loc)
			  (make-retvals-signature (list (if (tagged-identifier? expr-stx)
							    (identifier-tag expr-stx)
							  (untagged-tag-id))))))
	    (stx-error expr-stx "attempt to reference an unexportable variable")))

	 ((type-maker-reference)
	  ;;Here we  expand an identifier in  reference position, whose binding  is a
	  ;;struct or record  type name.  The result is an  expression that evaluates
	  ;;to the struct or record maker.
	  ;;
	  (%process-type-maker-reference expr-stx bind-val lexenv.run lexenv.expand))

	 ((type-maker-application)
	  ;;Here we  expand a  form whose  car is  an identifier  whose binding  is a
	  ;;struct or record  type name.  The result is an  expression that evaluates
	  ;;to the application of the struct or record maker.
	  ;;
	  (%process-type-maker-application expr-stx bind-val lexenv.run lexenv.expand))

	 ((tag-cast-operator)
	  (chi-expr (bless
		     `(splice-first-expand (tag-cast ,kwd)))
		    lexenv.run lexenv.expand))

	 (else
	  ;;(assertion-violation 'chi-expr "invalid type " type (strip expr-stx '()))
	  (stx-error expr-stx "invalid expression"))))
     lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (module (%chi-application)

    (define (%chi-application input-form.stx lexenv.run lexenv.expand)
      ;;Expand a  function application form.   This is  called when INPUT-FORM.STX  has the
      ;;format:
      ;;
      ;;   (?rator ?rand ...)
      ;;
      ;;and ?RATOR  is a pair  or a non-macro identifier.   For example it  is called
      ;;when INPUT-FORM.STX is:
      ;;
      ;;   (((?rator ?rand1 ...) ?rand2 ...) ?rand3 ...)
      ;;
      (syntax-match input-form.stx (values)
	(((?nested-rator ?nested-rand* ...) ?rand* ...)
	 ;;This is a function or macro application with possible splicing first expand.
	 ;;We  expand it  considering the  case  of the  first subform  expanding to  a
	 ;;SPLICE-FIRST-EXPAND form.
	 (let* ((rator.stx  (cons ?nested-rator ?nested-rand*))
		(rator.psi  (while-expanding-application-first-subform
			     (chi-expr rator.stx lexenv.run lexenv.expand)))
		(rator.expr (psi-core-expr rator.psi)))
	   ;;Here RATOR.EXPR  is either an  instance of "splice-first-envelope"  or a
	   ;;core language sexp.
	   (import SPLICE-FIRST-ENVELOPE)
	   (if (splice-first-envelope? rator.expr)
	       (syntax-match (splice-first-envelope-form rator.expr) ()
		 ((?nested-rator ?nested-rand** ...)
		  (chi-expr (cons ?nested-rator (append ?nested-rand** ?rand*))
			    lexenv.run lexenv.expand))
		 (_
		  (stx-error rator.stx
			     "expected list as argument of splice-first-expand"
			     'splice-first-expand)))
	     (%build-core-expression input-form.stx rator.psi ?rand* lexenv.run lexenv.expand))))

	((values ?rand* ...)
	 ;;A call  to VALUES  is special  because VALUES does  not have  a predefined
	 ;;retvals  signature,  but  the   retvals  signature  equals  the  arguments
	 ;;signature.
	 (let* ((rand*.psi  (while-not-expanding-application-first-subform
			     (chi-expr* ?rand* lexenv.run lexenv.expand)))
		(rand*.core (map psi-core-expr rand*.psi))
		(rand*.sign (map psi-retvals-signature rand*.psi))
		(rator.core (build-primref no-source 'values)))
	   ;;To be a valid VALUES argument an expression must have as signature:
	   ;;
	   ;;   (?tag)
	   ;;
	   ;;which means a single return  value.  Here we allow "<unspecified>", too,
	   ;;which means we accept an argument having unknown retval signature.
	   ;;
	   ;;NOTE We  could reject "<unspecified>"  as argument signature  and demand
	   ;;the caller  of VALUES to  cast the arguments, but  it would be  to much.
	   ;;(Marco Maggi; Mon Mar 31, 2014)
	   (unless (and (list rand*.sign)
			(for-all retvals-signature-single-tag-or-fully-unspecified? rand*.sign))
	     (retvals-signature-violation 'values input-form.stx
					  ;;expected signature
					  (map (lambda (obj)
						 (list (top-tag-id)))
					    rand*.sign)
					  rand*.sign))
	   (make-psi (build-application (syntax-annotation input-form.stx)
		       rator.core
		       rand*.core)
		     (make-retvals-signature (map (lambda (sign)
						    (if (retvals-signature-fully-unspecified? sign)
							(untagged-tag-id)
						      (car (retvals-signature-tags sign))))
					       rand*.sign)))))

	((?rator ?rand* ...)
	 ;;This  is a  common function  application: ?RATOR  is not  a syntax  keyword.
	 ;;Let's make sure that we expand ?RATOR first.
	 (let ((rator.psi (chi-expr ?rator lexenv.run lexenv.expand)))
	   (%build-core-expression input-form.stx rator.psi ?rand* lexenv.run lexenv.expand)))
	))

    (module (%build-core-expression)

      (define* (%build-core-expression input-form.stx {rator.psi psi?} rand*.stx lexenv.run lexenv.expand)
	(define rator.core (psi-core-expr         rator.psi))
	(define rator.sign (psi-retvals-signature rator.psi))
	(syntax-match (retvals-signature-tags rator.sign) ()
	  (?tag
	   (untagged-tag-id? ?tag)
	   ;;The rator type  is unknown.  Return a procedure application  and we will
	   ;;see at run-time what happens.
	   (%process-unknown-rator-type input-form.stx rator.core rand*.stx lexenv.run lexenv.expand))
	  ((?tag)
	   ;;The rator type is a single value.  Good, this is what it is meant to be.
	   (cond (($tag-super-and-sub? (procedure-tag-id) ?tag)
		  ;;The  rator  is  a  procedure.  Very  good.   Return  a  procedure
		  ;;application.
		  (let* ((rand*.psi  (while-not-expanding-application-first-subform
				      (chi-expr* rand*.stx lexenv.run lexenv.expand)))
			 (rand*.core (map psi-core-expr rand*.psi)))
		    (make-psi (build-application (syntax-annotation input-form.stx)
				rator.core
				rand*.core)
			      (psi-application-retvals-signature rator.psi))))
		 ((or ($untagged-tag-id? ?tag)
		      ($top-tag-id?      ?tag))
		  ;;The rator type is unknown.  Return a procedure application and we
		  ;;will see at run-time what happens.
		  (%process-unknown-rator-type input-form.stx rator.core rand*.stx lexenv.run lexenv.expand))
		 (else
		  ;;The  rator has  a  correct  single-value signature  and  it is  a
		  ;;specified tag, but it is not  a procedure.  We check that this is
		  ;;a call to an object type's dispatcher.
		  (syntax-match rand*.stx ()
		    (()
		     ;;No  operands.  This  is an  error, because  the input  form is
		     ;;something like:
		     ;;
		     ;;   ("ciao mamma")
		     ;;
		     ;;so we raise an exception.
		     (raise
		      (condition (make-who-condition __who__)
				 (make-message-condition "invalid call operator with no operands")
				 (make-syntax-violation input-form.stx #f)
				 (make-retvals-signature-condition rator.sign))))
		    (((?key00 ?key0* ...) (?key11* ?key1** ...) ...)
		     ;;There are operands and they match the getter keys syntax.
		     (let* ((getter.stx  (tag-identifier-getter ?tag rand*.stx input-form.stx))
			    (getter.psi  (chi-expr getter.stx lexenv.run lexenv.expand))
			    (getter.core (psi-core-expr getter.psi))
			    (getter.sign (psi-retvals-signature getter.psi)))
		       (make-psi (build-application (syntax-annotation input-form.stx)
				   getter.core
				   (list rator.core))
				 (psi-application-retvals-signature getter.psi))))
		    ((?member ?arg* ...)
		     (identifier? ?member)
		     ;;There  are  operands and  they  match  the  syntax for  a  tag
		     ;;dispatcher call, that's great.
		     (let* ((method.stx  (tag-identifier-dispatch ?tag ?member ?arg* input-form.stx))
			    (method.psi  (chi-expr method.stx lexenv.run lexenv.expand))
			    (method.core (psi-core-expr method.psi))
			    (method.sign (psi-retvals-signature method.psi)))
		       (make-psi (build-application (syntax-annotation input-form.stx)
				   method.core
				   (list rator.core))
				 (psi-application-retvals-signature method.psi))))
		    (_
		     ;;There are operands, but they do not match any of the supported
		     ;;syntaxes; for example the input form may be:
		     ;;
		     ;;   ("ciao" "mamma")
		     ;;
		     ;;so we raise an exception.
		     (raise
		      (condition (make-who-condition __who__)
				 (make-message-condition "invalid operands for non-procedure operator")
				 (make-syntax-violation input-form.stx #f)
				 (make-retvals-signature-condition rator.sign))))
		    ))))
	  (_
	   ;;The rator is  declared to evaluate to multiple values.   This is invalid
	   ;;in call context, so we raise an exception.
	   (raise
	    (condition (make-who-condition __who__)
		       (make-message-condition "call operator declared to evaluate to multiple values")
		       (syntax-match input-form.stx ()
			 ((?rator . ?rands)
			  (make-syntax-violation input-form.stx ?rator)))
		       (make-retvals-signature-condition rator.sign))))
	  ))

      (define (%process-unknown-rator-type input-form.stx rator.core rand*.stx lexenv.run lexenv.expand)
	(let* ((rand*.psi  (while-not-expanding-application-first-subform
			    (chi-expr* rand*.stx lexenv.run lexenv.expand)))
	       (rand*.core (map psi-core-expr rand*.psi)))
	  (make-psi (build-application (syntax-annotation input-form.stx)
		      rator.core
		      rand*.core))))

      #| end of module: %BUILD-CORE-EXPRESSION |# )

    #| end of module: %CHI-APPLICATION |# )

;;; --------------------------------------------------------------------

  (define (%chi-set! input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ (?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ...) ?new-value)
       (let* ((keys.stx  (cons (cons ?key00 ?key0*)
			       (map cons ?key11* ?key1**))))
	 (chi-expr (bless
		    `(tag-setter ,?expr ,keys.stx ,?new-value))
		   lexenv.run lexenv.expand)))

      ((_ ?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ... ?new-value)
       (let* ((keys.stx  (cons (cons ?key00 ?key0*)
			       (map cons ?key11* ?key1**))))
	 (chi-expr (bless
		    `(tag-setter ,?expr ,keys.stx ,?new-value))
		   lexenv.run lexenv.expand)))

      ((_ (?expr ?field-name) ?new-value)
       (identifier? ?field-name)
       (chi-expr (bless
		  `(tag-mutator ,?expr ,?field-name ,?new-value))
		 lexenv.run lexenv.expand))

      ((_ ?lhs ?rhs)
       (identifier? ?lhs)
       (receive (type bind-val kwd)
	   (expr-syntax-type ?lhs lexenv.run)
	 (case type
	   ((lexical)
	    (set-lexical-mutable! bind-val)
	    (let ((rhs.psi (chi-expr ?rhs lexenv.run lexenv.expand)))
	      (make-psi (build-lexical-assignment no-source
			  (lexical-var bind-val)
			  (psi-core-expr rhs.psi)))))
	   ((core-prim)
	    (stx-error input-form.stx "cannot modify imported core primitive"))

	   ((global)
	    (stx-error input-form.stx "attempt to modify an immutable binding"))

	   ((global-macro!)
	    (chi-expr (chi-global-macro bind-val input-form.stx lexenv.run #f) lexenv.run lexenv.expand))

	   ((local-macro!)
	    (chi-expr (chi-local-macro bind-val input-form.stx lexenv.run #f) lexenv.run lexenv.expand))

	   ((mutable)
	    (if (and (pair? bind-val)
		     (let ((lib (car bind-val)))
		       (eq? lib '*interaction*)))
		(let ((loc     (cdr bind-val))
		      (rhs.psi (chi-expr ?rhs lexenv.run lexenv.expand)))
		  (make-psi (build-global-assignment no-source
			      loc
			      (psi-core-expr rhs.psi))))
	      (stx-error input-form.stx "attempt to modify an unexportable variable")))

	   (else
	    (stx-error input-form.stx)))))
      ))

;;; --------------------------------------------------------------------

  (define (%process-type-maker-reference expr-stx bind-val lexenv.run lexenv.expand)
    ;;BIND-VAL is the binding value of an R6RS record-type:
    ;;
    ;;   (?rtd-id ?rcd-id)
    ;;   (?rtd-id ?rcd-id . ?r6rs-record-type-spec)
    ;;
    ;;or the binding value of a Vicare struct type:
    ;;
    ;;   #<struct-type-descriptor>
    ;;
    (cond ((r6rs-record-type-descriptor-bindval? bind-val)
	   ;;The binding is for an R6RS record type.
	   (let ((rcd-id (r6rs-record-type-descriptor-bindval-rcd bind-val)))
	     (chi-expr (bless
			`(record-constructor ,rcd-id))
		       lexenv.run lexenv.expand)))

	  ((struct-type-descriptor-bindval? bind-val)
	   ;;The binding is for a Vicare struct type.
	   (let ((field-name* (struct-type-field-names
			       (struct-type-descriptor-bindval-std bind-val))))
	     (chi-expr (bless
			`(lambda ,field-name*
			   ($struct (quote ,bind-val) . ,field-name*)))
		       lexenv.run lexenv.expand)))

	  (else
	   (stx-error expr-stx "invalid binding for identifier"))))

  (define (%process-type-maker-application expr-stx bind-val lexenv.run lexenv.expand)
    ;;BIND-VAL is the binding value of an R6RS record-type:
    ;;
    ;;   (?rtd-id ?rcd-id)
    ;;   (?rtd-id ?rcd-id . ?r6rs-record-type-spec)
    ;;
    ;;or the binding value of a Vicare struct type:
    ;;
    ;;   #<struct-type-descriptor>
    ;;
    (cond ((r6rs-record-type-descriptor-bindval? bind-val)
	   ;;The binding is for an R6RS record type.
	   (syntax-match expr-stx ()
	     ((?rtd ?arg* ...)
	      (let ((rcd-id (r6rs-record-type-descriptor-bindval-rcd bind-val)))
		(chi-expr (bless
			   `((record-constructor ,rcd-id) . ,?arg*))
			  lexenv.run lexenv.expand)))
	     ))

	  ((struct-type-descriptor-bindval? bind-val)
	   ;;The binding is for a Vicare struct type.
	   (syntax-match expr-stx ()
	     ((?rtd ?arg* ...)
	      (let ((field-name* (struct-type-field-names
				  (struct-type-descriptor-bindval-std bind-val))))
		(chi-expr (bless
			   `((lambda ,field-name*
			       ($struct (quote ,bind-val) . ,field-name*))
			     . ,?arg*))
			  lexenv.run lexenv.expand)))
	     ))

	  (else
	   (stx-error expr-stx "invalid binding for identifier"))))

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


;;;; chi procedures: definitions and lambda clauses

(module (chi-lambda chi-case-lambda chi-defun)

  (define (chi-defun input-form.stx lexenv.run lexenv.expand)
    ;;Expand a  syntax object representing a  DEFINE syntax for the  case of function
    ;;definition.  Return  an expanded language expression  representing the expanded
    ;;definition.
    ;;
    ;;The  returned expression  will  be  coupled (by  the  caller)  with an  already
    ;;generated  lex gensym  serving as  lexical variable  name; for  this reason  we
    ;;return a lambda core form rather than a define core form.
    ;;
    ;;NOTE This function assumes the INPUT-FORM.STX  has already been parsed, and the
    ;;binding for ?CTXT has already been added to LEXENV by the caller.
    ;;
    (define (%expand ctxt-id formals.stx body*.stx)
      ;;This procedure is  like CHI-LAMBDA, but, in addition, it  puts CTXT-ID in the
      ;;core language LAMBDA sexp's annotation.
      (receive (formals.core lambda-signature body.psi)
	  (%chi-lambda-clause input-form.stx formals.stx body*.stx lexenv.run lexenv.expand)
	;;FORMALS.CORE is composed of lex gensyms.
	(make-psi (build-lambda (syntax-annotation ctxt-id)
		    formals.core
		    (psi-core-expr body.psi))
		  (make-procedure-retval-signature)
		  lambda-signature)))
    (syntax-match input-form.stx (brace)
      ((_ ((brace ?ctxt ?rv-tag* ... . ?rest-rv-tag) . ?fmls) . ?body-form*)
       (%expand ?ctxt (bless
		       `((brace _ ,@?rv-tag* . ,?rest-rv-tag) . ,?fmls))
		?body-form*))
      ((_ (?ctxt . ?fmls) . ?body-form*)
       (%expand ?ctxt ?fmls ?body-form*))
      ))

  (define (chi-lambda input-form.stx formals.stx body*.stx lexenv.run lexenv.expand)
    ;;Expand the contents of CASE syntax and return a "psi" struct.
    ;;
    ;;INPUT-FORM.STX is a syntax object representing the original LAMBDA expression.
    ;;
    ;;FORMALS.STX is a syntax object representing the formals of the LAMBDA syntax.
    ;;
    ;;BODY*.STX is a list of syntax  objects representing the body expressions in the
    ;;LAMBDA syntax.
    ;;
    (receive (formals.lex lambda-signature body.psi)
	(%chi-lambda-clause input-form.stx formals.stx body*.stx lexenv.run lexenv.expand)
      (make-psi (build-lambda (syntax-annotation input-form.stx)
		  formals.lex
		  (psi-core-expr body.psi))
		(make-procedure-retval-signature)
		lambda-signature)))

  (define (chi-case-lambda input-form.stx formals*.stx body**.stx lexenv.run lexenv.expand)
    ;;Expand the clauses of a CASE-LAMBDA syntax and return a "psi" struct.
    ;;
    ;;INPUT-FORM.STX  is  a  syntax  object  representing  the  original  CASE-LAMBDA
    ;;expression.
    ;;
    ;;FORMALS*.STX is  a list of  syntax objects whose items  are the formals  of the
    ;;CASE-LAMBDA clauses.
    ;;
    ;;BODY**.STX  is a  list of  syntax objects  whose items  are the  bodies of  the
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
    (receive (formals*.lex lambda-signature* body**.psi)
	(%chi-lambda-clause* input-form.stx formals*.stx body**.stx lexenv.run lexenv.expand)
      (make-psi (build-case-lambda (syntax-annotation input-form.stx)
		  formals*.lex
		  (map psi-core-expr body**.psi))
		(make-procedure-retval-signature)
		(make-clambda-compound lambda-signature*))))

;;; --------------------------------------------------------------------

  (define* (%chi-lambda-clause input-form.stx formals.stx body-form*.stx lexenv.run lexenv.expand)
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
    ;;     (let ()
    ;;       ?body ...
    ;;       (tag-assert-and-return (<symbol>) ?last-body)))
    ;;
    (while-not-expanding-application-first-subform
     ;;Here we support both the R6RS  standard LAMBDA formals and the extended tagged
     ;;formals, with or without rest argument.
     (receive (standard-formals.stx lambda-signature)
	 (parse-tagged-lambda-proto-syntax formals.stx input-form.stx)
       (define formals-signature.tags
	 (lambda-signature-formals-tags lambda-signature))
       (syntax-match standard-formals.stx ()
	 ;;Without rest argument.
	 ((?arg* ...)
	  (let* ((lex*        (map gensym-for-lexical-var ?arg*))
		 (lab*        (map gensym-for-label       ?arg*))
		 (lexenv.run^ (add-lexical-bindings lab* lex* lexenv.run)))
	    (let* ((validation*.stx (%build-formals-validation-form* ?arg* formals-signature.tags #f #f))
		   (body-form^*.stx (push-lexical-contour
					(make-filled-rib ?arg* lab*)
				      (append validation*.stx
					      (%build-retvals-validation-form (not (null? validation*.stx))
									      (lambda-signature-retvals lambda-signature)
									      body-form*.stx)))))
	      ;;Here  we know  that the  formals signature  is a  proper list  of tag
	      ;;identifiers with the same structure of FORMALS.STX.
	      (map set-label-tag! lab* formals-signature.tags)
	      (values lex*
		      lambda-signature
		      (chi-internal-body body-form^*.stx lexenv.run^ lexenv.expand)))))

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
	      (let* ((validation*.stx (%build-formals-validation-form* ?arg* arg-tag* ?rest-arg rest-tag))
		     (body-form^*.stx (push-lexical-contour
					  (make-filled-rib (cons ?rest-arg ?arg*)
							   (cons rest-lab  lab*))
					(append validation*.stx
						(%build-retvals-validation-form (not (null? validation*.stx))
										(lambda-signature-retvals lambda-signature)
										body-form*.stx)))))
		;;Here we  know that the formals  signature is an improper  list with
		;;the same structure of FORMALS.STX.
		(map set-label-tag! lab* arg-tag*)
		(set-label-tag! rest-lab rest-tag)
		(values (append lex* rest-lex) ;yes, this builds an improper list
			lambda-signature
			(chi-internal-body body-form^*.stx lexenv.run^ lexenv.expand))))))

	 (_
	  (syntax-violation __who__
	    "invalid lambda formals syntax" input-form.stx formals.stx))))))

  (define (%chi-lambda-clause* input-form.stx formals*.stx body-form**.stx lexenv.run lexenv.expand)
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
	  (%chi-lambda-clause input-form.stx (car formals*.stx) (car body-form**.stx) lexenv.run lexenv.expand)
	(receive (formals-lex* lambda-signature* body*.psi)
	    (%chi-lambda-clause* input-form.stx (cdr formals*.stx) (cdr body-form**.stx) lexenv.run lexenv.expand)
	  (values (cons formals-lex       formals-lex*)
		  (cons lambda-signature  lambda-signature*)
		  (cons body.psi          body*.psi))))))

;;; --------------------------------------------------------------------

  (define (%build-formals-validation-form* arg* tag* rest-arg rest-tag)
    ;;Build and return a list of syntax objects representing expressions like:
    ;;
    ;;   (tag-procedure-argument-validator ?tag ?arg)
    ;;
    ;;excluding  the formals  in  which the  tag is  "<top>"  or "<untagged>",  whose
    ;;argument  are always  valid.   When there  is no  rest  argument: REST-ARG  and
    ;;REST-TAG must be #f.
    ;;
    (define-syntax-rule (%recur)
      (%build-formals-validation-form* ($cdr arg*) ($cdr tag*) rest-arg rest-tag))
    (cond ((pair? arg*)
	   (if (or (free-id=? ($car tag*) (untagged-tag-id))
		   (free-id=? ($car tag*) (top-tag-id)))
	       (%recur)
	     (cons (bless
		    `(tag-procedure-argument-validator ,($car tag*) ,($car arg*)))
		   (%recur))))
	  ((or (not rest-tag)
	       (free-id=? rest-tag (untagged-tag-id))
	       (free-id=? rest-tag (top-tag-id)))
	   '())
	  (else
	   (list (bless
		  `(tag-procedure-argument-validator ,rest-tag ,rest-arg))))))

  (define (%build-retvals-validation-form has-arguments-validators? retvals-signature body-form*.stx)
    ;;Add the return values validation to the last form in the body; return a list of
    ;;body forms.
    ;;
    ;;When there are arguments validators: the body forms are wrapped in a LET syntax
    ;;with no bindings to create an internal lexical scope.
    ;;
    ;;The  argument  HAS-ARGUMENTS-VALIDATORS?   is  really  required  to  avoid  LET
    ;;wrapping when  not needed;  without it:  expanding a  LAMBDA clause  causes the
    ;;generation of infinite nested LET syntaxes.
    ;;
    ;;NOTE I have tried different solutions to avoid this LET wrapping either without
    ;;success or  introducing unwanted (by  me) complications.  Notice  that wrapping
    ;;the TAG-PROCEDURE-ARGUMENT-VALIDATION  forms into  DEFINE syntaxes  followed by
    ;;the body  forms has not worked  because it can generate  binding conflicts with
    ;;the body defines capturing variable references in the validation forms.  Notice
    ;;that, whatever solution we use, we have to take into accound that when the code
    ;;is expanded  with arguments  validation turned off:  the validation  forms will
    ;;expand  to  empty run-time  code,  but  we  still want  expand-time  signatures
    ;;validation.  (Marco Maggi; Mon Mar 31, 2014)
    ;;
    (cond (has-arguments-validators?
           (if (retvals-signature-fully-unspecified? retvals-signature)
	       ;;The number and type of return values is unknown.
	       (bless
		`((let () . ,body-form*.stx)))
	     (receive (head*.stx last.stx)
		 (proper-list->head-and-last body-form*.stx)
	       (bless
		`((let ()
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

  #| end of module |# )


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
;;   (define var (+ 3 4))
;;   (display 5)
;;
;;all the forms are parsed and the following QRHS compounds are created:
;;
;;   (defun    . #'(define (fun) 1))
;;   (expr     . #'(+ 3 4))
;;   (top-expr . #'(display 5))
;;
;;The possible types are:
;;
;;DEFUN -
;;   For a function variable definition.  A syntax like:
;;
;;      (define (?id . ?formals) ?body ...)
;;
;;EXPR -
;;  For an non-function variable definition.  A syntax like:
;;
;;      (define ?id)
;;      (define ?id ?val)
;;
;;TOP-EXPR -
;;  For an expression that is not a  definition; this QRHS is created only when mixed
;;  definitions and expressions are allowed.  A syntax like:
;;
;;     ?expr
;;
;;  in this case the caller implicitly handles such expression as:
;;
;;     (define dummy ?expr)
;;
;;It is  responsibility of the  caller to create  the appropriate lexical  binding to
;;represent the DEFINE syntax; when CHI-QRHS and CHI-QRHS* are called the binding has
;;already been created.
;;
(define* (chi-qrhs qrhs lexenv.run lexenv.expand)
  ;;Expand a qualified right-hand side expression and return a PSI struct.
  ;;
  (case (car qrhs)
    ((defun)
     (let ((expr.stx (cdr qrhs)))
       ;;This returns a PSI struct containing a lambda core expression.
       (chi-defun expr.stx lexenv.run lexenv.expand)))

    ((expr)
     (let ((expr.stx (cdr qrhs)))
       (chi-expr expr.stx lexenv.run lexenv.expand)))

    ((top-expr)
     (let* ((expr.stx  (cdr qrhs))
	    (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi)))
       (make-psi (build-sequence no-source
		   (list expr.core
			 (build-void))))))

    (else
     (assertion-violation __who__ "Vicare: internal error: invalid qrhs" qrhs))))

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


;;;; chi procedures: internal body

(define (chi-internal-body body-form*.stx lexenv.run lexenv.expand)
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
  (while-not-expanding-application-first-subform
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
       ;;FIXME We want order here?  (Marco Maggi; Tue Mar 25, 2014)
       (let* ((init*.psi  (chi-expr* (append (reverse-and-append trailing-mod-expr-stx**^)
					     trailing-expr-stx*^)
				     lexenv.run^ lexenv.expand^))
	      (rhs*.psi   (chi-qrhs* qrhs*^ lexenv.run^ lexenv.expand^)))
	 (let ((init*.core (map psi-core-expr init*.psi))
	       (rhs*.core  (map psi-core-expr rhs*.psi)))
	   (make-psi (build-letrec* no-source
		       (reverse lex*^)
		       (reverse rhs*.core)
		       (build-sequence no-source
			 init*.core))
		     (psi-retvals-signature (proper-list->last-item init*.psi)))))))))


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
    (while-not-expanding-application-first-subform
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
		;;identifier;   TAG  is   the   tag  identifier   for  ID   (possibly
		;;"<untagged>");   SIGNATURE    is   false   or   an    instance   of
		;;"lambda-signature" or  "clambda-compound"; QRHS.STX the QRHS  to be
		;;expanded later.
		(receive (id tag signature qrhs.stx)
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
		    ;;If the binding is not a function: SIGNATURE is false.
		    (when signature
		      (set-label-callable-signature! lab signature))
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
		(receive (id rhs-stx)
		    (%parse-define-syntax body-form.stx)
		  (when (bound-id-member? id kwd*)
		    (stx-error body-form.stx "cannot redefine keyword"))
		  ;;We want order here!?!
		  (let ((lab      (gen-define-syntax-label id rib sd?))
			(rhs.core (with-exception-handler
				      (lambda (E)
					(raise
					 (condition E (make-macro-input-form-condition rhs-stx))))
				    (lambda ()
				      (%expand-macro-transformer rhs-stx lexenv.expand)))))
		    ;;First map  the identifier to  the label, creating  the binding;
		    ;;then evaluate the macro transformer.
		    (extend-rib! rib id lab sd?)
		    (let ((entry (cons lab (with-exception-handler
					       (lambda (E)
						 (raise
						  (condition E (make-macro-input-form-condition rhs-stx))))
					     (lambda ()
					       (%eval-macro-transformer rhs.core lexenv.run))))))
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
			 (rhs.core (with-exception-handler
				       (lambda (E)
					 (raise
					  (condition E (make-macro-input-form-condition rhs.stx))))
				     (lambda ()
				       (%expand-macro-transformer rhs.stx lexenv.expand)))))
		    ;;First map  the identifier to  the label,  so that it  is bound;
		    ;;then evaluate the macro transformer.
		    (extend-rib! rib id lab sd?)
		    (let* ((binding  (with-exception-handler
					 (lambda (E)
					   (raise
					    (condition E (make-macro-input-form-condition rhs.stx))))
				       (lambda ()
					 (%eval-macro-transformer rhs.core lexenv.run))))
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
			 (binding     (with-exception-handler
					  (lambda (E)
					    (raise
					     (condition E (make-macro-input-form-condition rhs.stx))))
					(lambda ()
					  (%eval-macro-transformer
					   (%expand-macro-transformer rhs.stx lexenv.expand)
					   lexenv.run))))
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
					   (with-exception-handler
					       (lambda (E)
						 (raise
						  (condition E (make-macro-input-form-condition in-form))))
					     (lambda ()
					       (%eval-macro-transformer
						(%expand-macro-transformer in-form lexenv.expand)
						lexenv.run)))))
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
		;;The  body form  is a  BEGIN-FOR-SYNTAX syntax  use.  We  expand the
		;;expressions using  LEXENV.EXPAND as LEXENV for  run-time, much like
		;;what we do when evaluating  the right-hand side of a DEFINE-SYNTAX,
		;;but handling the sequence of expressions as a body; then we build a
		;;special core  language expression with global  assignments; finally
		;;we evaluate result.
		;;
		(syntax-match body-form.stx ()
		  ((_ ?expr* ...)
		   (receive ( ;;
			     all-expr*.core rhs*.core
			     lexenv.expand^ lexenv.super^
			     lex*^ qrhs*^ mod**^ kwd*^ export-spec*^)
		       (let ((rtc (make-collector)))
			 (parametrise ((inv-collector rtc)
				       (vis-collector (lambda (x) (values))))
			   (receive (empty
				     lexenv.expand^ lexenv.super^
				     lex*^ qrhs*^ mod**^ kwd*^ export-spec*^)
			       ;;Expand   the   sequence   as   a   top-level   body,
			       ;;accumulating the definitions.
			       (let ((lexenv.super                     lexenv.expand)
				     (mix-definitions-and-expressions? #t)
				     (shadowing-definitions?           #t))
				 (chi-body* (list (cons (bless 'begin) ?expr*))
					    lexenv.expand lexenv.super
					    '() '() '() '() '() rib
					    mix-definitions-and-expressions?
					    shadowing-definitions?))
			     ;;There  should be  no trailing  expressions because  we
			     ;;allowed  mixing definitions  and expressions  as in  a
			     ;;top-level program.
			     (assert (null? empty))
			     ;;Expand  the   definitions  and  the   module  trailing
			     ;;expressions,   then   build   an   expanded   language
			     ;;expression.
			     (let* ((all-expr*.psi  (chi-expr* (reverse-and-append mod**^)
								lexenv.expand^ lexenv.super^))
				    (rhs*.psi       (chi-qrhs* qrhs*^ lexenv.expand^ lexenv.super^))
				    (all-expr*.core (map psi-core-expr all-expr*.psi))
				    (rhs*.core      (map psi-core-expr rhs*.psi)))
			       ;;Now that we have fully expanded the forms: we invoke
			       ;;all the libraries needed to evaluate them.
			       (for-each
				   (let ((register-visited-library (vis-collector)))
				     (lambda (lib)
				       (invoke-library lib)
				       (register-visited-library lib)))
				 (rtc))
			       ;;Let's  get   out  of  the   collectors'  PARAMETRISE
			       ;;syntaxes.
			       (values all-expr*.core rhs*.core
				       lexenv.expand^ lexenv.super^
				       lex*^ qrhs*^ mod**^ kwd*^ export-spec*^)))))
		     ;;Build an expanded code expression and evaluate it.
		     (let ((code-core (build-sequence no-source
					(list (if (null? rhs*.core)
						  (build-void)
						(build-sequence no-source
						  (map (lambda (lhs rhs)
							 (build-global-assignment no-source lhs rhs))
						    (reverse lex*^)
						    (reverse rhs*.core))))
					      (if (null? all-expr*.core)
						  (build-void)
						(build-sequence no-source
						  all-expr*.core))))))
		       (parametrise ((current-run-lexenv (lambda () lexenv.run)))
			 (eval-core (expanded->core code-core))))
		     ;;Done!  Now go on with the next body forms.
		     (chi-body* (cdr body-form*.stx)
				lexenv.run lexenv.expand^
				lex* qrhs* mod** kwd* export-spec* rib
				mix? sd?)))
		  ))

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
		  (values body-form*.stx lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec*))))))))))

;;; --------------------------------------------------------------------

  (define (%parse-define input-form.stx)
    ;;Syntax parser for R6RS's DEFINE and  extended tagged bindings syntax.  Return 4
    ;;values:
    ;;
    ;;1..The identifier of the binding variable.
    ;;
    ;;2..An identifier  representing the binding  tag.  "<untagged>" is used  when no
    ;;   tag is specified; "<procedure>" is used when a procedure is defined.
    ;;
    ;;3..False or an object representing  an instance of "lambda-signature"; false is
    ;;   returned when the binding is not a function.
    ;;
    ;;4..A qualified  right-hand side expression  (QRHS) representing the  binding to
    ;;   create.
    ;;
    (syntax-match input-form.stx (brace)
      ;;Function definition with tagged return values.
      ((_ ((brace ?id ?rv-tag* ... . ?rv-rest-tag) . ?fmls) ?b ?b* ...)
       (identifier? ?id)
       (receive (standard-formals-stx signature)
	   (parse-tagged-lambda-proto-syntax (bless
					      `((brace _ ,@?rv-tag* . ,?rv-rest-tag) . ,?fmls))
					     input-form.stx)
	 (values ?id (procedure-tag-id) signature (cons 'defun input-form.stx))))

      ;;Variable definition with tagged identifier.
      ((_ (brace ?id ?tag) ?val)
       (identifier? ?id)
       (values ?id ?tag #f (cons 'expr ?val)))

      ;;Variable definition with tagged identifier, no init.
      ((_ (brace ?id ?tag))
       (identifier? ?id)
       (values ?id ?tag #f (cons 'expr (bless '(void)))))

      ;;Function definition with possible tagged arguments.
      ((_ (?id . ?fmls) ?b ?b* ...)
       (identifier? ?id)
       (receive (standard-formals-stx signature)
	   (parse-tagged-lambda-proto-syntax ?fmls input-form.stx)
	 (values ?id (procedure-tag-id) signature (cons 'defun input-form.stx))))

      ;;R6RS variable definition.
      ((_ ?id ?val)
       (identifier? ?id)
       (values ?id (untagged-tag-id) #f (cons 'expr ?val)))

      ;;R6RS variable definition, no init.
      ((_ ?id)
       (identifier? ?id)
       (values ?id (untagged-tag-id) #f (cons 'expr (bless '(void)))))

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
;;End:
