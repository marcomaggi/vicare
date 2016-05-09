;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.pass-recordise)
  (export pass-recordize)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.system-value)
    (ikarus.compiler.scheme-objects-ontology))


;;;; recordisation compiler pass

(define-syntax __module_who__
  (identifier-syntax 'pass-recordize))

(define* (pass-recordize input-expr)
  ;;Given a symbolic expression INPUT-EXPR representing  a form in the core language,
  ;;convert it into  a nested hierarchy of struct instances;  return the outer struct
  ;;instance.
  ;;
  ;;This function expects a symbolic expression with perfect syntax: no syntax errors
  ;;are  checked.   We  expect  this  function to  be  executed  without  errors,  no
  ;;exceptions should be raised unless an internal bug makes it happen.
  ;;
  ;;Recognise the following core language forms:
  ;;
  ;;   (library-letrec* ((?lhs ?loc ?rhs) ...) ?body)
  ;;   (quote ?datum)
  ;;   (if ?test ?consequent ?alternate)
  ;;   (set! ?lhs ?rhs)
  ;;   (begin ?body0 ?body ...)
  ;;   (let     ((?lhs ?rhs) ...) ?body)
  ;;   (letrec  ((?lhs ?rhs) ...) ?body)
  ;;   (letrec* ((?lhs ?rhs) ...) ?body)
  ;;   (case-lambda (?formals ?body) ...)
  ;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
  ;;   (lambda ?formals ?body)
  ;;   (foreign-call "?function-name" ?arg ...)
  ;;   (primitive ?prim)
  ;;   (annotated-call ?annotation ?fun ?arg ...)
  ;;   ?lex
  ;;   (?func ?arg ...)
  ;;   (typed-expr ?expr ?core-type)
  ;;
  ;;where:  a standalone  ?LEX atom  is  a lex  gensym, interpreted  as reference  to
  ;;binding; ?LHS stands for "left-hand side" and it is a lex gensym; ?RHS stands for
  ;;"right-hand side"; ?LOC is a loc gensym;  ?PRIM is a symbol representing the name
  ;;of a primitive function.
  ;;
  ;;About the argument CTXT
  ;;-----------------------
  ;;
  ;;Whenever possible we want closure objects to  be annotated with their name in the
  ;;original source  code; the name  of a closure  is the lex  gensym to which  it is
  ;;bound.  Examples:
  ;;
  ;;   (define a
  ;;     (lambda () ;annotated: a
  ;;       ---))
  ;;
  ;;   (define a
  ;;     (begin
  ;;       (do-something)
  ;;       (lambda () ;annotated: a
  ;;         ---))
  ;;
  ;;   (define a
  ;;     (let ((a 1) (b 2))
  ;;       (do-something)
  ;;       (lambda () ;annotated: a
  ;;         ---))
  ;;
  ;;   (set! a (lambda () ;annotated: a
  ;;             ---))
  ;;
  ;;   (set! a (begin
  ;;             (do-something)
  ;;             (lambda () ;annotated: a
  ;;               ---))
  ;;
  ;;   ((lambda (x) x)
  ;;      (lambda (y) y)) ;annotated: x
  ;;
  ;;This is what the CTXT argument in the subfunctions is for; it is carefully handed
  ;;to the functions that process forms that  might evaluate to a closure and finally
  ;;used to annotate struct instances of type CLAMBDA.
  ;;
  ;;The CTXT argument is handled as follows:
  ;;
  ;;* Upon entering the input expression, no name is defined in the code: CTXT is set
  ;;to #f.
  ;;
  ;;* Upon entering the right-hand side expression of a lexical binding definition or
  ;;assignment: CTXT is  set to ?LEX-GENSYM, where ?LEX-GENSYM is  the left-hand side
  ;;of the definition or assignment.
  ;;
  ;;* When processing ?RATOR in an application form:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;the current value  of CTXT is wrapped in  a list of a single  item.  For example,
  ;;while processing:
  ;;
  ;;   (let ((x ((lambda (y) y) 1)))
  ;;     ?body)
  ;;
  ;;upon entering the  operator "(lambda (y) y)",  CTXT is set to "(x)".   Notice that the
  ;;operator "(lambda (y) y)" itself has no name.
  ;;
  ;;* When processing a ?RAND in an application form:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;the old value of CTXT is discarded and a new value is selected if the ?RATOR is a
  ;;lambda sexp.  For example, while processing:
  ;;
  ;;   ((lambda (x) x)
  ;;      (lambda (y) y)) ;annotated: x
  ;;
  ;;upon entering the operand "(lambda (y) y)", CTXT is set to "x".
  ;;
  ;;* When calling MAKE-CLAMBDA  the value of CTXT is consumed: if  CTXT is a symbol,
  ;;it becomes the value of the field NAME of the CLAMBDA struct.
  ;;
  ;;* Upon entering the  body of a CLAMBDA-CASE: if CTXT is a  list of a single item,
  ;;the item is unwrapped  and becomes the current value of CTXT.   This is to handle
  ;;the case in which  the last form of a CLAMBDA-CASE body  returns a CLAMBDA, which
  ;;in turn is bound to a lex gensym.  Example:
  ;;
  ;;   (let ((x ((lambda (y) (lambda () y))
  ;;             1)))
  ;;     x)
  ;;
  ;;here we have an application form:
  ;;
  ;;   ((lambda (y) (lambda () y)) 1)
  ;;
  ;;in which the operator is:
  ;;
  ;;   (lambda (y) (lambda () y))
  ;;
  ;;and its  return value is the  result of "(lambda ()  y)" which ends up  being bound to
  ;;"x"; so we want "(lambda () y)" to be annotated as "x".
  ;;
  (E input-expr))


(case-define* E
  ;;Convert the symbolic  expression X representing code in the  core language into a
  ;;nested hierarchy of struct instances.
  ;;
  ;;When  X  is  recordised  code  representing the  right-hand  side  of  a  binding
  ;;definition: CTXT is the corresponding lex gensym.
  ;;
  ((X)
   (E X #f))
  ((X ctxt)
   (cond ((pair? X)
	  (%recordize-pair-sexp X ctxt))

	 ((symbol? X)
	  (cond ((lexical X)
		 ;;X  is a  lex  gensym  referencing a  binding  defined inside  this
		 ;;INPUT-EXPR.  For now, the recordised  reference to such binding is
		 ;;simply its PRELEX struct; we will  decide in a later compiler pass
		 ;;if this reference must extract a value from a loc gensym or simply
		 ;;reference a memory location on the Scheme stack.
		 => (lambda (prel)
		      (set-prelex-source-referenced?! prel #t)
		      prel))
		(else
		 ;;X is *not* a lex gensym  referencing a binding defined inside this
		 ;;INPUT-EXPR.   We  default  to  interpreting  X  as  a  loc  gensym
		 ;;referencing a top level lexical variable defined by:
		 ;;
		 ;;* A previously processed input  expression; for example a previous
		 ;;expression  read  at the  REPL  and  expanded in  the  interaction
		 ;;environment.
		 ;;
		 ;;* A library that was imported in the environment.
		 ;;
		 ;;To reference such binding we have to generate recordised code that
		 ;;extracts the value from the binding's loc gensym; this is what the
		 ;;primitive TOP-LEVEL-VALUE does.
		 ;;
		 ;;NOTE When the binding was added to an interaction environment by a
		 ;;previously evaluated  expression: we  expect the expander  to have
		 ;;generated a  single gensym  to serve  both as  lex gensym  and loc
		 ;;gensym; so, here, X is both the lex gensym and the loc gensym.
		 ;;
		 ;;If instead X  is an unbound variable: the  call to TOP-LEVEL-VALUE
		 ;;will fail at run-time.
		 ;;
		 ;;NOTE TOP-LEVEL-VALUE is both a  primitive function and a primitive
		 ;;operation.
		 (make-funcall (mk-primref 'top-level-value)
			       (list (make-constant X))))))

	 (else
	  (compile-time-error __module_who__ __who__
	    "invalid core language expression" X)))))


(define-syntax-rule (%recordize-pair-sexp X ctxt)
  (case (car X)

    ;;Synopsis: (quote ?datum)
    ;;
    ;;Return a struct instance of type CONSTANT.
    ;;
    ((quote)
     (make-constant (cadr X)))

    ;;Synopsis: (typed-expr ?expr ?core-type-name)
    ;;
    ;;Return a struct instance of type TYPED-EXPR.
    ;;
    ((typed-expr)
     (let ((expr      (E (cadr X)))
	   (core-type (internal-body
			(module (name->core-type-tag)
			  (import SCHEME-OBJECTS-ONTOLOGY))
			(name->core-type-tag (caddr X)))))
       (make-typed-expr expr core-type)))

    ;;Synopsis: (if ?test ?consequent ?alternate)
    ;;
    ;;Return a struct instance of type CONDITIONAL.
    ;;
    ((if)
     (make-conditional
	 (E (cadr X))
	 (E (caddr X) ctxt)
       (E (cadddr X) ctxt)))

    ;;Synopsis: (set! ?lhs ?rhs)
    ;;
    ;;If  the left-hand  side references  a  lexical binding  defined by  INPUT-EXPR:
    ;;return a struct  instance of type ASSIGN.  If the  left-hand side references an
    ;;imported binding  or a  binding defined in  a previously  processed expression:
    ;;return a new struct instance of  type FUNCALL representing a SET! operation for
    ;;a variable  whose value is stored  in the "value"  field of a loc  gensym.  For
    ;;more details: see the documentation of the primitive TOP-LEVEL-VALUE.
    ;;
    ((set!)
     (let* ((lhs.sexp (cadr  X))   ;left-hand side
	    (rhs.sexp (caddr X))   ;right-hand side
	    ;;We recordize the right-hand side in the context of LHS.
	    (rhs.reco (E rhs.sexp lhs.sexp)))
       (cond ((lexical lhs.sexp)
	      => (lambda (prel)
		   (set-prelex-source-assigned?! prel #t)
		   (make-assign prel rhs.reco)))
	     (else
	      ;;Here we assume LHS.SEXP is a loc gensym.
	      (make-funcall (mk-primref '$init-symbol-value!)
			    (list (make-constant lhs.sexp) rhs.reco))))))

    ;;Synopsis: (begin ?body0 ?body ...)
    ;;
    ;;Build and return nested hierarchy of SEQ structures:
    ;;
    ;;   #[seq ?body0 #[seq ?body ...]]
    ;;
    ((begin)
     (let recur ((A (cadr X))
		 (D (cddr X)))
       (if (pair? D)
	   (make-seq (E A) (recur (car D) (cdr D)))
	 (E A ctxt))))

    ;;Synopsis: (let     ((?lhs ?rhs) ...) ?body)
    ;;Synopsis: (letrec  ((?lhs ?rhs) ...) ?body)
    ;;Synopsis: (letrec* ((?lhs ?rhs) ...) ?body)
    ;;
    ;;Each ?LHS is a lex gensym representing  the name of the binding; this gensym is
    ;;unique for this binding in the whole history of the Universe.
    ;;
    ;;Return, respectively, a struct instance of type: BIND, RECBIND, REC*BIND.
    ;;
    ((let letrec letrec*)
     (let ((bind* (cadr  X))		     ;list of bindings
	   (body  (caddr X)))		     ;list of body forms
       (let ((lex* ($map/stx car  bind*))    ;list of bindings left-hand sides
	     (rhs* ($map/stx cadr bind*)))   ;list of bindings right-hand sides
	 (with-prelex-structs-in-plists (prel* lex*)
	   (let* ((rhs*^ ($map/stx E rhs* lex*))
		  (body^ (E body ctxt)))
	     (case (car X)
	       ((let)
		(make-bind     prel* rhs*^ body^))
	       ((letrec)
		(make-recbind  prel* rhs*^ body^))
	       ((letrec*)
		(make-rec*bind prel* rhs*^ body^))))))))

    ;;Synopsis: (library-letrec* ((?lex ?loc ?rhs) ...) ?body)
    ;;
    ;;A LIBRARY form like:
    ;;
    ;;   (library (the-lib)
    ;;     (export ---)
    ;;     (import ---)
    ;;     (define ?lhs ?rhs)
    ;;     ...
    ;;     ?expr ...)
    ;;
    ;;is converted by the expander into:
    ;;
    ;;   (library-letrec* ((?lex ?loc ?rhs) ...) ?expr ...)
    ;;
    ;;where:
    ;;
    ;;* ?LEX is the  lex gensym representing the name of the  binding; this gensym is
    ;;  unique for this binding in the whole history of the Universe.
    ;;
    ;;* ?LOC is  the loc gensym used to  hold the value of the binding  (in the VALUE
    ;;  field of the  symbol's memory block); this gensym is  unique for this binding
    ;;  in the whole history of the Universe.
    ;;
    ;;* ?RHS is a symbolic expression which evaluates to the binding's value.
    ;;
    ;;Return a struct  instance of type REC*BIND.  The difference  between a REC*BIND
    ;;representing a LETREC* and a REC*BIND  representing a LIBRARY-LETREC* is in the
    ;;PRELEX structs.
    ;;
    ((library-letrec*)
     (let ((bind* (cadr  X))		      ;list of bindings
	   (body  (caddr X)))		      ;list of body forms
       (let ((lex* ($map/stx car   bind*))    ;list of lex gensyms
	     (loc* ($map/stx cadr  bind*))    ;list of loc gensyms
	     (rhs* ($map/stx caddr bind*)))   ;list of bindings right-hand sides
	 (with-prelex-structs-in-plists (prel* lex*)
	   ($for-each/stx set-prelex-global-location! prel* loc*)
	   (let* ((rhs*^ ($map/stx E rhs* lex*))
		  (body^ (E body ctxt)))
	     (make-rec*bind prel* rhs*^ body^))))))

    ;;Synopsis: (case-lambda (?formals ?body) ...)
    ;;
    ;;Return a struct instance of type CLAMBDA.
    ;;
    ((case-lambda)
     (let* ((name     (and (symbol? ctxt) ctxt))
	    (asmlabel (%name->asmlabel name))
	    (cases    (E-clambda-case* asmlabel (cdr X) ctxt)))
       (let ((cp       #f)
	     (freevar* #f))
	 (make-clambda asmlabel cases cp freevar* name))))

    ;;Synopsis: (annotated-case-lambda ?annotation (?formals ?body))
    ;;
    ;;Return a struct instance of type CLAMBDA.
    ;;
    ((annotated-case-lambda)
     (let* ((name     (cons (and (symbol? ctxt) ctxt)
			    ;;This annotation  is excluded  only when  building the
			    ;;boot image.
			    (and (not (strip-source-info))
				 (let ((annotated-expr (cadr X)))
				   (and (reader-annotation?       annotated-expr)
					(reader-annotation-source annotated-expr))))))
	    (asmlabel (%name->asmlabel name))
	    (cases    (E-clambda-case* asmlabel (cddr X) ctxt)))
       (let ((cp       #f)
	     (freevar* #f))
	 (make-clambda asmlabel cases cp freevar* name))))

    ;;Synopsis: (lambda ?formals ?body)
    ;;
    ;;LAMBDA functions are handled as special cases of CASE-LAMBDA functions.
    ;;
    ;;   (lambda ?formals ?body)
    ;;   ===> (case-lambda (?formals ?body))
    ;;
    ((lambda)
     (E `(case-lambda ,(cdr X)) ctxt))

    ;;Synopsis: (foreign-call "?function-name" ?arg ...)
    ;;
    ;;Return a struct instance of type FORCALL.
    ;;
    ((foreign-call)
     (let ((name (quoted-string (cadr X)))
	   (arg* (cddr X)))
       (make-forcall name ($map/stx E arg*))))

    ;;Synopsis: (primitive ?prim)
    ;;
    ;;Return a struct  instance of type PRIMREF.  ?PRIM is  a symbol representing the
    ;;public name of the primitive function or primitive operation.
    ;;
    ;;NOTE Every  time the  expander recognises an  identifier in  reference position
    ;;captured by  a lexical binding  (not a syntax) exported  by the boot  image: it
    ;;generates this symbolic expression as core language form.  For example:
    ;;
    ;;   (fx+ 1 2)
    ;;
    ;;is expanded into:
    ;;
    ;;   ((primitive fx+) '1 '2)
    ;;
    ;;and recordised to:
    ;;
    ;;   (funcall (primref fx+) (constant 1) (constant 2))
    ;;
    ((primitive)
     (let ((name (cadr X)))
       (mk-primref name)))

    ;;Synopsis: (annotated-call ?annotation ?fun ?arg ...)
    ;;
    ;;Return a struct instance of type FUNCALL.
    ;;
    ((annotated-call)
     (E-annotated-call X ctxt))

    (else	;if X is a pair here, it is a function call
     ;;Synopsis: (?func ?rand ...)
     ;;
     ;;Return a struct instance of type FUNCALL.
     ;;
     (let ((func  (car X))
	   (rand* (cdr X)))
       (E-app make-funcall func rand* ctxt)))))


(define (%name->asmlabel name)
  (if (generate-descriptive-labels?)
      (gensym (string-append "asmlabel:"
			     (cond ((symbol? name)
				    (symbol->string name))
				   ((and (pair? name)
					 (symbol? (car name)))
				    (symbol->string (car name)))
				   (else
				    "anonymous"))
			     ":clambda"))
    (gensym)))


(module (quoted-sym)

  (define (quoted-sym? obj)
    ;;Return true if OBJ is a sexp with the format:
    ;;
    ;;   (quote ?symbol)
    ;;
    (and (list? obj)
	 (null? (cddr obj))
	 (eq? 'quote (car obj))
	 (symbol? (cadr obj))))

  (define* (quoted-sym {x quoted-sym?})
    ;;Check that X has the format:
    ;;
    ;;   (quote ?symbol)
    ;;
    ;;and return ?SYMBOL.
    ;;
    (cadr x))

  #| end of module: quoted-sym |# )


(module (quoted-string)

  (define (quoted-string? obj)
    ;;Check that X has the format:
    ;;
    ;;   (quote ?string)
    ;;
    (and (list? obj)
	 (null? (cddr obj))
	 (eq? 'quote (car obj))
	 (string? (cadr obj))))

  (define* (quoted-string {x quoted-string?})
    ;;Check that X has the format:
    ;;
    ;;  (quote ?string)
    ;;
    ;;and return ?string.
    ;;
    (cadr x))

  #| end of module: quoted-string |# )


(module (E-clambda-case*)

  (define (E-clambda-case* asmlabel clause* ctxt)
    ;;Given a symbolic expression representing a lambda:
    ;;
    ;;   (lambda ?formals ?body)
    ;;   (case-lambda (?formals ?body) ...)
    ;;   (annotated-case-lambda ?annotation (?formals ?body))
    ;;
    ;;this function is called with CLAUSE* set to the list of clauses:
    ;;
    ;;   ((?formals ?body) ...)
    ;;
    ;;Return a list  holding new struct instances of type  CLAMBDA-CASE, one for each
    ;;clause.
    ;;
    ;;ASMLABEL is  the label  identifying the  machine code entry  point of  the full
    ;;CLAMBDA; it is used to generate descriptive labels for each clause.
    ;;
    (let ((ctxt (and (pair? ctxt) (car ctxt))))
      ($map/stx
	  (lambda (clause)
	    ;;We expect clause to have the format:
	    ;;
	    ;;   (?formals ?body)
	    ;;
	    (let ((fml* (car  clause))	  ;the formals
		  (body (cadr clause)))	  ;the body sequence
	      (let ((lex* (%properize-clambda-formals fml*)))
		(with-prelex-structs-in-plists (prel* lex*)
		  (let* ((body^ (E body ctxt))
			 ;;PROPER? is: true  if FML* is a proper list;  false if it
			 ;;is an improper list, including a standalone lex gensym.
			 (info  (let* ((proper?       (list? fml*))
				       (asmlabel-case (%asmlabel->asmlabel-case asmlabel proper? fml*)))
				  (make-case-info asmlabel-case prel* proper?))))
		    (make-clambda-case info body^))))))
	clause*)))

  (define (%asmlabel->asmlabel-case asmlabel proper? fml*)
    (if (generate-descriptive-labels?)
	(gensym (string-append (symbol->string asmlabel)
			       ":case-"
			       (if proper?
				   (number->string (length fml*))
				 "*")))
      (gensym)))

  (define (%properize-clambda-formals fml*)
    ;;Convert the formals FML* of a CASE-LAMBDA clause into a proper list.  We expect
    ;;FML* to be a valid CASE-LAMBDA formals specification, one among:
    ;;
    ;;   (?arg-symbol ...)
    ;;   (?arg-symbol ... . ?rest-symbol)
    ;;   ?args-symbol
    ;;
    ;;where the symbols are  lex gensyms; here we do *not* validate  the items in the
    ;;list as symbols.  If FML* is:
    ;;
    ;;* null or a proper list: return a new list holding the same values:
    ;;
    ;;     (%properize-clambda-formals '())			=> ()
    ;;     (%properize-clambda-formals '(arg1 arg2 arg3))	=> (arg1 arg2 arg3)
    ;;
    ;;* an improper list: return a new proper list holding the same values:
    ;;
    ;;     (%properize-clambda-formals '(arg1 arg2 . rest-arg))
    ;;     => (arg1 arg2 rest-arg)
    ;;
    ;;* not a list: return a list wrapping it:
    ;;
    ;;     (%properize-clambda-formals args) => (args)
    ;;
    (cond ((pair? fml*)
	   (cons (car fml*)
		 (%properize-clambda-formals (cdr fml*))))
	  ((null? fml*)
	   '())
	  (else
	   (list fml*))))

  #| end of module: E-clambda-case* |# )


(module (E-annotated-call)

  (define (E-annotated-call X ctxt)
    ;;This  function is  a  wrapper for  "E-app",  with the  purpose  of building  an
    ;;appropriate  MK-CALL  argument  for  it.   We  expect  X  to  be  the  symbolic
    ;;expression:
    ;;
    ;;   (annotated-call ?annotation ?fun ?arg ...)
    ;;
    ;;where ?ANNOTATION  is either a struct  instance of type ANNOTATION,  defined by
    ;;the reader, or a syntax object constructed by the expander.
    ;;
    ;;NOTE At present, this function is the only place in the compiler that makes use
    ;;of the parameter GENERATE-DEBUG-CALLS.  (Marco Maggi; Oct 11, 2012)
    ;;
    (let ((anno (cadr  X))    ;annotation
	  (func (caddr X))    ;expression evaluating to the function
	  (args (cdddr X)))   ;arguments
      (let ((mk-call (if (generate-debug-calls)
			 (%make-funcall-maker anno)
		       make-funcall)))
	(E-app mk-call func args ctxt))))

  (module (%make-funcall-maker)

    (define (%make-funcall-maker anno)
      (let ((src/expr (make-constant (if (reader-annotation? anno)
					 (cons (reader-annotation-source   anno)
					       (reader-annotation-stripped anno))
				       (cons #f (syntax->datum anno))))))
	(lambda (op rands)
	  (if (%core-primitive-reference? op)
	      ;;This is an  annotated call to a core primitive  function or primitive
	      ;;operation: ignore debugging mode, handle it as a normal call.
	      (make-funcall op rands)
	    ;;This is an  annotated call to a user-defined  function: honor debugging
	    ;;mode and generate a DEBUG-CALL.
	    (make-funcall (mk-primref 'debug-call)
			  (cons* src/expr op rands))))))

    (define (%core-primitive-reference? op)
      ;;Evaluate to  true if OP references  a lexical core primitive  exported by the
      ;;boot image.
      ;;
      ;;The SYSTEM-VALUE call below will fail  with an assertion violation if NAME is
      ;;not a  symbol associated  to a  lexical core primitive  exported by  the boot
      ;;image.  See the documentation of SYSTEM-VALUE for more details.
      ;;
      ;;NOTE When compiling a library: SYSTEM-VALUE  will return with no exception if
      ;;OP is a core primitive exported by the boot image.  When compiling a new boot
      ;;image: SYSTEM-VALUE will  return with no exception if OP  is a core primitive
      ;;exported by the *old* boot image; so SYSTEM-VALUE must be the one exported by
      ;;the old boot image.
      ;;
      (struct-case op
	((primref name)
	 (guard (C ((assertion-violation? C)
		    #t))
	   (system-value name)
	   #f))
	(else #f)))

    #| end of module: %MAKE-FUNCALL-MAKER |# )

  #| end of module: E-annotated-call |# )


(module (E-app)

  (define (E-app mk-call rator rand* ctxt)
    ;;Process a form  representing a function call; return a  struct instance of type
    ;;FUNCALL.   The final  purpose  of this  function  is to  apply  MK-CALL to  the
    ;;recordised version of RATOR and RAND*.
    ;;
    ;;MK-CALL is either MAKE-FUNCALL  or a wrapper for it.  RATOR  is a core language
    ;;expression evaluating  to the operator  of the call.  RAND*  is a list  of core
    ;;language expressions evaluating  to the call operands.  When  the function call
    ;;form is:
    ;;
    ;;   (?func ?arg ...)
    ;;
    ;;the argument RATOR is ?FUNC and the argument RAND* is (?ARG ...).
    ;;
    ;;We handle specially the cases in which RATOR is one of the sexps:
    ;;
    ;;   (primitive make-parameter)
    ;;
    ;;by  generating a  core language  expression to  be integrated  in the  original
    ;;source.
    ;;
    (define-syntax-rule (%common-function-application)
      (E-function-application mk-call rator rand* ctxt))
    (if (and (pair? rator)
	     (eq? 'primitive (car rator)))
	(case (cadr rator)
	  ((make-parameter)
	   (E-integration-make-parameter mk-call rand* ctxt))
	  ;;NOTE  With  this function  written  as  it  is,  everything is  ready  to
	  ;;introduce the integration of further lexical core primitives as in:
	  ;;
	  ;;((map)
	  ;; (E-integration-map mk-call rand* ctxt))
	  ;;
	  ;;But  we  should  consider  this   with  care,  because  introducing  such
	  ;;integrations  here   does  no  allow   us  to  take  advantage   of  type
	  ;;informations; it is most likely better  to do it in the expander.  (Marco
	  ;;Maggi; Wed Aug 27, 2014)
	  (else
	   (%common-function-application)))
      (%common-function-application)))

  (module (E-function-application)
    ;;NOTE In case RATOR is a lambda sexp with one of the formats:
    ;;
    ;;   (case-lambda                       (?formals ?body) ...)
    ;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
    ;;
    ;;the function application looks like:
    ;;
    ;;   ((lambda (x) x) 123)
    ;;
    ;;In this case, if one of the RAND* evaluates to a closure as in:
    ;;
    ;;   ((lambda (x) x) (lambda (y) y))
    ;;
    ;;we  want  the operand  "(lambda  (y)  y)" to  be  annotated  with the  corresponding
    ;;operator's formal name "x".
    ;;
    (define (E-function-application mk-call rator rand* ctxt)
      (let ((op    (E rator (list ctxt)))
	    (rand* (E-rand* rator rand*)))
	(mk-call op rand*)))

    (define (E-rand* rator rand*)
      (let ((fmls (%get-matching-formals rator rand*)))
	(if (null? fmls)
	    ($map/stx E rand*)
	  ;;FMLS is a proper or improper list of lex gensyms representing the formals
	  ;;of a  lambda sexp  case.  Here we  want to  apply E to  the RAND*  in the
	  ;;context of the corresponding formal.  Example, the function application:
	  ;;
	  ;;   ((lambda (lex.a lex.b lex.c) ?body)
	  ;;      ?rand0 ?rand1 ?rand2)
	  ;;
	  ;;has matching FMLS:
	  ;;
	  ;;   (lex.a lex.b lex.c)
	  ;;
	  ;;and will cause the following applications:
	  ;;
	  ;;   (E ?rand0 lex.a)
	  ;;   (E ?rand1 lex.b)
	  ;;   (E ?rand2 lex.c)
	  ;;
	  ;;Example, the function application:
	  ;;
	  ;;   ((lambda (lex.a . lex.rest) ?body)
	  ;;      ?rand0 ?rand1 ?rand2)
	  ;;
	  ;;has matching FMLS:
	  ;;
	  ;;   (lex.a . lex.rest)
	  ;;
	  ;;and will cause the following applications:
	  ;;
	  ;;   (E ?rand0 lex.a)
	  ;;   (E ?rand1)
	  ;;   (E ?rand2)
	  ;;
	  (let recur ((rand* rand*)
		      (fmls  fmls))
	    (if (pair? fmls)
		(cons (let ((ctxt (car fmls)))
			(E (car rand*) ctxt))
		      (recur (cdr rand*) (cdr fmls)))
	      ($map/stx E rand*))))))

    (module (%get-matching-formals)

      (define (%get-matching-formals rator rand*)
	;;RATOR  must be  a  core  language expression  representing  code that  will
	;;evaluate to  a function call operator.   RAND* must be a  list of arguments
	;;for such function.
	;;
	;;If RATOR is a lambda sexp, with one of the formats:
	;;
	;;   (case-lambda (?formals ?body) ...)
	;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
	;;
	;;scan the cases  in RATOR looking for  a set of formals  that matches RAND*:
	;;when found, return the list of formals ?FORMALS.
	;;
	;;When no  matching formals  are found  or the  RATOR is  not a  lambda sexp:
	;;return null.
	;;
	(let loop ((case* (%extract-lambda-cases rator)))
	  (cond ((null? case*)
		 ;;The RATOR is not a lambda sexp, or it is but no case matched.
		 '())
		((let ((fmls (caar case*)))
		   (%matching? fmls rand*))
		 ;;The RATOR is a lambda sexp and the first case in CASE* matches the
		 ;;RAND*: return the formals.
		 (caar case*))
		(else
		 ;;The RATOR is  a lambda sexp and  the first case in  CASE* does not
		 ;;match: try the next.
		 (loop (cdr case*))))))

      (define (%matching? fmls rand*)
	;;FMLS is a  proper or improper list of lex  gensyms representing the formals
	;;of  a lambda  case;  RAND* is  a  proper list  of  recordised code  structs
	;;representing funcation application operands.  Return true if FMLS and RAND*
	;;match each other, otherwise return false.
	;;
	(cond ((null? fmls)
	       (null? rand*))
	      ((pair? fmls)
	       (and (pair? rand*)
		    (%matching? (cdr fmls) (cdr rand*))))
	      (else #t)))

      (define (%extract-lambda-cases rator)
	;;Given the sexp RATOR with one of the formats:
	;;
	;;   (case-lambda                       (?formals ?body) ...)
	;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
	;;
	;;return the list of cases:
	;;
	;;   ((?formals ?body) ...)
	;;
	;;return null if RATOR is not a lambda sexp.
	;;
	(if (pair? rator)
	    (case (car rator)
	      ((case-lambda)
	       (cdr rator))
	      ((annotated-case-lambda)
	       (cddr rator))
	      (else '()))
	  '()))

      #| end of module: GET-MATCHING-FORMALS |# )

    #| end of module: E-function-application |# )

;;; --------------------------------------------------------------------

  (define (E-integration-make-parameter mk-call rand* ctxt)
    ;;If  the number  of  operands is  correct for  MAKE-PARAMETER,  generate a  core
    ;;language expression to be integrated in place of the function application:
    ;;
    ;;   ((primitive make-parameter) ?rand ...)
    ;;
    ;;otherwise we raise an assertion violation: at run-time if "strict R6RS" mode is
    ;;enabled, otherwise at compile-time.
    ;;
    ;;NOTE The one  below is the original Ikarus implementation;  it was applying the
    ;;guard function  every time  and also  applying the guard  function to  the init
    ;;value (Marco Maggi; Feb 3, 2012).
    ;;
    ;;   ((case-lambda
    ;;     ((,t0)
    ;;      (case-lambda
    ;;       (() ,t0)
    ;;       ((,x) (set! ,t0 (,f ,x))))
    ;;      (,f ,t))))
    ;;
    (case (length rand*)
      ((1)	;MAKE-PARAMETER called with one argument.
       (let ((val-expr	(car rand*))
	     (t		(gensym 't))
	     (x		(gensym 'x))
	     (bool		(gensym 'bool)))
	 (E `((lambda (,t)
		(case-lambda
		 (() ,t)
		 ((,x) (set! ,t ,x))
		 ((,x ,bool)
		  (set! ,t ,x))))
	      ,val-expr)
	    ctxt)))
      ((2)	;MAKE-PARAMETER called with two arguments.
       (let ((val-expr	(car rand*))
	     (guard-expr	(cadr rand*))
	     (f		(gensym 'f))
	     (t		(gensym 't))
	     (t0		(gensym 't))
	     (x		(gensym 'x))
	     (bool		(gensym 'bool)))
	 (E `((case-lambda
	       ((,t ,f)
		(if ((primitive procedure?) ,f)
		    ((case-lambda
		      ((,t0)
		       (case-lambda
			(() ,t0)
			((,x) (set! ,t0 (,f ,x)))
			((,x ,bool)
			 (if ,bool
			     (set! ,t0 (,f ,x))
			   (set! ,t0 ,x))))))
		     ,t)
		  ((primitive procedure-argument-violation) 'make-parameter
		   '"expected procedure as guard function argument" ,f))))
	      ,val-expr
	      ,guard-expr)
	    ctxt)))
      (else
       (if (options::strict-r6rs)
	   (begin
	     (print-compiler-warning-message
	      "invalid number of operands to core language function integration: ~a"
	      rand*)
	     (mk-call (make-primref 'make-parameter) ($map/stx E rand*)))
	 (assertion-violation 'make-parameter
	   "invalid number of operands to core language function integration"
	   rand*)))))

  #| end of module: E-app |# )


(module (with-prelex-structs-in-plists lexical lex*->prelex* %remove-prelex-from-proplist-of-lex)
  ;;This  module takes  care of  generating a  PRELEX structure  for each  lex gensym
  ;;associated to a lexical binding.
  ;;
  ;;Remember that the function RECORDIZE is called to process: full LIBRARY forms the
  ;;expander  has transformed  into LIBRARY-LETREC*  core language  forms; full  R6RS
  ;;programs the  expander has transformed  into LIBRARY-LETREC core  language forms;
  ;;standalone expressions from  invocations of R6RS's EVAL, for example  read by the
  ;;REPL.
  ;;
  ;;This is how bindings are handled:
  ;;
  ;;*  When  RECORDIZE  enters  a LAMBDA,  ANNOTATED-CASE-LAMBDA,  CASE-LAMBDA,  LET,
  ;;  LETREC, LETREC*, LIBRARY-LETREC* core language form: for each defined binding a
  ;;  PRELEX  struct is built and  stored in the  property list of the  binding's lex
  ;;  gensym.
  ;;
  ;;* While RECORDIZE processes the body of  the binding core language form: each lex
  ;;  gensym associated to a local binding contains a PRELEX in its property list.
  ;;
  ;;* When RECORDIZE exits the binding core language form: all the PRELEX structs are
  ;;  removed from the lex gensyms property lists.
  ;;
  ;;So:
  ;;
  ;;* While processing a LIBRARY-LETREC* form:  all the lex gensyms associated to top
  ;;  level bindings defined inside the form have a PRELEX in their property list.
  ;;
  ;;* While  processing a  standalone expression:  the lex  gensyms associated  to an
  ;;  internally  defined binding do  have a PRELEX in  their property list;  the lex
  ;;  gensyms associated  to a previously defined  binding do *not* have  a PRELEX in
  ;;  their property list.
  ;;
  ;;For example,  let's say we are  evaluating expressions at the  REPL; new bindings
  ;;created  by  DEFINE  are  added   to  the  interaction  environment  returned  by
  ;;INTERACTION-ENVIRONMENT.  So if we do:
  ;;
  ;;   vicare> (define a 1)
  ;;
  ;;the expander converts this DEFINE form into a SET! form and adds a binding to the
  ;;interaction environment; while recordizing this  expression the lex gensym *does*
  ;;have a  PRELEX in  its property  list; lexical bindings  added to  an interaction
  ;;environment have a  single gensym to be  used as both lex gensym  and loc gensym.
  ;;If later we do:
  ;;
  ;;   vicare> a
  ;;
  ;;the expander finds  the binding in the interaction environment  and converts this
  ;;variable  reference  into  a  standalone   lex  gensym;  while  recordizing  this
  ;;expression the lex gensym *does not* have a PRELEX in its property list.
  ;;
  (import (vicare system $symbols))

  (define-syntax (with-prelex-structs-in-plists stx)
    (syntax-case stx ()
      ((_ (?prel* ?lex*) ?body0 ?body ...)
       (and (identifier? #'?prel*)
	    (identifier? #'?lex*))
       #'(let ((?prel* (lex*->prelex* ?lex*)))
	   (begin0
	       (begin ?body0 ?body ...)
	     (%remove-prelex-from-proplist-of-lex ?lex*))))
      ))

  ;;FIXME Do we need a new cookie at each call to the RECORDIZE function?  Maybe not,
  ;;because   we    always   call    LEX*->PRELEX*   and    then   clean    up   with
  ;;%REMOVE-PRELEX-FROM-PROPLIST-OF-LEX.  (Marco Maggi; Oct 10, 2012)
  (define-constant *COOKIE*
    (gensym "prelex-for-lex"))

  (module (lexical)

    (define-syntax-rule (lexical ?X)
      ;;If  the lex  gensym ?X  has been  defined in  the expression  currently being
      ;;recordised: return  the associated PRELEX struct.   If the lex gensym  ?X has
      ;;been defined in a previously processed expression: return false.
      ;;
      ($getprop ?X *COOKIE*))

    (define ($getprop x k)
      ($assq+cdr k ($symbol-plist x)))

    (define-syntax ($assq+cdr stx)
      ;;The expansion of this macro is equivalent to:
      ;;
      ;;   (cond ((assq key ell)
      ;;          => cdr)
      ;;         (else #f))
      ;;
      (syntax-case stx ()
	((_ ?key ?ell)
	 #'(let loop ((key ?key)
		      (ell ?ell))
	     (and (pair? ell)
		  (if (eq? key (caar ell))
		      (cdar ell)
		    (loop key (cdr ell))))))
	))

    #| end of module: LEXICAL |# )

  (module (lex*->prelex*)

    (define (lex*->prelex* lex*)
      ;;Process the  formals and left-hand sides  of the core language  forms LAMBDA,
      ;;CASE-LAMBDA,  ANNOTATED-CASE-LAMBDA, LET,  LETREC, LETREC*,  LIBRARY-LETREC*.
      ;;Expect LEX*  to be  a list  of lex gensyms;  for each  LEX generate  a PRELEX
      ;;structure and store it  in the property list of the LEX.   Return the list of
      ;;PRELEX structures.
      ;;
      ;;The property list keyword is the gensym bound to *COOKIE*.
      ;;
      ($map/stx (lambda (lex)
		  (receive-and-return (prel)
		      (make-prelex lex)
		    ($putprop lex *COOKIE* prel)))
	lex*))

    (define-syntax ($putprop stx)
      ;;The expansion of this syntax is equivalent to:
      ;;
      ;;   (putprop symbol key value)
      ;;
      (syntax-case stx ()
	((_ ?symbol ?key ?value)
	 #'(let ((symbol  ?symbol)
		 (key     ?key)
		 (value   ?value))
	     (let loop ((plist ($symbol-plist symbol)))
	       (if (pair? plist)
		   (if (eq? key (caar plist))
		       (set-cdr! (car plist) value)
		     (loop (cdr plist)))
		 ($set-symbol-plist! symbol (cons (cons key value) plist))))))
	))

    #| end of module: LEX*->PRELEX* |# )

  (module (%remove-prelex-from-proplist-of-lex)

    (define (%remove-prelex-from-proplist-of-lex lex*)
      ;;Process the  formals and left-hand sides  of the core language  forms LAMBDA,
      ;;CASE-LAMBDA,  ANNOTATED-CASE-LAMBDA, LET,  LETREC, LETREC*,  LIBRARY-LETREC*.
      ;;Expect  LEX*  to  be  a  list  of  lex  gensyms  previsously  processed  with
      ;;LEX*->PRELEX*; for  each LEX  remove the PRELEX  structure from  its property
      ;;list.  Return unspecified values.
      ;;
      ;;The property list keyword is the gensym bound to *COOKIE*.
      ;;
      ($for-each/stx (lambda (lex)
		       ($remprop lex *COOKIE*))
	lex*))

    (define-syntax-rule ($remprop ?symbol ?key)
      (let* ((symbol ?symbol)
	     (key    ?key)
	     (plist  ($symbol-plist symbol)))
	(when (pair? plist)
	  (let ((a (car plist)))
	    (if (eq? (car a) key)
		($set-symbol-plist! symbol (cdr plist))
	      (let loop ((q     plist)
			 (plist (cdr plist)))
		(when (pair? plist)
		  (let ((a (car plist)))
		    (if (eq? (car a) key)
			(set-cdr! q (cdr plist))
		      (loop plist (cdr plist)))))))))))

    #| end of module: %REMOVE-PRELEX-FROM-PROPLIST-OF-LEX |# )

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
