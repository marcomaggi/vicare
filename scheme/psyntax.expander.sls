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


;;;; introduction: bibliography
;;
;;To  understand  this  code  we  must  read the  relevant  chapter  in  the  Texinfo
;;documentation.
;;
;;There are  multiple documents  we can study  to get a  better understanding  of the
;;expander's code.  Most likely the top document is:
;;
;;[1] Abdulaziz Ghuloum.  "Implicit Phasing for Library Dependencies".  Ph.D. thesis.
;;    Department of Computer Science, Indiana University.  December 2008.
;;
;;from the very  author of this library; it  is quite long.  Here are  some papers on
;;syntax-case macros and R6RS libraries:
;;
;;[2]  R.   Kent Dybvig.   "Writing  Hygienic  Macros  in Scheme  with  Syntax-Case".
;;    Department  of Computer  Science,  Indiana University.   Technical Report  356.
;;    June 1992.
;;
;;[3] Abdulaziz  Ghuloum, R.   Kent Dybvig.  "Implicit  Phasing for  R6RS Libraries".
;;    Department of Computer Science, Indiana University.
;;
;;and the very paper that introduced hygienic macros:
;;
;;[4]  Eugene  Kohlbecker,  Daniel  P.  Friedman,  Matthias  Felleisen,  Bruce  Duba.
;;    "Hygienic   Macro  Expansion".    Department  of   Computer  Science,   Indiana
;;    University.  1986.
;;
;;Here  is  another  long  document   covering  muliple  components  of  this  Scheme
;;implementation:
;;
;;[5] Oscar Waddell.  "Extending the  Scope of Syntactic Abstraction".  Ph.D. thesis.
;;    Department of Computer Science, Indiana University.  August 1999.
;;
;;A discussion of syntax parameters as implemented in Racket is in the paper:
;;
;;[6] Eli  Barzilay, Ryan  Culpepper, Matthew  Flatt. "Keeping  it clean  with syntax
;;    parameters".  2011.
;;


#!vicare
(library (psyntax.expander)
  (export
    eval
    environment				environment?
    null-environment			scheme-report-environment
    interaction-environment		new-interaction-environment

    enable-tagged-language		disable-tagged-language

    ;; inspection of non-interaction environment objects
    environment-symbols			environment-libraries
    environment-labels			environment-binding

    expand-form-to-core-language
    expand-top-level			expand-top-level->sexp
    expand-library			expand-library->sexp
    expand-r6rs-top-level-make-evaluator
    expand-r6rs-top-level-make-compiler

    make-variable-transformer		variable-transformer?
    variable-transformer-procedure

    make-synonym-transformer		synonym-transformer?
    synonym-transformer-identifier

    make-compile-time-value		compile-time-value?
    compile-time-value-object		syntax-parameter-value

    identifier?
    free-identifier=?			bound-identifier=?
    identifier-bound?			print-identifier-info
    datum->syntax			syntax->datum
    parse-logic-predicate-syntax

    ;; exception raisers
    syntax-violation			assertion-error
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Sat Apr 12,
    ;;2014)
    syntax-error

    ;;SYNTAX-CASE subroutines
    syntax-dispatch			ellipsis-map

    ;; expand-time object type specs: type specification
    make-object-type-spec			object-type-spec?
    object-type-spec-parent-spec
    object-type-spec-uids
    object-type-spec-type-id			object-type-spec-pred-stx
    object-type-spec-constructor-maker
    object-type-spec-accessor-maker		object-type-spec-mutator-maker
    object-type-spec-getter-maker		object-type-spec-setter-maker
    object-type-spec-dispatcher
    object-type-spec-ancestry

    ;; expand-time object type specs: parsing tagged identifiers
    tagged-identifier-syntax?			parse-tagged-identifier-syntax
    list-of-tagged-bindings?			parse-list-of-tagged-bindings
    tagged-lambda-proto-syntax?			parse-tagged-lambda-proto-syntax
    tagged-formals-syntax?			parse-tagged-formals-syntax
    standard-formals-syntax?
    formals-signature-syntax?			retvals-signature-syntax?

    make-clambda-compound			clambda-compound?
    clambda-compound-common-retvals-signature	clambda-compound-lambda-signatures

    make-lambda-signature			lambda-signature?
    lambda-signature-formals			lambda-signature-retvals
    lambda-signature-formals-tags		lambda-signature-retvals-tags
    lambda-signature=?

    make-formals-signature			formals-signature?
    formals-signature-tags			formals-signature=?

    make-retvals-signature			make-retvals-signature-single-value
    make-retvals-signature-fully-unspecified
    retvals-signature?
    retvals-signature-tags			retvals-signature=?
    retvals-signature-common-ancestor

    ;; expand-time object type specs: identifiers defining types
    tag-identifier?				all-tag-identifiers?
    tag-super-and-sub?				formals-signature-super-and-sub-syntax?
    identifier-object-type-spec			set-identifier-object-type-spec!
    label-object-type-spec			set-label-object-type-spec!
    tag-identifier-ancestry			tag-common-ancestor

    set-tag-identifier-callable-signature!	tag-identifier-callable-signature
    fabricate-procedure-tag-identifier

    top-tag-id					void-tag-id
    procedure-tag-id				predicate-tag-id
    list-tag-id					boolean-tag-id
    struct-tag-id				record-tag-id

    ;; expand-time object type specs: tagged binding identifiers
    tagged-identifier?
    set-identifier-tag!		identifier-tag		override-identifier-tag!
    set-label-tag!		label-tag		override-label-tag!

    ;; expand-time type checking exception stuff
    expand-time-type-signature-violation?
    expand-time-retvals-signature-violation?
    expand-time-retvals-signature-violation-expected-signature
    expand-time-retvals-signature-violation-returned-signature

    ;; expand-time type specs: stuff for built-in tags and core primitives
    initialise-type-spec-for-built-in-object-types
    initialise-core-prims-tagging

    ;; bindings for (vicare expander)
    current-inferior-lexenv)
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer)
    (only (vicare)
	  fluid-let-syntax)
    (prefix (rnrs syntax-case) sys.)
    (rnrs mutable-pairs)
    (psyntax.special-transformers)
    (psyntax.lexical-environment)
    (psyntax.syntax-match)
    (psyntax.syntactic-binding-properties)
    (psyntax.syntax-utilities)
    (psyntax.tag-and-tagged-identifiers)
    (only (psyntax.core-primitives-properties)
	  initialise-core-prims-tagging)
    (psyntax.library-manager)
    (psyntax.builders)
    (psyntax.compat)
    (psyntax.config)
    (psyntax.internal))


;;; helpers

;;FIXME To  be removed at the  next boot image  rotation.  (Marco Maggi; Sat  Apr 12,
;;2014)
(define syntax-error)

(include "psyntax.helpers.scm" #t)

(define-syntax-rule (with-tagged-language ?enabled? . ?body)
  (parametrise ((option.tagged-language? (or ?enabled? (option.tagged-language?))))
    (parametrise ((option.tagged-language.rhs-tag-propagation? (option.tagged-language?))
		  (option.tagged-language.datums-as-operators? (option.tagged-language?))
		  (option.tagged-language.setter-forms?        (option.tagged-language?)))
      . ?body)))

(define-syntax-rule (with-option-strict-r6rs ?enabled? . ?body)
  ;;We want to  enable "strict R6RS" if  it is requested with the  OPTIONS library or
  ;;program clause, but not disable it if the option is not used.
  (parametrise ((option.strict-r6rs (or ?enabled? (option.strict-r6rs))))
    . ?body))


;;;; library records collectors

(define (make-collector)
  (let ((ls '()))
    (case-lambda
     (()
      ls)
     ((x)
      (unless (eq? x '*interaction*)
	(assert (library? x))
	;;Prepend  X to  the  list LS  if it  is  not already  contained
	;;according to EQ?.
	(set! ls (if (memq x ls)
		     ls
		   (cons x ls))))))))

(define imp-collector
  ;;Imported  libraries  collector.   Holds a  collector  function  (see
  ;;MAKE-COLLECTOR)  filled with  the LIBRARY  records representing  the
  ;;libraries from an R6RS import specification: every time the expander
  ;;parses an IMPORT syntax, the selected libraries are represented by a
  ;;LIBRARY record and such record is added to this collection.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'imp-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'imp-collector "BUG: not a procedure" x))
      x)))

(define inv-collector
  ;;Invoked  libraries  collector.   Holds  a  collector  function  (see
  ;;MAKE-COLLECTOR)  filled with  the LIBRARY  records representing  the
  ;;libraries defining "global" entries  in the lexical environment that
  ;;are used in the code we are expanding.
  ;;
  ;;The library:
  ;;
  ;;   (library (subdemo)
  ;;     (export sub-var)
  ;;     (import (vicare))
  ;;     (define sub-var 456))
  ;;
  ;;is imported by the library:
  ;;
  ;;   (library (demo)
  ;;     (export var sub-var)
  ;;     (import (vicare) (subdemo))
  ;;     (define var 123))
  ;;
  ;;which is imported by the program:
  ;;
  ;;   (import (vicare) (demo))
  ;;   (define (doit)
  ;;     (list var sub-var))
  ;;   (doit)
  ;;
  ;;when the  body of the function  is expanded the identifiers  VAR and
  ;;SUB-VAR are captured by bindings in the lexical environment with the
  ;;format:
  ;;
  ;;   (global . (?library . ?gensym))
  ;;
  ;;where  ?LIBRARY  is the  record  of  type LIBRARY  representing  the
  ;;library that  defines the variable  and ?GENSYM is a  symbol holding
  ;;the variable's value in its  "value" slot.  Such LIBRARY records are
  ;;added to the INV-COLLECTOR.
  ;;
  ;;For the  identifier VAR:  ?LIBRARY represents the  library "(demo)";
  ;;for  the   identifier  SUB-VAR:  ?LIBRARY  represents   the  library
  ;;"(subdemo)".  Notice  that while "(demo)"  is present in  the IMPORT
  ;;specification, and  so it is  also registered in  the IMP-COLLECTOR,
  ;;"(subdemo)" is not and it is only present in the INV-COLLECTOR.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'inv-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'inv-collector "BUG: not a procedure" x))
      x)))

(define vis-collector
  ;;Visit  libraries   collector.   Holds  a  collector   function  (see
  ;;MAKE-COLLECTOR)  which  is  meant  to be  filled  with  the  LIBRARY
  ;;records.   This  collector  holds  the libraries  collected  by  the
  ;;INV-COLLECTOR  when  expanding  the  right-hand  side  of  a  syntax
  ;;definition.
  ;;
  ;;The library:
  ;;
  ;;  (library (demo)
  ;;    (export var)
  ;;    (import (vicare))
  ;;    (define var 123))
  ;;
  ;;is loaded by the program:
  ;;
  ;;  (import (vicare)
  ;;    (for (demo) expand))
  ;;  (define-syntax doit (lambda (stx) var))
  ;;  (doit)
  ;;
  ;;the right-hand side of the syntax definition is the expression:
  ;;
  ;;  (lambda (stx) var)
  ;;
  ;;when such expression is expanded: the  identifier VAR is found to be
  ;;captured by a binding in the lexical environment with the format:
  ;;
  ;;   (global . (?library . ?gensym))
  ;;
  ;;where  ?LIBRARY  is the  record  of  type LIBRARY  representing  the
  ;;library "(demo)" and ?GENSYM is a  symbol holding 123 in its "value"
  ;;slot.   The record  ?LIBRARY is  added first  to INV-COLLECTOR  and,
  ;;after finishing the expansion of the right-hand side, it is moved to
  ;;the INV-COLLECTOR.  See %EXPAND-MACRO-TRANSFORMER for details.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'vis-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'vis-collector "BUG: not a procedure" x))
      x)))

;;; --------------------------------------------------------------------

(define stale-when-collector
  ;;Collects  test  expressions  from STALE-WHEN  syntaxes  and  LIBRARY
  ;;records needed for such expressions.  This parameter holds a special
  ;;collector  function  (see  %MAKE-STALE-COLLECTOR)  which  handles  2
  ;;collections:  one for  expanded expressions  representing STALE-WHEN
  ;;test  expressions, one  for  LIBRARY records  defining the  imported
  ;;variables needed by the test expressions.
  ;;
  ;;The library:
  ;;
  ;;   (library (subsubdemo)
  ;;     (export sub-sub-var)
  ;;     (import (vicare))
  ;;     (define sub-sub-var 456))
  ;;
  ;;is imported by the library:
  ;;
  ;;   (library (subdemo)
  ;;     (export sub-var sub-sub-var)
  ;;     (import (vicare) (subsubdemo))
  ;;     (define sub-var 456))
  ;;
  ;;which is imported by the library:
  ;;
  ;;   (library (demo)
  ;;     (export var)
  ;;     (import (vicare) (subdemo))
  ;;     (define var
  ;;       (stale-when (< sub-var sub-sub-var)
  ;;         123)))
  ;;
  ;;which is imported by the program:
  ;;
  ;;   (import (vicare) (demo))
  ;;   (debug-print var)
  ;;
  ;;when the test  expression of the STALE-WHEN syntax  is expanded, the
  ;;identifiers SUB-VAR and SUB-SUB-VAR are  captured by bindings in the
  ;;lexical environment with the format:
  ;;
  ;;   (global . (?library . ?gensym))
  ;;
  ;;where  ?LIBRARY  is the  record  of  type LIBRARY  representing  the
  ;;library that  defines the variable  and ?GENSYM is a  symbol holding
  ;;the variable's value in its  "value" slot.  Such LIBRARY records are
  ;;added first to  INV-COLLECTOR and, after finishing  the expansion of
  ;;the  STALE-WHEN test,  they are  moved to  the STALE-WHEN-COLLECTOR.
  ;;See HANDLE-STALE-WHEN for details.
  ;;
  ;;For  the   identifier  SUB-VAR:  ?LIBRARY  represents   the  library
  ;;"(subdemo)"; for the identifier SUB-SUB-VAR: ?LIBRARY represents the
  ;;library "(subsubdemo)".  Notice that while "(subdemo)" is present in
  ;;the  IMPORT specification,  and  so  it is  also  registered in  the
  ;;IMP-COLLECTOR, "(subsubdemo)" is  not and it is only  present in the
  ;;STALE-WHEN-COLLECTOR.
  ;;
  ;;The  collector  function  referenced  by this  parameter  returns  2
  ;;values, which are usually named GUARD-CODE and GUARD-LIB*:
  ;;
  ;;GUARD-CODE  is  a single  core  language  expression representing  a
  ;;composition of  all the STALE-WHEN  test expressions present  in the
  ;;body  of  a library.   If  at  least  one  of the  test  expressions
  ;;evaluates to true: the whole composite expression evaluates to true.
  ;;
  ;;GUARD-LIB* is a  list of LIBRARY records  representing the libraries
  ;;needed to evaluate the composite test expression.
  ;;
  (make-parameter #f))


;;;; public interface: tagged language support

(module (enable-tagged-language
	 disable-tagged-language)

  (define (enable-tagged-language)
    ;;This is meant to be used at the  REPL to turn on tagged language support, which
    ;;is off by default.
    ;;
    (tagged-language-support #t))

  (define (disable-tagged-language)
    ;;This is meant to be used at the REPL to turn off tagged language support.
    ;;
    (tagged-language-support #f))

  (define (tagged-language-support enable?)
    (option.tagged-language? enable?)
    (option.tagged-language.rhs-tag-propagation? (option.tagged-language?))
    (option.tagged-language.datums-as-operators? (option.tagged-language?))
    (option.tagged-language.setter-forms?        (option.tagged-language?)))

  #| end of module |# )


(case-define* eval
  ((x {env environment?})
   ;;This  is  R6RS's eval.   Take  an  expression  and  an environment:  expand  the
   ;;expression, invoke its invoke-required libraries  and evaluate its expanded core
   ;;form.  Return the result of the expansion.
   ;;
   (eval x env #f #f))
  ((x {env environment?} expander-options compiler-options)
   ;;This is the Vicare extension.
   ;;
   (receive (x invoke-req*)
       (parametrise
	   ;;Here we  want to override  the value  of the parameters  STRICT-R6RS and
	   ;;TAGGED-LANGUAGE?.
	   ((option.strict-r6rs       (and expander-options (enum-set-member? 'strict-r6rs expander-options)))
	    (option.tagged-language?  (and expander-options (enum-set-member? 'strict-r6rs expander-options))))
	 (expand-form-to-core-language x env))
     ;;Here we use the expander and compiler options from the libraries.
     (for-each invoke-library invoke-req*)
     (parametrise ((option.strict-r6rs (and compiler-options
					    (enum-set-member? 'strict-r6rs compiler-options))))
       (compiler.eval-core (expanded->core x))))))

(define (environment . import-spec*)
  ;;This  is  R6RS's  environment.   It  parses  the  import  specs  and
  ;;constructs  an env  record that  can be  used later  by eval  and/or
  ;;expand.
  ;;
  ;;IMPORT-SPEC*  must be  a list  of SYNTAX-MATCH  expression arguments
  ;;representing import  specifications as  defined by R6RS  plus Vicare
  ;;extensions.
  ;;
  (let ((itc (make-collector)))
    (parametrise ((imp-collector itc))
      ;;NAME-VEC is a vector of  symbols representing the external names
      ;;of  the  imported bindings.   LABEL-VEC  is  a vector  of  label
      ;;gensyms uniquely associated to the imported bindings.
      (receive (name-vec label-vec)
	  (begin
	    (import PARSE-IMPORT-SPEC)
	    (parse-import-spec* import-spec*))
	(make-env name-vec label-vec itc)))))


;;;; built-in environment objects

(define (null-environment n)
  ;;Defined  by R6RS.   The null  environment is  constructed using  the
  ;;corresponding library.
  ;;
  (unless (eqv? n 5)
    (assertion-violation 'null-environment
      "only report version 5 is supported" n))
  (environment '(psyntax null-environment-5)))

(define (scheme-report-environment n)
  ;;Defined  by R6RS.   The R5RS  environment is  constructed using  the
  ;;corresponding library.
  ;;
  (unless (eqv? n 5)
    (assertion-violation 'scheme-report-environment
      "only report version 5 is supported" n))
  (environment '(psyntax scheme-report-environment-5)))

(case-define new-interaction-environment
  ;;Build and return a new interaction environment.
  ;;
  (()
   (new-interaction-environment (base-of-interaction-library)))
  ((libref)
   (let* ((lib (find-library-by-reference libref))
	  (rib (export-subst->rib (library-export-subst lib))))
     (make-interaction-env rib '() '()))))

(define interaction-environment
  ;;When  called  with  no   arguments:  return  an  environment  object
  ;;representing  the environment  active at  the  REPL; to  be used  as
  ;;argument for EVAL.
  ;;
  ;;When  called with  the argument  ENV, which  must be  an environment
  ;;object: set ENV as interaction environment.
  ;;
  (let ((current-env #f))
    (case-lambda
     (()
      (or current-env
	  (begin
	    (set! current-env (new-interaction-environment))
	    current-env)))
     ((env)
      (unless (environment? env)
	(assertion-violation 'interaction-environment
	  "expected environment object as argument" env))
      (set! current-env env)))))


(module (expand-form-to-core-language)
  (define-syntax __module_who__
    (identifier-syntax 'expand-form-to-core-language))

  (define (expand-form-to-core-language expr env)
    ;;Interface to the internal expression expander (chi-expr).  Take an
    ;;expression and  an environment.  Return two  values: the resulting
    ;;core-expression, a list  of libraries that must  be invoked before
    ;;evaluating the core expr.
    ;;
    (cond ((env? env)
	   (let ((rib (make-top-rib (env-names env) (env-labels env))))
	     (let ((expr.stx (make-<stx> expr TOP-MARK* (list rib) '()))
		   (rtc      (make-collector))
		   (vtc      (make-collector))
		   (itc      (env-itc env)))
	       (let ((psi (parametrise ((top-level-context #f)
					(inv-collector rtc)
					(vis-collector vtc)
					(imp-collector itc))
			    (let ((lexenv.run     '())
				  (lexenv.expand  '()))
			      (chi-expr expr.stx lexenv.run lexenv.expand)))))
		 (seal-rib! rib)
		 (values (psi-core-expr psi) (rtc))))))

	  ((interaction-env? env)
	   (let ((rib         (interaction-env-rib env))
		 (lexenv.run  (interaction-env-lexenv env))
		 (rtc         (make-collector)))
	     (let ((expr.stx (make-<stx> expr TOP-MARK* (list rib) '())))
	       (receive (expr.core lexenv.run^)
		   (parametrise ((top-level-context env)
				 (inv-collector rtc)
				 (vis-collector (make-collector))
				 (imp-collector (make-collector)))
		     (%chi-interaction-expr expr.stx rib lexenv.run))
		 (set-interaction-env-lexenv! env lexenv.run^)
		 (values expr.core (rtc))))))

	  (else
	   (assertion-violation __module_who__ "not an environment" env))))

  (define (%chi-interaction-expr expr.stx rib lexenv.run)
    (receive (trailing-init-form*.stx
	      lexenv.run^ lexenv.expand^
	      lex* qrhs*
	      module-init-form**.stx
	      kwd*.unused internal-export*.unused)
	(let ((mixed-definitions-and-expressions? #t)
	      (shadowing-definitions?             #f))
	  (chi-body* (list expr.stx) lexenv.run lexenv.run
		     '() '() '() '() '() rib
		     mixed-definitions-and-expressions?
		     shadowing-definitions?))
      (let ((expr*.core (%expand-interaction-qrhs*/init*
			 (reverse lex*) (reverse qrhs*)
			 (append (reverse-and-append module-init-form**.stx)
				 trailing-init-form*.stx)
			 lexenv.run^ lexenv.expand^)))
	(let ((expr.core (cond ((null? expr*.core)
				(build-void))
			       ((null? (cdr expr*.core))
				(car expr*.core))
			       (else
				(build-sequence no-source expr*.core)))))
	  (values expr.core lexenv.run^)))))

  (define (%expand-interaction-qrhs*/init* lhs* qrhs* trailing-init* lexenv.run lexenv.expand)
    ;;Return a list of expressions in the core language.
    ;;
    (let recur ((lhs*  lhs*)
		(qrhs* qrhs*))
      (if (null? lhs*)
	  (map (lambda (init)
		 (psi-core-expr (chi-expr init lexenv.run lexenv.expand)))
	    trailing-init*)
	(let ((lhs  (car lhs*))
	      (qrhs (car qrhs*)))
	  (define-syntax-rule (%recurse-and-cons ?expr.core)
	    (cons ?expr.core
		  (recur (cdr lhs*) (cdr qrhs*))))
	  (case (car qrhs)
	    ((defun)
	     (let ((psi (chi-defun (cdr qrhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source
				    lhs (psi-core-expr psi)))))
	    ((expr)
	     (let ((psi (chi-expr  (cdr qrhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source
				    lhs (psi-core-expr psi)))))
	    ((untagged-define-expr)
	     (let ((psi (chi-expr  (cddr qrhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source
				    lhs (psi-core-expr psi)))))
	    ((top-expr)
	     (let ((psi (chi-expr  (cdr qrhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (psi-core-expr psi))))
	    (else
	     (assertion-violation __module_who__
	       "invalid qualified RHS while expanding expression" qrhs)))))))

  #| end of module: EXPAND-FORM-TO-CORE-LANGUAGE |# )


(module (expand-r6rs-top-level-make-evaluator)

  (define (expand-r6rs-top-level-make-evaluator program-form*)
    ;;Given a list of  SYNTAX-MATCH expression arguments representing an
    ;;R6RS top level  program, expand it and return a  thunk which, when
    ;;evaluated,  compiles the  program and  returns an  INTERACTION-ENV
    ;;struct representing the environment after the program execution.
    ;;
    (receive (lib* invoke-code macro* export-subst export-env option*)
	(expand-top-level program-form*)
      (lambda ()
	;;Make  sure  that the  code  of  all  the needed  libraries  is
	;;compiled   and  evaluated.    The  storage   location  gensyms
	;;associated to  the exported bindings are  initialised with the
	;;global values.
	(for-each invoke-library lib*)
	;;Store  the  expanded  code  representing  the  macros  in  the
	;;associated location gensyms.
	(initial-visit! macro*)
	;;Convert  the expanded  language  code to  core language  code,
	;;compile it and evaluate it.
	(compiler.eval-core (expanded->core invoke-code))
	(make-interaction-env (export-subst->rib export-subst)
			      (map %export-env-entry->lexenv-entry export-env)
			      '()))))

  (define (%export-env-entry->lexenv-entry export-env-entry)
    (let* ((label           (car export-env-entry))
	   (export-binding  (cdr export-env-entry))
	   (type-sym        (export-binding-type export-binding))
	   (loc             (export-binding-loc  export-binding)))
      ;;Here we know  that TYPE-SYM is one  among: GLOBAL, GLOBAL-MACRO,
      ;;GLOBAL-MACRO!, GLOBAL-CTV.
      (cons* label type-sym '*interaction* loc)))

  #| end of module: EXPAND-R6RS-TOP-LEVEL-MAKE-EVALUATOR |# )

(define (expand-r6rs-top-level-make-compiler expr*)
  ;;Given a  list of  SYNTAX-MATCH expression arguments  representing an
  ;;R6RS top level program, expand it and return a thunk to be evaluated
  ;;to obtain a closure representing the program.
  ;;
  (receive (lib* invoke-code macro* export-subst export-env)
      (expand-top-level expr*)
    (lambda ()
      ;;Make sure that the code of  all the needed libraries is compiled
      ;;and evaluated.   The storage location gensyms  associated to the
      ;;exported bindings are initialised with the global values.
      (for-each invoke-library lib*)
      ;;Store  the   expanded  code  representing  the   macros  in  the
      ;;associated location gensyms.
      (initial-visit! macro*)
      (values (map library-descriptor lib*)
	      ;;Convert the expanded language code to core language code.
	      (compiler.compile-core-expr (expanded->core invoke-code))))))


;;;; R6RS program expander

(module (expand-top-level)

  (define-syntax __module_who__
    (identifier-syntax 'expand-top-level))

  (define (expand-top-level program-form*)
    ;;Given  a list  of SYNTAX-MATCH  expression arguments  representing an  R6RS top
    ;;level program, expand it.
    ;;
    (receive (import-spec* option* body*)
	(%parse-top-level-program program-form*)
      (receive (import-spec* invoke-lib* visit-lib* invoke-code macro* export-subst export-env)
	  (let ((option* (%parse-program-options option*))
		(mixed-definitions-and-expressions? #t))
	    (import CORE-BODY-EXPANDER)
	    (core-body-expander 'all import-spec* option* body* mixed-definitions-and-expressions?
				%verbose-messages-thunk))
	(values invoke-lib* invoke-code macro* export-subst export-env option*))))

  (define (%verbose-messages-thunk)
    (when (option.tagged-language?)
      (print-expander-warning-message "enabling tagged language support for program"))
    (when (option.strict-r6rs)
      (print-expander-warning-message "enabling expander's strict R6RS support for program")))

  (define (%parse-top-level-program program-form*)
    ;;Given  a list  of SYNTAX-MATCH  expression arguments  representing an  R6RS top
    ;;level program, possibly with Vicare extensions, parse it and return 3 values:
    ;;
    ;;1. A list of import specifications.
    ;;
    ;;2. A list of options specifications.
    ;;
    ;;3. A list of body forms.
    ;;
    (syntax-match program-form* ()
      (((?import  ?import-spec* ...)
	(?options ?option-spec* ...)
	?body* ...)
       (and (eq? (syntax->datum ?import)  'import)
	    (eq? (syntax->datum ?options) 'options))
       (values ?import-spec* ?option-spec* ?body*))

      (((?import ?import-spec* ...) ?body* ...)
       (eq? (syntax->datum ?import) 'import)
       (values ?import-spec* '() ?body*))

      (((?import . x) . y)
       (eq? (syntax->datum ?import) 'import)
       (syntax-violation __module_who__
	 "invalid syntax of top-level program" (syntax-car program-form*)))

      (_
       (assertion-violation __module_who__
	 "top-level program is missing an (import ---) clause"))))

  (define (%parse-program-options option*)
    (syntax-match option* ()
      (() '())
      ((?opt . ?other*)
       (symbol? (syntax->datum ?opt))
       (let ((sym (syntax->datum ?opt)))
	 (case sym
	   ((tagged-language)
	    (cons sym (%parse-program-options ?other*)))
	   ((strict-r6rs)
	    (cons sym (%parse-program-options ?other*)))
	   (else
	    (syntax-violation __module_who__
	      "invalid program option" ?opt)))))
      ))

  #| end of module: EXPAND-TOP-LEVEL |# )

(define (expand-top-level->sexp sexp)
  (receive (invoke-lib* invoke-code macro* export-subst export-env option*)
      (expand-top-level sexp)
    `((invoke-lib*	. ,invoke-lib*)
      (invoke-code	. ,invoke-code)
      (macro*		. ,macro*)
      (export-subst	. ,export-subst)
      (export-env	. ,export-env)
      (option*		. ,option*))))


;;;; R6RS library expander

(module (expand-library)
  ;;EXPAND-LIBRARY is the default library  expander; it expands a symbolic expression
  ;;representing  a LIBRARY  form  to core-form;  it registers  it  with the  library
  ;;manager, in other words it interns it.
  ;;
  ;;The argument LIBRARY-SEXP must be the symbolic expression:
  ;;
  ;;   (library . _)
  ;;
  ;;or an ANNOTATION struct representing such expression.
  ;;
  ;;The optional FILENAME must be #f or a string representing the source
  ;;file from which  the library was loaded; it is  used for information
  ;;purposes.
  ;;
  ;;The optional argument VERIFY-LIBNAME must be a procedure accepting a
  ;;R6RS  library  name  as  argument;  it  is  meant  to  perform  some
  ;;validation upon the library name components (especially the version)
  ;;and raise  an exception if  something is wrong; otherwise  it should
  ;;just return.
  ;;
  ;;The returned values are:
  ;;
  ;;UID -
  ;;  A gensym uniquely identifying this library.
  ;;
  ;;LIBNAME -
  ;;  A R6RS library name.  For the library:
  ;;
  ;;     (library (ciao (1 2))
  ;;       (export A)
  ;;       (import (rnrs))
  ;;       (define A 123))
  ;;
  ;;  LIBNAME is the list (ciao (1 2)).
  ;;
  ;;IMPORT-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;  to be  imported for the invoke  code.  Each item in the  list is a
  ;;  "library descriptor" as built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;VISIT-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;  to  be imported for the  visit code.  Each  item in the list  is a
  ;;  "library descriptor" as built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;INVOKE-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;   to be  invoked  to  make available  the  values  of the  imported
  ;;  variables.   Each item in  the list  is a "library  descriptor" as
  ;;  built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;INVOKE-CODE -
  ;;  A  symbolic expression  representing the code  to be  evaluated to
  ;;  create  the top-level  DEFINE bindings  and evaluate  the trailing
  ;;  init expressions.
  ;;
  ;;VISIT-CODE -
  ;;  A  symbolic expression  representing the code  to be  evaluated to
  ;;  create the expand-time code.
  ;;
  ;;EXPORT-SUBST -
  ;;  A subst representing the bindings to export.
  ;;
  ;;EXPORT-ENV -
  ;;  A list representing the bindings exported by the library.
  ;;
  ;;GUARD-CODE -
  ;;   A predicate  expression  in the  core  language representing  the
  ;;  stale-when tests from the body of the library.
  ;;
  ;;GUARD-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;  to  be invoked for  the STALE-WHEN  code; these are  the libraries
  ;;  accumulated  by the  INV-COLLECTOR while expanding  the STALE-WHEN
  ;;  test expressions.  Each item in the list is a "library descriptor"
  ;;  as built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;OPTION* -
  ;;   A list  of  symbolic expressions  representing  options from  the
  ;;  OPTIONS clause of the LIBRARY form.
  ;;
  ;;For example, expanding the library:
  ;;
  ;;   (library (ciao)
  ;;     (export var fun mac ctv)
  ;;     (import (vicare))
  ;;     (define var 1)
  ;;     (define (fun)
  ;;       2)
  ;;     (define-syntax (mac stx)
  ;;       3)
  ;;     (define-syntax ctv
  ;;       (make-compile-time-value
  ;;        (+ 4 5))))
  ;;
  ;;yields the INVOKE-CODE:
  ;;
  ;;   (library-letrec*
  ;;       ((lex.var loc.lex.var '1)
  ;;        (lex.fun loc.lex.fun (annotated-case-lambda fun (() '2))))
  ;;     ((primitive void)))
  ;;
  ;;the VISIT-CODE:
  ;;
  ;;   (begin
  ;;     (set! loc.lab.mac
  ;;           (annotated-case-lambda
  ;;               (#'lambda (#'stx) #'3)
  ;;             ((lex.stx) '3)))
  ;;     (set! loc.lab.ctv
  ;;           (annotated-call
  ;;               (make-compile-time-value (+ 4 5))
  ;;             (primitive make-compile-time-value)
  ;;             (annotated-call (+ 4 5) (primitive +) '4 '5))))
  ;;
  ;;the EXPORT-SUBST:
  ;;
  ;;   ((ctv . lab.ctv)
  ;;    (mac . lab.mac)
  ;;    (fun . lab.fun)
  ;;    (var . lab.var))
  ;;
  ;;the EXPORT-ENV
  ;;
  ;;   ((lab.var global		. loc.lex.var)
  ;;    (lab.fun global		. loc.lex.fun)
  ;;    (lab.mac global-macro	. loc.lab.mac)
  ;;    (lab.ctv global-ctv	. loc.lab.ctv))
  ;;
  ;;Another example, for the library:
  ;;
  ;;   (library (ciao (1 2))
  ;;     (export doit)
  ;;     (import (vicare))
  ;;     (stale-when (< 1 2)
  ;;       (define a 123))
  ;;     (stale-when (< 2 3)
  ;;       (define b 123))
  ;;     (define (doit)
  ;;       123))
  ;;
  ;;the GUARD-CODE is:
  ;;
  ;;   (if (if '#f
  ;;           '#t
  ;;          (annotated-call (< 1 2) (primitive <) '1 '2))
  ;;       '#t
  ;;     (annotated-call (< 2 3) (primitive <) '2 '3))
  ;;
  (case-define expand-library
    ((library-sexp)
     (expand-library library-sexp #f       (lambda (libname) (void))))
    ((library-sexp filename)
     (expand-library library-sexp filename (lambda (libname) (void))))
    ((library-sexp filename verify-libname)
     (receive (libname
	       import-lib* invoke-lib* visit-lib*
	       invoke-code macro*
	       export-subst export-env
	       guard-code guard-lib*
	       option*)
	 (parametrise ((source-code-location (or filename (source-code-location))))
	   (let ()
	     (import CORE-LIBRARY-EXPANDER)
	     (core-library-expander library-sexp verify-libname)))
       (let ((uid		(gensym)) ;library unique-symbol identifier
	     (import-libdesc*	(map library-descriptor import-lib*))
	     (visit-libdesc*	(map library-descriptor visit-lib*))
	     (invoke-libdesc*	(map library-descriptor invoke-lib*))
	     (guard-libdesc*	(map library-descriptor guard-lib*))
	     ;;Thunk to eval to visit the library.
	     (visit-proc	(lambda ()
				  ;;This initial visit is performed whenever a source
				  ;;library is visited.
				  (initial-visit! macro*)))
	     ;;Thunk to eval to invoke the library.
	     (invoke-proc	(lambda ()
				  (compiler.eval-core (expanded->core invoke-code))))
	     ;;This visit code is compiled and  stored in FASL files; the resulting
	     ;;code objects are  the ones evaluated whenever a  compiled library is
	     ;;loaded and visited.
	     (visit-code	(%build-visit-code macro* option*))
	     (visible?		#t))
	 (intern-library uid libname
			 import-libdesc* visit-libdesc* invoke-libdesc*
			 export-subst export-env
			 visit-proc invoke-proc
			 visit-code invoke-code
			 guard-code guard-libdesc*
			 visible? filename option*)
	 (values uid libname
		 import-libdesc* visit-libdesc* invoke-libdesc*
		 invoke-code visit-code
		 export-subst export-env
		 guard-code guard-libdesc*
		 option*)))))

  (define (%build-visit-code macro* option*)
    ;;Return  a  sexp  representing  code  that initialises  the  bindings  of  macro
    ;;definitions in the  core language: the visit code; code  evaluated whenever the
    ;;library  is visited;  each  library is  visited  only once  the  first time  an
    ;;exported binding is used.
    ;;
    ;;MACRO* is a list of sublists.  The entries with format:
    ;;
    ;;   (?loc . (?obj . ?core-code))
    ;;
    ;;represent  macros  defined by  DEFINE-SYNTAX;  here  we  build code  to  assign
    ;;?CORE-CODE to ?LOC.  The entries with format:
    ;;
    ;;   (#f   . ?core-code)
    ;;
    ;;are  the result  of  expanding  BEGIN-FOR-SYNTAX macro  uses;  here we  include
    ;;?CORE-CODE as is in the output.
    ;;
    ;;The returned sexp looks like this (one SET! for every macro):
    ;;
    ;;  (begin
    ;;    (set! G3
    ;;      (annotated-case-lambda
    ;;	      (#<syntax expr=lambda mark*=(top)>
    ;;	       (#<syntax expr=stx mark*=(top)>)
    ;;         #<syntax expr=3 mark*=(top)>)
    ;;	      ((stx) '3)))
    ;;    (set! G5
    ;;      (annotated-call
    ;;	      (make-compile-time-value (+ 4 5))
    ;;	      (primitive make-compile-time-value)
    ;;	      (annotated-call (+ 4 5)
    ;;          (primitive +) '4 '5))))
    ;;
    (if (null? macro*)
	(build-void)
      (build-with-compilation-options option*
        (build-sequence no-source
	  (map (lambda (entry)
		 (let ((loc (car entry)))
		   (if loc
		       (let ((rhs.core (cddr entry)))
			 (build-global-assignment no-source
			   loc rhs.core))
		     (let ((expr.core (cdr entry)))
		       expr.core))))
	    macro*)))))

  #| end of module: EXPAND-LIBRARY |# )

(define (expand-library->sexp libsexp)
  (receive (uid libname
	    import-libdesc* visit-libdesc* invoke-libdesc*
	    invoke-code visit-code
	    export-subst export-env
	    guard-code guard-libdesc*
	    option*)
      (expand-library libsexp)
    `((uid		. ,uid)
      (libname		. ,libname)
      (import-libdesc*	. ,import-libdesc*)
      (visit-libdesc*	. ,visit-libdesc*)
      (invoke-libdesc*	. ,invoke-libdesc*)
      (invoke-code	. ,invoke-code)
      (visit-code	. ,visit-code)
      (export-subst	. ,export-subst)
      (export-env	. ,export-env)
      (guard-code	. ,guard-code)
      (guard-libdesc*	. ,guard-libdesc*)
      (option*		. ,option*))))


(module CORE-LIBRARY-EXPANDER
  (core-library-expander)
  (define-constant __who__ 'core-library-expander)

  (define (core-library-expander library-sexp verify-libname)
    ;;Given a  SYNTAX-MATCH expression  argument representing  a LIBRARY
    ;;form:
    ;;
    ;;   (library . _)
    ;;
    ;;parse  it  and return  multiple  values  representing the  library
    ;;contents.
    ;;
    ;;The optional argument VERIFY-LIBNAME must be a procedure accepting
    ;;a  R6RS library  name as  argument; it  is meant  to perform  some
    ;;validation  upon  the  library  name  components  (especially  the
    ;;version) and raise  an exception if something  is wrong; otherwise
    ;;it should just return.
    ;;
    (receive (libname export-spec* import-spec* body* libopt*)
	(%parse-library library-sexp)
      (%validate-library-name libname verify-libname)
      (let* ((libname.sexp  (syntax->datum libname))
	     (option*       (%parse-library-options libopt*))
	     (stale-clt     (%make-stale-collector)))
	(receive (import-lib* invoke-lib* visit-lib* invoke-code macro* export-subst export-env)
	    (parametrise ((stale-when-collector    stale-clt))
	      (let ((mixed-definitions-and-expressions? #f))
		(import CORE-BODY-EXPANDER)
		(core-body-expander export-spec* import-spec* option* body*
				    mixed-definitions-and-expressions?
				    (%make-verbose-messages-thunk libname.sexp))))
	  (receive (guard-code guard-lib*)
	      (stale-clt)
	    (values libname.sexp
		    import-lib* invoke-lib* visit-lib*
		    invoke-code macro* export-subst
		    export-env guard-code guard-lib*
		    option*))))))

  (define (%make-verbose-messages-thunk libname.sexp)
    (lambda ()
      (when (option.tagged-language?)
	(print-expander-warning-message "enabling tagged language support for library: ~a" libname.sexp))
      (when (option.strict-r6rs)
	(print-expander-warning-message "enabling expander's strict R6RS support for library: ~a" libname.sexp))))

  (define (%parse-library library-sexp)
    ;;Given an  ANNOTATION struct  representing a LIBRARY  form symbolic
    ;;expression, return 4 values:
    ;;
    ;;1..The name part.  A SYNTAX-MATCH expression argument representing
    ;;   parts of the library name.
    ;;
    ;;2..The   export  specs.    A   SYNTAX-MATCH  expression   argument
    ;;   representing the exports specification.
    ;;
    ;;3..The   import  specs.    A   SYNTAX-MATCH  expression   argument
    ;;   representing the imports specification.
    ;;
    ;;4..The body  of the  library.  A SYNTAX-MATCH  expression argument
    ;;   representing the body of the library.
    ;;
    ;;This function  performs no validation  of the returned  values, it
    ;;just validates the structure of the LIBRARY form.
    ;;
    (syntax-match library-sexp ()
      ((?library (?name* ...)
		 (?options ?libopt* ...)
		 (?export ?exp* ...)
		 (?import ?imp* ...)
		 ?body* ...)
       (and (eq? (syntax->datum ?library) 'library)
	    (eq? (syntax->datum ?options) 'options)
	    (eq? (syntax->datum ?export)  'export)
	    (eq? (syntax->datum ?import)  'import))
       (values ?name* ?exp* ?imp* ?body* ?libopt*))
      ((?library (?name* ...)
		 (?export ?exp* ...)
		 (?import ?imp* ...)
		 ?body* ...)
       (and (eq? (syntax->datum ?library) 'library)
	    (eq? (syntax->datum ?export)  'export)
	    (eq? (syntax->datum ?import)  'import))
       (values ?name* ?exp* ?imp* ?body* '()))
      (_
       (syntax-violation __who__ "malformed library" library-sexp))))

  (define (%validate-library-name libname verify-libname)
    ;;Given a SYNTAX-MATCH expression argument LIBNAME  which is meant to represent a
    ;;R6RS  library name:  validate  it.  If  successful  return unspecified  values;
    ;;otherwise raise an exception.
    ;;
    (receive (name* ver*)
	(let recur ((sexp libname))
	  (syntax-match sexp ()
	    (((?vers* ...))
	     (for-all library-version-number? (map syntax->datum ?vers*))
	     (values '() (map syntax->datum ?vers*)))

	    ((?id . ?rest)
	     (symbol? (syntax->datum ?id))
	     (receive (name* vers*)
		 (recur ?rest)
	       (values (cons (syntax->datum ?id) name*) vers*)))

	    (()
	     (values '() '()))

	    (_
	     (syntax-violation __who__ "invalid library name" libname))))
      (when (null? name*)
	(syntax-violation __who__ "empty library name" libname)))
    (verify-libname (syntax->datum libname))
    (void))

  (define (%parse-library-options libopt*)
    (syntax-match libopt* ()
      (() '())
      ((?opt . ?other*)
       (symbol? (syntax->datum ?opt))
       (let ((sym (syntax->datum ?opt)))
	 (case sym
	   ((visit-upon-loading)
	    (cons sym (%parse-library-options ?other*)))
	   ((tagged-language)
	    (cons sym (%parse-library-options ?other*)))
	   ((strict-r6rs)
	    (cons sym (%parse-library-options ?other*)))
	   (else
	    (syntax-violation __who__
	      "invalid library option" ?opt)))))
      ))

  (module (%make-stale-collector)
    ;;When a library has code like:
    ;;
    ;;   (stale-when (< 1 2) (define a 123))
    ;;   (stale-when (< 2 3) (define b 123))
    ;;
    ;;we build STALE-CODE as follows:
    ;;
    ;;   (if (if '#f
    ;;           '#t
    ;;         (annotated-call (< 1 2) (primitive <) '1 '2))
    ;;       '#t
    ;;     (annotated-call (< 2 3) (primitive <) '2 '3))
    ;;
    ;;The value GUARD-LIB* is the list of LIBRARY records accumulated by
    ;;the INV-COLLECTOR while expanding the STALE-WHEN test expressions.
    ;;
    (define (%make-stale-collector)
      (let ((accumulated-code           (build-data no-source #f))
	    (accumulated-requested-lib* '()))
	(case-lambda
	 (()
	  (values accumulated-code accumulated-requested-lib*))
	 ((new-test-code requested-lib*)
	  (set! accumulated-code
		(build-conditional no-source
		  accumulated-code	    ;test
		  (build-data no-source #t) ;consequent
		  new-test-code))	    ;alternate
	  (set! accumulated-requested-lib*
		(%set-union requested-lib* accumulated-requested-lib*))))))

    (define (%set-union ls1 ls2)
      ;;Build and return a new list holding elements from LS1 and LS2 with
      ;;duplicates removed.
      ;;
      (cond ((null? ls1)
	     ls2)
	    ((memq (car ls1) ls2)
	     (%set-union (cdr ls1) ls2))
	    (else
	     (cons (car ls1)
		   (%set-union (cdr ls1) ls2)))))

    #| end of module: %MAKE-STALE-COLLECTOR |# )

  #| end of module: CORE-LIBRARY-EXPANDER |# )


(module CORE-BODY-EXPANDER
  (core-body-expander)
  ;;Both the R6RS  programs expander and the  R6RS library expander make  use of this
  ;;module to expand the body forms.
  ;;
  ;;Let's take this library as example:
  ;;
  ;;   (library (demo)
  ;;     (export var1
  ;;             (rename (var2 the-var2))
  ;;             mac)
  ;;     (import (vicare))
  ;;     (define var1 1)
  ;;     (define var2 2)
  ;;     (define-syntax (mac stx) 3))
  ;;
  ;;When expanding the body of a library: the argument EXPORT-SPEC* is a SYNTAX-MATCH
  ;;input  argument  representing  a  set  of  library  export  specifications;  when
  ;;expanding the body of a program: EXPORT-SPEC* is the symbol "all".
  ;;
  ;;IMPORT-SPEC*  is a  SYNTAX-MATCH input  argument  representing a  set of  library
  ;;import specifications.
  ;;
  ;;BODY-SEXP* is a SYNTAX-MATCH input argument representing the body forms.
  ;;
  ;;MIXED-DEFINITIONS-AND-EXPRESSIONS?  is true  when expanding  a program  and false
  ;;when expanding a library; when  true mixing top-level definitions and expressions
  ;;is fine.
  ;;
  ;;Return multiple values:
  ;;
  ;;1..A  list of  LIBRARY records  representing  the collection  accumulated by  the
  ;;   IMP-COLLECTOR.   The records  represent the libraries  imported by  the IMPORT
  ;;   syntaxes.
  ;;
  ;;2..A  list of  LIBRARY records  representing  the collection  accumulated by  the
  ;;    INV-COLLECTOR.  The  records  represent the  libraries  exporting the  global
  ;;   variable bindings referenced in the run-time code.
  ;;
  ;;3..A  list of  LIBRARY records  representing  the collection  accumulated by  the
  ;;    VIS-COLLECTOR.  The  records  represent the  libraries  exporting the  global
  ;;   variable bindings referenced in the right-hand sides of syntax definitions.
  ;;
  ;;4..INVOKE-CODE  is a  core language  LIBRARY-LETREC* expression  representing the
  ;;    result  of expanding  the  input  source.  For  the  library  in the  example
  ;;   INVOKE-CODE is:
  ;;
  ;;      (library-letrec*
  ;;          ((lex.var1 loc.lex.var1 '1)
  ;;           (lex.var2 loc.lec.var2 '2))
  ;;        ((primitive void)))
  ;;
  ;;5..MACRO* is a list of bindings representing the macros defined in the code.  For
  ;;   the example library MACRO* is:
  ;;
  ;;      ((lab.mac #<procedure> .
  ;;         (annotated-case-lambda (#'lambda (#'stx) #'3)
  ;;           ((#'stx) '3)))
  ;;
  ;;6..EXPORT-SUBST is an alist with entries having the format:
  ;;
  ;;      (?name . ?label)
  ;;
  ;;    where: ?NAME  is  a symbol  representing  the external  name  of an  exported
  ;;    syntactic binding;  ?LABEL is  a gensym  uniquely identifying  such syntactic
  ;;   binding.  For the library in the example, EXPORT-SUBST is:
  ;;
  ;;      ((mac      . lab.mac)
  ;;       (the-var2 . lab.var2)
  ;;       (var1     . lab.var1))
  ;;
  ;;7..EXPORT-ENV is  the lexical  environment of bindings  exported by  the library.
  ;;   Its format  is different from the  one of the LEXENV.*  values used throughout
  ;;   the expansion process.  For the library in the example, EXPORT-ENV is:
  ;;
  ;;      ((lab.var1 global       . loc.lex.var1)
  ;;       (lab.var2 global       . loc.lex.var2)
  ;;       (lab.mac  global-macro . loc.lab.mac))
  ;;
  (define (core-body-expander export-spec* import-spec* option* body-sexp* mixed-definitions-and-expressions?
			      verbose-messages-thunk)
    (define itc (make-collector))
    (parametrise ((imp-collector      itc)
		  (top-level-context  #f))
      (define rib
	(%process-import-specs-build-top-level-rib import-spec*))
      (define (wrap x)
	(make-<stx> x TOP-MARK* (list rib) '()))
      (with-tagged-language (memq 'tagged-language option*)
	(with-option-strict-r6rs (memq 'strict-r6rs option*)
	  (verbose-messages-thunk)
	  (let ((body-stx*	(map wrap body-sexp*))
		(rtc	(make-collector))
		(vtc	(make-collector)))
	    (parametrise ((inv-collector  rtc)
			  (vis-collector  vtc))
	      ;;INIT*.STX  is a  list  of syntax  objects  representing the  trailing
	      ;;non-definition forms from the body of the library and the body of the
	      ;;internal modules.
	      ;;
	      ;;LEX* is  a list of left-hand-side  lex gensyms to be  used in binding
	      ;;definitions when building core  language symbolic expressions for the
	      ;;glocal DEFINE forms in the library.   There is a lex gensym for every
	      ;;item in QRHS*.
	      ;;
	      ;;QRHS*  is  a list  of  qualified  right-hand sides  representing  the
	      ;;right-hand side expressions in the DEFINE  forms from the body of the
	      ;;library.
	      ;;
	      ;;INTERNAL-EXPORT* is  a list of identifiers  exported through internal
	      ;;EXPORT syntaxes rather  than the export spec at the  beginning of the
	      ;;library.
	      ;;
	      (receive (init*.stx lexenv.run lexenv.expand lex* qrhs* internal-export*)
		  (%process-internal-body body-stx* rib mixed-definitions-and-expressions?)
		(receive (export-name* export-id*)
		    (%parse-all-export-specs export-spec* internal-export* wrap rib)
		  (seal-rib! rib)
		  ;;RHS*.PSI  is  a list  of  PSI  structs containing  core  language
		  ;;symbolic expressions representing the DEFINE right-hand sides.
		  ;;
		  ;;INIT*.PSI  is a  list  of PSI  structs  containing core  language
		  ;;symbolic expressions representing the trailing init forms.
		  ;;
		  ;;We want order here?  Yes.   We expand first the definitions, then
		  ;;the init forms; so that tag  identifiers have been put where they
		  ;;are needed.
		  (let* ((rhs*.psi  (chi-qrhs* qrhs*     lexenv.run lexenv.expand))
			 (init*.psi (chi-expr* init*.stx lexenv.run lexenv.expand)))
		    ;;QUESTION Why do we unseal the rib  if we do not use it anymore?
		    ;;Is it  an additional check  of its internal  integrity?  (Marco
		    ;;Maggi; Sun Mar 23, 2014)
		    (unseal-rib! rib)
		    (let ((loc*          (map gensym-for-storage-location lex*))
			  (export-subst  (%make-export-subst export-name* export-id*)))
		      (receive (export-env macro*)
			  (%make-export-env/macro* lex* loc* lexenv.run)
			(%validate-exports export-spec* export-subst export-env)
			(let ((invoke-code (build-with-compilation-options option*
					     (build-library-letrec* no-source
					       mixed-definitions-and-expressions?
					       lex* loc* (map psi-core-expr rhs*.psi)
					       (if (null? init*.psi)
						   (build-void)
						 (build-sequence no-source
						   (map psi-core-expr init*.psi)))))))
			  (values (itc) (rtc) (vtc)
				  invoke-code macro* export-subst export-env)))))))))))))

  (define-syntax-rule (%expanding-program? ?export-spec*)
    (eq? 'all ?export-spec*))

  (define (%process-import-specs-build-top-level-rib import-spec*)
    ;;Parse the import  specifications from a library's IMPORT clause  or a program's
    ;;standalone  IMPORT  syntax;  build  and return  the  top-level  "rib"  struct
    ;;defining the top-level environment.
    ;;
    (import PARSE-IMPORT-SPEC)
    ;;NAME-VEC is a vector of symbols representing the external names of the imported
    ;;bindings.  LABEL-VEC  is a vector of  label gensyms uniquely associated  to the
    ;;imported bindings.
    (receive (name-vec label-vec)
	(parse-import-spec* import-spec*)
      (make-top-rib name-vec label-vec)))

  (define (%process-internal-body body-stx* rib mixed-definitions-and-expressions?)
    ;;Perform  the preliminary  expansion of  the top-level  forms in  the body;  the
    ;;right-hand  sides of  DEFINE syntaxes  are *not*  expanded here;  the body  and
    ;;module trailing init forms are *not* expanded here.
    ;;
    (receive (trailing-init-form*.stx
	      lexenv.run lexenv.expand
	      lex* qrhs*
	      module-init-form**.stx unused-kwd* internal-export*)
	(let ((shadowing-definitions? #t))
	  (chi-body* body-stx* '() '() '() '() '() '() '() rib
		     mixed-definitions-and-expressions?
		     shadowing-definitions?))
      ;;We build a list  of init form putting first the trailing  init forms from the
      ;;internal   MODULE  syntaxes,   then  the   trailing  init   forms  from   the
      ;;library/program body.
      (let ((init-form*.stx (append (reverse-and-append module-init-form**.stx)
				    trailing-init-form*.stx)))
	(values init-form*.stx
		lexenv.run lexenv.expand
		;;This is  a list of gensyms  to be used in  binding definitions when
		;;building core language symbolic expressions for the DEFINE forms in
		;;the library.  There is a gensym for every item in QRHS*.
		(reverse lex*)
		;;This  is a  list  of qualified  right-hand  sides representing  the
		;;right-hand side  expressions in the  DEFINE forms from the  body of
		;;the library.
		(reverse qrhs*)
		;;This   is   a  list   of   identifiers   representing  the   export
		;;specifications declared using the EXPORT syntax in the body.
		internal-export*))))

  (define (%parse-all-export-specs export-spec* internal-export* wrap top-level-rib)
    ;;Parse all the export specifications.
    ;;
    ;;EXPORT-SPEC*  must   be  a   list  of   identifiers  representing   the  export
    ;;specifications declared in the EXPORT clause of a LIBRARY form.
    ;;
    ;;INTERNAL-EXPORT*  must  be  a  list  of  identifiers  representing  the  export
    ;;specifications declared using the EXPORT syntax in the body.
    ;;
    (import PARSE-EXPORT-SPEC)
    (parse-export-spec* (if (%expanding-program? export-spec*)
			    (map wrap (top-marked-symbols top-level-rib))
			  (append (map wrap export-spec*)
				  internal-export*))))

  (define (%make-export-subst export-name* export-id*)
    ;;For  every  identifier in  ID:  get  the rib  of  ID  and extract  the  lexical
    ;;environment from it; search the environment  for a binding associated to ID and
    ;;acquire its label (a gensym).  Return an alist with entries having the format:
    ;;
    ;;   (?export-name . ?label)
    ;;
    ;;where ?EXPORT-NAME  is a symbol representing  the external name of  an exported
    ;;binding, ?LABEL is the corresponding gensym uniquely identifying the binding.
    ;;
    (map (lambda (export-name export-id)
	   (let ((label (id->label export-id)))
	     (if label
		 (cons export-name label)
	       (stx-error export-id "cannot export unbound identifier"))))
      export-name* export-id*))

  (define (%make-export-env/macro* lex* loc* lexenv.run)
    ;;For each entry in LEXENV.RUN: convert  the LEXENV entry to an EXPORT-ENV entry,
    ;;accumulating EXPORT-ENV;  if the syntactic  binding is a macro  or compile-time
    ;;value: accumulate the MACRO* alist.
    ;;
    ;;Notice that  EXPORT-ENV contains  an entry for  every global  lexical variable,
    ;;both the exported ones and the  non-exported ones.  It is responsibility of the
    ;;EXPORT-SUBST to select the entries representing the exported bindings.
    ;;
    ;;LEX*  must be  a  list of  gensyms representing  the  global lexical  variables
    ;;binding names.
    ;;
    ;;LOC*  must  be a  list  of  storage location  gensyms  for  the global  lexical
    ;;variables: there must be a loc in LOC* for every lex in LEX*.
    ;;
    (let loop ((lexenv.run	lexenv.run)
	       (export-env	'())
	       (macro*		'()))
      (if (null? lexenv.run)
	  (values export-env macro*)
	(let* ((entry    (car lexenv.run))
	       (label    (lexenv-entry.label entry))
	       (binding  (lexenv-entry.binding-descriptor entry)))
	  (case (syntactic-binding-type binding)
	    ((lexical)
	     ;;This binding is a lexical variable.   When we import a lexical binding
	     ;;from another library, we must see such entry as "global".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (lexical . (?lexvar . ?mutable)))
	     ;;
	     ;;Add to the EXPORT-ENV an entry like:
	     ;;
	     ;;   (?label . (?type . ?lex/loc))
	     ;;
	     ;;where: ?TYPE is the symbol  "mutable" or the symbol "global"; ?lex/loc
	     ;;is the loc gensym of the variable, used as lex gensym and loc gensym.
	     ;;
	     ;;NOTE The entries of type "mutable"  are forbidden to be exported.  The
	     ;;error will be raised later.
	     ;;
	     (let* ((bind-val  (syntactic-binding-value binding))
		    ;;Search for LEXICAL-GENSYM  in the list LEX*:  when found return
		    ;;the corresponding gensym from  LOC*.  LEXICAL-GENSYM must be an
		    ;;item in LEX*.
		    (loc       (let lookup ((lexical-gensym  (lexical-var bind-val))
					    (lex*            lex*)
					    (loc*            loc*))
				 (if (pair? lex*)
				     (if (eq? lexical-gensym (car lex*))
					 (car loc*)
				       (lookup lexical-gensym (cdr lex*) (cdr loc*)))
				   (assertion-violation 'make-export-env/lookup "internal error"))))
		    (type      (if (lexical-var-mutated? bind-val)
				   'mutable
				 'global)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label type loc) export-env)
		     macro*)))

	    ((local-macro)
	     ;;When we define  a binding for a non-identifier syntax:  the local code
	     ;;sees it  as "local-macro".   If we export  such binding:  the importer
	     ;;must see it as a "global-macro".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (local-macro . (?transformer . ?expanded-expr)))
	     ;;
	     ;;Add to the EXPORT-ENV an entry like:
	     ;;
	     ;;   (?label global-macro . ?loc)
	     ;;
	     ;;and to the MACRO* an entry like:
	     ;;
	     ;;   (?loc . (?transformer . ?expanded-expr))
	     ;;
	     (let ((loc (gensym-for-storage-location label)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label 'global-macro loc) export-env)
		     (cons (cons loc (syntactic-binding-value binding)) macro*))))

	    ((local-macro!)
	     ;;When we define a binding for an identifier syntax: the local code sees
	     ;;it as  "local-macro!".  If we  export such binding: the  importer must
	     ;;see it as a "global-macro!".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (local-macro! . (?transformer . ?expanded-expr)))
	     ;;
	     ;;Add to the EXPORT-ENV an entry like:
	     ;;
	     ;;   (?label global-macro . ?loc)
	     ;;
	     ;;and to the MACRO* an entry like:
	     ;;
	     ;;   (?loc . (?transformer . ?expanded-expr))
	     ;;
	     (let ((loc (gensym-for-storage-location label)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label 'global-macro! loc) export-env)
		     (cons (cons loc (syntactic-binding-value binding)) macro*))))

	    ((local-ctv)
	     ;;When we  define a binding  for a  compile-time value (CTV):  the local
	     ;;code sees it as "local-ctv".  If  we export such binding: the importer
	     ;;must see it as a "global-ctv".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (local-ctv . (?object . ?expanded-expr)))
	     ;;
	     ;;Add to the EXPORT-ENV an entry like:
	     ;;
	     ;;   (?label . (global-ctv . ?loc))
	     ;;
	     ;;and to the MACRO* an entry like:
	     ;;
	     ;;   (?loc . (?object . ?expanded-expr))
	     ;;
	     (let ((loc (gensym-for-storage-location label)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label 'global-ctv loc) export-env)
		     (cons (cons loc (syntactic-binding-value binding)) macro*))))

	    (($rtd $module $fluid $synonym)
	     ;;Just  add the  entry  "as  is" from  the  lexical  environment to  the
	     ;;EXPORT-ENV.
	     ;;
	     (loop (cdr lexenv.run)
		   (cons entry export-env)
		   macro*))

	    ((begin-for-syntax)
	     ;;This entry is  the result of expanding BEGIN-FOR-SYNTAX  macro use; we
	     ;;want this code to be part of the visit code.
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (begin-for-syntax . ?expanded-expr))
	     ;;
	     ;;add to the MACRO* an entry like:
	     ;;
	     ;;   (#f . ?expanded-expr)
	     ;;
	     (loop (cdr lexenv.run)
		   export-env
		   (cons (cons #f (syntactic-binding-value binding)) macro*)))

	    (else
	     (assertion-violation 'core-body-expander
	       "internal error: do not know how to export"
	       (syntactic-binding-type  binding)
	       (syntactic-binding-value binding))))))))

  (define (%validate-exports export-spec* export-subst export-env)
    ;;We want to forbid code like the following:
    ;;
    ;;    (library (proof)
    ;;      (export that doit)
    ;;      (import (vicare))
    ;;      (define that 123)
    ;;      (define (doit a)
    ;;	      (set! that a)))
    ;;
    ;;in which the mutable variable THAT is exported.
    ;;
    (define export-subst-entry-name  car)
    (define export-subst-entry-label cdr)
    (unless (%expanding-program? export-spec*)
      (for-each (lambda (subst)
		  (cond ((assq (export-subst-entry-label subst) export-env)
			 => (lambda (entry)
			      (when (eq? 'mutable (syntactic-binding-type (lexenv-entry.binding-descriptor entry)))
				(syntax-violation 'export
				  "attempt to export mutated variable"
				  (export-subst-entry-name subst)))))))
	export-subst)))

  #| end of module: CORE-BODY-EXPANDER |# )


(module PARSE-EXPORT-SPEC
  (parse-export-spec*)
  ;;Given a  list of SYNTAX-MATCH expression  arguments representing the
  ;;exports specification from a LIBRARY form, return 2 values:
  ;;
  ;;1. A list of symbols representing the external names of the exported
  ;;   bindings.
  ;;
  ;;2.  A  list of  identifiers  (syntax  objects  holding a  symbol  as
  ;;    expression)  representing the  internal  names  of the  exported
  ;;   bindings.
  ;;
  ;;This function checks that none  of the identifiers is BOUND-ID=?  to
  ;;another: the library does not export the same external *name* twice.
  ;;It is instead possible to  export the same identifier multiple times
  ;;if we give it different external names.
  ;;
  ;;According to R6RS, an export specification has the following syntax:
  ;;
  ;;   (export ?export-spec ...)
  ;;
  ;;   ?export-spec
  ;;     == ?identifier
  ;;     == (rename (?internal-identifier ?external-identifier) ...)
  ;;
  ;;Vicare adds the following:
  ;;
  ;;     == (prefix   (?internal-identifier ...) the-prefix)
  ;;     == (deprefix (?internal-identifier ...) the-prefix)
  ;;     == (suffix   (?internal-identifier ...) the-suffix)
  ;;     == (desuffix (?internal-identifier ...) the-suffix)
  ;;
  (define-constant __who__ 'export)

  (define (parse-export-spec* export-spec*)
    (case-define %synner
      ((message)
       (syntax-violation __who__ message export-spec*))
      ((message subform)
       (syntax-violation __who__ message export-spec* subform)))
    (let loop ((export-spec*          export-spec*)
	       (internal-identifier*  '())
	       (external-identifier*  '()))
      (if (null? export-spec*)
	  (if (valid-bound-ids? external-identifier*)
	      (values (map syntax->datum external-identifier*)
		      internal-identifier*)
	    (%synner "invalid exports" (%find-dups external-identifier*)))
	(syntax-match (car export-spec*) ()
	  (?identifier
	   (identifier? ?identifier)
	   (loop (cdr export-spec*)
		 (cons ?identifier internal-identifier*)
		 (cons ?identifier external-identifier*)))

	  ((?rename (?internal* ?external*) ...)
	   (and (eq? (syntax->datum ?rename) 'rename)
		(for-all identifier? ?internal*)
		(for-all identifier? ?external*))
	   (loop (cdr export-spec*)
		 (append ?internal* internal-identifier*)
		 (append ?external* external-identifier*)))

	  ((?prefix (?internal* ...) ?the-prefix)
	   (and (eq? (syntax->datum ?prefix) 'prefix)
		(for-all identifier? ?internal*)
		(identifier? ?the-prefix))
	   (if (option.strict-r6rs)
	       (%synner "prefix export specification forbidden in strict R6RS mode")
	     (let* ((prefix.str (symbol->string (syntax->datum ?the-prefix)))
		    (external*  (map (lambda (id)
				       (datum->syntax
					id (string->symbol
					    (string-append
					     prefix.str
					     (symbol->string (syntax->datum id))))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  ((?deprefix (?internal* ...) ?the-prefix)
	   (and (eq? (syntax->datum ?deprefix) 'deprefix)
		(for-all identifier? ?internal*)
		(identifier? ?the-prefix))
	   (if (option.strict-r6rs)
	       (%synner "deprefix export specification forbidden in strict R6RS mode")
	     (let* ((prefix.str (symbol->string (syntax->datum ?the-prefix)))
		    (prefix.len (string-length prefix.str))
		    (external*  (map (lambda (id)
				       (let* ((id.str  (symbol->string (syntax->datum id)))
					      (id.len  (string-length id.str)))
					 (if (and (< prefix.len id.len)
						  (string=? prefix.str
							    (substring id.str 0 prefix.len)))
					     (datum->syntax
					      id (string->symbol
						  (substring id.str prefix.len id.len)))
					   (%synner
					    (string-append "binding name \"" id.str
							   "\" cannot be deprefixed of \""
							   prefix.str "\"")))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  ((?suffix (?internal* ...) ?the-suffix)
	   (and (eq? (syntax->datum ?suffix) 'suffix)
		(for-all identifier? ?internal*)
		(identifier? ?the-suffix))
	   (if (option.strict-r6rs)
	       (%synner "suffix export specification forbidden in strict R6RS mode")
	     (let* ((suffix.str (symbol->string (syntax->datum ?the-suffix)))
		    (external*  (map (lambda (id)
				       (datum->syntax
					id (string->symbol
					    (string-append
					     (symbol->string (syntax->datum id))
					     suffix.str))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  ((?desuffix (?internal* ...) ?the-suffix)
	   (and (eq? (syntax->datum ?desuffix) 'desuffix)
		(for-all identifier? ?internal*)
		(identifier? ?the-suffix))
	   (if (option.strict-r6rs)
	       (%synner "desuffix export specification forbidden in strict R6RS mode")
	     (let* ((suffix.str (symbol->string (syntax->datum ?the-suffix)))
		    (suffix.len (string-length suffix.str))
		    (external*  (map (lambda (id)
				       (define id.str
					 (symbol->string (syntax->datum id)))
				       (define id.len
					 (string-length id.str))
				       (define prefix.len
					 (fx- id.len suffix.len))
				       (if (and (< suffix.len id.len)
						(string=? suffix.str
							  (substring id.str prefix.len id.len)))
					   (datum->syntax
					    id (string->symbol
						(substring id.str 0 prefix.len)))
					 (%synner
					  (string-append "binding name \"" id.str
							 "\" cannot be desuffixed of \""
							 suffix.str "\""))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  (_
	   (%synner "invalid export specification" (car export-spec*)))))))

  (module (%find-dups)

    (define-inline (%find-dups ls)
      (let loop ((ls    ls)
		 (dups  '()))
	(cond ((null? ls)
	       dups)
	      ((%find-bound=? (car ls) (cdr ls) (cdr ls))
	       => (lambda (x)
		    (loop (cdr ls)
			  (cons (list (car ls) x)
				dups))))
	      (else
	       (loop (cdr ls) dups)))))

    (define (%find-bound=? x lhs* rhs*)
      (cond ((null? lhs*)
	     #f)
	    ((bound-id=? x (car lhs*))
	     (car rhs*))
	    (else
	     (%find-bound=? x (cdr lhs*) (cdr rhs*)))))

    #| end of module: %FIND-DUPS |# )

  #| end of module: PARSE-EXPORT-SPEC* |# )


(module PARSE-IMPORT-SPEC
  (parse-import-spec*)
  ;;Given  a  list  of SYNTAX-MATCH  expression  arguments  representing
  ;;import specifications from  a LIBRARY form, as defined  by R6RS plus
  ;;Vicare extensions (which can simply be  the raw sexp argument to the
  ;;ENVIRONMENT function):
  ;;
  ;;1. Parse and validate the import specs.
  ;;
  ;;2. For libraries not yet loaded: load the selected library files and
  ;;    add  them  to  the  current  collector  function  referenced  by
  ;;   IMP-COLLECTOR.
  ;;
  ;;3.   Apply to  library-exported  binding  names the  transformations
  ;;   described by the import spec, obtaining the external names.
  ;;
  ;;4. Check for name conflicts between imported bindings.
  ;;
  ;;Return 2  values which can  be used to build  a new top  level RIB
  ;;record:
  ;;
  ;;1. NAME-VEC, a vector of  symbols representing the external names of
  ;;   the  imported bindings.
  ;;
  ;;2. LABEL-VEC is a vector of label gensyms uniquely associated to the
  ;;   imported bindings.
  ;;
  ;;
  ;;A  quick  summary  of  R6RS syntax  definitions  along  with  Vicare
  ;;extensions:
  ;;
  ;;  (import ?import-spec ...)
  ;;
  ;;  ?import-spec
  ;;     == ?import-set
  ;;     == (for ?import-set ?import-level)
  ;;
  ;;  ?import-set
  ;;     == ?library-reference
  ;;     == (library ?library-reference)
  ;;     == (only ?import-set ?identifier ...)
  ;;     == (except ?import-set ?identifier)
  ;;     == (rename ?import-set (?identifier1 ?identifier2) ...)
  ;;
  ;;  ?library-reference
  ;;     == (?identifier0 ?identifier ...)
  ;;     == (?identifier0 ?identifier ... ?version-reference)
  ;;
  ;;  ?version-reference
  ;;     == (?sub-version-reference ...)
  ;;     == (and ?version-reference ...)
  ;;     == (or  ?version-reference ...)
  ;;     == (not ?version-reference)
  ;;
  ;;  ?sub-version-reference
  ;;     == ?sub-version
  ;;     == (>=  ?sub-version)
  ;;     == (<=  ?sub-version)
  ;;     == (and ?sub-version-reference ...)
  ;;     == (or  ?sub-version-reference ...)
  ;;     == (not ?sub-version-reference)
  ;;
  ;;  ?sub-version
  ;;     == #<non-negative fixnum>
  ;;
  ;;Vicare extends ?IMPORT-SET with:
  ;;
  ;;     == (prefix ?import-set ?identifier)
  ;;     == (deprefix ?import-set ?identifier)
  ;;     == (suffix ?import-set ?identifier)
  ;;     == (desuffix ?import-set ?identifier)
  ;;
  ;;Example, given:
  ;;
  ;;  ((rename (only (foo)
  ;;                 x z)
  ;;           (x y))
  ;;   (only (bar)
  ;;         q))
  ;;
  ;;this function returns the names and labels:
  ;;
  ;;   #(z y q)		#(lab.z lab.x lab.q)
  ;;
  ;;Externally   visible   imported   bindings  are   selected   by   an
  ;;EXPORT-SUBST: an alist  whose keys are the external  symbol names of
  ;;the bindings and whose values are the associated label gensyms.
  ;;
  (define-constant __who__ 'import)

  (module (parse-import-spec*)

    (define (parse-import-spec* import-spec*)
      (let loop ((import-spec*  import-spec*)
		 (export-table  (make-eq-hashtable)))
	;;EXPORT-TABLE has  EXPORT-SUBST names as keys  and EXPORT-SUBST
	;;labels as  values.  It  is used to  check for  duplicate names
	;;with different labels, which is an error.  Example:
	;;
	;;   (import (rename (french)
	;;                   (salut	ciao))	;ERROR!
	;;           (rename (british)
	;;                   (hello	ciao)))	;ERROR!
	;;
	(if (pair? import-spec*)
	    (begin
	      (for-each (lambda (name.label)
			  (%add-subst-entry! export-table name.label))
		(%import-spec->export-subst ($car import-spec*)))
	      (loop ($cdr import-spec*) export-table))
	  (hashtable-entries export-table))))

    (define-inline (%add-subst-entry! export-table name.label)
      ;;Add  the   given  NAME.LABEL   entry  to   EXPORT-TABLE;  return
      ;;unspecified values.  Raise a  syntax violation if NAME.LABEL has
      ;;the same name of an entry in EXPORT-TABLE, but different label.
      ;;
      (let ((name  ($car name.label))
	    (label ($cdr name.label)))
	(cond ((hashtable-ref export-table name #f)
	       => (lambda (already-existent-label)
		    (unless (eq? already-existent-label label)
		      (%error-two-import-with-different-bindings name))))
	      (else
	       (hashtable-set! export-table name label)))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (%import-spec->export-subst)

    (define-inline (%import-spec->export-subst import-spec)
      ;;Process the IMPORT-SPEC and return the corresponding subst.
      ;;
      ;;The IMPORT-SPEC is  parsed; the specified library is loaded  and interned, if
      ;;not  already in  the  library  collection; the  raw  subst  from the  library
      ;;definition is processed according to the rules in IMPORT-SPEC.
      ;;
      ;;If an  error is found, including  library version non-conforming
      ;;to the library reference, an exception is raised.
      ;;
      (syntax-match import-spec ()
	((?for ?import-set . ?import-levels)
	 ;;FIXME Here  we should validate  ?IMPORT-LEVELS even if  it is
	 ;;not used by Vicare.  (Marco Maggi; Tue Apr 23, 2013)
	 (eq? (syntax->datum ?for) 'for)
	 (%import-set->export-subst ?import-set import-spec))

	(?import-set
	 (%import-set->export-subst ?import-set import-spec))))

    (define (%import-set->export-subst import-set import-spec)
      ;;Recursive  function.   Process  the IMPORT-SET  and  return  the
      ;;corresponding  EXPORT-SUBST.   IMPORT-SPEC  is the  full  import
      ;;specification from the IMPORT clause: it is used for descriptive
      ;;error reporting.
      ;;
      (define (%recurse import-set)
	(%import-set->export-subst import-set import-spec))
      (define (%local-synner message)
	(%synner message import-spec import-set))
      (syntax-match import-set ()
	((?spec ?spec* ...)
	 ;;According to R6RS, the symbol LIBRARY  can be used to quote a
	 ;;library reference whose first  identifier is "for", "rename",
	 ;;etc.
	 (not (memq (syntax->datum ?spec)
		    '(rename except only prefix deprefix suffix desuffix library)))
	 (%import-library (cons ?spec ?spec*)))

	((?rename ?import-set (?old-name* ?new-name*) ...)
	 (and (eq? (syntax->datum ?rename) 'rename)
	      (for-all symbol-syntax? ?old-name*)
	      (for-all symbol-syntax? ?new-name*))
	 (let ((subst       (%recurse ?import-set))
	       (?old-name*  (map syntax->datum ?old-name*))
	       (?new-name*  (map syntax->datum ?new-name*)))
	   ;;FIXME Rewrite this  to eliminate find* and  rem* and merge.
	   ;;(Abdulaziz Ghuloum)
	   (let ((old-label* (find* ?old-name* subst ?import-set)))
	     (let ((subst (rem* ?old-name* subst)))
	       ;;FIXME Make sure map is valid. (Abdulaziz Ghuloum)
	       (%merge-export-subst* (map cons ?new-name* old-label*) subst)))))

	((?except ?import-set ?sym* ...)
	 (and (eq? (syntax->datum ?except) 'except)
	      (for-all symbol-syntax? ?sym*))
	 (let ((subst (%recurse ?import-set)))
	   (rem* (map syntax->datum ?sym*) subst)))

	((?only ?import-set ?name* ...)
	 (and (eq? (syntax->datum ?only) 'only)
	      (for-all symbol-syntax? ?name*))
	 (let* ((subst  (%recurse ?import-set))
		(name*  (map syntax->datum ?name*))
		(name*  (remove-dups name*))
		(lab*   (find* name* subst ?import-set)))
	   (map cons name* lab*)))

	((?prefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?prefix) 'prefix)
	      (symbol-syntax? ?prefix))
	 (let ((subst   (%recurse ?import-set))
	       (prefix  (symbol->string (syntax->datum ?the-prefix))))
	   (map (lambda (x)
		  (cons (string->symbol
			 (string-append prefix (symbol->string (car x))))
			(cdr x)))
	     subst)))

	((?deprefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?deprefix) 'deprefix)
	      (symbol-syntax? ?the-prefix))
	 (if (option.strict-r6rs)
	     (%local-synner "deprefix import specification forbidden in strict R6RS mode")
	   (let* ((subst       (%recurse ?import-set))
		  (prefix.str  (symbol->string (syntax->datum ?the-prefix)))
		  (prefix.len  (string-length prefix.str)))
	     ;;This should never happen.
	     (when (zero? prefix.len)
	       (%local-synner "null deprefix prefix"))
	     (map (lambda (subst.entry)
		    (let* ((orig.str  (symbol->string (car subst.entry)))
			   (orig.len  (string-length orig.str)))
		      (if (and (< prefix.len orig.len)
			       (string=? prefix.str (substring orig.str 0 prefix.len)))
			  (cons (string->symbol (substring orig.str prefix.len orig.len))
				(cdr subst.entry))
			(%local-synner
			 (string-append "binding name \"" orig.str
					"\" cannot be deprefixed of \"" prefix.str "\"")))))
	       subst))))

	((?suffix ?import-set ?the-suffix)
	 (and (eq? (syntax->datum ?suffix) 'suffix)
	      (symbol-syntax? ?suffix))
	 (if (option.strict-r6rs)
	     (%local-synner "suffix import specification forbidden in strict R6RS mode")
	   (let ((subst   (%recurse ?import-set))
		 (suffix  (symbol->string (syntax->datum ?the-suffix))))
	     (map (lambda (x)
		    (cons (string->symbol
			   (string-append (symbol->string (car x)) suffix))
			  (cdr x)))
	       subst))))

	((?desuffix ?import-set ?the-suffix)
	 (and (eq? (syntax->datum ?desuffix) 'desuffix)
	      (symbol-syntax? ?the-suffix))
	 (if (option.strict-r6rs)
	     (%local-synner "desuffix import specification forbidden in strict R6RS mode")
	   (let* ((subst       (%recurse ?import-set))
		  (suffix.str  (symbol->string (syntax->datum ?the-suffix)))
		  (suffix.len  (string-length suffix.str)))
	     ;;This should never happen.
	     (when (zero? suffix.len)
	       (%local-synner "null desuffix suffix"))
	     (map (lambda (subst.entry)
		    (let* ((orig.str    (symbol->string (car subst.entry)))
			   (orig.len    (string-length orig.str))
			   (prefix.len  (fx- orig.len suffix.len)))
		      (if (and (< suffix.len orig.len)
			       (string=? suffix.str
					 (substring orig.str prefix.len orig.len)))
			  (cons (string->symbol (substring orig.str 0 prefix.len))
				(cdr subst.entry))
			(%local-synner
			 (string-append "binding name \"" orig.str
					"\" cannot be desuffixed of \"" suffix.str "\"")))))
	       subst))))

	;;According to R6RS:  the symbol LIBRARY can be used  to quote a
	;;library reference  whose first identifier is  "for", "rename",
	;;etc.
	((?library (?spec* ...))
	 (eq? (syntax->datum ?library) 'library)
	 (%import-library ?spec*))

	(_
	 (%synner "invalid import set" import-spec import-set))))

    (define (%import-library libref)
      (receive (name version-conforms-to-reference?)
	  (%parse-library-reference libref)
	(when (null? name)
	  (%synner "empty library name" libref))
	;;Search  for  the  library  first  in the  collection  of  already  interned
	;;libraires, then on  the file system.  If successful: LIB  is an instance of
	;;LIBRARY struct.
	(let ((lib (find-library-by-reference (syntax->datum libref))))
	  (unless (version-conforms-to-reference? (library-name->version (library-name lib)))
	    (%synner "library does not satisfy version specification" libref lib))
	  ((imp-collector) lib)
	  (library-export-subst lib))))

    #| end of module: %IMPORT-SPEC->EXPORT-SUBST |# )

;;; --------------------------------------------------------------------

  (module (%parse-library-reference)

    (define (%parse-library-reference libref)
      ;;Given a  SYNTAX-MATCH expression argument LIBREF  representing a
      ;;library reference  as defined  by R6RS:  parse and  validate it.
      ;;Return 2 values:
      ;;
      ;;1. A list of symbols representing the library spec identifiers.
      ;;
      ;;2. A predicate function to be used to check if a library version
      ;;   conforms with the requirements of this library specification.
      ;;
      (let recur ((spec libref))
	(syntax-match spec ()

	  (((?version-spec* ...))
	   (values '() (%build-version-pred ?version-spec* libref)))

	  ((?id . ?rest*)
	   (symbol-syntax? ?id)
	   (receive (name pred)
	       (recur ?rest*)
	     (values (cons (syntax->datum ?id) name)
		     pred)))

	  (()
	   (values '() (lambda (x) #t)))

	  (_
	   (%synner "invalid library specification in import set" libref spec)))))

    (define (%build-version-pred version-reference libref)
      ;;Recursive function.  Given a  version reference: validate it and
      ;;build and return a predicate function that can be used to verify
      ;;if library versions do conform.
      ;;
      ;;LIBREF must be  the enclosing library reference, it  is used for
      ;;descriptive error reporting.
      ;;
      (define (%recurse X)
	(%build-version-pred X libref))
      (syntax-match version-reference ()
	(()
	 (lambda (x) #t))

	((?and ?version* ...)
	 (eq? (syntax->datum ?and) 'and)
	 (let ((predicate* (map %recurse ?version*)))
	   (lambda (x)
	     (for-all (lambda (pred)
			(pred x))
	       predicate*))))

	((?or ?version* ...)
	 (eq? (syntax->datum ?or) 'or)
	 (let ((predicate* (map %recurse ?version*)))
	   (lambda (x)
	     (exists (lambda (pred)
		       (pred x))
	       predicate*))))

	((?not ?version)
	 (eq? (syntax->datum ?not) 'not)
	 (let ((pred (%recurse ?version)))
	   (lambda (x)
	     (not (pred x)))))

	((?subversion* ...)
	 (let ((predicate* (map (lambda (subversion)
				  (%build-subversion-pred subversion libref))
			     ?subversion*)))
	   (lambda (x)
	     (let loop ((predicate* predicate*)
			(x          x))
	       (cond ((null? predicate*)
		      #t)
		     ((null? x)
		      #f)
		     (else
		      (and ((car predicate*) (car x))
			   (loop (cdr predicate*) (cdr x)))))))))

	(_
	 (%synner "invalid version reference" libref version-reference))))

    (define (%build-subversion-pred subversion* libref)
      ;;Recursive function.   Given a subversion reference:  validate it
      ;;and build  and return a predicate  function that can be  used to
      ;;verify if library versions do conform.
      ;;
      ;;LIBREF must be  the enclosing library reference, it  is used for
      ;;descriptive error reporting.
      ;;
      (define (%recurse X)
	(%build-subversion-pred X libref))
      (syntax-match subversion* ()
	(?subversion-number
	 (%subversion? ?subversion-number)
	 (let ((N (syntax->datum ?subversion-number)))
	   (lambda (x)
	     (= x N))))

	((?and ?subversion* ...)
	 (eq? (syntax->datum ?and) 'and)
	 (let ((predicate* (map %recurse ?subversion*)))
	   (lambda (x)
	     (for-all (lambda (pred)
			(pred x))
	       predicate*))))

	((?or ?subversion* ...)
	 (eq? (syntax->datum ?or) 'or)
	 (let ((predicate* (map %recurse ?subversion*)))
	   (lambda (x)
	     (exists (lambda (pred)
		       (pred x))
	       predicate*))))

	((?not ?subversion)
	 (eq? (syntax->datum ?not) 'not)
	 (let ((pred (%recurse ?subversion)))
	   (lambda (x)
	     (not (pred x)))))

        ((?<= ?subversion-number)
	 (and (eq? (syntax->datum ?<=) '<=)
	      (%subversion? ?subversion-number))
	 (let ((N (syntax->datum ?subversion-number)))
	   (lambda (x)
	     (<= x N))))

	((?>= ?subversion-number)
	 (and (eq? (syntax->datum ?>=) '>=)
	      (%subversion? ?subversion-number))
	 (let ((N (syntax->datum ?subversion-number)))
	   (lambda (x)
	     (>= x N))))

	(_
	 (%synner "invalid sub-version specification in library reference"
		  libref subversion*))))

    (define-inline (%subversion? stx)
      (library-version-number? (syntax->datum stx)))

    #| end of module: %PARSE-LIBRARY-REFERENCE |# )

;;; --------------------------------------------------------------------

  (module (%merge-export-subst*)

    (define (%merge-export-subst* subst1 subst2)
      ;;Recursive function.  Given two substs: merge them and return the
      ;;result.
      ;;
      ;;Assume that SUBST1  has unique entries in itself  and SUBST2 has
      ;;unique entrie in  itself.  If an entry from SUBST1  has the name
      ;;name but different label from an entry in SUBST2: raise a syntax
      ;;error.
      ;;
      (if (pair? subst1)
	  (%insert-to-subst ($car subst1)
			    (%merge-export-subst* ($cdr subst1) subst2))
	subst2))

    (define-inline (%insert-to-subst entry subst)
      ;;Given a subst  ENTRY and a SUBST: insert the  entry in the subst
      ;;if it is not already present  and return the result; else return
      ;;SUBST.
      ;;
      (let ((name  ($car entry))
	    (label ($cdr entry)))
	(cond ((assq name subst)
	       ;;An entry for NAME already exists.
	       => (lambda (x)
		    (if (eq? (cdr x) label)
			;;Same name and same label: OK.
			subst
		      ;;Same name but different label: ERROR.
		      (%error-two-import-with-different-bindings name))))
	      (else
	       ;;Prepend the new entry.
	       (cons entry subst)))))

    #| end of module: %MERGE-EXPORT-SUBST* |# )

;;; --------------------------------------------------------------------

  (define (find* sym* subst import-spec-stx)
    ;;Find all the entries in SUBST  having as name the symbols in SYM*;
    ;;return the  list of labels  from the  selected entries.  It  is an
    ;;error if a name in SYM* is not present in the SUBST.
    ;;
    ;;IMPORT-SPEC-STX must  be a  syntax object representing  the import
    ;;spec in which  we search for the SYM*; it  is used for descriptive
    ;;error reporting.
    ;;
    ;;This function is the one that raises  an error if we try to import
    ;;an unexistent binding, as in:
    ;;
    ;;   (import (only (vicare) this-does-not-exist))
    ;;
    (map (lambda (sym)
	   (cond ((assq sym subst)
		  => cdr)
		 (else
		  (%synner "cannot find identifier in export list of import spec"
			   import-spec-stx sym))))
      sym*))

  (define (rem* sym* subst)
    ;;Remove  from SUBST  all the  entries having  name in  the list  of
    ;;symbols SYM*.  Return the new  subst with the entries removed.  It
    ;;is fine if some names in SYM* are not present in SUBST.
    ;;
    (let recur ((subst subst))
      (cond ((null? subst)
	     '())
	    ((memq (caar subst) sym*)
	     (recur (cdr subst)))
	    (else
	     (cons (car subst) (recur (cdr subst)))))))

  (define (remove-dups ls)
    ;;Recursive  function.  Remove  duplicate  items from  the list  LS.
    ;;Compare items with EQ?.
    ;;
    (cond ((null? ls)
	   '())
	  ((memq (car ls) (cdr ls))
	   (remove-dups (cdr ls)))
	  (else
	   (cons (car ls) (remove-dups (cdr ls))))))

;;; --------------------------------------------------------------------

  (define (symbol-syntax? obj)
    (symbol? (syntax->datum obj)))

  (define (%error-two-import-with-different-bindings name)
    (%synner "two imports with different bindings" name))

  (case-define %synner
    ((message form)
     (syntax-violation __who__ message form))
    ((message form subform)
     (syntax-violation __who__ message form subform)))

  #| end of module: PARSE-IMPORT-SPEC* |# )


;;;; EXPORT-ENV helpers

(define-syntax-rule (make-export-env-entry ?label ?type ?loc)
  ;;Given a  label gensym, a  symbol representing an EXPORT-ENV  type, a
  ;;loc gensym: build and return an entry of EXPORT-ENV.
  ;;
  (cons* ?label ?type ?loc))

(define-syntax-rule (export-binding-type ?export-binding)
  ;;Given an export binding return the symbol representin its type.
  ;;
  (car ?export-binding))

(define-syntax-rule (export-binding-loc ?export-binding)
  ;;Given an export  binding return the loc gensym holding  its value.  Remember that
  ;;not all the EXPORT-ENV entries have a loc gensym.
  ;;
  (cdr ?export-binding))


;;;; identifiers: syntax parameters

(define current-run-lexenv
  ;;This parameter holds  a function which is meant to  return the value
  ;;of LEXENV.RUN while a macro is being expanded.
  ;;
  ;;The  default  value will  return  null,  which represents  an  empty
  ;;LEXENV; when  such value is used  with LABEL->SYNTACTIC-BINDING: the
  ;;mapping   label/binding  is   performed   only   in  the   top-level
  ;;environment.
  ;;
  ;;Another possibility we could think of is to use as default value the
  ;;function:
  ;;
  ;;   (lambda ()
  ;;     (syntax-violation 'current-run-lexenv
  ;; 	   "called outside the extent of a macro expansion"
  ;; 	   '(current-run-lexenv)))
  ;;
  ;;However there are  cases where we actually want  the returned LEXENV
  ;;to be null, for example: when evaluating the visit code of a library
  ;;just loaded in  FASL form; such visit code might  need, for example,
  ;;to access the  syntactic binding property lists, and it  would do it
  ;;outside any macro expansion.
  ;;
  (make-parameter
      (lambda () '())
    (lambda* ({obj procedure?})
      obj)))

(define (current-inferior-lexenv)
  ((current-run-lexenv)))

(define* (syntax-parameter-value {id identifier?})
  (let ((label (id->label id)))
    (if label
	(let ((binding (label->syntactic-binding label ((current-run-lexenv)))))
	  (case (syntactic-binding-type binding)
	    ((local-ctv)
	     (local-compile-time-value-binding-object binding))

	    ((global-ctv)
	     (global-compile-time-value-binding-object binding))

	    (else
	     (procedure-argument-violation __who__
	       "expected identifier bound to compile-time value"
	       id))))
      (procedure-argument-violation __who__
	"unbound identifier" id))))


;;;; macro transformer modules

(module SPLICE-FIRST-ENVELOPE
  (make-splice-first-envelope
   splice-first-envelope?
   splice-first-envelope-form)

  (define-record splice-first-envelope
    (form))

  #| end of module |# )

(module NON-CORE-MACRO-TRANSFORMER
  (non-core-macro-transformer)
  (include "psyntax.expander.non-core-macro-transformers.scm" #t))

(module CORE-MACRO-TRANSFORMER
  (core-macro-transformer)
  (include "psyntax.expander.core-macro-transformers.scm" #t))


;;;; macro transformers helpers

(define (%expand-macro-transformer rhs-expr.stx lexenv.expand)
  ;;Given  a  syntax object  representing  the  right-hand  side  (RHS) of  a  syntax
  ;;definition   (DEFINE-SYNTAX,   LET-SYNTAX,  LETREC-SYNTAX,   DEFINE-FLUID-SYNTAX,
  ;;FLUID-LET-SYNTAX): expand it,  invoking libraries as needed, and  return the core
  ;;language  sexp representing  the expression.   Usually the  return value  of this
  ;;function is handed to %EVAL-MACRO-TRANSFORMER.
  ;;
  ;;For:
  ;;
  ;;   (define-syntax ?lhs ?rhs)
  ;;
  ;;this function is called as:
  ;;
  ;;   (%expand-macro-transformer #'?rhs lexenv.expand)
  ;;
  ;;For:
  ;;
  ;;   (let-syntax ((?lhs ?rhs)) ?body0 ?body ...)
  ;;
  ;;this function is called as:
  ;;
  ;;   (%expand-macro-transformer #'?rhs lexenv.expand)
  ;;
  (let* ((rtc (make-collector))
	 (rhs-expr.psi (parametrise ((inv-collector rtc)
				     (vis-collector (lambda (x) (void))))
			 (chi-expr rhs-expr.stx lexenv.expand lexenv.expand))))
    ;;We invoke all the libraries needed to evaluate the right-hand side.
    (for-each
	(let ((register-visited-library (vis-collector)))
	  (lambda (lib)
	    ;;LIB is a  record of type "library".  Here we  invoke the library, which
	    ;;means  we evaluate  its run-time  code.  Then  we mark  the library  as
	    ;;visited.
	    (invoke-library lib)
	    (register-visited-library lib)))
      (rtc))
    (psi-core-expr rhs-expr.psi)))

(define (%eval-macro-transformer rhs-expr.core lexenv.run)
  ;;Given a  core language sexp  representing the right-hand  side (RHS) of  a syntax
  ;;definition   (DEFINE-SYNTAX,   LET-SYNTAX,  LETREC-SYNTAX,   DEFINE-FLUID-SYNTAX,
  ;;FLUID-LET-SYNTAX):  evaluate it  and return  a proper  syntactic binding  for the
  ;;resulting  object.  Usually  this  function is  applied to  the  return value  of
  ;;%EXPAND-MACRO-TRANSFORMER.
  ;;
  ;;When the RHS of  a syntax definition is evaluated, the  returned object should be
  ;;either:  a  syntax transformer  procedure;  an  identifier-syntax transformer;  a
  ;;Vicare struct type  descriptor or an R6RS record type  descriptor; a compile-time
  ;;value; a synonim transformer.  If the return  value is not of such type: we raise
  ;;an assertion violation.
  ;;
  (let ((rv (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	      (compiler.eval-core (expanded->core rhs-expr.core)))))
    (cond ((procedure? rv)
	   (make-local-macro-binding rv rhs-expr.core))
	  ((variable-transformer? rv)
	   (make-local-identifier-macro-binding (variable-transformer-procedure rv) rhs-expr.core))
	  ((struct-or-record-type-descriptor-binding? rv)
	   rv)
	  ((compile-time-value? rv)
	   (make-local-compile-time-value-binding (compile-time-value-object rv) rhs-expr.core))
	  ((synonym-transformer? rv)
	   (let ((id (synonym-transformer-identifier rv)))
	     (make-synonym-syntax-binding (id->label/or-error 'expander id id))))
	  (else
	   (raise
	    (condition
	     (make-assertion-violation)
	     (make-who-condition 'expand)
	     (make-message-condition "invalid return value from syntax definition right-hand side")
	     (make-syntax-definition-expanded-rhs-condition rhs-expr.core)
	     (make-syntax-definition-expression-return-value-condition rv)))))))


;;;; formals syntax validation

(define (%error-invalid-formals-syntax input-form-stx formals-stx)
  ;;Raise an error  for invalid formals of LAMBDA,  CASE-LAMBDA, LET and
  ;;similar.
  ;;
  ;;If no  invalid formals  are found:  return unspecified  values, else
  ;;raise a syntax violation.  This function  is called when it has been
  ;;already determined that the formals have something wrong.
  ;;
  ;;For a LAMBDA syntax:
  ;;
  ;;   (lambda ?formals . ?body)
  ;;
  ;;it is called as:
  ;;
  ;;   (%error-invalid-formals-syntax
  ;;      #'(lambda ?formals . ?body)
  ;;      #'?formals)
  ;;
  ;;For a LET syntax:
  ;;
  ;;   (let ((?lhs* ?rhs*) ...) . ?body)
  ;;
  ;;it is called as:
  ;;
  ;;   (%error-invalid-formals-syntax
  ;;      #'(let ((?lhs* ?rhs*) ...) . ?body)
  ;;      #'?lhs*)
  ;;
  ;;NOTE  Invalid LET-VALUES  and LET*-VALUES  formals are  processed by
  ;;this function  indirectly; LET-VALUES  and LET*-VALUES  syntaxes are
  ;;first  transformed into  CALL-WITH-VALUES syntaxes,  then it  is the
  ;;LAMBDA syntax that takes care of formals validation.
  ;;
  (define (%synner message subform)
    (syntax-violation #f message input-form-stx subform))
  (syntax-match formals-stx ()
    ((?id* ... . ?last)
     (let recur ((?id* (cond ((identifier? ?last)
			      (cons ?last ?id*))
			     ((syntax-null? ?last)
			      ?id*)
			     (else
			      (%synner "not an identifier" ?last)))))
       (cond ((null? ?id*)
	      (void))
	     ((not (identifier? (car ?id*)))
	      (%synner "not an identifier" (car ?id*)))
	     (else
	      (recur (cdr ?id*))
	      (when (bound-id-member? (car ?id*)
				      (cdr ?id*))
		(%synner "duplicate binding" (car ?id*)))))))

    (_
     (%synner "malformed binding form" formals-stx))))


;;;; chi module
;;
;;The  "chi-*"  functions  are  the ones  visiting  syntax  objects  and
;;performing the expansion process.
;;
(module (make-psi
	 psi?			psi-stx
	 psi-core-expr		psi-retvals-signature
	 psi-application-retvals-signature
	 chi-expr		chi-expr*
	 chi-body*		chi-internal-body
	 chi-qrhs*		chi-defun
	 chi-lambda		chi-case-lambda
	 chi-application/psi-first-operand)
  (include "psyntax.expander.chi-module.scm" #t))


;;;; R6RS programs and libraries helpers

(define (initial-visit! macro*)
  ;;Whenever a source  library is loaded and expanded: all  its macro definitions and
  ;;BEGIN-FOR-SYNTAX macro uses are expanded and evaluated.   All it is left to do to
  ;;visit such  library is to  store in  the loc gensyms  of macros the  compiled RHS
  ;;code; this is done by this function.
  ;;
  ;;MACRO* is a list of sublists.  The entries with format:
  ;;
  ;;   (?loc . (?obj . ?core-code))
  ;;
  ;;represent macros defined by DEFINE-SYNTAX; here we store ?OBJ in the "value" slot
  ;;of ?LOC.  The entries with format:
  ;;
  ;;   (#f   . ?core-code)
  ;;
  ;;are the result of expanding BEGIN-FOR-SYNTAX macro uses; here we ignore these.
  ;;
  (for-each (lambda (x)
	      (let ((loc  (car  x))
		    (proc (cadr x)))
		(when loc
		  (set-symbol-value! loc proc))))
    macro*))


;;;; miscellaneous

(case-define parse-logic-predicate-syntax
  ;;Given a  syntax object STX parse  it as logic predicate  expression with expected
  ;;format:
  ;;
  ;;   STX = (and ?expr0 ?expr ...)
  ;;       | (or  ?expr0 ?expr ...)
  ;;       | (xor ?expr0 ?expr ...)
  ;;       | (not ?expr)
  ;;       | ?expr
  ;;
  ;;where  AND,  OR,  XOR, NOT  are  the  identifiers  exported  by (vicare).   If  a
  ;;standalone ?EXPR is found: apply the  procedure TAIL-PROC to it gather its single
  ;;return value; TAIL-PROC defaults to the identity function.
  ;;
  ;;Return  a syntax  object representing  the  logic predicate  with the  standalone
  ;;expressions replaced by the return values of TAIL-PROC.
  ;;
  ((stx)
   (parse-logic-predicate-syntax stx (lambda (stx) stx)))
  ((stx tail-proc)
   (define (recurse expr)
     (parse-logic-predicate-syntax expr tail-proc))
   (syntax-match stx (and or xor not)
     ((and ?expr0 ?expr* ...)
      (bless
       `(and ,@(map recurse (cons ?expr0 ?expr*)))))
     ((or  ?expr0 ?expr* ...)
      (bless
       `(or  ,@(map recurse (cons ?expr0 ?expr*)))))
     ((xor ?expr0 ?expr* ...)
      (bless
       `(xor ,@(map recurse (cons ?expr0 ?expr*)))))
     ((not ?expr)
      (bless
       `(not ,(recurse ?expr))))
     (else
      (tail-proc stx)))))


;;;; done

;; #!vicare
;; (import (only (vicare) foreign-call))
;; (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.expander before"))

;;Register the expander with the library manager.
(current-library-expander expand-library)

;; (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.expander after"))
;; (void)

#| end of library |# )

;;; end of file
;;Local Variables:
;;fill-column: 85
;;eval: (put 'build-with-compilation-options	'scheme-indent-function 1)
;;eval: (put 'build-library-letrec*		'scheme-indent-function 1)
;;eval: (put 'build-application			'scheme-indent-function 1)
;;eval: (put 'build-conditional			'scheme-indent-function 1)
;;eval: (put 'build-case-lambda			'scheme-indent-function 1)
;;eval: (put 'build-lambda			'scheme-indent-function 1)
;;eval: (put 'build-foreign-call		'scheme-indent-function 1)
;;eval: (put 'build-sequence			'scheme-indent-function 1)
;;eval: (put 'build-global-assignment		'scheme-indent-function 1)
;;eval: (put 'build-lexical-assignment		'scheme-indent-function 1)
;;eval: (put 'build-letrec*			'scheme-indent-function 1)
;;eval: (put 'build-data			'scheme-indent-function 1)
;;eval: (put 'if-wants-descriptive-gensyms	'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'set-interaction-env-lab.loc/lex*!	'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;eval: (put 'with-tagged-language		'scheme-indent-function 1)
;;eval: (put 'with-option-strict-r6rs		'scheme-indent-function 1)
;;End:
