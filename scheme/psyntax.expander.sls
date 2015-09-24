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

    enable-typed-language		disable-typed-language

    ;; inspection of non-interaction environment objects
    environment-symbols			environment-libraries
    environment-labels			environment-binding

    expand-form-to-core-language
    expand-top-level-program	expand-top-level-program->sexp
    expand-library			expand-library->sexp
    expand-top-level-make-compiler

    ;; exception raisers
    syntax-violation			assertion-error
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Sat Apr 12,
    ;;2014)
    syntax-error

    ;;SYNTAX-CASE subroutines
    syntax-dispatch			ellipsis-map

    typed-procedure-variable.unsafe-variant
    typed-procedure-variable.unsafe-variant-set!

    ;; expand-time type checking exception stuff
    expand-time-type-signature-violation?
    expand-time-retvals-signature-violation?
    expand-time-retvals-signature-violation-expected-signature
    expand-time-retvals-signature-violation-returned-signature)
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer)
    (prefix (rnrs syntax-case) sys.)
    (rnrs mutable-pairs)
    (except (psyntax.compat)
	    expand-library)
    (prefix (only (psyntax.config)
		  initialise-expander)
	    config.)
    (psyntax.setup)
    (psyntax.builders)
    (psyntax.special-transformers)
    (psyntax.lexical-environment)
    (psyntax.syntax-match)
    (psyntax.syntactic-binding-properties)
    (psyntax.syntax-utilities)
    (only (psyntax.import-spec-parser) parse-import-spec*)
    (only (psyntax.export-spec-parser) parse-export-spec*)
    (psyntax.type-identifiers-and-signatures)
    (only (psyntax.core-primitives-properties)
	  initialise-core-prims-tagging)
    (psyntax.import-spec-parser)
    (psyntax.export-spec-parser)
    (psyntax.library-utils)
    (psyntax.library-collectors)
    (psyntax.chi-procedures)
    (prefix (psyntax.library-manager) libman.)
    (psyntax.internal))


;;; helpers

;;FIXME To  be removed at the  next boot image  rotation.  (Marco Maggi; Sat  Apr 12,
;;2014)
(define syntax-error)

(include "psyntax.helpers.scm" #t)

(define-syntax-rule (with-typed-language ?enabled? . ?body)
  (parametrise ((option.typed-language? (or ?enabled? (option.typed-language?))))
    (parametrise ((option.typed-language.rhs-tag-propagation? (option.typed-language?)))
      . ?body)))

(define-syntax-rule (with-option-strict-r6rs ?enabled? . ?body)
  ;;We want to  enable "strict R6RS" if  it is requested with the  OPTIONS library or
  ;;program clause, but not disable it if the option is not used.
  (parametrise ((option.strict-r6rs (or ?enabled? (option.strict-r6rs))))
    . ?body))


;;;; public interface: tagged language support

(module (enable-typed-language
	 disable-typed-language)

  (define (enable-typed-language)
    ;;This is meant to be used at the  REPL to turn on tagged language support, which
    ;;is off by default.
    ;;
    (typed-language-support #t))

  (define (disable-typed-language)
    ;;This is meant to be used at the REPL to turn off tagged language support.
    ;;
    (typed-language-support #f))

  (define (typed-language-support enable?)
    (option.typed-language? enable?)
    (option.typed-language.rhs-tag-propagation? (option.typed-language?)))

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
	   ;;TYPED-LANGUAGE?.
	   ((option.strict-r6rs      (and expander-options (enum-set-member? 'strict-r6rs    expander-options)))
	    (option.typed-language?  (and expander-options (enum-set-member? 'typed-language expander-options))))
	 (expand-form-to-core-language x env))
     ;;Here we use the expander and compiler options from the libraries.
     (for-each libman.invoke-library invoke-req*)
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
  (config.initialise-expander)
  (let ((itc (make-collector)))
    (parametrise ((imp-collector itc))
      ;;NAME-VEC is a vector of  symbols representing the external names
      ;;of  the  imported bindings.   LABEL-VEC  is  a vector  of  label
      ;;gensyms uniquely associated to the imported bindings.
      (receive (name-vec label-vec)
	  (parse-import-spec* import-spec*)
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
   (config.initialise-expander)
   (let* ((lib    (libman.find-library-by-reference libref))
	  (rib    (export-subst->rib (libman.library-export-subst lib)))
	  (lexenv '()))
     (make-interaction-env rib lexenv))))

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


(define* (expand-form-to-core-language expr env)
  ;;Interface to the internal expression expander (chi-expr).  Take an expression and
  ;;an  environment.  Return  two values:  the resulting  core-expression, a  list of
  ;;libraries that must be invoked before evaluating the core expr.
  ;;
  (config.initialise-expander)
  (cond ((env? env)
	 (let ((rib (make-rib/top-from-source-names-and-labels (vector->list (env-names  env))
							       (vector->list (env-labels env)))))
	   (let ((expr.stx (wrap-source-expression expr rib))
		 (rtc      (make-collector)))
	     (let ((psi (let ((vtc (make-collector))
			      (itc (env-itc env)))
			  (parametrise ((top-level-context #f)
					(inv-collector rtc)
					(vis-collector vtc)
					(imp-collector itc))
			    (let ((lexenv.run     '())
				  (lexenv.expand  '()))
			      (chi-expr expr.stx lexenv.run lexenv.expand))))))
	       (seal-rib! rib)
	       (values (psi-core-expr psi) (rtc))))))

	((interaction-env? env)
	 (let ((rib         (interaction-env-rib    env))
	       (lexenv.run  (interaction-env-lexenv env)))
	   (let* ((expr.stx (wrap-source-expression expr rib))
		  (rtc      (make-collector)))
	     (receive (expr.core lexenv.run^)
		 (let ((vtc (make-collector))
		       (itc (make-collector)))
		   (parametrise ((top-level-context env)
				 (inv-collector rtc)
				 (vis-collector vtc)
				 (imp-collector itc))
		     (chi-interaction-expr expr.stx rib lexenv.run)))
	       ;;All the  new syntactic bindings  added to the  lexical environment
	       ;;are persistent across code evaluations.   The rib has already been
	       ;;mutated  to hold  them.   Here  we store  in  ENV  the new  lexenv
	       ;;entries.
	       (set-interaction-env-lexenv! env lexenv.run^)
	       (values expr.core (rtc))))))

	(else
	 (procedure-argument-violation __who__ "not an environment" env))))


(define (expand-top-level-make-compiler expr*)
  ;;Given a list  of SYNTAX-MATCH expression arguments representing a  R6RS top level
  ;;program,  expand it  and return  a thunk.
  ;;
  ;;If the returned thunk is called: it invokes the needed libraries; it compiles the
  ;;program; it  returns 2  values: a  list of  library descriptors  representing the
  ;;invoke libraries; a thunk to be called to run the program.
  ;;
  ;;The usage pattern is this:
  ;;
  ;;  (define program-sexp
  ;;    '((import (vicare))
  ;;      (display "Hello World!")
  ;;      (flush-output-port (current-output-port))))
  ;;
  ;;  ;;Expand the program.
  ;;  (define compiler-thunk
  ;;    (expand-top-level-make-compiler program-sexp))
  ;;
  ;;  ;;Invoke the dependency libraries and compile the program.
  ;;  (define-values (lib-descr* run-thunk option* foreign-library*)
  ;;    (compiler-thunk))
  ;;
  ;;  ;;Run the program.
  ;;  (run-thunk)
  ;;
  ;;To serialise a compiled program: we serialise the RUN-THUNK closure object.
  ;;
  (receive (invoke-lib* invoke-code visit-code* export-subst global-env option* foreign-library*)
      (expand-top-level-program expr*)
    (lambda ()
      ;;Make  sure  that  the code  of  all  the  needed  libraries is  compiled  and
      ;;evaluated.  The storage location gensyms  associated to the exported bindings
      ;;are initialised with the global values.
      (for-each libman.invoke-library invoke-lib*)
      ;;Store the  expanded code representing  the macros in the  associated location
      ;;gensyms.
      (initial-visit! visit-code*)
      (values (map libman.library-descriptor invoke-lib*)
	      ;;Convert  the  expanded language  code  to  core language  code,  then
	      ;;compile it and wrap it into a thunk.
	      (compiler.compile-core-expr-to-thunk (expanded->core invoke-code))
	      option*
	      foreign-library*))))


;;;; R6RS program expander

(module (expand-top-level-program)

  (define-module-who expand-top-level-program)

  (define (expand-top-level-program program-form*)
    ;;Given  a list  of SYNTAX-MATCH  expression arguments  representing an  R6RS top
    ;;level program, expand it.
    ;;
    (config.initialise-expander)
    (receive (import-spec* body* option* foreign-library*)
	(%parse-top-level-program program-form*)
      (let ((foreign-library*  (%parse-foreign-library* foreign-library*)))
	(map foreign.dynamically-load-shared-object-from-identifier foreign-library*)
	(receive (import-spec* invoke-lib* visit-lib* invoke-code visit-code* export-subst global-env)
	    (let ((option* (%parse-program-options option*))
		  (mixed-definitions-and-expressions? #t))
	      (import CORE-BODY-EXPANDER)
	      (core-body-expander 'all import-spec* option* body* mixed-definitions-and-expressions?
				  %verbose-messages-thunk))
	  (values invoke-lib* invoke-code visit-code* export-subst global-env option* foreign-library*)))))

  (define (%verbose-messages-thunk)
    (when (option.typed-language?)
      (print-expander-warning-message "enabling tagged language support for program"))
    (when (option.strict-r6rs)
      (print-expander-warning-message "enabling expander's strict R6RS support for program")))

  (define (%parse-top-level-program program-form*)
    ;;Given  a  list  of  SYNTAX-MATCH  expression  arguments  representing  an  R6RS
    ;;top-level program or a Vicare top-level program, parse it and return 4 values:
    ;;
    ;;1. A list of import specifications.
    ;;
    ;;2. A list of body forms.
    ;;
    ;;3. A list of options specifications.
    ;;
    ;;4. A list of shared library identifiers.
    ;;
    (syntax-match program-form* ()

      (((?import ?import-spec* ...) ?body* ...)
       (eq? (syntax->datum ?import) 'import)
       (values ?import-spec* ?body* '() '()))

      (((?program ?program-name . ?stuff*))
       (eq? (syntax->datum ?program) 'program)
       (let loop ((stuff*           ?stuff*)
		  (option*          '())
		  (foreign-library* '()))
	 (syntax-match stuff* ()
	   (((?import ?import-spec* ...) ?body* ...)
	    (eq? (syntax->datum ?import) 'import)
	    (values ?import-spec* ?body* option* (reverse foreign-library*)))

	   (((?options ?progopt* ...) . ?stuff*)
	    (eq? (syntax->datum ?options) 'options)
	    (if (null? option*)
		(loop ?stuff* ?progopt* foreign-library*)
	      (syntax-violation __module_who__
		"OPTIONS clause can appear only once in top-level PROGRAM forms")))

	   (((?foreign-library ?libid) . ?stuff*)
	    (eq? (syntax->datum ?foreign-library) 'foreign-library)
	    (loop ?stuff* option* (cons ?libid foreign-library*)))

	   (_
	    (syntax-violation __module_who__ "malformed top-level program")))))

      (_
       (assertion-violation __module_who__ "invalid syntax of top-level program"))))

  (define (%parse-program-options option*)
    (syntax-match option* ()
      (() '())
      ((?opt . ?other*)
       (symbol? (syntax->datum ?opt))
       (let ((sym (syntax->datum ?opt)))
	 (case sym
	   ((typed-language)
	    (cons sym (%parse-program-options ?other*)))
	   ;;"tagged-language"  is  kept  for  backwards  compatibility,  but  it  is
	   ;;deprecated.  We should use "typed-language".
	   ((tagged-language)
	    (cons 'typed-language (%parse-program-options ?other*)))
	   ((strict-r6rs)
	    (cons sym (%parse-program-options ?other*)))
	   (else
	    (syntax-violation __module_who__
	      "invalid program option" ?opt)))))
      ))

  (define (%parse-foreign-library* foreign-library*)
    (receive-and-return (id*)
	(map syntax->datum foreign-library*)
      (unless (for-all (lambda (id)
			 (and (string? id)
			      (not (fxzero? (string-length id)))))
		id*)
	(syntax-violation __module_who__
	  "invalid foreign library identifiers" foreign-library*))))

  #| end of module: EXPAND-TOP-LEVEL-PROGRAM |# )

(define (expand-top-level-program->sexp sexp)
  (receive (invoke-lib* invoke-code visit-code* export-subst global-env option* foreign-library*)
      (expand-top-level-program sexp)
    `((invoke-lib*	. ,invoke-lib*)
      (invoke-code	. ,invoke-code)
      (visit-code*	. ,visit-code*)
      (export-subst	. ,export-subst)
      (global-env	. ,global-env)
      (option*		. ,option*)
      (foreign-library* . ,foreign-library*))))


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
  ;;The optional FILENAME  must be #f or  a string representing the  source file from
  ;;which the library was loaded; it is used for information purposes.
  ;;
  ;;The optional argument VERIFY-LIBNAME must be a procedure accepting a R6RS library
  ;;name as argument;  it is meant to  perform some validation upon  the library name
  ;;components (especially the version) and raise an exception if something is wrong;
  ;;otherwise it should just return.
  ;;
  ;;Return a "library" object representing an interned library.
  ;;
  (case-define expand-library
    ((library-sexp)
     (expand-library library-sexp #f       (lambda (libname) (void))))
    ((library-sexp filename)
     (expand-library library-sexp filename (lambda (libname) (void))))
    ((library-sexp filename verify-libname)
     (config.initialise-expander)
     (receive (libname
	       import-lib* invoke-lib* visit-lib*
	       invoke-code visit-code*
	       export-subst global-env
	       guard-code guard-lib*
	       option* foreign-library*)
	 (parametrise ((libman.source-code-location (or filename (libman.source-code-location))))
	   (let ()
	     (import CORE-LIBRARY-EXPANDER)
	     (core-library-expander library-sexp verify-libname)))
       (let ((uid		(gensym)) ;library unique-symbol identifier
	     ;;Thunk to eval to visit the library.
	     (visit-proc	(lambda ()
				  ;;This initial visit is performed whenever a source
				  ;;library is visited.
				  (initial-visit! visit-code*)))
	     ;;Thunk to eval to invoke the library.
	     (invoke-proc	(lambda ()
				  (compiler.eval-core (expanded->core invoke-code))))
	     ;;This visit code is compiled and  stored in FASL files; the resulting
	     ;;code objects are  the ones evaluated whenever a  compiled library is
	     ;;loaded and visited.
	     (visit-code	(%build-visit-code visit-code* option*))
	     (visible?		#t))
	 ;;This call returns a "library" object.
	 (libman.intern-library
	  (libman.make-library uid libname
			       import-lib* visit-lib* invoke-lib*
			       export-subst global-env
			       visit-proc invoke-proc
			       visit-code invoke-code
			       guard-code guard-lib*
			       visible? filename
			       option* foreign-library*))))))

  (define (%build-visit-code visit-code* option*)
    ;;Return  a  sexp  representing  code  that initialises  the  bindings  of  macro
    ;;definitions in the  core language: the visit code; code  evaluated whenever the
    ;;library  is visited;  each library  is  visited only  once, the  first time  an
    ;;exported binding is used.
    ;;
    ;;VISIT-CODE* is a list of sublists.  The entries with format:
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
    ;;	      (make-expand-time-value (+ 4 5))
    ;;	      (primitive make-expand-time-value)
    ;;	      (annotated-call (+ 4 5)
    ;;          (primitive +) '4 '5))))
    ;;
    (if (null? visit-code*)
	(build-void)
      (build-with-compilation-options option*
        (build-sequence no-source
	  (map (lambda (entry)
		 (cond ((car entry)
			=> (lambda (loc)
			     (let ((rhs.core (cddr entry)))
			       (build-global-assignment no-source
				 loc rhs.core))))
		       (else
			(let ((expr.core (cdr entry)))
			  expr.core))))
	    visit-code*)))))

  #| end of module: EXPAND-LIBRARY |# )

(define (expand-library->sexp libsexp)
  (let ((lib (expand-library libsexp)))
    `((uid		. ,(libman.library-uid               lib))
      (libname		. ,(libman.library-name              lib))
      (import-libdesc*	. ,(map libman.library-descriptor    (libman.library-imp-lib* lib)))
      (visit-libdesc*	. ,(map libman.library-descriptor    (libman.library-vis-lib* lib)))
      (invoke-libdesc*	. ,(map libman.library-descriptor    (libman.library-inv-lib* lib)))
      (invoke-code	. ,(libman.library-invoke-code       lib))
      (visit-code	. ,(libman.library-visit-code        lib))
      (export-subst	. ,(libman.library-export-subst      lib))
      (global-env	. ,(libman.library-global-env        lib))
      (guard-code	. ,(libman.library-guard-code        lib))
      (guard-libdesc*	. ,(map libman.library-descriptor    (libman.library-guard-lib* lib)))
      (option*		. ,(libman.library-option*           lib))
      (foreign-library*	. ,(libman.library-foreign-library*  lib)))))


(module CORE-LIBRARY-EXPANDER
  (core-library-expander)
  (define-constant __module_who__ 'core-library-expander)

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
    (receive (libname export-spec* import-spec* body* libopt* foreign-library*)
	(%parse-library library-sexp)
      (%validate-library-name libname verify-libname)
      (let* ((libname.sexp  (syntax->datum libname))
	     (option*           (%parse-library-options libopt*))
	     (foreign-library*  (%parse-foreign-library* foreign-library*))
	     (stale-clt         (%make-stale-collector)))
	(map foreign.dynamically-load-shared-object-from-identifier foreign-library*)
	(receive (import-lib* invoke-lib* visit-lib* invoke-code visit-code* export-subst global-env)
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
		    invoke-code visit-code* export-subst
		    global-env guard-code guard-lib*
		    option* foreign-library*))))))

  (define (%make-verbose-messages-thunk libname.sexp)
    (lambda ()
      (when (option.typed-language?)
	(print-expander-warning-message "enabling tagged language support for library: ~a" libname.sexp))
      (when (option.strict-r6rs)
	(print-expander-warning-message "enabling expander's strict R6RS support for library: ~a" libname.sexp))))

  (define (%parse-library library-sexp)
    ;;Given an  ANNOTATION struct  representing a  LIBRARY form  symbolic expression,
    ;;return 6 values:
    ;;
    ;;1. The name part.  A SYNTAX-MATCH expression argument representing parts of the
    ;;library name.
    ;;
    ;;2.  The export  specs.   A SYNTAX-MATCH  expression  argument representing  the
    ;;exports specification.
    ;;
    ;;3.  The import  specs.   A SYNTAX-MATCH  expression  argument representing  the
    ;;imports specification.
    ;;
    ;;4. The  body of the  library.  A SYNTAX-MATCH expression  argument representing
    ;;the body of the library.
    ;;
    ;;5. The list of library options (like "strict-r6rs" and "visit-upon-loading").
    ;;
    ;;6. A list of strings representing platform's shared library identifiers.
    ;;
    ;;This function performs no validation of  the returned values, it just validates
    ;;the structure of the LIBRARY form.
    ;;
    (syntax-match library-sexp ()
      ((?library (?name* ...) . ?stuff*)
       (eq? (syntax->datum ?library) 'library)
       (let loop ((stuff*           ?stuff*)
		  (option*          '())
		  (foreign-library*  '()))
	 (syntax-match stuff* ()
	   (((?export ?exp* ...) (?import ?imp* ...) ?body* ...)
	    (and (eq? (syntax->datum ?export) 'export)
		 (eq? (syntax->datum ?import) 'import))
	    (values ?name* ?exp* ?imp* ?body* option*
		    (reverse foreign-library*)))

	   (((?options ?libopt* ...) . ?stuff*)
	    (eq? (syntax->datum ?options) 'options)
	    (if (null? option*)
		(loop ?stuff* ?libopt* foreign-library*)
	      (syntax-violation __module_who__
		"OPTIONS clause can appear only once in LIBRARY forms")))

	   (((?foreign-library ?libid) . ?stuff*)
	    (eq? (syntax->datum ?foreign-library) 'foreign-library)
	    (loop ?stuff* option* (cons ?libid foreign-library*)))

	   (_
	    (syntax-violation __module_who__ "malformed library" library-sexp)))))
      ))

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
	     (syntax-violation __module_who__ "invalid library name" libname))))
      (when (null? name*)
	(syntax-violation __module_who__ "empty library name" libname)))
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
	   ((typed-language)
	    (cons sym (%parse-library-options ?other*)))
	   ;;"tagged-language"  is  kept  for  backwards  compatibility,  but  it  is
	   ;;deprecated.  We should use "typed-language".
	   ((tagged-language)
	    (cons 'typed-language (%parse-library-options ?other*)))
	   ((strict-r6rs)
	    (cons sym (%parse-library-options ?other*)))
	   (else
	    (syntax-violation __module_who__
	      "invalid library option" ?opt)))))
      ))

  (define (%parse-foreign-library* foreign-library*)
    (receive-and-return (id*)
	(map syntax->datum foreign-library*)
      (unless (for-all (lambda (id)
			 (and (string? id)
			      (not (fxzero? (string-length id)))))
		id*)
	(syntax-violation __module_who__
	  "invalid foreign library identifiers" foreign-library*))))

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
  ;;5..VISIT-ENV* is a list of bindings  representing the macros defined in the code.
  ;;   For the example library VISIT-ENV* is:
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
  ;;7..GLOBAL-ENV is  the lexical  environment of bindings  exported by  the library.
  ;;   Its format  is different from the  one of the LEXENV.*  values used throughout
  ;;   the expansion process.  For the library in the example, GLOBAL-ENV is:
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
      (define (wrap-source-expression-with-top-rib expr)
	(wrap-source-expression expr rib))
      (with-typed-language (memq 'typed-language option*)
	(with-option-strict-r6rs (memq 'strict-r6rs option*)
	  (verbose-messages-thunk)
	  (let ((body-stx*	(map wrap-source-expression-with-top-rib body-sexp*))
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
		    (%parse-all-export-specs export-spec* internal-export* wrap-source-expression-with-top-rib rib)
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
		  (let* ((rhs*.psi      (chi-qrhs* qrhs*     lexenv.run lexenv.expand))
			 (init*.psi     (chi-expr* init*.stx lexenv.run lexenv.expand))
			 (loc*          (map generate-qrhs-loc qrhs*))
			 (export-subst  (%make-export-subst export-name* export-id*)))
		    (receive (global-env visit-env*)
			(%make-global-env/visit-env* lex* loc* lexenv.run)
		      (%validate-exports export-spec* export-subst global-env)
		      (let ((invoke-code (build-with-compilation-options option*
					   (build-library-letrec* no-source
					     mixed-definitions-and-expressions?
					     lex* loc* (map psi-core-expr rhs*.psi)
					     (if (null? init*.psi)
						 (build-void)
					       (build-sequence no-source
						 (map psi-core-expr init*.psi)))))))
			(values (itc) (rtc) (vtc)
				invoke-code visit-env* export-subst global-env))))))))))))

  (define-syntax-rule (%expanding-program? ?export-spec*)
    (eq? 'all ?export-spec*))

  (define (%process-import-specs-build-top-level-rib import-spec*)
    ;;Parse the import  specifications from a library's IMPORT clause  or a program's
    ;;standalone IMPORT syntax; build and  return the top-level "rib" struct defining
    ;;the top-level environment.
    ;;
    ;;IMPORT-NAME.VEC is a  vector of symbols representing the external  names of the
    ;;imported bindings;  this vector has  no duplicates.   LABEL.VEC is a  vector of
    ;;label gensyms uniquely associated to the  imported bindings; this vector has no
    ;;duplicates.
    (receive (import-name.vec label.vec)
	(parse-import-spec* import-spec*)
      (make-rib/top-from-source-names-and-labels (vector->list import-name.vec)
						 (vector->list label.vec))))

  (define (%process-internal-body body-stx* rib mixed-definitions-and-expressions?)
    ;;Perform  the preliminary  expansion of  the top-level  forms in  the body;  the
    ;;right-hand  sides of  DEFINE syntaxes  are *not*  expanded here;  the body  and
    ;;module trailing init forms are *not* expanded here.
    ;;
    (receive (trailing-init-form*.stx
	      lexenv.run lexenv.expand
	      lex* qrhs*
	      module-init-form**.stx unused-kwd* internal-export*)
	;;We are about  to expand syntactic forms  from the body in the  context of a
	;;non-interaction top-level environment.  When  calling CHI-BODY*, we set the
	;;argument SHADOW/REDEFINE-BINDINGS? to false because:
	;;
	;;* Syntactic  binding definitions at the  top-level of this body  must *not*
	;;shadow   imported  syntactic   bindings   established   by  the   top-level
	;;environment.  That is:
	;;
	;;   (import (rnrs))
	;;   (define display 123)
	;;
	;;is  a syntax  violation,  because the  DEFINE  use at  the  top-level of  a
	;;program's body cannot shadow the binding imported from "(rnrs)".
	;;
	;;* Syntactic binding definitions cannot be redefined.  That is:
	;;
	;;   (define a 1)
	;;   (define a 2)
	;;
	;;is  a syntax  violation  because the  use of  DEFINE  cannot redefined  the
	;;binding for "a".
	(let ((shadow/redefine-bindings?	#f)
	      (lexenv.run			'())
	      (lexenv.expand			'())
	      (lex*				'())
	      (qrhs*				'())
	      (mod**				'())
	      (kwd*				'())
	      (export-spec*			'()))
	  (chi-body* body-stx*
		     lexenv.run lexenv.expand
		     lex* qrhs* mod** kwd* export-spec* rib
		     mixed-definitions-and-expressions? shadow/redefine-bindings?))
      ;;We build a list  of init form putting first the trailing  init forms from the
      ;;internal   MODULE  syntaxes,   then  the   trailing  init   forms  from   the
      ;;library/program body.
      (let ((init-form*.stx (reverse-and-append-with-tail module-init-form**.stx trailing-init-form*.stx)))
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

  (define (%parse-all-export-specs export-spec* internal-export* wrap-source-expression-with-top-rib top-level-rib)
    ;;Parse all the export specifications.
    ;;
    ;;EXPORT-SPEC*  must   be  a   list  of   identifiers  representing   the  export
    ;;specifications declared in the EXPORT clause of a LIBRARY form.
    ;;
    ;;INTERNAL-EXPORT*  must  be  a  list  of  identifiers  representing  the  export
    ;;specifications declared using the EXPORT syntax in the body.
    ;;
    (parse-export-spec*
     (if (%expanding-program? export-spec*)
	 ;;In a program we do not care about the internal EXPORT syntax uses: we just
	 ;;ignore INTERNAL-EXPORT*.
	 (map wrap-source-expression-with-top-rib
	   (rib-src-marked-source-names top-level-rib))
       (append (map wrap-source-expression-with-top-rib
		 export-spec*)
	       internal-export*))))

  (define (%make-export-subst export-name* export-id*)
    ;;For every identifier in  EXPORT-ID*: get the rib of ID  and extract the lexical
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
	       (syntax-violation #f "cannot export unbound identifier" export-id))))
      export-name* export-id*))

  (define* (%make-global-env/visit-env* lex* loc* lexenv.run)
    ;;For each entry in LEXENV.RUN: convert  the LEXENV entry to an GLOBAL-ENV entry,
    ;;accumulating GLOBAL-ENV;  if the syntactic  binding is a macro  or expand-time
    ;;value: accumulate the VISIT-ENV* alist.
    ;;
    ;;Notice that  GLOBAL-ENV contains  an entry for  every global  lexical variable,
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
	       (global-env	'())
	       (visit-env*	'()))
      (if (null? lexenv.run)
	  (values global-env visit-env*)
	(let* ((entry    (car lexenv.run))
	       (label    (lexenv-entry.label entry))
	       (binding  (lexenv-entry.binding-descriptor entry)))
	  (case (syntactic-binding-descriptor.type binding)
	    ((lexical)
	     ;;This binding is an UNtyped lexical variable.  When we import a lexical
	     ;;binding from another library, we must see such entry as "global".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (lexical . (?lexvar . ?mutable)))
	     ;;
	     ;;Add to the GLOBAL-ENV an entry like:
	     ;;
	     ;;   (?label . (?type . ?lex/loc))
	     ;;
	     ;;where: ?TYPE  is the symbol  "global-mutable" or the  symbol "global";
	     ;;?lex/loc is the loc gensym of the variable, used as lex gensym and loc
	     ;;gensym.
	     ;;
	     ;;NOTE  The  entries  of  type  "global-mutable"  are  forbidden  to  be
	     ;;exported: entries  of this  type can  be in  the GLOBAL-ENV,  but they
	     ;;cannot  be referenced  in the  EXPORT-SUBST; this  validation will  be
	     ;;performed later.
	     ;;
	     (let* ((bind-val  (syntactic-binding-descriptor.value binding))
		    ;;Search for LEXICAL-GENSYM  in the list LEX*:  when found return
		    ;;the corresponding gensym from  LOC*.  LEXICAL-GENSYM must be an
		    ;;item in LEX*.
		    (loc       (let lookup ((lexical-gensym  (lexical-var-binding-descriptor-value.lex-name bind-val))
					    (lex*            lex*)
					    (loc*            loc*))
				 (if (pair? lex*)
				     (if (eq? lexical-gensym (car lex*))
					 (car loc*)
				       (lookup lexical-gensym (cdr lex*) (cdr loc*)))
				   (assertion-violation/internal-error 'make-global-env/lookup
				     "missing lexical gensym in lexenv" lexical-gensym))))
		    (type      (if (lexical-var-binding-descriptor-value.assigned? bind-val)
				   'global-mutable
				 'global)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label type loc) global-env)
		     visit-env*)))

	    ((lexical-typed)
	     ;;This binding  is an typed  lexical variable.   When we import  a typed
	     ;;lexical  binding from  another  library,  we must  see  such entry  as
	     ;;"global-typed".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (lexical-typed . ?lexical-typed-spec))
	     ;;
	     ;;Add to the GLOBAL-ENV an entry like:
	     ;;
	     ;;   (?label . (?type . ?global-typed-spec))
	     ;;
	     ;;where:  ?TYPE  is  the  symbol "global-typed-mutable"  or  the  symbol
	     ;;"global-typed";     ?global-typed-spec    is     an    instance     of
	     ;;"<global-typed-spec>".
	     ;;
	     ;;NOTE The  entries of type  "global-typed-mutable" are forbidden  to be
	     ;;exported: entries  of this  type can  be in  the GLOBAL-ENV,  but they
	     ;;cannot  be referenced  in the  EXPORT-SUBST; this  validation will  be
	     ;;performed later.
	     ;;
	     (let* ((spec  (syntactic-binding-descriptor.value binding))
		    ;;Search for LEXICAL-GENSYM  in the list LEX*:  when found return
		    ;;the corresponding gensym from  LOC*.  LEXICAL-GENSYM must be an
		    ;;item in LEX*.
		    (loc   (let lookup ((lexical-gensym  (lexical-typed-spec.lex spec))
					(lex*            lex*)
					(loc*            loc*))
			     (if (pair? lex*)
				 (if (eq? lexical-gensym (car lex*))
				     (car loc*)
				   (lookup lexical-gensym (cdr lex*) (cdr loc*)))
			       (assertion-violation/internal-error 'make-global-env/lookup
				 "missing lexical gensym in lexenv" lexical-gensym))))
		    (type      (if (lexical-typed-spec.assigned? spec)
				   'global-typed-mutable
				 'global-typed)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label type (make-global-typed-spec loc spec)) global-env)
		     visit-env*)))

	    ((local-macro)
	     ;;When we define a syntactic binding representing a keyword binding with
	     ;;non-variable transformer: the local code sees the descriptor with type
	     ;;"local-macro".  If we  export such binding: the importer  must see the
	     ;;descriptor with type "global-macro".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (local-macro . (?transformer . ?expanded-expr)))
	     ;;
	     ;;Add to the GLOBAL-ENV an entry like:
	     ;;
	     ;;   (?label global-macro . ?loc)
	     ;;
	     ;;and to the VISIT-ENV* an entry like:
	     ;;
	     ;;   (?loc . (?transformer . ?expanded-expr))
	     ;;
	     (let ((loc (generate-storage-location-gensym label)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label 'global-macro loc) global-env)
		     (cons (cons loc (syntactic-binding-descriptor.value binding)) visit-env*))))

	    ((local-macro!)
	     ;;When we define a syntactic binding representing a keyword binding with
	     ;;variable transformer:  the local  code sees  the descriptor  with type
	     ;;"local-macro!".  If we export such  binding: the importer must see the
	     ;;descriptor with type "global-macro!".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (local-macro! . (?transformer . ?expanded-expr)))
	     ;;
	     ;;Add to the GLOBAL-ENV an entry like:
	     ;;
	     ;;   (?label global-macro . ?loc)
	     ;;
	     ;;and to the VISIT-ENV* an entry like:
	     ;;
	     ;;   (?loc . (?transformer . ?expanded-expr))
	     ;;
	     (let ((loc (generate-storage-location-gensym label)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label 'global-macro! loc) global-env)
		     (cons (cons loc (syntactic-binding-descriptor.value binding)) visit-env*))))

	    ((local-etv)
	     ;;When we define a binding for a expand-time value (ETV): the local code
	     ;;sees it as "local-etv".  If we  export such binding: the importer must
	     ;;see it as a "global-etv".
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (local-etv . (?object . ?expanded-expr)))
	     ;;
	     ;;Add to the GLOBAL-ENV an entry like:
	     ;;
	     ;;   (?label . (global-etv . ?loc))
	     ;;
	     ;;and to the VISIT-ENV* an entry like:
	     ;;
	     ;;   (?loc . (?object . ?expanded-expr))
	     ;;
	     (let ((loc (generate-storage-location-gensym label)))
	       (loop (cdr lexenv.run)
		     (cons (cons* label 'global-etv loc) global-env)
		     (cons (cons loc (syntactic-binding-descriptor.value binding)) visit-env*))))

	    (($record-type-name $struct-type-name $scheme-type-name $module $fluid $synonym)
	     ;;Just  add the  entry  "as  is" from  the  lexical  environment to  the
	     ;;GLOBAL-ENV.
	     ;;
	     (loop (cdr lexenv.run)
		   (cons entry global-env)
		   visit-env*))

	    ((begin-for-syntax)
	     ;;This entry is  the result of expanding BEGIN-FOR-SYNTAX  macro use; we
	     ;;want this code to be part of the visit code.
	     ;;
	     ;;The entry from the LEXENV looks like this:
	     ;;
	     ;;   (?label . (begin-for-syntax . ?expanded-expr))
	     ;;
	     ;;add to the VISIT-ENV* an entry like:
	     ;;
	     ;;   (#f . ?expanded-expr)
	     ;;
	     (loop (cdr lexenv.run)
		   global-env
		   (cons (cons #f (syntactic-binding-descriptor.value binding)) visit-env*)))

	    (else
	     (assertion-violation/internal-error 'core-body-expander
	       "unknown or unexportable syntactic binding"
	       binding)))))))

  (define (%validate-exports export-spec* export-subst global-env)
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
    ;;Entries of type "global-mutable" and "global-mutable-typed" are forbidden to be
    ;;exported: entries  of this type  can be in the  GLOBAL-ENV, but they  cannot be
    ;;referenced in the EXPORT-SUBST.
    ;;
    (define export-subst-entry-name  car)
    (define export-subst-entry-label cdr)
    (unless (%expanding-program? export-spec*)
      (for-each (lambda (subst)
		  (cond ((assq (export-subst-entry-label subst) global-env)
			 => (lambda (entry)
			      (let ((type (syntactic-binding-descriptor.type (lexenv-entry.binding-descriptor entry))))
				(when (or (eq? type 'global-mutable)
					  (eq? type 'global-typed-mutable))
				  (syntax-violation 'export
				    "attempt to export mutated variable"
				    (export-subst-entry-name subst))))))))
	export-subst)))

  #| end of module: CORE-BODY-EXPANDER |# )


;;;; GLOBAL-ENV helpers

(define-syntax-rule (make-global-env-entry ?label ?type ?loc)
  ;;Given a  label gensym, a  symbol representing an GLOBAL-ENV  type, a
  ;;loc gensym: build and return an entry of GLOBAL-ENV.
  ;;
  (cons* ?label ?type ?loc))

(define-syntax-rule (export-binding-type ?export-binding)
  ;;Given an export binding return the symbol representin its type.
  ;;
  (car ?export-binding))

(define-syntax-rule (export-binding-loc ?export-binding)
  ;;Given an export  binding return the loc gensym holding  its value.  Remember that
  ;;not all the GLOBAL-ENV entries have a loc gensym.
  ;;
  (cdr ?export-binding))


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


;;;; done

;; #!vicare
;; (import (only (vicare) foreign-call))
;; (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.expander before"))

;;Register the expander with the library manager.
(libman.current-library-expander expand-library)

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
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;eval: (put 'with-typed-language		'scheme-indent-function 1)
;;eval: (put 'with-option-strict-r6rs		'scheme-indent-function 1)
;;End:
