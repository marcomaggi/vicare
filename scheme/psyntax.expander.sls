;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; copyright notice for the original code of the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;;copyright notice for the original code of RECEIVE
;;;
;;;Copyright (C) John David Stone (1999). All Rights Reserved.
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
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


(library (psyntax expander)
  (export
    eval
    environment				environment?
    null-environment			scheme-report-environment
    interaction-environment		new-interaction-environment

    ;; inspection of non-interaction environment objects
    environment-symbols			environment-libraries
    environment-labels			environment-binding

    expand-form-to-core-language	expand-top-level
    expand-library
    compile-r6rs-top-level		boot-library-expand
    make-compile-time-value

    generate-temporaries		identifier?
    free-identifier=?			bound-identifier=?
    datum->syntax			syntax->datum

    syntax-violation			assertion-error

    syntax-error

    make-variable-transformer		variable-transformer?
    variable-transformer-procedure

    syntax-dispatch			syntax-transpose
    ellipsis-map

    ;;The following are inspection functions for debugging purposes.
    (rename (<stx>?		syntax-object?)
	    (<stx>-expr		syntax-object-expression)
	    (<stx>-mark*	syntax-object-marks)
	    (<stx>-subst*	syntax-object-substs)
	    (<stx>-ae*		syntax-object-source-objects)))
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer
		  syntax-error)
    (prefix (rnrs syntax-case) sys.)
    (rnrs mutable-pairs)
    (psyntax library-manager)
    (psyntax builders)
    (psyntax compat)
    (psyntax config)
    (psyntax internal))


;;;; unsafe operations

(module UNSAFE
  ($car $cdr
   $vector-ref $vector-set! $vector-length
   $fx= $fxadd1)
  #;(import (ikarus system $pairs))
  (begin
    (define $car car)
    (define $cdr cdr))
  #;(import (ikarus system $vectors))
  (begin
    (define $vector-ref vector-ref)
    (define $vector-set! vector-set!)
    (define $vector-length vector-length))
  #;(import (ikarus system $fx))
  (begin
    (define $fx= =)
    (define ($fxadd1 N)
      (+ 1 N)))
  #| end of module |# )

(import UNSAFE)


;;; helpers

;;This syntax can be used as standalone identifier andexpands to #f.  It
;;is  used as  "annotated expression"  argument in  calls to  the BUILD-
;;functions when there is no annotated expression to be given.
;;
(define-syntax no-source
  (lambda (x) #f))

(define (%generate-unique-symbol seed)
  ;;Generate and return a fresh unique symbol.  SEED is used to seed the
  ;;generation: it must be a symbol or an identifier syntax object.
  ;;
  (define who '%generate-unique-symbol)
  (cond ((symbol? seed)
	 (gensym seed))
	((<stx>? seed)
	 (%generate-unique-symbol (identifier->symbol seed)))
	(else
	 (assertion-violation who
	   "expected symbol or identifier as argument" seed))))

(define (debug-print . args)
  ;;Print arguments for debugging purposes.
  ;;
  (pretty-print args (current-error-port))
  (newline (current-error-port))
  (newline (current-error-port)))


;;;; top-level environments
;;
;;The result of  parsing a set of  import specs in an  IMPORT clause (as
;;defined by R6RS and extended  by Vicare) and loading the corresponding
;;libraries is an  ENV data structure; ENV data  structures represent an
;;*immutable* top level environment.
;;
;;Whenever  a REPL  is created  (Vicare can  launch multiple  REPLs), an
;;interaction environment is created to  serve as top level environment.
;;The  interaction  environment  is  initialised with  the  core  Vicare
;;library  "(ikarus)";  an  interaction environment  is  *mutable*:  new
;;bindings can be added to it.  For this reason interaction environments
;;are  represented by  data  structures of  type INTERACTION-ENV,  whose
;;internal format allows adding new bindings.
;;
;;Let's  step back:  how does  the  REPL work?   Every time  we type  an
;;expression  and press  "Return":  the expression  is  expanded in  the
;;context of  the current  interaction environment, compiled  to machine
;;code, executed.   Every REPL expression  is like a full  R6RS program,
;;with the  exception that  the interaction environment  "remembers" the
;;bindings we define.
;;

;;An ENV record encapsulates a substitution and a set of libraries.
;;
(define-record env
  (names
		;A vector  of symbols  representing the public  names of
		;bindings from a set of import specifications as defined
		;by  R6RS.   These  names  are from  the  subst  of  the
		;libraries, already processed with the directives in the
		;import sets (prefix, deprefix, only, except, rename).
   labels
		;A vector of symbols representing the labels of bindings
		;from a set of import specifications as defined by R6RS.
		;These labels are from the subst of the libraries.
   itc
		;A collector  function (see MAKE-COLLECTOR)  holding the
		;LIBRARY structs representing  the libraries selected by
		;the source import specifications.
   )
  (lambda (S port sub-printer)
    (display "#<environment>" port)))

(define-record interaction-env
  (rib
		;The top <RIB>  structure for the evaluation  of code in
		;this environment.
   lexenv
		;The lexical  environment for  both run time  and expand
		;time.
   locs
		;???
   )
  (lambda (S port sub-printer)
    (display "#<interaction-environment>" port)))

(define (environment? obj)
  (or (env? obj)
      (interaction-env? obj)))

(define (environment-symbols x)
  ;;Return a list of symbols representing the names of the bindings from
  ;;the given environment.
  ;;
  (define who 'environment-symbols)
  (cond ((env? x)
	 (vector->list ($env-names x)))
	((interaction-env? x)
	 (map values ($<rib>-sym* ($interaction-env-rib x))))
	(else
	 (assertion-violation who "not an environment" x))))

(define (environment-labels x)
  ;;Return a  list of  symbols representing the  labels of  the bindings
  ;;from the given environment.
  ;;
  (define who 'environment-labels)
  (unless (env? x)
    (assertion-violation who
      "expected non-interaction environment object as argument" x))
  (vector->list ($env-labels x)))

(define (environment-libraries x)
  ;;Return  the  list  of  LIBRARY records  representing  the  libraries
  ;;forming the environment.
  ;;
  (define who 'environment-libraries)
  (unless (env? x)
    (assertion-violation who
      "expected non-interaction environment object as argument" x))
  (($env-itc x)))

(define (environment-binding sym env)
  ;;Search the symbol SYM in the non-interaction environment ENV; if SYM
  ;;is the public  name of a binding  in ENV return 2  values: the label
  ;;associated  to the  binding,  the list  of  values representing  the
  ;;binding.  If SYM is not present in ENV return false and false.
  ;;
  (define who 'environment-binding)
  (unless (env? env)
    (assertion-violation who
      "expected non-interaction environment object as argument" env))
  (let ((P (vector-exists (lambda (name label)
			    (import (ikarus system $symbols))
			    (and (eq? sym name)
				 (cons label ($symbol-value label))))
	     ($env-names  env)
	     ($env-labels env))))
    (if P
	(values (car P) (cdr P))
      (values #f #f))))

;;; --------------------------------------------------------------------

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
      (receive (subst.names subst.labels)
	  (parse-import-spec* import-spec*)
	(make-env subst.names subst.labels itc)))))

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

(define (new-interaction-environment)
  ;;Build and return a new interaction environment.
  ;;
  (let* ((lib (find-library-by-name (base-of-interaction-library)))
	 (rib (subst->rib (library-subst lib))))
    (make-interaction-env rib '() '())))

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


(module (eval expand-form-to-core-language)

  (define (eval x env)
    ;;This  is R6RS's  eval.   Take an  expression  and an  environment:
    ;;expand the  expression, invoke  its invoke-required  libraries and
    ;;evaluate  its  expanded  core  form.  Return  the  result  of  the
    ;;expansion.
    ;;
    (define who 'eval)
    (unless (environment? env)
      (error who "not an environment" env))
    (receive (x invoke-req*)
	(expand-form-to-core-language x env)
      (for-each invoke-library invoke-req*)
      (eval-core (expanded->core x))))

  (define (expand-form-to-core-language expr env)
    ;;Interface to the internal expression expander (chi-expr).  Take an
    ;;expression and  an environment.  Return two  values: the resulting
    ;;core-expression, a list  of libraries that must  be invoked before
    ;;evaluating the core expr.
    ;;
    (define who 'expand-form-to-core-language)
    (cond ((env? env)
	   (let ((rib (make-top-rib (env-names env) (env-labels env))))
	     (let ((expr.stx (make-<stx> expr top-mark* (list rib) '()))
		   (rtc      (make-collector))
		   (vtc      (make-collector))
		   (itc      (env-itc env)))
	       (let ((expr.core (parametrise ((top-level-context #f)
					      (inv-collector rtc)
					      (vis-collector vtc)
					      (imp-collector itc))
				  (chi-expr expr.stx
					    '() #;lexenv.run '() #;lexenv.expand))))
		 (seal-rib! rib)
		 (values expr.core (rtc))))))
	  ((interaction-env? env)
	   (let ((rib         (interaction-env-rib env))
		 (lexenv.run  (interaction-env-lexenv env))
		 (rtc         (make-collector)))
	     (let ((expr.stx (make-<stx> expr top-mark* (list rib) '())))
	       (receive (expr.core lexenv.run^)
		   (parametrise ((top-level-context env)
				 (inv-collector rtc)
				 (vis-collector (make-collector))
				 (imp-collector (make-collector)))
		     (%chi-interaction-expr expr.stx rib lexenv.run))
		 (set-interaction-env-lexenv! env lexenv.run^)
		 (values expr.core (rtc))))))
	  (else
	   (assertion-violation who "not an environment" env))))

  (define (%chi-interaction-expr expr.stx rib lexenv.run)
    (receive (e* lexenv.run^ lexenv.expand^ lex* rhs* mod** _kwd* _exp*)
	(chi-body* (list expr.stx) lexenv.run lexenv.run
		   '() '() '() '() '() rib #t #f)
      (let ((expr.core* (%expand-interaction-rhs*/init*
			 (reverse lex*) (reverse rhs*)
			 (append (apply append (reverse mod**)) e*)
			 lexenv.run^ lexenv.expand^)))
	(let ((expr.core (cond ((null? expr.core*)
				(build-void))
			       ((null? (cdr expr.core*))
				(car expr.core*))
			       (else
				(build-sequence no-source expr.core*)))))
	  (values expr.core lexenv.run^)))))

  (define (%expand-interaction-rhs*/init* lhs* rhs* init* lexenv.run lexenv.expand)
    ;;Return a list of expressions in the core language.
    ;;
    (define who 'expand-interaction)
    (let recur ((lhs* lhs*)
		(rhs* rhs*))
      (if (null? lhs*)
	  (map (lambda (init)
		 (chi-expr init lexenv.run lexenv.expand))
	    init*)
	(let ((lhs (car lhs*))
	      (rhs (car rhs*)))
	  (define-inline (%recurse-and-cons ?core-expr)
	    (cons ?core-expr
		  (recur (cdr lhs*) (cdr rhs*))))
	  (case (car rhs)
	    ((defun)
	     (let ((rhs (chi-defun (cdr rhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source lhs rhs))))
	    ((expr)
	     (let ((rhs (chi-expr (cdr rhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source lhs rhs))))
	    ((top-expr)
	     (let ((core-expr (chi-expr (cdr rhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons core-expr)))
	    (else
	     (error who "invalid" rhs)))))))

  #| end of module: EXPAND-FORM-TO-CORE-LANGUAGE |# )


;;;; R6RS top level programs

(define (compile-r6rs-top-level expr*)
  ;;Given a  list of  SYNTAX-MATCH expression arguments  representing an
  ;;R6RS top  level program, expand  it and  return a thunk  which, when
  ;;evaluated,  compiles  the  program and  returns  an  INTERACTION-ENV
  ;;struct representing the environment after the program execution.
  ;;
  (receive (lib* invoke-code macro* export-subst export-env)
      (expand-top-level expr*)
    (lambda ()
      (for-each invoke-library lib*)
      (initial-visit! macro*)
      (eval-core (expanded->core invoke-code))
      (make-interaction-env (subst->rib export-subst)
			    (map (lambda (x)
				   (let* ((label    (car x))
					  (binding  (cdr x))
					  (type     (car binding))
					  (val      (cdr binding)))
				     (cons* label type '*interaction* val)))
			      export-env)
			    '()))))

(module (expand-top-level)

  (define (expand-top-level expr*)
    ;;Given a list of  SYNTAX-MATCH expression arguments representing an
    ;;R6RS top level program, expand it.
    ;;
    (receive (import-spec* body*)
	(%parse-top-level-program expr*)
      (receive (import-spec* invoke-req* visit-req* invoke-code macro* export-subst export-env)
	  (library-body-expander 'all import-spec* body* #t)
	(values invoke-req* invoke-code macro* export-subst export-env))))

  (define (%parse-top-level-program expr*)
    ;;Given a list of  SYNTAX-MATCH expression arguments representing an
    ;;R6RS top level program, parse it and return 2 values:
    ;;
    ;;1. A list of import specifications.
    ;;
    ;;2. A list of body forms.
    ;;
    (syntax-match expr* ()
      (((?import ?import-spec* ...) body* ...)
       (eq? (syntax->datum ?import) 'import)
       (values ?import-spec* body*))

      (((?import . x) . y)
       (eq? (syntax->datum ?import) 'import)
       (syntax-violation 'expander
	 "invalid syntax of top-level program" (syntax-car expr*)))

      (_
       (assertion-violation 'expander
	 "top-level program is missing an (import ---) clause"))))

  #| end of module: EXPAND-TOP-LEVEL |# )


(define (boot-library-expand x)
  ;;When bootstrapping  the system,  visit-code is  not (and  cannot be)
  ;;used in the "next" system.  So, we drop it.
  ;;
  (receive (id
	    name ver
	    imp* vis* inv*
	    invoke-code visit-code export-subst export-env
	    guard-code guard-dep*)
      (expand-library x)
    (values name invoke-code export-subst export-env)))

(define expand-library
  ;;Expand  a  symbolic  expression   representing  a  LIBRARY  form  to
  ;;core-form;  register  it  with   the  library  manager;  return  its
  ;;invoke-code, visit-code, subst and env.
  ;;
  ;;The argument LIBRARY-SEXP must  be the symbolic expression:
  ;;
  ;;   (library . _)
  ;;
  ;;or an ANNOTATION struct representing such expression.
  ;;
  ;;The optional FILENAME must be #f or a string representing the source
  ;;file from which  the library was loaded; it is  used for information
  ;;purposes.
  ;;
  ;;The optional VERIFY-NAME  must be a procedure  accepting 2 arguments
  ;;and returning  unspecified values: the  first argument is a  list of
  ;;symbols from a  library name; the second argument is  null or a list
  ;;of exact integers representing  the library version.  VERIFY-NAME is
  ;;meant to  perform some validation  upon the library  name components
  ;;and raise  an exception if  something is wrong; otherwise  it should
  ;;just return.
  ;;
  (case-lambda
   ((library-sexp filename verify-name)
    (define (build-visit-code macro*)
      ;;Return a symbolic expression  representing MACRO* definitions in
      ;;the core language.
      ;;
      (if (null? macro*)
	  (build-void)
	(build-sequence no-source
			(map (lambda (x)
			       (let ((loc (car x))
				     (src (cddr x)))
				 (build-global-assignment no-source loc src)))
			  macro*))))
    (let-values (((name ver imp* inv* vis*
			invoke-code macro* export-subst export-env
			guard-code guard-req*)
		  (core-library-expander library-sexp verify-name)))
      (let ((id            (gensym)) ;library UID
	    (name          name)     ;list of name symbols
	    (ver           ver)	     ;null or list of version numbers

	    ;;From list  of LIBRARY  records to  list of  lists: library
	    ;;UID, list of name symbols, list of version numbers.
	    (imp*          (map library-spec imp*))
	    (vis*          (map library-spec vis*))
	    (inv*          (map library-spec inv*))
	    (guard-req*    (map library-spec guard-req*))

	    ;;Thunk to eval to visit the library.
	    (visit-proc    (lambda ()
			     (initial-visit! macro*)))
	    ;;Thunk to eval to invoke the library.
	    (invoke-proc   (lambda ()
			     (eval-core (expanded->core invoke-code))))
	    (visit-code    (build-visit-code macro*))
	    (invoke-code   invoke-code))
	(install-library id name ver
			 imp* vis* inv* export-subst export-env
			 visit-proc invoke-proc
			 visit-code invoke-code
			 guard-code guard-req*
			 #t #;visible? filename)
	(values id name ver imp* vis* inv*
		invoke-code visit-code
		export-subst export-env
		guard-code guard-req*))))
   ((library-sexp filename)
    (expand-library library-sexp filename (lambda (ids ver) (values))))
   ((library-sexp)
    (expand-library library-sexp #f       (lambda (ids ver) (values))))))


(define (core-library-expander library-sexp verify-name)
  ;;Given a SYNTAX-MATCH expression argument representing a LIBRARY form
  ;;symbolic expression:
  ;;
  ;;   (library . _)
  ;;
  ;;parse  it  and  return  multiple  values  representing  the  library
  ;;contents.
  ;;
  ;;VERIFY-NAME must be a procedure  accepting 2 arguments and returning
  ;;unspecified values: the  first argument is a list of  symbols from a
  ;;library  name; the  second  argument  is null  or  a  list of  exact
  ;;integers representing the library  version.  VERIFY-NAME is meant to
  ;;perform some validation  upon the library name  components and raise
  ;;an exception if something is wrong; otherwise it should just return.
  ;;
  (receive (library-name* export-spec* import-spec* body*)
      (parse-library library-sexp)
    (receive (libname.ids libname.version)
	(parse-library-name library-name*)
      (verify-name libname.ids libname.version)
      (let ((stale-c (make-stale-collector)))
	(receive (import-spec* invoke-req* visit-req*
			       invoke-code visit-code export-subst export-env)
	    (parametrise ((stale-when-collector stale-c))
	      (library-body-expander export-spec* import-spec* body* #f))
	  (receive (guard-code guard-req*)
	      (stale-c)
	    (values libname.ids libname.version
		    import-spec* invoke-req* visit-req*
		    invoke-code visit-code export-subst
		    export-env guard-code guard-req*)))))))


(define (parse-library library-sexp)
  ;;Given  an ANNOTATION  struct  representing a  LIBRARY form  symbolic
  ;;expression, return 4 values:
  ;;
  ;;1. The  name part.  A SYNTAX-MATCH  expression argument representing
  ;;   parts of the library name.
  ;;
  ;;2.   The  export   specs.    A   SYNTAX-MATCH  expression   argument
  ;;   representing the exports specification.
  ;;
  ;;3.   The  import   specs.    A   SYNTAX-MATCH  expression   argument
  ;;   representing the imports specification.
  ;;
  ;;4.  The  body of  the library.   A SYNTAX-MATCH  expression argument
  ;;   representing the body of the library.
  ;;
  ;;This function performs no validation of the returned values, it just
  ;;validates the structure of the LIBRARY form.
  ;;
  (syntax-match library-sexp ()
    ((library (?name* ...)
       (export ?exp* ...)
       (import ?imp* ...)
       ?body* ...)
     (and (eq? (syntax->datum library) 'library)
	  (eq? (syntax->datum export)  'export)
	  (eq? (syntax->datum import)  'import))
     (values ?name* ?exp* ?imp* ?body*))
    (_
     (syntax-violation 'expander "malformed library" library-sexp))))


(define (parse-library-name libname)
  ;;Given  a SYNTAX-MATCH  expression  argument  LIBNAME representing  a
  ;;library name as defined by R6RS, return 2 values:
  ;;
  ;;1. A list of symbols representing the name identifiers.
  ;;
  ;;2. A list of fixnums representing the version of the library.
  ;;
  ;;Example:
  ;;
  ;;   (parse-library-name (foo bar (1 2 3)))
  ;;   => (foo bar) (1 2 3)
  ;;
  (define who 'expander)
  (receive (name* ver*)
      (let recur ((sexp libname))
	(syntax-match sexp ()
	  (((?vers* ...))
	   (for-all library-version-number? (map syntax->datum ?vers*)) ;this is the fender
	   (values '() (map syntax->datum ?vers*)))

	  ((?id . ?rest)
	   (symbol? (syntax->datum ?id)) ;this is the fender
	   (receive (name* vers*)
	       (recur ?rest)
	     (values (cons (syntax->datum ?id) name*) vers*)))

	  (()
	   (values '() '()))

	  (_
	   (syntax-violation who "invalid library name" libname))))
    (when (null? name*)
      (syntax-violation who "empty library name" libname))
    (values name* ver*)))


(module (parse-export-spec*)
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
  (define who 'export)

  (define (parse-export-spec* export-spec*)
    (define %synner
      (case-lambda
       ((message)
	(syntax-violation who message export-spec*))
       ((message subform)
	(syntax-violation who message export-spec* subform))))
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
	   (if (strict-r6rs)
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
	   (if (strict-r6rs)
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
	   (if (strict-r6rs)
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
	   (if (strict-r6rs)
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


(module (parse-import-spec*)
  ;;Given  a  list  of SYNTAX-MATCH  expression  arguments  representing
  ;;import specifications from  a LIBRARY form, as defined  by R6RS plus
  ;;Vicare extensions:
  ;;
  ;;1. Parse and validate the import specs.
  ;;
  ;;2. For libraries not yet loaded: load the selected library files and
  ;;    add  them  to  the  current  collector  function  referenced  by
  ;;   IMP-COLLECTOR.
  ;;
  ;;3. Apply to  visible binding names the  transformations described by
  ;;   the import spec.
  ;;
  ;;4. Check for name conflicts between imported bindings.
  ;;
  ;;Return  2  values  which can  be  used  to  build  a new  top  level
  ;;environment object and so a top rib:
  ;;
  ;;1.   A vector  of  symbols  representing the  visible  names of  the
  ;;   imported  bindings.
  ;;
  ;;2. A  list of "labels":  unique symbols associated to  the binding's
  ;;   entry in the lexical environment.
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
  ;;   #(z y q)		#(z$label x$label q$label)
  ;;
  ;;Imported bindings are referenced by "substs".  A "subst" is an alist
  ;;whose keys are "names" and whose values are "labels":
  ;;
  ;;* "Name"  is a symbol  representing the  public name of  an imported
  ;;  binding, the one we use to reference it in the code of a library.
  ;;
  ;;* "Label"  is a unique symbol  associated to the binding's  entry in
  ;;  the lexical environment.
  ;;
  (define who 'import)

  (define (parse-import-spec* import-spec*)
    (let loop ((import-spec*  import-spec*)
	       (subst.table   (make-eq-hashtable)))
      ;;SUBST.TABLE has subst names as  keys and subst labels as values.
      ;;It is used  to check for duplicate names  with different labels,
      ;;which is an error.  Example:
      ;;
      ;;   (import (rename (french)
      ;;                   (salut	ciao))	;ERROR!
      ;;           (rename (british)
      ;;                   (hello	ciao)))	;ERROR!
      ;;
      (if (null? import-spec*)
	  (hashtable-entries subst.table)
	  ;; (receive (names labels)
	  ;;     (hashtable-entries subst.table)
	  ;;   (debug-print who names labels)
	  ;;   (values names labels))
	(begin
	  (for-each (lambda (subst.entry)
		      (%add-subst-entry! subst.table subst.entry))
	    (%import-spec->subst (car import-spec*)))
	  (loop (cdr import-spec*) subst.table)))))

  (define-inline (%add-subst-entry! subst.table subst.entry)
    ;;Add  the  given  SUBST.ENTRY to  SUBST.TABLE;  return  unspecified
    ;;values.  Raise a syntax violation if SUBST.ENTRY has the same name
    ;;of an entry in SUBST.TABLE, but different label.
    ;;
    (let ((entry.name  (car subst.entry))
	  (entry.label (cdr subst.entry)))
      (cond ((hashtable-ref subst.table entry.name #f)
	     => (lambda (label)
		  (unless (eq? label entry.label)
		    (%error-two-import-with-different-bindings entry.name))))
	    (else
	     (hashtable-set! subst.table entry.name entry.label)))))

;;; --------------------------------------------------------------------

  (module (%import-spec->subst)

    (define-inline (%import-spec->subst import-spec)
      ;;Process the IMPORT-SPEC and return the corresponding subst.
      ;;
      ;;The IMPORT-SPEC is  parsed; the specified library  is loaded and
      ;;installed, if  not already  in the  library collection;  the raw
      ;;subst from the library definition  is processed according to the
      ;;rules in IMPORT-SPEC.
      ;;
      ;;If an  error is found, including  library version non-conforming
      ;;to the library reference, an exception is raised.
      ;;
      (syntax-match import-spec ()
	((?for ?import-set . ?import-levels)
	 ;;FIXME Here we should validate ?IMPORT-LEVELS even if it is no
	 ;;used by Vicare.  (Marco Maggi; Tue Apr 23, 2013)
	 (eq? (syntax->datum ?for) 'for)
	 (%import-set->subst ?import-set import-spec))

	(?import-set
	 (%import-set->subst ?import-set import-spec))))

    (define (%import-set->subst import-set import-spec)
      ;;Recursive  function.   Process  the IMPORT-SET  and  return  the
      ;;corresponding   subst.    IMPORT-SPEC   is   the   full   import
      ;;specification from the IMPORT clause: it is used for descriptive
      ;;error reporting.
      ;;
      (define (%recurse import-set)
	(%import-set->subst import-set import-spec))
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
	      (for-all id-stx? ?old-name*)
	      (for-all id-stx? ?new-name*))
	 (let ((subst       (%recurse ?import-set))
	       (?old-name*  (map syntax->datum ?old-name*))
	       (?new-name*  (map syntax->datum ?new-name*)))
	   ;;FIXME Rewrite this  to eliminate find* and  rem* and merge.
	   ;;(Abdulaziz Ghuloum)
	   (let ((old-label* (find* ?old-name* subst ?import-set)))
	     (let ((subst (rem* ?old-name* subst)))
	       ;;FIXME Make sure map is valid. (Abdulaziz Ghuloum)
	       (merge-substs (map cons ?new-name* old-label*) subst)))))

	((?except ?import-set ?sym* ...)
	 (and (eq? (syntax->datum ?except) 'except)
	      (for-all id-stx? ?sym*))
	 (let ((subst (%recurse ?import-set)))
	   (rem* (map syntax->datum ?sym*) subst)))

	((?only ?import-set ?name* ...)
	 (and (eq? (syntax->datum ?only) 'only)
	      (for-all id-stx? ?name*))
	 (let* ((subst  (%recurse ?import-set))
		(name*  (map syntax->datum ?name*))
		(name*  (remove-dups name*))
		(lab*   (find* name* subst ?import-set)))
	   (map cons name* lab*)))

	((?prefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?prefix) 'prefix)
	      (id-stx? ?prefix))
	 (let ((subst   (%recurse ?import-set))
	       (prefix  (symbol->string (syntax->datum ?the-prefix))))
	   (map (lambda (x)
		  (cons (string->symbol
			 (string-append prefix (symbol->string (car x))))
			(cdr x)))
	     subst)))

	((?deprefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?deprefix) 'deprefix)
	      (id-stx? ?the-prefix))
	 (if (strict-r6rs)
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
	      (id-stx? ?suffix))
	 (if (strict-r6rs)
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
	      (id-stx? ?the-suffix))
	 (if (strict-r6rs)
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

    (define (%import-library spec*)
      (receive (name version-conforms-to-reference?)
	  (parse-library-reference spec*)
	(when (null? name)
	  (%synner "empty library name" spec*))
	;;Search  for the  library first  in the  collection of  already
	;;installed libraires, then on  the file system.  If successful:
	;;LIB is an instance of LIBRARY struct.
	(let ((lib (find-library-by-name name)))
	  (unless lib
	    (%synner "cannot find library with required name" name))
	  (unless (version-conforms-to-reference? (library-version lib))
	    (%synner "library does not satisfy version specification" spec* lib))
	  ((imp-collector) lib)
	  (library-subst lib))))

    #| end of module: %IMPORT-SPEC->SUBST |# )

;;; --------------------------------------------------------------------

  (module (parse-library-reference)

    (define (parse-library-reference libref)
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
	   (id-stx? ?id)
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

    #| end of module: PARSE-LIBRARY-REFERENCE |# )

;;; --------------------------------------------------------------------

  (module (merge-substs)

    (define (merge-substs subst1 subst2)
      ;;Recursive function.  Given two substs: merge them and return the
      ;;result.
      ;;
      ;;Assume that SUBST1  has unique entries in itself  and SUBST2 has
      ;;unique entrie in  itself.  If an entry from SUBST1  has the name
      ;;name but different label from an entry in SUBST2: raise a syntax
      ;;error.
      ;;
      (if (null? subst1)
	  subst2
	(%insert-to-subst (car subst1)
			  (merge-substs (cdr subst1) subst2))))

    (define-inline (%insert-to-subst entry subst)
      ;;Given a subst  ENTRY and a SUBST: insert the  entry in the subst
      ;;if it is not already present  and return the result; else return
      ;;SUBST.
      ;;
      (let ((name  (car entry))
	    (label (cdr entry)))
	(cond ((assq name subst)
	       => (lambda (x)
		    (if (eq? (cdr x) label)
			;;Same name and same label: OK.
			subst
		      ;;Same name but different label: ERROR.
		      (%error-two-import-with-different-bindings name))))
	      (else
	       (cons entry subst)))))

    #| end of module: MERGE-SUBSTS |# )

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

  (define (id-stx? x)
    ;;Return true if X is an identifier.
    ;;
    (symbol? (syntax->datum x)))

;;; --------------------------------------------------------------------

  (define (%error-two-import-with-different-bindings name)
    (%synner "two imports with different bindings" name))

  (define %synner
    (case-lambda
     ((message form)
      (syntax-violation who message form))
     ((message form subform)
      (syntax-violation who message form subform))))

  #| end of module: PARSE-IMPORT-SPEC* |# )


(module (library-body-expander)
  ;;Both the R6RS  programs expander and the R6RS  library expander make
  ;;use of this module to expand the body forms.
  ;;
  ;;When  expanding  the  body  of a  library:  MAIN/EXPORT-SPEC*  is  a
  ;;SYNTAX-MATCH  input argument  representing a  set of  library export
  ;;specifications.    When   expanding   the   body   of   a   program:
  ;;MAIN/EXPORT-SPEC* is the symbol "main".
  ;;
  ;;IMPORT-SPEC* is a SYNTAX-MATCH input  argument representing a set of
  ;;library import specifications.
  ;;
  ;;BODY* is a SYNTAX-MATCH input argument representing the body forms.
  ;;
  ;;MIX? is  true when expanding  a program  and false when  expanding a
  ;;library; when  true mixing top-level definitions  and expressions is
  ;;fine.
  ;;
  ;;Return multiple values:
  ;;
  ;;1.  A  collector function  (see MAKE-COLLECTOR) holding  the LIBRARY
  ;;   structs representing the import specifications.
  ;;
  ;;2.   A collector  function (see  MAKE-COLLECTOR) holding...   invoke
  ;;   code...
  ;;
  ;;3.   A collector  function  (see  MAKE-COLLECTOR) holding...   visit
  ;;   code...
  ;;
  ;;4. ...
  ;;
  ;;5. ...
  ;;
  ;;6. ...
  ;;
  ;;7. ...
  ;;
  (define (library-body-expander main/export-spec* import-spec* body* mix?)
    (define itc (make-collector))
    (parametrise ((imp-collector      itc)
		  (top-level-context  #f))
      (receive (subst-names subst-labels)
	  (parse-import-spec* import-spec*)
	(let ((rib (make-top-rib subst-names subst-labels)))
	  (define (wrap x)
	    (make-<stx> x top-mark* (list rib) '()))
	  (let ((body*  (map wrap body*))
		(rtc    (make-collector))
		(vtc    (make-collector)))
	    (parametrise ((inv-collector  rtc)
			  (vis-collector  vtc))
	      (receive (init* lexenv.run lexenv.expand lex* rhs* internal-exp*)
		  (%chi-library-internal body* rib mix?)
		(receive (exp-name* exp-id*)
		    (parse-export-spec* (if (eq? main/export-spec* 'all)
					    (map wrap (top-marked-symbols rib))
					  (append (map wrap main/export-spec*)
						  internal-exp*)))
		  (seal-rib! rib)
		  (let* ((init*  (chi-expr* init* lexenv.run lexenv.expand))
			 (rhs*   (chi-rhs*  rhs*  lexenv.run lexenv.expand)))
		    (unseal-rib! rib)
		    (let ((loc*          (map gen-global lex*))
			  (export-subst  (make-export-subst exp-name* exp-id*)))
		      (define errstr
			"attempt to export mutated variable")
		      (receive (export-env global* macro*)
			  (make-export-env/macros lex* loc* lexenv.run)
			(unless (eq? main/export-spec* 'all)
			  (for-each
			      (lambda (s)
				(let ((name  (car s))
				      (label (cdr s)))
				  (let ((p (assq label export-env)))
				    (when p
				      (let ((b (cdr p)))
					(let ((type (car b)))
					  (when (eq? type 'mutable)
					    (syntax-violation 'export errstr name))))))))
			    export-subst))
			(let ((invoke-body
			       (build-library-letrec* no-source
						      mix?
						      lex* loc* rhs*
						      (if (null? init*)
							  (build-void)
							(build-sequence no-source init*))))
			      ;;(invoke-body
			      ;; (build-letrec* no-source lex* rhs*
			      ;;    (build-exports global* init*)))
			      (invoke-definitions
			       (map build-global-define (map cdr global*))))
			  (values (itc) (rtc) (vtc)
				  (build-sequence no-source
						  (append invoke-definitions
							  (list invoke-body)))
				  macro* export-subst export-env)))))))))))))

  (define-inline (%chi-library-internal e* rib mix?)
    (receive (e* lexenv.run lexenv.expand lex* rhs* mod** _kwd* exp*)
	(chi-body* e* '() '() '() '() '() '() '() rib mix? #t)
      (values (append (apply append (reverse mod**)) e*)
	      lexenv.run lexenv.expand (reverse lex*) (reverse rhs*) exp*)))

  #| end of module: LIBRARY-BODY-EXPANDER |# )


;;;; lexical environments
;;
;;A "lexical  environment" is  an alist managed  somewhat like  a stack;
;;each entry is a pair, list or improper list and represents a binding.
;;
;;While  the  expansion proceeds,  visiting  the  code in  breadth-first
;;order:  the lexical  environment  is updated  by  pushing new  binding
;;entries on the stack.
;;
;;An entry in the lexical environment alist has the following format:
;;
;;   (?label . (?binding-type . ?binding-value))
;;             |..............................| binding
;;
;;where ?LABEL is a gensym, ?BINDING-TYPE is a symbol, ?BINDING-VALUE is
;;a value whose format depends on the binding type.
;;
;;* A  binding representing a  lexical variable,  as created by  LET and
;;  similar syntaxes, has the format:
;;
;;     (lexical . (?lexvar . ?mutable))
;;
;;  where  "lexical"  is  the  symbol "lexical";  ?LEXVAR  is  a  symbol
;;  representing the  name of  the binding in  the core  language forms;
;;  ?MUTABLE is a boolean, true if this binding is mutable.
;;
;;* A binding representing a pattern variable, as created by SYNTAX-CASE
;;  and SYNTAX-RULES, has the format:
;;
;;     (syntax . (?name . ?level))
;;
;;  where:  "syntax"  is  the  symbol  "syntax";  ?NAME  is  the  symbol
;;  representing  the  name  of  the   pattern  variable;  ?LEVEL  is  a
;;  non-negative exact integer representing the ellipsis nesting level.
;;
;;  The SYNTAX-CASE patterns below will generate the entries:
;;
;;     ?a			->  (syntax . (?a . 0))
;;     (?a)			->  (syntax . (?a . 0))
;;     (((?a)))			->  (syntax . (?a . 0))
;;     (?a ...)			->  (syntax . (?a . 1))
;;     ((?a) ...)		->  (syntax . (?a . 1))
;;     ((((?a))) ...)		->  (syntax . (?a . 1))
;;     ((?a ...) ...)		->  (syntax . (?a . 2))
;;     (((?a ...) ...) ...)	->  (syntax . (?a . 3))
;;
;;* A  binding representing  a Vicare's struct  type descriptor  has the
;;  format:
;;
;;     ($rtd . #<type-descriptor-struct>)
;;
;;  where "$rtd" is the symbol "$rtd".
;;
;;*  A binding  representing an  R6RS's record  type descriptor  and the
;;  default record constructor descriptor has the format:
;;
;;     ($rtd . (?rtd-id ?rcd-id))
;;
;;  where  "$rtd" is  the symbol  "$rtd", ?RTD-ID  is the  identifier to
;;  which the record type descriptor is bound, ?RCD-ID is the identifier
;;  to which the default record constructor descriptor is bound.
;;
;;  Optionally 2 or 4 additional fields are present:
;;
;;     ($rtd . (?rtd-id ?rcd-id
;;              ?safe-accessors-alist ?safe-mutators-alist))
;;
;;     ($rtd . (?rtd-id ?rcd-id
;;              ?safe-accessors-alist ?safe-mutators-alist
;;              ?unsafe-accessors-alist ?unsafe-mutators-alist))
;;
;;  in which:
;;
;;  -  ?SAFE-ACCESSORS-ALIST   is  an  alist  whose   keys  are  symbols
;;  representing  all  the   field  names  and  whose   values  are  the
;;  identifiers bound to the corresponding safe field accessors.
;;
;;  -  ?SAFE-FIELD-MUTATORS   is  an   alist  whose  keys   are  symbols
;;  representing  the   mutable  field   names  and  whose   values  are
;;  identifiers bound to the corresponding safe field mutators.
;;
;;  -  ?UNSAFE-ACCESSORS-ALIST  is  an  alist  whose  keys  are  symbols
;;  representing  all  the   field  names  and  whose   values  are  the
;;  identifiers bound to the corresponding safe unfield accessors.
;;
;;  -  ?UNSAFE-FIELD-MUTATORS  is  an   alist  whose  keys  are  symbols
;;  representing  the   mutable  field   names  and  whose   values  are
;;  identifiers bound to the corresponding unsafe field mutators.
;;
;;* A binding representing a fluid syntax has the format:
;;
;;     ($fluid . ?label)
;;
;;  where ?LABEL is the gensym associated to the fluid syntax.
;;
;;* The following special binding represents an unbound label:
;;
;;     (displaced-lexical . #f)
;;
;;* The  following special  binding represents the  result of  a lexical
;;  environment query with invalid label value (not a symbol):
;;
;;     (displaced-lexical . ())
;;

;;Build and return a new binding.
;;
(define make-binding cons)

;;Given  a binding  return its  type: a  symbol.  Possible  symbols are:
;;"lexical".
;;
(define binding-type car)

;;Given a binding return  is value: a pair.  The car of  the pair is the
;;lexical  variable; the  cdr  is  a boolean,  true  if  the binding  is
;;mutable.
;;
(define binding-value cdr)

;;Generate a  unique symbol to  represent the name  of a binding  in the
;;core language forms.
;;
(define gensym-for-lexical-var %generate-unique-symbol)

;;Accessors for the pair value in a binding list.
;;
(define lexical-var car)
(define lexical-mutable? cdr)

;;Mutator for the mutable boolean in a pair value of a binding list.
;;
(define set-lexical-mutable! set-cdr!)

(define (add-lexical label lexvar lexenv)
  ;;Push a  new entry on the  lexical environment LEXENV and  return the
  ;;resulting lexical environment.
  ;;
  ;;LABEL must be a unique symbol identifying a binding.  LEXVAR must be
  ;;a symbol representing  the name of the binding in  the core language
  ;;forms.
  ;;
  (cons (cons* label 'lexical lexvar #f)
	lexenv))

(define (add-lexicals label* lexvar* lexenv)
  ;;Push multiple entries  to the lexical environment  LEXENV and return
  ;;the resulting lexical environment.
  ;;
  ;;LABEL*  must  be a  list  of  unique symbols  identifying  bindings.
  ;;LEXVAR* must  be a  list of  symbols representing  the names  of the
  ;;binding in the core language forms.
  ;;
  (if (null? label*)
      lexenv
    (add-lexicals ($cdr label*) ($cdr lexvar*)
		  (add-lexical ($car label*) ($car lexvar*) lexenv))))

;;; --------------------------------------------------------------------

(define (label->binding label lexenv)
  ;;Look  up the  symbol LABEL  in  the lexical  environment LEXENV  (an
  ;;alist) as well  as in the global environment.  If  an entry with key
  ;;LABEL is found:  return the value of the entry  which is the binding
  ;;value; if no matching entry is found, return the special binding:
  ;;
  ;;   (displaced-lexical . #f)
  ;;
  ;;Since all labels are unique,  it doesn't matter which environment we
  ;;consult first.  we lookup the  global environment first because it's
  ;;faster  (uses a  hash table)  while  the lexical  environment is  an
  ;;alist.
  ;;
  (define (%fluid-syntax-binding? binding)
    (and (pair? binding)
	 (eq? (binding-type binding) '$fluid)))
  (let ((binding (label->binding/no-fluids label lexenv)))
    (if (%fluid-syntax-binding? binding)
	;;Fluid syntax bindings (created by DEFINE-FLUID-SYNTAX) require
	;;reversed  logic.   We  have  to  look them  up  in  the  local
	;;environment first, and then in the global.
	(let ((label (binding-value binding)))
	  (cond ((assq label lexenv)
		 => cdr)
		(else
		 (label->binding/no-fluids label '()))))
      binding)))

(define (label->binding/no-fluids label lexenv)
  ;;Like LABEL->BINDING, but actually does the job.
  ;;
  (cond ((not (symbol? label))
	 '(displaced-lexical))

	;;Each label in the boot  image environment and in every library
	;;environment  is a  gensym in  which the  "value" field  of the
	;;symbol object memory blcok contains the associated binding.
	;;
	;;So  if we  have  a label  we  can check  if  it references  an
	;;imported binding  simply by checking its  "value" field.  This
	;;is what IMPORTED-LABEL->BINDING does.
	;;
	((imported-label->binding label)
	 => (lambda (b)
	      (cond ((and (pair? b)
			  (eq? (car b) '$core-rtd))
		     (cons '$rtd (map bless (cdr b))))
		    ((and (pair? b)
			  (eq? (car b) 'global-rtd))
		     (let ((lib (cadr b))
			   (loc (cddr b)))
		       (cons '$rtd (symbol-value loc))))
		    (else b))))

	;;Search the given lexical environment.
	;;
	((assq label lexenv)
	 => cdr)

	;;Search the interaction top-level environment, if any.
	;;
	((top-level-context)
	 => (lambda (env)
	      (cond ((assq label (interaction-env-locs env))
		     => (lambda (p)
			  ;;Build and  return a binding  representing an
			  ;;immutable lexical variable.
			  (cons* 'lexical (cdr p) #f)))
		    (else
		     '(displaced-lexical . #f)))))

	;;Unbound label.
	;;
	(else
	 '(displaced-lexical . #f))))


;;;; <RIB> type definition

;;A  <RIB> is  a  record constructed  at  every lexical  contour in  the
;;program to  hold informations about  the variables introduced  in that
;;contour; "lexical contours" are, for example, LET and similar syntaxes
;;that can introduce bindings.
;;
(define-record <rib>
  (sym*
		;List of symbols representing the original binding names
		;in the source  code.  If the <RIB> is  sealed: the list
		;is converted to a vector.
   mark**
		;List of  lists of marks.   If the <RIB> is  sealed: the
		;list is converted to a vector.
   label*
		;List  of symbols  representing substitution  labels for
		;bindings.   If  the  <RIB>   is  sealed:  the  list  is
		;converted to a vector.
   sealed/freq
		;False or  vector of  exact integers.  When  false: this
		;<RIB> is extensible, that  is new bindings can be added
		;to it.  When a vector: this <RIB> is selaed.
		;
		;See  below  the  code  section "sealing  ribs"  for  an
		;explanation of the frequency vector.
   ))

(define-inline (make-empty-rib)
  (make-<rib> '() '() '() #f))

(define (make-full-rib id* label*)
  ;;Build and  return a  new <RIB> record  taking the binding  names and
  ;;marks  from the  list  of syntax  objects  ID* and  the labels  from
  ;;LABEL*.
  ;;
  ;;It may be a good idea to seal this <RIB>.
  ;;
  (make-<rib> (map identifier->symbol id*)
	      (map <stx>-mark* id*)
	      label*
	      #f))

(define (make-top-rib name* label*)
  ;;A top <RIB> is constructed as follows: given a subst:
  ;;
  ;;   name* -> label*
  ;;
  ;;where NAME* is a vector of symbols and LABEL* is a vector of labels,
  ;;generate a <RIB> containing:
  ;;
  ;;* name* as the <RIB>-SYM*,
  ;;
  ;;* a list of TOP-MARK* as the <RIB>-MARK**,
  ;;
  ;;* label* as the <RIB>-LABEL*
  ;;
  ;;so, a name in  a top <RIB> maps to its label if  and only if its set
  ;;of marks is TOP-MARK*.
  ;;
  (define who 'make-top-rib)
  (let ((rib (make-empty-rib)))
    (vector-for-each
        (lambda (name label)
          (if (symbol? name)
	      (extend-rib! rib
			   (make-<stx> name top-mark* '() #;subst* '() #;ae* )
			   label #t)
            (assertion-violation who
	      "Vicare bug: expected symbol as binding name" name)))
      name* label*)
    rib))

(define (subst->rib subst)
  ;;Build and return a new <RIB> structure initialised with SUBST.
  ;;
  ;;A "subst"  is an alist whose  keys are "names" and  whose values are
  ;;"labels":
  ;;
  ;;* "Name"  is a symbol  representing the  public name of  an imported
  ;;  binding, the one we use to reference it in the code of a library.
  ;;
  ;;* "Label"  is a unique symbol  associated to the binding's  entry in
  ;;  the lexical environment.
  ;;
  (let ((rib (make-empty-rib)))
    ($set-<rib>-sym*!   rib (map car subst))
    ($set-<rib>-mark**! rib (map (lambda (x) top-mark*) subst))
    ($set-<rib>-label*! rib (map cdr subst))
    rib))


;;;; extending ribs
;;
;;A <RIB>  may be extensible, or sealed.   Adding an identifier-to-label
;;mapping  to an  extensible <RIB>  is  achieved by  performing all  the
;;following operations:
;;
;;* consing the identifier's name to the list of symbols SYM*;
;;
;;* consing  the identifier's list of  marks to the  <RIB>'s MARK**;
;;
;;* consing the label to the <RIB>'s LABEL*.
;;
;;For example, an empty extensible <RIB> has fields:
;;
;;  sym*   = ()
;;  mark** = ()
;;  label* = ()
;;
;;adding a binding to it with  name "ciao", marks ("m.0") and label "G0"
;;means mutating the fields to:
;;
;;  sym*   = (ciao)
;;  mark** = (("m.0"))
;;  label* = (G0)
;;
;;pushing the "binding tuple": ciao, ("m.0"), G0.
;;
;;Adding another binding with name  "hello", mark ("m.0") and label "G1"
;;means mutating the fields to:
;;
;;  sym*   = (hello ciao)
;;  mark** = (("m.0") ("m.0"))
;;  label* = (G1 G0)
;;
;;As further example, let's consider the form:
;;
;;  (lambda ()
;;    (define a 1)
;;    (define b 2)
;;    (list a b))
;;
;;when starting to process the LAMBDA syntax: a new <RIB> is created and
;;is  added to  the  metadata of  the  LAMBDA form;  when each  internal
;;definition is  encountered, a  new entry for  the identifier  is added
;;(via side effect) to the <RIB>:
;;
;;  sym*   = (b a)
;;  mark** = (("m.0") ("m.0"))
;;  label* = (G1 G0)
;;
;;Notice that the order in which  the binding tuples appear in the <RIB>
;;does not matter: two tuples are different when both the symbol and the
;;marks are  different and it is  an error to  add twice a tuple  to the
;;same <RIB>.
;;

(define (extend-rib! rib id label sd?)
  ;;Extend RIB.
  ;;
  (define (%find sym mark* sym* mark** label*)
    ;;We know  that the list  of symbols SYM*  has at least  one element
    ;;equal to SYM; iterate through  SYM*, MARK** and LABEL* looking for
    ;;a tuple having marks equal to  MARK* and return the tail of LABEL*
    ;;having the associated label as  car.  If such binding is not found
    ;;return false.
    ;;
    (and (pair? sym*)
	 (if (and (eq? sym (car sym*))
		  (same-marks? mark* (car mark**)))
	     label*
	   (%find sym mark* (cdr sym*) (cdr mark**) (cdr label*)))))
  (when (<rib>-sealed/freq rib)
    (assertion-violation 'extend-rib!
      "Vicare: internal error: attempt to extend sealed RIB" rib))
  (let ((sym   (identifier->symbol id))
	(mark* (<stx>-mark* id))
	(sym*  (<rib>-sym* rib)))
    (cond ((and (memq sym (<rib>-sym* rib))
		(%find sym mark* sym* (<rib>-mark** rib) (<rib>-label* rib)))
	   => (lambda (label*-tail)
		(unless (eq? label (car label*-tail))
		  (if (not sd?) ;(top-level-context)
		      ;;XXX override label
		      (set-car! label*-tail label)
		    ;;Signal an error if the identifier was already in
		    ;;the rib.
		    (syntax-violation 'expander "multiple definitions of identifier" id)))))
	  (else
	   (set-<rib>-sym*!   rib (cons sym sym*))
	   (set-<rib>-mark**! rib (cons mark* (<rib>-mark** rib)))
	   (set-<rib>-label*! rib (cons label (<rib>-label* rib)))))))


;;;; sealing ribs
;;
;;A non-empty  <RIB> can be sealed  once all bindings  are inserted.  To
;;seal a <RIB>, we convert the  lists SYM*, MARK** and LABEL* to vectors
;;and insert a frequency vector in the SEALED/FREQ field.  The frequency
;;vector is a Scheme vector of exact integers.
;;
;;The  frequency vector  is an  optimization  that allows  the <RIB>  to
;;reorganize itself by  bubbling frequently used mappings to  the top of
;;the <RIB>.   This is possible because  the order in  which the binding
;;tuples appear in a <RIB> does not matter.
;;
;;The vector  is maintained in non-descending order  and an identifier's
;;entry in the <RIB> is incremented at every access.  If an identifier's
;;frequency  exceeds the  preceeding one,  the identifier's  position is
;;promoted  to the  top of  its  class (or  the bottom  of the  previous
;;class).
;;

(define (seal-rib! rib)
  (let ((sym* (<rib>-sym* rib)))
    (unless (null? sym*) ;only seal if RIB is not empty
      (let ((sym* (list->vector sym*)))
	(set-<rib>-sym*!        rib sym*)
	(set-<rib>-mark**!      rib (list->vector (<rib>-mark** rib)))
	(set-<rib>-label*!      rib (list->vector (<rib>-label* rib)))
	(set-<rib>-sealed/freq! rib (make-vector (vector-length sym*) 0))))))

(define (unseal-rib! rib)
  (when (<rib>-sealed/freq rib)
    (set-<rib>-sealed/freq! rib #f)
    (set-<rib>-sym*!        rib (vector->list (<rib>-sym*   rib)))
    (set-<rib>-mark**!      rib (vector->list (<rib>-mark** rib)))
    (set-<rib>-label*!      rib (vector->list (<rib>-label* rib)))))

(define (increment-rib-frequency! rib idx)
  (let* ((freq* (<rib>-sealed/freq rib))
	 (freq  (vector-ref freq* idx))
	 (i     (let loop ((i idx))
		  (if (zero? i)
		      0
		    (let ((j (- i 1)))
		      (if (= freq (vector-ref freq* j))
			  (loop j)
			i))))))
    (vector-set! freq* i (+ freq 1))
    (unless (= i idx)
      (let ((sym*   (<rib>-sym*   rib))
	    (mark** (<rib>-mark** rib))
	    (label* (<rib>-label* rib)))
	(let-syntax ((%vector-swap (syntax-rules ()
				     ((_ ?vec ?idx1 ?idx2)
				      (let ((V (vector-ref ?vec ?idx1)))
					(vector-set! ?vec ?idx1 (vector-ref ?vec ?idx2))
					(vector-set! ?vec ?idx2 V))))))
	  (%vector-swap sym*   idx i)
	  (%vector-swap mark** idx i)
	  (%vector-swap label* idx i))))))


;;;; syntax object type definition

(define-record <stx>
  (expr
   mark*
   subst*
   ae*
   )
  (lambda (S port subwriter) ;record printer function
    (define-inline (%display thing)
      (display thing port))
    (define-inline (%write thing)
      (write thing port))
    (define-inline (%pretty-print thing)
      (pretty-print* thing port 0 #f))
    (%display "#<syntax")
    (%display " expr=")		(%pretty-print (syntax->datum S))
    (%display " mark*=")	(%pretty-print (<stx>-mark* S))
    (let ((expr (<stx>-expr S)))
      (when (annotation? expr)
	(let ((pos (annotation-textual-position expr)))
	  (when (source-position-condition? pos)
	    (%display " line=")		(%display (source-position-line    pos))
	    (%display " column=")	(%display (source-position-column  pos))
	    (%display " source=")	(%display (source-position-port-id pos))))))
    (%display ">")))


;;;; marks

;;The body  of a library, when it  is first processed, gets  this set of
;;marks...
(define top-mark* '(top))

;;... consequently, every syntax object that  has a TOP in its marks set
;;was present in the program source.
(define-inline (top-marked? m*)
  (memq 'top m*))

(define (top-marked-symbols rib)
  ;;Scan the <RIB> RIB and return a list of symbols representing binding
  ;;names and having the top mark.
  ;;
  (receive (sym* mark**)
      ;;If RIB is sealed the fields  hold vectors, else they hold lists;
      ;;we want lists here.
      (let ((sym*   (<rib>-sym*   rib))
	    (mark** (<rib>-mark** rib)))
	(if (<rib>-sealed/freq rib)
	    (values (vector->list sym*)
		    (vector->list mark**))
	  (values sym* mark**)))
    (let recur ((sym*   sym*)
		(mark** mark**))
      (cond ((null? sym*)
	     '())
	    ((equal? (car mark**) top-mark*)
	     (cons (car sym*)
		   (recur (cdr sym*) (cdr mark**))))
	    (else
	     (recur (cdr sym*) (cdr mark**)))))))


;;;; stuff about labels
;;
;;Labels are gensyms.  After the expansion every binding has a gensym as
;;name, so that its name is unique in the whole program.  For example:
;;
;;  (let ((a 1))
;;    (define-syntax b (identifier-syntax a))
;;    (let ((a 2))
;;      (list a b)))
;;
;;is converted to:
;;
;;  (let ((G1 1))
;;    (let ((G2 2))
;;      (G0 G1 G2)))
;;
;;with the following table of substitutions:
;;
;;    name  | label
;;  --------+------
;;  list    |  G0
;;  outer a |  G1
;;  inner a |  G2
;;

(define top-level-context
  (make-parameter #f))

;;Used to generate global names (e.g. locations for library exports).
;;
(define gen-global %generate-unique-symbol)

(define (gensym-for-label _)
  ;;Every identifier in the program will have a label associated with it
  ;;in its substitution; this function generates such labels.
  ;;
  ;;The  labels  have to  have  read/write  EQ?   invariance to  support
  ;;separate compilation (when we write the expanded symbolic expression
  ;;to a  file and  then read it  back, the  labels must not  change and
  ;;still be globally unique).
  ;;
  (gensym))

(module (gen-define-label+loc
	 gen-define-label)

  (define (gen-define-label+loc id rib sd?)
    (if sd?
	(values (gensym) (gensym-for-lexical-var id))
      (let* ((env   (top-level-context))
	     (label (%gen-top-level-label id rib))
	     (locs  (interaction-env-locs env)))
	(values label
		(cond ((assq label locs)
		       => cdr)
		      (else
		       (let ((loc (gensym-for-lexical-var id)))
			 (set-interaction-env-locs! env (cons (cons label loc) locs))
			 loc)))))))

  (define (gen-define-label id rib sd?)
    (if sd?
	(gensym)
      (%gen-top-level-label id rib)))

  (define (%gen-top-level-label id rib)
    (let ((sym   (identifier->symbol id))
	  (mark* (<stx>-mark*        id))
	  (sym*  (<rib>-sym*         rib)))
      (cond ((and (memq sym (<rib>-sym* rib))
		  (%find sym mark* sym* (<rib>-mark** rib) (<rib>-label* rib)))
	     => (lambda (label)
		  ;;If we  are here  RIB contains a  binding for  ID and
		  ;;LABEL is its label.
		  ;;
		  ;;If  the symbol  LABEL is  associated to  an imported
		  ;;binding: the data  structure implementing the symbol
		  ;;object holds  informations about  the binding  in an
		  ;;internal field; else such field is set to false.
		  (if (imported-label->binding label)
		      ;;Create new label to shadow imported binding.
		      (gensym)
		    ;;Recycle old label.
		    label)))
	    (else
	     ;;Create a new label for a new binding.
	     (gensym)))))

  (define (%find sym mark* sym* mark** label*)
    ;;We know  that the list  of symbols SYM*  has at least  one element
    ;;equal to SYM; iterate through  SYM*, MARK** and LABEL* looking for
    ;;a  tuple having  marks equal  to MARK*  and return  the associated
    ;;label.  If such binding is not found return false.
    ;;
    (and (pair? sym*)
	 (if (and (eq? sym (car sym*))
		  (same-marks? mark* (car mark**)))
	     (car label*)
	   (%find sym mark* (cdr sym*) (cdr mark**) (cdr label*)))))

  #| end of module |# )


;;;; syntax objects handling
;;
;;First, let's  look at identifiers,  since they're the real  reason why
;;syntax objects are here to begin  with.  An identifier is an STX whose
;;EXPR is a symbol; in addition to the symbol naming the identifier, the
;;identifer has a  list of marks and a list  of substitutions.
;;
;;The idea  is that to get  the label of  an identifier, we look  up the
;;identifier's substitutions for  a mapping with the same  name and same
;;marks (see SAME-MARKS? below).
;;

(define (datum->stx id datum)
  ;;Since all the identifier->label bindings are encapsulated within the
  ;;identifier, converting a datum to a syntax object (non-hygienically)
  ;;is  done simply  by creating  an  STX that  has the  same marks  and
  ;;substitutions as the identifier.
  ;;
  (make-<stx> datum
	      (<stx>-mark*  id)
	      (<stx>-subst* id)
	      (<stx>-ae*    id)))

(define (datum->syntax id datum)
  (if (identifier? id)
      (datum->stx id datum)
    (assertion-violation 'datum->syntax
      "expected identifier as context syntax object" id)))

(define (syntax->datum S)
  (strip S '()))

(define (mkstx stx/expr mark* subst* ae*)
  ;;This is the proper constructor for wrapped syntax objects.
  ;;
  ;;STX/EXPR can be a raw symbolic expression, an instance of <STX> or a
  ;;wrapped syntax object.  MARK* is a  list of marks.  SUBST* is a list
  ;;of substs.
  ;;
  ;;AE* == annotated expressions???
  ;;
  ;;When STX/EXPR is a raw symbolic  expression: just build and return a
  ;;new syntax  object with the  lexical context described by  the given
  ;;arguments.
  ;;
  ;;When STX/EXPR is a syntax object:  join the wraps from STX/EXPR with
  ;;given wraps, making sure that marks and anti-marks and corresponding
  ;;shifts cancel properly.
  ;;
  (if (and (<stx>? stx/expr)
	   (not (top-marked? mark*)))
      (receive (mark* subst* ae*)
	  (join-wraps mark* subst* ae* stx/expr)
	(make-<stx> (<stx>-expr stx/expr) mark* subst* ae*))
    (make-<stx> stx/expr mark* subst* ae*)))

(define (bless x)
  ;;Given a raw  symbolic expression, a single syntax  object, a wrapped
  ;;syntax  object, an  unwrapped syntax  object or  a partly  unwrapped
  ;;syntax  object X:  return a  syntax object  representing the  input,
  ;;possibly X itself.
  ;;
  ;;When X  is a symbolic  expression or a (partially)  unwrapped syntax
  ;;object: raw  symbols in X  are considered references to  bindings in
  ;;the core  language: they are  converted to identifiers  having empty
  ;;lexical contexts.
  ;;
  (mkstx (let recur ((x x))
	   (cond ((<stx>? x)
		  x)
		 ((pair? x)
		  (cons (recur (car x)) (recur (cdr x))))
		 ((symbol? x)
		  (scheme-stx x))
		 ((vector? x)
		  (list->vector (map recur (vector->list x))))
		 ;;If we are here X is a self-evaluating datum.
		 (else x)))
	 '() #;mark* '() #;subst* '() #;ae* ))


;;;; syntax objects and marks

;;A syntax  object may be wrapped  or unwrapped, so what  does that mean
;;exactly?
;;
;;A "wrapped syntax object" is just  a way of saying it's an STX record.
;;All identifiers are  STX records (with a symbol  in their EXPR field);
;;other objects such  as pairs and vectors may  be wrapped or unwrapped.
;;A wrapped pair is an STX whose EXPR is a pair.  An unwrapped pair is a
;;pair whose car  and cdr fields are themselves  syntax objects (wrapped
;;or unwrapped).
;;
;;We always  maintain the  invariant that we  do not double  wrap syntax
;;objects.  The  only way  to get a  doubly-wrapped syntax object  is by
;;doing DATUM->STX  (above) where the  datum is itself a  wrapped syntax
;;object (R6RS  may not even  consider wrapped syntax objects  as datum,
;;but let's not worry now).
;;
;;Syntax objects  have, in  addition to the  EXPR, a  substitution field
;;SUBST*: it is a list where each  element is either a RIB or the symbol
;;"shift".  Normally,  a new  RIB is  added to an  STX at  every lexical
;;contour of the program in  order to capture the bindings introduced in
;;that contour.
;;
;;The MARK* field of an STX is  a list of marks; each of these marks can
;;be  either  a  generated mark  or  an  antimark.   Two marks  must  be
;;EQ?-comparable, so we use a string of one char (we assume that strings
;;are mutable in the underlying Scheme implementation).

(define gen-mark
  ;;Generate a new unique mark.  We want a new string for every function
  ;;call.
  string)
;;The version below is useful for debugging.
;;
;; (define gen-mark
;;   (let ((i 0))
;;     (lambda ()
;;       (set! i (+ i 1))
;;       (string-append "m." (number->string i)))))

;;We use #f as the anti-mark.
(define anti-mark #f)
(define anti-mark? not)

;;So, what's an anti-mark and why is it there?
;;
;;The theory goes like this: when a macro call is encountered, the input
;;stx to the  macro transformer gets an extra  anti-mark, and the output
;;of the  transformer gets a fresh  mark.  When a mark  collides with an
;;anti-mark, they cancel one another.   Therefore, any part of the input
;;transformer that gets copied to  the output would have a mark followed
;;immediately by an  anti-mark, resulting in the same  syntax object (no
;;extra marks).  Parts of the output  that were not present in the input
;;(e.g. inserted by the macro  transformer) would have no anti-mark and,
;;therefore, the mark would stick to them.
;;
;;Every time  a mark is pushed  to an <stx>-mark* list,  a corresponding
;;'shift  is pushed  to the  <stx>-subst* list.   Every time  a mark  is
;;cancelled  by   an  anti-mark,  the  corresponding   shifts  are  also
;;cancelled.

;;The procedure join-wraps,  here, is used to compute the  new mark* and
;;subst* that would  result when the m1*  and s1* are added  to an stx's
;;mark* and subst*.
;;
;;The only tricky part here is that  e may have an anti-mark that should
;;cancel with the last mark in m1*.  So, if:
;;
;;  m1* = (mx* ... mx)
;;  m2* = (#f my* ...)
;;
;;then the resulting marks should be:
;;
;;  (mx* ... my* ...)
;;
;;since mx  would cancel with the  anti-mark.  The substs would  have to
;;also cancel since:
;;
;;    s1* = (sx* ... sx)
;;    s2* = (sy sy* ...)
;;
;;then the resulting substs should be:
;;
;;    (sx* ... sy* ...)
;;
;;Notice that both SX and SY would be shift marks.
;;
;;All   this  work   is  performed   by  the   functions  ADD-MARK   and
;;DO-MACRO-CALL.
;;

(module (join-wraps)

  (define (join-wraps mark1* subst1* ae1* stx2)
    (let ((mark2*   ($<stx>-mark*  stx2))
	  (subst2*  ($<stx>-subst* stx2))
	  (ae2*     ($<stx>-ae*    stx2)))
      ;;If the first item in mark2* is an anti-mark...
      (if (and (not (null? mark1*))
	       (not (null? mark2*))
	       (anti-mark? ($car mark2*)))
	  ;;...cancel mark, anti-mark, and corresponding shifts.
	  (values (%append-cancel-facing mark1*  mark2*)
		  (%append-cancel-facing subst1* subst2*)
		  (%merge-ae* ae1* ae2*))
	;;..else no cancellation takes place.
	(values (append mark1*  mark2*)
		(append subst1* subst2*)
		(%merge-ae* ae1* ae2*)))))

  (define (%merge-ae* ls1 ls2)
    (if (and (pair? ls1)
	     (pair? ls2)
	     (not ($car ls2)))
	(%append-cancel-facing ls1 ls2)
      (append ls1 ls2)))

  (define (%append-cancel-facing ls1 ls2)
    ;;Given two non-empty lists: append them discarding the last item in
    ;;LS1 and the first item in LS2.  Examples:
    ;;
    ;;   (%append-cancel-facing '(1 2 3) '(4 5 6))	=> (1 2 5 6)
    ;;   (%append-cancel-facing '(1)     '(2 3 4))	=> (3 4)
    ;;   (%append-cancel-facing '(1)     '(2))		=> ()
    ;;
    (let recur ((x   ($car ls1))
		(ls1 ($cdr ls1)))
      (if (null? ls1)
	  ($cdr ls2)
	(cons x (recur ($car ls1) ($cdr ls1))))))

  #| end of module: JOIN-WRAPS |# )

(define (same-marks? x y)
  ;;Two lists  of marks are  considered the same  if they have  the same
  ;;length and the corresponding marks on each are EQ?.
  ;;
  (or (and (null? x) (null? y)) ;(eq? x y)
      (and (pair? x) (pair? y)
	   (eq? ($car x) ($car y))
	   (same-marks? ($cdr x) ($cdr y)))))


(define (push-lexical-contour rib stx/expr)
  ;;Add a rib to a syntax  object or expression and return the resulting
  ;;syntax object.  This  procedure introduces a lexical  contour in the
  ;;context of the given syntax object or expression.
  ;;
  ;;RIB must be an instance of <RIB>.
  ;;
  ;;STX/EXPR can be a raw symbolic expression, an instance of <STX> or a
  ;;wrapped syntax object.
  ;;
  ;;This function prepares  a computation that will  be lazily performed
  ;;later; the RIB will be pushed on the stack of substitutions in every
  ;;identifier in the fully unwrapped returned syntax object.
  ;;
  (mkstx stx/expr '() #;mark* (list rib) '() #;ae* ))

(define (add-mark mark subst expr ae)
  ;;Build and return  a new syntax object wrapping  EXPR and having MARK
  ;;pushed on its list of marks.
  ;;
  ;;SUBST can be #f or a list of substitutions.
  ;;
  (define (merge-ae* ls1 ls2)
    ;;Append LS1 and LS2 and return the result; if the car or LS2 is #f:
    ;;append LS1 and (cdr LS2).
    ;;
    ;;   (merge-ae* '(a b c) '(d  e f))   => (a b c d e f)
    ;;   (merge-ae* '(a b c) '(#f e f))   => (a b c e f)
    ;;
    (if (and (pair? ls1)
	     (pair? ls2)
	     (not (car ls2)))
	(cancel ls1 ls2)
      (append ls1 ls2)))
  (define (cancel ls1 ls2)
    ;;Expect LS1 to be a proper list  of one or more elements and LS2 to
    ;;be a proper  list of one or more elements.  Append  the cdr of LS2
    ;;to LS1 and return the result:
    ;;
    ;;   (cancel '(a b c) '(d e f))
    ;;   => (a b c e f)
    ;;
    ;;This function is like:
    ;;
    ;;   (append ls1 (cdr ls2))
    ;;
    ;;we just hope to be a bit more efficient.
    ;;
    (let recur ((A (car ls1))
		(D (cdr ls1)))
      (if (null? D)
	  (cdr ls2)
	(cons A (recur (car D) (cdr D))))))
  (define (f sub-expr mark subst1* ae*)
    (cond ((pair? sub-expr)
	   (let ((a (f (car sub-expr) mark subst1* ae*))
		 (d (f (cdr sub-expr) mark subst1* ae*)))
	     (if (eq? a d)
		 sub-expr
	       (cons a d))))
	  ((vector? sub-expr)
	   (let* ((ls1 (vector->list sub-expr))
		  (ls2 (map (lambda (x)
			      (f x mark subst1* ae*))
			 ls1)))
	     (if (for-all eq? ls1 ls2)
		 sub-expr
	       (list->vector ls2))))
	  ((<stx>? sub-expr)
	   (let ((mark*   (<stx>-mark*  sub-expr))
		 (subst2* (<stx>-subst* sub-expr)))
	     (cond ((null? mark*)
		    (f (<stx>-expr sub-expr)
		       mark
		       (append subst1* subst2*)
		       (merge-ae* ae* (<stx>-ae* sub-expr))))
		   ((eq? (car mark*) anti-mark)
		    (make-<stx> (<stx>-expr sub-expr) (cdr mark*)
				(cdr (append subst1* subst2*))
				(merge-ae* ae* (<stx>-ae* sub-expr))))
		   (else
		    (make-<stx> (<stx>-expr sub-expr)
				(cons mark mark*)
				(let ((s* (cons 'shift (append subst1* subst2*))))
				  (if subst
				      (cons subst s*)
				    s*))
				(merge-ae* ae* (<stx>-ae* sub-expr)))))))
	  ((symbol? sub-expr)
	   (syntax-violation #f
	     "raw symbol encountered in output of macro"
	     expr sub-expr))
	  (else
	   (make-<stx> sub-expr (list mark) subst1* ae*))))
  (mkstx (f expr mark '() '()) '() '() (list ae)))


;;;; deconstructors and predicates for syntax objects

(define (syntax-kind? x pred?)
  (cond ((<stx>? x)
	 (syntax-kind? (<stx>-expr x) pred?))
	((annotation? x)
	 (syntax-kind? (annotation-expression x) pred?))
	(else
	 (pred? x))))

(define (syntax-vector->list x)
  (cond ((<stx>? x)
	 (let ((ls (syntax-vector->list (<stx>-expr x)))
	       (m* (<stx>-mark* x))
	       (s* (<stx>-subst* x))
	       (ae* (<stx>-ae* x)))
	   (map (lambda (x)
		  (mkstx x m* s* ae*))
	     ls)))
	((annotation? x)
	 (syntax-vector->list (annotation-expression x)))
	((vector? x)
	 (vector->list x))
	(else
	 (assertion-violation 'syntax-vector->list "BUG: not a syntax vector" x))))

(define (syntax-pair? x)
  (syntax-kind? x pair?))

(define (syntax-vector? x)
  (syntax-kind? x vector?))

(define (syntax-null? x)
  (syntax-kind? x null?))

(define (syntax-list? x)
  ;;FIXME Should terminate on cyclic input.  (Abdulaziz Ghuloum)
  (or (syntax-null? x)
      (and (syntax-pair? x)
	   (syntax-list? (syntax-cdr x)))))

(define (syntax-car x)
  (cond ((<stx>? x)
	 (mkstx (syntax-car ($<stx>-expr x))
		($<stx>-mark*  x)
		($<stx>-subst* x)
		($<stx>-ae*    x)))
	((annotation? x)
	 (syntax-car (annotation-expression x)))
	((pair? x)
	 ($car x))
	(else
	 (assertion-violation 'syntax-car "BUG: not a pair" x))))

(define (syntax-cdr x)
  (cond ((<stx>? x)
	 (mkstx (syntax-cdr ($<stx>-expr x))
		($<stx>-mark*  x)
		($<stx>-subst* x)
		($<stx>-ae*    x)))
	((annotation? x)
	 (syntax-cdr (annotation-expression x)))
	((pair? x)
	 ($cdr x))
	(else
	 (assertion-violation 'syntax-cdr "BUG: not a pair" x))))

(define (syntax->list x)
  (cond ((syntax-pair? x)
	 (cons (syntax-car x)
	       (syntax->list (syntax-cdr x))))
	((syntax-null? x)
	 '())
	(else
	 (assertion-violation 'syntax->list "BUG: invalid argument" x))))

;;; --------------------------------------------------------------------

(define (identifier? x)
  ;;Return true if X is an  identifier: a syntax object whose expression
  ;;is a symbol.
  ;;
  (and (<stx>? x)
       (let ((expr ($<stx>-expr x)))
	 (symbol? (if (annotation? expr)
		      (annotation-stripped expr)
		    expr)))))

(define (identifier->symbol x)
  ;;Given an identifier return its symbol expression.
  ;;
  (define who 'identifier->symbol)
  (define (%error)
    (assertion-violation who "Vicare bug: expected identifier as argument" x))
  (unless (<stx>? x)
    (%error))
  (let* ((expr ($<stx>-expr x))
	 (sym  (if (annotation? expr)
		   (annotation-stripped expr)
		 expr)))
    (if (symbol? sym)
	sym
      (%error))))


;;;; utilities for identifiers

(define (bound-id=? id1 id2)
  ;;Two identifiers  are BOUND-ID=? if they  have the same name  and the
  ;;same set of marks.
  ;;
  (and (eq? (identifier->symbol id1) (identifier->symbol id2))
       (same-marks? ($<stx>-mark* id1) ($<stx>-mark* id2))))

(define (free-id=? id1 id2)
  ;;Two identifiers are  FREE-ID=? if either both are bound  to the same
  ;;label or if both are unbound and they have the same name.
  ;;
  (let ((t1 (id->label id1))
	(t2 (id->label id2)))
    (if (or t1 t2)
	(eq? t1 t2)
      (eq? (identifier->symbol id1) (identifier->symbol id2)))))

(define (valid-bound-ids? id*)
  ;;Given a list return #t if it  is made of identifers none of which is
  ;;BOUND-ID=? to another; else return #f.
  ;;
  ;;This function is called to validate  both list of LAMBDA formals and
  ;;list of LET binding identifiers.  The only guarantee about the input
  ;;is that it is a list.
  ;;
  (and (for-all identifier? id*)
       (distinct-bound-ids? id*)))

(define (distinct-bound-ids? id*)
  ;;Given a list of identifiers: return #t if none of the identifiers is
  ;;BOUND-ID=? to another; else return #f.
  ;;
  (or (null? id*)
      (and (not (bound-id-member? ($car id*) ($cdr id*)))
	   (distinct-bound-ids? ($cdr id*)))))

(define (bound-id-member? id id*)
  ;;Given an identifier  ID and a list of identifiers  ID*: return #t if
  ;;ID is BOUND-ID=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (bound-id=? id ($car id*))
	   (bound-id-member? id ($cdr id*)))))


(define (self-evaluating? x)
  (or (number?		x)
      (string?		x)
      (char?		x)
      (boolean?		x)
      (bytevector?	x)
      (keyword?		x)
      (would-block-object? x)))

(module (strip)
  ;;STRIP is used  to remove the wrap  of a syntax object.   It takes an
  ;;stx's expr  and marks.  If  the marks  contain a top-mark,  then the
  ;;expr is returned.
  ;;
  (define (strip x m*)
    (if (top-marked? m*)
	(if (or (annotation? x)
		(and (pair? x)
		     (annotation? ($car x)))
		(and (vector? x)
		     (> ($vector-length x) 0)
		     (annotation? ($vector-ref x 0))))
	    ;;TODO Ask Kent  why this is a  sufficient test.  (Abdulaziz
	    ;;Ghuloum)
	    (strip-annotations x)
	  x)
      (let f ((x x))
	(cond ((<stx>? x)
	       (strip ($<stx>-expr x) ($<stx>-mark* x)))
	      ((annotation? x)
	       (annotation-stripped x))
	      ((pair? x)
	       (let ((a (f ($car x)))
		     (d (f ($cdr x))))
		 (if (and (eq? a ($car x))
			  (eq? d ($cdr x)))
		     x
		   (cons a d))))
	      ((vector? x)
	       (let* ((old (vector->list x))
		      (new (map f old)))
		 (if (for-all eq? old new)
		     x
		   (list->vector new))))
	      (else x)))))

  (define (strip-annotations x)
    (cond ((pair? x)
	   (cons (strip-annotations ($car x))
		 (strip-annotations ($cdr x))))
	  ((vector? x)
	   (vector-map strip-annotations x))
	  ((annotation? x)
	   (annotation-stripped x))
	  (else x)))

  #| end of module: STRIP |# )


;;;; identifiers and labels

(module (id->label)

  (define (id->label id)
    ;;Given the identifier  ID search its substs for  a label associated
    ;;with the same sym and marks.  If found return the symbol being the
    ;;label, else return false.
    ;;
    (let ((sym (identifier->symbol id)))
      (let search ((subst* ($<stx>-subst* id))
		   (mark*  ($<stx>-mark*  id)))
	(cond ((null? subst*)
	       #f)
	      ((eq? ($car subst*) 'shift)
	       ;;A shift is inserted when a  mark is added.  So, we search
	       ;;the rest of the substitution without the mark.
	       (search ($cdr subst*) ($cdr mark*)))
	      (else
	       (let ((rib ($car subst*)))
		 (define (next-search)
		   (search ($cdr subst*) mark*))
		 (if ($<rib>-sealed/freq rib)
		     (%search-in-sealed-rib rib sym mark* next-search)
		   (%search-in-rib rib sym mark* next-search))))))))

  (define-inline (%search-in-sealed-rib rib sym mark* next-search)
    (define sym* ($<rib>-sym* rib))
    (let loop ((i       0)
	       (rib.len ($vector-length sym*)))
      (cond (($fx= i rib.len)
	     (next-search))
	    ((and (eq? ($vector-ref sym* i) sym)
		  (same-marks? mark* ($vector-ref ($<rib>-mark** rib) i)))
	     (let ((label ($vector-ref ($<rib>-label* rib) i)))
	       (increment-rib-frequency! rib i)
	       label))
	    (else
	     (loop ($fxadd1 i) rib.len)))))

  (define-inline (%search-in-rib rib sym mark* next-search)
    (let loop ((sym*    ($<rib>-sym*   rib))
	       (mark**  ($<rib>-mark** rib))
	       (label*  ($<rib>-label* rib)))
      (cond ((null? sym*)
	     (next-search))
	    ((and (eq? ($car sym*) sym)
		  (same-marks? ($car mark**) mark*))
	     ($car label*))
	    (else
	     (loop ($cdr sym*) ($cdr mark**) ($cdr label*))))))

  #| end of module: ID->LABEL |# )

(define (id->label/intern id)
  (or (id->label id)
      (cond ((top-level-context)
	     => (lambda (env)
		  ;;Fabricate binding.
		  (let ((rib (interaction-env-rib env)))
		    (receive (lab _loc)
			(gen-define-label+loc id rib #f)
		      ;;FIXME (Abdulaziz Ghuloum)
		      (extend-rib! rib id lab #t)
		      lab))))
	    (else #f))))


(define (syntax-type e r)
  ;;The type of an expression is determined by two things:
  ;;
  ;;- The shape of the expression (identifier, pair, or datum).
  ;;
  ;;- The binding of  the identifier (for id-stx) or the  type of car of
  ;;  the pair.
  ;;
  (cond ((identifier? e)
	 (let ((id e))
	   (let* ((label  (id->label/intern id))
		  (b      (label->binding label r))
		  (type   (binding-type b)))
	     (unless label ;;fail early
	       (%raise-unbound-error #f id id))
	     (case type
	       ((lexical core-prim macro macro! global local-macro
			 local-macro! global-macro global-macro!
			 displaced-lexical syntax import export $module
			 $core-rtd library mutable ctv local-ctv global-ctv)
		(values type (binding-value b) id))
	       (else
		(values 'other #f #f))))))
	((syntax-pair? e)
	 (let ((id (syntax-car e)))
	   (if (identifier? id)
	       (let* ((label  (id->label/intern id))
		      (b      (label->binding label r))
		      (type   (binding-type b)))
		 (unless label ;;fail early
		   (%raise-unbound-error #f id id))
		 (case type
		   ((define define-syntax core-macro begin macro
		      macro! local-macro local-macro! global-macro
		      global-macro! module library set! let-syntax
		      letrec-syntax import export $core-rtd
		      ctv local-ctv global-ctv stale-when
		      define-fluid-syntax)
		    (values type (binding-value b) id))
		   (else
		    (values 'call #f #f))))
	     (values 'call #f #f))))
	(else
	 (let ((d (syntax->datum e)))
	   (if (self-evaluating? d)
	       (values 'constant d #f)
	     (values 'other #f #f))))))

(define (sanitize-binding x src)
  ;;When  the rhs  of a  syntax definition  is evaluated,  it should  be
  ;;either a procedure, an identifier-syntax transformer or an:
  ;;
  ;;   ($rtd . #<rtd>)
  ;;
  ;;form (ikarus/chez).  SANITIZE-BINDING converts the output to one of:
  ;;
  ;;   (lacal-macro . procedure)
  ;;   (local-macro! . procedure)
  ;;   (local-ctv . compile-time-value)
  ;;   ($rtd . $rtd)
  ;;
  ;;and signals an assertion-violation otherwise.
  ;;
  (cond ((procedure? x)
	 (cons* 'local-macro x src))
	((and (pair? x)
	      (eq? (car x) 'macro!)
	      (procedure? (cdr x)))
	 (cons* 'local-macro! (cdr x) src))
	((and (pair? x)
	      (eq? (car x) '$rtd))
	 x)
	((and (pair? x)
	      (eq? (car x) 'ctv))
	 (cons* 'local-ctv (cdr x) src))
	(else
	 (assertion-violation 'expand "invalid transformer" x))))


(define (make-variable-transformer x)
  ;;R6RS's make-variable-transformer.
  ;;
  (define who 'make-variable-transformer)
  (if (procedure? x)
      (cons 'macro! x)
    (assertion-violation who "not a procedure" x)))

(define (make-compile-time-value x)
  (cons 'ctv x))

(define (variable-transformer? x)
  (and (pair? x)
       (eq? (car x) 'macro!)
       (procedure? (cdr x))))

(define (variable-transformer-procedure x)
  (define who 'variable-transformer-procedure)
  (if (variable-transformer? x)
      (cdr x)
    (assertion-violation who "not a variable transformer" x)))

(define (make-eval-transformer x)
  ;;Take  an  expanded  expression,  evaluate it  and  return  a  proper
  ;;syntactic binding for the resulting object.
  ;;
  (sanitize-binding (eval-core (expanded->core x)) x))


(define-syntax syntax-match
  ;;The SYNTAX-MATCH macro is almost like SYNTAX-CASE macro.  Except that:
  ;;
  ;;*  The syntax  objects matched  are OUR  stx objects,  not the  host
  ;;  systems syntax objects (whatever they may be we don't care).
  ;;
  ;;*  The literals  are matched  against  those in  the system  library
  ;;  (psyntax system $all).  -- see scheme-stx
  ;;
  ;;* The variables  in the patters are bound to  ordinary variables not
  ;;  to special pattern variables.
  ;;
  ;;The actual matching between the input expression and the patterns is
  ;;performed  by   the  function   SYNTAX-DISPATCH;  the   patterns  in
  ;;SYNTAX-MATCH are converted  to a symbolic expressions  and handed to
  ;;SYNTAX-DISPATCH along with the input expression.
  ;;
  (let ()
    (define (transformer stx)
      (syntax-case stx ()

	;;No  clauses.  Some  of  the  SYNTAX-MATCH clauses  recursively
	;;expand  to uses  of  SYNTAX-MATCH; when  no  more clauses  are
	;;available in the input  form, this SYNTAX-CASE clause matches.
	;;When this happens: we want to raise a syntax error.
	;;
	;;Notice that we  do not want to raise a  syntax error here, but
	;;in the expanded code.
	((_ ?expr (?literals ...))
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (syntax (syntax-violation #f "invalid syntax" ?expr)))

	;;The next clause has a fender.
	((_ ?expr (?literals ...) (?pattern ?fender ?body) ?clause* ...)
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (receive (pattern ptnvars/levels)
	     (%convert-single-pattern (syntax ?pattern) (syntax (?literals ...)))
	   (with-syntax
	       ((PATTERN                   (sys.datum->syntax (syntax here) pattern))
		(((PTNVARS . LEVELS) ...)  ptnvars/levels))
	     (syntax
	      (let ((T ?expr))
		;;If   the  input   expression   matches  the   symbolic
		;;expression PATTERN...
		(let ((ls/false (syntax-dispatch T 'PATTERN)))
		  (if (and ls/false
			   ;;...and  the pattern  variables satisfy  the
			   ;;fender...
			   (apply (lambda (PTNVARS ...) ?fender) ls/false))
		      ;;...evaluate the body  with the pattern variables
		      ;;assigned.
		      (apply (lambda (PTNVARS ...) ?body) ls/false)
		    ;;...else try to match the next clause.
		    (syntax-match T (?literals ...) ?clause* ...))))))))

	;;The next clause has NO fender.
	((_ ?expr (?literals ...) (?pattern ?body) clause* ...)
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (receive (pattern ptnvars/levels)
	     (%convert-single-pattern (syntax ?pattern) (syntax (?literals ...)))
	   (with-syntax
	       ((PATTERN                   (sys.datum->syntax (syntax here) pattern))
		(((PTNVARS . LEVELS) ...)  ptnvars/levels))
	     (syntax
	      (let ((T ?expr))
		;;If   the  input   expression   matches  the   symbolic
		;;expression PATTERN...
		(let ((ls/false (syntax-dispatch T 'PATTERN)))
		  (if ls/false
		      ;;...evaluate the body  with the pattern variables
		      ;;assigned.
		      (apply (lambda (PTNVARS ...) ?body) ls/false)
		    ;;...else try to match the next clause.
		    (syntax-match T (?literals ...) clause* ...))))))))

	;;This is a true error in he use of SYNTAX-MATCH.  We still want
	;;the  expanded  code  to  raise  the  violation.   Notice  that
	;;SYNTAX-VIOLATION  is not  bound in  the expand  environment of
	;;SYNTAX-MATCH's transformer.
	;;
	(?stuff
	 (syntax (syntax-violation #f "invalid syntax" stx)))
	))

    (module (%convert-single-pattern)

      (define %convert-single-pattern
	;;Recursive function.  Transform the PATTERN-STX into a symbolic
	;;expression to be handed  to SYNTAX-DISPATCH.  PATTERN-STX must
	;;be  a  syntax  object  holding  the  SYNTAX-MATCH  pattern  to
	;;convert.  LITERALS must  be a syntax object holding  a list of
	;;identifiers being the literals in the PATTERN-STX.
	;;
	;;Return 2 values:
	;;
	;;1. The pattern as symbolic expression.
	;;
	;;2.   An ordered  list of  pairs, each  representing a  pattern
	;;   variable that must be bound whenever the body associated to
	;;   the  pattern is  evaluated.  The  car of  each pair  is the
	;;   symbol  being the pattern  variable name.  The cdr  of each
	;;   pair is an exact  integer representing the nesting level of
	;;   the pattern variable.
	;;
	(case-lambda
	 ((pattern-stx literals)
	  (%convert-single-pattern pattern-stx literals 0 '()))

	 ((pattern-stx literals nesting-level pattern-vars)
	  (syntax-case pattern-stx ()

	    ;;A literal identifier is encoded as:
	    ;;
	    ;;   #(scheme-id ?identifier)
	    ;;
	    ;;the wildcard underscore identifier is encoded as:
	    ;;
	    ;;   _
	    ;;
	    ;;any  other  identifier will  bind  a  variable and  it  is
	    ;;encoded as:
	    ;;
	    ;;   any
	    ;;
	    (?identifier
	     (sys.identifier? (syntax ?identifier))
	     (cond ((%bound-identifier-member? pattern-stx literals)
		    (values `#(scheme-id ,(sys.syntax->datum pattern-stx)) pattern-vars))
		   ((sys.free-identifier=? pattern-stx (syntax _))
		    (values '_ pattern-vars))
		   (else
		    (values 'any (cons (cons pattern-stx nesting-level)
				       pattern-vars)))))

	    ;;A  tail  pattern  with  ellipsis which  does  not  bind  a
	    ;;variable is encoded as:
	    ;;
	    ;;   #(each ?pattern)
	    ;;
	    ;;a tail pattern with ellipsis which does bind a variable is
	    ;;encoded as:
	    ;;
	    ;;   each-any
	    ;;
	    ((?pattern ?dots)
	     (%ellipsis? (syntax ?dots))
	     (receive (pattern^ pattern-vars^)
		 (%convert-single-pattern (syntax ?pattern) literals
					  (+ nesting-level 1) pattern-vars)
	       (values (if (eq? pattern^ 'any)
			   'each-any
			 `#(each ,pattern^))
		       pattern-vars^)))

	    ;;A non-tail pattern with ellipsis is encoded as:
	    ;;
	    ;;  #(each+ ?pattern-ellipsis (?pattern-following ...) . ?tail-pattern)
	    ;;
	    ((?pattern-x ?dots ?pattern-y ... . ?pattern-z)
	     (%ellipsis? (syntax ?dots))
	     (let*-values
		 (((pattern-z pattern-vars)
		   (%convert-single-pattern (syntax ?pattern-z) literals
					    nesting-level pattern-vars))

		  ((pattern-y* pattern-vars)
		   (%convert-multi-pattern  (syntax (?pattern-y ...)) literals
					    nesting-level pattern-vars))

		  ((pattern-x pattern-vars)
		   (%convert-single-pattern (syntax ?pattern-x) literals
					    (+ nesting-level 1) pattern-vars)))
	       (values `#(each+ ,pattern-x ,(reverse pattern-y*) ,pattern-z)
		       pattern-vars)))

	    ;;A pair is encoded as pair.
	    ;;
	    ((?car . ?cdr)
	     (let*-values
		 (((pattern-cdr pattern-vars)
		   (%convert-single-pattern (syntax ?cdr) literals
					    nesting-level pattern-vars))

		  ((pattern-car pattern-vars)
		   (%convert-single-pattern (syntax ?car) literals
					    nesting-level pattern-vars)))
	       (values (cons pattern-car pattern-cdr) pattern-vars)))

	    ;;Null is encoded as null.
	    ;;
	    (()
	     (values '() pattern-vars))

	    ;;A vector is encoded as:
	    ;;
	    ;;   #(vector ?datum)
	    ;;
	    (#(?item ...)
	     (receive (pattern-item* pattern-vars)
		 (%convert-single-pattern (syntax (?item ...)) literals
					  nesting-level pattern-vars)
	       (values `#(vector ,pattern-item*) pattern-vars)))

	    ;;A datum is encoded as:
	    ;;
	    ;;   #(atom ?datum)
	    ;;
	    (?datum
	     (values `#(atom ,(sys.syntax->datum (syntax ?datum))) pattern-vars))
	    ))))

      (define (%convert-multi-pattern pattern* literals nesting-level pattern-vars)
	;;Recursive function.
	;;
	(if (null? pattern*)
	    (values '() pattern-vars)
	  (let*-values
	      (((y pattern-vars^)
		(%convert-multi-pattern  (cdr pattern*) literals nesting-level pattern-vars))
	       ((x pattern-vars^^)
		(%convert-single-pattern (car pattern*) literals nesting-level pattern-vars^)))
	    (values (cons x y) pattern-vars^^))))

      (define (%bound-identifier-member? id list-of-ids)
	;;Return #t if  the identifier ID is  BOUND-IDENTIFIER=?  to one
	;;of the identifiers in LIST-OF-IDS.
	;;
	(and (pair? list-of-ids)
	     (or (sys.bound-identifier=? id (car list-of-ids))
		 (%bound-identifier-member? id (cdr list-of-ids)))))

      (define (%ellipsis? x)
	(and (sys.identifier? x)
	     (sys.free-identifier=? x (syntax (... ...)))))

      ;;Commented out because unused.  (Marco Maggi; Thu Apr 25, 2013)
      ;;
      ;; (define (%free-identifier-member? id1 list-of-ids)
      ;;   ;;Return #t if  the identifier ID1 is  FREE-IDENTIFIER=?  to one
      ;;   ;;of the identifiers in LIST-OF-IDS.
      ;;   ;;
      ;;   (and (exists (lambda (id2)
      ;; 		     (sys.free-identifier=? id1 id2))
      ;; 	     list-of-ids)
      ;; 	   #t))

      #| end of module: %CONVERT-SINGLE-PATTERN |# )

    transformer))


(define scheme-stx
  ;;Take a symbol  and if it's in the library:
  ;;
  ;;   (psyntax system $all)
  ;;
  ;;create a fresh identifier that maps  only the symbol to its label in
  ;;that library.  Symbols not in that library become fresh.
  ;;
  (let ((scheme-stx-hashtable (make-eq-hashtable)))
    (lambda (sym)
      (or (hashtable-ref scheme-stx-hashtable sym #f)
	  (let* ((subst  (library-subst (find-library-by-name '(psyntax system $all))))
		 (stx    (make-<stx> sym top-mark* '() '()))
		 (stx    (cond ((assq sym subst)
				=> (lambda (subst.entry)
				     (let ((name  (car subst.entry))
					   (label (cdr subst.entry)))
				       (push-lexical-contour
					(make-<rib> (list name)
						    (list top-mark*)
						    (list label)
						    #f)
					stx))))
			       (else stx))))
	    (hashtable-set! scheme-stx-hashtable sym stx)
	    stx)))))


(module (non-core-macro-transformer)
  ;;We distinguish between "non-core macros" and "core macros".
  ;;
  ;;Core macros  are part of the  core language: they cannot  be further
  ;;expanded to a  composition of other more basic  macros.  Core macros
  ;;*do*  introduce bindings,  so their  transformer functions  take the
  ;;lexical environments as arguments.
  ;;
  ;;Non-core macros are  *not* part of the core language:  they *can* be
  ;;expanded to  a composition of  core macros.  Non-core macros  do not
  ;;introduce bindings, so their transformer functions do *not* take the
  ;;lexical environments as arguments.
  ;;
  ;;The  function NON-CORE-MACRO-TRANSFORMER  maps symbols  representing
  ;;non-core  macros  to  their   macro  transformers.   The  expression
  ;;returned by a non-core transformer is further visited to process the
  ;;core macros and introduce bindings.
  ;;
  ;;NOTE This  module is very  long, so it  is split into  multiple code
  ;;pages.  (Marco Maggi; Sat Apr 27, 2013)
  ;;
  (define (non-core-macro-transformer x)
    (define who 'non-core-macro-transformer)
    (define (%error-invalid-macro)
      (error who "Vicare: internal error: invalid macro" x))
    (cond ((procedure? x)
	   x)
	  ((symbol? x)
	   (case x
	     ((define-record-type)		define-record-type-macro)
	     ((record-type-and-record?)		record-type-and-record?-macro)
	     ((define-struct)			define-struct-macro)
	     ((define-condition-type)		define-condition-type-macro)
	     ((cond)				cond-macro)
	     ((let)				let-macro)
	     ((do)				do-macro)
	     ((or)				or-macro)
	     ((and)				and-macro)
	     ((let*)				let*-macro)
	     ((let-values)			let-values-macro)
	     ((let*-values)			let*-values-macro)
	     ((syntax-rules)			syntax-rules-macro)
	     ((quasiquote)			quasiquote-macro)
	     ((quasisyntax)			quasisyntax-macro)
	     ((with-syntax)			with-syntax-macro)
	     ((when)				when-macro)
	     ((unless)				unless-macro)
	     ((case)				case-macro)
	     ((identifier-syntax)		identifier-syntax-macro)
	     ((time)				time-macro)
	     ((delay)				delay-macro)
	     ((assert)				assert-macro)
	     ((guard)				guard-macro)
	     ((define-enumeration)		define-enumeration-macro)
	     ((let*-syntax)			let*-syntax-macro)

	     ((trace-lambda)			trace-lambda-macro)
	     ((trace-define)			trace-define-macro)
	     ((trace-let)			trace-let-macro)
	     ((trace-define-syntax)		trace-define-syntax-macro)
	     ((trace-let-syntax)		trace-let-syntax-macro)
	     ((trace-letrec-syntax)		trace-letrec-syntax-macro)

	     ((include)				include-macro)
	     ((define-integrable)		define-integrable-macro)
	     ((define-inline)			define-inline-macro)
	     ((define-constant)			define-constant-macro)
	     ((define-inline-constant)		define-inline-constant-macro)
	     ((define-values)			define-values-macro)
	     ((define-constant-values)		define-constant-values-macro)
	     ((receive)				receive-macro)
	     ((receive-and-return)		receive-and-return-macro)
	     ((begin0)				begin0-macro)
	     ((xor)				xor-macro)
	     ((define-syntax-rule)		define-syntax-rule-macro)
	     ((define-auxiliary-syntaxes)	define-auxiliary-syntaxes-macro)
	     ((define-syntax*)			define-syntax*-macro)
	     ((unwind-protect)			unwind-protect-macro)
	     ((with-implicits)			with-implicits-macro)
	     ((set-cons!)			set-cons!-macro)

	     ((eval-for-expand)			eval-for-expand-macro)

	     ;; non-Scheme style syntaxes
	     ((return)				return-macro)
	     ((continue)			continue-macro)
	     ((break)				break-macro)
	     ((while)				while-macro)
	     ((until)				until-macro)
	     ((for)				for-macro)
	     ((define-returnable)		define-returnable-macro)
	     ((lambda-returnable)		lambda-returnable-macro)
	     ((begin-returnable)		begin-returnable-macro)

	     ((parameterize)			parameterize-macro)
	     ((parametrise)			parameterize-macro)

	     ;; compensations
	     ((with-compensations)		with-compensations-macro)
	     ((with-compensations/on-error)	with-compensations/on-error-macro)
	     ((compensate)			compensate-macro)
	     ((with)				with-macro)
	     ((push-compensation)		push-compensation-macro)

	     ((eol-style)
	      (lambda (x)
		(%allowed-symbol-macro x '(none lf cr crlf nel crnel ls))))

	     ((error-handling-mode)
	      (lambda (x)
		(%allowed-symbol-macro x '(ignore raise replace))))

	     ((buffer-mode)
	      (lambda (x)
		(%allowed-symbol-macro x '(none line block))))

	     ((endianness)
	      endianness-macro)

	     ((file-options)
	      file-options-macro)

	     ((... => _ else unquote unquote-splicing
		   unsyntax unsyntax-splicing
		   fields mutable immutable parent protocol
		   sealed opaque nongenerative parent-rtd)
	      incorrect-usage-macro)

	     (else
	      (%error-invalid-macro))))
	  (else
	   (%error-invalid-macro))))

  (define (incorrect-usage-macro expr-stx)
    (syntax-violation #f "incorrect usage of auxiliary keyword" expr-stx))


;;;; module non-core-macro-transformer: DEFINE-AUXILIARY-SYNTAXES

(define (define-auxiliary-syntaxes-macro expr-stx)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;DEFINE-AUXILIARY-SYNTAXES  macros   from  the  top-level   built  in
  ;;environment.  Expand  the contents  of EXPR-STX.  Return  a symbolic
  ;;expression in the core language.
  ;;
  ;;Using an empty SYNTAX-RULES as  transformer function makes sure that
  ;;whenever an auxiliary syntax is referenced an error is raised.
  ;;
  (syntax-match expr-stx ()
    ((_ ?id* ...)
     (for-all identifier? ?id*)
     (bless
      `(begin
	 ,@(map (lambda (id)
		  `(define-syntax ,id (syntax-rules ())))
	     ?id*))))))


;;;; module non-core-macro-transformer: control structures macros

(define (when-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if ,?test (begin ,?expr . ,?expr*))))))

(define (unless-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if (not ,?test) (begin ,?expr . ,?expr*))))))


;;;; module non-core-macro-transformer: CASE

(module (case-macro)
  ;;Transformer function  used to expand  R6RS's CASE macros  from the
  ;;top-level built in environment.   Expand the contents of EXPR-STX.
  ;;Return a symbolic expression in the core language.
  ;;
  (define (case-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr)
       (bless `(let ((t ,?expr))
		 (if #f #f))))
      ((_ ?expr ?clause ?clause* ...)
       (bless
	`(let ((t ,?expr))
	   ,(let recur ((clause  ?clause)
			(clause* ?clause*))
	      (if (null? clause*)
		  (%build-last clause)
		(%build-one clause (recur (car clause*) (cdr clause*))))))))))

  (define (%build-one clause-stx k)
    (syntax-match clause-stx ()
      (((?datum* ...) ?expr ?expr* ...)
       `(if (memv t ',?datum*)
	    (begin ,?expr . ,?expr*) ,k))))

  (define (%build-last clause)
    (syntax-match clause (else)
      ((else ?expr ?expr* ...)
       `(let () #f ,?expr . ,?expr*))
      (_
       (%build-one clause '(if #f #f)))))

  #| end of module: CASE-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-RECORD-TYPE

(define (define-record-type-macro x)
  (define who 'define-record-type)

  (define (main stx)
    (syntax-match stx ()
      ((_ namespec clause* ...)
       (begin
	 (%verify-clauses x clause*)
	 (%do-define-record namespec clause*)))
      ))

;;; --------------------------------------------------------------------

  (define (%do-define-record namespec clause*)
    (define foo		(%get-record-name namespec))
    (define foo-rtd	(gensym))
    (define foo-rcd	(gensym))
    (define protocol	(gensym))
    (define make-foo	(%get-record-constructor-name namespec))
    (define fields	(%get-fields clause*))
    (define field-names
      (%get-field-names fields))
    (define mutable-field-names
      (%get-mutable-field-names fields))
    ;;Indexes for safe accessors and mutators.
    (define idx*	(%enumerate fields))
    (define set-foo-idx*
      (%get-mutator-indices fields))
    ;;Names of safe accessors and mutators.
    (define foo-x*
      (%get-accessors foo fields))
    (define set-foo-x!*
      (%get-mutators foo fields))
    ;;Names of unsafe accessors and mutators.
    (define unsafe-foo-x*
      (%get-unsafe-accessors foo fields))
    (define unsafe-set-foo-x!*
      (%get-unsafe-mutators foo fields))
    ;;Names for unsafe index bindings.
    (define unsafe-foo-x-idx*
      (%get-unsafe-accessors-idx-names foo fields))
    (define unsafe-set-foo-x!-idx*
      (%get-unsafe-mutators-idx-names foo fields))

    ;;Safe field accessors and mutators alists.
    (define foo-fields-safe-accessors-table
      ;;Here we want to build a sexp  which will be BLESSed below in the
      ;;output code.  The sexp will  evluate to an alist, having symbols
      ;;representing field  names as  keys and  an identifiers  bound to
      ;;unsafe accessors as values.
      (map (lambda (name func)
	     (list 'quasiquote (cons name (list 'unquote (list 'syntax func)))))
	(map syntax->datum field-names)
	foo-x*))
    (define foo-fields-safe-mutators-table
      ;;Here we want to build a sexp  which will be BLESSed below in the
      ;;output code.  The sexp will  evluate to an alist, having symbols
      ;;representing field  names as  keys and  an identifiers  bound to
      ;;unsafe mutators as values.
      (map (lambda (name func)
	     (list 'quasiquote (cons name (list 'unquote (list 'syntax func)))))
	(map syntax->datum mutable-field-names)
	set-foo-x!*))

    ;;Unsafe field accessors and mutators alists.
    (define foo-fields-unsafe-accessors-table
      ;;Here we want to build a sexp  which will be BLESSed below in the
      ;;output code.  The sexp will  evluate to an alist, having symbols
      ;;representing field  names as  keys and  an identifiers  bound to
      ;;unsafe accessors as values.
      (map (lambda (name func)
	     (list 'quasiquote (cons name (list 'unquote (list 'syntax func)))))
	(map syntax->datum field-names)
	unsafe-foo-x*))
    (define foo-fields-unsafe-mutators-table
      ;;Here we want to build a sexp  which will be BLESSed below in the
      ;;output code.  The sexp will  evluate to an alist, having symbols
      ;;representing field  names as  keys and  an identifiers  bound to
      ;;unsafe mutators as values.
      (map (lambda (name func)
	     (list 'quasiquote (cons name (list 'unquote (list 'syntax func)))))
	(map syntax->datum mutable-field-names)
	unsafe-set-foo-x!*))

    ;;Predicate name.
    (define foo?
      (%get-record-predicate-name namespec))
    ;;Code  for  record-type   descriptor  and  record-type  constructor
    ;;descriptor.
    (define foo-rtd-code
      (%make-rtd-code foo clause* (%make-parent-rtd-code clause*)))
    (define foo-rcd-code
      (%make-rcd-code clause* foo-rtd protocol (%make-parent-rcd-code clause*)))
    ;;Code for protocol.
    (define protocol-code
      (%get-protocol-code clause*))
    (define r6rs-output-code
      `(begin
	 ;;Record type descriptor.
	 (define ,foo-rtd ,foo-rtd-code)
	 ;;Protocol function.
	 (define ,protocol ,protocol-code)
	 ;;Record constructor descriptor.
	 (define ,foo-rcd ,foo-rcd-code)
	 ;;Record instance predicate.
	 (define ,foo? (record-predicate ,foo-rtd))
	 ;;Record instance constructor.
	 (define ,make-foo (record-constructor ,foo-rcd))
	 ;;Safe record fields accessors.
	 ,@(map (lambda (foo-x idx)
		  `(define ,foo-x (record-accessor ,foo-rtd ,idx)))
	     foo-x* idx*)
	 ;;Safe record fields mutators (if any).
	 ,@(map (lambda (set-foo-x! idx)
		  `(define ,set-foo-x! (record-mutator ,foo-rtd ,idx)))
	     set-foo-x!* set-foo-idx*)))
    (define vicare-output-code
      (if (strict-r6rs)
	  `( ;;Binding for record type name.   It is a spcial binding in
	     ;;the environment.
	    (define-syntax ,foo
	      (list '$rtd (syntax ,foo-rtd) (syntax ,foo-rcd)
		    (list ,@foo-fields-safe-accessors-table)
		    (list ,@foo-fields-safe-mutators-table))))
	`( ;;Binding  for record type name.   It is a spcial  binding in
	   ;;the environment.
	  (define-syntax ,foo
	    (list '$rtd (syntax ,foo-rtd) (syntax ,foo-rcd)
		  (list ,@foo-fields-safe-accessors-table)
		  (list ,@foo-fields-safe-mutators-table)
		  (list ,@foo-fields-unsafe-accessors-table)
		  (list ,@foo-fields-unsafe-mutators-table)))
	  ;; Unsafe record fields accessors.
	  ,@(map (lambda (unsafe-foo-x idx unsafe-foo-x-idx)
		   `(begin
		      (define ,unsafe-foo-x-idx
			;;The field at index 3  in the RTD is: the index
			;;of  the first  field  of this  subtype in  the
			;;layout of instances; it is the total number of
			;;fields of the parent type.
			(fx+ ,idx ($struct-ref ,foo-rtd 3)))
		      (define-syntax-rule (,unsafe-foo-x x)
			($struct-ref x ,unsafe-foo-x-idx))
		      ))
	      unsafe-foo-x* idx* unsafe-foo-x-idx*)
	  ;; Unsafe record fields mutators.
	  ,@(map (lambda (unsafe-set-foo-x! idx unsafe-set-foo-x!-idx)
		   `(begin
		      (define ,unsafe-set-foo-x!-idx
			;;The field at index 3  in the RTD is: the index
			;;of  the first  field  of this  subtype in  the
			;;layout of instances; it is the total number of
			;;fields of the parent type.
			(fx+ ,idx ($struct-ref ,foo-rtd 3)))
		      (define-syntax-rule (,unsafe-set-foo-x! x v)
			($struct-set! x ,unsafe-set-foo-x!-idx v))
		      ))
	      unsafe-set-foo-x!* set-foo-idx* unsafe-set-foo-x!-idx*)
	  )))
    (bless (append r6rs-output-code vicare-output-code)))

;;; --------------------------------------------------------------------

  (define (%get-record-name spec)
    (syntax-match spec ()
      ((foo make-foo foo?) foo)
      (foo foo)))

  (define (%get-record-constructor-name spec)
    (syntax-match spec ()
      ((foo make-foo foo?) make-foo)
      (foo (identifier? foo) (id foo "make-" (syntax->datum foo)))))

  (define (%get-record-predicate-name spec)
    (syntax-match spec ()
      ((foo make-foo foo?)
       foo?)
      (foo
       (identifier? foo)
       (id foo foo "?"))))

  (define (get-clause id ls)
    (syntax-match ls ()
      (()
       #f)
      (((x . rest) . ls)
       (if (free-id=? (bless id) x)
	   `(,x . ,rest)
	 (get-clause id ls)))))

  (define (%make-rtd-code name clause* parent-rtd-code)
    (define (convert-field-spec* ls)
      (list->vector
       (map (lambda (x)
	      (syntax-match x (mutable immutable)
		((mutable name . rest) `(mutable ,name))
		((immutable name . rest) `(immutable ,name))
		(name `(immutable ,name))))
	 ls)))
    (let ((uid-code
	   (syntax-match (get-clause 'nongenerative clause*) ()
	     ((_)     `',(gensym))
	     ((_ uid) `',uid)
	     (_       #f)))
	  (sealed?
	   (syntax-match (get-clause 'sealed clause*) ()
	     ((_ #t) #t)
	     (_      #f)))
	  (opaque?
	   (syntax-match (get-clause 'opaque clause*) ()
	     ((_ #t) #t)
	     (_      #f)))
	  (fields
	   (syntax-match (get-clause 'fields clause*) ()
	     ((_ field-spec* ...)
	      `(quote ,(convert-field-spec* field-spec*)))
	     (_ ''#()))))
      (bless
       `(make-record-type-descriptor ',name
				     ,parent-rtd-code
				     ,uid-code ,sealed? ,opaque? ,fields))))

  (define (%make-parent-rtd-code clause*)
    (syntax-match (get-clause 'parent clause*) ()
      ((_ name)
       `(record-type-descriptor ,name))
      (#f
       (syntax-match (get-clause 'parent-rtd clause*) ()
	 ((_ rtd rcd) rtd)
	 (#f #f)))))

  (define (%make-parent-rcd-code clause*)
    (syntax-match (get-clause 'parent clause*) ()
      ((_ name)
       `(record-constructor-descriptor ,name))
      (#f
       (syntax-match (get-clause 'parent-rtd clause*) ()
	 ((_ rtd rcd)	rcd)
	 (#f		#f)))))

  (define (%make-rcd-code clause* foo-rtd protocol parent-rcd-code)
    `(make-record-constructor-descriptor ,foo-rtd ,parent-rcd-code ,protocol))

  (define (%get-protocol-code clause*)
    (syntax-match (get-clause 'protocol clause*) ()
      ((_ expr)		expr)
      (_		#f)))

  (define (%get-fields clause*)
    (syntax-match clause* (fields)
      (()
       '())
      (((fields f* ...) . _)
       f*)
      ((_ . rest)
       (%get-fields rest))))

;;; --------------------------------------------------------------------

  (define (%get-field-names fields)
    ;;Given the fields specification clause return a list of identifiers
    ;;representing all the field names.
    ;;
    (map (lambda (field)
	   (syntax-match field (mutable immutable)
	     ((mutable name accessor mutator)	(identifier? accessor)	name)
	     ((immutable name accessor)		(identifier? accessor)	name)
	     ((mutable name)			(identifier? name)	name)
	     ((immutable name)			(identifier? name)	name)
	     (name				(identifier? name)	name)
	     (others
	      (stx-error field "invalid field spec"))))
      fields))

  (define (%get-mutable-field-names fields)
    ;;Given the fields specification clause return a list of identifiers
    ;;representing the mutable field names.
    ;;
    (syntax-match fields (mutable immutable)
      (()
       '())

      (((mutable name accessor mutator) . rest)
       (identifier? accessor)
       (cons name (%get-mutable-field-names rest)))

      (((immutable name accessor) . rest)
       (identifier? accessor)
       (%get-mutable-field-names rest))

      (((mutable name) . rest)
       (identifier? name)
       (cons name (%get-mutable-field-names rest)))

      (((immutable name) . rest)
       (identifier? name)
       (%get-mutable-field-names rest))

      ((name . rest)
       (identifier? name)
       (%get-mutable-field-names rest))

      (others
       (stx-error fields "invalid field spec"))))

;;; --------------------------------------------------------------------

  (define (%get-mutator-indices fields)
    (let recur ((fields fields) (i 0))
      (syntax-match fields (mutable)
	(()
	 '())
	(((mutable . _) . rest)
	 (cons i (recur rest (+ i 1))))
	((_ . rest)
	 (recur rest (+ i 1))))))

  (define (%get-mutators foo fields)
    (define (gen-name x)
      (id foo foo "-" x "-set!"))
    (let recur ((fields fields))
      (syntax-match fields (mutable)
	(()
	 '())
	(((mutable name accessor mutator) . rest)
	 (cons mutator (recur rest)))
	(((mutable name) . rest)
	 (cons (gen-name name) (recur rest)))
	((_ . rest)
	 (recur rest)))))

  (define (%get-unsafe-mutators foo fields)
    (define (gen-name x)
      (id foo "$" foo "-" x "-set!"))
    (let f ((fields fields))
      (syntax-match fields (mutable)
	(() '())
	(((mutable name accessor mutator) . rest)
	 (cons (gen-name name) (f rest)))
	(((mutable name) . rest)
	 (cons (gen-name name) (f rest)))
	((_ . rest) (f rest)))))

  (define (%get-unsafe-mutators-idx-names foo fields)
    (let f ((fields fields))
      (syntax-match fields (mutable)
	(() '())
	(((mutable name accessor mutator) . rest)
	 (cons (gensym) (f rest)))
	(((mutable name) . rest)
	 (cons (gensym) (f rest)))
	((_ . rest) (f rest)))))

;;; --------------------------------------------------------------------

  (define (%get-accessors foo fields)
    (define (gen-name x)
      (id foo foo "-" x))
    (map (lambda (field)
	   (syntax-match field (mutable immutable)
	     ((mutable name accessor mutator) (identifier? accessor) accessor)
	     ((immutable name accessor)       (identifier? accessor) accessor)
	     ((mutable name)                  (identifier? name) (gen-name name))
	     ((immutable name)                (identifier? name) (gen-name name))
	     (name                            (identifier? name) (gen-name name))
	     (others (stx-error field "invalid field spec"))))
      fields))

  (define (%get-unsafe-accessors foo fields)
    (define (gen-name x)
      (id foo "$" foo "-" x))
    (map (lambda (field)
	   (syntax-match field (mutable immutable)
	     ((mutable name accessor mutator) (identifier? accessor) (gen-name name))
	     ((immutable name accessor)       (identifier? accessor) (gen-name name))
	     ((mutable name)                  (identifier? name) (gen-name name))
	     ((immutable name)                (identifier? name) (gen-name name))
	     (name                            (identifier? name) (gen-name name))
	     (others (stx-error field "invalid field spec"))))
      fields))

  (define (%get-unsafe-accessors-idx-names foo fields)
    (map (lambda (x)
	   (gensym))
      fields))

;;; --------------------------------------------------------------------

  (define (%enumerate ls)
    ;;Return a list of zero-based exact integers with the same length of
    ;;LS.
    ;;
    (let recur ((ls ls)
		(i  0))
      (if (null? ls)
	  '()
	(cons i (recur (cdr ls) (+ i 1))))))

;;; --------------------------------------------------------------------

  (define (%verify-clauses x cls*)
    (define VALID-KEYWORDS
      (map bless
	'(fields parent parent-rtd protocol sealed opaque nongenerative)))
    (define (%free-id-member? x ls)
      (and (pair? ls)
	   (or (free-id=? x (car ls))
	       (%free-id-member? x (cdr ls)))))
    (let loop ((cls*  cls*)
	       (seen* '()))
      (unless (null? cls*)
	(syntax-match (car cls*) ()
	  ((kwd . rest)
	   (cond ((or (not (identifier? kwd))
		      (not (%free-id-member? kwd VALID-KEYWORDS)))
		  (stx-error kwd "not a valid define-record-type keyword"))
		 ((bound-id-member? kwd seen*)
		  (syntax-violation #f "duplicate use of keyword " x kwd))
		 (else
		  (loop (cdr cls*) (cons kwd seen*)))))
	  (cls
	   (stx-error cls "malformed define-record-type clause"))))))

  (define (id ctxt . str*)
    ;;Given the  identifier CTXT  and a  list of  strings or  symbols or
    ;;identifiers  STR*: concatenate  all the  items in  STR*, with  the
    ;;result build  and return a new  identifier in the same  context of
    ;;CTXT.
    ;;
    (datum->syntax ctxt (string->symbol (apply string-append
					       (map (lambda (x)
						      (cond ((symbol? x)
							     (symbol->string x))
							    ((string? x)
							     x)
							    ((identifier? x)
							     (symbol->string (syntax->datum x)))
							    (else
							     (assertion-violation who "BUG"))))
						 str*)))))

;;; --------------------------------------------------------------------

  (main x))


;;;; module non-core-macro-transformer: RECORD-TYPE-AND-RECORD?

(define (record-type-and-record?-macro expr-stx)
  ;;Transformer function used to expand Vicare's RECORD-TYPE-AND-RECORD?
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?type-name ?record)
     (identifier? ?type-name)
     (bless
      `(record-and-rtd? ,?record (record-type-descriptor ,?type-name))))
    ))


;;;; module non-core-macro-transformer: DEFINE-CONDITION-TYPE

(define define-condition-type-macro
  (lambda (x)
    (define (mkname name suffix)
      (datum->syntax name
		     (string->symbol
		      (string-append
		       (symbol->string (syntax->datum name))
		       suffix))))
    (syntax-match x ()
      ((ctxt name super constructor predicate (field* accessor*) ...)
       (and (identifier? name)
	    (identifier? super)
	    (identifier? constructor)
	    (identifier? predicate)
	    (for-all identifier? field*)
	    (for-all identifier? accessor*))
       (let ((aux-accessor* (map (lambda (x) (gensym)) accessor*)))
	 (bless
	  `(begin
	     (define-record-type (,name ,constructor ,(gensym))
	       (parent ,super)
	       (fields ,@(map (lambda (field aux)
				`(immutable ,field ,aux))
			   field* aux-accessor*))
	       (nongenerative)
	       (sealed #f) (opaque #f))
	     (define ,predicate (condition-predicate
				 (record-type-descriptor ,name)))
	     ,@(map
		   (lambda (accessor aux)
		     `(define ,accessor
			(condition-accessor
			 (record-type-descriptor ,name) ,aux)))
		 accessor* aux-accessor*))))))))


;;;; module non-core-macro-transformer: PARAMETERIZE and PARAMETRISE

(define parameterize-macro
  ;;
  ;;Notice that  MAKE-PARAMETER is  a primitive function  implemented in
  ;;"ikarus.compiler.sls" by "E-make-parameter".
  ;;
  (lambda (e)
    (syntax-match e ()
      ((_ () b b* ...)
       (bless `(let () ,b . ,b*)))
      ((_ ((olhs* orhs*) ...) b b* ...)
       (let ((lhs* (generate-temporaries olhs*))
	     (rhs* (generate-temporaries orhs*)))
	 (bless
	  `((lambda ,(append lhs* rhs*)
	      (let* ((guard? #t) ;apply the guard function only the first time
		     (swap   (lambda ()
			       ,@(map (lambda (lhs rhs)
					`(let ((t (,lhs)))
					   (,lhs ,rhs guard?)
					   (set! ,rhs t)))
				   lhs* rhs*)
			       (set! guard? #f))))
		(dynamic-wind
		    swap
		    (lambda () ,b . ,b*)
		    swap)))
	    ,@(append olhs* orhs*))))
       ;;Below is the original Ikarus code (Marco Maggi; Feb 3, 2012).
       ;;
       ;; (let ((lhs* (generate-temporaries olhs*))
       ;;       (rhs* (generate-temporaries orhs*)))
       ;;   (bless
       ;;     `((lambda ,(append lhs* rhs*)
       ;;         (let ((swap (lambda ()
       ;;                       ,@(map (lambda (lhs rhs)
       ;;                                `(let ((t (,lhs)))
       ;;                                   (,lhs ,rhs)
       ;;                                   (set! ,rhs t)))
       ;;                              lhs* rhs*))))
       ;;           (dynamic-wind
       ;;             swap
       ;;             (lambda () ,b . ,b*)
       ;;             swap)))
       ;;       ,@(append olhs* orhs*))))
       ))))


;;;; module non-core-macro-transformer: UNWIND-PROTECT

(define (unwind-protect-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  UNWIND-PROTECT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  ;;Not a  general UNWIND-PROTECT for Scheme,  but fine where we  do not
  ;;make the body return continuations to  the caller and then come back
  ;;again and again, calling CLEANUP multiple times.
  ;;
  (syntax-match expr-stx ()
    ((_ ?body ?cleanup0 ?cleanup* ...)
     (bless
      `(let ((cleanup (lambda () ,?cleanup0 ,@?cleanup*)))
	 (with-exception-handler
	     (lambda (E)
	       (cleanup)
	       (raise E))
	   (lambda ()
	     (begin0
		 ,?body
	       (cleanup)))))))
    ))


;;;; module non-core-macro-transformer: WITH-IMPLICITS

(define (with-implicits-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  WITH-IMPLICITS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (define (%make-bindings ctx ids)
    (map (lambda (id)
	   `(,id (datum->syntax ,ctx (quote ,id))))
      ids))

  (syntax-match expr-stx ()

    ((_ () ?body0 ?body* ...)
     (bless
      `(begin ,?body0 ,@?body*)))

    ((_ ((?ctx ?symbol0 ?symbol* ...))
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS ,?body0 ,@?body*))))

    ((_ ((?ctx ?symbol0 ?symbol* ...) . ?other-clauses)
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS (with-implicits ,?other-clauses ,?body0 ,@?body*)))))

    ))


;;;; module non-core-macro-transformer: SET-CONS!

(define (set-cons!-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  SET-CONS! macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?id ?obj)
     (identifier? ?id)
     (bless `(set! ,?id (cons ,?obj ,?id))))
    ))


;;;; module non-core-macro-transformer: WITH-IMPLICITS

(define (eval-for-expand-macro expr-stx)
  ;;Transformer function used to  expand Vicare's EVAL-FOR-EXPAND macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?body0 ?body* ...)
     (bless
      `(define-syntax ,(gensym "eval-for-expand")
	 (begin ,?body0 ,@?body* values))))
    ))


;;;; module non-core-macro-transformer: compensations

(define (with-macro expr-stx)
  (syntax-match expr-stx ()
    ((_)
     (bless
      (lambda (stx)
	(syntax-error 'with "syntax \"with\" out of context"))))))

(module (with-compensations/on-error-macro
	 with-compensations-macro)

  (define (with-compensations/on-error-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?body0 ?body* ...)
       (bless
	`(let ,(%make-store-binding)
	   (parametrise ((compensations store))
	     ,(%make-with-exception-handler ?body0 ?body*)))))
      ))

  (define (with-compensations-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?body0 ?body* ...)
       (bless
	`(let ,(%make-store-binding)
	   (parametrise ((compensations store))
	     (begin0
		 ,(%make-with-exception-handler ?body0 ?body*)
	       ;;Better  run  the  cleanup   compensations  out  of  the
	       ;;WITH-EXCEPTION-HANDLER.
	       (run-compensations-store store))))))
      ))

  (define (%make-store-binding)
    '((store (let ((stack '()))
	       (case-lambda
		(()
		 stack)
		((false/thunk)
		 (if false/thunk
		     (set! stack (cons false/thunk stack))
		   (set! stack '()))))))))

  (define (%make-with-exception-handler body0 body*)
    ;;We really have to close the handler upon the STORE function, it is
    ;;wrong to access the COMPENSATIONS parameter from the handler.  The
    ;;dynamic environment  is synchronised with continuations:  when the
    ;;handler is called by  RAISE or RAISE-CONTINUABLE, the continuation
    ;;is the one of the RAISE or RAISE-CONTINUABLE forms.
    ;;
    `(with-exception-handler
	 (lambda (E)
	   (run-compensations-store store)
	   (raise E))
       (lambda ()
	 ,body0 ,@body*)))

  #| end of module |# )

(define (push-compensation-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?release0 ?release* ...)
     (bless
      `(push-compensation-thunk (lambda () ,?release0 ,@?release*))))
    ))

(define (compensate-macro expr-stx)
  (define who 'compensate)
  (define (%synner message subform)
    (syntax-violation who message expr-stx subform))
  (syntax-match expr-stx ()
    ((_ ?alloc0 ?form* ...)
     (let ()
       (define free #f)
       (define alloc*
	 (let recur ((form-stx ?form*))
	   (syntax-match form-stx (with)
	     (((with ?release0 ?release* ...))
	      (begin
		(set! free `(push-compensation ,?release0 ,@?release*))
		'()))

	     (()
	      (%synner "invalid compensation syntax: missing WITH keyword" form-stx))

	     (((with))
	      (%synner "invalid compensation syntax: empty WITH keyword"
		       (bless '(with))))

	     ((?alloc ?form* ...)
	      (cons ?alloc (recur ?form*)))
	     )))
       (bless
	`(begin0 (begin ,?alloc0 ,@alloc*) ,free))))
    ))


;;;; module non-core-macro-transformer: DEFINE-STRUCT

(define define-struct-macro
  (if-wants-define-struct
   (lambda (e)
     (define enumerate
       (lambda (ls)
	 (let f ((i 0) (ls ls))
	   (cond
	    ((null? ls) '())
	    (else (cons i (f (+ i 1) (cdr ls))))))))
     (define mkid
       (lambda (id str)
	 (datum->stx id (string->symbol str))))
     (syntax-match e ()
       ((_ name (field* ...))
	(let* ((namestr		(symbol->string (identifier->symbol name)))
	       (fields		(map identifier->symbol field*))
	       (fieldstr*	(map symbol->string fields))
	       (rtd		(datum->stx name (make-struct-type namestr fields)))
	       (constr		(mkid name (string-append "make-" namestr)))
	       (pred		(mkid name (string-append namestr "?")))
	       (i*		(enumerate field*))
	       (getters		(map (lambda (x)
				       (mkid name (string-append namestr "-" x)))
				  fieldstr*))
	       (setters		(map (lambda (x)
				       (mkid name (string-append "set-" namestr "-" x "!")))
				  fieldstr*))
	       (unsafe-getters	(map (lambda (x)
				       (mkid name (string-append "$" namestr "-" x)))
				  fieldstr*))
	       (unsafe-setters	(map (lambda (x)
				       (mkid name (string-append "$set-" namestr "-" x "!")))
				  fieldstr*)))
	  (bless
	   `(begin
	      (define-syntax ,name (cons '$rtd ',rtd))
	      (define ,constr
		(lambda ,field*
		  (let ((S ($struct ',rtd ,@field*)))
		    (if ($struct-ref ',rtd 5) ;destructor
			($struct-guardian S)
		      S))))
	      (define ,pred
		(lambda (x) ($struct/rtd? x ',rtd)))
	      ,@(map (lambda (getter i)
		       `(define ,getter
			  (lambda (x)
			    (if ($struct/rtd? x ',rtd)
				($struct-ref x ,i)
			      (assertion-violation ',getter
				"not a struct of required type as struct getter argument"
				x ',rtd)))))
		  getters i*)
	      ,@(map (lambda (setter i)
		       `(define ,setter
			  (lambda (x v)
			    (if ($struct/rtd? x ',rtd)
				($struct-set! x ,i v)
			      (assertion-violation ',setter
				"not a struct of required type as struct setter argument"
				x ',rtd)))))
		  setters i*)
	      ,@(map (lambda (unsafe-getter i)
		       `(define-syntax ,unsafe-getter
			  (syntax-rules ()
			    ((_ x)
			     ($struct-ref x ,i))))
		       #;(unquote (define ,unsafe-getter
		       (lambda (x)
		       ($struct-ref x ,i)))))
	      unsafe-getters i*)
	   ,@(map (lambda (unsafe-setter i)
		    `(define-syntax ,unsafe-setter
		       (syntax-rules ()
			 ((_ x v)
			  ($struct-set! x ,i v))))
		    #;(unquote (define ,unsafe-setter
		    (lambda (x v)
		    ($struct-set! x ,i v)))))
	   unsafe-setters i*))
	)))))
   (lambda (stx)
     (stx-error stx "define-struct not supported"))))


;;;; module non-core-macro-transformer: SYNTAX-RULES

(define syntax-rules-macro
  (lambda (e)
    (syntax-match e ()
      ((_ (lits ...)
	  (pat* tmp*) ...)
       (begin
	 (%verify-literals lits e)
	 (bless `(lambda (x)
		   (syntax-case x ,lits
		     ,@(map (lambda (pat tmp)
			      (syntax-match pat ()
				((_ . rest)
				 `((g . ,rest) (syntax ,tmp)))
				(_
				 (syntax-violation #f
				   "invalid syntax-rules pattern"
				   e pat))))
			 pat* tmp*)))))))))

(define (define-syntax-rule-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ (?name ?arg* ... . ?rest) ?body0 ?body* ...)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name
	 (syntax-rules ()
	   ((_ ,@?arg* . ,?rest)
	    (begin ,?body0 ,@?body*))))))
    ))


;;;; module non-core-macro-transformer: DEFINE-SYNTAX*

(define (define-syntax*-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  DEFINE-SYNTAX* macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name (syntax-rules ()))))
    ((_ ?name ?expr)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name ,?expr)))
    ((_ (?name ?stx) ?body0 ?body* ...)
     (and (identifier? ?name)
	  (identifier? ?stx))
     (let ((WHO     (datum->syntax ?name '__who__))
	   (SYNNER  (datum->syntax ?name 'synner)))
       (bless
	`(define-syntax ,?name
	   (lambda (,?stx)
	     (let-syntax
		 ((,WHO (identifier-syntax (quote ,?name))))
	       (letrec
		   ((,SYNNER (case-lambda
			      ((message)
			       (,SYNNER message #f))
			      ((message subform)
			       (syntax-violation ,WHO message ,?stx subform)))))
		 ,?body0 ,@?body*)))))))
    ))


;;;; module non-core-macro-transformer: WITH-SYNTAX

(define with-syntax-macro
  (lambda (e)
    (syntax-match e ()
      ((_ ((pat* expr*) ...) b b* ...)
       (let ((idn*
	      (let f ((pat* pat*))
		(cond
		 ((null? pat*) '())
		 (else
		  (let-values (((pat idn*) (convert-pattern (car pat*) '())))
		    (append idn* (f (cdr pat*)))))))))
	 (%verify-formals-syntax (map car idn*) e)
	 (let ((t* (generate-temporaries expr*)))
	   (bless
	    `(let ,(map list t* expr*)
	       ,(let f ((pat* pat*) (t* t*))
		  (cond
		   ((null? pat*) `(let () ,b . ,b*))
		   (else
		    `(syntax-case ,(car t*) ()
		       (,(car pat*) ,(f (cdr pat*) (cdr t*)))
		       (_ (assertion-violation 'with-syntax
			    "pattern does not match value"
			    ',(car pat*)
			    ,(car t*)))))))))))))))


;;;; module non-core-macro-transformer: IDENTIFIER-SYNTAX

(define identifier-syntax-macro
  (lambda (stx)
    (syntax-match stx (set!)
      ((_ expr)
       (bless `(lambda (x)
		 (syntax-case x ()
		   (id (identifier? (syntax id)) (syntax ,expr))
		   ((id e* ...) (identifier? (syntax id))
		    (cons (syntax ,expr) (syntax (e* ...))))))))
      ((_ (id1 expr1) ((set! id2 expr2) expr3))
       (and (identifier? id1) (identifier? id2) (identifier? expr2))
       (bless `(cons 'macro!
		     (lambda (x)
		       (syntax-case x (set!)
			 (id (identifier? (syntax id)) (syntax ,expr1))
			 ((set! id ,expr2) (syntax ,expr3))
			 ((id e* ...) (identifier? (syntax id)) (syntax (,expr1 e* ...)))))))))))


;;;; module non-core-macro-transformer: LET, LET*, TRACE-LET

(define let-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ ((lhs* rhs*) ...) b b* ...)
       (if (valid-bound-ids? lhs*)
	   (bless `((lambda ,lhs* ,b . ,b*) . ,rhs*))
	 (%error-invalid-formals-syntax stx lhs*)))
      ((_ f ((lhs* rhs*) ...) b b* ...) (identifier? f)
       (if (valid-bound-ids? lhs*)
	   (bless `((letrec ((,f (lambda ,lhs* ,b . ,b*))) ,f) . ,rhs*))
	 (%error-invalid-formals-syntax stx lhs*))))))

(define let*-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ ((lhs* rhs*) ...) b b* ...) (for-all identifier? lhs*)
       (bless
	(let f ((x* (map list lhs* rhs*)))
	  (cond
	   ((null? x*) `(let () ,b . ,b*))
	   (else `(let (,(car x*)) ,(f (cdr x*)))))))))))

(define trace-let-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ f ((lhs* rhs*) ...) b b* ...) (identifier? f)
       (if (valid-bound-ids? lhs*)
	   (bless
	    `((letrec ((,f (trace-lambda ,f ,lhs* ,b . ,b*))) ,f) . ,rhs*))
	 (%error-invalid-formals-syntax stx lhs*))))))


;;;; module non-core-macro-transformer: LET-VALUES

(define let-values-macro
  (lambda (stx)
    (define (rename x old* new*)
      (unless (identifier? x)
	(syntax-violation #f "not an indentifier" stx x))
      (when (bound-id-member? x old*)
	(syntax-violation #f "duplicate binding" stx x))
      (let ((y (gensym (syntax->datum x))))
	(values y (cons x old*) (cons y new*))))
    (define (rename* x* old* new*)
      (cond
       ((null? x*) (values '() old* new*))
       (else
	(let*-values (((x old* new*) (rename (car x*) old* new*))
		      ((x* old* new*) (rename* (cdr x*) old* new*)))
	  (values (cons x x*) old* new*)))))
    (syntax-match stx ()
      ((_ () b b* ...)
       (cons* (bless 'let) '() b b*))
      ((_ ((lhs* rhs*) ...) b b* ...)
       (bless
	(let f ((lhs* lhs*) (rhs* rhs*) (old* '()) (new* '()))
	  (cond
	   ((null? lhs*)
	    `(let ,(map list old* new*) ,b . ,b*))
	   (else
	    (syntax-match (car lhs*) ()
	      ((x* ...)
	       (let-values (((y* old* new*) (rename* x* old* new*)))
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,y*
		      ,(f (cdr lhs*) (cdr rhs*) old* new*)))))
	      ((x* ... . x)
	       (let*-values (((y old* new*) (rename x old* new*))
			     ((y* old* new*) (rename* x* old* new*)))
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,(append y* y)
		      ,(f (cdr lhs*) (cdr rhs*)
			  old* new*)))))
	      (others
	       (syntax-violation #f "malformed bindings"
				 stx others)))))))))))


;;;; module non-core-macro-transformer: LET*-VALUES

(define let*-values-macro
  (lambda (stx)
    (define (check x*)
      (unless (null? x*)
	(let ((x (car x*)))
	  (unless (identifier? x)
	    (syntax-violation #f "not an identifier" stx x))
	  (check (cdr x*))
	  (when (bound-id-member? x (cdr x*))
	    (syntax-violation #f "duplicate identifier" stx x)))))
    (syntax-match stx ()
      ((_ () b b* ...)
       (cons* (bless 'let) '() b b*))
      ((_ ((lhs* rhs*) ...) b b* ...)
       (bless
	(let f ((lhs* lhs*) (rhs* rhs*))
	  (cond
	   ((null? lhs*)
	    `(begin ,b . ,b*))
	   (else
	    (syntax-match (car lhs*) ()
	      ((x* ...)
	       (begin
		 (check x*)
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,x*
		      ,(f (cdr lhs*) (cdr rhs*))))))
	      ((x* ... . x)
	       (begin
		 (check (cons x x*))
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,(append x* x)
		      ,(f (cdr lhs*) (cdr rhs*))))))
	      (others
	       (syntax-violation #f "malformed bindings"
				 stx others)))))))))))


;;;; module non-core-macro-transformer: LET*-SYNTAX

(define (let*-syntax-macro stx)
  (syntax-match stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(begin ,?body ,@?body*)))
    ;;Single binding.
    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 ,?body ,@?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 (let*-syntax ,(map list ?lhs* ?rhs*)
	   ,?body ,@?body*))))
    ))


;;;; module non-core-macro-transformer: TRACE-LAMBDA, TRACE-DEFINE and TRACE-DEFINE-SYNTAX

(define trace-lambda-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ who (fmls ...) b b* ...)
       (if (valid-bound-ids? fmls)
	   (bless `(make-traced-procedure ',who
					  (lambda ,fmls ,b . ,b*)))
	 (%error-invalid-formals-syntax stx fmls)))
      ((_  who (fmls ... . last) b b* ...)
       (if (valid-bound-ids? (cons last fmls))
	   (bless `(make-traced-procedure ',who
					  (lambda (,@fmls . ,last) ,b . ,b*)))
	 (%error-invalid-formals-syntax stx (append fmls last)))))))

(define trace-define-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ (who fmls ...) b b* ...)
       (if (valid-bound-ids? fmls)
	   (bless `(define ,who
		     (make-traced-procedure ',who
					    (lambda ,fmls ,b . ,b*))))
	 (%error-invalid-formals-syntax stx fmls)))
      ((_ (who fmls ... . last) b b* ...)
       (if (valid-bound-ids? (cons last fmls))
	   (bless `(define ,who
		     (make-traced-procedure ',who
					    (lambda (,@fmls . ,last) ,b . ,b*))))
	 (%error-invalid-formals-syntax stx (append fmls last))))
      ((_ who expr)
       (if (identifier? who)
	   (bless `(define ,who
		     (let ((v ,expr))
		       (if (procedure? v)
			   (make-traced-procedure ',who v)
			 v))))
	 (stx-error stx "invalid name"))))))

(define trace-define-syntax-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ who expr)
       (if (identifier? who)
	   (bless
	    `(define-syntax ,who
	       (make-traced-macro ',who ,expr)))
	 (stx-error stx "invalid name"))))))


;;;; module non-core-macro-transformer: TRACE-LET, TRACE-LET-SYNTAX, TRACE-LETREC-SYNTAX

(define trace-let/rec-syntax
  (lambda (who)
    (lambda (stx)
      (syntax-match stx ()
	((_ ((lhs* rhs*) ...) b b* ...)
	 (if (valid-bound-ids? lhs*)
	     (let ((rhs* (map (lambda (lhs rhs)
				`(make-traced-macro ',lhs ,rhs))
			   lhs* rhs*)))
	       (bless `(,who ,(map list lhs* rhs*) ,b . ,b*)))
	   (%error-invalid-formals-syntax stx lhs*)))))))

(define trace-let-syntax-macro
  (trace-let/rec-syntax 'let-syntax))

(define trace-letrec-syntax-macro
  (trace-let/rec-syntax 'letrec-syntax))


;;;; module non-core-macro-transformer: GUARD

(define guard-macro
  (lambda (x)
    (define (gen-clauses raised-obj con outerk clause*)
      (define (f x k)
	(syntax-match x (=>)
	  ((e => p)
	   (let ((t (gensym)))
	     `(let ((,t ,e))
		(if ,t (,p ,t) ,k))))
	  ((e)
	   (let ((t (gensym)))
	     `(let ((,t ,e))
		(if ,t ,t ,k))))
	  ((e v v* ...)
	   `(if ,e (begin ,v ,@v*) ,k))
	  (_ (stx-error x "invalid guard clause"))))
      (define (f* x*)
	(syntax-match x* (else)
	  (()
	   (let ((g (gensym)))
	     (values `(,g (lambda () (raise-continuable ,raised-obj))) g)))
	  (((else e e* ...))
	   (values `(begin ,e ,@e*) #f))
	  ((cls . cls*)
	   (let-values (((e g) (f* cls*)))
	     (values (f cls e) g)))
	  (others (stx-error others "invalid guard clause"))))
      (let-values (((code raisek) (f* clause*)))
	(if raisek
	    `((call/cc
                  (lambda (,raisek)
                    (,outerk
		     (lambda () ,code)))))
	  `(,outerk (lambda () ,code)))))
    (syntax-match x ()
      ((_ (con clause* ...) b b* ...)
       (identifier? con)
       (let ((outerk     (gensym))
	     (raised-obj (gensym)))
	 (bless
	  `((call/cc
		(lambda (,outerk)
		  (lambda ()
		    (with-exception-handler
			(lambda (,raised-obj)
			  (let ((,con ,raised-obj))
			    ,(gen-clauses raised-obj con outerk clause*)))
		      (lambda () ,b ,@b*))))))))))))


;;;; module non-core-macro-transformer: DEFINE-ENUMERATION

(define (define-enumeration-macro stx)
  (define who 'define-enumeration)
  (define (set? x)
    (or (null? x)
	(and (not (memq (car x) (cdr x)))
	     (set? (cdr x)))))
  (define (remove-dups ls)
    (if (null? ls)
	'()
      (cons (car ls)
	    (remove-dups (remq (car ls) (cdr ls))))))
  (syntax-match stx ()
    ((_ name (id* ...) maker)
     (begin
       (unless (identifier? name)
	 (syntax-violation who
	   "expected identifier as enumeration type name" stx name))
       (unless (for-all identifier? id*)
	 (syntax-violation who
	   "expected list of symbols as enumeration elements" stx id*))
       (unless (identifier? maker)
	 (syntax-violation who
	   "expected identifier as enumeration constructor syntax name" stx maker))
       (let ((name*		(remove-dups (syntax->datum id*)))
	     (the-constructor	(gensym)))
	 (bless
	  `(begin
	     (define ,the-constructor
	       (enum-set-constructor (make-enumeration ',name*)))

	     (define-syntax ,name
	       ;;Check at macro-expansion time whether the symbol ?ARG
	       ;;is in  the universe associated with NAME.   If it is,
	       ;;the result  of the  expansion is equivalent  to ?ARG.
	       ;;It is a syntax violation if it is not.
	       ;;
	       (lambda (x)
		 (define universe-of-symbols ',name*)
		 (define (%synner message subform)
		   (syntax-violation ',name message
				     (syntax->datum x) (syntax->datum subform)))
		 (syntax-case x ()
		   ((_ ?arg)
		    (not (identifier? (syntax ?arg)))
		    (%synner "expected symbol as argument to enumeration validator"
			     (syntax ?arg)))

		   ((_ ?arg)
		    (not (memq (syntax->datum (syntax ?arg)) universe-of-symbols))
		    (%synner "expected symbol in enumeration as argument to enumeration validator"
			     (syntax ?arg)))

		   ((_ ?arg)
		    (syntax (quote ?arg)))

		   (_
		    (%synner "invalid enumeration validator form" #f)))))

	     (define-syntax ,maker
	       ;;Given  any  finite sequence  of  the  symbols in  the
	       ;;universe, possibly  with duplicates, expands  into an
	       ;;expression that  evaluates to the  enumeration set of
	       ;;those symbols.
	       ;;
	       ;;Check  at macro-expansion  time  whether every  input
	       ;;symbol is in the universe associated with NAME; it is
	       ;;a syntax violation if one or more is not.
	       ;;
	       (lambda (x)
		 (define universe-of-symbols ',name*)
		 (define (%synner message subform-stx)
		   (syntax-violation ',maker message
				     (syntax->datum x) (syntax->datum subform-stx)))
		 (syntax-case x ()
		   ((_ . ?list-of-symbols)
		    ;;Check the input  symbols one by one partitioning
		    ;;the ones in the universe from the one not in the
		    ;;universe.
		    ;;
		    ;;If  an input element  is not  a symbol:  raise a
		    ;;syntax violation.
		    ;;
		    ;;After   all   the   input  symbols   have   been
		    ;;partitioned,  if the  list of  collected INvalid
		    ;;ones is not null:  raise a syntax violation with
		    ;;that list as  subform, else return syntax object
		    ;;expression   building  a  new   enumeration  set
		    ;;holding the list of valid symbols.
		    ;;
		    (let loop ((valid-symbols-stx	'())
			       (invalid-symbols-stx	'())
			       (input-symbols-stx	(syntax ?list-of-symbols)))
		      (syntax-case input-symbols-stx ()

			;;No more symbols to collect and non-null list
			;;of collected INvalid symbols.
			(()
			 (not (null? invalid-symbols-stx))
			 (%synner "expected symbols in enumeration as arguments \
                                     to enumeration constructor syntax"
				  (reverse invalid-symbols-stx)))

			;;No more symbols to  collect and null list of
			;;collected INvalid symbols.
			(()
			 (quasisyntax
			  (,the-constructor '(unsyntax (reverse valid-symbols-stx)))))

			;;Error if element is not a symbol.
			((?symbol0 . ?rest)
			 (not (identifier? (syntax ?symbol0)))
			 (%synner "expected symbols as arguments to enumeration constructor syntax"
				  (syntax ?symbol0)))

			;;Collect a symbol in the set.
			((?symbol0 . ?rest)
			 (memq (syntax->datum (syntax ?symbol0)) universe-of-symbols)
			 (loop (cons (syntax ?symbol0) valid-symbols-stx)
			       invalid-symbols-stx (syntax ?rest)))

			;;Collect a symbol not in the set.
			((?symbol0 . ?rest)
			 (loop valid-symbols-stx
			       (cons (syntax ?symbol0) invalid-symbols-stx)
			       (syntax ?rest)))

			))))))
	     )))))
    ))


;;;; module non-core-macro-transformer: DO

(define do-macro
  (lambda (stx)
    (define bind
      (lambda (x)
	(syntax-match x ()
	  ((x init)      `(,x ,init ,x))
	  ((x init step) `(,x ,init ,step))
	  (_  (stx-error stx "invalid binding")))))
    (syntax-match stx ()
      ((_ (binding* ...)
	  (test expr* ...)
	  command* ...)
       (syntax-match (map bind binding*) ()
	 (((x* init* step*) ...)
	  (if (valid-bound-ids? x*)
	      (bless
	       `(letrec ((loop
			  (lambda ,x*
			    (if ,test
				(begin (if #f #f) ,@expr*)
			      (begin
				,@command*
				(loop ,@step*))))))
		  (loop ,@init*)))
	    (stx-error stx "invalid bindings"))))))))


;;;; module non-core-macro-transformer: RETURN, CONTINUE, BREAK, WHILE, UNTIL, FOR

(define (return-macro expr-stx)
  (syntax-match expr-stx ()
    ((_)
     (bless
      (lambda (stx)
	(syntax-error 'return "syntax \"return\" out of context"))))))

(define (continue-macro expr-stx)
  (syntax-match expr-stx ()
    ((_)
     (bless
      (lambda (stx)
	(syntax-error 'continue "syntax \"continue\" out of any loop"))))))

(define (break-macro expr-stx)
  (syntax-match expr-stx ()
    ((_)
     (bless
      (lambda (stx)
	(syntax-error 'break "syntax \"continue\" out of any loop"))))))

(define (while-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (begin ,@?body* (loop))
		   (escape))))))))
    ))

(define (until-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (escape)
		   (begin ,@?body* (loop)))))))))
    ))

(define (for-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ (?init ?test ?incr) ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     ,?init
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (begin
		       ,@?body* ,?incr
		       (loop))
		   (escape))))))))
    ))


;;;; module non-core-macro-transformer: DEFINE-RETURNABLE, LAMBDA-RETURNABLE

(define (define-returnable-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ (?name . ?formals) ?body0 ?body* ...)
     (bless
      `(define (,?name . ,?formals)
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ,?body0 ,@?body*))))))
    ))

(define (lambda-returnable-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?formals ?body0 ?body* ...)
     (bless
      `(lambda ,?formals
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ,?body0 ,@?body*))))))
    ))

(define (begin-returnable-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?body0 ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (fluid-let-syntax ((return (syntax-rules ()
					  ((_ . ?args)
					   (escape . ?args)))))
	       ,?body0 ,@?body*)))))
    ))


;;;; module non-core-macro-transformer: OR, AND

(define or-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_) #f)
      ((_ e e* ...)
       (bless
	(let f ((e e) (e* e*))
	  (cond
	   ((null? e*) `(begin #f ,e))
	   (else
	    `(let ((t ,e))
	       (if t t ,(f (car e*) (cdr e*))))))))))))

(define and-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_) #t)
      ((_ e e* ...)
       (bless
	(let f ((e e) (e* e*))
	  (cond
	   ((null? e*) `(begin #f ,e))
	   (else `(if ,e ,(f (car e*) (cdr e*)) #f)))))))))


;;;; module non-core-macro-transformer: COND

(define cond-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ cls cls* ...)
       (bless
	(let f ((cls cls) (cls* cls*))
	  (cond
	   ((null? cls*)
	    (syntax-match cls (else =>)
	      ((else e e* ...) `(let () #f ,e . ,e*))
	      ((e => p) `(let ((t ,e)) (if t (,p t))))
	      ((e) `(or ,e (if #f #f)))
	      ((e e* ...) `(if ,e (begin . ,e*)))
	      (_ (stx-error stx "invalid last clause"))))
	   (else
	    (syntax-match cls (else =>)
	      ((else e e* ...) (stx-error stx "incorrect position of keyword else"))
	      ((e => p) `(let ((t ,e)) (if t (,p t) ,(f (car cls*) (cdr cls*)))))
	      ((e) `(or ,e ,(f (car cls*) (cdr cls*))))
	      ((e e* ...) `(if ,e (begin . ,e*) ,(f (car cls*) (cdr cls*))))
	      (_ (stx-error stx "invalid last clause")))))))))))


;;;; module non-core-macro-transformer: QUASIQUOTE

(define quasiquote-macro
  (let ()
    (define (datum x)
      (list (scheme-stx 'quote) (mkstx x top-mark* '() '())))
    (define-syntax app
      (syntax-rules (quote)
	((_ 'x arg* ...)
	 (list (scheme-stx 'x) arg* ...))))
    (define-syntax app*
      (syntax-rules (quote)
	((_ 'x arg* ... last)
	 (cons* (scheme-stx 'x) arg* ... last))))
    (define quasicons*
      (lambda (x y)
	(let f ((x x))
	  (if (null? x) y (quasicons (car x) (f (cdr x)))))))
    (define quasicons
      (lambda (x y)
	(syntax-match y (quote list)
	  ((quote dy)
	   (syntax-match x (quote)
	     ((quote dx) (app 'quote (cons dx dy)))
	     (_
	      (syntax-match dy ()
		(() (app 'list x))
		(_  (app 'cons x y))))))
	  ((list stuff ...)
	   (app* 'list x stuff))
	  (_ (app 'cons x y)))))
    (define quasiappend
      (lambda (x y)
	(let ((ls (let f ((x x))
		    (if (null? x)
			(syntax-match y (quote)
			  ((quote ()) '())
			  (_ (list y)))
		      (syntax-match (car x) (quote)
			((quote ()) (f (cdr x)))
			(_ (cons (car x) (f (cdr x)))))))))
	  (cond
	   ((null? ls) (app 'quote '()))
	   ((null? (cdr ls)) (car ls))
	   (else (app* 'append ls))))))
    (define quasivector
      (lambda (x)
	(let ((pat-x x))
	  (syntax-match pat-x (quote)
	    ((quote (x* ...)) (app 'quote (list->vector x*)))
	    (_ (let f ((x x) (k (lambda (ls) (app* 'vector ls))))
		 (syntax-match x (quote list cons)
		   ((quote (x* ...))
		    (k (map (lambda (x) (app 'quote x)) x*)))
		   ((list x* ...)
		    (k x*))
		   ((cons x y)
		    (f y (lambda (ls) (k (cons x ls)))))
		   (_ (app 'list->vector pat-x)))))))))
    (define vquasi
      (lambda (p lev)
	(syntax-match p ()
	  ((p . q)
	   (syntax-match p (unquote unquote-splicing)
	     ((unquote p ...)
	      (if (= lev 0)
		  (quasicons* p (vquasi q lev))
		(quasicons
		 (quasicons (datum 'unquote)
			    (quasi p (- lev 1)))
		 (vquasi q lev))))
	     ((unquote-splicing p ...)
	      (if (= lev 0)
		  (quasiappend p (vquasi q lev))
		(quasicons
		 (quasicons
		  (datum 'unquote-splicing)
		  (quasi p (- lev 1)))
		 (vquasi q lev))))
	     (p (quasicons (quasi p lev) (vquasi q lev)))))
	  (() (app 'quote '())))))
    (define quasi
      (lambda (p lev)
	(syntax-match p (unquote unquote-splicing quasiquote)
	  ((unquote p)
	   (if (= lev 0)
	       p
	     (quasicons (datum 'unquote) (quasi (list p) (- lev 1)))))
	  (((unquote p ...) . q)
	   (if (= lev 0)
	       (quasicons* p (quasi q lev))
	     (quasicons
	      (quasicons (datum 'unquote)
			 (quasi p (- lev 1)))
	      (quasi q lev))))
	  (((unquote-splicing p ...) . q)
	   (if (= lev 0)
	       (quasiappend p (quasi q lev))
	     (quasicons
	      (quasicons (datum 'unquote-splicing)
			 (quasi p (- lev 1)))
	      (quasi q lev))))
	  ((quasiquote p)
	   (quasicons (datum 'quasiquote)
		      (quasi (list p) (+ lev 1))))
	  ((p . q) (quasicons (quasi p lev) (quasi q lev)))
	  (#(x ...) (not (<stx>? x)) (quasivector (vquasi x lev)))
	  (p (app 'quote p)))))
    (lambda (x)
      (syntax-match x ()
	((_ e) (quasi e 0))))))


;;;; module non-core-macro-transformer: QUASISYNTAX

(define quasisyntax-macro
  (let () ;;; FIXME: not really correct
    (define quasi
      (lambda (p lev)
	(syntax-match p (unsyntax unsyntax-splicing quasisyntax)
	  ((unsyntax p)
	   (if (= lev 0)
	       (let ((g (gensym)))
		 (values (list g) (list p) g))
	     (let-values (((lhs* rhs* p) (quasi p (- lev 1))))
	       (values lhs* rhs* (list 'unsyntax p)))))
	  (unsyntax
	   (= lev 0)
	   (stx-error p "incorrect use of unsyntax"))
	  (((unsyntax p* ...) . q)
	   (let-values (((lhs* rhs* q) (quasi q lev)))
	     (if (= lev 0)
		 (let ((g* (map (lambda (x) (gensym)) p*)))
		   (values
		    (append g* lhs*)
		    (append p* rhs*)
		    (append g* q)))
	       (let-values (((lhs2* rhs2* p*) (quasi p* (- lev 1))))
		 (values
		  (append lhs2* lhs*)
		  (append rhs2* rhs*)
		  `((unsyntax . ,p*) . ,q))))))
	  (((unsyntax-splicing p* ...) . q)
	   (let-values (((lhs* rhs* q) (quasi q lev)))
	     (if (= lev 0)
		 (let ((g* (map (lambda (x) (gensym)) p*)))
		   (values
		    (append
		     (map (lambda (g) `(,g ...)) g*)
		     lhs*)
		    (append p* rhs*)
		    (append
		     (apply append
			    (map (lambda (g) `(,g ...)) g*))
		     q)))
	       (let-values (((lhs2* rhs2* p*) (quasi p* (- lev 1))))
		 (values
		  (append lhs2* lhs*)
		  (append rhs2* rhs*)
		  `((unsyntax-splicing . ,p*) . ,q))))))
	  (unsyntax-splicing (= lev 0)
			     (stx-error p "incorrect use of unsyntax-splicing"))
	  ((quasisyntax p)
	   (let-values (((lhs* rhs* p) (quasi p (+ lev 1))))
	     (values lhs* rhs* `(quasisyntax ,p))))
	  ((p . q)
	   (let-values (((lhs* rhs* p) (quasi p lev))
			((lhs2* rhs2* q) (quasi q lev)))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     (cons p q))))
	  (#(x* ...)
	   (let-values (((lhs* rhs* x*) (quasi x* lev)))
	     (values lhs* rhs* (list->vector x*))))
	  (_ (values '() '() p)))))
    (lambda (x)
      (syntax-match x ()
	((_ e)
	 (let-values (((lhs* rhs* v) (quasi e 0)))
	   (bless
	    `(syntax-case (list ,@rhs*) ()
	       (,lhs* (syntax ,v))))))))))


;;;; module non-core-macro-transformer: DEFINE-VALUES, DEFINE-CONSTANT-VALUES

(define (define-values-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  DEFINE-VALUES macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ (?var* ... ?var0) ?form* ... ?form0)
     (let ((TMP* (generate-temporaries ?var*)))
       (bless
	`(begin
	   ;;We must make sure that the ?FORMs do not capture the ?VARs.
	   (define (return-multiple-values)
	     ,@?form* ,?form0)
	   ,@(map (lambda (var)
		    `(define ,var #f))
	       ?var*)
	   (define ,?var0
	     (call-with-values
		 return-multiple-values
	       (lambda (,@TMP* T0)
		 ,@(map (lambda (var TMP)
			  `(set! ,var ,TMP))
		     ?var* TMP*)
		 T0)))
	   ))))
    ))

(define (define-constant-values-macro expr-stx)
  ;;Transformer function used  to expand Vicare's DEFINE-CONSTANT-VALUES
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ (?var* ... ?var0) ?form* ... ?form0)
     (let ((SHADOW* (generate-temporaries ?var*))
	   (TMP*    (generate-temporaries ?var*)))
       (bless
	`(begin
	   (define (return-multiple-values)
	     ,@?form* ,?form0)
	   ,@(map (lambda (SHADOW)
		    `(define ,SHADOW #f))
	       SHADOW*)
	   (define SHADOW0
	     (call-with-values
		 return-multiple-values
	       (lambda (,@TMP* T0)
		 ,@(map (lambda (SHADOW TMP)
			  `(set! ,SHADOW ,TMP))
		     SHADOW* TMP*)
		 T0)))
	   ,@(map (lambda (var SHADOW)
		    `(define-syntax ,var
		       (identifier-syntax ,SHADOW)))
	       ?var* SHADOW*)
	   (define-syntax ,?var0
	     (identifier-syntax SHADOW0))
	   ))))
    ))


;;;; module non-core-macro-transformer: RECEIVE, RECEIVE-AND-RETURN, BEGIN0, XOR

(define (receive-macro expr-stx)
  ;;Transformer function used to expand Vicare's RECEIVE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX.
  ;;Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?producer-expression ?form0 ?form* ...)
     (bless
      `(call-with-values
	   (lambda () ,?producer-expression)
	 (lambda ,?formals ,?form0 ,@?form*))))
    ))

(define (receive-and-return-macro expr-stx)
  ;;Transformer  function  used  to expand  Vicare's  RECEIVE-AND-RETURN
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ (?retval* ...) ?producer-expression ?body0 ?body* ...)
     (bless
      `(call-with-values
	   (lambda () ,?producer-expression)
	 (lambda ,?retval*
	   ,?body0 ,@?body*
	   (values ,@?retval*)))))
    ))

(define (begin0-macro expr-stx)
  ;;Transformer function used to expand  Vicare's BEGIN0 macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX.
  ;;Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?form0 ?form* ...)
     (bless
      `(call-with-values
	   (lambda () ,?form0)
	 (lambda args
	   ,@?form*
	   (apply values args)))))
    ))

(module (xor-macro)

  (define (xor-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr* ...)
       (bless (%xor-aux #f ?expr*)))
      ))

  (define (%xor-aux bool/var expr*)
    (cond ((null? expr*)
	   bool/var)
	  ((null? (cdr expr*))
	   `(let ((x ,(car expr*)))
	      (if ,bool/var
		  (and (not x) ,bool/var)
		x)))
	  (else
	   `(let ((x ,(car expr*)))
	      (and (or (not ,bool/var)
		       (not x))
		   (let ((n (or ,bool/var x)))
		     ,(%xor-aux 'n (cdr expr*))))))))

  #| end of module: XOR-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-INLINE, DEFINE-CONSTANT

(define (define-constant-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?name ?expr)
     (bless
      `(begin
	 (define ghost ,?expr)
	 (define-syntax ,?name
	   (identifier-syntax ghost)))))
    ))

(define (define-inline-constant-macro expr-stx)
  ;;Transformer function used  to expand Vicare's DEFINE-INLINE-CONSTANT
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  ;;We want to allow a generic expression to generate the constant value
  ;;at expand time.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?expr)
     (bless
      `(define-syntax ,?name
	 (let ((const ,?expr))
	   (lambda (stx)
	     (syntax-case stx ()
	       (?id
		(identifier? #'?id)
		#`(quote #,const))))))))
    ))

(define (define-inline-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  DEFINE-INLINE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ (?name ?arg* ... . ?rest) ?form0 ?form* ...)
     (and (identifier? ?name)
	  (for-all identifier? ?arg*)
	  (or (null? (syntax->datum ?rest))
	      (identifier? ?rest)))
     (let ((TMP* (generate-temporaries ?arg*)))
       (bless
	`(define-fluid-syntax ,?name
	   (syntax-rules ()
	     ((_ ,@TMP* . rest)
	      (fluid-let-syntax
		  ((,?name (lambda (stx)
			     (syntax-violation (quote ,?name)
			       "cannot recursively expand inline expression"
			       stx))))
		(let ,(append (map list ?arg* TMP*)
			      (if (null? (syntax->datum ?rest))
				  '()
				`((,?rest (list . rest)))))
		  ,?form0 ,@?form*))))))))
    ))


;;;; module non-core-macro-transformer: INCLUDE

(module (include-macro)
  ;;Transformer function used to expand Vicare's INCLUDE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX.
  ;;Return a symbolic expression in the core language.
  ;;
  (define who 'include)

  (define (include-macro expr-stx)
    (define (%synner message subform)
      (syntax-violation who message expr-stx subform))
    (syntax-match expr-stx ()
      ((?context ?filename)
       (%include-file ?filename ?context #f %synner))
      ((?context ?filename #t)
       (%include-file ?filename ?context #t %synner))
      ))

  (define (%include-file filename-stx context-id verbose? synner)
    (when verbose?
      (display (string-append "Vicare: searching include file: "
			      (syntax->datum filename-stx) "\n")
	       (current-error-port)))
    (let ((pathname (%filename-stx->pathname filename-stx synner)))
      (when verbose?
	(display (string-append "Vicare: including file: " pathname "\n")
		 (current-error-port)))
      (bless
       `(stale-when (let ()
		      (import (only (vicare $posix)
				    file-modification-time))
		      (or (not (file-exists? ,pathname))
			  (> (file-modification-time ,pathname)
			     ,(file-modification-time pathname))))
	  ,@(%read-content context-id pathname)))))

  (define (%filename-stx->pathname filename-stx synner)
    ;;Convert  the  string  FILENAME  into the  string  pathname  of  an
    ;;existing file; return the pathname.
    ;;
    (define filename
      (syntax->datum filename-stx))
    (unless (and (string? filename)
		 (not (fxzero? (string-length filename))))
      (synner "file name must be a nonempty string" filename-stx))
    (if (char=? (string-ref filename 0) #\/)
	;;It is an absolute pathname.
	(real-pathname filename)
      ;;It is a relative pathname.  Search the file in the library path.
      (let loop ((ls (library-path)))
	(if (null? ls)
	    (synner "file does not exist in library path" filename-stx)
	  (let ((ptn (string-append (car ls) "/" filename)))
	    (if (file-exists? ptn)
		(real-pathname ptn)
	      (loop (cdr ls))))))))

  (define (%read-content context-id pathname)
    ;;Open the  file PATHNAME, read all  the datums and convert  them to
    ;;syntax object  in the  lexical context  of CONTEXT-ID;  return the
    ;;resulting syntax object.
    ;;
    (with-exception-handler
	(lambda (E)
	  (raise-continuable (condition (make-who-condition who) E)))
      (lambda ()
	(with-input-from-file pathname
	  (lambda ()
	    (let recur ()
	      (let ((datum (get-annotated-datum (current-input-port))))
		(if (eof-object? datum)
		    '()
		  (cons (datum->syntax context-id datum)
			(recur))))))))))

  #| end of module: INCLUDE-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-INTEGRABLE

(define (define-integrable-macro expr-stx)
  ;;The original  syntax was  posted by "leppie"  on the  Ikarus mailing
  ;;list; subject "Macro Challenge of Last Year [Difficulty: *****]", 20
  ;;Oct 2009.
  ;;
  (syntax-match expr-stx (lambda)
    ((_ (?name . ?formals) ?form0 ?form* ...)
     (identifier? ?name)
     (bless `(define-integrable ,?name (lambda ,?formals ,?form0 ,@?form*))))

    ((_ ?name (lambda ?formals ?form0 ?form* ...))
     (identifier? ?name)
     (bless
      `(begin
	 (define-fluid-syntax ,?name
	   (lambda (x)
	     (syntax-case x ()
	       (_
		(identifier? x)
		#'xname)

	       ((_ arg ...)
		#'((fluid-let-syntax
		       ((,?name (identifier-syntax xname)))
		     (lambda ,?formals ,?form0 ,@?form*))
		   arg ...)))))
	 (define xname
	   (fluid-let-syntax ((,?name (identifier-syntax xname)))
	     (lambda ,?formals ,?form0 ,@?form*)))
	 )))
    ))


;;;; module non-core-macro-transformer: miscellanea

(define time-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ expr)
       (let ((str
	      (let-values (((p e) (open-string-output-port)))
		(write (syntax->datum expr) p)
		(e))))
	 (bless `(time-it ,str (lambda () ,expr))))))))

(define delay-macro
  (lambda (stx)
    (syntax-match stx ()
      ((_ expr)
       (bless `(make-promise (lambda () ,expr)))))))

(define (assert-macro stx)
  ;;Defined by R6RS.  An ASSERT  form is evaluated by evaluating EXPR.
  ;;If  EXPR returns a  true value,  that value  is returned  from the
  ;;ASSERT  expression.   If EXPR  returns  false,  an exception  with
  ;;condition  types  "&assertion"  and  "&message"  is  raised.   The
  ;;message  provided  in   the  condition  object  is  implementation
  ;;dependent.
  ;;
  ;;NOTE  Implementations should  exploit the  fact that  ASSERT  is a
  ;;syntax  to  provide as  much  information  as  possible about  the
  ;;location of the assertion failure.
  ;;
  (syntax-match stx ()
    ((_ expr)
     (let ((pos (or (expression-position stx)
		    (expression-position expr))))
       (bless
	(if (source-position-condition? pos)
	    `(or ,expr
		 (assertion-error
		  ',expr ,(source-position-port-id pos)
		  ,(source-position-byte pos) ,(source-position-character pos)
		  ,(source-position-line pos) ,(source-position-column    pos)))
	  `(or ,expr
	       (assertion-error ',expr "unknown source" #f #f #f #f))))))))

(define (file-options-macro expr-stx)
  ;;Transformer for  the FILE-OPTIONS macro.  File  options selection is
  ;;implemented   as   an   enumeration  type   whose   constructor   is
  ;;MAKE-FILE-OPTIONS from the boot environment.
  ;;
  (define (valid-option? opt-stx)
    (and (identifier? opt-stx)
	 (memq (identifier->symbol opt-stx) '(no-fail no-create no-truncate))))
  (syntax-match expr-stx ()
    ((_ ?opt* ...)
     (for-all valid-option? ?opt*)
     (bless `(make-file-options ',?opt*)))))

(define (endianness-macro expr-stx)
  ;;Transformer of  ENDIANNESS.  Support  the symbols:  "big", "little",
  ;;"network", "native"; convert "network" to "big".
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) '(big little network native)))
     (case (identifier->symbol ?name)
       ((network)
	(bless '(quote big)))
       ((native)
	(bless '(native-endianness)))
       ((big little)
	(bless `(quote ,?name)))))))

(define (%allowed-symbol-macro expr-stx allowed-symbol-set)
  ;;Helper  function used  to  implement the  transformer of:  EOL-STYLE
  ;;ERROR-HANDLING-MODE, BUFFER-MODE,  ENDIANNESS.  All of  these macros
  ;;should expand to a quoted symbol among a list of allowed ones.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) allowed-symbol-set))
     (bless `(quote ,?name)))))


;;; end of module: NON-CORE-MACRO-TRANSFORMER

)


(module (core-macro-transformer)
  ;;We distinguish between "non-core macros" and "core macros".
  ;;
  ;;Core macros  are part of the  core language: they cannot  be further
  ;;expanded to a  composition of other more basic  macros.  Core macros
  ;;*do*  introduce bindings,  so their  transformer functions  take the
  ;;lexical environments as arguments.
  ;;
  ;;Non-core macros are  *not* part of the core language:  they *can* be
  ;;expanded to  a composition of  core macros.  Non-core macros  do not
  ;;introduce bindings, so their transformer functions do *not* take the
  ;;lexical environments as arguments.
  ;;
  ;;The function  CORE-MACRO-TRANSFORMER maps symbols  representing core
  ;;macros to  their macro transformers.   The expression returned  by a
  ;;core transformer is expressed in the core language and does not need
  ;;to be further processed.
  ;;
  ;;NOTE This  module is very  long, so it  is split into  multiple code
  ;;pages.  (Marco Maggi; Sat Apr 27, 2013)
  ;;
  (define (core-macro-transformer name)
    (define who 'core-macro-transformer)
    (case name
      ((quote)				quote-transformer)
      ((lambda)				lambda-transformer)
      ((case-lambda)			case-lambda-transformer)
      ((letrec)				letrec-transformer)
      ((letrec*)			letrec*-transformer)
      ((if)				if-transformer)
      ((foreign-call)			foreign-call-transformer)
      ((syntax-case)			syntax-case-transformer)
      ((syntax)				syntax-transformer)
      ((type-descriptor)		type-descriptor-transformer)
      ((record-type-descriptor)		record-type-descriptor-transformer)
      ((record-constructor-descriptor)	record-constructor-descriptor-transformer)
      ((record-type-field-set!)		record-type-field-set!-transformer)
      ((record-type-field-ref)		record-type-field-ref-transformer)
      (($record-type-field-set!)	$record-type-field-set!-transformer)
      (($record-type-field-ref)		$record-type-field-ref-transformer)
      ((fluid-let-syntax)		fluid-let-syntax-transformer)
      (else
       (assertion-violation who
	 "Vicare: internal error: cannot find transformer" name))))


;;;; module core-macro-transformer: LETREC and LETREC*

(module (letrec-transformer letrec*-transformer)

  (define (letrec-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function  used to  expand  LETREC  syntaxes from  the
    ;;top-level built  in environment.  Expand the  contents of EXPR-STX
    ;;in  the  context  of   the  lexical  environments  LEXENV.RUN  and
    ;;LEXENV.EXPAND; return a  symbolic expression representing EXPR-STX
    ;;fully expanded to the core language.
    ;;
    (%letrec-helper expr-stx lexenv.run lexenv.expand build-letrec))

  (define (letrec*-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to  expand LETREC*  syntaxes from  the
    ;;top-level built  in environment.  Expand the  contents of EXPR-STX
    ;;in  the  context  of   the  lexical  environments  LEXENV.RUN  and
    ;;LEXENV.EXPAND; return a  symbolic expression representing EXPR-STX
    ;;fully expanded to the core language.
    ;;
    (%letrec-helper expr-stx lexenv.run lexenv.expand build-letrec*))

  (define (%letrec-helper expr-stx lexenv.run lexenv.expand core-lang-builder)
    (syntax-match expr-stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check  that  the  binding  names are  identifiers  and  without
       ;;duplicates.
       (if (not (valid-bound-ids? ?lhs*))
	   (%error-invalid-formals-syntax expr-stx ?lhs*)
	 ;;Generate  unique variable  names  and labels  for the  LETREC
	 ;;bindings.
	 (let ((lex* (map gensym-for-lexical-var ?lhs*))
	       (lab* (map gensym-for-label       ?lhs*)))
	   ;;Generate  what is  needed to  create a  lexical contour:  a
	   ;;<RIB>  and  an extended  lexical  environment  in which  to
	   ;;evaluate both the right-hand sides and the body.
	   ;;
	   ;;Notice that the region of  all the LETREC bindings includes
	   ;;all the right-hand sides.
	   (let ((rib        (make-full-rib ?lhs* lab*))
		 (lexenv.run (add-lexicals lab* lex* lexenv.run)))
	     ;;Create  the   lexical  contour  then  process   body  and
	     ;;right-hand sides of bindings.
	     (let ((body (chi-internal (push-lexical-contour rib (cons ?body ?body*))
				       lexenv.run lexenv.expand))
		   (rhs* (chi-expr*    (map (lambda (rhs)
					      (push-lexical-contour rib rhs))
					 ?rhs*)
				       lexenv.run lexenv.expand)))
	       ;;Build  the LETREC  or  LETREC* expression  in the  core
	       ;;language.
	       (core-lang-builder no-source lex* rhs* body))))))))

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(define (fluid-let-syntax-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand FLUID-LET-SYNTAX  syntaxes from
  ;;the top-level built in environment.  Expand the contents of EXPR-STX
  ;;in  the   context  of   the  lexical  environments   LEXENV.RUN  and
  ;;LEXENV.EXPAND;  return a  symbolic expression  representing EXPR-STX
  ;;fully expanded to the core language.
  ;;
  (define who 'expander)

  (define (transformer expr-stx)
    (syntax-match expr-stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the ?LHS* are all identifiers with no duplicates.
       (if (not (valid-bound-ids? ?lhs*))
	   (%error-invalid-formals-syntax expr-stx ?lhs*)
	 (let ((label*       (map %lookup-binding-in-run-lexenv ?lhs*))
	       (rhs-binding* (map (lambda (rhs)
				    (make-eval-transformer
				     (%expand-macro-transformer rhs lexenv.expand)))
			       ?rhs*)))
	   (chi-internal (cons ?body ?body*)
			 (append (map cons label* rhs-binding*) lexenv.run)
			 (append (map cons label* rhs-binding*) lexenv.expand)))))))

  (define (%lookup-binding-in-run-lexenv lhs)
    ;;Search  the  binding of  the  identifier  LHS in  LEXENV.RUN,  the
    ;;environment for run;  if present and of type  fluid syntax: return
    ;;the associated label.
    ;;
    (let* ((label    (or (id->label lhs)
			 (%synner "unbound identifier" lhs)))
	   (binding  (label->binding/no-fluids label lexenv.run)))
      (cond ((%fluid-syntax-binding? binding)
	     (binding-value binding))
	    (else
	     (%synner "not a fluid identifier" lhs)))))

  (define (%fluid-syntax-binding? binding)
    (and (pair? binding)
	 (eq? (binding-type binding) '$fluid)))

  (define (%synner message subform)
    (stx-error subform message)
    #;(syntax-violation who message expr-stx subform))

  (transformer expr-stx))


;;;; module core-macro-transformer: TYPE-DESCRIPTOR

(define (type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function   used  to  expand   Vicare's  TYPE-DESCRIPTOR
  ;;syntaxes  from  the  top-level  built in  environment.   Expand  the
  ;;contents  of EXPR-STX  in the  context of  the lexical  environments
  ;;LEXENV.RUN and LEXENV.EXPAND, the result must be a single identifier
  ;;representing  a Vicare  struct type.   Return a  symbolic expression
  ;;evaluating to the struct type descriptor.
  ;;
  ;;The binding in the lexical  environment representing the struct type
  ;;descriptor looks as follows:
  ;;
  ;;   ($rtd . #<type-descriptor-struct>)
  ;;    |..| binding-type
  ;;           |.......................|  binding-value
  ;;   |................................| binding
  ;;
  ;;where "$rtd" is the symbol "$rtd".
  ;;
  (define who 'type-descriptor)
  (define (%struct-type-descriptor-binding? binding)
    (and (eq? '$rtd (binding-type binding))
	 (not (list? (binding-value binding)))))
  (syntax-match expr-stx ()
    ((_ ?identifier)
     (identifier? ?identifier)
     (let ((label (id->label ?identifier)))
       (unless label
	 (%raise-unbound-error who expr-stx ?identifier))
       (let ((binding (label->binding label lexenv.run)))
	 (unless (%struct-type-descriptor-binding? binding)
	   (syntax-violation who "not a struct type" expr-stx ?identifier))
	 (build-data no-source (binding-value binding)))))))


;;;; module core-macro-transformer: RECORD-{TYPE,CONSTRUCTOR}-DESCRIPTOR-TRANSFORMER

(module (record-type-descriptor-transformer
	 record-constructor-descriptor-transformer
	 record-type-field-set!-transformer
	 record-type-field-ref-transformer
	 $record-type-field-set!-transformer
	 $record-type-field-ref-transformer)
  ;;The entry  in the lexical  environment representing the  record type
  ;;and constructor descriptors looks as follows:
  ;;
  ;;   ($rtd . (?rtd-id ?rcd-id))
  ;;    |..| binding-type
  ;;           |...............| binding-value
  ;;   |.......................| binding
  ;;
  ;;where  "$rtd" is  the symbol  "$rtd", ?RTD-ID  is the  identifier to
  ;;which the record type descriptor is bound, ?RCD-ID is the identifier
  ;;to which the default record constructor descriptor is bound.
  ;;
  ;;Optionally 2 or 4 additional fields are present:
  ;;
  ;;   ($rtd . (?rtd-id ?rcd-id
  ;;            ?safe-accessors-alist ?safe-mutators-alist))
  ;;
  ;;   ($rtd . (?rtd-id ?rcd-id
  ;;            ?safe-accessors-alist ?safe-mutators-alist
  ;;            ?unsafe-accessors-alist ?unsafe-mutators-alist))
  ;;
  ;;in which:
  ;;
  ;;*  ?SAFE-ACCESSORS-ALIST   is  an  alist  whose   keys  are  symbols
  ;;representing  all  the   field  names  and  whose   values  are  the
  ;;identifiers bound to the corresponding safe field accessors.
  ;;
  ;;*  ?SAFE-FIELD-MUTATORS   is  an   alist  whose  keys   are  symbols
  ;;representing  the   mutable  field   names  and  whose   values  are
  ;;identifiers bound to the corresponding safe field mutators.
  ;;
  ;;*  ?UNSAFE-ACCESSORS-ALIST  is  an  alist  whose  keys  are  symbols
  ;;representing  all  the   field  names  and  whose   values  are  the
  ;;identifiers bound to the corresponding safe unfield accessors.
  ;;
  ;;*  ?UNSAFE-FIELD-MUTATORS  is  an   alist  whose  keys  are  symbols
  ;;representing  the   mutable  field   names  and  whose   values  are
  ;;identifiers bound to the corresponding unsafe field mutators.
  ;;
  (define (%record-type-descriptor-binding? binding)
    (and (eq? '$rtd (binding-type binding))
	 (list? (binding-value binding))))

  (define (record-type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand R6RS's RECORD-TYPE-DESCRIPTOR
    ;;syntax uses from  the top-level built in  environment.  Expand the
    ;;contents of  EXPR-STX in the  context of the  lexical environments
    ;;LEXENV.RUN  and  LEXENV.EXPAND,  the   result  must  be  a  single
    ;;identifier  representing a  R6RS record  type.  Return  a symbolic
    ;;expression evaluating to the record type descriptor.
    ;;
    (define who 'record-type-descriptor)
    (syntax-match expr-stx ()
      ((_ ?identifier)
       (identifier? ?identifier)
       (let ((label (id->label ?identifier)))
	 (unless label
	   (%raise-unbound-error who expr-stx ?identifier))
	 (let ((binding (label->binding label lexenv.run)))
	   (unless (%record-type-descriptor-binding? binding)
	     (syntax-violation who "not a record type" expr-stx ?identifier))
	   (chi-expr (car (binding-value binding))
		     lexenv.run lexenv.expand))))))

  (define (record-constructor-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer      function      used     to      expand      R6RS's
    ;;RECORD-CONSTRUCTOR-DESCRIPTOR syntax uses from the top-level built
    ;;in environment.  Expand the contents of EXPR-STX in the context of
    ;;the lexical environments LEXENV.RUN  and LEXENV.EXPAND, the result
    ;;must  be a  single  identifier representing  a  R6RS record  type.
    ;;Return a  symbolic expression evaluating to  the record destructor
    ;;descriptor.
    ;;
    (define who 'record-constructor-descriptor-transformer)
    (syntax-match expr-stx ()
      ((_ ?identifier)
       (identifier? ?identifier)
       (let ((label (id->label ?identifier)))
	 (unless label
	   (%raise-unbound-error who expr-stx ?identifier))
	 (let ((binding (label->binding label lexenv.run)))
	   (unless (%record-type-descriptor-binding? binding)
	     (syntax-error who "invalid type" expr-stx ?identifier))
	   (chi-expr (cadr (binding-value binding))
		     lexenv.run lexenv.expand))))))

;;; --------------------------------------------------------------------

  (define (record-type-field-ref-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function  used to expand  R6RS's RECORD-TYPE-FIELD-REF
    ;;syntax uses from  the top-level built in  environment.  Expand the
    ;;contents of  EXPR-STX in the  context of the  lexical environments
    ;;LEXENV.RUN  and  LEXENV.EXPAND.    Return  a  symbolic  expression
    ;;evaluating to the record type descriptor.
    ;;
    (define who 'record-type-field-ref)
    (syntax-match expr-stx ()
      ((_ ?type-name ?field-name ?record)
       (and (identifier? ?type-name)
	    (identifier? ?field-name))
       (let ((label (id->label ?type-name)))
	 (unless label
	   (%raise-unbound-error who expr-stx ?type-name))
	 (let ((binding (label->binding label lexenv.run)))
	   (unless (%record-type-descriptor-binding? binding)
	     (syntax-violation who "not a record type" expr-stx ?type-name))
	   (let* ((table    (%get-alist-of-safe-field-accessors who binding))
		  (accessor (assq (syntax->datum ?field-name) table)))
	     (unless accessor
	       (syntax-violation who "unknown record field name" expr-stx ?field-name))
	     (chi-expr (bless `(,(cdr accessor) ,?record))
		       lexenv.run lexenv.expand)))))))

  (define (record-type-field-set!-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand R6RS's RECORD-TYPE-FIELD-SET!
    ;;syntax uses from  the top-level built in  environment.  Expand the
    ;;contents of  EXPR-STX in the  context of the  lexical environments
    ;;LEXENV.RUN  and  LEXENV.EXPAND.    Return  a  symbolic  expression
    ;;evaluating to the record type descriptor.
    ;;
    (define who 'record-type-field-set!)
    (syntax-match expr-stx ()
      ((_ ?type-name ?field-name ?record ?new-value)
       (and (identifier? ?type-name)
	    (identifier? ?field-name))
       (let ((label (id->label ?type-name)))
	 (unless label
	   (%raise-unbound-error who expr-stx ?type-name))
	 (let ((binding (label->binding label lexenv.run)))
	   (unless (%record-type-descriptor-binding? binding)
	     (syntax-violation who "not a record type" expr-stx ?type-name))
	   (let* ((table   (%get-alist-of-safe-field-mutators who binding))
		  (mutator (assq (syntax->datum ?field-name) table)))
	     (unless mutator
	       (syntax-violation who "unknown record field name or immutable field" expr-stx ?field-name))
	     (chi-expr (bless `(,(cdr mutator) ,?record ,?new-value))
		       lexenv.run lexenv.expand)))))))

  (define ($record-type-field-ref-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand R6RS's $RECORD-TYPE-FIELD-REF
    ;;syntax uses from  the top-level built in  environment.  Expand the
    ;;contents of  EXPR-STX in the  context of the  lexical environments
    ;;LEXENV.RUN  and  LEXENV.EXPAND.    Return  a  symbolic  expression
    ;;evaluating to the record type descriptor.
    ;;
    (define who '$record-type-field-ref)
    (syntax-match expr-stx ()
      ((_ ?type-name ?field-name ?record)
       (and (identifier? ?type-name)
	    (identifier? ?field-name))
       (let ((label (id->label ?type-name)))
	 (unless label
	   (%raise-unbound-error who expr-stx ?type-name))
	 (let ((binding (label->binding label lexenv.run)))
	   (unless (%record-type-descriptor-binding? binding)
	     (syntax-violation who "not a record type" expr-stx ?type-name))
	   (let* ((table    (%get-alist-of-unsafe-field-accessors who binding))
		  (accessor (assq (syntax->datum ?field-name) table)))
	     (unless accessor
	       (syntax-violation who "unknown record field name" expr-stx ?field-name))
	     (chi-expr (bless `(,(cdr accessor) ,?record))
		       lexenv.run lexenv.expand)))))))

  (define ($record-type-field-set!-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand R6RS's $RECORD-TYPE-FIELD-SET!
    ;;syntax uses from  the top-level built in  environment.  Expand the
    ;;contents of  EXPR-STX in the  context of the  lexical environments
    ;;LEXENV.RUN  and  LEXENV.EXPAND.    Return  a  symbolic  expression
    ;;evaluating to the record type descriptor.
    ;;
    (define who '$record-type-field-set!)
    (syntax-match expr-stx ()
      ((_ ?type-name ?field-name ?record ?new-value)
       (and (identifier? ?type-name)
	    (identifier? ?field-name))
       (let ((label (id->label ?type-name)))
	 (unless label
	   (%raise-unbound-error who expr-stx ?type-name))
	 (let ((binding (label->binding label lexenv.run)))
	   (unless (%record-type-descriptor-binding? binding)
	     (syntax-violation who "not a record type" expr-stx ?type-name))
	   (let* ((table   (%get-alist-of-unsafe-field-mutators who binding))
		  (mutator (assq (syntax->datum ?field-name) table)))
	     (unless mutator
	       (syntax-violation who "unknown record field name or immutable field" expr-stx ?field-name))
	     (chi-expr (bless `(,(cdr mutator) ,?record ,?new-value))
		       lexenv.run lexenv.expand)))))))

  (define (%get-alist-of-safe-field-accessors who binding)
    ;;Inspect a lexical  environment binding with key  "$rtd" and return
    ;;the alist  of safe R6RS record  field accessors.  If the  alist is
    ;;not present: raise a syntax violation.
    ;;
    (let ((val (binding-value binding)))
      (if (<= 4 (length val))
	  (list-ref val 2)
	(syntax-violation who
	  "request for safe accessors of R6RS record for which they are not defined"
	  binding))))

  (define (%get-alist-of-safe-field-mutators who binding)
    ;;Inspect a lexical  environment binding with key  "$rtd" and return
    ;;the alist of safe R6RS record field mutators.  If the alist is not
    ;;present: raise a syntax violation.
    ;;
    (let ((val (binding-value binding)))
      (if (<= 4 (length val))
	  (list-ref val 3)
	(syntax-violation who
	  "request for safe mutators of R6RS record for which they are not defined"
	  binding))))

  (define (%get-alist-of-unsafe-field-accessors who binding)
    ;;Inspect a lexical  environment binding with key  "$rtd" and return
    ;;the alist of unsafe R6RS record  field accessors.  If the alist is
    ;;not present: raise a syntax violation.
    ;;
    (let ((val (binding-value binding)))
      (if (<= 6 (length val))
	  (list-ref val 4)
	(syntax-violation who
	  "request for unsafe accessors of R6RS record for which they are not defined"
	  binding))))

  (define (%get-alist-of-unsafe-field-mutators who binding)
    ;;Inspect a lexical  environment binding with key  "$rtd" and return
    ;;the alist of  unsafe R6RS record field mutators.  If  the alist is
    ;;not present: raise a syntax violation.
    ;;
    (let ((val (binding-value binding)))
      (if (<= 6 (length val))
	  (list-ref val 5)
	(syntax-violation who
	  "request for unsafe mutators of R6RS record for which they are not defined"
	  binding))))

  #| end of module |# )


;;;; module core-macro-transformer: IF

(define (if-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  R6RS's IF  syntaxes from  the
  ;;top-level built in environment.  Expand  the contents of EXPR-STX in
  ;;the   context   of   the   lexical   environments   LEXENV.RUN   and
  ;;LEXENV.EXPAND.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?consequent ?alternate)
     (build-conditional no-source
			(chi-expr ?test       lexenv.run lexenv.expand)
			(chi-expr ?consequent lexenv.run lexenv.expand)
			(chi-expr ?alternate  lexenv.run lexenv.expand)))
    ((_ ?test ?consequent)
     (build-conditional no-source
			(chi-expr ?test       lexenv.run lexenv.expand)
			(chi-expr ?consequent lexenv.run lexenv.expand)
			(build-void)))))


;;;; module core-macro-transformer: QUOTE

(define (quote-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand R6RS's QUOTE  syntaxes from the
  ;;top-level built in environment.  Expand  the contents of EXPR-STX in
  ;;the   context   of   the   lexical   environments   LEXENV.RUN   and
  ;;LEXENV.EXPAND.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?datum)
     (build-data no-source (syntax->datum ?datum)))))


;;;; module core-macro-transformer: LAMBDA and CASE-LAMBDA

(define (case-lambda-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS's CASE-LAMBDA syntaxes from
  ;;the top-level built in environment.  Expand the contents of EXPR-STX
  ;;in  the   context  of   the  lexical  environments   LEXENV.RUN  and
  ;;LEXENV.EXPAND.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (receive (formals* body*)
	 (chi-lambda-clause* expr-stx ?formals*
			     (map cons ?body* ?body**) lexenv.run lexenv.expand)
       (build-case-lambda (syntax-annotation expr-stx) formals* body*)))))

(define (lambda-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  R6RS's LAMBDA syntaxes from the
  ;;top-level built in environment.  Expand  the contents of EXPR-STX in
  ;;the   context   of   the   lexical   environments   LEXENV.RUN   and
  ;;LEXENV.EXPAND.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?body ?body* ...)
     (receive (formals body)
	 (chi-lambda-clause expr-stx ?formals
			    (cons ?body ?body*) lexenv.run lexenv.expand)
       (build-lambda (syntax-annotation expr-stx) formals body)))))


;;;; module core-macro-transformer: FOREIGN-CALL

(define (foreign-call-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  FOREIGN-CALL syntaxes
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX in the  context of the lexical  environments LEXENV.RUN and
  ;;LEXENV.EXPAND.  Return a symbolic expression in the core language.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?arg* ...)
     (build-foreign-call no-source
			 (chi-expr  ?name lexenv.run lexenv.expand)
			 (chi-expr* ?arg* lexenv.run lexenv.expand)))))


;;;; module core-macro-transformer: SYNTAX

(module (syntax-transformer)
  ;;Transformer function used to expand  R6RS's SYNTAX syntaxes from the
  ;;top-level built in environment.  Process  the contents of USE-STX in
  ;;the   context   of   the   lexical   environments   LEXENV.RUN   and
  ;;LEXENV.EXPAND.
  ;;
  ;;According to R6RS, the use of the SYNTAX macro must have the format:
  ;;
  ;;  (syntax ?template)
  ;;
  ;;where ?TEMPLATE is one among:
  ;;
  ;;  ?datum
  ;;  ?pattern-variable
  ;;  ?id
  ;;  (?subtemplate ...)
  ;;  (?subtemplate ... . ?template)
  ;;  #(?subtemplate ...)
  ;;
  ;;in  which:  ?DATUM  is  a literal  datum,  ?PATTERN-VARIABLE  is  an
  ;;identifier referencing  a pattern  variable created  by SYNTAX-CASE,
  ;;?ID   is  an   identifier  not   referencing  a   pattern  variable,
  ;;?SUBTEMPLATE  is  a  template  followed by  zero  or  more  ellipsis
  ;;identifiers.
  ;;
  ;;Return a symbolic expression representing  code in the core language
  ;;which, when evaluated, returns a  wrapped or unwrapped syntax object
  ;;containing an expression in which:
  ;;
  ;;* All the template identifiers being references to pattern variables
  ;;  are substituted with the corresponding syntax objects.
  ;;
  ;;     (syntax-case #'123 (?obj (syntax ?obj)))
  ;;     => #<syntax expr=123>
  ;;
  ;;     (syntax-case #'(1 2) ((?a ?b) (syntax #(?a ?b))))
  ;;     => #(#<syntax expr=1> #<syntax expr=1>)
  ;;
  ;;* All the identifiers not  being references to pattern variables are
  ;;  left  alone to  be captured  by the lexical  context at  the level
  ;;  below the current,  in the context of the SYNTAX  macro use or the
  ;;  context of the output form.
  ;;
  ;;     (syntax-case #'(1) ((?a) (syntax (display ?b))))
  ;;     => (#<syntax expr=display>
  ;;         #<syntax expr=1> . #<syntax expr=()>)
  ;;
  ;;* All the sub-templates followed by ellipsis are replicated to match
  ;;  the input pattern.
  ;;
  ;;     (syntax-case #'(1 2 3) ((?a ...) (syntax #(?a ...))))
  ;;     => #(1 2 3)
  ;;
  ;;About pattern variables:  they are present in  a lexical environment
  ;;as entries with format:
  ;;
  ;;   (?label . (syntax . (?name . ?level)))
  ;;
  ;;where:  ?LABEL  is the  label  in  the identifier's  syntax  object,
  ;;"syntax" is  the symbol "syntax",  ?NAME is the  symbol representing
  ;;the  name  of the  pattern  variable,  ?LEVEL  is an  exact  integer
  ;;representing the  nesting ellipsis level.  The  SYNTAX-CASE patterns
  ;;below will generate the given entries:
  ;;
  ;;   ?a			->  (syntax . (?a . 0))
  ;;   (?a)			->  (syntax . (?a . 0))
  ;;   (((?a)))			->  (syntax . (?a . 0))
  ;;   (?a ...)			->  (syntax . (?a . 1))
  ;;   ((?a) ...)		->  (syntax . (?a . 1))
  ;;   ((((?a))) ...)		->  (syntax . (?a . 1))
  ;;   ((?a ...) ...)		->  (syntax . (?a . 2))
  ;;   (((?a ...) ...) ...)	->  (syntax . (?a . 3))
  ;;
  ;;The  input template  is  first visited  in  post-order, building  an
  ;;intermediate  symbolic  representation  of  it;  then  the  symbolic
  ;;representation is visited in post-order, building core language code
  ;;that  evaluates  to  the   resulting  syntax  object.   Examples  of
  ;;intermediate  representation  (-->)  and  expansion  (==>)  follows,
  ;;assuming identifiers starting with "?"  are pattern variables:
  #|
      (syntax display)
      --> (quote #<syntax expr=display>)
      ==> (quote #<syntax expr=display>)

      (syntax (display 123))
      --> (quote #<syntax expr=(display 123)>)
      ==> (quote #<syntax expr=(display 123)>)

      (syntax ?a)
      --> (ref ?a)
      ==> ?a

      (syntax (?a))
      --> (cons (ref ?a) (quote #<syntax expr=()>))
      ==> ((primitive cons) ?a (quote #<syntax expr=()>))

      (syntax (?a 1))
      --> (cons (ref ?a) (quote #<syntax expr=(1)>))
      ==> ((primitive cons) ?a (quote #<syntax expr=(1)>))

      (syntax (1 ?a 2))
      --> (cons (quote #<syntax expr=1>)
                (cons (ref ?a) (quote #<syntax expr=(2)>)))
      ==> ((primitive cons)
	   (quote #<syntax expr=1>)
	   ((primitive cons) ?a (quote #<syntax expr=(2)>)))

      (syntax (display ?a))
      ==> (cons
	   (quote #<syntax expr=display>)
	   (cons (ref ?a) (quote #<syntax expr=()>)))
      ==> ((primitive cons)
	   (quote #<syntax expr=display>)
	   ((primitive cons) ?a (quote #<syntax expr=()>)))

      (syntax #(?a))
      --> (vector (ref ?a))
      ==> ((primitive vector) ?a)

      (syntax (?a ...))
      --> (ref ?a)
      ==> ?a

      (syntax ((?a ...) ...))
      --> (ref ?a)
      ==> ?a

      (syntax ((?a ?b ...) ...))
      -- (map (primitive cons) (ref ?a) (ref ?b))
      ==> ((primitive ellipsis-map) (primitive cons) ?a ?b)

      (syntax (((?a ?b ...) ...) ...))
      --> (map (lambda (tmp2 tmp1)
                 (map (primitive cons) tmp1 tmp2))
	    (ref ?b) (ref ?a))
      ==> ((primitive ellipsis-map)
	   (case-lambda
	    ((tmp2 tmp1)
	     ((primitive ellipsis-map) (primitive cons) tmp1 tmp2)))
           ?b ?a)

      (syntax ((?a (?a ...)) ...))
      --> (map (lambda (tmp)
                 (cons (ref tmp)
		       (cons (ref ?a)
			     (quote #<syntax expr=()>))))
            (ref ?a))
      ==> ((primitive ellipsis-map)
           (case-lambda
            ((tmp)
             ((primitive cons) tmp
	                       ((primitive cons) ?a
                                        	 (quote #<syntax expr=()>)))))
           ?a)
  |#
  (define (syntax-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?template)
       (receive (intermediate-sexp maps)
	   (%gen-syntax use-stx ?template lexenv.run '() ellipsis? #f)
	 (let ((code (%generate-output-code intermediate-sexp)))
	   #;(debug-print 'syntax (syntax->datum ?template) intermediate-sexp code)
	   code)))))

  (define (%gen-syntax use-stx template-stx lexenv maps ellipsis? vec?)
    ;;Recursive function.  Expand the contents of a SYNTAX use.
    ;;
    ;;USE-STX must be  the syntax object containing  the original SYNTAX
    ;;macro use; it is used for descriptive error reporting.
    ;;
    ;;TEMPLATE-STX must be the template from the SYNTAX macro use.
    ;;
    ;;LEXENV is  the lexical  environment in  which the  expansion takes
    ;;place;  it must  contain  the pattern  variables  visible by  this
    ;;SYNTAX use.
    ;;
    ;;MAPS is  a list  of alists,  one alist  for each  ellipsis nesting
    ;;level.  If the template has 3 nested ellipsis patterns:
    ;;
    ;;   (((?a ...) ...) ...)
    ;;
    ;;while  we are  processing the  inner "(?a  ...)"  MAPS  contains 3
    ;;alists.  The  alists are  used when processing  ellipsis templates
    ;;that recursively reference the same pattern variable, for example:
    ;;
    ;;   ((?a (?a ...)) ...)
    ;;
    ;;the inner  ?A is mapped  to a gensym which  is used to  generate a
    ;;binding in the output code.
    ;;
    ;;ELLIPSIS? must be a predicate function returning true when applied
    ;;to the  ellipsis identifier from  the built in  environment.  Such
    ;;function  is made  an argument,  so that  it can  be changed  to a
    ;;predicate  returning   always  false   when  we   are  recursively
    ;;processing a quoted template:
    ;;
    ;;   (... ?sub-template)
    ;;
    ;;in which the ellipses in ?SUB-TEMPLATE are to be handled as normal
    ;;identifiers.
    ;;
    ;;VEC? is a boolean: true when this function is processing the items
    ;;of a vector.
    ;;
    (syntax-match template-stx ()

      ;;Standalone ellipses are not allowed.
      ;;
      (?dots
       (ellipsis? ?dots)
       (stx-error use-stx "misplaced ellipsis in syntax form"))

      ;;Match  a standalone  identifier.   ?ID can  be:  a reference  to
      ;;pattern variable created by SYNTAX-CASE; an identifier that will
      ;;be captured by  some binding; an identifier that  will result to
      ;;be free,  in which  case an "unbound  identifier" error  will be
      ;;raised later.
      ;;
      (?id
       (identifier? ?id)
       (let ((binding (label->binding (id->label ?id) lexenv)))
	 (if (eq? (binding-type binding) 'syntax)
	     ;;It is a reference to pattern variable.
	     (receive (var maps)
		 (let* ((name.level  (binding-value binding))
			(name        (car name.level))
			(level       (cdr name.level)))
		   (%gen-ref use-stx name level maps))
	       (values (list 'ref var) maps))
	   ;;It is some other identifier.
	   (values (list 'quote ?id) maps))))

      ;;Ellipses starting a vector template are not allowed:
      ;;
      ;;   #(... 1 2 3)   ==> ERROR
      ;;
      ;;but ellipses  starting a list  template are allowed,  they quote
      ;;the subsequent sub-template:
      ;;
      ;;   (... ...)		==> quoted ellipsis
      ;;   (... ?sub-template)	==> quoted ?SUB-TEMPLATE
      ;;
      ;;so that the ellipses in  the ?SUB-TEMPLATE are treated as normal
      ;;identifiers.  We change the  ELLIPSIS? argument for recursion to
      ;;a predicate that always returns false.
      ;;
      ((?dots ?sub-template)
       (ellipsis? ?dots)
       (if vec?
	   (stx-error use-stx "misplaced ellipsis in syntax form")
	 (%gen-syntax use-stx ?sub-template lexenv maps (lambda (x) #f) #f)))

      ;;Match a template followed by ellipsis.
      ;;
      ((?template ?dots . ?rest)
       (ellipsis? ?dots)
       (let loop
	   ((rest.stx ?rest)
	    (kont     (lambda (maps)
			(receive (template^ maps)
			    (%gen-syntax use-stx ?template lexenv (cons '() maps) ellipsis? #f)
			  (if (null? (car maps))
			      (stx-error use-stx "extra ellipsis in syntax form")
			    (values (%gen-map template^ (car maps))
				    (cdr maps)))))))
	 (syntax-match rest.stx ()
	   (()
	    (kont maps))

	   ((?dots . ?tail)
	    (ellipsis? ?dots)
	    (loop ?tail (lambda (maps)
			  (receive (template^ maps)
			      (kont (cons '() maps))
			    (if (null? (car maps))
				(stx-error use-stx "extra ellipsis in syntax form")
			      (values (%gen-mappend template^ (car maps))
				      (cdr maps)))))))

	   (_
	    (receive (rest^ maps)
		(%gen-syntax use-stx rest.stx lexenv maps ellipsis? vec?)
	      (receive (template^ maps)
		  (kont maps)
		(values (%gen-append template^ rest^) maps))))
	   )))

      ;;Process pair templates.
      ;;
      ((?car . ?cdr)
       (receive (car.new maps)
	   (%gen-syntax use-stx ?car lexenv maps ellipsis? #f)
	 (receive (cdr.new maps)
	     (%gen-syntax use-stx ?cdr lexenv maps ellipsis? vec?)
	   (values (%gen-cons template-stx ?car ?cdr car.new cdr.new)
		   maps))))

      ;;Process a vector template.  We set to true the VEC? argument for
      ;;recursion.
      ;;
      (#(?item* ...)
       (receive (item*.new maps)
	   (%gen-syntax use-stx ?item* lexenv maps ellipsis? #t)
	 (values (%gen-vector template-stx ?item* item*.new)
		 maps)))

      ;;Everything else is just quoted in the output.  This includes all
      ;;the literal datums.
      ;;
      (_
       (values `(quote ,template-stx) maps))
      ))

  (define (%gen-ref use-stx var level maps)
    ;;Recursive function.
    ;;
    #;(debug-print 'gen-ref maps)
    (if (zero? level)
	(values var maps)
      (if (null? maps)
	  (stx-error use-stx "missing ellipsis in syntax form")
	(receive (outer-var outer-maps)
	    (%gen-ref use-stx var (- level 1) (cdr maps))
	  (cond ((assq outer-var (car maps))
		 => (lambda (b)
		      (values (cdr b) maps)))
		(else
		 (let ((inner-var (gensym-for-lexical-var 'tmp)))
		   (values inner-var
			   (cons (cons (cons outer-var inner-var)
				       (car maps))
				 outer-maps)))))))))

  (define (%gen-append x y)
    (if (equal? y '(quote ()))
	x
      `(append ,x ,y)))

  (define (%gen-mappend e map-env)
    `(apply (primitive append) ,(%gen-map e map-env)))

  (define (%gen-map e map-env)
    (let ((formals (map cdr map-env))
	  (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
      (cond
       ;; identity map equivalence:
       ;; (map (lambda (x) x) y) == y
       ((eq? (car e) 'ref)
	(car actuals))
       ;; eta map equivalence:
       ;; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
       ((for-all
	    (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
	  (cdr e))
	(let ((args (map (let ((r (map cons formals actuals)))
			   (lambda (x) (cdr (assq (cadr x) r))))
		      (cdr e))))
	  `(map (primitive ,(car e)) . ,args)))
       (else
	(cons* 'map (list 'lambda formals e) actuals)))))

  (define (%gen-cons e x y x.new y.new)
    (case (car y.new)
      ((quote)
       (cond ((eq? (car x.new) 'quote)
	      (let ((x.new (cadr x.new))
		    (y.new (cadr y.new)))
		(if (and (eq? x.new x)
			 (eq? y.new y))
		    `(quote ,e)
		  `(quote ,(cons x.new y.new)))))
	     ((null? (cadr y.new))
	      `(list ,x.new))
	     (else
	      `(cons ,x.new ,y.new))))
      ((list)
       `(list ,x.new . ,(cdr y.new)))
      (else
       `(cons ,x.new ,y.new))))

  (define (%gen-vector e ls lsnew)
    (cond ((eq? (car lsnew) 'quote)
	   (if (eq? (cadr lsnew) ls)
	       `(quote ,e)
	     `(quote #(,@(cadr lsnew)))))

	  ((eq? (car lsnew) 'list)
	   `(vector . ,(cdr lsnew)))

	  (else
	   `(list->vector ,lsnew))))

  (define (%generate-output-code x)
    ;;Recursive function.
    ;;
    (case (car x)
      ((ref)
       (build-lexical-reference no-source (cadr x)))
      ((primitive)
       (build-primref no-source (cadr x)))
      ((quote)
       (build-data no-source (cadr x)))
      ((lambda)
       (build-lambda no-source (cadr x) (%generate-output-code (caddr x))))
      ((map)
       (let ((ls (map %generate-output-code (cdr x))))
	 (build-application no-source
			    (build-primref no-source 'ellipsis-map)
			    ls)))
      (else
       (build-application no-source
			  (build-primref no-source (car x))
			  (map %generate-output-code (cdr x))))))

  #| end of module: syntax-transformer |# )


;;;; module core-macro-transformer: SYNTAX-CASE

(module (syntax-case-transformer)
  ;;Transformer function used to expand R6RS's SYNTAX-CASE syntaxes from
  ;;the top-level built in environment.  Process the contents of USE-STX
  ;;in  the   context  of   the  lexical  environments   LEXENV.RUN  and
  ;;LEXENV.EXPAND.
  ;;
  ;;Notice  that   the  parsing   of  the   patterns  is   performed  by
  ;;CONVERT-PATTERN at  expand time and  the actual pattern  matching is
  ;;performed by SYNTAX-DISPATCH at run time.
  ;;
  (define (syntax-case-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?expr (?literal* ...) ?clauses* ...)
       (%verify-literals ?literal* use-stx)
       (let* ( ;;The identifier to  which the result of evaluating the
	      ;;?EXPR is bound.
	      (expr.id    (gensym-for-lexical-var 'tmp))
	      ;;The full SYNTAX-CASE  pattern matching code, generated
	      ;;and transformed to core language.
	      (body.core  (%gen-syntax-case expr.id ?literal* ?clauses*
					    lexenv.run lexenv.expand))
	      ;;The ?EXPR transformed to core language.
	      (expr.core  (chi-expr ?expr lexenv.run lexenv.expand)))
	 ;;Return a form like:
	 ;;
	 ;;   ((lambda (expr.id) body.core) expr.core)
	 ;;
	 (build-application no-source
	   (build-lambda no-source (list expr.id) body.core)
	   (list expr.core))))
      ))

  (define (%gen-syntax-case expr.id literals clauses lexenv.run lexenv.expand)
    ;;Recursive function.  Generate and return the full pattern matching
    ;;code in the core language to match the given CLAUSES.
    ;;
    (syntax-match clauses ()
      ;;No pattern matched the input  expression: return code to raise a
      ;;syntax error.
      ;;
      (()
       (build-application no-source
	 (build-primref no-source 'syntax-error)
	 (list (build-lexical-reference no-source expr.id))))

      ;;The pattern  is a standalone  identifier, neither a  literal nor
      ;;the ellipsis,  and it  has no  fender.  A  standalone identifier
      ;;with no fender matches everything,  so it is useless to generate
      ;;the code  for the next clauses:  the code generated here  is the
      ;;last one.
      ;;
      (((?pattern ?output-expr) . ?unused-clauses)
       (and (identifier? ?pattern)
	    (not (bound-id-member? ?pattern literals))
	    (not (ellipsis? ?pattern)))
       (if (free-id=? ?pattern (scheme-stx '_))
	   ;;The clause is:
	   ;;
	   ;;   (_ ?output-expr)
	   ;;
	   ;;the underscore  identifier matches everything and  binds no
	   ;;pattern variables.
	   (chi-expr ?output-expr lexenv.run lexenv.expand)
	 ;;The clause is:
	 ;;
	 ;;   (?id ?output-expr)
	 ;;
	 ;;a standalone identifier matches everything  and binds it to a
	 ;;pattern variable whose name is ?ID.
	 (let ((label (gensym-for-label ?pattern))
	       (lex   (gensym-for-lexical-var ?pattern)))
	   ;;The expression  must be  expanded in a  lexical environment
	   ;;augmented with the pattern variable.
	   (define output-expr^
	     (push-lexical-contour (make-full-rib (list ?pattern) (list label))
				   ?output-expr))
	   (define lexenv.run^
	     ;;Push a pattern variable entry to the lexical environment.
	     ;;The ellipsis nesting level is 0.
	     (cons (cons label (make-binding 'syntax (cons lex 0)))
		   lexenv.run))
	   (define output-expr.core
	     (chi-expr output-expr^ lexenv.run^ lexenv.expand))
	   (build-application no-source
	     (build-lambda no-source
	       (list lex)
	       output-expr.core)
	     (list (build-lexical-reference no-source expr.id))))))

      ;;The  pattern is  neither  a standalone  pattern  variable nor  a
      ;;standalone underscore.  It has no fender, which is equivalent to
      ;;having a "#t" as fender.
      ;;
      (((?pattern ?output-expr) . ?next-clauses)
       (%gen-clause expr.id literals
		    ?pattern #t #;fender ?output-expr
		    lexenv.run lexenv.expand
		    ?next-clauses))

      ;;The pattern has a fender.
      ;;
      (((?pattern ?fender ?output-expr) . ?next-clauses)
       (%gen-clause expr.id literals
		    ?pattern ?fender ?output-expr
		    lexenv.run lexenv.expand
		    ?next-clauses))
      ))

  (define (%gen-clause expr.id literals
		       pattern.stx fender.stx output-expr.stx
		       lexenv.run lexenv.expand
		       next-clauses)
    ;;Generate  the  code needed  to  match  the clause  represented  by
    ;;PATTERN.STX, FENDER.STX and  OUTPUT-EXPR.STX; recursively generate
    ;;the code to match the other clauses in NEXT-CLAUSES.
    ;;
    ;;When there is a fender, we build the output form (pseudo-code):
    ;;
    ;;  ((lambda (y)
    ;;      (if (if y
    ;;              (fender-matches?)
    ;;            #f)
    ;;          (output-expr)
    ;;        (match-next-clauses))
    ;;   (syntax-dispatch expr.id pattern))
    ;;
    ;;when there is no fender, build the output form (pseudo-code):
    ;;
    ;;  ((lambda (tmp)
    ;;      (if tmp
    ;;          (output-expr)
    ;;        (match-next-clauses))
    ;;   (syntax-dispatch expr.id pattern))
    ;;
    ;;notice that the  return value of SYNTAX-DISPATCH is:  false if the
    ;;pattern did not match, otherwise the list of values to be bound to
    ;;the pattern variables.
    ;;
    (receive (pattern.dispatch pvars.levels)
	;;CONVERT-PATTERN  return 2  values: the  pattern in  the format
	;;accepted by SYNTAX-DISPATCH, an alist representing the pattern
	;;variables:
	;;
	;;* The keys of the alist are identifiers representing the names
	;;  of the pattern variables.
	;;
	;;*  The values  of the  alist are  non-negative exact  integers
	;;  representing the ellipsis nesting level of the corresponding
	;;  pattern variable.  See SYNTAX-TRANSFORMER for details.
	;;
	(convert-pattern pattern.stx literals)
      (let ((pvars (map car pvars.levels)))
	(unless (distinct-bound-ids? pvars)
	  (%invalid-ids-error pvars pattern.stx "pattern variable")))
      (unless (for-all (lambda (x)
			 (not (ellipsis? (car x))))
		pvars.levels)
	(stx-error pattern.stx "misplaced ellipsis in syntax-case pattern"))
      (let* ((tmp-sym      (gensym-for-lexical-var 'tmp))
	     (fender-cond  (%build-fender-conditional expr.id literals tmp-sym pvars.levels
						      fender.stx output-expr.stx
						      lexenv.run lexenv.expand
						      next-clauses)))
	(build-application no-source
	  (build-lambda no-source
	    (list tmp-sym)
	    fender-cond)
	  (list
	   (build-application no-source
	     (build-primref no-source 'syntax-dispatch)
	     (list (build-lexical-reference no-source expr.id)
		   (build-data no-source pattern.dispatch))))))))

  (define (%build-fender-conditional expr.id literals tmp-sym pvars.levels
				     fender.stx output-expr.stx
				     lexenv.run lexenv.expand
				     next-clauses)
    ;;Generate the  code that tests  the fender: if the  fender succeeds
    ;;run the output expression, else try to match the next clauses.
    ;;
    ;;When there is a fender, we build the output form (pseudo-code):
    ;;
    ;;   (if (if y
    ;;           (fender-matches?)
    ;;         #f)
    ;;       (output-expr)
    ;;     (match-next-clauses))
    ;;
    ;;when there is no fender, build the output form (pseudo-code):
    ;;
    ;;   (if tmp
    ;;       (output-expr)
    ;;     (match-next-clauses))
    ;;
    (define-inline (%build-call expr.stx)
      (%build-dispatch-call pvars.levels expr.stx tmp-sym lexenv.run lexenv.expand))
    (let ((test     (if (eq? fender.stx #t)
			;;There is no fender.
			tmp-sym
		      ;;There is a fender.
		      (build-conditional no-source
			(build-lexical-reference no-source tmp-sym)
			(%build-call fender.stx)
			(build-data no-source #f))))
	  (conseq    (%build-call output-expr.stx))
	  (altern    (%gen-syntax-case expr.id literals next-clauses lexenv.run lexenv.expand)))
      (build-conditional no-source
	test conseq altern)))

  (define (%build-dispatch-call pvars.levels expr.stx tmp-sym lexenv.run lexenv.expand)
    ;;Generate  code to  evaluate EXPR.STX  in an  environment augmented
    ;;with the pattern variables defined by PVARS.LEVELS.  Return a core
    ;;language expression representing the following pseudo-code:
    ;;
    ;;   (apply (lambda (pattern-var ...) expr) tmp)
    ;;
    (define ids
      ;;For each pattern variable: the identifier representing its name.
      (map car pvars.levels))
    (define labels
      ;;For each pattern variable: a gensym used as label in the lexical
      ;;environment.
      (map gensym-for-label ids))
    (define names
      ;;For each pattern variable: a gensym used as unique variable name
      ;;in the lexical environment.
      (map gensym-for-lexical-var ids))
    (define levels
      ;;For  each pattern  variable: an  exact integer  representing the
      ;;ellipsis nesting level.  See SYNTAX-TRANSFORMER for details.
      (map cdr pvars.levels))
    (define bindings
      ;;For each pattern variable: a binding to be pushed on the lexical
      ;;environment.
      (map (lambda (label name level)
	     (cons label (make-binding 'syntax (cons name level))))
	labels names levels))
    (define expr.core
      ;;Expand the  expression in  a lexical environment  augmented with
      ;;the pattern variables.
      ;;
      ;;NOTE We could have created a syntax object:
      ;;
      ;;  #`(lambda (pvar ...) #,expr.stx)
      ;;
      ;;and then  expanded it:  EXPR.STX would have  been expanded  in a
      ;;lexical environment augmented with the PVAR bindings.
      ;;
      ;;Instead we have chosen to push  the PVAR bindings on the lexical
      ;;environment "by hand", then to  expand EXPR.STX in the augmented
      ;;environment,  finally   to  put  the  resulting   core  language
      ;;expression in a core language LAMBDA syntax.
      ;;
      ;;The two methods are fully equivalent;  the one we have chosen is
      ;;a bit faster.
      ;;
      (chi-expr (push-lexical-contour (make-full-rib ids labels) expr.stx)
		(append bindings lexenv.run)
		lexenv.expand))
    (build-application no-source
      (build-primref no-source 'apply)
      (list (build-lambda no-source names expr.core)
	    (build-lexical-reference no-source tmp-sym))))

  (define (%invalid-ids-error id* e class)
    (let find ((id* id*)
	       (ok* '()))
      (if (null? id*)
	  (stx-error e) ; shouldn't happen
	(if (identifier? (car id*))
	    (if (bound-id-member? (car id*) ok*)
		(syntax-error (car id*) "duplicate " class)
	      (find (cdr id*) (cons (car id*) ok*)))
	  (syntax-error (car id*) "invalid " class)))))

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;; end of module: CORE-MACRO-TRANSFORMER

)


;;;; local macro transformer

(define (local-macro-transformer x)
  (car x))


;;;; macro transformers helpers

(define (%expand-macro-transformer expr-stx lexenv.expand)
  ;;Called to expand the right-hand  side of syntax definitions.  Return
  ;;the fully expanded right-hand side.
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
  (let* ((rtc           (make-collector))
	 (expanded-rhs  (parametrise ((inv-collector rtc)
				      (vis-collector (lambda (x) (values))))
			  (chi-expr expr-stx lexenv.expand lexenv.expand))))
    (for-each
	(let ((mark-visit (vis-collector)))
	  (lambda (x)
	    (invoke-library x)
	    (mark-visit x)))
      (rtc))
    expanded-rhs))


;;;; formals syntax validation

(define (%verify-formals-syntax formals-stx stx)
  ;;Verify  that  FORMALS-STX  is  a syntax  object  representing  valid
  ;;formals for  LAMBDA and WITH-SYNTAX syntaxes.   If successful return
  ;;unspecified values, else raise a syntax violation.
  ;;
  (syntax-match formals-stx ()
    ((id* ...)
     (unless (valid-bound-ids? id*)
       (%error-invalid-formals-syntax stx formals-stx)))

    ((id* ... . last-id)
     (unless (valid-bound-ids? (cons last-id id*))
       (%error-invalid-formals-syntax stx formals-stx)))

    (_
     (stx-error stx "invalid syntax"))))

(define (%error-invalid-formals-syntax stx formals-stx)
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
    (syntax-violation #f message stx subform))
  (syntax-match formals-stx ()
    ((?id* ... . ?last)
     (let recur ((?id* (cond ((identifier? ?last)
			      (cons ?last ?id*))
			     ((syntax-null? ?last)
			      ?id*)
			     (else
			      (%synner "not an identifier" ?last)))))
       (cond ((null? ?id*)
	      (values))
	     ((not (identifier? (car ?id*)))
	      (%synner "not an identifier" (car ?id*)))
	     (else
	      (recur (cdr ?id*))
	      (when (bound-id-member? (car ?id*)
				      (cdr ?id*))
		(%synner "duplicate binding" (car ?id*)))))))

    (_
     (%synner "malformed binding form" formals-stx))
    ))


;;;; pattern matching helpers

(define (convert-pattern pattern-stx literals)
  ;;This function is used both by  the transformer of the non-core macro
  ;;WITH-SYNTAX and  by the transformer  of the core  macro SYNTAX-CASE.
  ;;Transform the syntax object  PATTERN-STX, representing a SYNTAX-CASE
  ;;pattern, into a pattern in the format recognised by SYNTAX-DISPATCH.
  ;;
  ;;LITERALS is null or a  list of identifiers representing the literals
  ;;from a SYNTAX-CASE use.  Notice that the ellipsis and the underscore
  ;;identifiers cannot be literals.
  ;;
  ;;Return  2   values:  the  pattern  for   SYNTAX-DISPATCH,  an  alist
  ;;representing the pattern variables:
  ;;
  ;;* The  keys of the alist  are identifiers representing the  names of
  ;;  the pattern variables.
  ;;
  ;;*  The  values   of  the  alist  are   non-negative  exact  integers
  ;;   representing  the ellipsis  nesting  level  of the  corresponding
  ;;  pattern variable.  See SYNTAX-TRANSFORMER for details.
  ;;
  ;;The returned  pattern for  SYNTAX-DISPATCH is a  symbolic expression
  ;;with the following format:
  ;;
  ;; P in pattern:                    |  matches:
  ;;----------------------------------+---------------------------
  ;;  ()                              |  empty list
  ;;  _                               |  anything (no binding created)
  ;;  any                             |  anything
  ;;  (p1 . p2)                       |  pair
  ;;  #(free-id <key>)                |  <key> with free-identifier=?
  ;;  each-any                        |  any proper list
  ;;  #(each p)                       |  (p*)
  ;;  #(each+ p1 (p2_1 ... p2_n) p3)  |   (p1* (p2_n ... p2_1) . p3)
  ;;  #(vector p)                     |  #(x ...) if p matches (x ...)
  ;;  #(atom <object>)                |  <object> with "equal?"
  ;;
  (define (%convert* pattern* ellipsis-nesting-level pvars.levels)
    (if (null? pattern*)
	(values '() pvars.levels)
      (receive (y pvars.levels)
	  (%convert* (cdr pattern*) ellipsis-nesting-level pvars.levels)
	(receive (x pvars.levels)
	    (%convert (car pattern*) ellipsis-nesting-level pvars.levels)
	  (values (cons x y) pvars.levels)))))

  (define (%convert p ellipsis-nesting-level pvars.levels)
    (syntax-match p ()
      (?id
       (identifier? ?id)
       (cond ((bound-id-member? ?id literals)
	      (values `#(free-id ,?id) pvars.levels))
	     ((free-id=? ?id (scheme-stx '_))
	      (values '_ pvars.levels))
	     (else
	      ;;It is a pattern variable.
	      (values 'any (cons (cons ?id ellipsis-nesting-level)
				 pvars.levels)))))

      ((p dots)
       (ellipsis? dots)
       (receive (p pvars.levels)
	   (%convert p (+ ellipsis-nesting-level 1) pvars.levels)
	 (values (if (eq? p 'any)
		     'each-any
		   `#(each ,p))
		 pvars.levels)))

      ((x dots ys ... . z)
       (ellipsis? dots)
       (receive (z pvars.levels)
	   (%convert z ellipsis-nesting-level pvars.levels)
	 (receive (ys pvars.levels)
	     (%convert* ys ellipsis-nesting-level pvars.levels)
	   (receive (x pvars.levels)
	       (%convert x (+ ellipsis-nesting-level 1) pvars.levels)
	     (values `#(each+ ,x ,(reverse ys) ,z)
		     pvars.levels)))))

      ((x . y)
       (receive (y pvars.levels)
	   (%convert y ellipsis-nesting-level pvars.levels)
	 (receive (x pvars.levels)
	     (%convert x ellipsis-nesting-level pvars.levels)
	   (values (cons x y) pvars.levels))))

      (()
       (values '() pvars.levels))

      (#(?item* ...)
       (not (<stx>? ?item*))
       (receive (item* pvars.levels)
	   (%convert ?item* ellipsis-nesting-level pvars.levels)
	 (values `#(vector ,item*) pvars.levels)))

      (?datum
       (values `#(atom ,(syntax->datum ?datum))
	       pvars.levels))))

  (%convert pattern-stx 0 '()))

(module (ellipsis? underscore?)

  (define (ellipsis? x)
    (%free-identifier-and-symbol? x '...))

  (define (underscore? x)
    (%free-identifier-and-symbol? x '_))

  (define (%free-identifier-and-symbol? x sym)
    (and (identifier? x)
	 (free-id=? x (scheme-stx sym))))

  #| end of module |# )

(define (%verify-literals literals use-stx)
  ;;Verify that  identifiers selected as literals  are: identifiers, not
  ;;ellipsisi, not usderscore.  If successful: return true, else raise a
  ;;syntax violation
  ;;
  ;;LITERALS is  a list  of literals  from SYNTAX-CASE  or SYNTAX-RULES.
  ;;USE-STX  is a  syntax  object  representing the  full  macro use  of
  ;;SYNTAX-CASE or SYNTAX-RULES:  it is used here  for descriptive error
  ;;reporting.
  ;;
  (for-each (lambda (x)
	      (when (or (not (identifier? x))
			(ellipsis? x)
			(underscore? x))
		(syntax-violation #f "invalid literal" use-stx x)))
    literals)
  #t)

(define (ellipsis-map proc ls . ls*)
  ;;This function  is used at  expand time  to generate the  portions of
  ;;macro  output  form  generated  by  templates  with  ellipsis.   See
  ;;SYNTAX-TRANSFORMER for details.
  ;;
  ;;For a syntax template:
  ;;
  ;;   (syntax ((?a ?b ...) ...))
  ;;
  ;;this function is called in the core language as:
  ;;
  ;;   ((primitive ellipsis-map) (primitive cons) ?a ?b)
  ;;
  (define who '...)
  (unless (list? ls)
    (assertion-violation who "not a list" ls))
  ;;LS* must be a list of  sublists, each sublist having the same length
  ;;of LS.
  (unless (null? ls*)
    (let ((n (length ls)))
      (for-each
          (lambda (x)
            (unless (list? x)
              (assertion-violation who "not a list" x))
            (unless (= (length x) n)
              (assertion-violation who "length mismatch" ls x)))
	ls*)))
  (apply map proc ls ls*))

(module (syntax-transpose)
  ;;Mh... what  does this do?   Take BASE-ID  and NEW-ID, which  must be
  ;;FREE-IDENTIFIER=?, compute the difference  between their marks, push
  ;;such difference  on top of the  marks of OBJECT, return  the result.
  ;;What for?  (Marco Maggi; Sun May 5, 2013)
  ;;
  (define who 'syntax-transpose)

  (define (syntax-transpose object base-id new-id)
    (unless (identifier? base-id)
      (%synner "not an identifier" base-id))
    (unless (identifier? new-id)
      (%synner "not an identifier" new-id))
    (unless (free-identifier=? base-id new-id)
      (%synner "not the same identifier" base-id new-id))
    (receive (mark* subst* annotated-expr*)
	(diff (car ($<stx>-mark* base-id))
	      ($<stx>-mark*   new-id)
	      ($<stx>-subst*  new-id)
	      ($<stx>-ae*     new-id)
	      (lambda ()
		(%synner "unmatched identifiers" base-id new-id)))
      (if (and (null? mark*)
	       (null? subst*))
	  object
	(mkstx object mark* subst* annotated-expr*))))

  (define (diff base.mark new.mark* new.subst* new.annotated-expr* error)
    (if (null? new.mark*)
	(error)
      (let ((new.mark1 (car new.mark*)))
	(if (eq? base.mark new.mark1)
	    (values '() (final new.subst*) '())
	  (receive (subst1* subst2*)
	      (split new.subst*)
	    (receive (nm* ns* nae*)
		(diff base.mark (cdr new.mark*) subst2* (cdr new.annotated-expr*) error)
	      (values (cons new.mark1 nm*)
		      (append subst1* ns*)
		      (cons (car new.annotated-expr*) nae*))))))))

  (define (split subst*)
    ;;Non-tail recursive  function.  Split  SUBST* and return  2 values:
    ;;the prefix  of SUBST*  up to  and including  the first  shift, the
    ;;suffix of SUBST* from the first shift excluded to the end.
    ;;
    (if (eq? (car subst*) 'shift)
	(values (list 'shift)
		(cdr subst*))
      (receive (subst1* subst2*)
	  (split (cdr subst*))
	(values (cons (car subst*) subst1*)
		subst2*))))

  (define (final subst*)
    ;;Non-tail recursive  function.  Return the  prefix of SUBST*  up to
    ;;and not including  the first shift.  The returned prefix  is a new
    ;;list spine sharing the cars with SUBST*.
    ;;
    (if (or (null? subst*)
	    (eq? (car subst*) 'shift))
	'()
      (cons (car subst*)
	    (final (cdr subst*)))))

  (define-syntax-rule (%synner ?message ?irritant ...)
    (assertion-violation who ?message ?irritant ...))

  #| end of module: SYNTAX-TRANSPOSE |# )


;;;; pattern matching

(module (syntax-dispatch)
  ;;Perform  the actual  matching between  an input  symbolic expression
  ;;being a  (wrapped, unwrapped  or partially unwrapped)  syntax object
  ;;and a  pattern symbolic expression.   If the expression  matches the
  ;;pattern return null or  a list of syntax objects to  be bound to the
  ;;pattern variables; else return false.
  ;;
  ;;The order of  syntax objects in the returned list  is established by
  ;;the pattern and it is the  same order in which the pattern variables
  ;;appear in the alist returned by CONVERT-PATTERN.
  ;;
  ;;The pattern  for SYNTAX-DISPATCH is  a symbolic expression  with the
  ;;following format:
  ;;
  ;; P in pattern:                    |  matches:
  ;;----------------------------------+---------------------------
  ;;  ()                              |  empty list
  ;;  _                               |  anything (no binding created)
  ;;  any                             |  anything
  ;;  (p1 . p2)                       |  pair
  ;;  #(free-id <key>)                |  <key> with free-identifier=?
  ;;  each-any                        |  any proper list
  ;;  #(each p)                       |  (p*)
  ;;  #(each+ p1 (p2_1 ... p2_n) p3)  |   (p1* (p2_n ... p2_1) . p3)
  ;;  #(vector p)                     |  #(x ...) if p matches (x ...)
  ;;  #(atom <object>)                |  <object> with "equal?"
  ;;
  (define (syntax-dispatch expr pattern)
    (%match expr pattern '() #;mark* '() #;subst* '() #;annotated-expr*
	    '() #;pvar* ))

  (define (%match expr pattern mark* subst* annotated-expr* pvar*)
    (cond ((not pvar*)
	   ;;No match.
	   #f)
	  ((eq? pattern '_)
	   ;;Match anything, bind nothing.
	   pvar*)
	  ((eq? pattern 'any)
	   ;;Match anything, bind a pattern variable.
	   (cons (%make-syntax-object expr mark* subst* annotated-expr*)
		 pvar*))
	  ((<stx>? expr)
	   ;;Visit the syntax object.
	   (and (not (top-marked? mark*))
		(receive (mark*^ subst*^ annotated-expr*^)
		    (join-wraps mark* subst* annotated-expr* expr)
		  (%match (<stx>-expr expr) pattern mark*^ subst*^ annotated-expr*^ pvar*))))
	  ((annotation? expr)
	   ;;Visit the ANNOTATION struct.
	   (%match (annotation-expression expr) pattern mark* subst* annotated-expr* pvar*))
	  (else
	   (%match* expr pattern mark* subst* annotated-expr* pvar*))))

  (define (%match* expr pattern mark* subst* annotated-expr* pvar*)
    (cond
     ;;End of list pattern: match the end of a list expression.
     ;;
     ((null? pattern)
      (and (null? expr)
	   pvar*))

     ;;Match a pair expression.
     ;;
     ((pair? pattern)
      (and (pair? expr)
	   (%match (car expr) (car pattern) mark* subst* annotated-expr*
		   (%match (cdr expr) (cdr pattern) mark* subst* annotated-expr* pvar*))))

     ;;Match any  proper list  expression and  bind a  pattern variable.
     ;;This happens when the original pattern symbolic expression is:
     ;;
     ;;   (?var ...)
     ;;
     ;;everything  in the  proper  list  must be  bound  to the  pattern
     ;;variable ?VAR.
     ;;
     ((eq? pattern 'each-any)
      (let ((l (%match-each-any expr mark* subst* annotated-expr*)))
	(and l (cons l pvar*))))

     (else
      ;;Here we expect the PATTERN to be a vector of the format:
      ;;
      ;;   #(?symbol ?stuff ...)
      ;;
      ;;where ?SYMBOL is a symbol.
      ;;
      (case (vector-ref pattern 0)

	;;The pattern is:
	;;
	;;   #(each ?sub-pattern)
	;;
	;;the expression  matches if it  is a  list in which  every item
	;;matches ?SUB-PATTERN.
	;;
	((each)
	 (if (null? expr)
	     (%match-empty (vector-ref pattern 1) pvar*)
	   (let ((pvar** (%match-each expr (vector-ref pattern 1)
				      mark* subst* annotated-expr*)))
	     (and pvar**
		  (%combine pvar** pvar*)))))

	;;The pattern is:
	;;
	;;   #(free-id ?literal)
	;;
	;;the  expression  matches  if  it is  an  identifier  equal  to
	;;?LITERAL according to FREE-IDENTIFIER=?.
	;;
	((free-id)
	 (and (symbol? expr)
	      (top-marked? mark*)
	      (free-id=? (%make-syntax-object expr mark* subst* annotated-expr*)
			 (vector-ref pattern 1))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(scheme-id ?symbol)
	;;
	;;the  expression matches  if it  is an  identifier equal  to an
	;;identifier    having   ?SYMBOL    as    name   according    to
	;;FREE-IDENTIFIER=?.
	;;
	((scheme-id)
	 (and (symbol? expr)
	      (top-marked? mark*)
	      (free-id=? (%make-syntax-object expr mark* subst* annotated-expr*)
			 (scheme-stx (vector-ref pattern 1)))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(each+ p1 (p2_1 ... p2_n) p3)
	;;
	;;which originally was:
	;;
	;;   (p1 ?ellipsis p2_1 ... p2_n . p3)
	;;
	;;the expression matches if ...
	;;
	((each+)
	 (receive (xr* y-pat pvar*)
	     (%match-each+ expr
			   (vector-ref pattern 1)
			   (vector-ref pattern 2)
			   (vector-ref pattern 3)
			   mark* subst* annotated-expr* pvar*)
	   (and pvar*
		(null? y-pat)
		(if (null? xr*)
		    (%match-empty (vector-ref pattern 1) pvar*)
		  (%combine xr* pvar*)))))

	;;The pattern is:
	;;
	;;  #(atom ?object)
	;;
	;;the  expression matches  if it  is  a single  object equal  to
	;;?OBJECT according to EQUAL?.
	;;
	((atom)
	 (and (equal? (vector-ref pattern 1)
		      (strip expr mark*))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(vector ?sub-pattern)
	;;
	;;the expression matches if it is a vector whose items match the
	;;?SUB-PATTERN.
	;;
	((vector)
	 (and (vector? expr)
	      (%match (vector->list expr) (vector-ref pattern 1)
		      mark* subst* annotated-expr* pvar*)))

	(else
	 (assertion-violation 'syntax-dispatch "invalid pattern" pattern))))))

  (define (%match-each expr pattern mark* subst* annotated-expr*)
    ;;Recursive function.   The expression  matches if it  is a  list in
    ;;which  every item  matches  PATTERN.   Return null  or  a list  of
    ;;sublists, each sublist being a list of pattern variable values.
    ;;
    (cond ((pair? expr)
	   (let ((first (%match (car expr) pattern mark* subst* annotated-expr* '())))
	     (and first
		  (let ((rest (%match-each (cdr expr) pattern mark* subst* annotated-expr*)))
		    (and rest (cons first rest))))))
	  ((null? expr)
	   '())
	  ((<stx>? expr)
	   (and (not (top-marked? mark*))
		(receive (mark*^ subst*^ annotated-expr*^)
		    (join-wraps mark* subst* annotated-expr* expr)
		  (%match-each (<stx>-expr expr) pattern mark*^ subst*^ annotated-expr*^))))
	  ((annotation? expr)
	   (%match-each (annotation-expression expr) pattern mark* subst* annotated-expr*))
	  (else #f)))

  (define (%match-each+ e x-pat y-pat z-pat mark* subst* annotated-expr* pvar*)
    (let loop ((e e) (mark* mark*) (subst* subst*) (annotated-expr* annotated-expr*))
      (cond ((pair? e)
	     (receive (xr* y-pat pvar*)
		 (loop (cdr e) mark* subst* annotated-expr*)
	       (if pvar*
		   (if (null? y-pat)
		       (let ((xr (%match (car e) x-pat mark* subst* annotated-expr* '())))
			 (if xr
			     (values (cons xr xr*) y-pat pvar*)
			   (values #f #f #f)))
		     (values '()
			     (cdr y-pat)
			     (%match (car e) (car y-pat) mark* subst* annotated-expr* pvar*)))
		 (values #f #f #f))))
	    ((<stx>? e)
	     (if (top-marked? mark*)
		 (values '() y-pat (%match e z-pat mark* subst* annotated-expr* pvar*))
	       (receive (mark* subst* annotated-expr*)
		   (join-wraps mark* subst* annotated-expr* e)
		 (loop (<stx>-expr e) mark* subst* annotated-expr*))))
	    ((annotation? e)
	     (loop (annotation-expression e) mark* subst* annotated-expr*))
	    (else
	     (values '() y-pat (%match e z-pat mark* subst* annotated-expr* pvar*))))))

  (define (%match-each-any e mark* subst* annotated-expr*)
    (cond ((pair? e)
	   (let ((l (%match-each-any (cdr e) mark* subst* annotated-expr*)))
	     (and l (cons (%make-syntax-object (car e) mark* subst* annotated-expr*) l))))
	  ((null? e)
	   '())
	  ((<stx>? e)
	   (and (not (top-marked? mark*))
		(receive (mark* subst* annotated-expr*)
		    (join-wraps mark* subst* annotated-expr* e)
		  (%match-each-any (<stx>-expr e) mark* subst* annotated-expr*))))
	  ((annotation? e)
	   (%match-each-any (annotation-expression e) mark* subst* annotated-expr*))
	  (else #f)))

  (define (%match-empty p pvar*)
    (cond ((null? p)
	   pvar*)
	  ((eq? p '_)
	   pvar*)
	  ((eq? p 'any)
	   (cons '() pvar*))
	  ((pair? p)
	   (%match-empty (car p) (%match-empty (cdr p) pvar*)))
	  ((eq? p 'each-any)
	   (cons '() pvar*))
	  (else
	   (case (vector-ref p 0)
	     ((each)
	      (%match-empty (vector-ref p 1) pvar*))
	     ((each+)
	      (%match-empty (vector-ref p 1)
			    (%match-empty (reverse (vector-ref p 2))
					  (%match-empty (vector-ref p 3) pvar*))))
	     ((free-id atom)
	      pvar*)
	     ((scheme-id atom)
	      pvar*)
	     ((vector)
	      (%match-empty (vector-ref p 1) pvar*))
	     (else
	      (assertion-violation 'syntax-dispatch "invalid pattern" p))))))

  (define (%make-syntax-object stx mark* subst* annotated-expr*)
    (if (and (null? mark*)
	     (null? subst*)
	     (null? annotated-expr*))
	stx
      (mkstx stx mark* subst* annotated-expr*)))

  (define (%combine pvar** pvar*)
    (if (null? (car pvar**))
	pvar*
      (cons (map car pvar**)
	    (%combine (map cdr pvar**) pvar*))))

  #| end of module: SYNTAX-DISPATCH |# )


;;;; chi procedures: macro calls

(module (chi-macro chi-local-macro chi-global-macro)

  (define (chi-macro p e r rib)
    (%do-macro-call (non-core-macro-transformer p) e r rib))

  (define (chi-local-macro p e r rib)
    (%do-macro-call (local-macro-transformer p) e r rib))

  (define (chi-global-macro p e r rib)
    (let ((lib (car p))
	  (loc (cdr p)))
      (unless (eq? lib '*interaction*)
	(visit-library lib))
      (let ((x (symbol-value loc)))
	(let ((transformer (cond ((procedure? x)
				  x)
				 ((and (pair? x)
				       (eq? (car x) 'macro!)
				       (procedure? (cdr x)))
				  (cdr x))
				 (else
				  (assertion-violation 'chi-global-macro
				    "Vicare: internal error: not a procedure" x)))))
	  (%do-macro-call transformer e r rib)))))

  (define (%do-macro-call transformer expr r rib)
    (define (return x)
      (let f ((x x))
	;;Don't feed me cycles.
	(unless (<stx>? x)
	  (cond
	   ((pair? x) (f (car x)) (f (cdr x)))
	   ((vector? x) (vector-for-each f x))
	   ((symbol? x)
	    (syntax-violation #f
	      "raw symbol encountered in output of macro"
	      expr x)))))
      (add-mark (gen-mark) rib x expr))
    (let ((x (transformer (add-mark anti-mark #f expr #f))))
      (if (procedure? x)
	  (return
	   (x (lambda (id)
		(unless (identifier? id)
		  (assertion-violation 'rho "not an identifier" id))
		(let ((label (id->label id)))
		  (let ((binding (label->binding label r)))
		    (case (car binding)
		      ((local-ctv) (cadr binding))
		      ((global-ctv)
		       (let ((lib (cadr binding))
			     (loc (cddr binding)))
			 (unless (eq? lib '*interaction*)
			   (visit-library lib))
			 (symbol-value loc)))
		      (else #f)))))))
	(return x))))

  #| end of module |# )


;;;; chi procedures: expressions

(define chi-expr*
  (lambda (e* r mr)
      ;;; expand left to right
    (cond
     ((null? e*) '())
     (else
      (let ((e (chi-expr (car e*) r mr)))
	(cons e (chi-expr* (cdr e*) r mr)))))))

(define chi-application
  (lambda (e r mr)
    (syntax-match e  ()
      ((rator rands ...)
       (let ((rator (chi-expr rator r mr)))
	 (build-application (syntax-annotation e)
			    rator
			    (chi-expr* rands r mr)))))))

(define chi-expr
  (lambda (e r mr)
    (let-values (((type value kwd) (syntax-type e r)))
      (case type
	((core-macro)
	 (let ((transformer (core-macro-transformer value)))
	   (transformer e r mr)))
	((global)
	 (let* ((lib (car value))
		(loc (cdr value)))
	   ((inv-collector) lib)
	   (build-global-reference no-source loc)))
	((core-prim)
	 (let ((name value))
	   (build-primref no-source name)))
	((call) (chi-application e r mr))
	((lexical)
	 (let ((lex (lexical-var value)))
	   (build-lexical-reference no-source lex)))
	((global-macro global-macro!)
	 (chi-expr (chi-global-macro value e r #f) r mr))
	((local-macro local-macro!)
	 (chi-expr (chi-local-macro value e r #f) r mr))
	((macro macro!)
	 (chi-expr (chi-macro value e r #f) r mr))
	((constant)
	 (let ((datum value))
	   (build-data no-source datum)))
	((set!) (chi-set! e r mr))
	((begin)
	 (syntax-match e ()
	   ((_ x x* ...)
	    (build-sequence no-source
			    (chi-expr* (cons x x*) r mr)))))
	((stale-when)
	 ;;STALE-WHEN  acts  like  BEGIN,  but  in  addition  causes  an
	 ;;expression  to  be  registered   in  the  current  stale-when
	 ;;collector.   When such  expression  evaluates  to false:  the
	 ;;compiled library is  stale with respect to  some source file.
	 ;;See for example the INCLUDE syntax.
	 (syntax-match e ()
	   ((_ guard x x* ...)
	    (begin
	      (handle-stale-when guard mr)
	      (build-sequence no-source
			      (chi-expr* (cons x x*) r mr))))))
	((let-syntax letrec-syntax)
	 (syntax-match e ()
	   ((_ ((xlhs* xrhs*) ...) xbody xbody* ...)
	    (unless (valid-bound-ids? xlhs*)
	      (stx-error e "invalid identifiers"))
	    (let* ((xlab* (map gensym-for-label xlhs*))
		   (xrib (make-full-rib xlhs* xlab*))
		   (xb* (map (lambda (x)
			       (make-eval-transformer
				(%expand-macro-transformer
				 (if (eq? type 'let-syntax)
				     x
				   (push-lexical-contour xrib x))
				 mr)))
			  xrhs*)))
	      (build-sequence no-source
			      (chi-expr*
			       (map (lambda (x) (push-lexical-contour xrib x)) (cons xbody xbody*))
			       (append (map cons xlab* xb*) r)
			       (append (map cons xlab* xb*) mr)))))))
	((displaced-lexical)
	 (stx-error e "identifier out of context"))
	((syntax) (stx-error e "reference to pattern variable outside a syntax form"))
	((define define-syntax define-fluid-syntax module import library)
	 (stx-error e
		    (string-append
		     (case type
		       ((define)              "a definition")
		       ((define-syntax)       "a define-syntax")
		       ((define-fluid-syntax) "a define-fluid-syntax")
		       ((module)              "a module definition")
		       ((library)             "a library definition")
		       ((import)              "an import declaration")
		       ((export)              "an export declaration")
		       (else                  "a non-expression"))
		     " was found where an expression was expected")))
	((mutable)
	 (if (and (pair? value) (let ((lib (car value))) (eq? lib '*interaction*)))
	     (let ((loc (cdr value))) (build-global-reference no-source loc))
	   (stx-error e "attempt to reference an unexportable variable")))
	(else
		;(assertion-violation 'chi-expr "invalid type " type (strip e '()))
	 (stx-error e "invalid expression"))))))

(define chi-set!
  (lambda (e r mr)
    (syntax-match e ()
      ((_ x v) (identifier? x)
       (let-values (((type value kwd) (syntax-type x r)))
	 (case type
	   ((lexical)
	    (set-lexical-mutable! value #t)
	    (build-lexical-assignment no-source
				      (lexical-var value)
				      (chi-expr v r mr)))
	   ((core-prim)
	    (stx-error e "cannot modify imported core primitive"))
	   ((global)
	    (stx-error e "attempt to modify an immutable binding"))
	   ((global-macro!)
	    (chi-expr (chi-global-macro value e r #f) r mr))
	   ((local-macro!)
	    (chi-expr (chi-local-macro value e r #f) r mr))
	   ((mutable)
	    (if (and (pair? value) (let ((lib (car value))) (eq? lib '*interaction*)))
		(let ((loc (cdr value)))
		  (build-global-assignment no-source loc
					   (chi-expr v r mr)))
	      (stx-error e "attempt to modify an unexportable variable")))
	   (else (stx-error e))))))))

(define chi-lambda-clause
  (lambda (stx fmls body* r mr)
    (syntax-match fmls ()
      ((x* ...)
       (begin
	 (%verify-formals-syntax fmls stx)
	 (let ((lex* (map gensym-for-lexical-var x*))
	       (lab* (map gensym-for-label x*)))
	   (values
	    lex*
	    (chi-internal
	     (push-lexical-contour (make-full-rib x* lab*) body*)
	     (add-lexicals lab* lex* r)
	     mr)))))
      ((x* ... . x)
       (begin
	 (%verify-formals-syntax fmls stx)
	 (let ((lex* (map gensym-for-lexical-var x*)) (lab* (map gensym-for-label x*))
	       (lex (gensym-for-lexical-var x)) (lab (gensym-for-label x)))
	   (values
	    (append lex* lex)
	    (chi-internal
	     (push-lexical-contour
	      (make-full-rib (cons x x*) (cons lab lab*))
	      body*)
	     (add-lexicals (cons lab lab*) (cons lex lex*) r)
	     mr)))))
      (_ (stx-error fmls "invalid syntax")))))

(define chi-lambda-clause*
  (lambda (stx fmls* body** r mr)
    (cond
     ((null? fmls*) (values '() '()))
     (else
      (let-values (((a b)
		    (chi-lambda-clause stx (car fmls*) (car body**) r mr)))
	(let-values (((a* b*)
		      (chi-lambda-clause* stx (cdr fmls*) (cdr body**) r mr)))
	  (values (cons a a*) (cons b b*))))))))

(define (chi-defun x r mr)
  (syntax-match x ()
    ((_ (ctxt . fmls) . body*)
     (let-values (((fmls body)
		   (chi-lambda-clause fmls fmls body* r mr)))
       (build-lambda (syntax-annotation ctxt) fmls body)))))

(define chi-rhs
  (lambda (rhs r mr)
    (case (car rhs)
      ((defun) (chi-defun (cdr rhs) r mr))
      ((expr)
       (let ((expr (cdr rhs)))
	 (chi-expr expr r mr)))
      ((top-expr)
       (let ((expr (cdr rhs)))
	 (build-sequence no-source
			 (list (chi-expr expr r mr)
			       (build-void)))))
      (else (assertion-violation 'chi-rhs "BUG: invalid rhs" rhs)))))

(define chi-rhs*
  (lambda (rhs* r mr)
    (let f ((ls rhs*))
      (cond ;;; chi-rhs in order
       ((null? ls) '())
       (else
	(let ((a (chi-rhs (car ls) r mr)))
	  (cons a (f (cdr ls)))))))))

(define chi-internal
  (lambda (e* r mr)
    (let ((rib (make-empty-rib)))
      (let-values (((e* r mr lex* rhs* mod** kwd* _exp*)
		    (chi-body*
		     (map (lambda (x) (push-lexical-contour rib x)) (syntax->list e*))
		     r mr '() '() '() '() '() rib #f #t)))
	(when (null? e*)
	  (stx-error e* "no expression in body"))
	(let* ((init*
		(chi-expr* (append (apply append (reverse mod**)) e*) r mr))
	       (rhs* (chi-rhs* rhs* r mr)))
	  (build-letrec* no-source
			 (reverse lex*) (reverse rhs*)
			 (build-sequence no-source init*)))))))


;;;; chi procedures: body

(module (chi-body*)

  (define (chi-body* e* r mr lex* rhs* mod** kwd* exp* rib mix? sd?)
    (if (null? e*)
	(values e* r mr lex* rhs* mod** kwd* exp*)
      (let ((e (car e*)))
	(let-values (((type value kwd) (syntax-type e r)))
	  (let ((kwd* (if (identifier? kwd) (cons kwd kwd*) kwd*)))
	    (case type
	      ((define)
	       (let-values (((id rhs) (parse-define e)))
		 (when (bound-id-member? id kwd*)
		   (stx-error e "cannot redefine keyword"))
		 (let-values (((lab lex) (gen-define-label+loc id rib sd?)))
		   (extend-rib! rib id lab sd?)
		   (chi-body* (cdr e*)
			      (add-lexical lab lex r) mr
			      (cons lex lex*) (cons rhs rhs*)
			      mod** kwd* exp* rib mix? sd?))))
	      ((define-syntax)
	       (let-values (((id rhs) (%parse-define-syntax e)))
		 (when (bound-id-member? id kwd*)
		   (stx-error e "cannot redefine keyword"))
		 (let* ((lab (gen-define-label id rib sd?))
			(expanded-rhs (%expand-macro-transformer rhs mr)))
		   (extend-rib! rib id lab sd?)
		   (let ((b (make-eval-transformer expanded-rhs)))
		     (chi-body* (cdr e*)
				(cons (cons lab b) r) (cons (cons lab b) mr)
				lex* rhs* mod** kwd* exp* rib
				mix? sd?)))))
	      ((define-fluid-syntax)
	       (let-values (((id rhs) (%parse-define-syntax e)))
		 (when (bound-id-member? id kwd*)
		   (stx-error e "cannot redefine keyword"))
		 (let* ((lab (gen-define-label id rib sd?))
			(flab (gen-define-label id rib sd?))
			(expanded-rhs (%expand-macro-transformer rhs mr)))
		   (extend-rib! rib id lab sd?)
		   (let ((b (make-eval-transformer expanded-rhs)))
		     (let ((t1 (cons lab (cons '$fluid flab)))
			   (t2 (cons flab b)))
		       (chi-body* (cdr e*)
				  (cons* t1 t2 r) (cons* t1 t2 mr)
				  lex* rhs* mod** kwd* exp* rib
				  mix? sd?))))))
	      ((let-syntax letrec-syntax)
	       (syntax-match e ()
		 ((_ ((xlhs* xrhs*) ...) xbody* ...)
		  (unless (valid-bound-ids? xlhs*)
		    (stx-error e "invalid identifiers"))
		  (let* ((xlab* (map gensym-for-label xlhs*))
			 (xrib (make-full-rib xlhs* xlab*))
			 (xb* (map (lambda (x)
				     (make-eval-transformer
				      (%expand-macro-transformer
				       (if (eq? type 'let-syntax)
					   x
					 (push-lexical-contour xrib x))
				       mr)))
				xrhs*)))
		    (chi-body*
		     (append (map (lambda (x) (push-lexical-contour xrib x)) xbody*) (cdr e*))
		     (append (map cons xlab* xb*) r)
		     (append (map cons xlab* xb*) mr)
		     lex* rhs* mod** kwd* exp* rib
		     mix? sd?)))))
	      ((begin)
	       (syntax-match e ()
		 ((_ x* ...)
		  (chi-body* (append x* (cdr e*))
			     r mr lex* rhs* mod** kwd* exp* rib
			     mix? sd?))))
	      ((stale-when)
	       (syntax-match e ()
		 ((_ guard x* ...)
		  (begin
		    (handle-stale-when guard mr)
		    (chi-body* (append x* (cdr e*))
			       r mr lex* rhs* mod** kwd* exp* rib
			       mix? sd?)))))
	      ((global-macro global-macro!)
	       (chi-body*
		(cons (chi-global-macro value e r rib) (cdr e*))
		r mr lex* rhs* mod** kwd* exp* rib mix? sd?))
	      ((local-macro local-macro!)
	       (chi-body*
		(cons (chi-local-macro value e r rib) (cdr e*))
		r mr lex* rhs* mod** kwd* exp* rib mix? sd?))
	      ((macro macro!)
	       (chi-body*
		(cons (chi-macro value e r rib) (cdr e*))
		r mr lex* rhs* mod** kwd* exp* rib mix? sd?))
	      ((module)
	       (let-values (((lex* rhs* m-exp-id* m-exp-lab* r mr mod** kwd*)
			     (chi-internal-module e r mr lex* rhs* mod** kwd*)))
		 (vector-for-each
		     (lambda (id lab) (extend-rib! rib id lab sd?))
		   m-exp-id* m-exp-lab*)
		 (chi-body* (cdr e*) r mr lex* rhs* mod** kwd*
			    exp* rib mix? sd?)))
	      ((library)
	       (expand-library (syntax->datum e))
	       (chi-body* (cdr e*) r mr lex* rhs* mod** kwd* exp*
			  rib mix? sd?))
	      ((export)
	       (syntax-match e ()
		 ((_ exp-decl* ...)
		  (chi-body* (cdr e*) r mr lex* rhs* mod** kwd*
			     (append exp-decl* exp*) rib
			     mix? sd?))))
	      ((import)
	       (let ()
		 (define (module-import? e)
		   (syntax-match e ()
		     ((_ id) (identifier? id) #t)
		     ((_ imp* ...) #f)
		     (_ (stx-error e "malformed import form"))))
		 (define (module-import e r)
		   (syntax-match e ()
		     ((_ id) (identifier? id)
		      (let-values (((type value kwd) (syntax-type id r)))
			(case type
			  (($module)
			   (let ((iface value))
			     (values
			      (module-interface-exp-id* iface id)
			      (module-interface-exp-lab-vec iface))))
			  (else (stx-error e "invalid import")))))))
		 (define (library-import e)
		   (syntax-match e ()
		     ((ctxt imp* ...)
		      (receive (subst-names subst-labels)
			  (parse-import-spec* (syntax->datum imp*))
			(values (vector-map (lambda (name)
					      (datum->stx ctxt name))
				  subst-names)
				subst-labels)))
		     (_
		      (stx-error e "invalid import form"))))
		 (define (any-import ctxt e r)
		   (if (identifier? e)
		       (module-import (list ctxt e) r)
		     (library-import (list ctxt e))))
		 (define (any-import* ctxt e* r)
		   (if (null? e*)
		       (values '#() '#())
		     (let-values (((t1 t2) (any-import ctxt (car e*) r))
				  ((t3 t4) (any-import* ctxt (cdr e*) r)))
		       (values (vector-append t1 t3)
			       (vector-append t2 t4)))))
		 (define (any-import*-checked e r)
		   (syntax-match e ()
		     ((ctxt e* ...) (any-import* ctxt e* r))
		     (_ (stx-error e "invalid import form"))))
		 (let-values (((id* lab*)
			       ;;(if (module-import? e)
			       ;;    (module-import e r)
			       ;;  (library-import e))
			       (any-import*-checked e r)))
		   (vector-for-each
		       (lambda (id lab) (extend-rib! rib id lab sd?))
		     id* lab*))
		 (chi-body* (cdr e*) r mr lex* rhs* mod** kwd*
			    exp* rib mix? sd?)))
	      (else
	       (if mix?
		   (chi-body* (cdr e*) r mr
			      (cons (gensym-for-lexical-var 'dummy) lex*)
			      (cons (cons 'top-expr e) rhs*)
			      mod** kwd* exp* rib #t sd?)
		 (values e* r mr lex* rhs* mod** kwd* exp*)))))))))

  (define (parse-define x)
    ;;Syntax parser for R6RS's DEFINE.
    ;;
    (syntax-match x ()
      ((_ (id . fmls) b b* ...)
       (identifier? id)
       (begin
	 (%verify-formals-syntax fmls x)
	 (values id (cons 'defun x))))

      ((_ id val) (identifier? id)
       (values id (cons 'expr val)))

      ((_ id)
       (identifier? id)
       (values id (cons 'expr (bless '(void)))))
      ))

  (define (%parse-define-syntax stx)
    ;;Syntax  parser for  R6RS's DEFINE-SYNTAX,  extended with  Vicare's
    ;;syntax.  Accept both:
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

  (define (chi-internal-module e r mr lex* rhs* mod** kwd*)
    (let-values (((name exp-id* e*) (parse-module e)))
      (let* ((rib (make-empty-rib))
	     (e*  (map (lambda (x)
			 (push-lexical-contour rib x))
		    (syntax->list e*))))
	(let-values (((e* r mr lex* rhs* mod** kwd* _exp*)
		      (chi-body* e* r mr lex* rhs* mod** kwd* '() rib #f #t)))
	  (let* ((exp-id*  (vector-append exp-id* (list->vector _exp*)))
		 (exp-lab* (vector-map
			       (lambda (x)
				 (or (id->label (make-<stx> (identifier->symbol x)
							    (<stx>-mark* x)
							    (list rib)
							    '()))
				     (stx-error x "cannot find module export")))
			     exp-id*))
		 (mod** (cons e* mod**)))
	    (if (not name) ;;; explicit export
		(values lex* rhs* exp-id* exp-lab* r mr mod** kwd*)
	      (let ((lab (gensym-for-label 'module))
		    (iface
		     (make-module-interface
		      (car (<stx>-mark* name))
		      (vector-map
			  (lambda (x)
			    (make-<stx> (<stx>-expr x) (<stx>-mark* x) '() '()))
			exp-id*)
		      exp-lab*)))
		(values lex* rhs*
			(vector name) ;;; FIXME: module cannot
			(vector lab)  ;;;  export itself yet
			(cons (cons lab (cons '$module iface)) r)
			(cons (cons lab (cons '$module iface)) mr)
			mod** kwd*))))))))

  #| end of module: CHI-BODY* |# )


(define parse-module
  (lambda (e)
    (syntax-match e ()
      ((_ (export* ...) b* ...)
       (begin
	 (unless (for-all identifier? export*)
	   (stx-error e "module exports must be identifiers"))
	 (values #f (list->vector export*) b*)))
      ((_ name (export* ...) b* ...)
       (begin
	 (unless (identifier? name)
	   (stx-error e "module name must be an identifier"))
	 (unless (for-all identifier? export*)
	   (stx-error e "module exports must be identifiers"))
	 (values name (list->vector export*) b*))))))

(define-record module-interface (first-mark exp-id-vec exp-lab-vec))

(define (module-interface-exp-id* iface id)
  (define (diff-marks ls x)
    (when (null? ls) (error 'diff-marks "BUG: should not happen"))
    (let ((a (car ls)))
      (if (eq? a x)
	  '()
	(cons a (diff-marks (cdr ls) x)))))
  (let ((diff
	 (diff-marks (<stx>-mark* id) (module-interface-first-mark iface)))
	(id-vec (module-interface-exp-id-vec iface)))
    (if (null? diff)
	id-vec
      (vector-map
	  (lambda (x)
	    (make-<stx> (<stx>-expr x) (append diff (<stx>-mark* x)) '() '()))
	id-vec))))


(define (rev-map-append f ls ac)
  (cond
   ((null? ls) ac)
   (else
    (rev-map-append f (cdr ls)
		    (cons (f (car ls)) ac)))))

(define build-exports
  (lambda (lex*+loc* init*)
    (build-sequence no-source
		    (cons (build-void)
			  (rev-map-append
			   (lambda (x)
			     (build-global-assignment no-source (cdr x) (car x)))
			   lex*+loc*
			   init*)))))

(define (make-export-subst name* id*)
  (map
      (lambda (name id)
        (let ((label (id->label id)))
          (unless label
            (stx-error id "cannot export unbound identifier"))
          (cons name label)))
    name* id*))

(define (make-export-env/macros lex* loc* r)
  (define (lookup x)
    (let f ((x x) (lex* lex*) (loc* loc*))
      (cond
       ((pair? lex*)
	(if (eq? x (car lex*))
	    (car loc*)
	  (f x (cdr lex*) (cdr loc*))))
       (else (assertion-violation 'lookup-make-export "BUG")))))
  (let f ((r r) (env '()) (global* '()) (macro* '()))
    (cond
     ((null? r) (values env global* macro*))
     (else
      (let ((x (car r)))
	(let ((label (car x)) (b (cdr x)))
	  (case (binding-type b)
	    ((lexical)
	     (let ((v (binding-value b)))
	       (let ((loc (lookup (lexical-var v)))
		     (type (if (lexical-mutable? v)
			       'mutable
			     'global)))
		 (f (cdr r)
		    (cons (cons* label type loc) env)
		    (cons (cons (lexical-var v) loc) global*)
		    macro*))))
	    ((local-macro)
	     (let ((loc (gensym)))
	       (f (cdr r)
		  (cons (cons* label 'global-macro loc) env)
		  global*
		  (cons (cons loc (binding-value b)) macro*))))
	    ((local-macro!)
	     (let ((loc (gensym)))
	       (f (cdr r)
		  (cons (cons* label 'global-macro! loc) env)
		  global*
		  (cons (cons loc (binding-value b)) macro*))))
	    ((local-ctv)
	     (let ((loc (gensym)))
	       (f (cdr r)
		  (cons (cons* label 'global-ctv loc) env)
		  global*
		  (cons (cons loc (binding-value b)) macro*))))
	    (($rtd $module $fluid)
	     (f (cdr r) (cons x env) global* macro*))
	    (else
	     (assertion-violation 'expander "BUG: do not know how to export"
				  (binding-type b) (binding-value b))))))))))

(define generate-temporaries
  (lambda (ls)
    (syntax-match ls ()
      ((ls ...)
       (map (lambda (x)
	      (make-<stx>
	       (let ((x (syntax->datum x)))
		 (cond
		  ((or (symbol? x) (string? x))
		   (gensym x))
		  (else (gensym 't))))
	       top-mark* '() '()))
	 ls))
      (_
       (assertion-violation 'generate-temporaries "not a list")))))

(define free-identifier=?
  (lambda (x y)
    (if (identifier? x)
	(if (identifier? y)
	    (free-id=? x y)
	  (assertion-violation 'free-identifier=? "not an identifier" y))
      (assertion-violation 'free-identifier=? "not an identifier" x))))

(define bound-identifier=?
  (lambda (x y)
    (if (identifier? x)
	(if (identifier? y)
	    (bound-id=? x y)
	  (assertion-violation 'bound-identifier=? "not an identifier" y))
      (assertion-violation 'bound-identifier=? "not an identifier" x))))

(define (expression->source-position-condition x)
  (expression-position x))

(define (expression-position x)
  (if (<stx>? x)
      (let ((x (<stx>-expr x)))
	(if (annotation? x)
	    (annotation-textual-position x)
	  (condition)))
    (condition)))

(define (syntax-annotation x)
  (if (<stx>? x) (<stx>-expr x) x))

		;  (define (syntax-annotation x)
		;    (if (<stx>? x)
		;        (let ((expr (<stx>-expr x)))
		;          (if (annotation? x)
		;              x
		;              (syntax->datum x)))
		;        (syntax->datum x)))


;;;; errors

(define (assertion-error expr source-identifier
			 byte-offset character-offset
			 line-number column-number)
  ;;Invoked by the  expansion of the ASSERT macro to  raise an assertion
  ;;violation.
  ;;
  (raise
   (condition (make-assertion-violation)
	      (make-who-condition 'assert)
	      (make-message-condition "assertion failed")
	      (make-irritants-condition (list expr))
	      (make-source-position-condition source-identifier
					      byte-offset character-offset
					      line-number column-number))))

(define syntax-violation
  ;;Defined  by R6RS.   WHO must  be false  or a  string or  a symbol.
  ;;MESSAGE must be a string.  FORM must be a syntax object or a datum
  ;;value.  SUBFORM must be a syntax object or a datum value.
  ;;
  ;;The  SYNTAX-VIOLATION procedure raises  an exception,  reporting a
  ;;syntax violation.  WHO should  describe the macro transformer that
  ;;detected the exception.  The  MESSAGE argument should describe the
  ;;violation.  FORM should be the erroneous source syntax object or a
  ;;datum value  representing a  form.  The optional  SUBFORM argument
  ;;should be a syntax object  or datum value representing a form that
  ;;more precisely locates the violation.
  ;;
  ;;If WHO is false, SYNTAX-VIOLATION attempts to infer an appropriate
  ;;value for the  condition object (see below) as  follows: when FORM
  ;;is  either  an  identifier  or  a  list-structured  syntax  object
  ;;containing an  identifier as its first element,  then the inferred
  ;;value is the identifier's symbol.   Otherwise, no value for WHO is
  ;;provided as part of the condition object.
  ;;
  ;;The condition object provided with the exception has the following
  ;;condition types:
  ;;
  ;;*  If WHO  is not  false  or can  be inferred,  the condition  has
  ;;condition type  "&who", with  WHO as the  value of its  field.  In
  ;;that  case,  WHO should  identify  the  procedure  or entity  that
  ;;detected the  exception.  If it  is false, the condition  does not
  ;;have condition type "&who".
  ;;
  ;;* The condition has condition type "&message", with MESSAGE as the
  ;;value of its field.
  ;;
  ;;* The condition has condition type "&syntax" with FORM and SUBFORM
  ;;as the value of its fields.  If SUBFORM is not provided, the value
  ;;of the subform field is false.
  ;;
  (case-lambda
   ((who msg form)
    (syntax-violation who msg form #f))
   ((who msg form subform)
    (%syntax-violation who msg form
		       (make-syntax-violation form subform)))))

(define-syntax stx-error
  ;;Convenience wrapper for raising syntax violations.
  ;;
  (syntax-rules (quote)
    ((_ ?expr-stx)
     (syntax-violation #f "invalid syntax" ?expr-stx))
    ((_ ?expr-stx ?msg)
     (syntax-violation #f ?msg ?expr-stx))
    ))

(module (syntax-error
	 %syntax-violation
	 %raise-unbound-error)

  (define (syntax-error x . args)
    (unless (for-all string? args)
      (assertion-violation 'syntax-error "invalid argument" args))
    (raise
     (condition (make-message-condition (if (null? args)
					    "invalid syntax"
					  (apply string-append args)))
		(make-syntax-violation (syntax->datum x) #f)
		(expression->source-position-condition x)
		(extract-trace x))))

  (define (%syntax-violation source-who msg form condition-object)
    (define who 'syntax-violation)
    (unless (string? msg)
      (assertion-violation who "message is not a string" msg))
    (let ((source-who (cond ((or (string? source-who)
				 (symbol? source-who))
			     source-who)
			    ((not source-who)
			     (syntax-match form ()
			       (id
				(identifier? id)
				(syntax->datum id))
			       ((id . rest)
				(identifier? id)
				(syntax->datum id))
			       (_  #f)))
			    (else
			     (assertion-violation who "invalid who argument" source-who)))))
      (raise
       (condition (if source-who
		      (make-who-condition source-who)
		    (condition))
		  (make-message-condition msg)
		  condition-object
		  (expression->source-position-condition form)
		  (extract-trace form)))))

  (define (%raise-unbound-error source-who form id)
    (raise
     (condition (if source-who
		    (make-who-condition source-who)
		  (condition))
		(make-message-condition "unbound identifier")
		(make-undefined-violation)
		(make-syntax-violation form id)
		(expression->source-position-condition id)
		(extract-trace id))))

  (define (extract-trace x)
    (define-condition-type &trace &condition
      make-trace trace?
      (form trace-form))
    (let f ((x x))
      (cond ((<stx>? x)
	     (apply condition
		    (make-trace x)
		    (map f (<stx>-ae* x))))
	    ((annotation? x)
	     (make-trace (make-<stx> x '() '() '())))
	    (else
	     (condition)))))

  #| end of module |# )


;;;; miscellaneous collectors

(define (make-collector)
  (let ((ls '()))
    (case-lambda
     (()
      ls)
     ((x)
      (unless (eq? x '*interaction*)
	;;Prepend  X to  the  list LS  if it  is  not already  contained
	;;according to EQ?.
	(set! ls (if (memq x ls)
		     ls
		   (cons x ls))))))))

(define inv-collector
  (make-parameter
      (lambda args
        (assertion-violation 'inv-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'inv-collector "BUG: not a procedure" x))
      x)))

(define vis-collector
  (make-parameter
      (lambda args
        (assertion-violation 'vis-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'vis-collector "BUG: not a procedure" x))
      x)))

(define imp-collector
  ;;Imported  libraries  collector.   Holds a  collector  function  (see
  ;;MAKE-COLLECTOR)  filled with  the LIBRARY  structs representing  the
  ;;libraries from an R6RS import specification.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'imp-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'imp-collector "BUG: not a procedure" x))
      x)))


;;;; stale stuff

(define stale-when-collector
  (make-parameter #f))

(module (make-stale-collector)

  (define (make-stale-collector)
    (let ((code (build-data no-source #f))
	  (req* '()))
      (case-lambda
       (()
	(values code req*))
       ((c r*)
	(set! code (build-conditional no-source code (build-data no-source #t) c))
	(set! req* (%set-union r* req*))))))

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

  #| end of module: MAKE-STALE-COLLECTOR |# )

(define (handle-stale-when guard-expr mr)
  (let ((stc (make-collector)))
    (let ((core-expr (parametrise ((inv-collector stc))
		       (chi-expr guard-expr mr mr))))
      (cond ((stale-when-collector)
	     => (lambda (c)
		  (c core-expr (stc))))))))


;;;; R6RS programs and libraries helpers

(define (initial-visit! macro*)
  (for-each (lambda (x)
	      (let ((loc  (car  x))
		    (proc (cadr x)))
		(set-symbol-value! loc proc)))
    macro*))


;;;; done

;;Register the expander with the library manager.
(current-library-expander expand-library)

)

;;; end of file
;;Local Variables:
;;eval: (put 'build-application 'scheme-indent-function 1)
;;eval: (put 'build-conditional 'scheme-indent-function 1)
;;eval: (put 'build-lambda 'scheme-indent-function 1)
;;End:
