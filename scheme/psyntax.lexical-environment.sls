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


(library (psyntax.lexical-environment)
  (export

    ;; module interfaces
    PSYNTAX-SYNTAX-MATCH
    PSYNTAX-SYNTAX-UTILITIES
    PSYNTAX-TYPE-IDENTIFIERS-AND-SIGNATURES

    ;;configuration
    generate-descriptive-gensyms?
    generate-descriptive-marks?

    top-level-context
    self-evaluating?

    ;; non-interaction environment objects
    env
    make-env				env?
    env-names
    env-labels
    env-itc

    ;; interaction environment objects
    interaction-env
    make-interaction-env		interaction-env?
    interaction-env-rib			set-interaction-env-rib!
    interaction-env-lexenv		set-interaction-env-lexenv!

    ;; operations
    environment?
    environment-symbols
    environment-labels
    environment-libraries
    environment-binding

    ;; syntax utilities
    generate-temporaries
    syntax-null?
    syntax-pair?			syntax-list?
    syntax-car				syntax-cdr
    syntax->list
    syntax-vector?			syntax-vector->list
    syntax-unwrap

    parse-logic-predicate-syntax
    error-invalid-formals-syntax

    ;; label gensyms, lexical variable gensyms, storage location gensyms
    generate-lexical-gensym
    generate-storage-location-gensym
    generate-label-gensym

    ;; LEXENV entries and syntactic binding descriptors
    make-lexenv-entry
    lexenv-entry.label
    lexenv-entry.binding-descriptor
    push-entry-on-lexenv

    syntactic-binding-descriptor.type
    syntactic-binding-descriptor.value

    make-syntactic-binding-descriptor/lexical-var
    lexical-var-binding-descriptor-value.lex-name
    lexical-var-binding-descriptor-value.assigned?
    lexenv-add-lexical-var-binding
    lexenv-add-lexical-var-bindings

    make-syntactic-binding-descriptor/lexical-typed-var
    make-syntactic-binding-descriptor/lexical-typed-var/from-data
    syntactic-binding-descriptor/lexical-typed-var?
    syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec
    syntactic-binding-descriptor/lexical-typed-var.value.lex
    syntactic-binding-descriptor/lexical-typed-var.value.type-id
    syntactic-binding-descriptor/lexical-typed-var.value.assigned?

    make-global-typed-variable-spec-and-maker-core-expr
    syntactic-binding-descriptor/global-typed-var?
    syntactic-binding-descriptor/global-typed-var.typed-variable-spec

    ;; core primitive with type signature
    syntactic-binding-descriptor/core-prim-typed?
    core-prim-typed-binding-descriptor.prim-name
    core-prim-typed-binding-descriptor.type-id
    core-prim-typed-binding-descriptor.unsafe-variant-id
    core-prim-typed-binding-descriptor.value.prim-name
    core-prim-typed-binding-descriptor.value.type-id
    core-prim-typed-binding-descriptor.value.unsafe-variant-id

    ;; syntactic bindings utilities: base object-type specifications
    syntactic-binding-descriptor/object-type-name?

    ;; Scheme object-type specifications
    syntactic-binding-descriptor/scheme-type-name?
    syntactic-binding-descriptor/core-scheme-type-name?
    syntactic-binding-descriptor/core-list-type-name?

    make-syntactic-binding-descriptor/closure-type-name
    syntactic-binding-descriptor/closure-type-name?
    make-syntactic-binding/closure-type-name
    make-fabricated-closure-type-name

    make-syntactic-binding-descriptor/list-sub-type-name
    syntactic-binding-descriptor/list-sub-type-name?

    make-syntactic-binding-descriptor/vector-sub-type-name
    syntactic-binding-descriptor/vector-sub-type-name?

    ;; syntactic bindings utilities: struct-type specifications
    make-syntactic-binding-descriptor/struct-type-name
    syntactic-binding-descriptor/struct-type-name?
    struct-type-name-binding-descriptor.type-descriptor

    ;; syntactic bindings utilities: record-type specifications
    make-syntactic-binding-descriptor/record-type-name
    syntactic-binding-descriptor/record-type-name?

    ;; syntactic bindings utilities: macros
    make-syntactic-binding-descriptor/local-macro/non-variable-transformer
    syntactic-binding-descriptor/local-macro/non-variable-transformer?
    make-syntactic-binding-descriptor/local-macro/variable-transformer
    syntactic-binding-descriptor/local-macro/variable-transformer?
    make-syntactic-binding-descriptor/local-global-macro/fluid-syntax
    syntactic-binding-descriptor/fluid-syntax?
    fluid-syntax-binding-descriptor.fluid-label
    make-syntactic-binding-descriptor/local-global-macro/synonym-syntax
    make-syntactic-binding-descriptor/local-macro/expand-time-value
    local-expand-time-value-binding-descriptor.object
    global-expand-time-value-binding-descriptor.object
    retrieve-expand-time-value
    make-syntactic-binding-descriptor/local-global-macro/module-interface

    make-syntactic-binding-descriptor/pattern-variable
    syntactic-binding-descriptor/pattern-variable?

    ;; object types specifications: base types
    <object-type-spec>
    object-type-spec?
    object-type-spec.parent-id
    object-type-spec.constructor-sexp			object-type-spec.destructor-sexp
    object-type-spec.type-predicate-sexp
    object-type-spec.safe-accessor-sexp			object-type-spec.safe-mutator-sexp
    object-type-spec.applicable-method-sexp
    object-type-spec.memoised-list-id			object-type-spec.memoised-list-id-set!

    ;; object-type specifications: utilities
    object-type-spec.subtype-and-supertype?		object-type-spec-override-predicate

    ;; object-type specifications: built-in object types
    <scheme-type-spec>					scheme-type-spec?

    ;; object-type specifications: closure object
    <closure-type-spec>
    make-closure-type-spec				closure-type-spec?
    closure-type-spec.signature

    ;; object-type specifications: compound types, lists
    <list-type-spec>
    <list-type-spec>-rtd				<list-type-spec>-rcd
    make-list-type-spec					list-type-spec?
    list-type-spec.type-id

    ;; object-type specifications: compound types, vectors
    <vector-type-spec>
    <vector-type-spec>-rtd				<vector-type-spec>-rcd
    make-vector-type-spec				vector-type-spec?
    vector-type-spec.type-id

    ;; object-type specifications: structs
    <struct-type-spec>
    make-struct-type-spec				struct-type-spec?
    struct-type-spec.std

    ;; object types specifications: R6RS records
    <record-type-spec>
    make-record-type-spec				record-type-spec?
    record-type-spec.rtd-id				record-type-spec.rcd-id
    record-type-spec.super-protocol-id

    ;; typed variable specification: base type
    <typed-variable-spec>
    typed-variable-spec?
    typed-variable-spec.type-id
    typed-variable-spec.unsafe-variant-sexp		typed-variable-spec.unsafe-variant-sexp-set!

    ;; typed lexical variable specification
    <lexical-typed-variable-spec>
    make-lexical-typed-variable-spec			lexical-typed-variable-spec?
    lexical-typed-variable-spec.lex			lexical-typed-variable-spec.type-id
    lexical-typed-variable-spec.assigned?		lexical-typed-variable-spec.assigned?-set!

    ;; typed global variable specification
    <global-typed-variable-spec>
    make-global-typed-variable-spec			global-typed-variable-spec?
    global-typed-variable-spec.variable-loc		global-typed-variable-spec.type-id

    ;; lexical environment utilities
    label->syntactic-binding-descriptor
    label->syntactic-binding-descriptor/no-indirection

    system-label	system-label-gensym	SYSTEM-LABEL-GENSYM

    ;; type signature
    <type-signature>
    <type-signature>-rtd				<type-signature>-rcd
    make-type-signature					type-signature?
    type-signature-tags

    ;; marks of lexical contours
    src-marked?
    rib-src-marked-source-names

    ;; rib objects
    make-rib					rib?
    rib-name*					set-rib-name*!
    rib-mark**					set-rib-mark**!
    rib-label*					set-rib-label*!
    rib-sealed/freq				set-rib-sealed/freq!

    ;; rib operations
    false-or-rib?				list-of-ribs?
    make-rib/empty
    make-rib/from-identifiers-and-labels	make-rib/top-from-source-names-and-labels
    seal-rib!					unseal-rib!
    extend-rib!					export-subst->rib

    ;; syntax objects basics
    syntax-object?			stx?
    stx					mkstx
    make-stx
    stx-expr				stx-mark*
    stx-rib*				stx-annotated-expr*
    datum->syntax			~datum->syntax
    syntax->datum
    push-lexical-contour
    syntax-annotation			syntax-object-strip-annotations
    make-top-level-syntactic-identifier-from-source-name-and-label
    make-syntactic-identifier-for-temporary-variable
    make-top-level-syntax-object/quoted-quoting
    wrap-source-expression

    ;; syntax objects: mapping identifiers to labels and similar
    id->label				id->label/or-error
    id->object-type-specification
    id->record-type-specification
    id->struct-type-specification
    id->typed-variable-spec
    case-identifier-syntactic-binding-descriptor
    case-identifier-syntactic-binding-descriptor/no-indirection
    __descr__

    ;; syntax objects: marks
    same-marks?
    join-wraps
    ADD-MARK

    ;; identifiers from the built-in environment
    system-id-gensym			system-id
    bless
    trace-bless
    scheme-stx
    core-prim-id
    underscore-id			underscore-id?
    ellipsis-id				ellipsis-id?
    place-holder-id			place-holder-id?
    brace-id				brace-id?
    method-id				method-id?
    case-method-id			case-method-id?
    procedure-pred-id

    ;; public interface: identifiers handling
    identifier?				false-or-identifier-bound?
    false-or-identifier?
    bound-identifier=?			~bound-identifier=?
    free-identifier=?			~free-identifier=?
    identifier-bound?			~identifier-bound?
    identifier->symbol
    syntax-parameter-value

    ;; core type identifiers
    procedure-tag-id		$procedure-tag-id?	procedure-tag-id?
    predicate-tag-id		$predicate-tag-id?	predicate-tag-id?
    list-tag-id			$list-tag-id?		list-tag-id?
    null-tag-id			$null-tag-id?		null-tag-id?
    vector-tag-id		$vector-tag-id?		vector-tag-id?
    nlist-tag-id		$nlist-tag-id?		nlist-tag-id?
    top-tag-id			$top-tag-id?		top-tag-id?
    boolean-tag-id		void-tag-id
    struct-tag-id		record-tag-id

    ;; utilities for identifiers
    valid-bound-ids?			distinct-bound-ids?
    duplicate-bound-formals?
    bound-id-member?			free-id-member?
    identifier-append

    ;; condition object types
    &type-syntactic-identifier
    &type-syntactic-identifier-rtd
    &type-syntactic-identifier-rcd
    make-type-syntactic-identifier-condition
    type-syntactic-identifier-condition?
    condition-type-syntactic-identifier

    &argument-type-syntactic-identifier
    &argument-type-syntactic-identifier-rtd
    &argument-type-syntactic-identifier-rcd
    make-argument-type-syntactic-identifier-condition
    argument-type-syntactic-identifier-condition?
    condition-argument-type-syntactic-identifier

    &operand-type-syntactic-identifier
    &operand-type-syntactic-identifier-rtd
    &operand-type-syntactic-identifier-rcd
    make-operand-type-syntactic-identifier-condition
    operand-type-syntactic-identifier-condition?
    condition-operand-type-syntactic-identifier

    &argument-index
    &argument-index-rtd
    &argument-index-rcd
    make-argument-index-condition
    argument-index-condition?
    condition-argument-index

    &type-method-name
    make-type-method-name-condition
    condition-type-method-name?
    condition-type-method-name

    &syntactic-binding-descriptor
    make-syntactic-binding-descriptor-condition
    syntactic-binding-descriptor-condition?
    condition-syntactic-binding-descriptor

    &syntax-definition-expanded-rhs-condition
    make-syntax-definition-expanded-rhs-condition
    syntax-definition-expanded-rhs-condition?
    condition-syntax-definition-expanded-rhs

    &syntax-definition-expression-return-value
    make-syntax-definition-expression-return-value-condition
    syntax-definition-expression-return-value-condition?
    condition-syntax-definition-expression-return-value

    &application-operator
    make-application-operator-condition
    application-operator-condition?
    application-operator-condition.operator

    &application-operands
    make-application-operands-condition
    application-operands-condition?
    application-operands-condition.operands

    &retvals-signature-condition
    make-retvals-signature-condition
    retvals-signature-condition?
    retvals-signature-condition-signature

    &macro-use-input-form-condition
    make-macro-use-input-form-condition
    macro-use-input-form-condition?
    condition-macro-use-input-form

    &macro-expansion-trace
    make-macro-expansion-trace macro-expansion-trace?
    macro-expansion-trace-form

    &expand-time-type-signature-violation
    &expand-time-type-signature-violation-rtd
    &expand-time-type-signature-violation-rcd
    make-expand-time-type-signature-violation
    expand-time-type-signature-violation?

    &expand-time-retvals-signature-violation
    &expand-time-retvals-signature-violation-rtd
    &expand-time-retvals-signature-violation-rcd
    make-expand-time-retvals-signature-violation
    expand-time-retvals-signature-violation?
    expand-time-retvals-signature-violation-expected-signature
    expand-time-retvals-signature-violation-returned-signature

    assertion-error
    syntax-violation/internal-error
    assertion-violation/internal-error
    syntax-violation
    expand-time-retvals-signature-violation
    raise-unbound-error
    raise-compound-condition-object

    ;; helpers
    expression-position
    current-run-lexenv

    ;; bindings for (vicare expander)
    (rename (id->label syntactic-identifier->label))
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
    (vicare system $symbols)
    (rnrs mutable-pairs)
    (prefix (rnrs syntax-case) sys.)
    (psyntax.setup)
    (psyntax.compat)
    (psyntax.builders)
    (only (psyntax.special-transformers)
	  expand-time-value?
	  expand-time-value-object)
    (only (psyntax.library-manager)
	  library?
	  library-name
	  visit-library
	  label->imported-syntactic-binding-descriptor))


;;;; helpers

(include "psyntax.helpers.scm" #t)

(define-syntax let-syntax-rules
  (syntax-rules ()
    ((_ (((?lhs0 . ?args0) ?rhs0) ((?lhs . ?args) ?rhs) ...) ?body0 ?body ...)
     (let-syntax ((?lhs0 (syntax-rules () ((_ . ?args0) ?rhs0)))
		  (?lhs  (syntax-rules () ((_ . ?args)  ?rhs)))
		  ...)
       ?body0 ?body ...))
    ))


;;;; configuration

(define generate-descriptive-gensyms?
  (make-parameter #f))

(define generate-descriptive-marks?
  (make-parameter #f))


;;;; top-level environments
;;
;;The result of parsing a set of import specs and loading the corresponding libraries
;;with the R6RS function  ENVIRONMENT is a record of type  ENV; ENV records represent
;;an *immutable* top level environment.
;;
;;Whenever  a REPL  is created  (Vicare can  launch multiple  REPLs), an  interaction
;;environment  is  created  to  serve  as top  level  environment.   The  interaction
;;environment is initialised with the  core Vicare library "(vicare)"; an interaction
;;environment  is *mutable*:  new  bindings can  be  added to  it.   For this  reason
;;interaction environments are represented by  records of type INTERACTION-ENV, whose
;;internal format allows adding new bindings.
;;
;;Let's step  back: how does  the REPL  work?  Every time  we type an  expression and
;;press  "Return":  the  expression  is  expanded  in  the  context  of  the  current
;;interaction environment, compiled to machine code, executed.  Every REPL expression
;;is like  a full R6RS program,  with the exception that  the interaction environment
;;"remembers" the bindings we define.
;;
;;Notice that it is possible to use  an interaction environment as second argument to
;;EVAL, allowing for persistence of bindings among evaluations:
;;
;;   (eval '(begin
;;            (define a 1)
;;            (define b 2))
;;         (interaction-environment))
;;   (eval '(list a b)
;;         (interaction-environment))
;;   => (1 2)
;;

;;Set to false or a record of  type INTERACTION-ENV.  This parameter is meant to hold
;;the  top-level   context  when  running   the  REPL   or  invoking  EVAL   with  an
;;INTERACTION-ENV as initial lexical environment.
;;
(define top-level-context
  (make-parameter #f))


;;;; top level environment objects: type definitions

;;An ENV record encapsulates a substitution and a set of libraries.
;;
(define-record-type env
  (nongenerative vicare:expander:env)
  (fields names
		;A vector of symbols representing the public names of bindings from a
		;set of  import specifications as  defined by R6RS.  These  names are
		;from  the  subst  of  the  libraries,  already  processed  with  the
		;directives  in  the import  sets  (prefix,  deprefix, only,  except,
		;rename).
	  labels
		;A vector of  gensyms representing the labels of bindings  from a set
		;of import specifications as defined  by R6RS.  These labels are from
		;the subst of the libraries.
	  itc
		;A  collector  function  (see  MAKE-COLLECTOR)  holding  the  LIBRARY
		;records  representing the  libraries selected  by the  source IMPORT
		;specifications.  These libraries have already been interned.
	  ))

(module ()
  ($record-type-printer-set! (record-type-descriptor env)
    (lambda (S port sub-printer)
      (display "#<environment>" port))))

(define-record-type interaction-env
  (nongenerative vicare:expander:interaction-env)
  (fields (mutable rib		interaction-env-rib	set-interaction-env-rib!)
		;The  top  RIB   structure  for  the  evaluation  of   code  in  this
		;environment.  It maps bound identifiers to labels.
	  (mutable lexenv	interaction-env-lexenv	set-interaction-env-lexenv!)
		;The LEXENV  for both run  time and expand  time.  It maps  labels to
		;syntactic binding descriptors.
	  ))

(module ()
  ($record-type-printer-set! (record-type-descriptor interaction-env)
    (lambda (S port sub-printer)
      (display "#<interaction-environment>" port))))


;;;; top level environment objects: operations

(define (environment? obj)
  (or (env? obj)
      (interaction-env? obj)))

(define* (environment-symbols x)
  ;;Return a list of symbols representing the names of the bindings from
  ;;the given environment.
  ;;
  (cond ((env? x)
	 (vector->list (env-names x)))
	((interaction-env? x)
	 (map values (rib-name* ($interaction-env-rib x))))
	(else
	 (assertion-violation __who__ "not an environment" x))))

(define* (environment-labels x)
  ;;Return a  list of  symbols representing the  labels of  the bindings
  ;;from the given environment.
  ;;
  (unless (env? x)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" x))
  (vector->list (env-labels x)))

(define* (environment-libraries x)
  ;;Return  the  list  of  LIBRARY records  representing  the  libraries
  ;;forming the environment.
  ;;
  (unless (env? x)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" x))
  ((env-itc x)))

(define* (environment-binding sym env)
  ;;Search the symbol SYM in the non-interaction environment ENV; if SYM
  ;;is the public  name of a binding  in ENV return 2  values: the label
  ;;associated  to the  binding,  the list  of  values representing  the
  ;;binding.  If SYM is not present in ENV return false and false.
  ;;
  (unless (env? env)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" env))
  (let ((P (vector-exists (lambda (name label)
			    (import (vicare system $symbols))
			    (and (eq? sym name)
				 (cons label ($symbol-value label))))
	     (env-names  env)
	     (env-labels env))))
    (if P
	(values (car P) (cdr P))
      (values #f #f))))


;;;; include modules

;; Definitions and utilities for object type specification.
(include "psyntax.lexical-environment.object-type-specs.scm" #t)

;; Definitions and utilities for typed lexical variables specification.
(include "psyntax.lexical-environment.typed-variable-specs.scm" #t)

;; Lexical environment: LEXENV entries and syntactic bindings helpers.
(include "psyntax.lexical-environment.syntactic-bindings.scm" #t)


;;;; label gensym, lexical variable gensyms, storage location gensyms

(define-syntax-rule (%fastest-gensym)
  ($make-symbol #f))

;;Every syntactic binding  has a label associated  to it as unique  identifier in the
;;whole running process; this function generates such labels as gensyms.
;;
;;Labels must have  read/write EQ?  invariance to support  separate compilation (when
;;we write  the expanded sexp to  a file and then  read it back, the  labels must not
;;change and still be globally unique).
;;
(define* (generate-label-gensym seed)
  (if (generate-descriptive-gensyms?)
      (cond ((identifier? seed)
	     ($make-symbol (string-append "lab." ($symbol->string ($identifier->symbol seed)))))
	    ((symbol? seed)
	     ($make-symbol (string-append "lab." ($symbol->string seed))))
	    ((string? seed)
	     ($make-symbol (string-append "lab." seed)))
	    (else
	     (procedure-argument-violation __who__ "expected identifier, symbol or string as argument" seed)))
    (%fastest-gensym)))

;;Generate a unique  symbol to represent the  name of a lexical variable  in the core
;;language forms.  Such symbols have the purpose of being unique in the core language
;;expressions representing a full library or full program.
;;
(define* (generate-lexical-gensym seed)
  (if (generate-descriptive-gensyms?)
      (cond ((identifier? seed)
	     ($make-symbol (string-append "lex." ($symbol->string ($identifier->symbol seed)))))
	    ((symbol? seed)
	     ($make-symbol (string-append "lex." ($symbol->string seed))))
	    (else
	     (procedure-argument-violation __who__ "expected identifier, symbol or string as argument" seed)))
    (%fastest-gensym)))

(define* (generate-storage-location-gensym seed)
  ;;Build  and return  a gensym  to be  used as  storage location  for a
  ;;global lexical variable.  The "value" slot of such gensym is used to
  ;;hold the value of the variable.
  ;;
  (if (generate-descriptive-gensyms?)
      (cond ((identifier? seed)
	     ($make-symbol (string-append "loc." ($symbol->string ($identifier->symbol seed)))))
	    ((symbol? seed)
	     ($make-symbol (string-append "loc." ($symbol->string seed))))
	    ((string? seed)
	     ($make-symbol (string-append "loc." seed)))
	    (else
	     (procedure-argument-violation __who__ "expected identifier, symbol or string as argument" seed)))
    (%fastest-gensym)))


;;;; lexical environment: mapping labels to syntactic binding descriptors

(module (label->syntactic-binding-descriptor
	 label->syntactic-binding-descriptor/no-indirection)

  (define* (label->syntactic-binding-descriptor/no-indirection label lexenv)
    ;;Look up the  symbol LABEL in the  LEXENV as well as in  the global environment.
    ;;If an  entry with key LABEL  is found: return the  associated syntactic binding
    ;;descriptor;  if  no  matching  entry  is  found,  return  one  of  the  special
    ;;descriptors:
    ;;
    ;;   (displaced-lexical . ())
    ;;   (displaced-lexical . #f)
    ;;
    ;;If the  binding descriptor represents a  fluid syntax or a  synonym syntax: *do
    ;;not* follow through and return the binding descriptor of the syntax definition.
    ;;
    ;;Since all  labels are unique,  it doesn't  matter which environment  we consult
    ;;first; we lookup the global environment first because it's faster.
    ;;
    (cond ((not (symbol? label))
	   ;;If LABEL is  the result of a  previous call to ID->LABEL  for an unbound
	   ;;identifier: LABEL  is false.  This  check makes  it possible to  use the
	   ;;concise expression:
	   ;;
	   ;;   (define ?binding
	   ;;     (label->syntactic-binding-descriptor (id->label ?id) ?lexenv))
	   ;;
	   ;;provided that later we check for the type of ?BINDING.
	   SYNTACTIC-BINDING-DESCRIPTOR/INVALID-LABEL)

	  ;;If  a  label is  associated  to  a  syntactic  binding from  a  library's
	  ;;GLOBAL-ENV (including  those established by  the boot image): it  has the
	  ;;associated descriptor in  its "value" field; otherwise such  field is set
	  ;;to #f.
	  ;;
	  ;;So, if we have a label, we can check if it references an imported binding
	  ;;simply    by    checking    its    "value"   field;    this    is    what
	  ;;LABEL->IMPORTED-SYNTACTIC-BINDING-DESCRIPTOR does.
	  ;;
	  ((label->imported-syntactic-binding-descriptor label)
	   => (lambda (descr)
		;;The  first   time  we  access  a   syntactic  binding's  descriptor
		;;representing a core definition: we mutate  it to a format usable by
		;;the code.
		(cond ((syntactic-binding-descriptor/hard-coded-core-prim-typed? descr)
		       (hard-coded-core-prim-typed-binding-descriptor->core-closure-type-name-binding-descriptor! descr))
		      ((syntactic-binding-descriptor/core-scheme-type-name? descr)
		       (core-scheme-type-name-symbolic-binding-descriptor->core-scheme-type-name-binding-descriptor! descr))
		      ((syntactic-binding-descriptor/core-condition-object-type-name? descr)
		       (core-condition-object-type-name-symbolic-binding-descriptor->core-record-type-name-binding-descriptor! descr))
		      ((syntactic-binding-descriptor/core-list-type-name? descr)
		       (core-list-type-name-symbolic-binding-descriptor->core-list-type-name-binding-descriptor! descr))
		      ((syntactic-binding-descriptor/core-record-type-name? descr)
		       (core-record-type-name-symbolic-binding-descriptor->core-record-type-name-binding-descriptor! descr)))
		descr))

	  ;;Search the given LEXENV.
	  ;;
	  ((assq label lexenv)
	   => lexenv-entry.binding-descriptor)

	  ;;Unbound label.
	  ;;
	  (else
	   SYNTACTIC-BINDING-DESCRIPTOR/UNBOUND-LABEL)))

  (define* (label->syntactic-binding-descriptor label lexenv)
    ;;Look up the  symbol LABEL in the  LEXENV as well as in  the global environment.
    ;;If an  entry with key LABEL  is found: return the  associated syntactic binding
    ;;descriptor;  if  no  matching  entry  is  found,  return  one  of  the  special
    ;;descriptors:
    ;;
    ;;   (displaced-lexical . ())
    ;;   (displaced-lexical . #f)
    ;;
    ;;If the binding  descriptor represents a fluid syntax or  synonym syntax: follow
    ;;through and return the innermost re-definition of the binding.
    ;;
    (if #f	;for debugging purposes
	(receive-and-return (descr)
	    (%label->descriptor label lexenv '())
	  (debug-print __who__ descr))
      (%label->descriptor label lexenv '())))

  (define (%label->descriptor label lexenv accum-labels)
    (let ((descr (label->syntactic-binding-descriptor/no-indirection label lexenv)))
      (cond ((eq? 'displaced-lexical (car descr))
	     descr)
	    ((syntactic-binding-descriptor/fluid-syntax? descr)
	     (%follow-through-fluid-descriptor descr lexenv accum-labels))
	    ((syntactic-binding-descriptor/synonym-syntax? descr)
	     (%follow-through-synonym-descriptor descr lexenv accum-labels))
	    (else
	     descr))))

  (define (%follow-through-fluid-descriptor descr lexenv accum-labels)
    ;;Fluid syntax bindings (created by DEFINE-FLUID-SYNTAX) require different logic.
    ;;The lexical environment contains the main fluid syntax definition entry and one
    ;;or more subordinate fluid syntax re-definition entries.
    ;;
    ;;LABEL is associated to the main binding definition entry; its syntactic binding
    ;;descriptor  contains the  fluid  label,  which is  associated  to  one or  more
    ;;subordinate  re-definitions.  We  extract the  fluid label  from BINDING,  then
    ;;search for the innermost fluid binding associated to it.
    ;;
    ;;Such search  for the fluid  re-definition binding  must begin from  LEXENV, and
    ;;then  in  the  global  environment.   This  is because  we  can  nest  at  will
    ;;FLUID-LET-SYNTAX forms that  redefine the binding by  pushing new re-definition
    ;;entries on  the LEXENV.  To  reach for the innermost  we must query  the LEXENV
    ;;first.
    ;;
    ;;If there is no binding descriptor for FLUID-LABEL: the return value will be:
    ;;
    ;;   (displaced-lexical . #f)
    ;;
    (let* ((fluid-label (fluid-syntax-binding-descriptor.fluid-label descr))
	   (fluid-descr (cond ((assq fluid-label lexenv)
			       => lexenv-entry.binding-descriptor)
			      (else
			       (label->syntactic-binding-descriptor/no-indirection fluid-label '())))))
      (if (syntactic-binding-descriptor/synonym-syntax? fluid-descr)
	  (%follow-through-synonym-descriptor descr lexenv accum-labels)
	fluid-descr)))

  (define (%follow-through-synonym-descriptor descr lexenv accum-labels)
    (let ((synonym-label (synonym-syntax-binding-descriptor.synonym-label descr)))
      (if (memq synonym-label accum-labels)
	  (syntax-violation #f "circular reference detected while resolving synonym transformers" #f)
	(%label->descriptor synonym-label lexenv (cons synonym-label accum-labels)))))

  (define-constant SYNTACTIC-BINDING-DESCRIPTOR/UNBOUND-LABEL
    '(displaced-lexical . #f))

  (define-constant SYNTACTIC-BINDING-DESCRIPTOR/INVALID-LABEL
    '(displaced-lexical . ()))

  #| end of module |# )


;;;; marks of lexical contours

(module (generate-new-mark)

  (define counter 0)

  (define (generate-new-mark)
    ;;Generate a new unique mark.  We want a new string for every function call.
    ;;
    (if (generate-descriptive-marks?)
	(begin
	  (set! counter (fxadd1 counter))
	  (string-append "mark." (number->string counter)))
      (string)))

  #| end of module |# )

;;By default we use #f as the anti-mark.
;;
(define-constant THE-ANTI-MARK #f)

(define-syntax-rule (lexical-contour-mark? ?obj)
  (string? ?obj))

(define-syntax-rule (src-mark? ?obj)
  (eq? ?obj 'src))

(define-syntax-rule (anti-mark? ?obj)
  (not ?obj))

(define (mark? obj)
  (or (src-mark? obj)
      (lexical-contour-mark? obj)
      (anti-mark? obj)))

(define-list-of-type-predicate list-of-marks? mark?)

;;The body of a library, when it is first processed, gets this set of marks:
;;
;;* This for the "mark*" field of "stx" objects:
(define-constant SRC-MARK*	'(src))
;;
;;* This for the "mark**" field of "rib" objects:
(define-constant SRC-MARK**	'((src)))
;;
;;consequently, every  syntax object  that has a  "src" symbol in  its marks  set was
;;present in the program source.
;;
(define-syntax-rule (src-marked? mark*)
  (memq 'src mark*))


;;;; rib type definition

(define-record-type rib
  (nongenerative vicare:expander:rib)
  (fields
   (mutable name*	rib-name*	set-rib-name*!)
		;List  of symbols  representing  the original  binding  names in  the
		;source code.
		;
		;When the RIB is sealed: the list is converted to a vector.

   (mutable mark**	rib-mark**	set-rib-mark**!)
		;List of  sublists of marks;  there is a  sublist of marks  for every
		;item in NAME*.
		;
		;When the RIB is sealed: the list is converted to a vector.

   (mutable label*	rib-label*	set-rib-label*!)
		;List of  label gensyms uniquely identifying  the syntactic bindings;
		;there is a label for each item in NAME*.
		;
		;When the RIB is sealed: the list is converted to a vector.

   (mutable sealed/freq	rib-sealed/freq	set-rib-sealed/freq!)
		;False  or  vector  of  exact  integers.  When  false:  this  RIB  is
		;extensible, that is new bindings can be added to it.  When a vector:
		;this RIB is  sealed; see the documentation in Texinfo  format for an
		;explanation of the frequency vector.
   ))

(module ()
  ($record-type-printer-set! (record-type-descriptor rib)
    (lambda (S port subwriter) ;record printer function
      (define-syntax-rule (%display ?thing)
	(display ?thing port))
      (define-syntax-rule (%write ?thing)
	(write ?thing port))
      (define-syntax-rule (%pretty-print ?thing)
	(pretty-print* ?thing port 0 #f))
      (%display "#<rib")
      (%display " name*=")		(%display (rib-name*  S))
      ;; (%display " mark**=")		(subwriter (rib-mark** S))
      ;; (%display " label*=")		(subwriter (rib-label* S))
      ;; (%display " sealed/freq=")	(subwriter (rib-sealed/freq S))
      (%display ">"))))

;;; --------------------------------------------------------------------

(define (false-or-rib? obj)
  (or (not obj)
      (rib? obj)))

(define-list-of-type-predicate list-of-ribs? rib?)

(define (rib-or-shift? obj)
  (or (rib? obj)
      (eq? obj 'shift)))

(define-list-of-type-predicate list-of-ribs-and-shifts? rib-or-shift?)

;;; --------------------------------------------------------------------
;;; rib constructors

(define-syntax-rule (make-rib/empty)
  ;;Build and return a new, empty RIB object.
  ;;
  ;;Empty ribs  are used to  represent freshly created  lexical contours in  which no
  ;;initial  bindings  are  defined.   For example,  internal  bodies  might  contain
  ;;internal definitions:
  ;;
  ;;   (internal-body
  ;;     (define a 1)
  ;;     (define b 2)
  ;;     (display a))
  ;;
  ;;but we  know about  them only when  we begin their  expansion; when  creating the
  ;;lexical contour for them we must create an empty rib.
  ;;
  ;;If  STX* is  a  list of  syntax  objects representing  expressions  that must  be
  ;;expanded in a new lexical contour:
  ;;
  ;;   (define stx*  #'((define a 1)
  ;;                    (define b 2)
  ;;                    (display a)))
  ;;
  ;;we do:
  ;;
  ;;   (define rib (make-rib/empty))
  ;;   (define stx (push-lexical-contour rib stx*))
  ;;
  ;;and then hand the resulting syntax object STX to the appropriate "chi-*" function
  ;;to perform the expansion.  Later we can add bindings to the rib with:
  ;;
  ;;   (extend-rib! rib id label shadow/redefine-bindings?)
  ;;
  (make-rib '() '() '() #f))

(define (make-rib/from-identifiers-and-labels id* label*)
  ;;Build  and return  a  new RIB  record  initialised with  bindings  having ID*  as
  ;;original identifiers and LABEL* as associated label gensyms.
  ;;
  ;;The  argument ID*  must  be  a list  of  syntactic  identifiers representing  the
  ;;original binding  names.  The argument LABEL*  must be the list  of label gensyms
  ;;associated to the identifiers.
  ;;
  ;;For  example, when  creating a  rib to  represent the  lexical context  of a  LET
  ;;syntax:
  ;;
  ;;   (let ((?lhs* ?rhs*) ...) ?body* ...)
  ;;
  ;;we do:
  ;;
  ;;   (define lhs.id*    ?lhs*)
  ;;   (define body.stx   ?body*)
  ;;   (define lhs.label* (map generate-label-gensym lhs.id*))
  ;;   (define rib        (make-rib/from-identifiers-and-labels lhs.id* lhs.label*))
  ;;   (define body.stx^  (push-lexical-contour rib body.stx))
  ;;
  ;;and then  hand the resulting syntax  object BODY.STX^ to the  appropriate "chi-*"
  ;;function to perform the expansion.
  ;;
  (let ((name*        (map identifier->symbol id*))
	(mark**       (map stx-mark* id*))
	(sealed/freq  #f))
    (make-rib name* mark** label* sealed/freq)))

(define (make-rib/top-from-source-names-and-labels source-name* lab*)
  ;;Build and return a  new "rib" object to be used at the  top-level of a library or
  ;;program body.
  ;;
  ;;The argument SOURCE-NAME*  must be a list of symbols  representing names, without
  ;;duplicates,  of syntactic  bindings  as  they appear  in  the  source code.   The
  ;;argument LAB* must  be the list of label gensyms,  without duplicates, associated
  ;;to the SOURCE-NAME* bindings.
  ;;
  (let ((sealed/freq #f))
    (make-rib source-name* (map (lambda (dummy) SRC-MARK*) lab*) lab* sealed/freq)))

(define (make-rib/top-from-source-name-and-label source-name lab)
  ;;Build and return a  new "rib" object to be used at the  top-level of a library or
  ;;program body.
  ;;
  ;;The argument  SOURCE-NAME must be a  symbol representing the name  of a syntactic
  ;;binding as  it appears in the  source code.  The  argument LAB must be  the label
  ;;gensym associated to the SOURCE-NAME binding.
  ;;
  (let ((sealed/freq #f))
    (make-rib (list source-name) SRC-MARK** (list lab) sealed/freq)))

;;; --------------------------------------------------------------------

(define (export-subst->rib export-subst)
  ;;Build  and  return   a  new  RIB  structure  initialised  with   the  entries  of
  ;;EXPORT-SUBST, which is meant to come from a library object.
  ;;
  ;;An  EXPORT-SUBST  selects the  exported  bindings  among the  syntactic  bindings
  ;;defined at  the top-level of a  LIBRARY form; it is  an alist whose keys  are the
  ;;external symbol names  of the bindings and whose values  are the associated label
  ;;gensyms.
  ;;
  ;;Being defined at  the top-level: every name  is associated to the  "src" mark, so
  ;;its list of marks is SRC-MARK*.
  ;;
  (let loop ((nam.lab*  export-subst)
	     (name*     '())
	     (mark**    '())
	     (label*    '()))
    (if (pair? nam.lab*)
	(let ((nam.lab ($car nam.lab*)))
	  (loop ($cdr nam.lab*)
		(cons ($car nam.lab) name*)
		(cons SRC-MARK*      mark**)
		(cons ($cdr nam.lab) label*)))
      (let ((sealed/freq #f))
	(make-rib name* mark** label* sealed/freq)))))

;;; --------------------------------------------------------------------
;;; rib sealing

(define* (seal-rib! {rib rib?})
  (let ((name* (rib-name* rib)))
    (unless (null? name*) ;only seal if RIB is not empty
      (let ((name* (list->vector name*)))
	(set-rib-name*!       rib name*)
	(set-rib-mark**!      rib (list->vector (rib-mark** rib)))
	(set-rib-label*!      rib (list->vector (rib-label* rib)))
	(set-rib-sealed/freq! rib (make-vector (vector-length name*) 0))))))

(define* (unseal-rib! {rib rib?})
  (when (rib-sealed/freq rib)
    (set-rib-sealed/freq! rib #f)
    (set-rib-name*!       rib (vector->list (rib-name*  rib)))
    (set-rib-mark**!      rib (vector->list (rib-mark** rib)))
    (set-rib-label*!      rib (vector->list (rib-label* rib)))))

;;; --------------------------------------------------------------------

(module (extend-rib!)

  (define* (extend-rib! {rib rib?} {id identifier?} label shadow/redefine-bindings?)
    ;;Add  to  the  RIB  a  syntactic binding's  association  between  the  syntactic
    ;;identifier ID and the LABEL gensym.
    ;;
    ;;This  function  is used  only  while  expanding  a  body, either:  a  library's
    ;;top-level body; a  program's top-level body; an internal body  (uses of LET and
    ;;similar syntaxes,  uses of INTERNAL-BODY);  a top-level expression  expanded in
    ;;the context of an interaction environment.
    ;;
    ;;This function is used to add to a rib object the syntactic bindings established
    ;;by  the  syntaxes:  DEFINE, DEFINE-SYNTAX,  DEFINE-FLUID-SYNTAX,  DEFINE-ALIAS,
    ;;MODULE, IMPORT.
    ;;
    ;;This function  is called  to establish new  syntactic bindings  (either lexical
    ;;variables or macros), but  it is also called when an  IMPORT syntax is expanded
    ;;in an  internal body.   In the first  case, we  need to raise  an error  when a
    ;;syntactic binding is established twice:
    ;;
    ;;   (define a 1)
    ;;   (define a 2)
    ;;
    ;;in the second case, we need to allow:
    ;;
    ;;   (import (rnrs))
    ;;   (import (rnrs))
    ;;
    ;;because importing twice the same binding is fine.
    ;;
    ;;This function  is the only  place where the  argument SHADOW/REDEFINE-BINDINGS?
    ;;makes some difference:
    ;;
    ;;* When SHADOW/REDEFINE-BINDINGS?  is false: if a syntactic binding capturing ID
    ;;already exists  in RIB,  the call  to this function  is interpreted  as illegal
    ;;attempt  to:  redefine  a  previously   defined  syntactic  binding;  shadow  a
    ;;previously imported syntactic  binding.  It is responsibility  of this function
    ;;to raise an exception.
    ;;
    ;;* When SHADOW/REDEFINE-BINDINGS?  is true:  if a syntactic binding capturing ID
    ;;already exists  in RIB,  the new  syntactic binding is  allowed to  redefine or
    ;;shadow the existing  syntactic binding, by replacing the  original label gensym
    ;;with LABEL.
    ;;
    ;;See the  documentation of CHI-BODY*  for the  full description of  the argument
    ;;SHADOW/REDEFINE-BINDINGS?.
    ;;
    (when (rib-sealed/freq rib)
      (assertion-violation/internal-error __who__
	"attempt to extend sealed RIB" rib))
    (let ((id.source-name  ($identifier->symbol id))
	  (id.mark*        ($stx-mark*  id))
	  (rib.name*       (rib-name*  rib))
	  (rib.mark**      (rib-mark** rib))
	  (rib.label*      (rib-label* rib)))
      (cond ((and (memq id.source-name rib.name*)
		  (%find-syntactic-binding-with-same-name-and-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*))
	     => (lambda (tail-of-label*)
		  ;;If  we  are here:  we  have  found  in  RIB a  syntactic  binding
		  ;;capturing ID (same source-name, same marks).
		  (cond ((let ((label-in-rib (car tail-of-label*)))
			   (and (label-binding label)
				(eq? label label-in-rib)))
			 ;;This happens when importing twice the same binding, as in:
			 ;;
			 ;;   (import (rnrs))
			 ;;   (import (rnrs))
			 ;;
			 (void))
			(shadow/redefine-bindings?
			 ;;We replace  the old  label with  the new  one, in-so-doing
			 ;;redefining or shadowing the already existing binding.
			 (set-car! tail-of-label* label))
			(else
			 ;;Signal an error if the  identifier was already in the rib.
			 ;;This is the case of:
			 ;;
			 ;;   (define a 1)
			 ;;   (define a 2)
			 ;;
			 (syntax-violation __who__ "multiple definitions of identifier" id)))))
	    (else
	     ;;No capturing binding  exists for ID in RIB: let's  establish a new one
	     ;;by pushing the appropriate tuple on the rib.
	     (set-rib-name*!  rib (cons id.source-name  rib.name*))
	     (set-rib-mark**! rib (cons id.mark*        rib.mark**))
	     (set-rib-label*! rib (cons label           rib.label*))))))

  (define (%find-syntactic-binding-with-same-name-and-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*)
    ;;Here we  know that the  list of source-names RIB.NAME*  has one element  EQ? to
    ;;ID.SOURCE-NAME; we  do not know,  yet, if such  source-name is associated  to a
    ;;list of marks equal to ID.MARK* or not.   In other words: we do not know if the
    ;;rib holds a syntactic binding capturing ID.
    ;;
    ;;Iterate the tuples in RIB.NAME*, RIB.MARK**  and RIB.LABEL* looking for a tuple
    ;;having  name  equal  to  ID.SOURCE-NAME  and marks  equal  to  ID.MARK*.   When
    ;;successful: return the  tail sublist of RIB.LABEL* having  the associated label
    ;;as car.  If no capturing binding is found: return false.
    ;;
    (define-syntax-rule (%recurse rib.name* rib.mark** rib.label*)
      (%find-syntactic-binding-with-same-name-and-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*))
    (and (pair? rib.name*)
	 (if (and (same-name?  id.source-name ($car rib.name*))
		  (same-marks? id.mark*       ($car rib.mark**)))
	     ;;Capturing binding found!
	     rib.label*
	   (%recurse ($cdr rib.name*) ($cdr rib.mark**) ($cdr rib.label*)))))

  (define-syntax-rule (same-name? x y)
    (eq? x y))

  #| end of module: EXTEND-RIB! |# )

;;; --------------------------------------------------------------------

(define* (rib-src-marked-source-names {rib rib?})
  ;;Scan the  RIB and return a  list of symbols representing  syntactic binding names
  ;;and having only the "src" mark.
  ;;
  (define (%src-marks? obj)
    ;;Return true if OBJ is equal to SRC-MARK*.
    ;;
    (and (pair? obj)
	 (eq? 'src ($car obj))
	 (null? ($cdr obj))))
  (let ((name*  (rib-name*  rib))
	(mark** (rib-mark** rib)))
    (if (rib-sealed/freq rib)
	;;This rib is sealed: here NAME* and MARK** are vectors.
	(let recur ((i    0)
		    (imax ($vector-length name*)))
	  (let-syntax-rules (((%recursion)       (recur ($fxadd1 i) imax))
			     ((%more?)           ($fx< i imax))
			     ((%next-mark* arg)  ($vector-ref arg i))
			     ((%next-name arg)   ($vector-ref arg i)))
	    (if (%more?)
		(if (%src-marks? (%next-mark* mark**))
		    (cons (%next-name name*) (%recursion))
		  (%recursion))
	      '())))
      ;;This rib is not sealed: here NAME* and MARK** are lists.
      (let recur ((name*  name*)
		  (mark** mark**))
	(let-syntax-rules (((%recursion)       (recur ($cdr name*) ($cdr mark**)))
			   ((%more?)           (pair? name*))
			   ((%next-mark* arg)  ($car arg))
			   ((%next-name arg)   ($car arg)))
	  (if (%more?)
	      (if (%src-marks? (%next-mark* mark**))
		  (cons (%next-name name*) (%recursion))
		(%recursion))
	    '()))))))

;;; --------------------------------------------------------------------

;;So, what's an anti-mark and why is it there?
;;
;;The theory goes like  this: when a macro call is encountered, the  input stx to the
;;macro transformer gets an extra anti-mark, and the output of the transformer gets a
;;fresh  mark.  When  a mark  collides with  an anti-mark,  they cancel  one another.
;;Therefore, any part of  the input transformer that gets copied  to the output would
;;have a  mark followed  immediately by  an anti-mark, resulting  in the  same syntax
;;object (no extra  marks).  Parts of the  output that were not present  in the input
;;(e.g. inserted  by the macro transformer)  would have no anti-mark  and, therefore,
;;the mark would stick to them.
;;
;;Every time a mark is pushed to an STX-MARK* list, a corresponding "shift" symbol is
;;pushed to the STX-RIB*  list.  Every time a mark is cancelled  by an anti-mark, the
;;corresponding shifts are also cancelled.

;;The procedure  JOIN-WRAPS, here,  is used to  compute the new  MARK* and  RIB* that
;;would result when the m1* and s1* are added to an stx's MARK* and RIB*.
;;
;;The only tricky part  here is that e may have an anti-mark  that should cancel with
;;the last mark in m1*.  So, if:
;;
;;  m1* = (mx* ... mx)
;;  m2* = (#f my* ...)
;;
;;then the resulting marks should be:
;;
;;  (mx* ... my* ...)
;;
;;since mx  would cancel  with the  anti-mark.  The  ribs would  have to  also cancel
;;since:
;;
;;    s1* = (sx* ... sx)
;;    s2* = (sy sy* ...)
;;
;;then the resulting ribs should be:
;;
;;    (sx* ... sy* ...)
;;
;;Notice that both SX and SY would be shift marks.
;;
;;All this work is performed by the functions ADD-MARK and %DO-MACRO-CALL.
;;

(module WRAPS-UTILITIES
  (%merge-annotated-expr*
   %append-cancel-facing)

  (define (%merge-annotated-expr* ls1 ls2)
    ;;Append LS1 and LS2 and return the result; if the car or LS2 is #f:
    ;;append LS1 and (cdr LS2).
    ;;
    ;;   (%merge-annotated-expr* '(a b c) '(d  e f))   => (a b c d e f)
    ;;   (%merge-annotated-expr* '(a b c) '(#f e f))   => (a b c e f)
    ;;
    (if (and (pair? ls1)
	     (pair? ls2)
	     (not ($car ls2)))
	(%append-cancel-facing ls1 ls2)
      (append ls1 ls2)))

  (define (%append-cancel-facing ls1 ls2)
    ;;Expect LS1 to be a proper list  of one or more elements and LS2 to
    ;;be a proper list  of one or more elements.  Append  the cdr of LS2
    ;;to LS1 and return the result:
    ;;
    ;;   (%append-cancel-facing '(1 2 3) '(4 5 6))	=> (1 2 5 6)
    ;;   (%append-cancel-facing '(1)     '(2 3 4))	=> (3 4)
    ;;   (%append-cancel-facing '(1)     '(2))		=> ()
    ;;
    ;;This function is like:
    ;;
    ;;   (append ls1 (cdr ls2))
    ;;
    ;;we just hope to be a bit more efficient.
    ;;
    (let recur ((A1 ($car ls1))
		(D1 ($cdr ls1)))
      (if (null? D1)
	  ($cdr ls2)
	(cons A1 (recur ($car D1) ($cdr D1))))))

  #| end of module: WRAPS-UTILITIES |# )

(define (same-marks? x y)
  ;;Two lists  of marks are  considered the same  if they have  the same
  ;;length and the corresponding marks on each are EQ?.
  ;;
  (or (eq? x y)
      (and (pair? x) (pair? y)
	   (eq? ($car x) ($car y))
	   (same-marks? ($cdr x) ($cdr y)))))

(define* (join-wraps {stx1.mark* list-of-marks?} {stx1.rib* list-of-ribs-and-shifts?} stx1.ae {stx2 stx?})
  ;;Join the given wraps with the  ones in STX2 and return the resulting
  ;;wraps; with "wraps" we mean the marks and ribs, with the addition of
  ;;the annotated  expressions for debugging purposes.   The scenario is
  ;;this:
  ;;
  ;;* A syntax object STX1 (wrapped or partially unwrapped) contains the
  ;;  syntax object STX2 (an instance of stx) as subexpression.
  ;;
  ;;* Whenever STX1 is fully unwrapped (for example by SYNTAX-MATCH) its
  ;;   marks  and  ribs  must   be  propagated  to  all  its  identifier
  ;;  subexpressions.
  ;;
  ;;* In practice: the  marks of STX1 must be prepended  to the marks of
  ;;  STX2, the ribs of STX1 must be prepended to the ribs of STX2.
  ;;
  ;;this is what this function does.  But: we must also handle anti-mark
  ;;annihilation and the associated removal of shifts from the ribs.
  ;;
  (import WRAPS-UTILITIES)
  (let ((stx2.mark* ($stx-mark* stx2))
	(stx2.rib*  ($stx-rib*  stx2))
	(stx2.ae*   ($stx-annotated-expr*   stx2)))
    ;;If the first item in stx2.mark* is an anti-mark...
    (if (and (not (null? stx1.mark*))
	     (not (null? stx2.mark*))
	     (anti-mark? ($car stx2.mark*)))
	;;...cancel mark, anti-mark, and corresponding shifts.
	(values (%append-cancel-facing stx1.mark* stx2.mark*)
		(%append-cancel-facing stx1.rib*  stx2.rib*)
		(%merge-annotated-expr* stx1.ae stx2.ae*))
      ;;..else no cancellation takes place.
      (values (append stx1.mark* stx2.mark*)
	      (append stx1.rib*  stx2.rib*)
	      (%merge-annotated-expr* stx1.ae stx2.ae*)))))

(module ADD-MARK
  (add-new-mark
   add-anti-mark)
  (import WRAPS-UTILITIES)

  (define (add-anti-mark input-form-stx)
    ;;Push an anti-mark on the input form of a macro use.
    ;;
    (add-mark THE-ANTI-MARK #f input-form-stx #f))

  (define (add-new-mark rib output-form-expr input-form-stx)
    ;;Push a new mark on the output form of a macro use.
    ;;
    (add-mark (generate-new-mark) rib output-form-expr input-form-stx))

  (define* (add-mark mark {rib false-or-rib?} expr ae)
    ;;Build and return a new syntax object wrapping EXPR and having MARK
    ;;pushed on its list of marks.  This function used only in 2 places:
    ;;
    ;;* It  is applied to  the input form  of a macro  transformer, with
    ;;  MARK being the anti-mark.
    ;;
    ;;* It  is applied to the  output form of a  macro transformer, with
    ;;  MARK being a proper mark.
    ;;
    ;;MARK is either the anti-mark or a new mark.
    ;;
    ;;RIB can be #f (when MARK is the anti-mark) or an instance of rib
    ;;(when MARK is a new mark).
    ;;
    ;;EXPR is either  the input form of a macro  transformer call or the
    ;;output  form of  a macro  transformer call;  it must  be a  syntax
    ;;object, either wrapped or unwrapped.
    ;;
    ;;AE is either #f (when MARK is  the anti-mark) or the input form of
    ;;a macro call (when MARK is a  new mark).  This argument is used to
    ;;keep track of  the transformation a form  undergoes when processed
    ;;as macro use.
    ;;
    ;;The return value  can be either a stx instance  or a (partially)
    ;;unwrapped syntax object.
    ;;
    (%find-meaningful-stx expr mark rib expr '() (list ae)))

  ;;NOTE The one  below was the original (modified,  but still original)
  ;;implementation of this function; it  is kept here for reference.  In
  ;;my  eternal ignorance,  I do  not understand  why the  syntax object
  ;;return value of the recursive  function is enclosed in another stx
  ;;with empty wraps; so I removed  such an envelope object.  It appears
  ;;that there  is no need  for this function  to return an  instance of
  ;;stx: the  code works fine when  the return value is  a (partially)
  ;;unwrapped syntax object.  (Marco Maggi; Tue Mar 18, 2014)
  ;;
  ;; (define* ({add-mark stx?} mark {rib false-or-rib?} expr ae)
  ;;   (let ((mark* '())
  ;;         (rib*  '())
  ;;         (ae*   (list ae)))
  ;;     (mkstx (%find-meaningful-stx expr mark rib expr '() '())
  ;;            mark* rib* ae*)))

  (define (%find-meaningful-stx top-expr new-mark rib expr accum-rib* ae*)
    ;;Recursively visit EXPR while EXPR is: a pair, a vector or an stx
    ;;with empty MARK*.  Stop the recursion  when EXPR is: an stx with
    ;;non-empty MARK* or a  non-compound datum (boolean, number, string,
    ;;..., struct, record,  null).  Raise an exception if EXPR  is a raw
    ;;symbol.
    ;;
    ;;When a  stx with non-empty  MARK* is found: perform  the action;
    ;;see below for details.
    ;;
    ;;Return a wrapped  or (partially) unwrapped syntax  object with the
    ;;mark added.
    ;;
    ;;ACCUM-RIB* is  the list of  RIB* (ribs and "shift"  symbols), from
    ;;outer to  inner, collected so  far while visiting  stx instances
    ;;with empty MARK*.
    ;;
    (define-syntax-rule (%recurse ?expr ?accum-rib* ?ae*)
      (%find-meaningful-stx top-expr new-mark rib ?expr ?accum-rib* ?ae*))
    (cond ((pair? expr)
	   ;;Visit the  items in the  pair.  If the visited  items equal
	   ;;the original items: keep EXPR as result.
	   (let ((A (%recurse (car expr) accum-rib* ae*))
		 (D (%recurse (cdr expr) accum-rib* ae*)))
	     (if (eq? A D)
		 expr
	       (cons A D))))

	  ((vector? expr)
	   ;;Visit all  the items in  the vector.  If the  visited items
	   ;;equal the original items: keep EXPR as result.
	   (let* ((ls1 (vector->list expr))
		  (ls2 (map (lambda (item)
			      (%recurse item accum-rib* ae*))
			 ls1)))
	     (if (for-all eq? ls1 ls2)
		 expr
	       (list->vector ls2))))

	  ((stx? expr)
	   (let ((expr.mark* ($stx-mark* expr))
		 (expr.rib*  ($stx-rib*  expr)))
	     (cond ((null? expr.mark*)
		    ;;EXPR  with  empty  MARK*: collect  its  RIB*  then
		    ;;recurse into its expression.
		    (%recurse ($stx-expr expr)
			      (append accum-rib* expr.rib*)
			      (%merge-annotated-expr* ae* ($stx-annotated-expr* expr))))

		   ((anti-mark? (car expr.mark*))
		    ;;EXPR with non-empty MARK*  having the anti-mark as
		    ;;first mark; this means EXPR is the input form of a
		    ;;macro transformer call.
		    ;;
		    ;;Drop  both   NEW-MARK  and  the   anti-mark  (they
		    ;;annihilate each other) from the resulting MARK*.
		    ;;
		    ;;Join the collected  ACCUM-RIB* with the EXPR.RIB*;
		    ;;the first item in the resulting RIB* is associated
		    ;;to the  anti-mark (it is  a "shift" symbol)  so we
		    ;;drop it.
		    ;;
		    #;(assert (or (and (not (null? accum-rib*))
				     (eq? 'shift (car accum-rib*)))
				(and (not (null? expr.rib*))
				     (eq? 'shift (car expr.rib*)))))
		    (let* ((result.rib* (append accum-rib* expr.rib*))
			   (result.rib* (cdr result.rib*)))
		      (make-stx ($stx-expr expr) (cdr expr.mark*)
				  result.rib*
				  (%merge-annotated-expr* ae* ($stx-annotated-expr* expr)))))

		   (else
		    ;;EXPR with non-empty MARK*  having a proper mark as
		    ;;first  mark; this  means EXPR  is a  syntax object
		    ;;created by a macro transformer and inserted in its
		    ;;output form.
		    ;;
		    ;;Push NEW-MARK on the resulting MARK*.
		    ;;
		    ;;Join the  collected ACCUM-RIB* with  the EXPR.RIB*
		    ;;of  EXPR; push  a "shift"  on the  resulting RIB*,
		    ;;associated to NEW-MARK in MARK*.
		    ;;
		    (let* ((result.rib* (append accum-rib* expr.rib*))
			   (result.rib* (cons 'shift result.rib*))
			   (result.rib* (if rib
					    (cons rib result.rib*)
					  result.rib*)))
		      (make-stx ($stx-expr expr) (cons new-mark expr.mark*)
				  result.rib*
				  (%merge-annotated-expr* ae* ($stx-annotated-expr* expr))))))))

	  ((symbol? expr)
	   ;;A raw symbol is invalid.
	   (syntax-violation #f
	     "raw symbol encountered in output of macro"
	     top-expr expr))

	  (else
	   ;;If  we are  here EXPR  is a  non-compound datum  (booleans,
	   ;;numbers, strings, ..., structs, records).
	   #;(assert (non-compound-sexp? expr))
	   expr)))

  #| end of module: ADD-MARK |# )


;;;; syntax object type definition

(define-record-type stx
  (nongenerative vicare:expander:stx)
  (fields expr
		;A symbolic expression, possibly  annotated, whose subexpressions can
		;also be instances of stx.
	  mark*
		;Null or a proper list of marks, including the symbol "src".
	  rib*
		;Null or  a proper list of  rib instances or "shift"  symbols.  Every
		;rib represents  a nested lexical  contour; a "shift"  represents the
		;return from a macro transformer application.
		;
		;NOTE The items in the fields  MARK* and RIB* are not associated: the
		;two lists  can grow independently  of each other.   But, considering
		;the whole structure of nested stx  instances: the items in all the
		;MARK* fields  are associated to the  items in all the  RIB*, see the
		;functions JOIN-WRAPS and ADD-MARK for details.
	  annotated-expr*
		;List of annotated expressions: null or a proper list whose items are
		;#f or input  forms of macro transformer calls.  It  is used to trace
		;the transformations a  form undergoes when it is  processed as macro
		;use.
		;
		;The #f items  are inserted when this instance is  processed as input
		;form of a macro call, but is later discarded.
	  ))

(define (stx-record-printer S port subwriter)
  (define-syntax-rule (%display ?thing)
    (display ?thing port))
  (define-syntax-rule (%write ?thing)
    (write ?thing port))
  (define-syntax-rule (%pretty-print ?thing)
    (pretty-print* ?thing port 0 #f))
  (define raw-expr
    (syntax->datum S))
  (if (symbol? raw-expr)
      (%display "#<syntactic-identifier")
    (%display "#<syntax"))
  (%display " expr=")	(%write raw-expr)
  (%display " mark*=")	(%display (stx-mark* S))
  (let ((expr (stx-expr S)))
    (when (annotation? expr)
      (let ((pos (annotation-textual-position expr)))
	(when (source-position-condition? pos)
	  (%display " line=")	(%display (source-position-line    pos))
	  (%display " column=")	(%display (source-position-column  pos))
	  (%display " source=")	(%display (source-position-port-id pos))))))
  (%display ">"))

;;; --------------------------------------------------------------------

(define (syntax-object? obj)
  ;;Return #t if  OBJ is a wrapped  or unwrapped syntax object;  otherwise return #f.
  ;;This  is not  a full  validation, because  a component  stx may  contain a  raw
  ;;symbol.
  ;;
  (cond ((stx? obj)
	 obj)
	((pair? obj)
	 (cons (syntax-object? ($car obj)) (syntax-object? ($cdr obj))))
	((symbol? obj)
	 #f)
	((vector? obj)
	 (vector-map syntax-object? obj))
	(else
	 (non-compound-sexp? obj))))

;;; --------------------------------------------------------------------

(define* (datum->syntax {id identifier?} datum)
  (~datum->syntax id datum))

(define (~datum->syntax id datum)
  ;;Since all the identifier->label bindings  are encapsulated within the identifier,
  ;;converting  a datum  to  a syntax  object (non-hygienically)  is  done simply  by
  ;;creating an stx that has the same marks and ribs as the identifier.
  ;;
  ;;We include also  the annotated expression from ID because,  when showing an error
  ;;trace, it helps to understand from where the returned object comes.
  ;;
  (make-stx datum ($stx-mark* id) ($stx-rib* id) ($stx-annotated-expr* id)))

(define (syntax->datum S)
  (syntax-object-strip-annotations S '()))

(define (make-top-level-syntactic-identifier-from-source-name-and-label sym lab)
  (wrap-source-expression sym (make-rib/top-from-source-name-and-label sym lab)))

(define* (make-syntactic-identifier-for-temporary-variable {source-name symbol?})
  ;;Build and return a  new src marked syntactic identifier to  be used for temporary
  ;;variables.   It is  responsibility of  the caller  to select  a unique  symbol as
  ;;SOURCE-NAME: usually we should use GENSYM  for this.  The returned identifier can
  ;;be an item in the list generated by GENERATE-TEMPORARIES.
  ;;
  (make-stx source-name SRC-MARK* '() '()))

(define* (make-top-level-syntax-object/quoted-quoting sym)
  ;;Return a src-marked syntax object representing one among:
  ;;
  ;;   (quote quasiquote)
  ;;   (quote unquote)
  ;;   (quote unquote-splicing)
  ;;
  (case sym
    ((quasiquote unquote unquote-splicing)
     (let ((Q (core-prim-id 'quote)))
       (list Q (make-stx sym SRC-MARK* (stx-rib* Q) '()))))
    (else
     (syntax-violation __who__
       "invalid quoting syntax name, expected one among: quasiquote, unquote, unquote-splicing"
       sym))))

(define* (wrap-source-expression expr {rib rib?})
  ;;Wrap EXPR in a  syntax object giving it the source mark and  the RIB.  Return the
  ;;stx object.
  ;;
  ;;EXPR   must  be   a  symbolic   expression,   possibly  annotated   as  read   by
  ;;GET-ANNOTATED-DATUM.
  ;;
  (make-stx expr SRC-MARK* (list rib) '()))

;;; --------------------------------------------------------------------

(define* (mkstx expr-stx mark* {rib* list-of-ribs-and-shifts?} annotated-expr*)
  ;;This is the proper constructor for wrapped syntax objects.
  ;;
  ;;EXPR-STX can be a raw sexp, an  instance of STX or a (partially) unwrapped syntax
  ;;object.
  ;;
  ;;MARK* must be null or a proper list of marks, including the symbol "src".
  ;;
  ;;RIB* must be null or a proper list of rib instances and "shift" symbols.
  ;;
  ;;ANNOTATED-EXPR* must  be null or a  proper list of annotated  expressions: syntax
  ;;objects being input forms for macro transformer calls.
  ;;
  ;;When EXPR-STX is a raw sexp or  an unwrapped syntax object: just build and return
  ;;a new syntax object with the lexical context described by the given arguments.
  ;;
  ;;When EXPR-STX is a  stx instance: join the wraps from  EXPR-STX with given wraps,
  ;;making sure that marks and anti-marks and corresponding shifts cancel properly.
  ;;
  (if (and (stx? expr-stx)
	   (not (src-marked? mark*)))
      (receive (mark* rib* annotated-expr*)
	  (join-wraps mark* rib* annotated-expr* expr-stx)
	(make-stx (stx-expr expr-stx) mark* rib* annotated-expr*))
    (make-stx expr-stx mark* rib* annotated-expr*)))

(define* (push-lexical-contour {rib rib?} expr-stx)
  ;;Add  a rib  to a  syntax object  or expression  and return  the resulting  syntax
  ;;object.  During  the expansion process  we visit  the nested subexpressions  in a
  ;;syntax  object  repesenting source  code:  this  procedure introduces  a  lexical
  ;;contour in the context of EXPR-STX, for example when we enter a LET syntax.
  ;;
  ;;RIB must be an instance of RIB.
  ;;
  ;;EXPR-STX can be a raw sexp, an instance of STX or a wrapped syntax object.
  ;;
  ;;This function prepares a computation that will be lazily performed later; the RIB
  ;;will be pushed  on the stack of  ribs in every identifier in  the fully unwrapped
  ;;version of the returned syntax object.
  ;;
  (let ((mark*	'())
	(ae*	'()))
    (mkstx expr-stx mark* (list rib) ae*)))

(define (syntax-annotation x)
  (if (stx? x)
      (stx-expr x)
    x))

;;; --------------------------------------------------------------------

(module (syntax-object-strip-annotations)

  (define (syntax-object-strip-annotations expr mark*)
    ;;Remove  the wrap  of a  syntax object.   EXPR  and MARK*  are meant  to be  the
    ;;expression and associated marks of a  wrapped or unwrapped syntax object.  This
    ;;function is also the implementation of SYNTAX->DATUM.
    ;;
    ;;NOTE This function assumes that: if  MARK* contains the symbol "src", then EXPR
    ;;is a raw  symbolic expression or an annotated symbolic  expression; that is: it
    ;;is not a syntax object.
    ;;
    (if (src-marked? mark*)
	(if (or (annotation? expr)
		(and (pair? expr)
		     (annotation? ($car expr)))
		(and (vector? expr)
		     (not ($vector-empty? expr))
		     (annotation? ($vector-ref expr 0))))
	    ;;TODO Ask Kent why this is a sufficient test.  (Abdulaziz Ghuloum)
	    (%strip-annotations expr)
	  expr)
      (let f ((x expr))
	(cond ((stx? x)
	       (syntax-object-strip-annotations ($stx-expr x) ($stx-mark* x)))
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

  (define (%strip-annotations x)
    (cond ((pair? x)
	   (cons (%strip-annotations ($car x))
		 (%strip-annotations ($cdr x))))
	  ((vector? x)
	   (vector-map %strip-annotations x))
	  ((annotation? x)
	   (annotation-stripped x))
	  (else x)))

  #| end of module: SYNTAX-OBJECT-STRIP-ANNOTATIONS |# )


;;;; syntax objects: mapping identifiers to labels

(define (id->label/or-error who input-form.stx id)
  (or (id->label id)
      (raise-unbound-error who input-form.stx id)))

(module (id->label)

  (define* (id->label {id identifier?})
    ;;Given  the syntactic  identifier ID  search its  ribs for  a syntactic  binding
    ;;having the  same source-name  and marks.  If  successful: return  the syntactic
    ;;binding's label gensym; otherwise return false.
    ;;
    (define id.source-name ($identifier->symbol id))
    #;(debug-print __who__ id.source-name)
    (let search ((rib*  ($stx-rib* id))
		 (mark* ($stx-mark* id)))
      (and (pair? rib*)
	   (if (eq? ($car rib*) 'shift)
	       ;;This is the only  place in the expander where a  symbol "shift" in a
	       ;;RIB* makes some  difference; a "shift" is pushed on  the RIB* when a
	       ;;mark is pushed on the MARK*.
	       ;;
	       ;;When we  find a "shift" in  RIB*: we skip the  corresponding mark in
	       ;;MARK*.
	       (search ($cdr rib*) ($cdr mark*))
	     (let ((rib ($car rib*)))
	       (define (search-in-next-rib)
		 (search ($cdr rib*) mark*))
	       (if (rib-sealed/freq rib)
		   (%search-in-rib/sealed rib id.source-name mark* search-in-next-rib)
		 (%search-in-rib/non-sealed rib id.source-name mark* search-in-next-rib)))))))

  (define-syntax-rule (same-name? x y)
    (eq? x y))

  (define (%search-in-rib/non-sealed rib id.source-name id.mark* search-in-next-rib)
    (let loop ((rib.source-name* (rib-name*  rib))
	       (rib.mark**       (rib-mark** rib))
	       (rib.label*       (rib-label* rib)))
      (let-syntax-rules (((more-tuples?)		(pair? rib.source-name*))
			 ((loop-to-next-rib-tuple)	(loop ($cdr rib.source-name*) ($cdr rib.mark**) ($cdr rib.label*)))
			 ((next-rib-source-name)	($car rib.source-name*))
			 ((next-rib-mark*)		($car rib.mark**))
			 ((next-rib-label)		($car rib.label*)))
	(if (more-tuples?)
	    (if (and (same-name?  id.source-name (next-rib-source-name))
		     (same-marks? id.mark*       (next-rib-mark*)))
		(next-rib-label)
	      (loop-to-next-rib-tuple))
	  (search-in-next-rib)))))

  (define (%search-in-rib/sealed rib id.source-name id.mark* search-in-next-rib)
    (define rib.source-name* (rib-name*  rib))
    (define rib.mark**       (rib-mark** rib))
    (define rib.label*       (rib-label* rib))
    (let loop ((i    0)
	       (imax ($vector-length rib.source-name*)))
      (let-syntax-rules (((more-tuples?)		($fx< i imax))
			 ((loop-to-next-rib-tuple)	(loop ($fxadd1 i) imax))
			 ((next-rib-source-name)	($vector-ref rib.source-name* i))
			 ((next-rib-mark*)		($vector-ref rib.mark**       i))
			 ((next-rib-label)		(receive-and-return (label)
							    ($vector-ref rib.label* i)
							  (%increment-rib-frequency! rib i))))
	(if (more-tuples?)
	    (if (and (same-name?  id.source-name (next-rib-source-name))
		     (same-marks? id.mark*       (next-rib-mark*)))
		(next-rib-label)
	      (loop-to-next-rib-tuple))
	  (search-in-next-rib)))))

  (define (%increment-rib-frequency! rib src.idx)
    ;;The argument RIB is a sealed rib  object.  The argument SRC.IDX is the index of
    ;;the tuple in RIB that was last accessed.
    ;;
    ;;If the scenario before the access is:
    ;;
    ;;   name*       = #(b a)
    ;;   sealed/freq = #(0 0)
    ;;
    ;;and we access A, we want the scenario after the access to be:
    ;;
    ;;   name*       = #(a b)
    ;;   sealed/freq = #(1 0)
    ;;
    ;;so the tuple of A is moved to the left and its access frequency is incremented.
    ;;
    ;;If the scenario before the access is:
    ;;
    ;;   name*       = #(f e d c b a)
    ;;   sealed/freq = #(1 1 1 0 0 0)
    ;;
    ;;and we  access A:  first we want  the tuple of  A to  be moved in  the leftmost
    ;;position in the group of tuples with FREQ equal to 0:
    ;;
    ;;   name*       = #(f e d a b c)
    ;;   sealed/freq = #(1 1 1 0 0 0)
    ;;
    ;;then we increment its access frequency; the scenario after the access must be:
    ;;
    ;;   name*       = #(f e d a b c)
    ;;   sealed/freq = #(1 1 1 1 0 0)
    ;;
    ;;notice that:  while we  need to  swap the  tuple values  in the  vectors NAME*,
    ;;MARK** and LABEL*, we  just need to increment the freq  in the destination slot
    ;;of A: there is no swapping needed in the freq vector.
    ;;
    (unless ($fxzero? src.idx)
      (let* ((rib.freq* (rib-sealed/freq rib))
	     (freq      ($vector-ref rib.freq* src.idx))
	     ;;Search for the leftmost slot, starting from SRC.IDX, that has the same
	     ;;freq of the slot at SRC.IDX.
	     (dst.idx   (let loop ((i src.idx))
			  (if ($fxzero? i)
			      0
			    (let ((j ($fxsub1 i)))
			      (if ($fx= freq ($vector-ref rib.freq* j))
				  ;;The  freq of  the slot  previous to  I is  equal:
				  ;;loop.
				  (loop j)
				;;The  freq of  the slot  previous to  I is  greater:
				;;accept I as swap position.
				i))))))
	;;Rather than swapping the slots DST.IDX and SRC.IDX in the frequency vector:
	;;we just increment the dst slot.
	($vector-set! rib.freq* dst.idx ($fxadd1 freq))
	(unless ($fx= dst.idx src.idx)
	  (let ((rib.name*  (rib-name*  rib))
		(rib.mark** (rib-mark** rib))
		(rib.label* (rib-label* rib)))
	    (let-syntax ((%vector-swap (syntax-rules ()
					 ((_ ?vec ?idx1 ?idx2)
					  (let ((V ($vector-ref ?vec ?idx1)))
					    ($vector-set! ?vec ?idx1 ($vector-ref ?vec ?idx2))
					    ($vector-set! ?vec ?idx2 V))))))
	      (%vector-swap rib.name*  src.idx dst.idx)
	      (%vector-swap rib.mark** src.idx dst.idx)
	      (%vector-swap rib.label* src.idx dst.idx)))))))

  #| end of module: ID->LABEL |# )


;;;; syntax objects: mapping identifiers to values

(define (id->object-type-specification who input-form.stx id lexenv)
  ;;ID is meant to be a  syntactic identifier representing an object-type name, whose
  ;;syntactic  binding's descriptor  contains  an  instance of  "<object-type-spec>";
  ;;retrieve its label, then its descriptor  from LEXENV, finally return the instance
  ;;of "<object-type-spec>".
  ;;
  ;;If  ID is  bound  to an  imported  syntactic binding:  the  exporting library  is
  ;;visited.
  ;;
  ;;If ID  is unbound:  raise an  "unbound identifier"  exception.  If  the syntactic
  ;;binding's  descriptor does  not represent  an  object-type name:  raise a  syntax
  ;;violation exception.
  ;;
  (define (%error-wrong-descriptor message descr)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-syntax-violation input-form.stx id)
		(make-syntactic-binding-descriptor-condition descr))))
  (unless (identifier? id)
    (syntax-violation who
      "expected identifier as object-type name" input-form.stx id))
  (case-identifier-syntactic-binding-descriptor (who input-form.stx id lexenv)
    ((local-object-type-name)
     (syntactic-binding-descriptor/local-object-type.object-type-spec  __descr__))
    ((global-object-type-name)
     (syntactic-binding-descriptor/global-object-type.object-type-spec __descr__))
    ((core-object-type-name)
     (syntactic-binding-descriptor/core-object-type.object-type-spec __descr__))
    (else
     (%error-wrong-descriptor "identifier not bound to an object-type specification" __descr__))))

(define (id->record-type-specification who input-form.stx id lexenv)
  ;;ID is meant  to be a syntactic identifier representing  a record-type name, whose
  ;;syntactic  binding's descriptor  contains  an  instance of  "<record-type-spec>";
  ;;retrieve its label,  then its binding descriptor from LEXENV,  finally return the
  ;;instance of "<record-type-spec>".
  ;;
  ;;If  ID is  bound  to an  imported  syntactic binding:  the  exporting library  is
  ;;visited.
  ;;
  ;;If ID  is unbound:  raise an  "unbound identifier"  exception.  If  the syntactic
  ;;binding's  descriptor does  not  represent  a record-type  name:  raise a  syntax
  ;;violation exception.
  ;;
  (define (%error-wrong-descriptor message descr)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-syntax-violation input-form.stx id)
		(make-syntactic-binding-descriptor-condition descr))))
  (define (%error-wrong-type-identifier descr)
    (%error-wrong-descriptor "the given type identifier is not bound to a record-type specification" descr))
  (unless (identifier? id)
    (syntax-violation who
      "expected identifier as record-type name" input-form.stx id))
  (case-identifier-syntactic-binding-descriptor (who input-form.stx id lexenv)
    ((local-object-type-name)
     (receive-and-return (ots)
	 (syntactic-binding-descriptor/local-object-type.object-type-spec  __descr__)
       (unless (record-type-spec? ots)
	 (%error-wrong-type-identifier __descr__))))
    ((global-object-type-name)
     (receive-and-return (ots)
	 (syntactic-binding-descriptor/global-object-type.object-type-spec __descr__)
       (unless (record-type-spec? ots)
	 (%error-wrong-type-identifier __descr__))))
    ((core-object-type-name)
     (receive-and-return (ots)
	 (syntactic-binding-descriptor/core-object-type.object-type-spec __descr__)
       (unless (record-type-spec? ots)
	 (%error-wrong-type-identifier __descr__))))
    (else
     (%error-wrong-descriptor "identifier not bound to an object-type specification" __descr__))))

(define (id->struct-type-specification who input-form.stx id lexenv)
  ;;ID is meant  to be a syntactic identifier representing  a struct-type name, whose
  ;;syntactic  binding's descriptor  contains  an  instance of  "<struct-type-spec>";
  ;;retrieve its label,  then its binding descriptor from LEXENV,  finally return the
  ;;instance of "<struct-type-name>".
  ;;
  ;;If  ID is  bound  to an  imported  syntactic binding:  the  exporting library  is
  ;;visited.
  ;;
  ;;If ID  is unbound:  raise an  "unbound identifier"  exception.  If  the syntactic
  ;;binding's  descriptor does  not represent  an  struct-type name:  raise a  syntax
  ;;violation exception.
  ;;
  (define (%error-wrong-descriptor message descr)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-syntax-violation input-form.stx id)
		(make-syntactic-binding-descriptor-condition descr))))
  (define (%error-wrong-type-identifier descr)
    (%error-wrong-descriptor "the given type identifier is not bound to a struct-type specification" descr))
  (unless (identifier? id)
    (syntax-violation who
      "expected identifier as struct-type name" input-form.stx id))
  (case-identifier-syntactic-binding-descriptor (who input-form.stx id lexenv)
    ((local-object-type-name)
     (receive-and-return (ots)
	 (syntactic-binding-descriptor/local-object-type.object-type-spec  __descr__)
       (unless (struct-type-spec? ots)
	 (%error-wrong-type-identifier __descr__))))
    ((global-object-type-name)
     (receive-and-return (ots)
	 (syntactic-binding-descriptor/global-object-type.object-type-spec __descr__)
       (unless (struct-type-spec? ots)
	 (%error-wrong-type-identifier __descr__))))
    ((core-object-type-name)
     (receive-and-return (ots)
	 (syntactic-binding-descriptor/core-object-type.object-type-spec __descr__)
       (unless (struct-type-spec? ots)
	 (%error-wrong-type-identifier __descr__))))
    (else
     (%error-wrong-descriptor "identifier not bound to an object-type specification" __descr__))))

(define* (id->typed-variable-spec who input-form.stx id lexenv)
  ;;ID is  meant to be a  syntactic identifier representing a  typed variable, either
  ;;lexical or global,  whose syntactic binding's descriptor contains  an instance of
  ;;"<typed-variable-spec>";  retrieve its  label, then  its binding  descriptor from
  ;;LEXENV, finally return the instance of "<typed-variable-spec>".
  ;;
  ;;If  ID is  bound  to an  imported  syntactic binding:  the  exporting library  is
  ;;visited.
  ;;
  ;;If ID  is unbound:  raise an  "unbound identifier"  exception.  If  the syntactic
  ;;binding's  descriptor  does  not  represent  a typed  variable:  raise  a  syntax
  ;;violation exception.
  ;;
  (define (%error-wrong-descriptor message descr)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-syntax-violation input-form.stx id)
		(make-syntactic-binding-descriptor-condition descr))))
  (define (%error-wrong-type-identifier descr)
    (%error-wrong-descriptor "the identifier is not bound to a typed variable" descr))
  (unless (identifier? id)
    (syntax-violation who
      "expected identifier as typed variable name" input-form.stx id))
  (case-identifier-syntactic-binding-descriptor (who input-form.stx id lexenv)
    ((lexical-typed)
     ;;We expect the descriptor to have the format:
     ;;
     ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
     ;;
     (car (syntactic-binding-descriptor.value __descr__)))
    ((global-typed global-typed-mutable)
     ;;We expect the descriptor to have the format:
     ;;
     ;;   (global-typed         . (#<library> . ?loc))
     ;;   (global-typed-mutable . (#<library> . ?loc))
     ;;
     ;;where ?LOC  is a loc gensym  containing in its  VALUE slots a reference  to an
     ;;instance of "<global-typed-variable-spec>".
     (let ((tvs (symbol-value (cdr (syntactic-binding-descriptor.value __descr__)))))
       (if (global-typed-variable-spec? tvs)
	   tvs
	 (assertion-violation who
	   "invalid object in \"value\" slot of loc gensym for global typed variable"
	   id __descr__ tvs))))
    (else
     (%error-wrong-descriptor "identifier not bound to an object-type specification" __descr__))))


;;;; identifier to syntactic binding's descriptor

(define-fluid-syntax __descr__
  (lambda (stx)
    (sys.syntax-violation '__descr__ "unset fluid syntax" stx)))

(module (case-identifier-syntactic-binding-descriptor case-identifier-syntactic-binding-descriptor/no-indirection)

  (define-syntax case-identifier-syntactic-binding-descriptor
    (syntax-rules ()
      ((_ (?who ?input-form.stx ?id ?lexenv)
	  ((?type0 ?type ...) . ?body)
	  ...
	  (else . ?else-body))
       (%case-identifier-syntactic-binding-descriptor
	label->syntactic-binding-descriptor
	(?who ?input-form.stx ?id ?lexenv)
	((?type0 ?type ...) . ?body)
	...
	(else . ?else-body)))
      ))

  (define-syntax case-identifier-syntactic-binding-descriptor/no-indirection
    (syntax-rules ()
      ((_ (?who ?input-form.stx ?id ?lexenv)
	  ((?type0 ?type ...) . ?body)
	  ...
	  (else . ?else-body))
       (%case-identifier-syntactic-binding-descriptor
	label->syntactic-binding-descriptor/no-indirection
	(?who ?input-form.stx ?id ?lexenv)
	((?type0 ?type ...) . ?body)
	...
	(else . ?else-body)))
      ))

  (define-syntax %case-identifier-syntactic-binding-descriptor
    (lambda (stx)
      (define (%id-or-false X)
	(or (sys.identifier? X)
	    (not (sys.syntax->datum X))))
      (sys.syntax-case stx (else)
	((_ ?label->descr
	    (?who ?input-form.stx ?id ?lexenv)
	    ((?type0 ?type ...) . ?body)
	    ...
	    (else . ?else-body))
	 (and (%id-or-false (sys.syntax ?who))
	      (%id-or-false (sys.syntax ?input-form.stx))
	      (sys.identifier? (sys.syntax ?lexenv))
	      (sys.identifier? (sys.syntax ?id)))
	 (sys.syntax
	  (let* ((label (id->label/or-error ?who ?input-form.stx ?id))
		 (descr (?label->descr label ?lexenv)))
	    (fluid-let-syntax ((__descr__ (identifier-syntax descr)))
	      (case (syntactic-binding-descriptor.type descr)
		((displaced-lexical)
		 (syntax-violation ?who "identifier out of context (identifier's label not in LEXENV)" ?input-form.stx ?id))
		((?type0 ?type ...) . ?body)
		...
		(else . ?else-body))))))
	)))

  #| end of module |# )


;;;; system label gensym

(define-constant SYSTEM-LABEL-GENSYM
  ;;Notice  that  this  gensym is  generated  a-new  every  time  the boot  image  is
  ;;initialised.  We  must avoid the source  optimizer to precompute and  hard-code a
  ;;value.
  ;;
  ;;This syntactic binding is  not part of the public API: it is  not exported by the
  ;;library  "(vicare)".  However,  it  is  used by  the  init  library generated  by
  ;;"makefile.sps" when building a new boot image.
  (expand-time-gensym "system-label-gensym"))

(define (system-label-gensym)
  SYSTEM-LABEL-GENSYM)

(define* (system-label {x symbol?})
  ;;If X is  the symbol name of  a primitive procedure: return  its syntactic binding
  ;;label gensym, otherwise return false.
  ;;
  (getprop x SYSTEM-LABEL-GENSYM))


;;;; system id gensym

(module (system-id-gensym system-id)

  (define-constant SYSTEM-ID-GENSYM
    ;;Notice  that this  gensym  is generated  a-new  every time  the  boot image  is
    ;;initialised.  We must avoid the source  optimizer to precompute and hard-code a
    ;;value.
    (expand-time-gensym "system-id-gensym"))

  (define (system-id-gensym)
    SYSTEM-ID-GENSYM)

  (case-define* system-id
    (({x symbol?})
     ;;If X is the symbol name of a primitive procedure: return its syntactic binding
     ;;identifier, otherwise return false.
     ;;
     (getprop x SYSTEM-ID-GENSYM))
    (({x symbol?} property)
     (putprop x SYSTEM-ID-GENSYM property)))

  #| end of module |# )


;;;; identifiers from the built-in environment

(define (bless input-form.stx)
  ;;Given a raw sexp,  a single syntax object, a wrapped  syntax object, an unwrapped
  ;;syntax  object or  a partly  unwrapped syntax  object X:  return a  syntax object
  ;;representing the input, possibly X itself.
  ;;
  ;;When  INPUT-FORM.STX is  a sexp  or a  (partially) unwrapped  syntax object:  raw
  ;;symbols in INPUT-FORM.STX are converted to:
  ;;
  ;;* Bound identifiers  that will be captured  by a core primitive  binding from the
  ;;  top-level image.
  ;;
  ;;* Free identifiers that will not be captured by any binding.  These can be safely
  ;;  used for local bindings.
  ;;
  (cond ((stx? input-form.stx)
	 input-form.stx)
	((pair? input-form.stx)
	 (cons (bless ($car input-form.stx))
	       (bless ($cdr input-form.stx))))
	((symbol? input-form.stx)
	 (scheme-stx input-form.stx))
	((vector? input-form.stx)
	 (vector-map bless input-form.stx))
	(else
	 ;;If we  are here INPUT-FORM.STX  is a non-compound datum  (boolean, number,
	 ;;string, ..., struct, record, null).
	 #;(assert (non-compound-sexp? input-form.stx))
	 input-form.stx)))

(define (trace-bless input-form.stx)
  (receive-and-return (output-stx)
      (bless input-form.stx)
    (debug-print 'bless-input  (syntax->datum input-form.stx)
		 'bless-output (syntax->datum output-stx))))

;;; --------------------------------------------------------------------

(define* (scheme-stx {sym symbol?})
  ;;Take a symbol and if it's the public  name of a syntactic binding exported by the
  ;;library "(psyntax system  $all)": create a fresh identifier that  maps the symbol
  ;;to its label in that library.  Symbols not in that library become fresh.
  ;;
  (or (system-id sym)
      (getprop sym '*vicare-scheme-temporary-variable-id*)
      (cond ((system-label sym)
	     ;;SYM is the  name of a core  primitive, so we build  a bound identifier
	     ;;with a proper "rib" and  the binding's label.  Such bound identifier
	     ;;will be captured by the entry in the top-level environment.
	     => (lambda (label)
		  (receive-and-return (id)
		      (make-top-level-syntactic-identifier-from-source-name-and-label sym label)
		    (system-id sym id))))
	    (else
	     ;;SYM is  not the  name of  a core primitive,  so we  just build  a free
	     ;;identifier.   Such free  identifier  will work  just  fine in  binding
	     ;;position.
	     (receive-and-return (stx)
		 (make-stx sym SRC-MARK* '() '())
	       (putprop sym '*vicare-scheme-temporary-variable-id* stx))))))

;;; --------------------------------------------------------------------

(define* (core-prim-id {sym symbol?})
  ;;Take a symbol  and if it's in the library:
  ;;
  ;;   (psyntax system $all)
  ;;
  ;;create a fresh identifier that maps only the symbol to its label in that library.
  ;;This function is similar to SCHEME-STX,  but it does not create fresh identifiers
  ;;for non-core-primitive symbols.
  ;;
  (or (system-id sym)
      (cond ((system-label sym)
	     ;;SYM is the  name of a core  primitive, so we build  a bound identifier
	     ;;with a  proper "rib" and  the binding's label.  Such  bound identifier
	     ;;will be captured by the entry in the top-level environment.
	     => (lambda (label)
		  (receive-and-return (id)
		      (make-top-level-syntactic-identifier-from-source-name-and-label sym label)
		    (system-id sym id))))
	    (else
	     (assertion-violation __who__ "invalid core primitive symbol name" sym)))))

(define* (make-syntactic-identifier-for-fake-core-primitive {source-name symbol?})
  (make-stx source-name '() '() '()))

(let-syntax
    ((define-core-prim-id-retriever (syntax-rules ()
				      ((_ ?who ?core-prim)
				       (define ?who
					 (let ((memoized-id #f))
					   (lambda ()
					     (or memoized-id
						 (receive-and-return (id)
						     (core-prim-id '?core-prim)
						   (set! memoized-id id))))))))))
  (define-core-prim-id-retriever underscore-id		_)
  (define-core-prim-id-retriever ellipsis-id		...)
  (define-core-prim-id-retriever place-holder-id	<>)
  (define-core-prim-id-retriever procedure-pred-id	procedure?)
  (define-core-prim-id-retriever method-id		method)
  (define-core-prim-id-retriever case-method-id		case-method)
  (define-core-prim-id-retriever brace-id		brace)
  #| end of let-syntax |# )

(define (underscore-id? id)
  (and (identifier? id)
       (~free-identifier=? id (underscore-id))))

(define (ellipsis-id? id)
  (and (identifier? id)
       (~free-identifier=? id (ellipsis-id))))

(define (place-holder-id? id)
  (and (identifier? id)
       (~free-identifier=? id (place-holder-id))))

(define (method-id? id)
  (and (identifier? id)
       (~free-identifier=? id (method-id))))

(define (case-method-id? id)
  (and (identifier? id)
       (~free-identifier=? id (case-method-id))))

(define (brace-id? id)
  (and (identifier? id)
       (~free-identifier=? id (brace-id))))


;;;; public interface: identifiers handling

(define (identifier? x)
  ;;Return true if X is an  identifier: a syntax object whose expression
  ;;is a symbol.
  ;;
  (and (stx? x)
       (let ((expr ($stx-expr x)))
	 (symbol? (if (annotation? expr)
		      (annotation-stripped expr)
		    expr)))))

(define (false-or-identifier? x)
  (or (not x)
      (identifier? x)))

;;; --------------------------------------------------------------------

(define* (bound-identifier=? {x identifier?} {y identifier?})
  (~bound-identifier=? x y))

(define (~bound-identifier=? id1 id2)
  ;;Two identifiers  are ~BOUND-IDENTIFIER=? if they  have the same name  and the
  ;;same set of marks.
  ;;
  (and (eq? ($identifier->symbol id1) ($identifier->symbol id2))
       (same-marks? ($stx-mark* id1) ($stx-mark* id2))))

(define* (free-identifier=? {x identifier?} {y identifier?})
  (~free-identifier=? x y))

(define (~free-identifier=? id1 id2)
  ;;Two identifiers are ~FREE-IDENTIFIER=? if either both are bound to the same label
  ;;or if both are unbound and they have the same name.
  ;;
  (let ((t1 (id->label id1))
	(t2 (id->label id2)))
    (if (or t1 t2)
	(eq? t1 t2)
      (eq? ($identifier->symbol id1)
	   ($identifier->symbol id2)))))

;;; --------------------------------------------------------------------

(define* (identifier-bound? {id identifier?})
  (~identifier-bound? id))

(define (~identifier-bound? id)
  (and (id->label id) #t))

(define (false-or-identifier-bound? id)
  (or (not id)
      (and (identifier? id)
	   (~identifier-bound? id))))

;;; --------------------------------------------------------------------

(define* (identifier->symbol {x identifier?})
  ;;Given an identifier return its symbol expression.
  ;;
  ($identifier->symbol x))

(define ($identifier->symbol x)
  (let ((expr ($stx-expr x)))
    (if (annotation? expr)
	(annotation-stripped expr)
      expr)))


;;;; current inferior lexenv
;;
;;When calling a macro transformer procedure: we set the paramerer CURRENT-RUN-LEXENV
;;to a function  returning the current inferior LEXENV.  This  allows us to implement
;;syntax parameters and compile-time value retrieving.
;;

(define current-run-lexenv
  ;;This parameter holds a function which is  meant to return the value of LEXENV.RUN
  ;;while a macro is being expanded.
  ;;
  ;;The default value  will return null, which represents an  empty LEXENV; when such
  ;;value is used with LABEL->SYNTACTIC-BINDING-DESCRIPTOR: the mapping label/binding
  ;;is performed only in the top-level environment.
  ;;
  ;;Another possibility we could think of is to use as default value the function:
  ;;
  ;;   (lambda ()
  ;;     (syntax-violation 'current-run-lexenv
  ;; 	   "called outside the extent of a macro expansion"
  ;; 	   '(current-run-lexenv)))
  ;;
  ;;However there are  cases where we actually  want the returned LEXENV  to be null,
  ;;for example:  when evaluating  the visit code  of a library  just loaded  in FASL
  ;;form; such  visit code might need,  for example, to access  the syntactic binding
  ;;property lists, and it would do it outside any macro expansion.
  ;;
  (make-parameter
      (lambda () '())
    (lambda* ({obj procedure?})
      obj)))

(define (current-inferior-lexenv)
  ((current-run-lexenv)))


;;;; identifiers: syntax parameters

(define* (syntax-parameter-value {id identifier?})
  (define lexenv
    (current-inferior-lexenv))
  (case-identifier-syntactic-binding-descriptor (__who__ #f id lexenv)
    ((local-etv)
     (local-expand-time-value-binding-descriptor.object __descr__))

    ((global-etv)
     (global-expand-time-value-binding-descriptor.object __descr__))

    (else
     (procedure-argument-violation __who__
       "expected identifier bound to compile-time value" id))))


;;;; utilities for identifiers

(define (valid-bound-ids? id*)
  ;;Given a list return #t if it  is made of identifers none of which is
  ;;~BOUND-IDENTIFIER=? to another; else return #f.
  ;;
  ;;This function is called to validate  both list of LAMBDA formals and
  ;;list of LET binding identifiers.  The only guarantee about the input
  ;;is that it is a list.
  ;;
  (and (for-all identifier? id*)
       (distinct-bound-ids? id*)))

(define (distinct-bound-ids? id*)
  ;;Given a list of identifiers: return #t if none of the identifiers is
  ;;~BOUND-IDENTIFIER=? to another; else return #f.
  ;;
  (or (null? id*)
      (and (not (bound-id-member? ($car id*) ($cdr id*)))
	   (distinct-bound-ids? ($cdr id*)))))

(define (duplicate-bound-formals? standard-formals-stx)
  ;;Given  a  syntax  object  representing a  list  of  UNtagged  LAMBDA
  ;;formals:  return #f  if none  of the  identifiers is  ~BOUND-IDENTIFIER=?  to
  ;;another; else return a duplicate identifier.
  ;;
  (let recur ((fmls          standard-formals-stx)
	      (collected-id* '()))
    (syntax-case fmls ()
      ;;STANDARD-FORMALS-STX is a proper list and it ends here.  Good.
      (() #f)
      ;;STANDARD-FORMALS-STX is an IMproper list and it ends here with a
      ;;rest argument.  Check it.
      (?rest
       (identifier? #'?rest)
       (if (bound-id-member? #'?rest collected-id*)
	   #'?rest
	 #f))
      ((?id . ?rest)
       (identifier? #'?id)
       (if (bound-id-member? #'?id collected-id*)
	   #'?id
	 (recur #'?rest (cons #'?id collected-id*))))
      (_
       (syntax-violation #f "invalid formals" standard-formals-stx)))))

(define (free-id-member? id id*)
  ;;Given  an identifier  ID  and a  list  of identifiers  ID*: return  #t  if ID  is
  ;;~BOUND-IDENTIFIER=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (~free-identifier=? id (car id*))
	   (free-id-member?    id (cdr id*)))))

(define (bound-id-member? id id*)
  ;;Given  an identifier  ID  and a  list  of identifiers  ID*: return  #t  if ID  is
  ;;~BOUND-IDENTIFIER=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (~bound-identifier=? id (car id*))
	   (bound-id-member?    id (cdr id*)))))

(define* (identifier-append {ctxt identifier?} . str*)
  ;;Given  the identifier  CTXT  and a  list of  strings  or symbols  or
  ;;identifiers STR*: concatenate all the items in STR*, with the result
  ;;build and return a new identifier in the same context of CTXT.
  ;;
  (~datum->syntax ctxt
		  (string->symbol
		   (apply string-append
			  (map (lambda (x)
				 (cond ((symbol? x)
					(symbol->string x))
				       ((string? x)
					x)
				       ((identifier? x)
					(symbol->string (syntax->datum x)))
				       (else
					(assertion-violation __who__ "BUG" ctxt str*))))
			    str*)))))


;;;; basic object-type identifiers

(let-syntax
    ((define-tag-retriever (syntax-rules ()
			     ((_ ?who ?tag)
			      (define ?who
				(let ((memoized-id #f))
				  (lambda ()
				    (or memoized-id
					(receive-and-return (id)
					    (core-prim-id '?tag)
					  (set! memoized-id id))))))))))
  (define-tag-retriever top-tag-id		<top>)
  (define-tag-retriever void-tag-id		<void>)
  (define-tag-retriever procedure-tag-id	<procedure>)
  (define-tag-retriever predicate-tag-id	<predicate>)
  (define-tag-retriever boolean-tag-id		<boolean>)
  (define-tag-retriever struct-tag-id		<struct>)
  (define-tag-retriever record-tag-id		<record>)
  (define-tag-retriever vector-tag-id		<vector>)
  (define-tag-retriever list-tag-id		<list>)
  (define-tag-retriever null-tag-id		<null>)
  (define-tag-retriever nlist-tag-id		<nlist>)
  #| end of let-syntax |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((define-unsafe-tag-predicate (syntax-rules ()
				    ((_ ?who ?tag-retriever)
				     (define (?who id)
				       (~free-identifier=? id (?tag-retriever)))))))
  (define-unsafe-tag-predicate $top-tag-id?			top-tag-id)
  (define-unsafe-tag-predicate $procedure-tag-id?		procedure-tag-id)
  (define-unsafe-tag-predicate $predicate-tag-id?		predicate-tag-id)
  (define-unsafe-tag-predicate $vector-tag-id?			vector-tag-id)
  (define-unsafe-tag-predicate $list-tag-id?			list-tag-id)
  (define-unsafe-tag-predicate $null-tag-id?			null-tag-id)
  (define-unsafe-tag-predicate $nlist-tag-id?			nlist-tag-id)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((define-tag-predicate (syntax-rules ()
			     ((_ ?who ?unsafe-pred)
			      (define (?who obj)
				(and (identifier? obj)
				     (?unsafe-pred obj)))))))
  (define-tag-predicate top-tag-id?			$top-tag-id?)
  (define-tag-predicate procedure-tag-id?		$procedure-tag-id?)
  (define-tag-predicate predicate-tag-id?		$predicate-tag-id?)
  (define-tag-predicate vector-tag-id?			$vector-tag-id?)
  (define-tag-predicate list-tag-id?			$list-tag-id?)
  (define-tag-predicate null-tag-id?			$null-tag-id?)
  (define-tag-predicate nlist-tag-id?			$nlist-tag-id?)
  #| end of LET-SYNTAX |# )


;;;; more external modules

(include "psyntax.lexical-environment.syntax-match.scm" #t)
(include "psyntax.lexical-environment.syntax-utilities.scm" #t)
(include "psyntax.lexical-environment.type-identifiers-and-signatures.scm" #t)

(import PSYNTAX-SYNTAX-MATCH)
(import PSYNTAX-SYNTAX-UTILITIES)
(import PSYNTAX-TYPE-IDENTIFIERS-AND-SIGNATURES)


;;;; errors helpers

(define (expression-position x)
  (if (stx? x)
      (let ((x (stx-expr x)))
	(if (annotation? x)
	    (annotation-textual-position x)
	  (condition)))
    (condition)))


;;;; condition object types: descriptive objects

;;This used to describe the syntax object  acting as operator in an application form.
;;The single  argument to the  constructor must be  a syntax object  representing the
;;operator.
;;
(define-condition-type &application-operator
    &condition
  make-application-operator-condition
  application-operator-condition?
  (operator application-operator-condition.operator))

;;This used to describe the syntax object  acting as operator in an application form.
;;The  single  argument  to  the  constructor  must  be  a  list  of  syntax  objects
;;representing the operands.
(define-condition-type &application-operands
    &condition
  make-application-operands-condition
  application-operands-condition?
  (operands application-operands-condition.operands))

;;This is used to describe a type involved in an exception, for example when the type
;;identifier is  used by  the syntax  IS-A? or  METHOD-CALL.  The  field must  be the
;;syntactic identifier  bound to the type  specification (for example the  name of an
;;R6RS record type).
;;
(define-condition-type &type-syntactic-identifier
    &condition
  make-type-syntactic-identifier-condition
  type-syntactic-identifier-condition?
  (type-identifier	condition-type-syntactic-identifier))
(define &type-syntactic-identifier-rtd
  (record-type-descriptor &type-syntactic-identifier))
(define &type-syntactic-identifier-rcd
  (record-constructor-descriptor &type-syntactic-identifier))

;;This  is used  to describe  the type  identifier of  an argument  to function,  the
;;expected type of object used in the function call.
;;
(define-condition-type &argument-type-syntactic-identifier
    &condition
  make-argument-type-syntactic-identifier-condition
  argument-type-syntactic-identifier-condition?
  (argument-type-identifier	condition-argument-type-syntactic-identifier))
(define &argument-type-syntactic-identifier-rtd
  (record-type-descriptor &argument-type-syntactic-identifier))
(define &argument-type-syntactic-identifier-rcd
  (record-constructor-descriptor &argument-type-syntactic-identifier))

;;This is used to describe the type identifier of an operand to function, the type of
;;the actual object used as operand in a specific function call.
;;
(define-condition-type &operand-type-syntactic-identifier
    &condition
  make-operand-type-syntactic-identifier-condition
  operand-type-syntactic-identifier-condition?
  (operand-type-identifier	condition-operand-type-syntactic-identifier))
(define &operand-type-syntactic-identifier-rtd
  (record-type-descriptor &operand-type-syntactic-identifier))
(define &operand-type-syntactic-identifier-rcd
  (record-constructor-descriptor &operand-type-syntactic-identifier))

(define-condition-type &argument-index
    &condition
  make-argument-index-condition
  argument-index-condition?
  (argument-index	condition-argument-index))
(define &argument-index-rtd
  (record-type-descriptor &argument-index))
(define &argument-index-rcd
  (record-constructor-descriptor &argument-index))

;;This is used to describe a type's  method name involved in an exception.  The field
;;must be a symbol representing the method name.
(define-condition-type &type-method-name
    &condition
  make-type-method-name-condition
  condition-type-method-name?
  (method-name condition-type-method-name))

(define-condition-type &syntactic-binding-descriptor
    &condition
  make-syntactic-binding-descriptor-condition
  syntactic-binding-descriptor-condition?
  (descr	condition-syntactic-binding-descriptor))

;;This  is  used  to describe  exceptions  in  which  the  expanded expression  of  a
;;right-hand side (RHS) syntax  definition (DEFINE-SYNTAX, LET-SYNTAX, LETREC-SYNTAX,
;;DEFINE-FLUID-SYNTAX,  FLUID-LET-SYNTAX,  etc.)   has  a role.   The  value  in  the
;;CORE-EXPR slot must be a symbolic expression representing the a core expression.
(define-condition-type &syntax-definition-expanded-rhs-condition
    &condition
  make-syntax-definition-expanded-rhs-condition
  syntax-definition-expanded-rhs-condition?
  (core-expr condition-syntax-definition-expanded-rhs))

;;This is used to describe exceptions in  which the return value of the evaluation of
;;a   right-hand   side   (RHS)   syntax   definition   (DEFINE-SYNTAX,   LET-SYNTAX,
;;LETREC-SYNTAX, DEFINE-FLUID-SYNTAX, FLUID-LET-SYNTAX, etc.)  has a role.  The value
;;in the  RETVAL slot  must be  the value returned  by the  evaluation of  the syntax
;;definition RHS expression.
(define-condition-type &syntax-definition-expression-return-value
    &condition
  make-syntax-definition-expression-return-value-condition
  syntax-definition-expression-return-value-condition?
  (retval condition-syntax-definition-expression-return-value))

;;This  is used  to include  a retvals  signature specification  in generic  compound
;;objects, for example because we were expecting a signature with some properties and
;;the one we got does not have them.
(define-condition-type &retvals-signature-condition
    &condition
  %make-retvals-signature-condition
  retvals-signature-condition?
  (signature retvals-signature-condition-signature))

(define* (make-retvals-signature-condition {sig type-signature?})
  (%make-retvals-signature-condition sig))

;;This is used  to describe exceptions in which  the input form of a macro  use has a
;;role.  The value  in the FORM slot  must be a syntax object  representing the macro
;;use input form.
;;
;;See MAKE-MACRO-USE-INPUT-FORM-CONDITION for details.
(define-condition-type &macro-use-input-form-condition
    &condition
  %make-macro-use-input-form-condition
  macro-use-input-form-condition?
  (form condition-macro-use-input-form))

;;This is used to represent the succession  of transformations a macro use input form
;;undergoes while  expanded; there is  an instance of  this condition type  for every
;;transformation.
;;
;;See MAKE-MACRO-USE-INPUT-FORM-CONDITION for details.
(define-condition-type &macro-expansion-trace
    &condition
  make-macro-expansion-trace macro-expansion-trace?
  (form macro-expansion-trace-form))


;;;; condition object types: error objects

;;This  is used  to  describe exceptions  in  which  there is  a  mismatch between  a
;;resulting expression type signature and an expected one; this condition type should
;;be the root of all the condition types in this category.
(define-condition-type &expand-time-type-signature-violation
    &violation
  make-expand-time-type-signature-violation
  expand-time-type-signature-violation?)

(define &expand-time-type-signature-violation-rtd
  (record-type-descriptor &expand-time-type-signature-violation))

(define &expand-time-type-signature-violation-rcd
  (record-constructor-descriptor &expand-time-type-signature-violation))

;;; --------------------------------------------------------------------

;;This is  used to describe  exceptions in which:  after expanding an  expression, we
;;were expecting it to  have a "retvals-signature" matching a given  one, but the one
;;we got does not match.
;;
;;See the function EXPAND-TIME-RETVALS-SIGNATURE-VIOLATION for details.
(define-condition-type &expand-time-retvals-signature-violation
    &expand-time-type-signature-violation
  %make-expand-time-retvals-signature-violation
  expand-time-retvals-signature-violation?
  (expected-signature expand-time-retvals-signature-violation-expected-signature)
  (returned-signature expand-time-retvals-signature-violation-returned-signature))

(define &expand-time-retvals-signature-violation-rtd
  (record-type-descriptor &expand-time-retvals-signature-violation))

(define &expand-time-retvals-signature-violation-rcd
  (record-constructor-descriptor &expand-time-retvals-signature-violation))

(define* (make-expand-time-retvals-signature-violation {expected-signature type-signature?}
						       {returned-signature type-signature?})
  (%make-expand-time-retvals-signature-violation expected-signature returned-signature))

;;; --------------------------------------------------------------------

(define-condition-type &vicare-scheme-internal-error
    &error
  make-vicare-scheme-internal-error
  vicare-scheme-internal-error?)


;;;; exception raising functions

(define (assertion-error expr source-identifier
			 byte-offset character-offset
			 line-number column-number)
  ;;Invoked by the expansion of the ASSERT macro to raise an assertion violation.
  ;;
  (raise
   (condition (make-assertion-violation)
	      (make-who-condition 'assert)
	      (make-message-condition "assertion failed")
	      (make-irritants-condition (list expr))
	      (make-source-position-condition source-identifier
	      				      byte-offset character-offset
	      				      line-number column-number))))

(case-define* syntax-violation/internal-error
  ((who msg form)
   (syntax-violation/internal-error who msg form #f))
  ((who {msg string?} form subform)
   (raise
    (condition (make-who-condition who)
	       (make-message-condition (string-append "Vicare Scheme: internal error: " msg))
	       (make-syntax-violation form subform)
	       (make-vicare-scheme-internal-error)))))

(define* (assertion-violation/internal-error who {msg string?} . irritants)
  (raise
   (condition (make-vicare-scheme-internal-error)
	      (make-assertion-violation)
	      (make-who-condition who)
	      (make-message-condition (string-append "Vicare Scheme: internal error: " msg))
	      (make-irritants-condition irritants))))

(module (raise-unbound-error
	 syntax-violation
	 expand-time-retvals-signature-violation
	 make-macro-use-input-form-condition
	 raise-compound-condition-object)

  (case-define* syntax-violation
    ;;Defined by R6RS.  WHO must be false or a string or a symbol.  MESSAGE must be a
    ;;string.  FORM  must be a  syntax object  or a datum  value.  SUBFORM must  be a
    ;;syntax object or a datum value.
    ;;
    ;;The  SYNTAX-VIOLATION  procedure  raises   an  exception,  reporting  a  syntax
    ;;violation.   WHO  should  describe  the macro  transformer  that  detected  the
    ;;exception.  The MESSAGE argument should describe the violation.  FORM should be
    ;;the erroneous source  syntax object or a datum value  representing a form.  The
    ;;optional SUBFORM argument should be a syntax object or datum value representing
    ;;a form that more precisely locates the violation.
    ;;
    ;;If WHO  is false, SYNTAX-VIOLATION attempts  to infer an appropriate  value for
    ;;the condition object (see below) as  follows: when FORM is either an identifier
    ;;or  a list-structured  syntax  object  containing an  identifier  as its  first
    ;;element, then  the inferred  value is the  identifier's symbol.   Otherwise, no
    ;;value for WHO is provided as part of the condition object.
    ;;
    ;;The condition  object provided with  the exception has the  following condition
    ;;types:
    ;;
    ;;* If  WHO is not  false or  can be inferred,  the condition has  condition type
    ;;   "&who", with  WHO as  the value  of  its field.   In that  case, WHO  should
    ;;   identify the  procedure or  entity that  detected the  exception.  If  it is
    ;;  false, the condition does not have condition type "&who".
    ;;
    ;;* The condition has condition type "&message", with MESSAGE as the value of its
    ;;  field.
    ;;
    ;;* The condition has condition type "&syntax" with FORM and SUBFORM as the value
    ;;  of its fields.  If SUBFORM is not provided, the value of the subform field is
    ;;  false.
    ;;
    ((who {msg string?} form)
     (syntax-violation who msg form #f))
    ((who {msg string?} form subform)
     (raise-compound-condition-object who msg form (make-syntax-violation form subform))))

  (define* (expand-time-retvals-signature-violation source-who form subform
						    {expected-retvals-signature type-signature?}
						    {returned-retvals-signature type-signature?})
    ;;To be used at  expand-time when we were expecting a  signature from an expanded
    ;;expression  and we  received  an incompatible  one, for  example  in the  macro
    ;;transformers of TAG-ASSERT and TAG-ASSERT-AND-RETURN.
    ;;
    (raise-compound-condition-object source-who "expand-time return values signature mismatch" form
				     (condition
				      (%make-expand-time-retvals-signature-violation expected-retvals-signature
										     returned-retvals-signature)
				      (make-syntax-violation form subform))))

  (define (raise-unbound-error source-who input-form.stx id)
    ;;Raise an  "unbound identifier"  exception.  This  is to  be used  when applying
    ;;ID->LABEL  to the  identifier  ID returns  false, and  such  result is  invalid
    ;;because we were expecting ID to be bound.
    ;;
    ;;Often INPUT-FORM.STX is ID itself, and we can do nothing about it.
    ;;
    (raise-compound-condition-object source-who "unbound identifier" input-form.stx
				     (condition
				      (make-undefined-violation)
				      (make-syntax-violation input-form.stx id))))

  (define* (raise-compound-condition-object source-who {msg string?} input-form.stx condition-object)
    ;;Raise a compound condition object.
    ;;
    ;;SOURCE-WHO can be  a string, symbol or  false; it is used as  value for "&who".
    ;;When  false: INPUT-FORM.STX  is inspected  to  determine a  possible value  for
    ;;"&who".
    ;;
    ;;MSG must be a string; it is used as value for "&message".
    ;;
    ;;INPUT-FORM.STX must be a (wrapped  or unwrapped) syntax object representing the
    ;;subject of  the raised exception.   It is used for  both inferring a  value for
    ;;"&who" and retrieving source location informations.
    ;;
    ;;CONDITION-OBJECT is  an already  built condition  object that  is added  to the
    ;;raised compound.
    ;;
    (let ((source-who (cond ((or (string? source-who)
				 (symbol? source-who))
			     source-who)
			    ((not source-who)
			     (syntax-case input-form.stx ()
			       (?id
				(identifier? #'?id)
				(syntax->datum #'?id))
			       ((?id . ?rest)
				(identifier? #'?id)
				(syntax->datum #'?id))
			       (_  #f)))
			    (else
			     (assertion-violation __who__ "invalid who argument" source-who)))))
      (raise
       (condition (if source-who
		      (make-who-condition source-who)
		    (condition))
		  (make-message-condition msg)
		  condition-object
		  (%expression->source-position-condition input-form.stx)
		  (%extract-macro-expansion-trace input-form.stx)))))

  (define* (make-macro-use-input-form-condition stx)
    (condition
     (%make-macro-use-input-form-condition stx)
     (%extract-macro-expansion-trace stx)))

  (define (%extract-macro-expansion-trace stx)
    ;;Extraxt from the (wrapped or unwrapped) syntax object STX the sequence of macro
    ;;expansion traces  from the AE* field  of "stx" records and  return a compound
    ;;condition object representing them, as instances of "&macro-expansion-trace".
    ;;
    ;;NOTE Unfortunately it  does not always go  as we would like.   For example, the
    ;;program:
    ;;
    ;;   (import (vicare))
    ;;   (define-syntax (one stx)
    ;;     (syntax-case stx ()
    ;;       ((_)
    ;;        (syntax-violation 'one "demo" stx #f))))
    ;;   (define-syntax (two stx)
    ;;     (syntax-case stx ()
    ;;       ((_)
    ;;        #'(one))))
    ;;   (two)
    ;;
    ;;raises the error:
    ;;
    ;;*** Vicare: unhandled exception:
    ;;  Condition components:
    ;;    1. &who: one
    ;;    2. &message: "demo"
    ;;    3. &syntax:
    ;;        form: #<syntax expr=(one) mark*=(#f "" src) ...>
    ;;        subform: #f
    ;;    4. &source-position:
    ;;        port-id: "../tests/test-demo.sps"
    ;;        byte: 516
    ;;        character: 514
    ;;        line: 31
    ;;        column: 8
    ;;    5. &macro-expansion-trace: #<syntax expr=(one) mark*=(#f "" src) ...>
    ;;    6. &macro-expansion-trace: #<syntax expr=(two) mark*=(src) ...>
    ;;
    ;;and we can see  the expansion's trace.  But if we compose  the output form with
    ;;pieces of different origin:
    ;;
    ;;   (import (vicare))
    ;;   (define-syntax (one stx)
    ;;     (syntax-case stx ()
    ;;       ((_ . ?stuff)
    ;;        (syntax-violation 'one "demo" stx #f))))
    ;;   (define-syntax (two stx)
    ;;     (syntax-case stx ()
    ;;       ((_ ?id)
    ;;        #`(one ?id))))
    ;;   (two display)
    ;;
    ;;the error is:
    ;;
    ;;   *** Vicare: unhandled exception:
    ;;    Condition components:
    ;;      1. &who: one
    ;;      2. &message: "demo"
    ;;      3. &syntax:
    ;;          form: (#<syntax expr=one mark*=(#f "" src) ...>
    ;;                 #<syntax expr=display mark*=(#f src) ...>
    ;;                 . #<syntax expr=() mark*=(#f "" src)>)
    ;;          subform: #f
    ;;
    ;;and there is no trace.  This is  because the syntax object used as "form" value
    ;;in "&syntax" is  not wrapped, and SYNTAX-VIOLATION cannot decide  from which of
    ;;its components it makes sense to extract  the trace.  (Marco Maggi; Sat Apr 12,
    ;;2014)
    ;;
    (let loop ((X stx))
      (cond ((stx? X)
	     (apply condition
		    (make-macro-expansion-trace X)
		    (map loop (stx-annotated-expr* X))))
	    ((annotation? X)
	     ;;Here we  only want to wrap  X into an  "stx" object, we do  not care
	     ;;about the context.  (Marco Maggi; Sat Apr 11, 2015)
	     (make-macro-expansion-trace (make-stx X '() '() '())))
	    (else
	     (condition)))))

  (define (%expression->source-position-condition x)
    (expression-position x))

  #| end of module |# )


;;;; done

($record-type-printer-set! (record-type-descriptor stx) stx-record-printer)

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'let-syntax-rules			'scheme-indent-function 1)
;; eval: (put 'case-identifier-syntactic-binding-descriptor			'scheme-indent-function 1)
;; eval: (put 'case-identifier-syntactic-binding-descriptor/no-indirection	'scheme-indent-function 1)
;; End:
