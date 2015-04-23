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

    top-level-context
    self-evaluating?

    ;; non-interaction environment objects
    env
    make-env				env?
    env-names				set-env-names!
    env-labels				set-env-labels!
    env-itc				set-env-itc!

    ;; interaction environment objects
    interaction-env
    make-interaction-env		interaction-env?
    interaction-env-rib			set-interaction-env-rib!
    interaction-env-lexenv		set-interaction-env-lexenv!
    interaction-env-lab.loc/lex*	set-interaction-env-lab.loc/lex*!

    ;; operations
    environment?
    environment-symbols
    environment-labels
    environment-libraries
    environment-binding

    ;; label gensyms, lexical variable gensyms, storage location gensyms
    generate-lexical-gensym
    generate-storage-location-gensym
    generate-label-gensym
    generate-or-retrieve-label-and-lex-gensyms
    generate-or-retrieve-define-syntax-label-gensym

    ;; LEXENV entries and syntactic binding descriptors
    lexenv-entry.label
    lexenv-entry.binding-descriptor
    syntactic-binding-descriptor.type
    syntactic-binding-descriptor.value

    lexical-var-binding-descriptor-value.lex-name
    lexical-var-binding-descriptor-value.assigned?
    lexenv-add-lexical-var-binding
    lexenv-add-lexical-var-bindings

    make-syntactic-binding-descriptor/local-macro/non-variable-transformer
    make-syntactic-binding-descriptor/local-macro/variable-transformer

    make-syntactic-binding-descriptor/struct-type-name
    struct-type-name-binding-descriptor?
    struct-type-name-binding-descriptor.type-descriptor

    make-syntactic-binding-descriptor/record-type-name
    record-type-name-binding-descriptor?
    record-type-name-binding-descriptor.rtd-id
    record-type-name-binding-descriptor.rcd-id
    record-type-name-binding-descriptor.spec
    R6RS-RECORD-TYPE-SPEC

    make-syntactic-binding-descriptor/local-global-macro/fluid-syntax
    fluid-syntax-binding-descriptor?
    fluid-syntax-binding-descriptor.fluid-label

    make-syntactic-binding-descriptor/local-global-macro/synonym-syntax

    make-syntactic-binding-descriptor/local-macro/compile-time-value
    local-compile-time-value-binding-descriptor.object
    global-compile-time-value-binding-descriptor.object

    make-syntactic-binding-descriptor/local-global-macro/module-interface

    make-syntactic-binding-descriptor/pattern-variable
    pattern-variable-binding-descriptor?

    ;; lexical environment utilities
    label->syntactic-binding-descriptor
    label->syntactic-binding-descriptor/no-indirection

    ;; marks of lexical contours
    src-marked?
    rib-src-marked-source-names

    ;; rib objects
    rib
    make-rib					rib?
    rib-name*					set-rib-name*!
    rib-mark**					set-rib-mark**!
    rib-label*					set-rib-label*!
    rib-sealed/freq				set-rib-sealed/freq!

    $rib-name*					$set-rib-name*!
    $rib-mark**					$set-rib-mark**!
    $rib-label*					$set-rib-label*!
    $rib-sealed/freq				$set-rib-sealed/freq!

    ;; rib operations
    false-or-rib?				list-of-ribs?
    make-empty-rib
    make-rib-from-identifiers-and-labels	make-top-rib-from-source-names-and-labels
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

    ;; syntax objects: mapping identifiers to labels
    id->label
    id->label/intern
    id->label/or-error
    id->record-type-name-binding-descriptor

    ;; syntax objects: marks
    same-marks?
    join-wraps
    ADD-MARK

    ;; identifiers from the built-in environment
    bless
    trace-bless
    scheme-stx
    core-prim-id
    underscore-id			underscore-id?
    ellipsis-id				ellipsis-id?
    place-holder-id			place-holder-id?
    procedure-pred-id
    jolly-id?

    ;; public interface: identifiers handling
    identifier?				false-or-identifier-bound?
    false-or-identifier?
    bound-identifier=?			~bound-identifier=?
    free-identifier=?			~free-identifier=?
    identifier-bound?			~identifier-bound?
    identifier->symbol
    syntax-parameter-value

    ;; utilities for identifiers
    valid-bound-ids?
    distinct-bound-ids?
    duplicate-bound-formals?
    bound-id-member?
    identifier-append

    ;; condition object types
    &syntax-definition-expanded-rhs-condition
    make-syntax-definition-expanded-rhs-condition
    syntax-definition-expanded-rhs-condition?
    condition-syntax-definition-expanded-rhs

    &syntax-definition-expression-return-value
    make-syntax-definition-expression-return-value-condition
    syntax-definition-expression-return-value-condition?
    condition-syntax-definition-expression-return-value

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
    make-expand-time-type-signature-violation
    expand-time-type-signature-violation?

    &expand-time-retvals-signature-violation
    ;;%make-expand-time-retvals-signature-violation
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
    (rnrs mutable-pairs)
    (prefix (rnrs syntax-case) sys.)
    (psyntax.config)
    (psyntax.compat)
    (only (psyntax.special-transformers)
	  compile-time-value?
	  compile-time-value-object)
    (only (psyntax.library-manager)
	  visit-library
	  label->imported-syntactic-binding-descriptor))


;;;; helpers

(include "psyntax.helpers.scm" #t)

(define (non-compound-sexp? obj)
  (or (null? obj)
      (self-evaluating? obj)
      ;;Notice that struct instances are not self evaluating.
      (struct? obj)))

(define (self-evaluating? x)
  (or (number?			x)
      (string?			x)
      (char?			x)
      (boolean?			x)
      (bytevector?		x)
      (keyword?			x)
      (would-block-object?	x)
      (unbound-object?		x)
      (bwp-object?		x)))

(define-syntax let-syntax-rules
  (syntax-rules ()
    ((_ (((?lhs0 . ?args0) ?rhs0) ((?lhs . ?args) ?rhs) ...) ?body0 ?body ...)
     (let-syntax ((?lhs0 (syntax-rules () ((_ . ?args0) ?rhs0)))
		  (?lhs  (syntax-rules () ((_ . ?args)  ?rhs)))
		  ...)
       ?body0 ?body ...))
    ))


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
(define-record env
  (names
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
   )
  (lambda (S port sub-printer)
    (display "#<environment>" port)))

(define-record interaction-env
  (rib
		;The  top  RIB   structure  for  the  evaluation  of   code  in  this
		;environment.  It maps bound identifiers to labels.
   lexenv
		;The LEXENV  for both run  time and expand  time.  It maps  labels to
		;syntactic binding descriptors.
   lab.loc/lex*
		;An alist having label gensyms as keys and loc gensyms as values; the
		;loc gensyms are also used as lex gensyms.  It maps binding labels to
		;storage location gensyms.
   )
  (lambda (S port sub-printer)
    (display "#<interaction-environment>" port)))


;;;; top level environment objects: operations

(define (environment? obj)
  (or (env? obj)
      (interaction-env? obj)))

(define* (environment-symbols x)
  ;;Return a list of symbols representing the names of the bindings from
  ;;the given environment.
  ;;
  (cond ((env? x)
	 (vector->list ($env-names x)))
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
  (vector->list ($env-labels x)))

(define* (environment-libraries x)
  ;;Return  the  list  of  LIBRARY records  representing  the  libraries
  ;;forming the environment.
  ;;
  (unless (env? x)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" x))
  (($env-itc x)))

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
	     ($env-names  env)
	     ($env-labels env))))
    (if P
	(values (car P) (cdr P))
      (values #f #f))))


;;;; lexical environment: LEXENV entries and syntactic bindings helpers

;;Given the entry  from a lexical environment: return  the gensym acting
;;as label.
;;
(define lexenv-entry.label car)

;;Given the entry from a lexical environment: return the binding value.
;;
(define lexenv-entry.binding-descriptor cdr)

(define-syntax-rule (make-syntactic-binding-descriptor ?bind-type ?bind-val)
  ;;Build and return a new syntactic binding descriptor.
  ;;
  (cons (quote ?bind-type) ?bind-val))

(define-syntax-rule (syntactic-binding-descriptor.type ?binding-descriptor)
  ;;Given a syntactic binding descriptor, return its type: a symbol.
  ;;
  (car ?binding-descriptor))

(define-syntax-rule (syntactic-binding-descriptor.value ?binding-descriptor)
  ;;Given a syntactic binding descriptor, return its value: a pair.
  ;;
  (cdr ?binding-descriptor))

;;; --------------------------------------------------------------------
;;; helpers

(define-syntax-rule (define-syntactic-binding-descriptor-predicate ?who ?type)
  (define (?who obj)
    ;;Return true if  OBJ is a syntactic binding descriptor  of type ?TYPE; otherwise
    ;;return false.
    ;;
    (and (pair? obj)
	 (eq? (quote ?type) (syntactic-binding-descriptor.type obj)))))

;;; --------------------------------------------------------------------
;;; core primitive bindings

;;NOTE Commented out because unused.  Kept here for reference.  (Marco Maggi; Sat Apr
;;18, 2015)
;;
(comment
 (define (make-syntactic-binding-descriptor/core-primitive public-name)
   ;;Build and  return a syntactic  binding descriptor representing a  core primitive.
   ;;PUBLIC-NAME must be a symbol representing the public name of the primitive.
   ;;
   ;;The returned descriptor has format:
   ;;
   ;;   (core-primt . ?public-name)
   ;;
   (make-syntactic-binding-descriptor core-prim public-name))

 (define-syntactic-binding-descriptor-predicate core-primitive-binding-descriptor? core-prim)

 (define-syntax-rule (core-primitive-binding-descriptor.public-name ?descriptor)
   ;;Given a  syntactic binding descriptor  representing a core primitive:  return its
   ;;public name symbol.
   ;;
   (cdr ?descriptor))
 /comment)

;;; --------------------------------------------------------------------
;;; lexical variable bindings

(define (make-syntactic-binding-descriptor/lexical-var lex-name)
  ;;Build and return  a syntactic binding descriptor representing  a constant lexical
  ;;variable;  this variable  is never  assigned  in the  code.  LEX-NAME  must be  a
  ;;lexical  gensym representing  the  name of  a lexical  variable  in the  expanded
  ;;language forms.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (lexical . (?lex-name . #f))
  ;;
  (make-syntactic-binding-descriptor lexical (cons lex-name #f)))

(define-syntactic-binding-descriptor-predicate lexical-var-binding-descriptor? lexical)

(define-syntax-rule (lexical-var-binding-descriptor-value.lex-name ?descriptor-value)
  ;;Accessor  for  the lexical  gensym  in  a  lexical variable's  syntactic  binding
  ;;descriptor's value.
  ;;
  ;;A syntactic binding representing a lexical variable has descriptor with format:
  ;;
  ;;   (lexical . ?descriptor-value)
  ;;
  ;;where ?DESCRIPTOR-VALUE has format:
  ;;
  ;;  (?lex-name . ?mutable)
  ;;
  ;;this macro returns  the ?LEX-NAME, a lexical gensym representing  the variable in
  ;;the expanded code.
  ;;
  (car ?descriptor-value))

(define-syntax lexical-var-binding-descriptor-value.assigned?
  ;;Accessor  and mutator  for assigned  boolean  in a  lexical variable's  syntactic
  ;;binding descriptor's value.
  ;;
  ;;A syntactic binding representing a lexical variable has descriptor with format:
  ;;
  ;;   (lexical . ?descriptor-value)
  ;;
  ;;where ?DESCRIPTOR-VALUE has format:
  ;;
  ;;  (?lex-name . ?assigned)
  ;;
  ;;The accessor macro returns  the ?ASSIGNED value, which is a  boolean: true if the
  ;;lexical variable  is assigned at  least once in  the code, otherwise  false.  The
  ;;mutator macro sets a new ?ASSIGNED value.
  ;;
  (syntax-rules ()
    ((_ ?descriptor-value)
     (cdr ?descriptor-value))
    ((_ ?descriptor-value #t)
     (set-cdr! ?descriptor-value #t))
    ))

(define (lexenv-add-lexical-var-binding label lex lexenv)
  ;;Push  on the  LEXENV  a  new entry  representing  a  non-assigned local  variable
  ;;binding; return the resulting LEXENV.
  ;;
  ;;LABEL must be a  syntactic binding's label gensym.  LEX must  be a lexical gensym
  ;;representing the name of a lexical variable in the expanded language forms.
  ;;
  (cons (cons label (make-syntactic-binding-descriptor/lexical-var lex))
	lexenv))

(define (lexenv-add-lexical-var-bindings label* lex* lexenv)
  ;;Push  on the  LEXENV multiple  entries representing  non-assigned local  variable
  ;;bindings; return the resulting LEXENV.
  ;;
  ;;LABEL* must be a list of syntactic  binding's label gensyms.  LEX* must be a list
  ;;of lexical  gensyms representing the names  of lexical variables in  the expanded
  ;;language forms.
  ;;
  (if (pair? label*)
      (lexenv-add-lexical-var-bindings ($cdr label*) ($cdr lex*)
				       (lexenv-add-lexical-var-binding ($car label*) ($car lex*) lexenv))
    lexenv))

;;; --------------------------------------------------------------------
;;; local macro with non-variable transformer bindings

(define (make-syntactic-binding-descriptor/local-macro/non-variable-transformer transformer expanded-expr)
  ;;Build  and return  a syntactic  binding descriptor  representing a  local keyword
  ;;binding  with  non-variable  transformer.
  ;;
  ;;The argument TRANSFORMER must be  the transformer's closure object.  The argument
  ;;EXPANDED-EXPR must be  a symbolic expression representing the  right-hand side of
  ;;this macro definition in the core language.
  ;;
  ;;Given the definition:
  ;;
  ;;   (define-syntax ?lhs ?rhs)
  ;;
  ;;EXPANDED-EXPR  is the  result of  expanding ?RHS,  TRANSFORMER is  the result  of
  ;;compiling and evaluating EXPANDED-EXPR.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (local-macro . (?transformer . ?expanded-expr))
  ;;
  (make-syntactic-binding-descriptor local-macro (cons transformer expanded-expr)))

;;; --------------------------------------------------------------------
;;; local macro with variable transformer bindings

(define (make-syntactic-binding-descriptor/local-macro/variable-transformer transformer expanded-expr)
  ;;Build  and return  a syntactic  binding descriptor  representing a  local keyword
  ;;binding with variable transformer.
  ;;
  ;;The argument TRANSFORMER must be  the transformer's closure object.  The argument
  ;;EXPANDED-EXPR must be  a symbolic expression representing the  right-hand side of
  ;;this macro definition in the core language.
  ;;
  ;;Given the definition:
  ;;
  ;;   (define-syntax ?lhs ?rhs)
  ;;
  ;;EXPANDED-EXPR  is the  result of  expanding ?RHS,  TRANSFORMER is  the result  of
  ;;compiling  and evaluating  EXPANDED-EXPR.  Syntactic  bindings of  this type  are
  ;;generated  when the  return  value of  ?RHS  is the  return value  of  a call  to
  ;;MAKE-VARIABLE-TRANSFORMER.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (local-macro . (?transformer . ?expanded-expr))
  ;;
  (make-syntactic-binding-descriptor local-macro! (cons transformer expanded-expr)))

;;; --------------------------------------------------------------------
;;; Vicare struct-type name bindings

(define (make-syntactic-binding-descriptor/struct-type-name type-descriptor)
  ;;Build and return a syntactic binding descriptor representing a struct-type name.
  ;;
  ;;The argument TYPE-DESCRIPTOR must be the struct-type descriptor itself.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   ($struct-type-name . ?type-descriptor)
  ;;
  (make-syntactic-binding-descriptor $struct-type-name type-descriptor))

;;Return true if the argument is  a syntactic binding descriptor representing a local
;;or imported binding describing a struct-type descriptor.
;;
(define-syntactic-binding-descriptor-predicate struct-type-name-binding-descriptor?
  $struct-type-name)

(define-syntax-rule (struct-type-name-binding-descriptor.type-descriptor ?descriptor)
  ;;Given a syntactic binding descriptor  representing a struct-type name: return the
  ;;struct-type descriptor itself.
  ;;
  ;;We expect the descriptor to have the format:
  ;;
  ;;   ($struct-type-name . ?type-descriptor)
  ;;
  (syntactic-binding-descriptor.value ?descriptor))

;;; --------------------------------------------------------------------
;;; core R6RS record-type descriptor binding

(define (core-record-type-name-binding-descriptor->record-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic  binding  descriptor  from  the  representation  of  a  core
  ;;record-type  name (established  by  the  boot image)  to  a  representation of  a
  ;;record-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have format:
  ;;
  ;;   ($core-rtd . (?rtd-name ?rcd-name))
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   ($record-type-name . (?rtd-id ?rcd-id))
  ;;
  (set-car! descriptor '$record-type-name)
  (set-cdr! descriptor (let ((bindval (syntactic-binding-descriptor.value descriptor)))
			 (list (bless (core-record-type-name-binding-descriptor-value.rtd-name bindval))
			       (bless (core-record-type-name-binding-descriptor-value.rcd-name bindval))))))

;;Commented out  because unused, but kept  for reference.  (Marco Maggi;  Mon Apr 20,
;;2015)
;;
;; (define (core-record-type-name-binding-descriptor->record-type-name-binding-descriptor descriptor)
;;   ;;Convert  a syntactic  binding  descriptor representing  a  core record-type  name
;;   ;;(established by the boot image)  into a syntactic binding descriptor representing
;;   ;;a record-type name in the format usable by the expander.
;;   ;;
;;   ;;We expect the core descriptor to have format:
;;   ;;
;;   ;;   ($core-rtd . (?rtd-name ?rcd-name))
;;   ;;
;;   ;;and the usable descriptor to have the format:
;;   ;;
;;   ;;   ($record-type-name . (?rtd-id ?rcd-id))
;;   ;;
;;   (let ((bindval (syntactic-binding-descriptor.value descriptor)))
;;     (make-syntactic-binding-descriptor/record-type-name
;;      (bless (core-record-type-name-binding-descriptor-value.rtd-name bindval))
;;      (bless (core-record-type-name-binding-descriptor-value.rcd-name bindval)))))

;;Return true  if the argument  is a syntactic  binding descriptor describing  a R6RS
;;record-type descriptor established by the boot image.
;;
(define-syntactic-binding-descriptor-predicate core-record-type-name-binding-descriptor?
  $core-rtd)

(define-syntax-rule ($core-record-type-name-binding-descriptor? ?descriptor)
  (eq? '$core-rtd ($car ?descriptor)))

(define-syntax-rule (core-record-type-name-binding-descriptor-value.rtd-name ?descriptor-value)
  (car ?descriptor-value))

(define-syntax-rule (core-record-type-name-binding-descriptor-value.rcd-name ?descriptor-value)
  (cadr ?descriptor-value))

;;; --------------------------------------------------------------------
;;; R6RS record-type descriptor binding

(case-define make-syntactic-binding-descriptor/record-type-name
  ;;Build and return a syntactic binding descriptor representing a record-type name.
  ;;
  ;;The  argument RTD-ID  must be  a syntactic  identifier bound  to the  record-type
  ;;descriptor object.  The  argument RCD-ID must be a syntactic  identifier bound to
  ;;the  record-constructor descriptor  object.   The optional  argument  SPEC is  an
  ;;instance of "r6rs-record-type-spec".
  ;;
  ;;The returned descriptor has one of the formats:
  ;;
  ;;   ($record-type-name . (?rtd-id ?rcd-id))
  ;;   ($record-type-name . (?rtd-id ?rcd-id . spec))
  ;;
  ((rtd-id rcd-id)
   (make-syntactic-binding-descriptor $record-type-name (list rtd-id rcd-id)))
  ((rtd-id rcd-id spec)
   (make-syntactic-binding-descriptor $record-type-name (cons rtd-id (cons rcd-id spec)))))

;;Return true if the argument is  a syntactic binding descriptor representing a local
;;or imported binding describing a R6RS record-type descriptor.
;;
(define-syntactic-binding-descriptor-predicate record-type-name-binding-descriptor?
  $record-type-name)

(define-syntax-rule (record-type-name-binding-descriptor.rtd-id ?descriptor)
  (record-type-name-binding-descriptor-value.rtd-id (syntactic-binding-descriptor.value ?descriptor)))

(define-syntax-rule (record-type-name-binding-descriptor.rcd-id ?descriptor)
  (record-type-name-binding-descriptor-value.rcd-id (syntactic-binding-descriptor.value ?descriptor)))

(define-syntax-rule (record-type-name-binding-descriptor.spec ?descriptor)
  (record-type-name-binding-descriptor-value.spec   (syntactic-binding-descriptor.value ?descriptor)))

(define-syntax-rule (record-type-name-binding-descriptor-value.rtd-id ?descriptor-value)
  (car ?descriptor-value))

(define-syntax-rule (record-type-name-binding-descriptor-value.rcd-id ?descriptor-value)
  (cadr ?descriptor-value))

(define-syntax-rule (record-type-name-binding-descriptor-value.spec ?descriptor-value)
  (cddr ?descriptor-value))

;;;

(module R6RS-RECORD-TYPE-SPEC
  (make-r6rs-record-type-spec
   r6rs-record-type-spec?
   record-type-name-binding-descriptor.safe-accessor
   record-type-name-binding-descriptor.safe-mutator
   record-type-name-binding-descriptor.unsafe-accessor
   record-type-name-binding-descriptor.unsafe-mutator)

  (define-record r6rs-record-type-spec
    (safe-accessors-table
		;Alist mapping all  field names to the identifiers to  which the safe
		;accessors are bound.
     safe-mutators-table
		;Alist mapping mutable  field names to the identifiers  to which safe
		;mutators are bound.
     unsafe-accessors-table
		;False or alist  mapping all field names to the  identifiers to which
		;the unsafe accessors are bound.
     unsafe-mutators-table
		;False or  alist mapping  mutable field names  to the  identifiers to
		;which unsafe mutators are bound.
     ))

  (define (record-type-name-binding-descriptor.safe-accessor binding-descriptor field-name-id synner)
    (%spec-actor binding-descriptor field-name-id r6rs-record-type-spec-safe-accessors-table 'record-accessor synner))

  (define (record-type-name-binding-descriptor.safe-mutator binding-descriptor field-name-id synner)
    (%spec-actor binding-descriptor field-name-id r6rs-record-type-spec-safe-mutators-table  'record-mutator synner))

  (define (record-type-name-binding-descriptor.unsafe-accessor binding-descriptor field-name-id synner)
    (%spec-actor binding-descriptor field-name-id r6rs-record-type-spec-unsafe-accessors-table 'unsafe-record-accessor synner))

  (define (record-type-name-binding-descriptor.unsafe-mutator binding-descriptor field-name-id synner)
    (%spec-actor binding-descriptor field-name-id r6rs-record-type-spec-unsafe-mutators-table  'unsafe-record-mutator synner))

  (define (%spec-actor binding-descriptor field-name-id table-getter actor-constructor synner)
    ;;Given  an  R6RS  record-type  name's   binding  descriptor  and  an  identifier
    ;;representing  a record's  field name:  return a  syntax object  representing an
    ;;expression which, expanded  and evaluated, returns the accessor  or mutator for
    ;;the named field.
    ;;
    ;;TABLE-GETTER must be a function which,  applied to the record spec, returns the
    ;;required association list.
    ;;
    ;;ACTOR-CONSTRUCTOR must be one  of the symbols: record-accessor, record-mutator,
    ;;unsafe-record-accessor, unsafe-record-mutator;  these are  the public  names of
    ;;the core primitives building the accessors and mutators.
    ;;
    (let ((field-name-sym (syntax->datum field-name-id))
	  (spec           (record-type-name-binding-descriptor.spec binding-descriptor)))
      (cond ((and (r6rs-record-type-spec? spec)
		  (assq field-name-sym (table-getter spec)))
	     => cdr)
	    (else
	     ;;Fallback  to   the  common  field  accessor   or  mutator
	     ;;constructor.
	     (let ((rtd-id (record-type-name-binding-descriptor.rtd-id binding-descriptor)))
	       (bless
		`(,actor-constructor ,rtd-id (quote ,field-name-sym))))))))

  #| end of module |# )

;;; --------------------------------------------------------------------
;;; fluid syntax bindings

(define-syntax-rule (make-syntactic-binding-descriptor/local-global-macro/fluid-syntax ?fluid-label)
  ;;Build and returnn a syntactic binding descriptor representing a fluid syntax; the
  ;;descriptor can be used to represent both a local and imported syntactic binding.
  ;;
  ;;?FLUID-LABEL is the label gensym that can  be used to redefine the binding.  With
  ;;the definition:
  ;;
  ;;   (define-fluid-syntax syn ?transformer)
  ;;
  ;;the identifier #'SRC  is bound to a  ?LABEL in the current rib  and the following
  ;;entry is pushed on the lexenv:
  ;;
  ;;   (?label . ?fluid-descriptor)
  ;;
  ;;where ?FLUID-DESCRIPTOR is the value returned by this macro; it has the format:
  ;;
  ;;   ($fluid . ?fluid-label)
  ;;
  ;;Another entry is pushed on the lexenv:
  ;;
  ;;   (?fluid-label . ?transformer-descriptor)
  ;;
  ;;representing the current macro transformer.
  ;;
  (make-syntactic-binding-descriptor $fluid ?fluid-label))

(define-syntactic-binding-descriptor-predicate fluid-syntax-binding-descriptor? $fluid)

(define-syntax-rule (fluid-syntax-binding-descriptor.fluid-label ?binding)
  (syntactic-binding-descriptor.value ?binding))

;;; --------------------------------------------------------------------
;;; synonym bindings

(define (make-syntactic-binding-descriptor/local-global-macro/synonym-syntax src-id)
  ;;Build  a  syntactic  binding  descriptor   representing  a  synonym  syntax;  the
  ;;descriptor can be used to represent  both a local and imported syntactic binding.
  ;;When successful return the descriptor; if an error occurs: raise an exception.
  ;;
  ;;SRC-ID is the identifier implementing the synonym.  With the definitions:
  ;;
  ;;   (define src 123)
  ;;   (define-syntax syn
  ;;     (make-synonym-transformer #'src))
  ;;
  ;;the argument SRC-ID is the identifier #'SRC.
  ;;
  ;;We build and return a descriptor with format:
  ;;
  ;;   ($synonym . ?label)
  ;;
  ;;where ?LABEL is the label gensym associated to SRC-ID.
  ;;
  (make-syntactic-binding-descriptor $synonym (id->label/or-error 'expander src-id src-id)))

(define-syntactic-binding-descriptor-predicate synonym-syntax-binding-descriptor? $synonym)

(define-syntax-rule (synonym-syntax-binding-descriptor.synonym-label ?binding)
  (syntactic-binding-descriptor.value ?binding))

;;; --------------------------------------------------------------------
;;; compile-time values bindings

(define (make-syntactic-binding-descriptor/local-macro/compile-time-value obj expanded-expr)
  ;;Build a  syntactic binding  descriptor representing  a local  compile-time value.
  ;;
  ;;OBJ  must  be  the  actual   object  computed  from  a  compile-time  expression.
  ;;EXPANDED-EXPR  must be  the core  language expression  representing the  original
  ;;right-hand side already expanded.
  ;;
  ;;Given the definition:
  ;;
  ;;   (define-syntax ctv
  ;;      (make-compile-time-value ?expr))
  ;;
  ;;the  argument OBJ  is the  return value  of expanding  and evaluating  ?EXPR; the
  ;;argument EXPANDED-EXPR  is the result of  expanding the whole right-hand  side of
  ;;the DEFINE-SYNTAX form.
  ;;
  (make-syntactic-binding-descriptor local-ctv (cons obj expanded-expr)))

(define-syntax-rule (local-compile-time-value-binding-descriptor.object ?descriptor)
  ;;Given a  syntactic binding  descriptor representing a  local compile  time value:
  ;;return the actual compile-time object.  We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (local-ctv . (?obj . ?expanded-expr))
  ;;
  ;;and we want to return ?OBJ.
  ;;
  (cadr ?descriptor))

;;; --------------------------------------------------------------------

;;Commented out  because unused, but kept  for reference.  (Marco Maggi;  Mon Apr 20,
;;2015)
;;
;; (define (make-syntactic-binding-descriptor/global-compile-time-value lib loc)
;;   ;;Build  and  return  a  syntactic  binding  descriptor  representing  an  imported
;;   ;;compile-time value.
;;   ;;
;;   ;;The argument LOC is a loc gensym.  The argument LIB is either: a "library" object
;;   ;;representing  the  library  from  which  the  binding  is  imported;  the  symbol
;;   ;;"*interaction*",  if  the  binding  was  established  by  a  previous  expression
;;   ;;evaluated in an interaction environment.
;;   ;;
;;   ;;The returned descriptor has format:
;;   ;;
;;   ;;   (global-ctv . (?library . ?loc))
;;   ;;
;;   (make-syntactic-binding-descriptor global-ctv (cons lib loc)))

(define-syntax-rule (global-compile-time-value-binding-descriptor.lib ?descriptor)
  (cadr ?descriptor))

(define-syntax-rule (global-compile-time-value-binding-descriptor.loc ?descriptor)
  (cddr ?descriptor))

(define (global-compile-time-value-binding-descriptor.object descriptor)
  ;;Given a syntactic binding descriptor representing an imported compile time value:
  ;;return the actual compile-time object.  We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (global-ctv . (?library . ?loc))
  ;;
  ;;where: ?LIBRARY is either an object of type "library" describing the library from
  ;;which the  binding is  imported or  the symbol "*interaction*";  ?LOC is  the log
  ;;gensym containing the actual object in its VALUE slot (but only after the library
  ;;has been visited).
  ;;
  (let ((lib (global-compile-time-value-binding-descriptor.lib descriptor))
	(loc (global-compile-time-value-binding-descriptor.loc descriptor)))
    ;;If this global binding use is the first  time a binding from LIB is used: visit
    ;;the library.   This makes  sure that  the actual  object is  stored in  the loc
    ;;gensym.
    (unless (eq? lib '*interaction*)
      (visit-library lib))
    ;;When the  library LIB  has been  loaded from  source: the  compile-time value's
    ;;object is stored in the loc gensym.   When the library LIB has been loaded from
    ;;a compiled file: the compile-time value itself is in the loc gensym, so we have
    ;;to extract it.
    (let ((ctv (symbol-value loc)))
      (if (compile-time-value? ctv)
	  (compile-time-value-object ctv)
	ctv))))

;;; --------------------------------------------------------------------
;;; module bindings

(define (make-syntactic-binding-descriptor/local-global-macro/module-interface iface)
  ;;Build and return a syntactic  binding descriptor representing a module interface;
  ;;the  descriptor can  be used  to represent  both a  local and  imported syntactic
  ;;binding.
  ;;
  ;;Given the definition:
  ;;
  ;;   (module ?name (?export ...) . ?body)
  ;;
  ;;the argument IFACE is an object of type "module-interface" representing the ?NAME
  ;;identifier and  the lexical  context of  the syntactic  bindings exported  by the
  ;;module.
  ;;
  (make-syntactic-binding-descriptor $module iface))

;;; --------------------------------------------------------------------
;;; pattern variable bindings

(define (make-syntactic-binding-descriptor/pattern-variable name ellipsis-nesting-level)
  ;;Build and return a syntactic  binding descriptor representing a pattern variable.
  ;;The descriptor can be used only to represent  a local binding: there is no way to
  ;;export a pattern variable binding.
  ;;
  ;;The  argument NAME  is the  source name  of the  pattern variable.   The argument
  ;;ELLIPSIS-NESTING-LEVEL is a non-negative fixnum representing the ellipsis nesting
  ;;level of the pattern variable in the pattern definition.
  ;;
  ;;The returned descriptor as format:
  ;;
  ;;   (pattern-variable . (?name . ?ellipsis-nesting-level))
  ;;
  (make-syntactic-binding-descriptor pattern-variable (cons name ellipsis-nesting-level)))

(define-syntactic-binding-descriptor-predicate pattern-variable-binding-descriptor? pattern-variable)


;;;; label gensym, lexical variable gensyms, storage location gensyms

(define* (generate-lexical-gensym seed)
  ;;Generate a unique symbol to represent the name of a lexical variable
  ;;in the core language forms.  Such  symbols have the purpose of being
  ;;unique in the core language  expressions representing a full library
  ;;or full program.
  ;;
  (if-wants-descriptive-gensyms
      (cond ((identifier? seed)
	     (gensym (string-append "lex." (symbol->string (~identifier->symbol seed)))))
	    ((symbol? seed)
	     (gensym (string-append "lex." (symbol->string seed))))
	    (else
	     (assertion-violation __who__
	       "expected symbol or identifier as argument" seed)))
    (cond ((identifier? seed)
	   (gensym (symbol->string (~identifier->symbol seed))))
	  ((symbol? seed)
	   (gensym (symbol->string seed)))
	  (else
	   (assertion-violation __who__
	     "expected symbol or identifier as argument" seed)))))

(define generate-storage-location-gensym
  ;;Build  and return  a gensym  to be  used as  storage location  for a
  ;;global lexical variable.  The "value" slot of such gensym is used to
  ;;hold the value of the variable.
  ;;
  (if-wants-descriptive-gensyms
      (case-lambda
       (()
	(gensym "loc.anonymous"))
       ((seed)
	(cond ((identifier? seed)
	       (gensym (string-append "loc." (symbol->string (~identifier->symbol seed)))))
	      ((symbol? seed)
	       (gensym (string-append "loc." (symbol->string seed))))
	      ((string? seed)
	       (gensym (string-append "loc." seed)))
	      (else
	       (gensym)))))
    ;;It is  really important to  use a  seeded gensym here,  because it
    ;;will show up in some error messages about unbound identifiers.
    (case-lambda
     (()
      (gensym))
     ((seed)
      (cond ((identifier? seed)
	     (gensym (symbol->string (~identifier->symbol seed))))
	    ((symbol? seed)
	     (gensym (symbol->string seed)))
	    ((string? seed)
	     (gensym seed))
	    (else
	     (gensym)))))))

(define (generate-label-gensym seed)
  ;;Every  syntactic binding  has a  label  associated to  it as  unique
  ;;identifier  in the  whole running  process; this  function generates
  ;;such labels as gensyms.
  ;;
  ;;Labels  must have  read/write  EQ?  invariance  to support  separate
  ;;compilation (when we write the expanded sexp to a file and then read
  ;;it back, the labels must not change and still be globally unique).
  ;;
  (if-wants-descriptive-gensyms
      (cond ((identifier? seed)
	     (gensym (string-append "lab." (symbol->string (~identifier->symbol seed)))))
	    ((symbol? seed)
	     (gensym (string-append "lab." (symbol->string seed))))
	    ((string? seed)
	     (gensym (string-append "lab." seed)))
	    (else
	     (gensym)))
    (gensym)))

(module (generate-or-retrieve-label-and-lex-gensyms
	 generate-or-retrieve-define-syntax-label-gensym)

  (define (generate-or-retrieve-label-and-lex-gensyms id rib shadow/redefine-bindings?)
    ;;Whenever a DEFINE syntax:
    ;;
    ;;   (define ?id ?rhs)
    ;;
    ;;is expanded  we need to generate  for it: a label  gensym, a lex gensym,  a loc
    ;;gensym.  This function  returns 2 values: the label gensym  and the lex gensym.
    ;;If the argument SHADOW/REDEFINE-BINDINGS? is true:  the lex gensym also acts as
    ;;loc gensym.
    ;;
    ;;The argument ID must be a syntactic identifier, either the name of a new DEFINE
    ;;binding or an already bound identifier for  which we want to retrieve the label
    ;;and lex.
    ;;
    ;;The argument RIB must be the rib object describing the lexical contour in which
    ;;the syntactic binding of ID must be established or resolved.
    ;;
    ;;The argument SHADOW/REDEFINE-BINDINGS? is interpreted as boolean:
    ;;
    ;;* When set to false: this function generates new label and lex gensyms.
    ;;
    ;;*  When set  to  true: if  the  RIB holds  a syntactic  binding  with the  same
    ;;source-name of  ID, the label and  lex of that binding  are returned; otherwise
    ;;new label and lex gensyms are generated.
    ;;
    ;;see the  documentation of CHI-BODY*  for the  full description of  the argument
    ;;SHADOW/REDEFINE-BINDINGS?.
    ;;
    (if shadow/redefine-bindings?
	(let ((label (%generate-or-retrieve-top-level-label-gensym id rib)))
	  (let* ((env           (top-level-context))
		 ;;LAB.LOC/LEX*  is an  alist having  label gensyms  as keys  and loc
		 ;;gensyms as values;  the loc gensyms are also used  as lex gensyms.
		 ;;It maps binding labels to  storage location gensyms in the context
		 ;;of an interaction environment.
		 (lab.loc/lex*  (interaction-env-lab.loc/lex* env)))
	    (values label
		    (cond ((assq label lab.loc/lex*)
			   => cdr)
			  (else
			   ;;There was no binding in RIB with the same source name of
			   ;;ID, so LABEL is a new  label gensym.  Here we generate a
			   ;;new lex gensym, also acting as loc gensym, and add it to
			   ;;the interaction environment.
			   (receive-and-return (loc)
			       (generate-storage-location-gensym id)
			     (set-interaction-env-lab.loc/lex*! env
			       (cons (cons label loc)
				     lab.loc/lex*))))))))
      (values (generate-label-gensym id) (generate-lexical-gensym id))))

  (define (generate-or-retrieve-define-syntax-label-gensym id rib shadow/redefine-bindings?)
    ;;Whenever a syntactic form:
    ;;
    ;;   (define-syntax       ?id ?rhs)
    ;;   (define-fluid-syntax ?id ?rhs)
    ;;
    ;;is expanded, we need to generate for  it: a label gensym, and that's all.  This
    ;;function returns the label.
    ;;
    ;;The argument ID must be an  identifier representing the name of a DEFINE-SYNTAX
    ;;or DEFINE-FLUID-SYNTAX binding.
    ;;
    ;;The argument RIB must be the rib object describing the lexical contour in which
    ;;the binding is created.
    ;;
    ;;The argument SHADOW/REDEFINE-BINDINGS? is interpreted as boolean:
    ;;
    ;;* When set to false: this function generates a new label gensym.
    ;;
    ;;*  When set  to  true: if  the  RIB holds  a syntactic  binding  with the  same
    ;;source-name of ID, the label of that binding is returned; otherwise a new label
    ;;gensym is generated.
    ;;
    ;;see the  documentation of CHI-BODY*  for the  full description of  the argument
    ;;SHADOW/REDEFINE-BINDINGS?.
    ;;
    (if shadow/redefine-bindings?
	(%generate-or-retrieve-top-level-label-gensym id rib)
      (generate-label-gensym id)))

  (define (%generate-or-retrieve-top-level-label-gensym id rib)
    (let ((id.source-name   (~identifier->symbol id))
	  (id.mark*         ($stx-mark* id))
	  (rib.source-name* ($rib-name* rib)))
      (cond ((and (memq id.source-name rib.source-name*)
		  (%find-label-of-source-name id.source-name id.mark* rib.source-name* ($rib-mark** rib) ($rib-label* rib)))
	     => (lambda (label)
		  ;;If we are  here RIB contains a binding with  the same source-name
		  ;;of ID and LABEL is its label.
		  ;;
		  ;;If LABEL is associated to an imported binding: the data structure
		  ;;implementing  the  symbol  object  holds  the  syntactic  binding
		  ;;descriptor in its  "value" field; otherwise such field  is set to
		  ;;false.
		  (if (label->imported-syntactic-binding-descriptor label)
		      ;;Create new label to shadow imported binding.
		      (generate-label-gensym id)
		    ;;Recycle old label.
		    label)))
	    (else
	     ;;Create a new label for a new syntactic binding.
	     (generate-label-gensym id)))))

  (define (%find-label-of-source-name source-name id.mark* rib.source-name* rib.mark** rib.label*)
    ;;Upon  entering a  call to  this  function: we  know  that the  list of  symbols
    ;;RIB.SOURCE-NAME* has at least one element equal to SOURCE-NAME.
    ;;
    ;;We recurse iterating over RIB.SOURCE-NAME*,  RIB.MARK** and RIB.LABEL*; we look
    ;;for a tuple having marks equal to ID.MARK* and return the associated label.  If
    ;;such binding is not found return false.
    ;;
    (and (pair? rib.source-name*)
	 (if (and (eq? source-name (car rib.source-name*))
		  (same-marks? id.mark* (car rib.mark**)))
	     (car rib.label*)
	   (%find-label-of-source-name source-name id.mark* (cdr rib.source-name*) (cdr rib.mark**) (cdr rib.label*)))))

  #| end of module |# )


;;;; lexical environment: mapping labels to syntactic binding descriptors

(module (label->syntactic-binding-descriptor)

  (case-define label->syntactic-binding-descriptor
    ;;Look up the  symbol LABEL in the  LEXENV as well as  in the global
    ;;environment.   If an  entry with  key LABEL  is found:  return the
    ;;associated syntactic  binding descriptor; if no  matching entry is
    ;;found, return one of the special descriptors:
    ;;
    ;;   (displaced-lexical . ())
    ;;   (displaced-lexical . #f)
    ;;
    ;;If the  binding descriptor  represents a  fluid syntax  or synonym
    ;;syntax: follow  through and return the  innermost re-definition of
    ;;the binding.
    ;;
    ((label lexenv)
     (label->syntactic-binding-descriptor label lexenv '()))
    ((label lexenv accum-labels)
     (let ((binding (label->syntactic-binding-descriptor/no-indirection label lexenv)))
       (cond ((fluid-syntax-binding-descriptor? binding)
	      ;;Fluid syntax  bindings (created  by DEFINE-FLUID-SYNTAX)
	      ;;require  different   logic.   The   lexical  environment
	      ;;contains the main fluid  syntax definition entry and one
	      ;;or more subordinate fluid syntax re-definition entries.
	      ;;
	      ;;LABEL  is  associated  to the  main  binding  definition
	      ;;entry;  its syntactic  binding  descriptor contains  the
	      ;;fluid  label,  which  is   associated  to  one  or  more
	      ;;subordinate re-definitions.  We  extract the fluid label
	      ;;from  BINDING,  then  search  for  the  innermost  fluid
	      ;;binding associated to it.
	      ;;
	      ;;Such  search for  the fluid  re-definition binding  must
	      ;;begin from  LEXENV, and then in  the global environment.
	      ;;This  is because  we can  nest at  will FLUID-LET-SYNTAX
	      ;;forms  that   redefine  the   binding  by   pushing  new
	      ;;re-definition entries  on the LEXENV.  To  reach for the
	      ;;innermost we must query the LEXENV first.
	      ;;
	      ;;If there  is no binding descriptor  for FLUID-LABEL: the
	      ;;return value will be:
	      ;;
	      ;;   (displaced-lexical . #f)
	      ;;
	      (let* ((fluid-label   (fluid-syntax-binding-descriptor.fluid-label binding))
		     (fluid-binding (cond ((assq fluid-label lexenv)
					   => lexenv-entry.binding-descriptor)
					  (else
					   (label->syntactic-binding-descriptor/no-indirection fluid-label '())))))
		(if (synonym-syntax-binding-descriptor? fluid-binding)
		    (%follow-through-synonym-descriptor binding lexenv accum-labels)
		  fluid-binding)))

	     ((synonym-syntax-binding-descriptor? binding)
	      (%follow-through-synonym-descriptor binding lexenv accum-labels))

	     (else
	      binding)))))

  (define (%follow-through-synonym-descriptor binding lexenv accum-labels)
    (let ((synonym-label (synonym-syntax-binding-descriptor.synonym-label binding)))
      (if (memq synonym-label accum-labels)
	  (syntax-violation #f "circular reference detected while resolving synonym transformers" #f)
	(label->syntactic-binding-descriptor synonym-label lexenv (cons synonym-label accum-labels)))))

  #| end of module |# )

(define (label->syntactic-binding-descriptor/no-indirection label lexenv)
  ;;Look up  the symbol  LABEL in the  LEXENV as well  as in  the global
  ;;environment.   If an  entry  with  key LABEL  is  found: return  the
  ;;associated  syntactic binding  descriptor; if  no matching  entry is
  ;;found, return one of the special descriptors:
  ;;
  ;;   (displaced-lexical . ())
  ;;   (displaced-lexical . #f)
  ;;
  ;;If the  binding descriptor  represents a fluid  syntax or  a synonym
  ;;syntax: *do not* follow through and return the binding descriptor of
  ;;the syntax definition.
  ;;
  ;;Since all labels are unique,  it doesn't matter which environment we
  ;;consult first; we  lookup the global environment  first because it's
  ;;faster.
  ;;
  (cond ((not label)
	 ;;If LABEL is the result of a previous call to ID->LABEL for an
	 ;;unbound  identifier: LABEL  is  false.  This  check makes  it
	 ;;possible to use the concise expression:
	 ;;
	 ;;   (define ?binding
	 ;;     (label->syntactic-binding-descriptor (id->label ?id) ?lexenv))
	 ;;
	 ;;provided that later we check for the type of ?BINDING.
	 '(displaced-lexical))

	;;If a label is associated to  a binding from the the boot image
	;;environment or  to a binding  from a library's  EXPORT-ENV: it
	;;has the associated descriptor  in its "value" field; otherwise
	;;such field is set to #f.
	;;
	;;So,  if we  have a  label, we  can check  if it  references an
	;;imported binding simply by checking its "value" field; this is
	;;what LABEL->IMPORTED-SYNTACTIC-BINDING-DESCRIPTOR does.
	;;
	((label->imported-syntactic-binding-descriptor label)
	 => (lambda (descriptor)
	      ;;The first time we access  a syntactic binding descriptor representing
	      ;;a core record-type name: we mutate it to a format usable by the code.
	      (when ($core-record-type-name-binding-descriptor? descriptor)
		(core-record-type-name-binding-descriptor->record-type-name-binding-descriptor! descriptor))
	      descriptor))

	;;Search the given LEXENV.
	;;
	((assq label lexenv)
	 => lexenv-entry.binding-descriptor)

	;;Search the interaction top-level environment, if any.
	;;
	((top-level-context)
	 => (lambda (env)
	      (cond ((assq label (interaction-env-lab.loc/lex* env))
		     => (lambda (lab.loc/lex)
			  ;;Fabricate a  binding descriptor representing
			  ;;an immutated  lexical variable.  We  need to
			  ;;remember that  for interaction environments:
			  ;;we  reuse  the  storage location  gensym  as
			  ;;lexical gensym.
			  (make-syntactic-binding-descriptor/lexical-var (cdr lab.loc/lex))))
		    (else
		     ;;Unbound label.
		     '(displaced-lexical . #f)))))

	;;Unbound label.
	;;
	(else
	 '(displaced-lexical . #f))))


;;;; marks of lexical contours

(define-syntax define-mark-generator
  (lambda (stx)
    (sys.syntax-case stx ()
      ((?kwd)
       (with-syntax
	   ((WHO (sys.datum->syntax (sys.syntax ?kwd) 'generate-new-mark)))
	 (if (option.descriptive-marks)
	     (begin
	       (fprintf (current-error-port) "vicare: enabled descriptive marks generation\n")
	       (sys.syntax
		(define WHO
		  ;;Generate a new  unique mark.  We want a new  string for every function
		  ;;call.
		  (let ((i 0))
		    (lambda ()
		      (set! i (+ i 1))
		      (string-append "mark." (number->string i)))))))
	   (begin
	     (fprintf (current-error-port) "vicare: enabled non-descriptive marks generation\n")
	     (sys.syntax
	      (define-syntax-rule (WHO)
		;;Generate a  new unique mark.   We want a  new string for  every function
		;;call.
		(string)))))))
      )))
(define-mark-generator)

;;We use #f as the anti-mark.
(define-constant anti-mark #f)

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

(define-record (rib make-rib rib?)
  (name*
		;List  of symbols  representing  the original  binding  names in  the
		;source code.
		;
		;When the RIB is sealed: the list is converted to a vector.

   mark**
		;List of  sublists of marks;  there is a  sublist of marks  for every
		;item in NAME*.
		;
		;When the RIB is sealed: the list is converted to a vector.

   label*
		;List of  label gensyms uniquely identifying  the syntactic bindings;
		;there is a label for each item in NAME*.
		;
		;When the RIB is sealed: the list is converted to a vector.

   sealed/freq
		;False  or  vector  of  exact  integers.  When  false:  this  RIB  is
		;extensible, that is new bindings can be added to it.  When a vector:
		;this RIB is  sealed; see the documentation in Texinfo  format for an
		;explanation of the frequency vector.
   )
  (lambda (S port subwriter) ;record printer function
    (define-syntax-rule (%display ?thing)
      (display ?thing port))
    (define-syntax-rule (%write ?thing)
      (write ?thing port))
    (define-syntax-rule (%pretty-print ?thing)
      (pretty-print* ?thing port 0 #f))
    (%display "#<rib")
    (%display " name*=")	(%pretty-print (rib-name*  S))
    (%display " mark**=")	(%pretty-print (rib-mark** S))
    (%display " label*=")	(%pretty-print (rib-label* S))
    (%display " sealed/freq=")	(%pretty-print (rib-sealed/freq S))
    (%display ">")))

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

(define-syntax-rule (make-empty-rib)
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
  ;;   (define rib (make-empty-rib))
  ;;   (define stx (push-lexical-contour rib stx*))
  ;;
  ;;and then hand the resulting syntax object STX to the appropriate "chi-*" function
  ;;to perform the expansion.  Later we can add bindings to the rib with:
  ;;
  ;;   (extend-rib! rib id label shadow/redefine-bindings?)
  ;;
  (make-rib '() '() '() #f))

(define (make-rib-from-identifiers-and-labels id* label*)
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
  ;;   (define rib        (make-rib-from-identifiers-and-labels lhs.id* lhs.label*))
  ;;   (define body.stx^  (push-lexical-contour rib body.stx))
  ;;
  ;;and then  hand the resulting syntax  object BODY.STX^ to the  appropriate "chi-*"
  ;;function to perform the expansion.
  ;;
  (let ((name*        (map identifier->symbol id*))
	(mark**       (map stx-mark* id*))
	(sealed/freq  #f))
    (make-rib name* mark** label* sealed/freq)))

(define (make-top-rib-from-source-names-and-labels source-name* lab*)
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

(define (make-top-rib-from-source-name-and-label source-name lab)
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
  (let ((name* ($rib-name* rib)))
    (unless (null? name*) ;only seal if RIB is not empty
      (let ((name* (list->vector name*)))
	($set-rib-name*!       rib name*)
	($set-rib-mark**!      rib (list->vector ($rib-mark** rib)))
	($set-rib-label*!      rib (list->vector ($rib-label* rib)))
	($set-rib-sealed/freq! rib (make-vector (vector-length name*) 0))))))

(define* (unseal-rib! {rib rib?})
  (when ($rib-sealed/freq rib)
    ($set-rib-sealed/freq! rib #f)
    ($set-rib-name*!       rib (vector->list ($rib-name*  rib)))
    ($set-rib-mark**!      rib (vector->list ($rib-mark** rib)))
    ($set-rib-label*!      rib (vector->list ($rib-label* rib)))))

;;; --------------------------------------------------------------------

(module (extend-rib!)
  ;;A RIB can be extensible, or  sealed.  Adding an identifier-to-label mapping to an
  ;;extensible RIB is achieved by prepending items to the field lists.
  ;;
  ;;For example, an empty extensible RIB has fields:
  ;;
  ;;   name*  = ()
  ;;   mark** = ()
  ;;   label* = ()
  ;;
  ;;adding a syntactic binding to it with source name "ciao", marks "(src)" and label
  ;;"lab.ciao" means mutating the fields to:
  ;;
  ;;   name*  = (ciao)
  ;;   mark** = ((src))
  ;;   label* = (lab.ciao)
  ;;
  ;;we can think of this as "pushing the syntactic binding's tuple":
  ;;
  ;;   { ciao, (src), lab.ciao }
  ;;
  ;;on the  rib.  Adding another binding  with source name "hello",  mark "(src)" and
  ;;label "lab.hello" means mutating the fields to:
  ;;
  ;;   name*  = (hello     ciao)
  ;;   mark** = ((src)     (src))
  ;;   label* = (lab.hello lab.ciao)
  ;;
  ;;As further example, let's consider the form:
  ;;
  ;;   (lambda ()
  ;;     (define a 1)
  ;;     (define b 2)
  ;;     (list a b))
  ;;
  ;;when starting to process  LAMBDA's internal body: a new rib  is created and added
  ;;to the  metadata of  the syntax  object representing the  body itself;  when each
  ;;internal definition is encountered, a new  entry for the identifier is added (via
  ;;side effect) to the rib:
  ;;
  ;;   name*  = (b       a)
  ;;   mark** = ((src)   (src))
  ;;   label* = (lab.b   lab.a)
  ;;
  ;;The order  in which the  binding tuples  appear in the  rib does not  matter: two
  ;;tuples are different when  both the symbol and the marks are  different and it is
  ;;an error to add twice a tuple to the same RIB.
  ;;
  ;;However,  it is  possible to  redefine  a syntactic  binding.  Let's  say we  are
  ;;evaluating forms  in an interaction  environment (for example: we  are evaluating
  ;;forms read from the REPL): in this case the argument SHADOW/REDEFINE-BINDINGS? is
  ;;set to true.  If we type:
  ;;
  ;;   vicare> (define a 1)
  ;;   vicare> (define a 2)
  ;;
  ;;after the first DEFINE is parsed the tuples are:
  ;;
  ;;   name*  = (a)
  ;;   mark** = ((src))
  ;;   label* = (lab.a.1)
  ;;
  ;;and after the secon DEFINE is parsed the tuples are:
  ;;
  ;;   name*  = (a)
  ;;   mark** = ((src))
  ;;   label* = (lab.a.2)
  ;;
  ;;we see that the label has changed.
  ;;
  ;;If the  argument SHADOW/REDEFINE-BINDINGS?  is  false: the new  syntactic binding
  ;;must    have   unique    source-name   in    this   RIB.     If   the    argument
  ;;SHADOW/REDEFINE-BINDINGS?   is true:  the  new syntactic  binding  is allowed  to
  ;;redefine an existing syntactic binding with  the same source-name in this RIB, by
  ;;replacing  the  original label  gensym  with  LABEL.   See the  documentation  of
  ;;CHI-BODY* for the full description of the argument SHADOW/REDEFINE-BINDINGS?.
  ;;
  ;;It is responsibility of this function to raise an exception if an illegal attempt
  ;;to redefine or shadow a binding is performed.
  ;;
  (define* (extend-rib! {rib rib?} {id identifier?} label shadow/redefine-bindings?)
    (when ($rib-sealed/freq rib)
      (assertion-violation/internal-error __who__
	"attempt to extend sealed RIB" rib))
    (let ((id.source-name  (~identifier->symbol id))
	  (id.mark*        ($stx-mark*  id))
	  (rib.name*       ($rib-name*  rib))
	  (rib.mark**      ($rib-mark** rib))
	  (rib.label*      ($rib-label* rib)))
      (cond ((%find-binding-with-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*)
	     ;;A binding for ID already exists in this lexical contour.  For example,
	     ;;in an internal body we have:
	     ;;
	     ;;   (define a 1)
	     ;;   (define a 2)
	     ;;
	     ;;* In an R6RS program or library: we must raise an exception.
	     ;;
	     ;;* In the  context of an interaction environment: we  just redefine the
	     ;;  binding.
	     ;;
	     => (lambda (tail-of-label*)
		  (unless (eq? label (car tail-of-label*))
		    (if shadow/redefine-bindings?
			;;We override the already existent label with the new label.
			(set-car! tail-of-label* label)
		      ;;Signal an error if the identifier was already in the rib.
		      (syntax-violation 'expander "multiple definitions of identifier" id)))))
	    (else
	     ;;No binding exists for ID in this lexical contour: create a new one.
	     ($set-rib-name*!  rib (cons id.source-name  rib.name*))
	     ($set-rib-mark**! rib (cons id.mark*        rib.mark**))
	     ($set-rib-label*! rib (cons label           rib.label*))))))

  (define-syntax-rule (%find-binding-with-same-marks id.source-name id.mark*
						     rib.name* rib.mark** rib.label*)
    (and (memq id.source-name rib.name*)
	 (%find id.source-name id.mark* rib.name* rib.mark** rib.label*)))

  (define (%find id.source-name id.mark* rib.name* rib.mark** rib.label*)
    ;;Here we know that the list of  symbols RIB.NAME* has at least one element equal
    ;;to ID.SOURCE-NAME; we iterate through  RIB.NAME*, RIB.MARK** and RIB.LABEL* looking for
    ;;a tuple having name equal to ID.SOURCE-NAME  and marks equal to ID.MARK* and return the
    ;;tail of RIB.LABEL* having the associated label  as car.  If such binding is not
    ;;found return false.
    ;;
    (and (pair? rib.name*)
	 (if (and (eq? id.source-name ($car rib.name*))
		  (same-marks? id.mark* ($car rib.mark**)))
	     rib.label*
	   (%find id.source-name id.mark* ($cdr rib.name*) ($cdr rib.mark**) ($cdr rib.label*)))))

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
  (let ((name*  ($rib-name*  rib))
	(mark** ($rib-mark** rib)))
    (if ($rib-sealed/freq rib)
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
    (add-mark anti-mark #f input-form-stx #f))

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

		   ((eq? (car expr.mark*) anti-mark)
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

(define-record stx
  (expr
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
   )
  (lambda (S port subwriter) ;record printer function
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
    (%display " expr=")		(%pretty-print raw-expr)
    (%display " mark*=")	(%pretty-print (stx-mark* S))
    (let ((expr (stx-expr S)))
      (when (annotation? expr)
	(let ((pos (annotation-textual-position expr)))
	  (when (source-position-condition? pos)
	    (%display " line=")		(%display (source-position-line    pos))
	    (%display " column=")	(%display (source-position-column  pos))
	    (%display " source=")	(%display (source-position-port-id pos))))))
    (%display ">")))

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
  (wrap-source-expression sym (make-top-rib-from-source-name-and-label sym lab)))

(define* (make-syntactic-identifier-for-temporary-variable {sym symbol?})
  ;;Build and return a  new src marked syntactic identifier to  be used for temporary
  ;;variables.   The returned  identifier can  be an  item in  the list  generated by
  ;;GENERATE-TEMPORARIES.
  ;;
  (make-stx sym SRC-MARK* '() '()))

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

(module (id->label)

  (define* (id->label {id identifier?})
    ;;Given  the syntactic  identifier ID  search its  ribs for  a syntactic  binding
    ;;having the  same source-name  and marks.  If  successful: return  the syntactic
    ;;binding's label gensym; otherwise return false.
    ;;
    (define id.source-name (~identifier->symbol id))
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
	       (if ($rib-sealed/freq rib)
		   (%search-in-rib/sealed rib id.source-name mark* search-in-next-rib)
		 (%search-in-rib/non-sealed rib id.source-name mark* search-in-next-rib)))))))

  (define-syntax-rule (same-name? x y)
    (eq? x y))

  (define (%search-in-rib/non-sealed rib id.source-name id.mark* search-in-next-rib)
    (let loop ((rib.source-name* ($rib-name*  rib))
	       (rib.mark**       ($rib-mark** rib))
	       (rib.label*       ($rib-label* rib)))
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
    (define rib.source-name* ($rib-name*  rib))
    (define rib.mark**       ($rib-mark** rib))
    (define rib.label*       ($rib-label* rib))
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
    (unless (fxzero? src.idx)
      (let* ((rib.freq* (rib-sealed/freq rib))
	     (freq      (vector-ref rib.freq* src.idx))
	     ;;Search for the leftmost slot, starting from SRC.IDX, that has the same
	     ;;freq of the slot at SRC.IDX.
	     (dst.idx   (let loop ((i src.idx))
			  (if (fxzero? i)
			      0
			    (let ((j (fxsub1 i)))
			      (if (fx= freq (vector-ref rib.freq* j))
				  ;;The  freq of  the slot  previous to  I is  equal:
				  ;;loop.
				  (loop j)
				;;The  freq of  the slot  previous to  I is  greater:
				;;accept I as swap position.
				i))))))
	;;Rather than swapping the slots DST.IDX and SRC.IDX in the frequency vector:
	;;we just increment the dst slot.
	(vector-set! rib.freq* dst.idx (fxadd1 freq))
	(unless (fx= dst.idx src.idx)
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

(define (id->label/intern id)
  ;;Given  the identifier  ID  search  the lexical  environment  for  a binding  that
  ;;captures it:
  ;;
  ;;* If a capturing binding is found: return the associated label.
  ;;
  ;;*  If no  capturing binding  is found  and the  top-level environment  is not  an
  ;;interaction environment: return false.
  ;;
  ;;*  If  no  capturing  binding  is  found but  the  top-level  environment  is  an
  ;;interaction  environment: fabricate  a new  syntactic binding  in such  top-level
  ;;environment so  that there  exists a  lex gensym to  name the  binding and  a loc
  ;;gensym in  which to  store a value  (actually the  lex and the  loc are  the same
  ;;gensym).
  ;;
  ;;The syntactic binding fabrication allows us  to write "special" code on the REPL,
  ;;for example:
  ;;
  ;;   vicare> (set! a 1)
  ;;
  ;;when A is not  defined will not fail, rather it will  implicitly define a binding
  ;;as if we had typed:
  ;;
  ;;   vicare> (define a)
  ;;   vicare> (set! a 1)
  ;;
  ;;another example of weird code that will not fail at the REPL:
  ;;
  ;;   vicare> (let ()
  ;;             (set! a 1)
  ;;             (debug-print a))
  ;;
  ;;will just print A as if we had typed:
  ;;
  ;;   vicare> (let ()
  ;;             (define a)
  ;;             (set! a 1)
  ;;             (debug-print a))
  ;;
  ;;fabricating the lexical binding is like injecting the syntax "(define id)".
  ;;
  (or (id->label id)
      ;;If we are here we know that  no capturing binding was found: either fabricate
      ;;a new binding or return false.
      (cond ((top-level-context)
	     => (lambda (env)
		  (let ((rib (interaction-env-rib env)))
		    (receive (lab unused-lex/loc)
			;;Here  we  know  that  we  are trying  to  map  a  syntactic
			;;identifier  to  its  label  gensym in  the  context  of  an
			;;interaction environment.
			;;
			;;If  a  syntactic  binding in  the  interaction  environment
			;;captures  ID:  we  retrieve  its label.   Otherwise  a  new
			;;binding is added to the interaction environment.
			(let ((shadow/redefine-bindings? #t))
			  (generate-or-retrieve-label-and-lex-gensyms id rib shadow/redefine-bindings?))
		      ;;FIXME (Abdulaziz Ghuloum)
		      (let ((shadow/redefine-bindings? #f))
			(extend-rib! rib id lab shadow/redefine-bindings?))
		      lab))))
	    (else #f))))

;;; --------------------------------------------------------------------

(define (id->label/or-error who input-form.stx id)
  (or (id->label id)
      (raise-unbound-error who input-form.stx id)))

(define (id->record-type-name-binding-descriptor who input-form.stx type-name-id lexenv)
  ;;TYPE-NAME-ID is meant  to be a syntactic identifier bound  to an R6RS record-type
  ;;name  binding descriptor;  retrieve its  label then  its binding  descriptor from
  ;;LEXENV, finally return the binding descriptor.
  ;;
  ;;If  TYPE-NAME-ID is  unbound: raise  an "unbound  identifier" exception.   If the
  ;;syntactic binding descriptor does not represent an R6RS record-type name: raise a
  ;;syntax violation exception.
  ;;
  (let* ((label (id->label/or-error who input-form.stx type-name-id))
	 (descr (label->syntactic-binding-descriptor label lexenv)))
    (if (record-type-name-binding-descriptor? descr)
	descr
      (syntax-violation who
	"identifier not bound to a record type descriptor"
	input-form.stx type-name-id))))


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

(define* (scheme-stx {sym symbol?})
  ;;Take a symbol and if it's the public  name of a syntactic binding exported by the
  ;;library "(psyntax system  $all)": create a fresh identifier that  maps the symbol
  ;;to its label in that library.  Symbols not in that library become fresh.
  ;;
  (or (getprop sym system-id-gensym)
      (getprop sym '*vicare-scheme-temporary-variable-id*)
      (cond ((system-label sym)
	     ;;SYM is the  name of a core  primitive, so we build  a bound identifier
	     ;;with a proper "rib" and  the binding's label.  Such bound identifier
	     ;;will be captured by the entry in the top-level environment.
	     => (lambda (label)
		  (receive-and-return (id)
		      (make-top-level-syntactic-identifier-from-source-name-and-label sym label)
		    (putprop sym system-id-gensym id))))
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
  (or (getprop sym system-id-gensym)
      (cond ((system-label sym)
	     ;;SYM is the  name of a core  primitive, so we build  a bound identifier
	     ;;with a proper "rib" and  the binding's label.  Such bound identifier
	     ;;will be captured by the entry in the top-level environment.
	     => (lambda (label)
		  (receive-and-return (id)
		      (make-top-level-syntactic-identifier-from-source-name-and-label sym label)
		    (putprop sym system-id-gensym id))))
	    (else
	     (assertion-violation __who__ "invalid core primitive symbol name" sym)))))

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

(define (jolly-id? id)
  ;;Return  true if  ID is  a  syntactic identifier  that  could be  captured by  the
  ;;primitive binding "_" or "<>"; otherwise return false.
  ;;
  (and (identifier? id)
       (or (~free-identifier=? id (underscore-id))
	   (~free-identifier=? id (place-holder-id)))))


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
  (and (eq? (~identifier->symbol id1) (~identifier->symbol id2))
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
      (eq? (~identifier->symbol id1)
	   (~identifier->symbol id2)))))

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
  (~identifier->symbol x))

(define (~identifier->symbol x)
  (let ((expr ($stx-expr x)))
    (if (annotation? expr)
	(annotation-stripped expr)
      expr)))


;;;; identifiers: syntax parameters

(define current-run-lexenv
  ;;This parameter holds  a function which is meant to  return the value
  ;;of LEXENV.RUN while a macro is being expanded.
  ;;
  ;;The default value  will return null, which represents an  empty LEXENV; when such
  ;;value is used with LABEL->SYNTACTIC-BINDING-DESCRIPTOR: the mapping label/binding
  ;;is performed only in the top-level environment.
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
	(let ((binding (label->syntactic-binding-descriptor label ((current-run-lexenv)))))
	  (case (syntactic-binding-descriptor.type binding)
	    ((local-ctv)
	     (local-compile-time-value-binding-descriptor.object binding))

	    ((global-ctv)
	     (global-compile-time-value-binding-descriptor.object binding))

	    (else
	     (procedure-argument-violation __who__
	       "expected identifier bound to compile-time value"
	       id))))
      (procedure-argument-violation __who__
	"unbound identifier" id))))


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

(define (bound-id-member? id id*)
  ;;Given  an identifier  ID  and a  list  of identifiers  ID*: return  #t  if ID  is
  ;;~BOUND-IDENTIFIER=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (~bound-identifier=? id ($car id*))
	   (bound-id-member? id ($cdr id*)))))

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
					(assertion-violation __who__ "BUG"))))
			    str*)))))


;;;; errors helpers

(define (retvals-signature? x)
  ;;FIXME This  is defined  here to  nothing to avoid  sharing contexts  between this
  ;;library and the  ones that defines the  correct version.  It is to  be removed in
  ;;some future.  Maybe at  the next boot image rotation.  (Marco  Maggi; Sat Apr 11,
  ;;2015)
  ;;
  #t)

(define (expression-position x)
  (if (stx? x)
      (let ((x (stx-expr x)))
	(if (annotation? x)
	    (annotation-textual-position x)
	  (condition)))
    (condition)))


;;;; condition object types: descriptive objects

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

(define* (make-retvals-signature-condition {sig retvals-signature?})
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

(define* (make-expand-time-retvals-signature-violation {expected-signature retvals-signature?}
						       {returned-signature retvals-signature?})
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

  (case-define syntax-violation
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
    ((who msg form)
     (syntax-violation who msg form #f))
    ((who msg form subform)
     (raise-compound-condition-object who msg form (make-syntax-violation form subform))))

  (define* (expand-time-retvals-signature-violation source-who form subform
						    {expected-retvals-signature retvals-signature?}
						    {returned-retvals-signature retvals-signature?})
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

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'let-syntax-rules		'scheme-indent-function 1)
;; End:
