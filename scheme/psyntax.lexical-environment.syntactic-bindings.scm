;;; -*- coding: utf-8-unix -*-
;;;
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


(module PSYNTAX-SYNTACTIC-BINDINGS
    (
;;; local lexical variables, standard variant
     make-syntactic-binding-descriptor/lexical-var
     syntactic-binding-descriptor/lexical-var?
     lexical-var-binding-descriptor-value.lex-name
     lexical-var-binding-descriptor-value.assigned?
     lexenv-add-lexical-var-binding
     lexenv-add-lexical-var-bindings

;;; local lexical variables, typed variant
     make-syntactic-binding-descriptor/lexical-typed-var
     make-syntactic-binding-descriptor/lexical-typed-var/from-data
     syntactic-binding-descriptor/lexical-typed-var?
     syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec

;;; global lexical variables, typed variant
     make-global-typed-variable-spec-and-maker-core-expr
     syntactic-binding-descriptor/global-typed-var?
     syntactic-binding-descriptor/global-typed-var.typed-variable-spec

;;; local macro with non-variable transformer bindings
     make-syntactic-binding-descriptor/local-macro/non-variable-transformer
     syntactic-binding-descriptor/local-macro/non-variable-transformer?

;;; local macro with variable transformer bindings
     make-syntactic-binding-descriptor/local-macro/variable-transformer
     syntactic-binding-descriptor/local-macro/variable-transformer?

;;; base object-type descriptors
     make-syntactic-binding-descriptor/object-type-name
     syntactic-binding-descriptor/object-type-name?
     syntactic-binding-descriptor/local-object-type.object-type-spec
     syntactic-binding-descriptor/local-object-type.expanded-expr
     syntactic-binding-descriptor/global-object-type.library
     syntactic-binding-descriptor/global-object-type.loc
     syntactic-binding-descriptor/global-object-type.object-type-spec
     syntactic-binding-descriptor/core-object-type.object-type-spec
     syntactic-binding-descriptor/object-type-spec.ots

;;; list sub-type binding
     syntactic-binding-descriptor/list-of-type-name?

;;; vector sub-type binding
     syntactic-binding-descriptor/vector-of-type-name?

;;; hard-coded core primitive with type signature binding
     hard-coded-typed-core-prim-binding-descriptor->type-core-prim-binding-descriptor!
     syntactic-binding-descriptor/hard-coded-typed-core-prim?

;;; core primitive with type signature binding
     syntactic-binding-descriptor/core-prim-typed?
     core-prim-typed-binding-descriptor.core-prim-type-spec

;;; core built-in object-type descriptor binding
     hard-coded-core-scheme-type-name-symbolic-binding-descriptor->core-scheme-type-name-binding-descriptor!
     syntactic-binding-descriptor/hard-coded-core-scheme-type-name?

;;; Vicare struct-type name bindings
     syntactic-binding-descriptor/struct-type-name?
     struct-type-name-binding-descriptor.type-descriptor

;;; usable R6RS record-type descriptor binding
     syntactic-binding-descriptor/record-type-name?

;;; hard-coded core R6RS record-type descriptor binding
     hard-coded-core-record-type-name-binding-descriptor->core-record-type-name-binding-descriptor!
     syntactic-binding-descriptor/hard-coded-core-record-type-name?

;;; core R6RS condition object record-type descriptor binding
     hard-coded-core-condition-object-type-name-binding-descriptor->core-record-type-name-binding-descriptor!
     syntactic-binding-descriptor/hard-coded-core-condition-object-type-name?

;;; hard-coded type annotation descriptor binding
     hard-coded-core-type-annotation-symbolic-binding-descriptor->core-type-annotation-binding-descriptor!
     syntactic-binding-descriptor/hard-coded-type-annotation?

;;; fluid syntax bindings
     make-syntactic-binding-descriptor/local-global-macro/fluid-syntax
     syntactic-binding-descriptor/fluid-syntax?
     fluid-syntax-binding-descriptor.fluid-label

;;; synonym bindings
     make-syntactic-binding-descriptor/local-global-macro/synonym-syntax
     syntactic-binding-descriptor/synonym-syntax?
     synonym-syntax-binding-descriptor.synonym-label

;;; compile-time values bindings
     make-syntactic-binding-descriptor/local-macro/expand-time-value
     local-expand-time-value-binding-descriptor.object
     global-expand-time-value-binding-descriptor.lib
     global-expand-time-value-binding-descriptor.loc
     global-expand-time-value-binding-descriptor.object

;;; module bindings
     make-syntactic-binding-descriptor/local-global-macro/module-interface

;;; pattern variable bindings
     make-syntactic-binding-descriptor/pattern-variable
     syntactic-binding-descriptor/pattern-variable?

     #| end of exports |# )

(import PSYNTAX-TYPE-SIGNATURES)
(import PSYNTAX-TYPE-CALLABLES)


;;; helpers

(define-syntax define-syntactic-binding-descriptor-predicate
  (syntax-rules ()
    ((_ ?who ?type)
     (define (?who obj)
       ;;Return  true  if OBJ  is  a  syntactic  binding  descriptor of  type  ?TYPE;
       ;;otherwise return false.
       ;;
       (and (pair? obj)
	    (eq?  (syntactic-binding-descriptor.type obj)
		  (quote ?type)))))
    ((_ ?who ?type0 ?type1 ?type ...)
     (define (?who obj)
       ;;Return  true  if OBJ  is  a  syntactic  binding  descriptor of  type  ?TYPE;
       ;;otherwise return false.
       ;;
       (and (pair? obj)
	    (memq (syntactic-binding-descriptor.type obj)
		  (quote (?type0 ?type1 ?type ...))))))
    ))

(define (%alist-ref-or-null ell idx)
  ;;Traverse the proper  list ELL until the contained object  at zero-based index IDX
  ;;is found; such obect is meant to be an alist with format:
  ;;
  ;;   ((?name . ?applicable-sexp) ...)
  ;;
  ;;where ?APPLICABLE-SEXP  is a symbolic  expression, to be BLESSed,  representing a
  ;;Scheme  expression   which,  expanded  and   evaluated,  returns  one   among:  a
  ;;record-type's  field  accessor, a  record-type's  field  mutator, an  object-type
  ;;method implementation function.
  ;;
  ;;Build and return an alist with the format:
  ;;
  ;;   ((?name . ?applicable-stx) ...)
  ;;
  ;;where ?APPLICABLE-STX is the result of applying BLESS to ?APPLICABLE-SEXP.
  ;;
  ;;If ELL is too short to have an item at index IDX: return null (as empty alist).
  ;;
  (cond ((null? ell)
	 '())
	((fxzero? idx)
	 (if (pair? ell)
	     (map (lambda (P)
		    (cons (car P) (bless (cdr P))))
	       (car ell))
	   '()))
	(else
	 (%alist-ref-or-null (cdr ell) (fxsub1 idx)))))


;;;; syntactic binding descriptor: lexical variables

(define (make-syntactic-binding-descriptor/lexical-var lex-name)
  ;;Build and return  a syntactic binding descriptor representing  an untyped lexical
  ;;variable.   The returned  descriptor marks  this variable  as non-assigned;  this
  ;;state can be  changed later.  LEX-NAME must be a  lexical gensym representing the
  ;;name of a lexical variable in the expanded language forms.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (lexical . (?lex-name . #f))
  ;;
  (make-syntactic-binding-descriptor lexical (cons lex-name #f)))

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/lexical-var?
  lexical)

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
  (push-entry-on-lexenv label (make-syntactic-binding-descriptor/lexical-var lex)
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
      (lexenv-add-lexical-var-bindings (cdr label*) (cdr lex*)
				       (lexenv-add-lexical-var-binding (car label*) (car lex*) lexenv))
    lexenv))


;;;; syntactic binding descriptor: typed lexical variables
;;
;;A  syntactic binding  representing a  typed  lexical variable  has descriptor  with
;;format:
;;
;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
;;
;;where ?EXPANDED-EXPR is  a core language expression which,  when evaluated, returns
;;rebuilds the record of type "<lexical-typed-variable-spec>".
;;

(define* (make-syntactic-binding-descriptor/lexical-typed-var {lts lexical-typed-variable-spec?} lts.core-expr)
  ;;Build  and return  a syntactic  binding descriptor  representing a  typed lexical
  ;;variable.  LTS is  the specification of the variable's  type.  LTS.CORE-EXPR must
  ;;be  a core  language  expression  which, evaluated  at  expand-time, returns  LTS
  ;;itself.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
  ;;
  ;;and it is similar to the one of a local macro.  Indeed syntactic bindings of this
  ;;type are similar to identifier macros.
  ;;
  (make-syntactic-binding-descriptor lexical-typed (cons lts lts.core-expr)))

(define* (make-syntactic-binding-descriptor/lexical-typed-var/from-data {variable.ots object-type-spec?} {lex gensym?})
  ;;The OTS  "<untyped>" is for  internal use, so  we do not  want to create  a typed
  ;;variable with "<untyped>" OTS: so we normalise it to "<top>".
  (let* ((variable.ots		(if (<untyped>-ots? variable.ots)
				    (<top>-ots)
				  variable.ots))
	 (lts			(make-lexical-typed-variable-spec variable.ots lex))
	 (lts-maker.core-expr	(build-application no-source
				  (build-primref no-source 'make-lexical-typed-variable-spec)
				  (list (build-data no-source (object-type-spec.name variable.ots))
					(build-data no-source lex)))))
    (make-syntactic-binding-descriptor/lexical-typed-var lts lts-maker.core-expr)))

;;; --------------------------------------------------------------------

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/lexical-typed-var?
  lexical-typed)

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec ?descriptor)
  ;;Extract the  record of  type "<typed-variable-spec>"  from a  syntactic binding's
  ;;descriptor of type "lexical-typed".
  ;;
  (cadr ?descriptor))


;;;; syntactic binding descriptor: global typed lexical variables
;;
;;A syntactic binding  representing a global typed lexical  variable has descriptor
;;with format:
;;
;;   (global-typed         . (#<library> . ?loc))
;;   (global-typed-mutable . (#<library> . ?loc))
;;
;;where ?LOC is a loc gensym holding an instance of "<global-typed-variable-spec>".
;;

(define* (make-global-typed-variable-spec-and-maker-core-expr {variable.ots object-type-spec?} variable.loc)
  (let* ((variable.gts	(make-global-typed-variable-spec variable.ots variable.loc))
	 (core-expr	(build-application no-source
			  (build-primref no-source 'make-global-typed-variable-spec)
			  (list (build-data no-source variable.ots)
				(build-data no-source variable.loc)))))
    (values variable.gts core-expr)))

;;; --------------------------------------------------------------------

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/global-typed-var?
  global-typed global-typed-mutable)

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/global-typed-var.typed-variable-spec ?descriptor)
  ;;Extract the  record of  type "<typed-variable-spec>"  from a  syntactic binding's
  ;;descriptor of type "global-typed" or "global-typed-mutable".
  ;;
  (symbol-value (cddr ?descriptor)))


;;;; syntactic binding descriptor: local macro with non-variable transformer bindings

(define (make-syntactic-binding-descriptor/local-macro/non-variable-transformer transformer expanded-expr)
  ;;Build  and return  a syntactic  binding descriptor  representing a  local keyword
  ;;syntactic binding with non-variable transformer.
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
  ;;generated when the return value of ?RHS is a closure object.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (local-macro . (?transformer . ?expanded-expr))
  ;;
  (make-syntactic-binding-descriptor local-macro (cons transformer expanded-expr)))

;;Return true if  the argument is a syntactic binding  descriptor representing alocal
;;keyword syntactic binding with variable transformer.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/local-macro/non-variable-transformer?
  local-macro)


;;;; syntactic binding descriptor: local macro with variable transformer bindings

(define (make-syntactic-binding-descriptor/local-macro/variable-transformer transformer expanded-expr)
  ;;Build  and return  a syntactic  binding descriptor  representing a  local keyword
  ;;syntactic binding with variable transformer.
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
  ;;   (local-macro! . (?transformer . ?expanded-expr))
  ;;
  (make-syntactic-binding-descriptor local-macro! (cons transformer expanded-expr)))

;;Return true if  the argument is a syntactic binding  descriptor representing alocal
;;keyword syntactic binding with variable transformer.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/local-macro/variable-transformer?
  local-macro!)


;;;; base object-type syntactic binding descriptors

(define-syntax-rule (define-syntactic-binding-descriptor-predicate/object-type-spec ?who ?pred)
  (define (?who obj)
    ;;Return  true  if  OBJ  is  a syntactic  binding's  descriptor  representing  an
    ;;object-type  specification; otherwise  return false.   We expect  the syntactic
    ;;binding's descriptor to have one of the formats:
    ;;
    ;;   (core-object-type-name   . (#<object-type-spec> . ?symbolic-expr))
    ;;   (local-object-type-name  . (#<object-type-spec> . ?expanded-expr))
    ;;   (global-object-type-name . (#<library> . ?loc))
    ;;
    ;;where ?LOC  is a  loc gensym  containing in its  VALUE slot  a reference  to an
    ;;instance of "<object-type-spec>".
    ;;
    (and (pair? obj)
	 (let ((descr.value (syntactic-binding-descriptor.value obj)))
	   (case (syntactic-binding-descriptor.type obj)
	     ((local-object-type-name)
	      (?pred (syntactic-binding-descriptor/local-object-type.object-type-spec  obj)))
	     ((global-object-type-name)
	      (?pred (syntactic-binding-descriptor/global-object-type.object-type-spec obj)))
	     ((core-object-type-name)
	      (?pred (syntactic-binding-descriptor/core-object-type.object-type-spec obj)))
	     (else #f))))))

(define* (make-syntactic-binding-descriptor/object-type-name {ots object-type-spec?} rhs.core)
  ;;Given an instance  of "<object-type-spec>" and the core  language expression used
  ;;to build it:  build and return a syntactic binding's  descriptor representing the
  ;;type.  This  function is used to  define syntactic bindings associated  with type
  ;;identifiers.
  ;;
  ;;Examples:
  ;;
  ;;* For Vicare's structs, the definition:
  ;;
  ;;   (define-struct ?type-name ...)
  ;;
  ;;expands into:
  ;;
  ;;   (define-syntax ?type-name (make-struct-type-spec ...))
  ;;
  ;;and the syntactic identifier ?TYPE-NAME is associated to the descriptor:
  ;;
  ;;   (local-object-type-name . (#<struct-type-spec> . ?rhs.core))
  ;;
  ;;* For R6RS records, the definition:
  ;;
  ;;   (define-record-type ?type-name ...)
  ;;
  ;;expands into:
  ;;
  ;;   (define-syntax ?type-name (make-record-type-spec ...))
  ;;
  ;;and the syntactic identifier ?TYPE-NAME is associated to the descriptor:
  ;;
  ;;   (local-object-type-name . (#<record-type-spec> . ?rhs.core))
  ;;
  ;;* For homogeneous list types, the definition:
  ;;
  ;;   (define-type <list-of-fixnums> (list-of <fixnum>))
  ;;
  ;;expands into:
  ;;
  ;;   (define-syntax <list-of-fixnums>
  ;;     (make-list-of-type-spec #'<fixnum> #'<list-of-fixnums>))
  ;;
  ;;and the syntactic identifier "<list-of-fixnums>" is associated to the descriptor:
  ;;
  ;;   (local-object-type-name . (#<list-of-type-spec> . ?rhs.core))
  ;;
  (make-syntactic-binding-descriptor local-object-type-name (cons ots rhs.core)))

;;Return true  if the argument  is a  syntactic binding's descriptor  representing an
;;object-type specification; otherwise return  false.  Syntactic identifiers bound to
;;such syntactic  binding descriptors  can be  used with  the generic  syntaxes: NEW,
;;IS-A?, SLOT-REF, SLOT-SET!, METHOD-CALL.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-binding-descriptor/object-type-name?
  object-type-spec?)

(define* (syntactic-binding-descriptor/object-type-spec.ots descr)
  (case (syntactic-binding-descriptor.type descr)
    ((core-object-type-name)
     (syntactic-binding-descriptor/core-object-type.object-type-spec descr))
    ((local-object-type-name)
     (syntactic-binding-descriptor/local-object-type.object-type-spec descr))
    ((global-object-type-name)
     (syntactic-binding-descriptor/global-object-type.object-type-spec descr))
    (else
     (assertion-violation __who__
       "internal error in object-type syntactic binding's format"
       descr))))

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/local-object-type.object-type-spec ?descriptor)
  ;;We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (local-object-type-name . (#<object-type-spec> . ?expanded-expr))
  ;;
  ;;and we return #<object-type-spec>.
  ;;
  (car (syntactic-binding-descriptor.value ?descriptor)))

(define-syntax-rule (syntactic-binding-descriptor/local-object-type.expanded-expr ?descriptor)
  ;;We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (local-object-type-name . (#<object-type-spec> . ?expanded-expr))
  ;;
  ;;and we return ?EXPANDED-EXPR.
  ;;
  (cdr (syntactic-binding-descriptor.value ?descriptor)))

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/global-object-type.library ?descriptor)
  ;;We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (global-object-type-name . (#<library> . ?loc))
  ;;
  ;;and we return #<library>.
  ;;
  (car (syntactic-binding-descriptor.value ?descriptor)))

(define-syntax-rule (syntactic-binding-descriptor/global-object-type.loc ?descriptor)
  ;;We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (global-object-type-name . (#<library> . ?loc))
  ;;
  ;;and we return ?LOC.
  ;;
  (cdr (syntactic-binding-descriptor.value ?descriptor)))

(define* (syntactic-binding-descriptor/global-object-type.object-type-spec descr)
  ;;We expect DESCR to have the format:
  ;;
  ;;   (global-object-type-name . (#<library> . ?loc))
  ;;
  ;;and we return  the value in the  slot "value" of the ?LOC  gensym, which contains
  ;;the instance of "<object-type-spec>".
  ;;
  (let ((loc (syntactic-binding-descriptor/global-object-type.loc descr)))
    (if (symbol-bound? loc)
	(let ((ots (symbol-value loc)))
	  (if (object-type-spec? ots)
	      ots
	    (assertion-violation __who__
	      "invalid object in \"value\" slot of loc gensym for syntactic binding's descriptor of global object-type name"
	      descr ots)))
      (assertion-violation __who__
	"unbound loc gensym associated to syntactic binding's descriptor of global object-type name"
	descr))))

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/core-object-type.object-type-spec ?descriptor)
  ;;We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (core-object-type-name . (#<object-type-spec> . ?symbolic-expr))
  ;;
  ;;and we return #<object-type-spec>.
  ;;
  (car (syntactic-binding-descriptor.value ?descriptor)))


;;;; syntactic binding descriptor: closure type binding

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;closure object-type specification; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-binding-descriptor/closure-type-name?
  closure-type-spec?)


;;;; syntactic binding descriptor: list sub-type binding

;;Return true  if the argument  is a  syntactic binding's descriptor  representing an
;;object-type specification for a "<list>" sub-type; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-binding-descriptor/list-of-type-name?
  list-of-type-spec?)


;;;; syntactic binding descriptor: vector sub-type binding

;;Return true  if the argument  is a  syntactic binding's descriptor  representing an
;;object-type specification for a "<vector>" sub-type; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-binding-descriptor/vector-of-type-name?
  vector-of-type-spec?)


;;;; syntactic binding descriptor: core primitive

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
   ;;   (core-prim . ?public-name)
   ;;
   (make-syntactic-binding-descriptor core-prim public-name))

 (define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/core-primitive?
   core-prim)

 (define-syntax-rule (core-primitive-binding-descriptor.public-name ?descriptor)
   ;;Given a  syntactic binding descriptor  representing a core primitive:  return its
   ;;public name symbol.
   ;;
   (cdr ?descriptor))
 /comment)


;;;; syntactic binding descriptor: hard-coded core primitive with type signature binding

(module (hard-coded-typed-core-prim-binding-descriptor->type-core-prim-binding-descriptor!)

  (define-module-who hard-coded-typed-core-prim-binding-descriptor->type-core-prim-binding-descriptor!)

  (define* (hard-coded-typed-core-prim-binding-descriptor->type-core-prim-binding-descriptor! descriptor)
    ;;Mutate a syntactic  binding's descriptor from the representation  of a built-in
    ;;core primitive (established by the boot image) to the representation of a typed
    ;;core  primitive in  the  format  usable by  the  expander.  Return  unspecified
    ;;values.
    ;;
    ;;We expect the core descriptor to have the format:
    ;;
    ;;   ($core-prim-typed . ?hard-coded-sexp)
    ;;
    ;;where ?HARD-CODED-SEXP has the format:
    ;;
    ;;   #(?core-prim-name ?safety-boolean (?signature-spec0 ?signature-spec ...))
    ;;
    ;;?CORE-PRIM-NAME  is a  symbol  representing the  core  primitive's public  name
    ;;("display", "write", "list", et cetera).  ?SIGNATURE-SPEC has the format:
    ;;
    ;;   ?signature-spec    = (?retvals-signature . ?argvals-signature)
    ;;
    ;;   ?retvals-signature = <list>
    ;;                      | <list-sub-type>
    ;;                      | (?type0 ?type ...)
    ;;                      | (?type0 ?type ... . <list>)
    ;;                      | (?type0 ?type ... . <list-sub-type>)
    ;;
    ;;   ?argvals-signature = <list>
    ;;                      | <list-sub-type>
    ;;                      | (?type0 ?type ...)
    ;;                      | (?type0 ?type ... . <list>)
    ;;                      | (?type0 ?type ... . <list-sub-type>)
    ;;
    ;;The usable descriptor has the format:
    ;;
    ;;   (core-prim-typed . (#<core-prim-type-spec> . ?hard-coded-sexp))
    ;;
    ;;Syntactic binding's  descriptors of  type "$core-prim-typed" are  hard-coded in
    ;;the boot image and generated directly by the makefile at boot image build-time.
    ;;Whenever the  function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used  to retrieve
    ;;the descriptor from the label: this function is used to convert the descriptor.
    ;;
    (let ((hard-coded-sexp		(syntactic-binding-descriptor.value descriptor)))
      (let ((core-prim.sym		(vector-ref hard-coded-sexp 0))
	    (safety.boolean		(vector-ref hard-coded-sexp 1))
	    (signature*.sexp		(vector-ref hard-coded-sexp 2)))
	(let* ((clambda-sig (%signature-sexp->case-lambda-signature core-prim.sym signature*.sexp))
	       (closure.ots (make-closure-type-spec clambda-sig)))
	  (set-car! descriptor 'core-prim-typed)
	  (set-cdr! descriptor (cons (make-core-prim-type-spec core-prim.sym safety.boolean closure.ots)
				     hard-coded-sexp))))))

  (define* (%signature-sexp->case-lambda-signature core-prim.sym signature*.sexp)
    (let ((clause*.sig (map (lambda (signature.sexp)
			      (%signature-sexp->clause-signature core-prim.sym signature.sexp))
			 signature*.sexp)))
      (make-case-lambda-signature clause*.sig)))

  (define* (%signature-sexp->clause-signature core-prim.sym sexp)
    (let* ((retvals.sexp (car sexp))
  	   (formals.sexp (cdr sexp))
  	   (retvals.stx  (bless retvals.sexp))
  	   (formals.stx  (bless formals.sexp)))
      (let ((retvals.sig (with-exception-handler
			     (lambda (E)
			       (raise-continuable
				(condition E
					   (make-who-condition __module_who__)
					   (make-message-condition "error initialising core primitive retvals signature")
					   (make-irritants-condition (list core-prim.sym retvals.stx)))))
			   (lambda ()
			     (make-type-signature retvals.stx))))
	    (formals.sig (with-exception-handler
			     (lambda (E)
			       (raise-continuable
				(condition E
					   (make-who-condition __module_who__)
					   (make-message-condition "error initialising core primitive formals signature")
					   (make-irritants-condition (list core-prim.sym formals.stx)))))
			   (lambda ()
			     (make-type-signature formals.stx)))))
	(make-lambda-signature retvals.sig formals.sig))))

  #| end of module: HARD-CODED-TYPED-CORE-PRIM-BINDING-DESCRIPTOR->TYPE-CORE-PRIM-BINDING-DESCRIPTOR! |# )

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;hard-coded typed core primitive; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/hard-coded-typed-core-prim?
  $core-prim-typed)


;;;; syntactic binding descriptor: core primitive with type signature binding
;;
;;The syntactic binding's descriptor has the format:
;;
;;   (core-prim-typed . (#<core-prim-type-spec> . ?hard-coded-sexp))
;;

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;typed core primitive, in a format usable by the expander; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/core-prim-typed?
  core-prim-typed)

(define-syntax-rule (core-prim-typed-binding-descriptor.core-prim-type-spec ?descriptor)
  (cadr ?descriptor))


;;;; syntactic binding descriptor: core built-in object-type descriptor binding

(define* (hard-coded-core-scheme-type-name-symbolic-binding-descriptor->core-scheme-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic binding's  descriptor  from  the  representation of  a  core
  ;;built-in object-type name (established by the  boot image) to a representation of
  ;;an object-type  name in the  format usable  by the expander.   Return unspecified
  ;;values.
  ;;
  ;;The core descriptor has the format:
  ;;
  ;;   ($core-scheme-object-type-name . ?hard-coded-sexp)
  ;;
  ;;and ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?type-name ?parent-name
  ;;     ?constructor-name ?type-predicate-name
  ;;     ?equality-predicate-name ?comparison-procedure-name ?hash-function-name
  ;;     ?type-descriptor-name
  ;;     ((?method-name . ?method-implementation-procedure) ...))
  ;;
  ;;and the usable descriptor has the format:
  ;;
  ;;   (core-object-type-name . (#<scheme-type-spec> . ?hard-coded-sexp))
  ;;
  ;;Syntactic binding descriptors of  type "$core-scheme-object-type-name" are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  ;;These core  descriptors are the ones  bound to the identifiers:  <top>, <fixnum>,
  ;;<string>, et cetera.
  ;;
  (let* ((descr.type			(syntactic-binding-descriptor.type  descriptor))
	 (descr.value			(syntactic-binding-descriptor.value descriptor))
	 (type-name.sym			(car descr.value))
	 (parent-name.sexp		(list-ref descr.value 1))
	 (constructor.stx		(bless (list-ref descr.value 2)))
	 (type-predicate.stx		(bless (list-ref descr.value 3)))
	 (equality-predicate.sexp	(list-ref descr.value 4))
	 (comparison-procedure.sexp	(list-ref descr.value 5))
	 (hash-function.sexp		(list-ref descr.value 6))
	 (type-descriptor.id		(core-prim-id (list-ref descr.value 7)))
	 (methods-table			(%alist-ref-or-null descr.value 8)))
    (let ((type-name.id			(core-prim-id type-name.sym))
	  (parent-name.id		(and parent-name.sexp		(core-prim-id parent-name.sexp)))
	  (equality-predicate.stx	(and equality-predicate.sexp	(core-prim-id equality-predicate.sexp)))
	  (comparison-procedure.stx	(and comparison-procedure.sexp	(core-prim-id comparison-procedure.sexp)))
	  (hash-function.stx		(and hash-function.sexp		(core-prim-id hash-function.sexp))))
      (let* ((parent-name.ots	(and parent-name.id
				     (with-exception-handler
					 (lambda (E)
					   (raise (condition E (make-who-condition __who__))))
				       (lambda ()
					 (id->object-type-spec parent-name.id (make-empty-lexenv))))))
	     (type-name.ots	(make-scheme-type-spec type-name.id parent-name.ots
						       constructor.stx type-predicate.stx
						       equality-predicate.stx comparison-procedure.stx hash-function.stx
						       type-descriptor.id methods-table)))
	(set-car! descriptor 'core-object-type-name)
	(set-cdr! descriptor (cons type-name.ots descr.value))))))

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;built-in Scheme object-type name; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/hard-coded-core-scheme-type-name?
  $core-scheme-object-type-name)


;;;; syntactic binding descriptor: Vicare struct-type name bindings

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;struct-type specification; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-binding-descriptor/struct-type-name?
  struct-type-spec?)

(define-syntax-rule (struct-type-name-binding-descriptor.type-descriptor ?descriptor)
  ;;Given a  syntactic binding's descriptor  representing a struct-type  name: return
  ;;the struct-type descriptor itself.
  ;;
  (struct-type-spec.std (car (syntactic-binding-descriptor.value ?descriptor))))


;;;; syntactic binding descriptor: usable R6RS record-type descriptor binding

;;Return true if the argument is a syntactic binding's descriptor representing a base
;;R6RS record-type specification; otherwise return false.
;;
;;We expect these syntactic binding's descriptors to have the format:
;;
;;   (local-object-type-spec . (?spec . ?expanded-expr))
;;
;;where ?SPEC is an instance of a sub-type of "<record-type-spec>".
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-binding-descriptor/record-type-name?
  record-type-spec?)


;;;; syntactic binding descriptor: hard-coded core R6RS record-type descriptor binding

(define (hard-coded-core-record-type-name-binding-descriptor->core-record-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic binding's  descriptor  from  the  representation of  a  core
  ;;record-type  name (established  by  the  boot image)  to  a  representation of  a
  ;;record-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-record-type-name . ?hard-coded-sexp)
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   (core-object-type-name . (#<record-type-spec> . ?hard-coded-sexp))
  ;;
  ;;where ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?type-name ?rtd-name ?rcd-name ?parent-id
  ;;    ?constructor-id ?type-predicate-id
  ;;    ?accessors-alist ?mutators-alist)
  ;;
  ;;Syntactic binding descriptors of  type "$core-record-type-name" are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  (let* ((descr.type			(syntactic-binding-descriptor.type  descriptor))
	 (hard-coded-sexp		(syntactic-binding-descriptor.value descriptor))
	 (type-name.id			(core-prim-id (car hard-coded-sexp)))
	 (rtd.id			(core-prim-id (list-ref hard-coded-sexp 1)))
	 (rcd.id			(core-prim-id (list-ref hard-coded-sexp 2)))
	 (super-protocol.id		#f)
	 (parent.id			(cond ((list-ref hard-coded-sexp 3)
					       => core-prim-id)
					      (else #f)))
	 (constructor-sexp		(bless (list-ref hard-coded-sexp 4)))
	 (destructor-sexp		#f)
	 (type-predicate-sexp		(bless (list-ref hard-coded-sexp 5)))
	 (equality-predicate.id		#f)
	 (comparison-procedure.id	#f)
	 (hash-function.id		#f)
	 (accessors-table		(%alist-ref-or-null hard-coded-sexp 6))
	 (mutators-table		'())
	 (methods-table			accessors-table)
	 (ots				(make-record-type-spec type-name.id rtd.id rcd.id super-protocol.id parent.id
							       constructor-sexp destructor-sexp type-predicate-sexp
							       equality-predicate.id comparison-procedure.id hash-function.id
							       accessors-table mutators-table methods-table)))
    (set-car! descriptor 'core-object-type-name)
    (set-cdr! descriptor (cons ots hard-coded-sexp))))

;;Return true if the argument is a syntactic binding's descriptor representing a R6RS
;;record-type descriptor established by the boot image; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/hard-coded-core-record-type-name?
  $core-record-type-name)


;;;; syntactic binding descriptor: core R6RS condition object record-type descriptor binding

(define (hard-coded-core-condition-object-type-name-binding-descriptor->core-record-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic binding's  descriptor  from  the  representation of  a  core
  ;;condition  object  record-type  name  (established   by  the  boot  image)  to  a
  ;;representation  of a  record-type  name in  the format  usable  by the  expander.
  ;;Return unspecified values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-condition-object-type-name . ?hard-coded-sexp)
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   (core-object-type-name . (#<record-type-spec> . ?hard-coded-sexp))
  ;;
  ;;where ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?type-name ?rtd-name ?rcd-name
  ;;    ?parent-name ?constructor-name ?type-predicate-name ?accessors-alist)
  ;;
  ;;Syntactic  binding  descriptors  of type  "$core-condition-object-type-name"  are
  ;;hard-coded in the boot image and generated directly by the makefile at boot image
  ;;build-time.  Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to
  ;;retrieve the  descriptor from  the label:  this function is  used to  convert the
  ;;descriptor.
  ;;
  (let* ((descr.type			(syntactic-binding-descriptor.type  descriptor))
	 (hard-coded-sexp		(syntactic-binding-descriptor.value descriptor))
	 (type-name.id			(core-prim-id (car hard-coded-sexp)))
	 (rtd.id			(core-prim-id (list-ref hard-coded-sexp 1)))
	 (rcd.id			(core-prim-id (list-ref hard-coded-sexp 2)))
	 (super-protocol.id		#f)
	 (parent.id			(cond ((list-ref hard-coded-sexp 3)
					       => core-prim-id)
					      (else #f)))
	 (constructor.id		(core-prim-id (list-ref hard-coded-sexp 4)))
	 (destructor.id			#f)
	 (type-predicate.id		(core-prim-id (list-ref hard-coded-sexp 5)))
	 (equality-predicate.id		#f)
	 (comparison-procedure.id	#f)
	 (hash-function.id		#f)
	 (accessors-table		(%alist-ref-or-null hard-coded-sexp 6))
	 (mutators-table		'())
	 (methods-table			accessors-table)
	 (ots				(make-record-type-spec type-name.id
							       rtd.id rcd.id super-protocol.id parent.id
							       constructor.id destructor.id type-predicate.id
							       equality-predicate.id comparison-procedure.id hash-function.id
							       accessors-table mutators-table methods-table)))
    (set-car! descriptor 'core-object-type-name)
    (set-cdr! descriptor (cons ots hard-coded-sexp))))

;;Return true if the argument is a syntactic binding's descriptor representing a R6RS
;;condition object record-type descriptor established  by the boot image (for example
;;"&who", "&error", et cetera); otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/hard-coded-core-condition-object-type-name?
  $core-condition-object-type-name)


;;;; syntactic binding descriptor: core built-in object-type descriptor binding

(define* (hard-coded-core-type-annotation-symbolic-binding-descriptor->core-type-annotation-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic binding's  descriptor  from  the  representation of  a  core
  ;;built-in type annotation  (established by the boot image) to  a representation of
  ;;an type  annotation in  the format  usable by  the expander.   Return unspecified
  ;;values.
  ;;
  ;;The core descriptor has the format:
  ;;
  ;;   ($core-type-annotation . ?hard-coded-sexp)
  ;;
  ;;and ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?type-name ?type-annotation)
  ;;
  ;;and the usable descriptor has the format:
  ;;
  ;;   (core-object-type-name . (#<object-type-spec> . ?hard-coded-sexp))
  ;;
  ;;Syntactic binding  descriptors of type "$core-type-annotation"  are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  (let* ((descr.type		(syntactic-binding-descriptor.type  descriptor))
	 (descr.value		(syntactic-binding-descriptor.value descriptor))
	 (type-name.sym		(car descr.value))
	 (type-annotation.sexp	(list-ref descr.value 1)))
    (let ((type-name.id		(core-prim-id type-name.sym))
	  (type-annotation.stx	(bless type-annotation.sexp)))
      (let* ((type.ots		(type-annotation->object-type-spec type-annotation.stx (make-empty-lexenv))))
	(set-car! descriptor 'core-object-type-name)
	(set-cdr! descriptor (cons type.ots descr.value))))))

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;built-in type annotation; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/hard-coded-type-annotation?
  $core-type-annotation)


;;;; syntactic binding descriptor: fluid syntax bindings

(define-syntax-rule (make-syntactic-binding-descriptor/local-global-macro/fluid-syntax ?fluid-label)
  ;;Build and return a syntactic binding  descriptor representing a fluid syntax; the
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

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/fluid-syntax?
  $fluid)

(define-syntax-rule (fluid-syntax-binding-descriptor.fluid-label ?binding)
  (syntactic-binding-descriptor.value ?binding))


;;;; syntactic binding descriptor: synonym bindings

(define* (make-syntactic-binding-descriptor/local-global-macro/synonym-syntax src-id)
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
  (cond ((id->label src-id)
	 => (lambda (label)
	      (make-syntactic-binding-descriptor $synonym label)))
	(else
	 (error-unbound-identifier __who__ src-id))))

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/synonym-syntax?
  $synonym)

(define-syntax-rule (synonym-syntax-binding-descriptor.synonym-label ?binding)
  (syntactic-binding-descriptor.value ?binding))


;;;; syntactic binding descriptor: compile-time values bindings

(define (make-syntactic-binding-descriptor/local-macro/expand-time-value obj expanded-expr)
  ;;Build a  syntactic binding  descriptor representing  a local  compile-time value.
  ;;
  ;;OBJ  must  be  the  actual   object  computed  from  a  compile-time  expression.
  ;;EXPANDED-EXPR  must be  the core  language expression  representing the  original
  ;;right-hand side already expanded.
  ;;
  ;;Given the definition:
  ;;
  ;;   (define-syntax etv
  ;;      (make-expand-time-value ?expr))
  ;;
  ;;the  argument OBJ  is the  return value  of expanding  and evaluating  ?EXPR; the
  ;;argument EXPANDED-EXPR  is the result of  expanding the whole right-hand  side of
  ;;the DEFINE-SYNTAX form.
  ;;
  (make-syntactic-binding-descriptor local-etv (cons obj expanded-expr)))

(define-syntax-rule (local-expand-time-value-binding-descriptor.object ?descriptor)
  ;;Given a  syntactic binding  descriptor representing a  local compile  time value:
  ;;return the actual compile-time object.  We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (local-etv . (?obj . ?expanded-expr))
  ;;
  ;;and we want to return ?OBJ.
  ;;
  (car (syntactic-binding-descriptor.value ?descriptor)))

;;; --------------------------------------------------------------------

;;Commented out  because unused, but kept  for reference.  (Marco Maggi;  Mon Apr 20,
;;2015)
;;
;; (define (make-syntactic-binding-descriptor/global-expand-time-value lib loc)
;;   ;;Build  and  return  a  syntactic  binding  descriptor  representing  an  imported
;;   ;;compile-time value.
;;   ;;
;;   ;;The  argument LOC  is a  loc  gensym.  The  argument  LIB is  a "library"  object
;;   ;;representing the library from which the binding is imported.
;;   ;;
;;   ;;The returned descriptor has format:
;;   ;;
;;   ;;   (global-etv . (?library . ?loc))
;;   ;;
;;   (make-syntactic-binding-descriptor global-etv (cons lib loc)))

(define-syntax-rule (global-expand-time-value-binding-descriptor.lib ?descriptor)
  (car (syntactic-binding-descriptor.value ?descriptor)))

(define-syntax-rule (global-expand-time-value-binding-descriptor.loc ?descriptor)
  (cdr (syntactic-binding-descriptor.value ?descriptor)))

(define (global-expand-time-value-binding-descriptor.object descriptor)
  ;;Given a syntactic binding descriptor representing an imported compile time value:
  ;;return the actual compile-time object.  We expect ?DESCRIPTOR to have the format:
  ;;
  ;;   (global-etv . (?library . ?loc))
  ;;
  ;;where: ?LIBRARY is an object of  type "library" describing the library from which
  ;;the binding is imported;  ?LOC is the log gensym containing  the actual object in
  ;;its VALUE slot (but only after the library has been visited).
  ;;
  (let ((lib (global-expand-time-value-binding-descriptor.lib descriptor))
	(loc (global-expand-time-value-binding-descriptor.loc descriptor)))
    ;;When the  library LIB  has been  loaded from  source: the  compile-time value's
    ;;object is stored in the loc gensym.   When the library LIB has been loaded from
    ;;a compiled file: the compile-time value itself is in the loc gensym, so we have
    ;;to extract it.
    (let ((etv (symbol-value loc)))
      (if (expand-time-value? etv)
	  (expand-time-value-object etv)
	etv))))


;;;; syntactic binding descriptor: module bindings

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


;;;; syntactic binding descriptor: pattern variable bindings

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

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/pattern-variable?
  pattern-variable)


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
