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


;;;; syntactic bindings core definitions

(define-syntax-rule (make-lexenv-entry ?label ?descr)
  (cons ?label ?descr))

;;Given the entry from a lexical environment: return the gensym acting as label.
;;
(define lexenv-entry.label car)

;;Given the entry from a lexical environment: return the binding value.
;;
(define lexenv-entry.binding-descriptor cdr)

(define-syntax-rule (push-entry-on-lexenv ?label ?descr ?lexenv)
  (cons (make-lexenv-entry ?label ?descr) ?lexenv))

;;; --------------------------------------------------------------------

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


;;; helpers

(define-syntax define-syntactic-binding-descriptor-predicate
  (syntax-rules ()
    ((_ ?who ?type)
     (define (?who obj)
       ;;Return  true  if OBJ  is  a  syntactic  binding  descriptor of  type  ?TYPE;
       ;;otherwise return false.
       ;;
       (and (pair? obj)
	    (eq? (quote ?type) (syntactic-binding-descriptor.type obj)))))
    ((_ ?who ?type0 ?type1 ?type ...)
     (define (?who obj)
       ;;Return  true  if OBJ  is  a  syntactic  binding  descriptor of  type  ?TYPE;
       ;;otherwise return false.
       ;;
       (and (pair? obj)
	    (memq (quote (?type0 ?type1 ?type ...)) (syntactic-binding-descriptor.type obj)))))
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

(define-syntactic-binding-descriptor-predicate lexical-var-binding-descriptor?
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

(define (make-syntactic-binding-descriptor/lexical-typed-var/from-data type-id lex)
  (define lts
    (make-lexical-typed-variable-spec type-id lex))
  (define expanded-expr
    (build-application no-source
      (build-primref no-source 'make-lexical-typed-variable-spec)
      (list (build-data no-source type-id)
	    (build-data no-source lex))))
  (make-syntactic-binding-descriptor/lexical-typed-var lts expanded-expr))

;;; --------------------------------------------------------------------

(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/lexical-typed-var?
  lexical-typed)

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec ?descriptor)
  ;;Extract the  record of  type "<typed-variable-spec>"  from a  syntactic binding's
  ;;descriptor of type "lexical-typed".
  ;;
  (cadr ?descriptor))

;;; --------------------------------------------------------------------

(define-syntax-rule (syntactic-binding-descriptor/lexical-typed-var.value.lex ?descriptor-value)
  ;;Accessor for the  lexical gensym in a typed lexical  variable's syntactic binding
  ;;descriptor.
  ;;
  ;;A syntactic  binding representing  a typed lexical  variable has  descriptor with
  ;;format:
  ;;
  ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
  ;;
  (lexical-typed-variable-spec.lex (car ?descriptor-value)))

(define-syntax-rule (syntactic-binding-descriptor/lexical-typed-var.value.type-id ?descriptor-value)
  ;;Accessor for the type identifier in  a typed lexical variable's syntactic binding
  ;;descriptor.
  ;;
  ;;A syntactic binding representing a lexical variable has descriptor with format:
  ;;
  ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
  ;;
  (lexical-typed-variable-spec.type-id (car ?descriptor-value)))

(define-syntax syntactic-binding-descriptor/lexical-typed-var.value.assigned?
  (syntax-rules ()
    ((_ ?descriptor-value)
     (lexical-typed-variable-spec.assigned? (car ?descriptor-value)))
    ((_ ?descriptor-value ?bool)
     (lexical-typed-variable-spec.assigned?-set! (car ?descriptor-value) (and ?bool #t)))
    ))


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

(define* (make-global-typed-variable-spec-and-maker-core-expr {lts lexical-typed-variable-spec?} variable-loc)
  (let* ((type-id		(typed-variable-spec.type-id             lts))
	 (unsafe-variant	(typed-variable-spec.unsafe-variant-sexp lts))
	 (gts			(make-global-typed-variable-spec type-id unsafe-variant variable-loc))
	 (core-expr		(build-application no-source
				  (build-primref no-source 'make-global-typed-variable-spec)
				  (list (build-data no-source type-id)
					(build-data no-source unsafe-variant)
					(build-data no-source variable-loc)))))
    (values gts core-expr)))

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
(define-syntactic-binding-descriptor-predicate local-macro/non-variable-transformer-binding-descriptor?
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
(define-syntactic-binding-descriptor-predicate local-macro/variable-transformer-binding-descriptor?
  local-macro!)


;;;; base object-type syntactic binding descriptors

(define-syntax-rule (define-syntactic-binding-descriptor-predicate/object-type-spec ?who ?pred)
  (define (?who obj)
    ;;Return  true  if  OBJ  is  a syntactic  binding's  descriptor  representing  an
    ;;object-type  specification; otherwise  return false.   We expect  the syntactic
    ;;binding's descriptor to have one of the formats:
    ;;
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
	     (else #f))))))

;;Return true  if the argument  is a  syntactic binding's descriptor  representing an
;;object-type specification; otherwise return  false.  Syntactic identifiers bound to
;;such syntactic  binding descriptors  can be  used with  the generic  syntaxes: NEW,
;;IS-A?, SLOT-REF, SLOT-SET!, METHOD-CALL.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec object-type-name-binding-descriptor?
  object-type-spec?)

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


;;;; syntactic binding descriptor: usable built-in object type binding

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;Scheme object-type specification; otherwise return false.
;;
;;We expect the syntactic binding's descriptor to have the format:
;;
;;   (local-object-type-name . (?spec . ?expanded-expr))
;;
;;where ?SPEC is an instance of a sub-type of "<scheme-type-spec>".
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec scheme-type-name-binding-descriptor?
  scheme-type-spec?)


;;;; syntactic binding descriptor: closure type binding

(define* (make-syntactic-binding-descriptor/closure-type-name {ots closure-type-spec?} ots-maker.core-expr)
  ;;Build and  return a  syntactic binding's  descriptor representing  a closure-type
  ;;name.  Such type is a sub-type of "<procedure>".
  ;;
  ;;The returned syntactic binding's descriptor has the format:
  ;;
  ;;   (local-object-type-name . (#<closure-type-spec> . ?ots-maker.core-expr))
  ;;
  (make-syntactic-binding-descriptor local-object-type-name (cons ots ots-maker.core-expr)))

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;closure object-type specification; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec closure-type-name-binding-descriptor?
  closure-type-spec?)

(case-define* make-syntactic-binding/closure-type-name
  ;;Establish the full  syntactic binding representing a  closure-type: the signature
  ;;of  a closure  object  to be  used  at  expand-time.  This  function  is used  by
  ;;INTERNAL-DEFINE when defining the syntactic binding of a function.
  ;;
  ;;TYPE-ID must  the syntacitc identifier  that will  be bound to  the closure-type.
  ;;SIGNATURE must be an instance of  "<callable-signature>".  RIB must be the rib in
  ;;which the syntactic binding's identifier is associated to its label.  LEXENV must
  ;;be the LEXENV in which the label is associated to the descriptor.
  ;;
  ;;The established syntactic binding can be  included in the export environment of a
  ;;library.
  ;;
  ;;NOTE  The operation  is similar  to what  the C  language "typedef"  does in  the
  ;;following code chunk:
  ;;
  ;;   int func (double a, char * b) { ... }
  ;;   typedef int func_t (double a, char * b);
  ;;   func_t * the_func = func;
  ;;
  ((type-id signature rib lexenv)
   (make-syntactic-binding/closure-type-name type-id signature rib lexenv #f))
  ((type-id signature rib lexenv shadow/redefine-bindings?)
   (let* ((ots			(make-closure-type-spec signature))
	  (ots.core-expr	(build-application no-source
				  (build-primref no-source 'make-closure-type-spec)
				  (list (build-data no-source signature))))
	  (type-id.descr	(make-syntactic-binding-descriptor/closure-type-name ots ots.core-expr))
	  (type-id.lab		(generate-label-gensym type-id))
	  (lexenv^		(push-entry-on-lexenv type-id.lab type-id.descr lexenv)))
     (extend-rib! rib type-id type-id.lab shadow/redefine-bindings?)
     lexenv^)))

(define* (make-fabricated-closure-type-name {who false-or-symbol?})
  (gensym (string-append "<"
			 (if who
			     (symbol->string who)
			   "anonymous")
			 "/closure-signature>")))


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

 (define-syntactic-binding-descriptor-predicate core-primitive-binding-descriptor?
   core-prim)

 (define-syntax-rule (core-primitive-binding-descriptor.public-name ?descriptor)
   ;;Given a  syntactic binding descriptor  representing a core primitive:  return its
   ;;public name symbol.
   ;;
   (cdr ?descriptor))
 /comment)


;;;; syntactic binding descriptor: hard-coded core primitive with type signature binding

(module (hard-coded-core-prim-typed-binding-descriptor->core-closure-type-name-binding-descriptor!)

  (define (hard-coded-core-prim-typed-binding-descriptor->core-closure-type-name-binding-descriptor! descriptor)
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
    ;;   (?core-prim-name ?unsafe-core-prim-name (?signature-spec0 ?signature-spec ...))
    ;;
    ;;?CORE-PRIM-NAME  is a  symbol  representing the  core  primitive's public  name
    ;;("display", "write", "list", et cetera).  ?SIGNATURE-SPEC has the format:
    ;;
    ;;   ?signature-spec    = (?retval-signature ?formals-signature)
    ;;
    ;;   ?retval-signature  = <list>
    ;;                      | <list-sub-type>
    ;;                      | (?type0 ?type ...)
    ;;                      | (?type0 ?type ... . <list>)
    ;;                      | (?type0 ?type ... . <list-sub-type>)
    ;;
    ;;   ?formals-signature = <list>
    ;;                      | <list-sub-type>
    ;;                      | (?type0 ?type ...)
    ;;                      | (?type0 ?type ... . <list>)
    ;;                      | (?type0 ?type ... . <list-sub-type>)
    ;;
    ;;The usable descriptor has the format:
    ;;
    ;;   (core-prim-typed . ((?core-prim-name ?type-id . ?unsafe-variant.id) . ?hard-coded-sexp))
    ;;
    ;;Syntactic binding's  descriptors of  type "$core-prim-typed" are  hard-coded in
    ;;the boot image and generated directly by the makefile at boot image build-time.
    ;;Whenever the  function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used  to retrieve
    ;;the descriptor from the label: this function is used to convert the descriptor.
    ;;
    (let* ((descr.type			(syntactic-binding-descriptor.type  descriptor))
	   (descr.value			(syntactic-binding-descriptor.value descriptor))
	   (core-prim.sym		(car descr.value))
	   (unsafe-core-prim.sym	(cadr descr.value))
	   (signature*.sexp		(list-ref descr.value 2))
	   (signature			(%signature-sexp->callable-signature signature*.sexp))
	   (type-id			(fabricate-closure-type-identifier core-prim.sym signature))
	   (unsafe-variant.id		(core-prim-id unsafe-core-prim.sym)))
      (set-car! descriptor 'core-prim-typed)
      (set-cdr! descriptor (cons (cons* core-prim.sym type-id unsafe-variant.id) descr.value))))

  (define (%signature-sexp->callable-signature signature*.sexp)
    (if (null? (cdr signature*.sexp))
  	(%signature-sexp->lambda-signature (car signature*.sexp))
      (make-clambda-compound (map %signature-sexp->lambda-signature signature*.sexp))))

  (define (%signature-sexp->lambda-signature sexp)
    (let* ((retvals.sexp (car  sexp))
  	   (formals.sexp (cadr sexp))
  	   (retvals.stx  (%any-list->ids retvals.sexp))
  	   (formals.stx  (%any-list->ids formals.sexp)))
      (make-lambda-signature (make-retvals-signature retvals.stx)
  			     (make-formals-signature formals.stx))))

  (define (%any-list->ids ell)
    ;;Convert a proper  or improper list of symbols representing  core type identifiers
    ;;into the corresponding proper or improper list of type identifiers.
    ;;
    (cond ((symbol? ell)
  	   (core-prim-id ell))
  	  ((pair? ell)
  	   (cons (core-prim-id   (car ell))
  		 (%any-list->ids (cdr ell))))
  	  (else '())))

  #| end of module |# )

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;hard-coded typed core primitive; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/hard-coded-core-prim-typed?
  $core-prim-typed)


;;;; syntactic binding descriptor: core primitive with type signature binding
;;
;;The syntactic binding's descriptor has the format:
;;
;;   (core-prim-typed . ((?core-prim-name ?type-id . ?unsafe-core-prim.id) . ?hard-coded-sexp))
;;

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;typed core primitive, in a format usable by the expander; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate syntactic-binding-descriptor/core-prim-typed?
  core-prim-typed)

;;; --------------------------------------------------------------------

(define-syntax-rule (core-prim-typed-binding-descriptor.prim-name ?descriptor)
  (car  (cadr ?descriptor)))

(define-syntax-rule (core-prim-typed-binding-descriptor.type-id ?descriptor)
  (cadr (cadr ?descriptor)))

(define-syntax-rule (core-prim-typed-binding-descriptor.unsafe-variant-id ?descriptor)
  (cddr (cadr ?descriptor)))

;;; --------------------------------------------------------------------

(define-syntax-rule (core-prim-typed-binding-descriptor.value.prim-name ?descriptor-value)
  (caar ?descriptor-value))

(define-syntax-rule (core-prim-typed-binding-descriptor.value.type-id ?descriptor-value)
  (cadar ?descriptor-value))

(define-syntax-rule (core-prim-typed-binding-descriptor.value.unsafe-variant-id ?descriptor-value)
  (cddar ?descriptor-value))


;;;; syntactic binding descriptor: core built-in object-type descriptor binding

(define (core-scheme-type-name-binding-descriptor->scheme-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic binding's  descriptor  from  the  representation of  a  core
  ;;built-in object-type name (established by the  boot image) to a representation of
  ;;an object-type  name in the  format usable  by the expander.   Return unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-scheme-type-name . ?hard-coded-sexp)
  ;;
  ;;and ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?parent-name ?constructor-name ?type-predicate-name ?methods-alist)
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   (local-object-type-name . (#<core-scheme-type-spec> . ?hard-coded-sexp))
  ;;
  ;;Syntactic binding descriptors of  type "$core-scheme-type-name" are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  ;;These core  descriptors are the ones  bound to the identifiers:  <top>, <fixnum>,
  ;;<string>, et cetera.
  ;;
  (let* ((descr.type		(syntactic-binding-descriptor.type  descriptor))
	 (descr.value		(syntactic-binding-descriptor.value descriptor))
	 (parent-id		(cond ((list-ref descr.value 1)
				       => core-prim-id)
				      (else #f)))
	 (constructor.sexp	(bless (list-ref descr.value 2)))
	 (type-predicate.sexp	(bless (list-ref descr.value 3)))
	 (methods-table		(%alist-ref-or-null descr.value 4)))
    (define spec
      (make-core-scheme-type-spec parent-id constructor.sexp type-predicate.sexp methods-table))
    (set-car! descriptor 'local-object-type-name)
    (set-cdr! descriptor (cons spec descr.value))))

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;built-in Scheme object-type name; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate core-scheme-type-name-binding-descriptor?
  $core-scheme-type-name)


;;;; syntactic binding descriptor: Vicare struct-type name bindings

(define* (make-syntactic-binding-descriptor/struct-type-name {sts struct-type-spec?} expanded-expr)
  ;;Build  and return  a syntactic  binding's descriptor  representing a  struct-type
  ;;name.
  ;;
  ;;The argument STS must be an instance of "<struct-type-spec>".
  ;;
  ;;Given the definition:
  ;;
  ;;   (define-struct ?type-name (?field-name ...))
  ;;
  ;;the syntax use DEFINE-STRUCT expands into multiple forms, one of which is:
  ;;
  ;;   (define-syntax ?type-name
  ;;     (make-struct-type-spec ?std ...))
  ;;
  ;;where   ?STD   is   a   struct-type  descriptor   built   at   expand-time   with
  ;;MAKE-STRUCT-TYPE.  Syntactic bindings of this  type are generated when evaluating
  ;;the right-hand side of a DEFINE-SYNTAX returns the return value of this function.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   (local-object-type-name . (#<struct-type-spec> . ?expanded-expr))
  ;;
  (make-syntactic-binding-descriptor local-object-type-name (cons sts expanded-expr)))

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;struct-type specification; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec struct-type-name-binding-descriptor?
  struct-type-spec?)

(define-syntax-rule (struct-type-name-binding-descriptor.type-descriptor ?descriptor)
  ;;Given a  syntactic binding's descriptor  representing a struct-type  name: return
  ;;the struct-type descriptor itself.
  ;;
  (struct-type-spec.std (car (syntactic-binding-descriptor.value ?descriptor))))


;;;; syntactic binding descriptor: base record-type descriptor binding

;;Return true if the argument is a syntactic binding's descriptor representing a base
;;R6RS record-type specification; otherwise return false.
;;
;;We expect these syntactic binding's descriptors to have the format:
;;
;;   (local-object-type-spec . (?spec . ?expanded-expr))
;;
;;where ?SPEC is an instance of a sub-type of "<record-type-spec>".
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec record-type-name-binding-descriptor?
  record-type-spec?)


;;;; syntactic binding descriptor: usable R6RS record-type descriptor binding

(define* (make-syntactic-binding-descriptor/syntactic-record-type-name {rts syntactic-record-type-spec?} expanded-expr)
  ;;Build and return a syntactic binding's descriptor representing a record-type name
  ;;defined by the syntactic layer.  We handle this entry in a way that is similar to
  ;;a local macro.
  ;;
  ;;Given the syntax use:
  ;;
  ;;   (define-record-type ?type-name ?clause ...)
  ;;
  ;;the output form of its expansion contains multiple forms, one of which is:
  ;;
  ;;   (define-syntax ?type-name
  ;;     (make-record-type-spec ?arg ...))
  ;;
  ;;Syntactic bindings of this type are generated when evaluating the right-hand side
  ;;of a DEFINE-SYNTAX returns an instance of "<syntactic-record-type-spec>".
  ;;
  ;;The returned descriptor has the format:
  ;;
  ;;   (local-object-type-name . (#<syntactic-record-type-spec> . ?expanded-expr))
  ;;
  (make-syntactic-binding-descriptor local-object-type-name (cons rts expanded-expr)))

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;record-type name defined by the syntactic layer; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate/object-type-spec syntactic-record-type-name-binding-descriptor?
  syntactic-record-type-spec?)


;;;; syntactic binding descriptor: old-style core R6RS record-type descriptor binding

(define (core-rtd-binding-descriptor->record-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic binding's  descriptor  from  the  representation of  a  core
  ;;record-type  name (established  by  the  boot image)  to  a  representation of  a
  ;;record-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-rtd . ?hard-coded-sexp)
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   (local-object-type-name . (#<core-record-type-spec> . ?hard-coded-sexp))
  ;;
  ;;where ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?rtd-name ?rcd-name)
  ;;
  ;;Syntactic  binding descriptors  of type  "$core-rtd" are  hard-coded in  the boot
  ;;image and generated directly by the  makefile at boot image build-time.  Whenever
  ;;the  function   LABEL->SYNTACTIC-BINDING-DESCRIPTOR  is  used  to   retrieve  the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  ;;NOTE This old  style of record-type definition is deprecated:  it has very little
  ;;informations  about   the  type.   We  should   use  the  new  style   with  type
  ;;"$core-record-type-name".
  ;;
  (let* ((descr.type		(syntactic-binding-descriptor.type  descriptor))
	 (hard-coded-sexp	(syntactic-binding-descriptor.value descriptor))
	 (rtd-id		(core-prim-id (car  hard-coded-sexp)))
	 (rcd-id		(core-prim-id (cadr hard-coded-sexp)))
	 (ots			(make-core-record-type-spec rtd-id rcd-id)))
    (set-car! descriptor 'local-object-type-name)
    (set-cdr! descriptor (cons ots hard-coded-sexp))))

(define-syntactic-binding-descriptor-predicate core-rtd-binding-descriptor?
  $core-rtd)


;;;; syntactic binding descriptor: new-style core R6RS record-type descriptor binding

(define (core-record-type-name-binding-descriptor->record-type-name-binding-descriptor! descriptor)
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
  ;;   (local-object-type-name . (#<core-record-type-spec> . ?hard-coded-sexp))
  ;;
  ;;where ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?rtd-name ?rcd-name ?parent-id ?constructor-id ?type-predicate-id
  ;;    ?accessors-alist ?mutators-alist)
  ;;
  ;;Syntactic binding descriptors of  type "$core-record-type-name" are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  (let* ((descr.type		(syntactic-binding-descriptor.type  descriptor))
	 (hard-coded-sexp	(syntactic-binding-descriptor.value descriptor))
	 (rtd-id		(core-prim-id (car  hard-coded-sexp)))
	 (rcd-id		(core-prim-id (cadr hard-coded-sexp)))
	 (super-protocol-id	#f)
	 (parent-id		(cond ((list-ref hard-coded-sexp 2)
				       => core-prim-id)
				      (else #f)))
	 (constructor-sexp	(bless (list-ref hard-coded-sexp 3)))
	 (destructor-sexp	#f)
	 (type-predicate-sexp	(bless (list-ref hard-coded-sexp 4)))
	 (accessors-table	(%alist-ref-or-null hard-coded-sexp 5))
	 (mutators-table	(%alist-ref-or-null hard-coded-sexp 6))
	 (methods-table		accessors-table)
	 (ots			(make-core-record-type-spec rtd-id rcd-id super-protocol-id parent-id
							    constructor-sexp destructor-sexp type-predicate-sexp
							    accessors-table mutators-table methods-table)))
    (set-car! descriptor 'local-object-type-name)
    (set-cdr! descriptor (cons ots hard-coded-sexp))))

;;Return true if the argument is a syntactic binding's descriptor representing a R6RS
;;record-type descriptor established by the boot image; otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate core-record-type-name-binding-descriptor?
  $core-record-type-name)


;;;; syntactic binding descriptor: core R6RS condition object record-type descriptor binding

(define (core-condition-object-type-name-binding-descriptor->record-type-name-binding-descriptor! descriptor)
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
  ;;   (local-object-type-name . (#<core-condition-type-spec> . ?hard-coded-sexp))
  ;;
  ;;where ?HARD-CODED-SEXP has the format:
  ;;
  ;;   (?rtd-name ?rcd-name ?parent-id ?constructor-id ?type-predicate-id ?accessors-alist)
  ;;
  ;;Syntactic  binding  descriptors  of type  "$core-condition-object-type-name"  are
  ;;hard-coded in the boot image and generated directly by the makefile at boot image
  ;;build-time.  Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to
  ;;retrieve the  descriptor from  the label:  this function is  used to  convert the
  ;;descriptor.
  ;;
  (let* ((descr.type		(syntactic-binding-descriptor.type  descriptor))
	 (hard-coded-sexp	(syntactic-binding-descriptor.value descriptor))
	 (rtd-id		(core-prim-id (car  hard-coded-sexp)))
	 (rcd-id		(core-prim-id (cadr hard-coded-sexp)))
	 (super-protocol-id	#f)
	 (parent-id		(cond ((list-ref hard-coded-sexp 2)
				       => core-prim-id)
				      (else #f)))
	 (constructor-id	(bless (list-ref hard-coded-sexp 3)))
	 (destructor-id		#f)
	 (type-predicate-id	(bless (list-ref hard-coded-sexp 4)))
	 (accessors-table	(%alist-ref-or-null hard-coded-sexp 5))
	 (mutators-table	'())
	 (methods-table		accessors-table)
	 (ots			(make-core-condition-type-spec rtd-id rcd-id super-protocol-id parent-id
							       constructor-id destructor-id type-predicate-id
							       accessors-table mutators-table methods-table)))
    (set-car! descriptor 'local-object-type-name)
    (set-cdr! descriptor (cons ots hard-coded-sexp))))

;;Return true if the argument is a syntactic binding's descriptor representing a R6RS
;;condition object record-type descriptor established  by the boot image (for example
;;"&who", "&error", et cetera); otherwise return false.
;;
(define-syntactic-binding-descriptor-predicate core-condition-object-type-name-binding-descriptor?
  $core-condition-object-type-name)


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

(define-syntactic-binding-descriptor-predicate fluid-syntax-binding-descriptor?
  $fluid)

(define-syntax-rule (fluid-syntax-binding-descriptor.fluid-label ?binding)
  (syntactic-binding-descriptor.value ?binding))


;;;; syntactic binding descriptor: synonym bindings

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

(define-syntactic-binding-descriptor-predicate synonym-syntax-binding-descriptor?
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

(define* (retrieve-expand-time-value {id identifier?})
  ;;This is the compile-time values  retriever function.  Given an identifier: search
  ;;an  entry in  the lexical  environment; when  found return  its value,  otherwise
  ;;return false.
  ;;
  (let* ((label (id->label/or-error __who__ #f id))
	 (descr (label->syntactic-binding-descriptor label (current-inferior-lexenv))))
    (case (syntactic-binding-descriptor.type descr)
      ((displaced-lexical)
       (assertion-violation __who__
	 "identifier out of context (identifier's label not in LEXENV)" id))
      ;;The given  identifier is  bound to  a local  compile-time value.   The actual
      ;;object is stored in the descriptor itself.
      ((local-etv)
       (local-expand-time-value-binding-descriptor.object descr))
      ;;The given identifier is bound to a compile-time value imported from a library
      ;;or the  top-level environment.  The  actual object  is stored in  the "value"
      ;;field of a loc gensym.
      ((global-etv)
       (global-expand-time-value-binding-descriptor.object descr))
      (else
       ;; (assertion-violation __who__
       ;;   "identifier not bound to an object-type specification"
       ;;   id descr)
       #f))))

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
    ;;If this global binding use is the first  time a binding from LIB is used: visit
    ;;the library.   This makes  sure that  the actual  object is  stored in  the loc
    ;;gensym.
    #;(visit-library lib)
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

(define-syntactic-binding-descriptor-predicate pattern-variable-binding-descriptor?
  pattern-variable)


;;;; done


;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'let-syntax-rules			'scheme-indent-function 1)
;; End:
