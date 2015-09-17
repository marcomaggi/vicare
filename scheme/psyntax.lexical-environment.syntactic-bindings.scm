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

;;Given the entry from a lexical environment: return the gensym acting as label.
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

(define (object-type-name-binding-descriptor? obj)
  ;;Return true  if OBJ is a  syntactic binding descriptor  of a type among  the ones
  ;;having an  instance of "<object-type-spec>"  (or one  of its subtypes)  as value;
  ;;otherwise return  false.  Syntactic identifiers  bound to such  syntactic binding
  ;;descriptors can  be used with  the generic syntaxes: IS-A?,  SLOT-REF, SLOT-SET!,
  ;;METHOD-CALL.
  ;;
  ;;This predicate returns true if OBJ is one among:
  ;;
  ;;   ($record-type-name	. ?record-type-spec)
  ;;   ($scheme-type-name	. ?scheme-type-spec)
  ;;
  (and (pair? obj)
       (case (syntactic-binding-descriptor.type obj)
	 (($record-type-name $scheme-type-name)
	  #t)
	 (else #f))))


;;; helpers

(define-syntax-rule (define-syntactic-binding-descriptor-predicate ?who ?type)
  (define (?who obj)
    ;;Return true if  OBJ is a syntactic binding descriptor  of type ?TYPE; otherwise
    ;;return false.
    ;;
    (and (pair? obj)
	 (eq? (quote ?type) (syntactic-binding-descriptor.type obj)))))

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


;;;; syntactic binding descriptor: lexical variables

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


;;;; syntactic binding descriptor: Vicare struct-type name bindings

(define (make-syntactic-binding-descriptor/struct-type-name std)
  ;;Build and return a syntactic binding descriptor representing a struct-type name.
  ;;
  ;;The argument STD must be the struct-type descriptor itself.
  ;;
  ;;Given the definition:
  ;;
  ;;   (define-struct ?type-name (?field-name ...))
  ;;
  ;;the syntax use DEFINE-STRUCT expands into multiple forms, one of which is:
  ;;
  ;;   (define-syntax ?type-name
  ;;     (make-syntactic-binding-descriptor/struct-type-name ?std))
  ;;
  ;;where   ?STD   is   a   struct-type  descriptor   built   at   expand-time   with
  ;;MAKE-STRUCT-TYPE.  Syntactic bindings of this  type are generated when evaluating
  ;;the right-hand side of a DEFINE-SYNTAX returns the return value of this function.
  ;;
  ;;The returned descriptor has format:
  ;;
  ;;   ($struct-type-name . ?type-descriptor)
  ;;
  (make-syntactic-binding-descriptor $struct-type-name std))

;;Return true if the argument is  a syntactic binding descriptor representing a local
;;or imported syntactic binding describing a struct-type descriptor.
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


;;;; syntactic binding descriptor: old-style core R6RS record-type descriptor binding

(define (core-rtd-binding-descriptor->record-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic  binding  descriptor  from  the  representation  of  a  core
  ;;record-type  name (established  by  the  boot image)  to  a  representation of  a
  ;;record-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-rtd . (?rtd-name ?rcd-name))
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   ($record-type-name . #<record-type-spec>)
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
  (set-car! descriptor '$record-type-name)
  (set-cdr! descriptor
	    (let* ((bindval (syntactic-binding-descriptor.value descriptor))
		   (rtd-id  (core-prim-id (car  bindval)))
		   (rcd-id  (core-prim-id (cadr bindval))))
	      (make-record-type-spec rtd-id rcd-id))))

;;Return true if the argument is a syntactic binding's descriptor representing a core
;;R6RS record-type descriptor established by the boot image.
;;
(define-syntactic-binding-descriptor-predicate core-rtd-binding-descriptor?
  $core-rtd)


;;;; syntactic binding descriptor: new-style core R6RS record-type descriptor binding

(define (core-record-type-name-binding-descriptor->record-type-name-binding-descriptor! descriptor)
  ;;Mutate  a  syntactic  binding  descriptor  from  the  representation  of  a  core
  ;;record-type  name (established  by  the  boot image)  to  a  representation of  a
  ;;record-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-record-type-name . (?rtd-name ?rcd-name ?parent-id
  ;;                              ?constructor-id ?type-predicate-id
  ;;                              ?accessors-alist))
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   ($record-type-name . #<record-type-spec>)
  ;;
  ;;Syntactic binding descriptors of  type "$core-record-type-name" are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  (let* ((bindval (syntactic-binding-descriptor.value descriptor))
	 (rtd-id		(core-prim-id (car  bindval)))
	 (rcd-id		(core-prim-id (cadr bindval)))
	 (super-protocol-id	#f)
	 (parent-id		(cond ((list-ref bindval 2)
				       => core-prim-id)
				      (else #f)))
	 (constructor-sexp	(bless (list-ref bindval 3)))
	 (destructor-sexp	#f)
	 (type-predicate-sexp	(bless (list-ref bindval 4)))
	 (accessors-table	(%alist-ref-or-null bindval 5))
	 (mutators-table	'())
	 (methods-table		accessors-table))
    (set-car! descriptor '$record-type-name)
    (set-cdr! descriptor
	      (make-record-type-spec rtd-id rcd-id super-protocol-id parent-id
				     constructor-sexp destructor-sexp type-predicate-sexp
				     accessors-table mutators-table methods-table))))

;;Return true if the argument is a syntactic binding's descriptor representing a R6RS
;;record-type descriptor established by the boot image.
;;
(define-syntactic-binding-descriptor-predicate core-record-type-name-binding-descriptor?
  $core-record-type-name)


;;;; syntactic binding descriptor: core R6RS condition object record-type descriptor binding

(define (core-condition-object-type-name-binding-descriptor->record-type-name-binding-descriptor! descriptor)
  ;;Mutate a syntactic binding descriptor from the representation of a core condition
  ;;object record-type name (established by the  boot image) to a representation of a
  ;;record-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-condition-object-type-name
  ;;    . (?rtd-name ?rcd-name ?parent-id
  ;;       ?constructor-id ?type-predicate-id ?accessors-alist))
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   ($record-type-name . #<record-type-spec>)
  ;;
  ;;Syntactic  binding  descriptors  of type  "$core-condition-object-type-name"  are
  ;;hard-coded in the boot image and generated directly by the makefile at boot image
  ;;build-time.  Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to
  ;;retrieve the  descriptor from  the label:  this function is  used to  convert the
  ;;descriptor.
  ;;
  (let* ((bindval (syntactic-binding-descriptor.value descriptor))
	 (rtd-id			(core-prim-id (car  bindval)))
	 (rcd-id			(core-prim-id (cadr bindval)))
	 (super-protocol-id		#f)
	 (parent-id			(cond ((list-ref bindval 2)
					       => core-prim-id)
					      (else #f)))
	 (default-constructor-id	(bless (list-ref bindval 3)))
	 (default-destructor-id		#f)
	 (type-predicate-id		(bless (list-ref bindval 4)))
	 (accessors-table		(%alist-ref-or-null bindval 5))
	 (mutators-table		'())
	 (methods-table			accessors-table))
    (set-car! descriptor '$record-type-name)
    (set-cdr! descriptor
	      (make-record-type-spec rtd-id rcd-id super-protocol-id parent-id
				     default-constructor-id default-destructor-id type-predicate-id
				     accessors-table mutators-table methods-table))))

;;Return true if the argument is a syntactic binding's descriptor representing a R6RS
;;condition object record-type descriptor established by the boot image (for example:
;;the built-in condition object types).
;;
(define-syntactic-binding-descriptor-predicate core-condition-object-type-name-binding-descriptor?
  $core-condition-object-type-name)


;;;; syntactic binding descriptor: usable R6RS record-type descriptor binding

(define* (make-syntactic-binding-descriptor/record-type-name {rts record-type-spec?})
  ;;Build and return a syntactic binding descriptor representing a record-type name.
  ;;
  ;;When called with two arguments: RTD-ID must be the syntactic identifier that will
  ;;be  bound to  the record-type  descriptor object;  RCD-ID must  be the  syntactic
  ;;identifier that will be bound to the record-constructor descriptor object.
  ;;
  ;;When called with one argument: RTS must be an instance of "<record-type-spec>".
  ;;
  ;;Given the syntax use:
  ;;
  ;;   (define-record-type ?type-name ?clause ...)
  ;;
  ;;the output form of its expansion contains multiple forms, one of which is:
  ;;
  ;;   (define-syntax ?type-name
  ;;      (make-syntactic-binding-descriptor/record-type-name
  ;;        (make-record-type-spec ?arg ...))
  ;;
  ;;Syntactic bindings of this type are generated when evaluating the right-hand side
  ;;of a DEFINE-SYNTAX returns the return value of this function.
  ;;
  ;;The returned descriptor has the format:
  ;;
  ;;   ($record-type-name . #<record-type-spec>)
  ;;
  (make-syntactic-binding-descriptor $record-type-name rts))

;;Return true if the argument is  a syntactic binding descriptor representing a local
;;or imported binding describing a R6RS record-type descriptor.
;;
(define-syntactic-binding-descriptor-predicate record-type-name-binding-descriptor?
  $record-type-name)


;;;; syntactic binding descriptor: core built-in object-type descriptor binding

(define (core-scheme-type-name-binding-descriptor->scheme-type-name-binding-descriptor! descriptor)
  ;;Mutate a syntactic binding descriptor from  the representation of a core built-in
  ;;object-type  name (established  by  the boot  image) to  a  representation of  an
  ;;object-type  name in  the  format  usable by  the  expander.  Return  unspecified
  ;;values.
  ;;
  ;;We expect the core descriptor to have the format:
  ;;
  ;;   ($core-scheme-type-name
  ;;     . (?parent-name ?constructor-name ?type-predicate-name ?methods-alist))
  ;;
  ;;and the usable descriptor to have the format:
  ;;
  ;;   ($scheme-type-name . #<scheme-type-spec>)
  ;;
  ;;Syntactic binding descriptors of  type "$core-scheme-type-name" are hard-coded in
  ;;the boot image  and generated directly by the makefile  at boot image build-time.
  ;;Whenever the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the
  ;;descriptor from the label: this function is used to convert the descriptor.
  ;;
  ;;These core  descriptors are the ones  bound to the identifiers:  <top>, <fixnum>,
  ;;<string>, et cetera.
  ;;
  (let* ((bindval		(syntactic-binding-descriptor.value descriptor))
	 (parent-id		(cond ((car bindval)
				       => core-prim-id)
				      (else #f)))
	 (constructor.sexp	(bless (list-ref bindval 1)))
	 (type-predicate.sexp	(bless (list-ref bindval 2)))
	 (methods-table		(%alist-ref-or-null bindval 3)))
    (set-car! descriptor '$scheme-type-name)
    (set-cdr! descriptor
	      (make-scheme-type-spec parent-id constructor.sexp type-predicate.sexp methods-table))))

;;Return true if the argument is a syntactic binding's descriptor representing a core
;;built-in object-type descriptor established by the boot image.
;;
(define-syntactic-binding-descriptor-predicate core-scheme-type-name-binding-descriptor?
  $core-scheme-type-name)


;;;; syntactic binding descriptor: usable built-in object type binding

;;Return true  if the  argument is  a syntactic  binding's descriptor  representing a
;;built-in object-type descriptor usable by the expander.
;;
(define-syntactic-binding-descriptor-predicate scheme-type-name-binding-descriptor?
  $scheme-type-name)


;;;; syntactic binding descriptor: fluid syntax bindings

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
  (cadr ?descriptor))

(define* (retrieve-expand-time-value {id identifier?})
  ;;This is the compile-time values  retriever function.  Given an identifier: search
  ;;an  entry in  the lexical  environment; when  found return  its value,  otherwise
  ;;return false.
  ;;
  (let ((descriptor (label->syntactic-binding-descriptor (id->label id) (current-inferior-lexenv))))
    (case (syntactic-binding-descriptor.type descriptor)
      ;;The given  identifier is  bound to  a local  compile-time value.   The actual
      ;;object is stored in the descriptor itself.
      ((local-etv)
       (local-expand-time-value-binding-descriptor.object descriptor))

      ;;The given identifier is bound to a compile-time value imported from a library
      ;;or the  top-level environment.  The  actual object  is stored in  the "value"
      ;;field of a loc gensym.
      ((global-etv)
       (global-expand-time-value-binding-descriptor.object descriptor))

      ;;The given identifier is not bound to a compile-time value.
      (else #f))))

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
  (cadr ?descriptor))

(define-syntax-rule (global-expand-time-value-binding-descriptor.loc ?descriptor)
  (cddr ?descriptor))

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
    (visit-library lib)
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
;; coding: utf-8-unix
;; eval: (put 'let-syntax-rules			'scheme-indent-function 1)
;; End:
