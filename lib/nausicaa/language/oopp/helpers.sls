;;;
;;;Part of: Nausicaa Scheme
;;;Contents: helpers for OOPP
;;;Date: Tue May  1, 2012
;;;
;;;Abstract
;;;
;;;	This  library implements  helper  functions and  macros for  the
;;;	expand phase of the library (nausicaa language oopp).
;;;
;;;	  In  an attempt to  make the  hierarchy of  libraries (nausicaa
;;;	language  oopp  ---)  self-sufficient  we  accept  some  code
;;;	duplication  between  this  library  and the  library  (nausicaa
;;;	language syntax-utilities).
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (nausicaa language oopp helpers)
  (export
    parse-label-definition		parse-label-clauses
    parse-class-definition		parse-class-clauses
    parse-mixin-definition		parse-mixin-clauses
    parse-tag-name-spec			filter-and-validate-mixins-clauses

    parse-with-tags-bindings
    parse-let-bindings			parse-let-values-bindings
    parse-formals-bindings		make-tagged-variable-transformer
    process-method-application		oopp-syntax-transformer

    ;; helpers
    case-symbol				case-identifier
    single-identifier-subst		multi-identifier-subst

    ;; special identifier builders
    tag-id->record-type-id		tag-id->from-fields-constructor-id
    tag-id->constructor-id		tag-id->default-protocol-id
    tag-id->nongenerative-uid		tag-id->private-predicate-id
    tag-id->public-predicate-id		tag-id->list-of-uids-id

    ;; data types

    <parsed-spec>?
    <parsed-spec>-name-id		<parsed-spec>-member-identifiers
    <parsed-spec>-definitions		<parsed-spec>-public-constructor-id
    <parsed-spec>-public-predicate-id	<parsed-spec>-private-predicate-id
    <parsed-spec>-parent-id		<parsed-spec>-concrete-fields
    <parsed-spec>-virtual-fields	<parsed-spec>-methods-table
    <parsed-spec>-getter		<parsed-spec>-setter
    <parsed-spec>-abstract?		<parsed-spec>-sealed?
    <parsed-spec>-opaque?		<parsed-spec>-maker-transformer
    <parsed-spec>-finaliser-expression
    <parsed-spec>-shadowed-identifier
    <parsed-spec>-nongenerative-uid	<parsed-spec>-mutable-fields-data
    <parsed-spec>-immutable-fields-data	<parsed-spec>-concrete-fields-data
    <parsed-spec>-concrete-fields-names	<parsed-spec>-common-protocol
    <parsed-spec>-public-protocol	<parsed-spec>-super-protocol
    <parsed-spec>-satisfactions
    <parsed-spec>-accessor-transformer	<parsed-spec>-mutator-transformer

    <class-spec>?
    <class-spec>-record-type-id		<class-spec>-satisfaction-clauses

    <label-spec>?
    <label-spec>-satisfaction-clauses

    <mixin-spec>?
    <mixin-spec>-clauses

    <field-spec>?
    <field-spec>-name-id		<field-spec>-tag-id
    <field-spec>-accessor-id		<field-spec>-mutator-id

    <concrete-field-spec>?		<virtual-field-spec>?
    )
  (import (for (vicare) run expand)
    (for (only (rnrs)
	       lambda define define-syntax set!)
      (meta -1))
    (vicare unsafe operations)
    (vicare language-extensions identifier-substitutions)
    (prefix (only (nausicaa language oopp configuration)
		  enable-satisfactions)
	    config.)
    (for (nausicaa language oopp auxiliary-syntaxes)
      (meta -1))
    (for (prefix (only (nausicaa language auxiliary-syntaxes)
		       parent		nongenerative
		       sealed		opaque
		       predicate	abstract
		       fields		virtual-fields
		       mutable		immutable
		       method		method-syntax		methods
		       protocol		public-protocol		super-protocol
		       getter		setter
		       shadows		satisfies
		       mixins
		       maker		finaliser)
		 aux.)
      (meta -1)))


;;;; private helpers

(define-auxiliary-syntaxes
  parser-name
  clause-keyword
  next-who
  body
  flag-accessor)


;;;; public helpers

(define-syntax case-symbol
  ;;Like  CASE defined  by R6RS,  but specialised  to branch  on symbols
  ;;using EQ?.
  ;;
  (syntax-rules (else)
    ((_ ?expr
	((?symbol0 ?symbol ...)
	 ?sym-body0 ?sym-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((sym ?expr))
       (cond ((or (eq? sym '?symbol0)
		  (eq? sym '?symbol)
		  ...)
	      ?sym-body0 ?sym-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?symbol0 ?symbol ...)
	 ?sym-body0 ?sym-body ...)
	...)
     (let ((sym ?expr))
       (cond ((or (eq? sym '?symbol0)
		  (eq? sym '?symbol)
		  ...)
	      ?sym-body0 ?sym-body ...)
	     ...)))
    ))

(define-syntax case-identifier
  ;;Like CASE defined by R6RS,  but specialised to branch on identifiers
  ;;using FREE-IDENTIIFER=?.
  ;;
  (syntax-rules (else)
    ((_ ?expr
	((?id0 ?id ...)
	 ?id-body0 ?id-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((sym ?expr))
       (cond ((or (free-identifier=? sym #'?id0)
		  (free-identifier=? sym #'?id)
		  ...)
	      ?id-body0 ?id-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?id0 ?id ...)
	 ?id-body0 ?id-body ...)
	...)
     (let ((sym ?expr))
       (cond ((or (free-identifier=? sym #'?id0)
		  (free-identifier=? sym #'?id)
		  ...)
	      ?id-body0 ?id-body ...)
	     ...)))
    ))


;;;; public helpers: visible identifiers composition

(define (make-method-identifier tag-id method-id)
  (identifier-append tag-id
		     (identifier->string tag-id)
		     "-"
		     (identifier->string method-id)))

;;; --------------------------------------------------------------------

(define-inline (tag-id->record-type-id id)
  (identifier-suffix id "-record-type"))

(define-inline (tag-id->nongenerative-uid id)
  id)

;;; --------------------------------------------------------------------

(define-inline (tag-id->from-fields-constructor-id id)
  (identifier-prefix "make-from-fields-" id))

(define-inline (tag-id->constructor-id id)
  (identifier-prefix "make-" id))

(define-inline (tag-id->common-rcd-id id)
  (identifier-suffix id "-common-rcd"))

(define-inline (tag-id->common-constructor-id id)
  (identifier-prefix "make-" id))

;;; --------------------------------------------------------------------

(define-inline (tag-id->superclass-rcd-id id)
  (identifier-suffix id "-superclass-rcd"))

;;; --------------------------------------------------------------------

(define-inline (tag-id->custom-maker-rcd-id id)
  (identifier-suffix id "-custom-maker-rcd"))

(define-inline (tag-id->custom-maker-constructor-id id)
  (identifier-prefix "make-" id))

(define-inline (tag-id->default-protocol-id id)
  (identifier-suffix id "-default-protocol"))

;;; --------------------------------------------------------------------

(define-inline (tag-id->private-predicate-id id)
  (identifier-suffix id "-private-predicate"))

(define-inline (tag-id->public-predicate-id id)
  (identifier-suffix id "?"))

;;; --------------------------------------------------------------------

(define-inline (tag-id->list-of-uids-id id)
  (identifier-suffix id "-list-of-uids"))


(define* (make-tagged-variable-transformer (tag-id identifier?) (src-var-id identifier?))
  ;;Build  and   return  the   transformer  function   implementing  the
  ;;identifier syntax  for tagged  variables.  When  we define  a tagged
  ;;variable with:
  ;;
  ;;   (<integer> a 123)
  ;;
  ;;we can imagine the following expansion:
  ;;
  ;;   (define src-var 123)
  ;;   (define-syntax a
  ;;     (make-tagged-variable-transformer #'<integer> #'src-var))
  ;;
  ;;and when we define a tagged variable with:
  ;;
  ;;   (let/tags (((a <integer>) 123)) ---)
  ;;
  ;;we can imagine the following expansion:
  ;;
  ;;   (let ((src-var 123))
  ;;     (let-syntax
  ;;         ((a (make-tagged-variable-transformer
  ;;               #'<integer> #'src-var)))
  ;;       ---))
  ;;
  ;;TAG-ID must be the identifier  bound to the tag syntax (defined with
  ;;DEFINE-LABEL or DEFINE-CLASS).
  ;;
  ;;SRC-VAR-ID must be  the identifier to which the  value of the tagged
  ;;variable is bound.
  ;;
  (make-variable-transformer
   (lambda (stx)
     (syntax-case stx (set!)

       ;;Syntax to reference the value of the binding.
       (?var
	(identifier? #'?var)
	src-var-id)

       ;;Syntax to mutate the value of the binding.
       ((set! ?var ?val)
	#`(set! #,src-var-id (#,tag-id :assert-type-and-return ?val)))

       (?form
	#`(#,tag-id #:oopp-syntax ?form))

       (_
	(syntax-violation (syntax->datum tag-id)
	  "invalid tagged-variable syntax use" stx))))))

(define (oopp-syntax-transformer tag-id form-stx synner)
  ;;This function is  called to expand the usage of  OOPP syntax, either
  ;;through a tagged variable:
  ;;
  ;;  (?tag ?id ?expr)
  ;;  (?id ?arg ...) ;OOPP syntax
  ;;
  ;;or directly by using the #:oopp-syntax keyword:
  ;;
  ;;  (?tag #:oopp-syntax (?expr ?arg ...))
  ;;
  (syntax-case form-stx ()

    ;;Syntax to apply the field mutator for the tag of ?VAR.
    ((?expr :mutator (?field-name ?arg ...) ?value)
     (identifier? #'?field-name)
     #`(#,tag-id :mutator ?expr (?field-name ?arg ...) ?value))

    ;;Syntax to apply the setter for the tag of ?VAR.
    ((?expr :setter ((?key ...) ...) ?value)
     #`(#,tag-id :setter (?expr ((?key ...) ...) ?value)))

    ;;Syntax to  apply the getter for  the tag of ?VAR.   A plain getter
    ;;syntax is as follows:
    ;;
    ;;  (?var (?key0 ...) (?key ...) ...)
    ;;
    ;;where "<tag>"  is the  tag assigned to  the getter  runtime return
    ;;value from the getter transformer function.
    ;;
    ((?expr (?key0 ...) (?key ...) ...)
     #`(#,tag-id :getter (?expr ((?key0 ...) (?key ...) ...))))

    ;;Syntax to apply a method or reference a field of the tag of ?VAR.
    ((?expr . ?stuff)
     #`(#,tag-id :dispatch (?expr . ?stuff)))

    (_
     (synner "invalid OOPP syntax" form-stx))))


(define (process-method-application rv-tag-id application-stx)
  ;;Process a tag's method application.   RV-TAG-ID must be false of the
  ;;tag   identifier   of   the   single   application   return   value.
  ;;APPLICATION-STX  must be  a  syntax object  representing the  method
  ;;application.
  ;;
  ;;When there  is no  return-value tag or  the method  returns multiple
  ;;values: RV-TAG-ID  must be false.  In  this case we just  return the
  ;;application syntax object.
  ;;
  ;;When the method  has a single tagged return value:  we want to allow
  ;;OOPP syntax  for the returned  value.  For example, knowing  the the
  ;;method SUBVECTOR of "<vector>" has return value with tag "<vector>":
  ;;
  ;;  (<vector> V '#(0 1 2 3))
  ;;
  ;;  (V subvector 0 2) => #(0 1)	;plain method application
  ;;  ((V subvector 0 2) length) => 2	;return value OOPP syntax
  ;;
  ;;we want the expansion:
  ;;
  ;;  (V subvector 0 2)
  ;;  ==> (splice-first-expand
  ;;       (<vector> #:flat-oopp-syntax (subvector V 0 1)))
  ;;
  ;;so that  if the  application is  the first  subform of  an enclosing
  ;;subform and there are arguments, the full expansion is:
  ;;
  ;;  ((V subvector 0 2) length)
  ;;  ==> (<vector> #:flat-oopp-syntax (subvector V 0 1) length)
  ;;  ==> (<vector> #:oopp-syntax ((subvector V 0 1) length))
  ;;  ==> (vector-length (subvector V 0 1))
  ;;
  ;;otherwise the expansion is just:
  ;;
  ;;  (begin (V subvector 0 2))
  ;;  ==> (begin
  ;;       (splice-first-expand
  ;;        (<vector> #:flat-oopp-syntax (subvector V 0 1))))
  ;;  ==> (begin
  ;;       (<vector> #:flat-oopp-syntax (subvector V 0 1)))
  ;;  ==> (begin (subvector V 0 1))
  ;;
  (if (syntax->datum rv-tag-id)
      #`(#,rv-tag-id #:nested-oopp-syntax #,application-stx)
    application-stx))


(define parse-with-tags-bindings
  (case-lambda
   ((bindings-stx synner)
    (parse-with-tags-bindings bindings-stx synner '() '() '()))
   ((bindings-stx synner vars tags syntax-bindings)
    ;;Recursive   function.   Parse   the  syntax   object  BINDINGS-STX
    ;;expecting it to be a  list of tagged WITH-TAGS bindings; supported
    ;;syntaxes for the bindings are:
    ;;
    ;;   ()
    ;;   (?var0 ?var ...)
    ;;
    ;;where each ?VAR must have the following syntax:
    ;;
    ;;   (?var-id ?tag-id)
    ;;   #(?var-id ?tag-id)
    ;;
    ;;The return value is a syntax object with the structure:
    ;;
    ;;   ((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
    ;;
    ;;where each  VAR is an identifier  to be used to  create a binding,
    ;;each TAG is the identifier of the type tag and each SYNTAX-BINDING
    ;;is the  associated LET-SYNTAX  binding.
    ;;
    ;;SYNNER must be a closure to be used to raise syntax violations.
    ;;
    (syntax-case bindings-stx ()
      ;;No more bindings.
      (()
       (list (reverse vars) (reverse tags) (reverse syntax-bindings)))

      ;;Tagged binding, parentheses envelope.
      (((?var ?tag) . ?other-bindings)
       (and (identifier? #'?var)
	    (identifier? #'?tag))
       (let ((tag-id #'?tag))
	 (parse-with-tags-bindings #'?other-bindings synner
				   (cons #'?var vars)
				   (cons tag-id tags)
				   (cons #'(?var (make-tagged-variable-transformer #'?tag #'?var))
					 syntax-bindings))))

      ;;Tagged binding, vector envelope.
      ((#(?var ?tag) . ?other-bindings)
       (and (identifier? #'?var)
	    (identifier? #'?tag))
       (let ((tag-id #'?tag))
	 (parse-with-tags-bindings #'?other-bindings synner
				   (cons #'?var vars)
				   (cons tag-id tags)
				   (cons #'(?var (make-tagged-variable-transformer #'?tag #'?var))
					 syntax-bindings))))

      ;;Syntax error.
      (_
       (synner "invalid bindings syntax" bindings-stx))))))

(define parse-let-bindings
  (case-lambda
   ((bindings-stx top-id synner)
    (parse-let-bindings bindings-stx top-id synner '() '() '()))
   ((bindings-stx top-id synner vars tags syntax-bindings)
    ;;Recursive   function.   Parse   the  syntax   object  BINDINGS-STX
    ;;expecting  it to  be  a  list of  tagged  LET bindings;  supported
    ;;syntaxes for the bindings are:
    ;;
    ;;   ()
    ;;   (?var0 ?var ...)
    ;;
    ;;where each ?VAR must have one of the following syntaxes:
    ;;
    ;;   ?var-id
    ;;   (?var-id)
    ;;   (?var-id ?tag-id)
    ;;   #(?var-id ?tag-id)
    ;;
    ;;The return value is a syntax object with the structure:
    ;;
    ;;   ((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
    ;;
    ;;where each  VAR is an identifier  to be used to  create a binding,
    ;;each TAG is the identifier of the type tag and each SYNTAX-BINDING
    ;;is the  associated LET-SYNTAX  binding.  If  a variable  is tagged
    ;;with TOP-ID: no syntax binding is generated.
    ;;
    ;;When the BINDINGS-STX comes from a LET, the returned syntax object
    ;;should be used to compose an output form as:
    ;;
    ;;   #'(let ((VAR (TAG :assert-type-and-return ?init)) ...)
    ;;       (let-syntax (SYNTAX-BINDING ...)
    ;;         ?body0 ?body ...))
    ;;
    ;;TOP-ID must the the identifier bound  to the "<top>" tag; this tag
    ;;is used  as default when  no tag is  given for a  binding.  SYNNER
    ;;must be a closure to be used to raise syntax violations.
    ;;
    (syntax-case bindings-stx ()
      ;;No more bindings.
      (()
       (list (reverse vars) (reverse tags) (reverse syntax-bindings)))

      ;;Tagged binding, parentheses envelope.
      (((?var ?tag) . ?other-bindings)
       (and (identifier? #'?var)
	    (identifier? #'?tag))
       (let ((tag-id #'?tag))
	 (parse-let-bindings #'?other-bindings top-id synner
			     (cons #'?var vars)
			     (cons tag-id tags)
			     (if (free-identifier=? tag-id top-id)
				 syntax-bindings
			       (cons #'(?var (make-tagged-variable-transformer #'?tag #'?var))
				     syntax-bindings)))))

      ;;Tagged binding, vector envelope.
      ((#(?var ?tag) . ?other-bindings)
       (and (identifier? #'?var)
	    (identifier? #'?tag))
       (let ((tag-id #'?tag))
	 (parse-let-bindings #'?other-bindings top-id synner
			     (cons #'?var vars)
			     (cons tag-id tags)
			     (if (free-identifier=? tag-id top-id)
				 syntax-bindings
			       (cons #'(?var (make-tagged-variable-transformer #'?tag #'?var))
				     syntax-bindings)))))

      ;;Non-tagged binding.
      ((?var . ?other-bindings)
       (identifier? #'?var)
       (parse-let-bindings #'?other-bindings top-id synner
			   (cons #'?var vars)
			   (cons top-id tags)
			   syntax-bindings))

      ;;Special case of non-tagged binding in parens.
      ;;
      ;;FIXME Why are we supporting this?  Is there some special case I do
      ;;not rememeber?  (Marco Maggi; Thu Jul 18, 2013)
      (((?var) . ?other-bindings)
       (identifier? #'?var)
       (parse-let-bindings #'?other-bindings top-id synner
			   (cons #'?var vars)
			   (cons top-id tags)
			   syntax-bindings))

      ;;Syntax error.
      (_
       (synner "invalid bindings syntax" bindings-stx))))))

(define parse-let-values-bindings
  (case-lambda
   ((bindings-stx top-id synner)
    (parse-let-values-bindings bindings-stx top-id synner '() '()))
   ((bindings-stx top-id synner values-vars syntax-bindings)
    ;;Recursive   function.   Parse   the  syntax   object  BINDINGS-STX
    ;;expecting it to be a list of tagged LET-VALUES bindings; supported
    ;;syntaxes for the bindings are:
    ;;
    ;;   ()
    ;;   (?vars0 ?vars ...)
    ;;
    ;;where  each  ?VARS must  have  the  syntax  of the  tagged  LAMBDA
    ;;formals:
    ;;
    ;;   (?var0 ?var ...)
    ;;   (?var0 ?var ... . ?rest)
    ;;   ?var-id
    ;;
    ;;and each ?VAR must have one of the following syntaxes:
    ;;
    ;;   ?var-id
    ;;   (?var-id)
    ;;   (?var-id ?tag-id)
    ;;   #(?var-id ?tag-id)
    ;;
    ;;The return value is a syntax object with the structure:
    ;;
    ;;   ((VARS ...) (SYNTAX-BINDING ...))
    ;;
    ;;where each  VARS is  a list  of identifiers to  be used  to create
    ;;bindings  and the  SYNTAX-BINDING  are  the associated  LET-SYNTAX
    ;;bindings.  If a variable is  tagged with TOP-ID: no syntax binding
    ;;is generated.
    ;;
    ;;When the BINDINGS-STX comes from a LET-VALUES, the returned syntax
    ;;object should be used to compose an output form as:
    ;;
    ;;   #'(let-values ((VARS ?init) ...)
    ;;       (let-syntax (SYNTAX-BINDING ...)
    ;;         ?body0 ?body ...))
    ;;
    ;;TOP-ID must the the identifier bound  to the "<top>" tag; this tag
    ;;is used  as default when  no tag is  given for a  binding.  SYNNER
    ;;must be a closure to be used to raise syntax violations.
    ;;
    (define-inline (%final stx)
      (reverse (syntax->list stx)))
    (syntax-case bindings-stx ()
      (()
       (list (%final values-vars) (%final syntax-bindings)))

      ((?vars . ?other-bindings)
       (with-syntax
	   (((FORMALS VALIDATIONS (SYNTAX-BINDING ...))
	     (parse-formals-bindings #'?vars top-id synner)))
	 (parse-let-values-bindings #'?other-bindings top-id synner
				    #`(FORMALS . #,values-vars)
				    #`(SYNTAX-BINDING ... . #,syntax-bindings))))

      (_
       (synner "invalid bindings syntax" bindings-stx))))))

(define (parse-formals-bindings formals-stx top-id synner)
  ;;Parse the  syntax object  FORMALS-STX expecting it  to be a  list of
  ;;tagged LAMBDA formals; supported syntaxes for the formals are:
  ;;
  ;;   ()
  ;;   ?args-id
  ;;   #(?args-id ?tag-id)
  ;;   (?var0 ?var ...)
  ;;   (?var0 ?var ... . ?rest)
  ;;
  ;;where  ?ARGS-ID is an  identifier, each  ?VAR must  have one  of the
  ;;following syntaxes:
  ;;
  ;;   ?var-id
  ;;   (?var-id)
  ;;   (?var-id ?tag-id)
  ;;   #(?var-id ?tag-id)
  ;;
  ;;and ?REST must have one of the following syntaxes:
  ;;
  ;;   ?rest-id
  ;;   #(?rest-id ?tag-id)
  ;;
  ;;notice  that  tagging  for  ?ARGS-ID  is not  supported  because  no
  ;;suitable syntax is possible.
  ;;
  ;;The return value is a syntax object with the structure:
  ;;
  ;;   (FORMALS (VALIDATION ...) (SYNTAX-BINDING ...))
  ;;
  ;;the FORMALS  component represents a  valid list of  formal arguments
  ;;for  a LAMBDA  syntax;  each VALIDATION  component  is a  validation
  ;;clause; each SYNTAX-BINDING component represents a valid binding for
  ;;LET-SYNTAX.  If a variable is  tagged with TOP-ID: no syntax binding
  ;;is generated.
  ;;
  ;;When the FORMALS-STX comes from a LAMBDA, the returned syntax object
  ;;should be used to compose an output form as:
  ;;
  ;;   #'(lambda FORMALS (let-syntax (SYNTAX-BINDING ...) ?body0 ?body ...))
  ;;
  ;;When  the  FORMALS-STX  comes  from  a clause  of  CASE-LAMBDA,  the
  ;;returned syntax object should be used to compose an output form as:
  ;;
  ;;   #'(case-lambda
  ;;      (FORMALS (let-syntax (SYNTAX-BINDING ...) ?body0 ?body ...))
  ;;      ...)
  ;;
  ;;TOP-ID must  the the  identifier bound to  the "<top>"  tag.  SYNNER
  ;;must be a closure to be used to raise syntax violations.
  ;;
  (syntax-case formals-stx ()
    ;;No arguments.
    (()
     #'(() () ()))

    ;;List of all the arguments, with tag.
    (#(?args-id ?tag-id)
     (and (identifier? #'?args-id)
	  (identifier? #'?tag-id))
     (with-syntax ((((VAR) (TAG) (BINDING))
		    (parse-let-bindings #'((?args-id ?tag-id)) top-id synner)))
       #'(VAR ((TAG VAR)) (BINDING))))

    ;;List of all the arguments, no tag.
    (?args-id
     (identifier? #'?args-id)
     #'(?args-id () ()))

    ;;Fixed number of arguments.
    ((?var ...)
     (with-syntax ((((VAR ...) (TAG ...) (BINDING ...))
		    (parse-let-bindings #'(?var ...) top-id synner)))
       #'((VAR ...) ((TAG VAR) ...) (BINDING ...))))

    ;;Mandatory arguments plus untagged rest argument.
    ((?var0 ?var ... . ?rest)
     (identifier? #'?rest)
     (with-syntax ((((REST-VAR VAR ...) (REST-TAG TAG ...) (BINDING ...))
		    (parse-let-bindings #'(?rest ?var0 ?var ...) top-id synner)))
       #'((VAR ... . REST-VAR) ((TAG VAR) ...) (BINDING ...))))

    ;;Mandatory arguments plus tagged rest argument.
    ((?var0 ?var ... . #(?rest-id ?tag-id))
     (and (identifier? #'?rest)
	  (identifier? #'?tag-id))
     (with-syntax ((((REST-VAR VAR ...) (REST-TAG TAG ...) (BINDING ...))
		    (parse-let-bindings #'(#(?rest-id ?tag-id) ?var0 ?var ...) top-id synner)))
       #'((VAR ... . REST-VAR) ((TAG VAR) ... (REST-TAG REST-VAR)) (BINDING ...))))

    ))


;;;; data types: parsed clauses representation

;;This record type represents the parsing results of all the clauses for
;;all the tag definitions.  Some  clauses apply only to classes and some
;;clauses apply  only to  labels, but  we do not  represent them  in the
;;class and label specification record type: the mixins must support all
;;the clauses, so all the parsing results go here for simplicity.
;;
(define-record-type <parsed-spec>
  (nongenerative nausicaa:language:oopp:<parsed-spec>)
  (protocol
   (lambda (make-instance)
     (lambda (name-id top-id lambda-id)
       (assert (identifier? name-id))
       (make-instance name-id top-id lambda-id
	 '() #;member-identifiers	'() #;definitions
	 #f  #;abstract?		#f  #;public-constructor-id
	 #f  #;public-predicate-id	#f  #;private-predicate-id
	 #f  #;common-protocol		#f  #;public-protocol
	 #f  #;super-protocol
	 #f  #;parent-id
	 '() #;concrete-fields		'() #;virtual-fields
	 '() #;methods-table
	 #f  #;sealed?			#f  #;opaque?
	 #f  #;getter			#f  #;setter
	 #f  #;nongenerative-uid	#f  #;maker-transformer
	 #f  #;finaliser-expression
	 #f  #;shadowed-identifier	'() #;satisfactions
	 ))))
  (fields (immutable name-id)
	  (immutable top-id)
	  (immutable lambda-id)

	  (mutable member-identifiers)
		;Null or  a proper list of  identifiers representing the
		;names of  the members of  this label/class/mixin.  This
		;list  is  used to  check  for  duplicate names  between
		;fields and methods.

	  (mutable definitions)
		;Null or  a proper  list of syntax  objects representing
		;function  definition forms  or syntax  definition forms
		;for  methods,   getter,  setter  and   virtual  fields'
		;accessors and mutators.

	  (mutable abstract?)
		;Boolean value.  When true  the class is abstract and it
		;cannot  be  instantiated;   when  false  the  class  is
		;concrete and it can be instantiated.
		;
		;An abstract  class cannot have a common  protocol nor a
		;public protocol:  when this field is set  to true, both
		;the COMMON-PROTOCOL and  PUBLIC-PROTOCOL fields are set
		;to false.

	  (mutable public-constructor-id)
		;Identifier  to   be  used   as  name  for   the  public
		;constructor.

	  (mutable public-predicate-id)
		;Identifier  to  be  used  as  name  for  the  predicate
		;function or syntax.

	  (mutable private-predicate-id)
		;Identifier  to  be  used  as  name  for  the  predicate
		;function or syntax.

	  (mutable common-protocol)
		;False  or   syntax  object  containing   an  expression
		;evaluating to the common constructor protocol.

	  (mutable public-protocol)
		;False  or   syntax  object  containing   an  expression
		;evaluating to the public constructor protocol.

	  (mutable super-protocol)
		;False  or   syntax  object  containing   an  expression
		;evaluating to the  superclass or superlabel constructor
		;protocol.

	  (mutable parent-id)
		;False or an identifier  bound to the parent tag syntax.
		;When false: the value will default to "<top>".

	  (mutable concrete-fields)
		;Null or a proper list of <CONCRETE-FIELD-SPEC> records.
		;The  order  of the  records  in  the  list matches  the
		;definition order.   Notice that fields  from the mixins
		;come last.

	  (mutable virtual-fields)
		;Null or a  proper list of <VIRTUAL-FIELD-SPEC> records.
		;The  order  of  the   record  is  the  reverse  of  the
		;definition order.

	  (mutable methods-table)
		;Null  or   an  associative  list   having:  identifiers
		;representing method names as keys, alist as value.  The
		;first item in  the list value is the  tag identifier of
		;the  single return  value from  the adddress  or #f  if
		;there  is no  such tag.   The second  item in  the list
		;value is an identifier representing the method function
		;name or syntax name as values.
		;
		;  ((?method-name-id . (?rv-tag ?method-implementation-id))
		;   ...)
		;
		;The  order  of  the  entries  is  the  reverse  of  the
		;definition order.

	  (mutable sealed?)
		;Boolean value.  When true the record type is sealed.

	  (mutable opaque?)
		;Boolean value.  When true the record type is opaque.

	  (mutable getter)
		;False or a syntax  object representing an expression to
		;evaluate once,  at expand  time, to acquire  the getter
		;syntax function.

	  (mutable setter)
		;False or a syntax  object representing an expression to
		;evaluate once,  at expand  time, to acquire  the setter
		;syntax function.

	  (mutable nongenerative-uid)
		;A symbol  uniquely identifying  this type in  the whole
		;program.    When  non-false:   this   record  type   is
		;nongenerative.

	  (mutable maker-transformer)
		;False  or   a  syntax  object   holding  an  expression
		;evaluating to  a macro transformer to be  used to parse
		;the maker syntax.

	  (mutable finaliser-expression)
		;False  or   a  syntax  object  holding   an  expression
		;evaluating to function to  be used as record destructor
		;by the garbage collector.

	  (mutable shadowed-identifier)
		;False or identifier.  It is the identifier to insert in
		;place   of  the   label  tag   identifier   when  using
		;WITH-LABEL-SHADOWING.

	  (mutable satisfactions)
		;Null or  list of identifiers being  syntax keywords for
		;satisfactions.
	  ))

(define-record-type <class-spec>
  (nongenerative nausicaa:language:oopp:<class-spec>)
  (parent <parsed-spec>)
  (fields (immutable record-type-id)
		;Identifier to  be used for the actual  R6RS record type
		;in the automatically composed DEFINE-RECORD-TYPE form.
	  )
  (protocol
   (lambda (make-spec)
     (lambda (name-id top-id lambda-id)
       ((make-spec name-id top-id lambda-id)
	(tag-id->record-type-id name-id))))))

(define-record-type <label-spec>
  (nongenerative nausicaa:language:oopp:<label-spec>)
  (parent <parsed-spec>)
  (protocol
   ;;R6RS mandates that a record type  with custom RCD must have a custom
   ;;RCD, not a default one.
   (lambda (make-spec)
     (lambda (name-id top-id lambda-id)
       ((make-spec name-id top-id lambda-id))))))

(define-record-type <mixin-spec>
  (nongenerative nausicaa:language:oopp:<mixin-spec>)
  (parent <parsed-spec>)
  (fields (immutable clauses)
		;The  syntax  object  representing  the clauses  in  the
		;DEFINE-MIXIN form.
	  )
  (protocol
   ;;R6RS mandates that a record type  with custom RCD must have a custom
   ;;RCD, not a default one.
   (lambda (make-spec)
     (lambda (name-id top-id lambda-id clauses)
       ((make-spec name-id top-id lambda-id) clauses)))))


;;;; data types: field specification

(define-record-type <field-spec>
  (nongenerative nausicaa:language:oopp:helpers:<field-spec>)
  (protocol
   (lambda (make-record)
     (lambda (name acc mut tag)
       (assert (identifier? name))
       (assert (identifier? acc))
       (assert (or (not mut) (identifier? mut)))
       (assert (or (not tag) (identifier? tag)))
       (make-record name acc mut tag))))
  (fields (immutable name-id)
		;Identifier representing the field name.
	  (immutable accessor-id)
		;Identifier  representing  the   accessor  name;  it  is
		;automatically built when not specified.
	  (immutable mutator-id)
		;For an  immutable field:  false.  For a  mutable field:
		;identifier  representing  the   accessor  name;  it  is
		;automatically built when not specified.
	  (immutable tag-id)
		;Identifier  representing the type  tag for  this field.
		;If  no type  tag is  specified  in the  label or  class
		;definition: the parser functions must set this field to
		;the top tag identifier.
	  ))

(define-record-type <concrete-field-spec>
  (nongenerative nausicaa:language:oopp:helpers:<concrete-field-spec>)
  (parent <field-spec>)
  (protocol
   (lambda (make-field-spec)
     (lambda (name acc mut tag)
       ((make-field-spec name acc mut tag))))))

(define-record-type <virtual-field-spec>
  (nongenerative nausicaa:language:oopp:helpers:<virtual-field-spec>)
  (parent <field-spec>)
  (protocol
   (lambda (make-field-spec)
     (lambda (name acc mut tag)
       ((make-field-spec name acc mut tag))))))


;;;; data type methods

(define (<parsed-spec>-member-identifiers-cons! parsed-spec id what-string synner)
  ;;Add  the  identifier  ID  to  the  list  of  member  identifiers  in
  ;;PARSED-SPEC.  If such identifier  is already present: raise a syntax
  ;;violation.
  ;;
  (let ((member-identifiers (<parsed-spec>-member-identifiers parsed-spec)))
    (cond ((identifier-memq id member-identifiers free-identifier=?)
	   => (lambda (summy)
		(synner (string-append what-string " conflicts with other member name") id)))
	  (else
	   (<parsed-spec>-member-identifiers-set! parsed-spec (cons id member-identifiers))))))

(define-inline (<parsed-spec>-definitions-cons! parsed-spec definition)
  ;;Prepend a definition form to the list of definitions in PARSED-SPEC.
  ;;
  (assert (<parsed-spec>? parsed-spec))
  ($<parsed-spec>-definitions-set! parsed-spec (cons definition ($<parsed-spec>-definitions parsed-spec))))

(define-inline (<parsed-spec>-concrete-fields-cons! parsed-spec field-record)
  ;;Prepend a  field record  to the  list of  concrete field  records in
  ;;PARSED-SPEC.
  ;;
  (assert (<parsed-spec>? parsed-spec))
  ($<parsed-spec>-concrete-fields-set! parsed-spec (cons field-record ($<parsed-spec>-concrete-fields parsed-spec))))

(define-inline (<parsed-spec>-virtual-fields-cons! parsed-spec field-record)
  ;;Prepend  a field  record to  the list  of virtual  field  records in
  ;;PARSED-SPEC.
  ;;
  (assert (<parsed-spec>? parsed-spec))
  ($<parsed-spec>-virtual-fields-set! parsed-spec (cons field-record ($<parsed-spec>-virtual-fields parsed-spec))))

(define-inline (<parsed-spec>-methods-table-cons! parsed-spec method-name-id method-rv-tag-id method-implementation-id)
  ;;Prepend a  method name  identifier to  the list  of method  names in
  ;;PARSED-SPEC.
  ;;
  (assert (<parsed-spec>? parsed-spec))
  ($<parsed-spec>-methods-table-set! parsed-spec (cons (list method-name-id method-rv-tag-id method-implementation-id)
						       ($<parsed-spec>-methods-table parsed-spec))))

;;; --------------------------------------------------------------------

(define (<parsed-spec>-mutable-fields-data spec)
  ;;Select the mutable fields among  the concrete and virtual fields and
  ;;return a list of lists with the format:
  ;;
  ;;	((?field-name ?accessor ?mutator ?tag) ...)
  ;;
  (assert (<parsed-spec>? spec))
  (map (lambda (field-record)
	 (list ($<field-spec>-name-id		field-record)
	       ($<field-spec>-accessor-id	field-record)
	       ($<field-spec>-mutator-id	field-record)
	       ($<field-spec>-tag-id		field-record)))
    (filter (lambda (field-record)
	      ($<field-spec>-mutator-id field-record))
      (if (<class-spec>? spec)
	  (append ($<parsed-spec>-concrete-fields spec)
		  ($<parsed-spec>-virtual-fields spec))
	($<parsed-spec>-virtual-fields spec)))))

(define (<parsed-spec>-unsafe-mutable-fields-data spec)
  ;;Select the  mutable fields  among the concrete  fields and  return a
  ;;list of lists with the format:
  ;;
  ;;	((?field-name ?unsafe-field-name ?tag) ...)
  ;;
  (assert (<parsed-spec>? spec))
  (if (<class-spec>? spec)
      (map (lambda (field-record)
	     (define field-name-id
	       ($<field-spec>-name-id field-record))
	     (list field-name-id
		   (identifier-prefix "$" field-name-id)
		   ($<field-spec>-tag-id field-record)))
	(filter (lambda (field-record)
		  ($<field-spec>-mutator-id field-record))
	  ($<parsed-spec>-concrete-fields spec)))
    '()))

;;; --------------------------------------------------------------------

(define (<parsed-spec>-immutable-fields-data spec)
  ;;Select the  immutable fields among  the concrete and  virtual fields
  ;;and return a list of lists with the format:
  ;;
  ;;	((?field-name ?unsafe-field-name ?accessor ?tag) ...)
  ;;
  (assert (<parsed-spec>? spec))
  (map (lambda (field-record)
	 (list ($<field-spec>-name-id		field-record)
	       ($<field-spec>-accessor-id	field-record)
	       ($<field-spec>-tag-id		field-record)))
    (filter (lambda (field-record)
	      (not ($<field-spec>-mutator-id field-record)))
      (if (<class-spec>? spec)
	  (append ($<parsed-spec>-concrete-fields spec)
		  ($<parsed-spec>-virtual-fields  spec))
	($<parsed-spec>-virtual-fields spec)))))

(define (<parsed-spec>-unsafe-immutable-fields-data spec)
  ;;Select the immutable  fields among the concrete fields  and return a
  ;;list of lists with the format:
  ;;
  ;;	((?field-name ?unsafe-field-name ?tag) ...)
  ;;
  (assert (<parsed-spec>? spec))
  (if (<class-spec>? spec)
      (map (lambda (field-record)
	     (define field-name-id
	       ($<field-spec>-name-id field-record))
	     (list field-name-id
		   (identifier-prefix "$" field-name-id)
		   ($<field-spec>-tag-id field-record)))
	(filter (lambda (field-record)
		  (not ($<field-spec>-mutator-id field-record)))
	  ($<parsed-spec>-concrete-fields spec)))
    '()))

;;; --------------------------------------------------------------------

(define (<parsed-spec>-concrete-fields-data spec)
  ;;Take the concrete fields and return a list of lists with the format:
  ;;
  ;;   (?field-spec ...)
  ;;
  ;;where ?FIELD-SPEC has one of the formats:
  ;;
  ;;   (mutable   ?field-name ?accessor ?mutator)
  ;;   (immutable ?field-name ?accessor)
  ;;
  ;;The returned  list can  be used  as content for  a FIELDS  clause of
  ;;DEFINE-RECORD-TYPE as defined by R6RS.
  ;;
  (assert (<parsed-spec>? spec))
  (map (lambda (field-record)
	 (let ((name-id     ($<field-spec>-name-id     field-record))
	       (accessor-id ($<field-spec>-accessor-id field-record))
	       (mutator     ($<field-spec>-mutator-id  field-record)))
	   (if mutator
	       (list #'aux.mutable name-id accessor-id mutator)
	     (list #'aux.immutable name-id accessor-id))))
    ($<parsed-spec>-concrete-fields spec)))

(define (<parsed-spec>-concrete-fields-names spec)
  ;;Take the concrete fields and return a list with the format:
  ;;
  ;;   (?field-name ...)
  ;;
  ;;where ?FIELD-NAME is an identifier representing a field name.
  ;;
  (assert (<parsed-spec>? spec))
  (map (lambda (field-record)
	 ($<field-spec>-name-id field-record))
    ($<parsed-spec>-concrete-fields spec)))

;;; --------------------------------------------------------------------

(define (<label-spec>-satisfaction-clauses spec)
  (assert (<parsed-spec>? spec))
  (let-values (((virtual-mutable-fields virtual-immutable-fields)
		(%field-spec-satisfaction-clauses ($<parsed-spec>-virtual-fields  spec))))
    (list (list ($<parsed-spec>-name-id spec)
		($<parsed-spec>-public-constructor-id spec)
		($<parsed-spec>-public-predicate-id spec))
	  (list #'aux.parent		($<parsed-spec>-parent-id spec))
	  (cons #'aux.virtual-fields	virtual-mutable-fields)
	  (cons #'aux.virtual-fields	virtual-immutable-fields)
	  (cons #'aux.methods		($<parsed-spec>-methods-table spec))
	  (list #'aux.getter		($<parsed-spec>-getter spec))
	  (list #'aux.setter		($<parsed-spec>-setter spec))
	  (list #'aux.nongenerative	($<parsed-spec>-nongenerative-uid spec))
	  (list #'aux.shadows		($<parsed-spec>-shadowed-identifier spec))
	  )))

(define (<class-spec>-satisfaction-clauses spec)
  (assert (<parsed-spec>? spec))
  (let-values (((concrete-mutable-fields concrete-immutable-fields)
		(%field-spec-satisfaction-clauses ($<parsed-spec>-concrete-fields spec)))
	       ((virtual-mutable-fields virtual-immutable-fields)
		(%field-spec-satisfaction-clauses ($<parsed-spec>-virtual-fields  spec))))
    (list (list ($<parsed-spec>-name-id spec)
		($<parsed-spec>-public-constructor-id	spec)
		($<parsed-spec>-public-predicate-id	spec)
		($<class-spec>-record-type-id		spec))
	  (list #'aux.parent		($<parsed-spec>-parent-id		spec))
	  (cons #'aux.fields		concrete-mutable-fields)
	  (cons #'aux.fields		concrete-immutable-fields)
	  (cons #'aux.virtual-fields	virtual-mutable-fields)
	  (cons #'aux.virtual-fields	virtual-immutable-fields)
	  (cons #'aux.methods		($<parsed-spec>-methods-table		spec))
	  (list #'aux.getter		($<parsed-spec>-getter			spec))
	  (list #'aux.setter		($<parsed-spec>-setter			spec))
	  (list #'aux.nongenerative	($<parsed-spec>-nongenerative-uid	spec))
	  (list #'aux.sealed		($<parsed-spec>-sealed?			spec))
	  (list #'aux.opaque		($<parsed-spec>-opaque?			spec))
	  (list #'aux.abstract		($<parsed-spec>-abstract?		spec))
	  )))

(define (%field-spec-satisfaction-clauses fields)
  (receive (mutables immutables)
      (partition <field-spec>-mutator-id fields)
    (values (map (lambda (field-record)
		   (let ((name     ($<field-spec>-name-id     field-record))
			 (tag      ($<field-spec>-tag-id      field-record))
			 (accessor ($<field-spec>-accessor-id field-record))
			 (mutator  ($<field-spec>-mutator-id  field-record)))
		     #`(aux.mutable (#,name #,tag) #,accessor #,mutator)))
	      mutables)
	    (map (lambda (field-record)
		   (let ((name     ($<field-spec>-name-id     field-record))
			 (tag      ($<field-spec>-tag-id      field-record))
			 (accessor ($<field-spec>-accessor-id field-record)))
		     #`(aux.immutable (#,name #,tag) #,accessor #f)))
	      immutables))))


;;;; tag accessor and mutator transformers

(define (<parsed-spec>-accessor-transformer spec)
  ;;Given  the "<parsed-spec>"  instance  SPEC: return  a syntax  object
  ;;representing the accessor transformer function for the tag; whenever
  ;;a tagged variable is referenced in a form like:
  ;;
  ;;   (?var ?arg0 ?arg ...)
  ;;
  ;;first the symbol ?ARG0 is compared  to the names of the tag methods:
  ;;if it matches the form is  a method call; if no method name matches,
  ;;the form is handed to the accessor transformer function to attempt a
  ;;match between ?ARG0 and a field  name; if no field name matches, the
  ;;form is handed to the parent  tag to attempt a match with the parent
  ;;tag's members.
  ;;
  ;;Let's consider the example:
  ;;
  ;;   (define-label <alpha>
  ;;     (fields a)
  ;;     (getter ---)
  ;;     (method (do O) ---))
  ;;
  ;;   (define-label <beta>
  ;;     (fields (b <alpha>)))
  ;;
  ;;   (define-label <gamma>
  ;;     (fields (c <beta>)))
  ;;
  ;;   (<gamma> O)
  ;;
  ;;where  O is  the tagged  variable; the  following  syntax expansions
  ;;should happen:
  ;;
  ;;   (O c)
  ;;   ---> (<gamma>-c O)
  ;;
  ;;   (O c b)
  ;;   ---> (<beta>-b (<gamma>-c O))
  ;;
  ;;   (O c b a)
  ;;   ---> (<alpha>-a (<beta>-b (<gamma>-c O)))
  ;;
  ;;we  also want  to support  nested  getter invocations,  that is  the
  ;;following expansion should happen:
  ;;
  ;;   (O c b[123])
  ;;   ---> (<alpha> :let (<gamma>-c (<beta>-b O)) G0 (G0[123]))
  ;;
  ;;we  also want  to support  nested  method invocations,  that is  the
  ;;following expansion should hapen:
  ;;
  ;;   (O c b do)
  ;;   ---> (<alpha> :let (<beta>-b (<gamma>-c O)) G0 (G0 do))
  ;;
  ;;To make all of this happen the accessor transformer has to implement
  ;;the following expansions:
  ;;
  ;;   (O c)
  ;;   ---> (<gamma>-c O)
  ;;
  ;;   (O c b)
  ;;   ---> (<beta> :let (<gamma>-c O) G0 (G0 b))
  ;;
  ;;   (O c b a)
  ;;   ---> (<beta> :let (<gamma>-c O) G0 (G0 b a))
  ;;
  ;;   (O c b[123])
  ;;   ---> (<beta> :let (<gamma>-c O) G0 (G0 b[123]))
  ;;
  ;;   (O c b do)
  ;;   ---> (<beta> :let (<gamma>-c O) G0 (G0 b do))
  ;;
  ;;and then, recursively,  the tagged variable G0 (a  gensym) will take
  ;;care of doing what is needed.
  ;;
  ;;Notice that the  getter syntax is handled by  the getter transformer
  ;;function, not by the accessor function.
  ;;
  (with-syntax
      ((THE-TAG
	(<parsed-spec>-name-id spec))
       (THE-PARENT
	(<parsed-spec>-parent-id spec))
       (THE-RECORD-TYPE
	(if (<class-spec>? spec)
	    (<class-spec>-record-type-id spec)
	  #f))
       (((IMMUTABLE-FIELD IMMUTABLE-ACCESSOR IMMUTABLE-TAG) ...)
	(<parsed-spec>-immutable-fields-data spec))
       (((MUTABLE-FIELD MUTABLE-ACCESSOR MUTABLE-MUTATOR MUTABLE-TAG) ...)
	(<parsed-spec>-mutable-fields-data spec))
       (((IMMUTABLE-CONCRETE-FIELD UNSAFE-IMMUTABLE-CONCRETE-FIELD IMMUTABLE-CONCRETE-TAG) ...)
	(<parsed-spec>-unsafe-immutable-fields-data spec))
       (((MUTABLE-CONCRETE-FIELD UNSAFE-MUTABLE-CONCRETE-FIELD MUTABLE-CONCRETE-TAG) ...)
	(<parsed-spec>-unsafe-mutable-fields-data spec)))
    #'(lambda (original-stx expr-stx args-stx)
	;;Process  a  field  accessor  form in  which:  EXPR-STX  is  an
	;;expression  evaluating to  a  Scheme object  of type  THE-TAG;
	;;ARGS-STX is the list of  syntax objects representing the field
	;;name and additional subordinate arguments.
	;;
	;;ORIGINAL-STX is  a syntax  object holding the  original syntax
	;;use that generated the accessor call.
	;;
	(syntax-case args-stx ()

	  ;;Try to match the access to a field.
	  ((??field-name)
	   (identifier? #'??field-name)
	   (case-symbol (syntax->datum #'??field-name)
	     ;;Safe accessors.
	     ((IMMUTABLE-FIELD)
	      #`(IMMUTABLE-ACCESSOR #,expr-stx))
	     ...
	     ((MUTABLE-FIELD)
	      #`(MUTABLE-ACCESSOR   #,expr-stx))
	     ...
	     ;;Unsafe accessors.
	     ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
	      #`($record-type-field-ref THE-RECORD-TYPE IMMUTABLE-CONCRETE-FIELD #,expr-stx))
	     ...
	     ((UNSAFE-MUTABLE-CONCRETE-FIELD)
	      #`($record-type-field-ref THE-RECORD-TYPE   MUTABLE-CONCRETE-FIELD #,expr-stx))
	     ...
	     (else
	      #`(THE-PARENT :dispatch (#,expr-stx ??field-name)))))

	  ;;Try to match a field name followed by the getter syntax.
	  ((??field-name (??key0 (... ...)) (??key (... ...)) (... ...))
	   (identifier? #'??field-name)
	   (with-syntax ((KEYS #'((??key0 (... ...)) (??key (... ...)) (... ...))))
	     (case-symbol (syntax->datum #'??field-name)
	       ;;Safe accessors.
	       ((IMMUTABLE-FIELD)
		#`(IMMUTABLE-TAG :let (IMMUTABLE-ACCESSOR #,expr-stx) o (o . KEYS)))
	       ...
	       ((MUTABLE-FIELD)
		#`(MUTABLE-TAG   :let (MUTABLE-ACCESSOR   #,expr-stx) o (o . KEYS)))
	       ...
	       ;;Unsafe accessors.
	       ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
		#`(IMMUTABLE-CONCRETE-TAG :let ($record-type-field-ref THE-RECORD-TYPE IMMUTABLE-CONCRETE-FIELD #,expr-stx) o (o . KEYS)))
	       ...
	       ((UNSAFE-MUTABLE-CONCRETE-FIELD)
		#`(MUTABLE-CONCRETE-TAG   :let ($record-type-field-ref THE-RECORD-TYPE   MUTABLE-CONCRETE-FIELD #,expr-stx) o (o . KEYS)))
	       ...
	       (else
		#`(THE-PARENT :dispatch (#,expr-stx ??field-name . KEYS))))))

	  ;;Try to match a field name followed by arguments.
	  ((??field-name . ??args)
	   (identifier? #'??field-name)
	   (case-symbol (syntax->datum #'??field-name)
	     ;;Safe accessors.
	     ((IMMUTABLE-FIELD)
	      #`(IMMUTABLE-TAG :let (IMMUTABLE-ACCESSOR #,expr-stx) o (o . ??args)))
	     ...
	     ((MUTABLE-FIELD)
	      #`(MUTABLE-TAG   :let (MUTABLE-ACCESSOR   #,expr-stx) o (o . ??args)))
	     ...
	     ;;Unsafe accessors.
	     ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
	      #`(IMMUTABLE-CONCRETE-TAG :let ($record-type-field-ref THE-RECORD-TYPE IMMUTABLE-CONCRETE-FIELD #,expr-stx) o (o . ??args)))
	     ...
	     ((UNSAFE-MUTABLE-CONCRETE-FIELD)
	      #`(MUTABLE-CONCRETE-TAG   :let ($record-type-field-ref THE-RECORD-TYPE   MUTABLE-CONCRETE-FIELD #,expr-stx) o (o . ??args)))
	     ...
	     (else
	      #`(THE-PARENT :dispatch (#,expr-stx ??field-name . ??args)))))

	  (_
	   (syntax-violation 'THE-TAG "invalid :accessor tag syntax" original-stx))))))

(define (<parsed-spec>-mutator-transformer spec)
  ;;Given  the "<parsed-spec>"  instance  SPEC: return  a syntax  object
  ;;representing the mutator transformer  function for the tag; whenever
  ;;a tagged variable is referenced in a form like:
  ;;
  ;;   (set!/tags (?var ?arg0 ?arg ...) ?val)
  ;;
  ;;the is handed to the mutator transformer function to attempt a match
  ;;between ?ARG0 and  a field name; if no field  name matches, the form
  ;;is handed to the parent tag's mutator transformer.
  ;;
  ;;Let's consider the example:
  ;;
  ;;   (define-label <alpha>
  ;;     (fields a)
  ;;     (setter ---))
  ;;
  ;;   (define-label <beta>
  ;;     (fields (b <alpha>)))
  ;;
  ;;   (define-label <gamma>
  ;;     (fields (c <beta>)))
  ;;
  ;;   (<gamma> O)
  ;;
  ;;where  O is  the tagged  variable; the  following  syntax expansions
  ;;should happen:
  ;;
  ;;   (set!/tags (O c) 99)
  ;;   ---> (<gamma>-c-set! O 99)
  ;;
  ;;   (set!/tags (O c b) 99)
  ;;   ---> (<beta>-b-set! (<gamma>-c O) 99)
  ;;
  ;;   (set!/tags (O c b a) 99)
  ;;   ---> (<alpha>-a-set! (<beta>-b (<gamma>-c O)) 99)
  ;;
  ;;we  also want  to support  nested  setter invocations,  that is  the
  ;;following expansion should happen:
  ;;
  ;;   (set!/tags (O c b[77]) 99)
  ;;   ---> (<alpha> :let (<gamma>-c (<beta>-b O)) G0 (set!/tags (G0[77]) 99))
  ;;
  (with-syntax
      ((THE-TAG
	(<parsed-spec>-name-id spec))
       (THE-PARENT
	(<parsed-spec>-parent-id spec))
       (THE-RECORD-TYPE
	(if (<class-spec>? spec)
	    (<class-spec>-record-type-id spec)
	  #f))
       (((IMMUTABLE-FIELD IMMUTABLE-ACCESSOR IMMUTABLE-TAG) ...)
	(<parsed-spec>-immutable-fields-data spec))
       (((MUTABLE-FIELD MUTABLE-ACCESSOR MUTABLE-MUTATOR MUTABLE-TAG) ...)
	(<parsed-spec>-mutable-fields-data spec))
       (((IMMUTABLE-CONCRETE-FIELD UNSAFE-IMMUTABLE-CONCRETE-FIELD IMMUTABLE-CONCRETE-TAG) ...)
	(<parsed-spec>-unsafe-immutable-fields-data spec))
       (((MUTABLE-CONCRETE-FIELD UNSAFE-MUTABLE-CONCRETE-FIELD MUTABLE-CONCRETE-TAG) ...)
	(<parsed-spec>-unsafe-mutable-fields-data spec)))
    #'(lambda (stx expr-stx keys-stx value-stx)
	;;Process a mutator form equivalent to:
	;;
	;;   (set!/tags (?var ?field-name0 ?arg ...) ?value)
	;;
	;;which has been previously decomposed so that KEYS-STX is:
	;;
	;;   (?field-name0 ?arg ...)
	;;
	;;and  VALUE-STX is  ?VALUE.  ?VAR  is a  tagged  variable whose
	;;actual value is in EXPR-STX.
	;;
	;;ORIGINAL-STX is  a syntax  object holding the  original syntax
	;;use that generated the mutator call.
	;;
	(syntax-case keys-stx ()
	  ((??field-name)
	   (identifier? #'??field-name)
	   (case-symbol (syntax->datum #'??field-name)
	     ;;Safe mutators.
	     ((MUTABLE-FIELD)
	      #`(MUTABLE-MUTATOR #,expr-stx (MUTABLE-TAG :assert-type-and-return #,value-stx)))
	     ...
	     ((IMMUTABLE-FIELD)
	      (syntax-violation 'THE-TAG "attempt to mutate immutable field" stx #'IMMUTABLE-FIELD))
	     ...
	     ;;Unsafe mutators.
	     ((UNSAFE-MUTABLE-CONCRETE-FIELD)
	      #`($record-type-field-set! THE-RECORD-TYPE MUTABLE-CONCRETE-FIELD #,expr-stx
					 (MUTABLE-CONCRETE-TAG :assert-type-and-return #,value-stx)))
	     ...
	     ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
	      (syntax-violation 'THE-TAG "attempt to mutate immutable field" stx #'IMMUTABLE-CONCRETE-FIELD))
	     ...
	     (else
	      #`(THE-PARENT :mutator #,expr-stx (??field-name) #,value-stx))))

	  ;;Try to  match a  field name followed  by the  getter syntax.
	  ;;This is the setter syntax  with the keys *not* enclosed in a
	  ;;list.
	  ((??field-name (??key0 (... ...)) (??key (... ...)) (... ...))
	   (identifier? #'??field-name)
	   (with-syntax ((KEYS #'((??key0 (... ...)) (??key (... ...)) (... ...))))
	     (case-symbol (syntax->datum #'??field-name)
	       ;;Safe mutators.
	       ((MUTABLE-FIELD)
		#`(MUTABLE-TAG   :let (MUTABLE-ACCESSOR   #,expr-stx) o
				 (MUTABLE-TAG   :setter (o KEYS #,value-stx))))
	       ...
	       ((IMMUTABLE-FIELD)
		#`(IMMUTABLE-TAG :let (IMMUTABLE-ACCESSOR #,expr-stx) o
				 (IMMUTABLE-TAG :setter (o KEYS #,value-stx))))
	       ...
	       ;;Unsafe mutators.
	       ((UNSAFE-MUTABLE-CONCRETE-FIELD)
		#`(MUTABLE-CONCRETE-TAG   :let ($record-type-field-ref THE-RECORD-TYPE   MUTABLE-CONCRETE-FIELD #,expr-stx) o
					  (MUTABLE-CONCRETE-TAG   :setter (o KEYS #,value-stx))))
	       ...
	       ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
		#`(IMMUTABLE-CONCRETE-TAG :let ($record-type-field-ref THE-RECORD-TYPE IMMUTABLE-CONCRETE-FIELD #,expr-stx) o
					  (IMMUTABLE-CONCRETE-TAG :setter (o KEYS #,value-stx))))
	       ...
	       (else
		#`(THE-PARENT :mutator #,expr-stx #,keys-stx #,value-stx)))))

	  ;;Try to match a field name followed by arguments.
	  ((??field-name0 ??arg (... ...))
	   (identifier? #'??field-name0)
	   (case-symbol (syntax->datum #'??field-name0)
	     ;;Safe mutators.
	     ((MUTABLE-FIELD)
	      #`(MUTABLE-TAG   :mutator (MUTABLE-ACCESSOR   #,expr-stx)
			       (??arg (... ...)) #,value-stx))
	     ...
	     ((IMMUTABLE-FIELD)
	      #`(IMMUTABLE-TAG :mutator (IMMUTABLE-ACCESSOR #,expr-stx)
			       (??arg (... ...)) #,value-stx))
	     ...
	     ;;Unsafe mutators.
	     ((UNSAFE-MUTABLE-CONCRETE-FIELD)
	      #`(MUTABLE-CONCRETE-TAG   :mutator ($record-type-field-ref THE-RECORD-TYPE   MUTABLE-CONCRETE-FIELD #,expr-stx)
					(??arg (... ...)) #,value-stx))
	     ...
	     ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
	      #`(IMMUTABLE-CONCRETE-TAG :mutator ($record-type-field-ref THE-RECORD-TYPE IMMUTABLE-CONCRETE-FIELD #,expr-stx)
					(??arg (... ...)) #,value-stx))
	     ...
	     (else
	      #`(THE-PARENT :mutator #,expr-stx (??field-name0 ??arg (... ...)) #,value-stx))))

	  (_
	   (syntax-violation 'THE-TAG "invalid :mutator tag syntax" stx))))))


;;;; common clause parser definer: at-most-once clause, single argument

(define-syntax define-clause-parser/at-most-once-clause/single-argument
  (lambda (stx)
    (syntax-case stx (parser-name clause-keyword next-who body flag-accessor)
      ((_ (parser-name ?who)
	  (clause-keyword ?clause-keyword)
	  (flag-accessor ?flag-accessor)
	  (body ?body0 ?body ...))
       (let ((context #'?who))
	 (define (stx->stx stx)
	   (datum->syntax context (syntax->datum stx)))
	 (define-syntax sym->stx
	   (syntax-rules ()
	     ((_ ?sym)
	      (datum->syntax context ?sym))))
	 (with-syntax
	     ((BODY		(stx->stx #'(begin ?body0 ?body ...)))
	      (PARSED-SPEC	(sym->stx 'parsed-spec))
		;Identifier bound to the <PARSED-SPEC> record instance.
	      (INPUT-CLAUSES	(sym->stx 'input-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially unwrapped input clauses.
	      (OUTPUT-CLAUSES	(sym->stx 'output-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially  unwrapped  output  clauses;  these  are  the
		;clauses to be handed to the next parser function.
	      (SINGLE-ARGUMENT	(sym->stx '?single-argument))
		;The  single  argument   from  the  successfully  parsed
		;clause.
	      (SYNNER		(sym->stx 'synner))
		;Identifier bound to the  function used to report syntax
		;violations.
	      (%RECURSE		(sym->stx '%recurse))
		;Identifier bound to the syntax used to recursively call
		;this function.
	      )
	   #'(define (?who PARSED-SPEC INPUT-CLAUSES next-parsers SYNNER OUTPUT-CLAUSES)
	       ;;Tail-recursive  function.  Parse  clauses  with keyword
	       ;;?CLAUSE-KEYWORD  and mutate  the appropriate  fields of
	       ;;PARSED-SPEC  with the  result;  raise an  error if  the
	       ;;clause is  used multiple times; the clause  must have a
	       ;;single argument.
	       ;;
	       ;;After parsing:
	       ;;
	       ;;* If no more clauses are present: return null.
	       ;;
	       ;;*  If more  clauses  are present:  tail-call the  first
	       ;;function in  the NEXT-PARSERS list  to continue parsing
	       ;;of clauses.
	       ;;
	       (define-inline (%RECURSE input-clauses output-clauses)
		 (?who PARSED-SPEC input-clauses next-parsers SYNNER output-clauses))
	       (cond
		;;No more input clauses.
		((null? INPUT-CLAUSES)
		 (if (null? OUTPUT-CLAUSES)
		     (final-clauses-parser PARSED-SPEC (reverse OUTPUT-CLAUSES) '() SYNNER '())
		   ((car next-parsers) PARSED-SPEC (reverse OUTPUT-CLAUSES)
		    (cdr next-parsers) SYNNER '())))
		;;Parse matching clause.
		((free-identifier=? #'?clause-keyword (caar INPUT-CLAUSES))
		 (if (?flag-accessor PARSED-SPEC)
		     (SYNNER "clause expected at most once is used multiple times, \
                            or clauses specifying the same features are used together"
			     ($car INPUT-CLAUSES))
		   (syntax-case ($cdar INPUT-CLAUSES) ()
		     ((SINGLE-ARGUMENT)
		      (begin BODY (%RECURSE ($cdr INPUT-CLAUSES) OUTPUT-CLAUSES)))
		     (_
		      (SYNNER "a single-argument clause was expected" ($car INPUT-CLAUSES))))))
		;;Parse non-matching clause.
		(else
		 (%RECURSE ($cdr INPUT-CLAUSES)
			   (cons ($car INPUT-CLAUSES) OUTPUT-CLAUSES)))))
	   )))
      )))


;;;; common clause parser definer: at-most-once clause, no argument

(define-syntax define-clause-parser/at-most-once-clause/no-argument
  (lambda (stx)
    (syntax-case stx (parser-name clause-keyword next-who body flag-accessor)
      ((_ (parser-name ?who)
	  (clause-keyword ?clause-keyword)
	  (flag-accessor ?flag-accessor)
	  (body ?body0 ?body ...))
       (let ((context #'?who))
	 (define (stx->stx stx)
	   (datum->syntax context (syntax->datum stx)))
	 (define-syntax sym->stx
	   (syntax-rules ()
	     ((_ ?sym)
	      (datum->syntax context ?sym))))
	 (with-syntax
	     ((BODY		(stx->stx #'(begin ?body0 ?body ...)))
	      (PARSED-SPEC	(sym->stx 'parsed-spec))
		;Identifier bound to the <PARSED-SPEC> record instance.
	      (INPUT-CLAUSES	(sym->stx 'input-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially unwrapped input clauses.
	      (OUTPUT-CLAUSES	(sym->stx 'output-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially  unwrapped  output  clauses;  these  are  the
		;clauses to be handed to the next parser function.
	      (SYNNER		(sym->stx 'synner))
		;Identifier bound to the  function used to report syntax
		;violations.
	      (%RECURSE		(sym->stx '%recurse))
		;Identifier bound to the syntax used to recursively call
		;this function.
	      )
	   #'(define (?who PARSED-SPEC INPUT-CLAUSES next-parsers SYNNER OUTPUT-CLAUSES)
	       ;;Tail-recursive  function.  Parse  clauses  with keyword
	       ;;?CLAUSE-KEYWORD  and mutate  the appropriate  fields of
	       ;;PARSED-SPEC  with the  result;  raise an  error if  the
	       ;;clause is used multiple  times; the clause must have no
	       ;;arguments.
	       ;;
	       ;;After parsing:
	       ;;
	       ;;* If no more clauses are present: return null.
	       ;;
	       ;;*  If more  clauses  are present:  tail-call the  first
	       ;;function in  the NEXT-PARSERS list  to continue parsing
	       ;;of clauses.
	       ;;
	       (define-inline (%RECURSE input-clauses output-clauses)
		 (?who PARSED-SPEC input-clauses next-parsers SYNNER output-clauses))
	       (cond
		;;No more input clauses.
		((null? INPUT-CLAUSES)
		 (if (null? OUTPUT-CLAUSES)
		     (final-clauses-parser PARSED-SPEC (reverse OUTPUT-CLAUSES) '() SYNNER '())
		   ((car next-parsers) PARSED-SPEC (reverse OUTPUT-CLAUSES)
		    (cdr next-parsers) SYNNER '())))
		;;Parse matching clause.
		((free-identifier=? #'?clause-keyword (caar INPUT-CLAUSES))
		 (if (?flag-accessor PARSED-SPEC)
		     (SYNNER "clause expected at most once is used multiple times, \
                              or clauses specifying the same features are used together"
			     ($car INPUT-CLAUSES))
		   (syntax-case ($cdar INPUT-CLAUSES) ()
		     (()
		      (begin BODY (%RECURSE ($cdr INPUT-CLAUSES) OUTPUT-CLAUSES)))
		     (_
		      (SYNNER "a single-argument clause was expected" ($car INPUT-CLAUSES))))))
		;;Parse non-matching clause.
		(else
		 (%RECURSE ($cdr INPUT-CLAUSES)
			   (cons ($car INPUT-CLAUSES) OUTPUT-CLAUSES)))))
	   )))
      )))


;;;; common clause parser definer: zero or more clauses with one or more arguments

(define-syntax define-clause-parser/zero-or-more-clauses/zero-or-more-arguments
  (lambda (stx)
    (syntax-case stx (parser-name clause-keyword next-who body)
      ((_ (parser-name ?who)
	  (clause-keyword ?clause-keyword)
	  (body ?body0 ?body ...))
       (let ((context #'?who))
	 (define (stx->stx stx)
	   (datum->syntax context (syntax->datum stx)))
	 (define-syntax sym->stx
	   (syntax-rules ()
	     ((_ ?sym)
	      (datum->syntax context ?sym))))
	 (with-syntax
	     ((BODY		(stx->stx #'(begin ?body0 ?body ...)))
	      (PARSED-SPEC	(sym->stx 'parsed-spec))
		;Identifier bound to the <PARSED-SPEC> record instance.
	      (INPUT-CLAUSES	(sym->stx 'input-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially unwrapped input clauses.
	      (OUTPUT-CLAUSES	(sym->stx 'output-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially  unwrapped  output  clauses;  these  are  the
		;clauses to be handed to the next parser function.
	      (ARGUMENTS	(sym->stx '?arguments))
		;The  multiple arguments  from  the successfully  parsed
		;clause.
	      (SYNNER		(sym->stx 'synner))
		;Identifier bound to the  function used to report syntax
		;violations.
	      (%RECURSE		(sym->stx '%recurse))
		;Identifier bound to the syntax used to recursively call
		;this function.
	      )
	   #'(define (?who PARSED-SPEC INPUT-CLAUSES next-parsers SYNNER OUTPUT-CLAUSES)
	       ;;Tail-recursive  function.  Parse  clauses  with keyword
	       ;;?CLAUSE-KEYWORD  and mutate  the appropriate  fields of
	       ;;PARSED-SPEC  with the  result; accept  multiple clauses
	       ;;having multiple arguments.
	       ;;
	       ;;After parsing:
	       ;;
	       ;;* If no more clauses are present: return null.
	       ;;
	       ;;*  If more  clauses  are present:  tail-call the  first
	       ;;function in  the NEXT-PARSERS list  to continue parsing
	       ;;of clauses.
	       ;;
	       (define-inline (%RECURSE input-clauses output-clauses)
		 (?who PARSED-SPEC input-clauses next-parsers SYNNER output-clauses))
	       (cond
		;;No more input clauses.
		((null? INPUT-CLAUSES)
		 (if (null? OUTPUT-CLAUSES)
		     (final-clauses-parser PARSED-SPEC (reverse OUTPUT-CLAUSES) '() SYNNER '())
		   ((car next-parsers) PARSED-SPEC (reverse OUTPUT-CLAUSES)
		    (cdr next-parsers) SYNNER '())))
		;;Parse matching clause.
		((free-identifier=? #'?clause-keyword (caar INPUT-CLAUSES))
		 (syntax-case ($cdar INPUT-CLAUSES) ()
		   (()
		    (%RECURSE ($cdr INPUT-CLAUSES) OUTPUT-CLAUSES))
		   (ARGUMENTS
		    (begin BODY (%RECURSE ($cdr INPUT-CLAUSES) OUTPUT-CLAUSES)))))
		;;Parse non-matching clause.
		(else
		 (%RECURSE ($cdr INPUT-CLAUSES)
			   (cons ($car INPUT-CLAUSES) OUTPUT-CLAUSES)))))
	   )))
      )))


;;;; common clause parser definer: zero or more clauses with multiple arguments

(define-syntax define-clause-parser/zero-or-more-clauses/multiple-mandatory-arguments
  (lambda (stx)
    (syntax-case stx (parser-name clause-keyword next-who body)
      ((_ (parser-name ?who)
	  (clause-keyword ?clause-keyword)
	  (body ?body0 ?body ...))
       (let ((context #'?who))
	 (define (stx->stx stx)
	   (datum->syntax context (syntax->datum stx)))
	 (define-syntax sym->stx
	   (syntax-rules ()
	     ((_ ?sym)
	      (datum->syntax context ?sym))))
	 (with-syntax
	     ((BODY		(stx->stx #'(begin ?body0 ?body ...)))
	      (PARSED-SPEC	(sym->stx 'parsed-spec))
		;Identifier bound to the <PARSED-SPEC> record instance.
	      (INPUT-CLAUSES	(sym->stx 'input-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially unwrapped input clauses.
	      (OUTPUT-CLAUSES	(sym->stx 'output-clauses))
		;Identifier  bound  to  the  syntax object  holding  the
		;partially  unwrapped  output  clauses;  these  are  the
		;clauses to be handed to the next parser function.
	      (ARGUMENTS	(sym->stx '?arguments))
		;The  multiple arguments  from  the successfully  parsed
		;clause.
	      (SYNNER		(sym->stx 'synner))
		;Identifier bound to the  function used to report syntax
		;violations.
	      (%RECURSE		(sym->stx '%recurse))
		;Identifier bound to the syntax used to recursively call
		;this function.
	      )
	   #'(define (?who PARSED-SPEC INPUT-CLAUSES next-parsers SYNNER OUTPUT-CLAUSES)
	       ;;Tail-recursive  function.  Parse  clauses  with keyword
	       ;;?CLAUSE-KEYWORD  and mutate  the appropriate  fields of
	       ;;PARSED-SPEC  with the  result; accept  multiple clauses
	       ;;having multiple arguments.
	       ;;
	       ;;After parsing:
	       ;;
	       ;;* If no more clauses are present: return null.
	       ;;
	       ;;*  If more  clauses  are present:  tail-call the  first
	       ;;function in  the NEXT-PARSERS list  to continue parsing
	       ;;of clauses.
	       ;;
	       (define-inline (%RECURSE input-clauses output-clauses)
		 (?who PARSED-SPEC input-clauses next-parsers SYNNER output-clauses))
	       (cond
		;;No more input clauses.
		((null? INPUT-CLAUSES)
		 (if (null? OUTPUT-CLAUSES)
		     (final-clauses-parser PARSED-SPEC (reverse OUTPUT-CLAUSES) '() SYNNER '())
		   ((car next-parsers) PARSED-SPEC (reverse OUTPUT-CLAUSES)
		    (cdr next-parsers) SYNNER '())))
		;;Parse matching clause.
		((free-identifier=? #'?clause-keyword (caar INPUT-CLAUSES))
		 (syntax-case ($cdar INPUT-CLAUSES) ()
		   (()
		    (SYNNER "invalid empty clause" ($car INPUT-CLAUSES)))
		   (ARGUMENTS
		    (begin BODY (%RECURSE ($cdr INPUT-CLAUSES) OUTPUT-CLAUSES)))))
		;;Parse non-matching clause.
		(else
		 (%RECURSE ($cdr INPUT-CLAUSES)
			   (cons ($car INPUT-CLAUSES) OUTPUT-CLAUSES)))))
	   )))
      )))


;;;; clauses verification and unwrapping

(define (%verify-and-partially-unwrap-clauses clauses synner)
  ;;Non-tail  recursive  function.   Parse  the  syntax  object  CLAUSES
  ;;validating  it as  list  of clauses;  return  a partially  unwrapped
  ;;syntax object holding the same clauses.
  ;;
  ;;The structure of  the returned object is "list  of pairs" whose cars
  ;;are identifiers  representing a clauses' keyword and  whose cdrs are
  ;;syntax objects representing the clauses' arguments.  Input clauses:
  ;;
  ;;   #<syntax expr=((?id . ?args) ...)>
  ;;
  ;;output clauses:
  ;;
  ;;   ((#<syntax expr=?id> . #<syntax expr=?args>) ...)
  ;;
  (syntax-case clauses ()
    (() '())

    (((?keyword . ?args) . ?other-input-clauses)
     (identifier? #'?keyword)
     (cons (cons #'?keyword #'?args)
	   (%verify-and-partially-unwrap-clauses #'?other-input-clauses synner)))

    ((?input-clause . ?other-input-clauses)
     (synner "invalid clause syntax" #'?input-clause))))


;;;; parsers entry points: class definition

(define (parse-class-definition stx top-id lambda-id synner)
  ;;Parse the  full class definition form  in the syntax  object STX and
  ;;return an instance of record type "<class-spec>".
  ;;
  ;;TOP-ID must  be an identifier  bound to the "<top>"  tag.  LAMBDA-ID
  ;;must be  an identifier bound  to the LAMBDA macro  supporting tagged
  ;;formal  arguments.  SYNNER must  be a  closure to  be used  to raise
  ;;syntax violations.
  ;;
  ;;Notice  how  we delegate  to  the  function PARSE-CLASS-CLAUSES  the
  ;;responsibility of building the  "<class-spec>" record: this makes it
  ;;easiers to test the parser functions.
  ;;
  (syntax-case stx ()
    ((_ ?tag-spec ?clause ...)
     (let-values (((name-id public-constructor-id predicate-id)
		   (parse-tag-name-spec #'?tag-spec synner)))
       (let ((spec (parse-class-clauses #'(?clause ...) name-id top-id lambda-id synner)))
	 (<parsed-spec>-public-constructor-id-set! spec public-constructor-id)
	 (<parsed-spec>-public-predicate-id-set!   spec predicate-id)
	 spec)))
    (_
     (synner "syntax error in class definition"))))

(define (parse-class-clauses clauses name-id top-id lambda-id synner)
  ;;Parse the  clauses in a class  definition form in  the syntax object
  ;;STX and return an instance of record type "<class-spec>".
  ;;
  ;;TOP-ID must  be an identifier  bound to the "<top>"  tag.  LAMBDA-ID
  ;;must be  an identifier bound  to the LAMBDA macro  supporting tagged
  ;;formal  arguments.  SYNNER must  be a  closure to  be used  to raise
  ;;syntax violations.
  ;;
  (($car class-parser-functions)
   (make-<class-spec> name-id top-id lambda-id)
   (%verify-and-partially-unwrap-clauses clauses synner)
   ($cdr class-parser-functions)
   synner '()))


;;;; parsers entry points: label definition

(define (parse-label-definition stx top-id lambda-id synner)
  ;;Parse the  full label definition form  in the syntax  object STX and
  ;;return an instance of record type "<label-spec>".
  ;;
  ;;TOP-ID must  be an identifier  bound to the "<top>"  tag.  LAMBDA-ID
  ;;must be  an identifier bound  to the LAMBDA macro  supporting tagged
  ;;formal  arguments.  SYNNER must  be a  closure to  be used  to raise
  ;;syntax violations.
  ;;
  ;;Notice  how  we delegate  to  the  function PARSE-LABEL-CLAUSES  the
  ;;responsibility of building the  "<label-spec>" record: this makes it
  ;;easiers to test the parser functions.
  ;;
  (syntax-case stx ()
    ((_ ?tag-spec ?clause ...)
     (receive (name-id public-constructor-id predicate-id)
	 (parse-tag-name-spec #'?tag-spec synner)
       (let ((spec (parse-label-clauses #'(?clause ...) name-id top-id lambda-id synner)))
	 (<parsed-spec>-public-constructor-id-set! spec public-constructor-id)
	 (<parsed-spec>-public-predicate-id-set!   spec predicate-id)
	 spec)))
    (_
     (synner "syntax error in class definition"))))

(define (parse-label-clauses clauses name-id top-id lambda-id synner)
  ;;Parse the  clauses in a label  definition form in  the syntax object
  ;;STX and return an instance of record type "<label-spec>".
  ;;
  ;;TOP-ID must  be an identifier  bound to the "<top>"  tag.  LAMBDA-ID
  ;;must be  an identifier bound  to the LAMBDA macro  supporting tagged
  ;;formal  arguments.  SYNNER must  be a  closure to  be used  to raise
  ;;syntax violations.
  ;;
  (($car label-parser-functions)
   (make-<label-spec> name-id top-id lambda-id)
   (%verify-and-partially-unwrap-clauses clauses synner)
   ($cdr label-parser-functions)
   synner '()))


;;;; parsers entry points: mixin definition

(define (parse-mixin-definition stx top-id lambda-id synner)
  ;;Parse the  full mixin definition form  in the syntax  object STX and
  ;;return an instance of record type "<mixin-spec>".
  ;;
  ;;TOP-ID must  be an identifier  bound to the "<top>"  tag.  LAMBDA-ID
  ;;must be  an identifier bound  to the LAMBDA macro  supporting tagged
  ;;formal  arguments.  SYNNER must  be a  closure to  be used  to raise
  ;;syntax violations.
  ;;
  ;;Notice  how  we delegate  to  the  function PARSE-MIXIN-CLAUSES  the
  ;;responsibility of building the  "<mixin-spec>" record: this makes it
  ;;easiers to test the parser functions.
  ;;
  (syntax-case stx ()
    ((_ ?tag-spec ?clause ...)
     (let-values (((name-id public-constructor-id predicate-id)
		   (parse-tag-name-spec #'?tag-spec synner)))
       (parse-mixin-clauses #'(?clause ...) name-id top-id lambda-id synner)))
    (_
     (synner "syntax error in class definition"))))

(define (parse-mixin-clauses clauses name-id top-id lambda-id synner)
  ;;Parse the  clauses in a mixin  definition form in  the syntax object
  ;;STX and return an instance of record type "<mixin-spec>".
  ;;
  ;;TOP-ID must  be an identifier  bound to the "<top>"  tag.  LAMBDA-ID
  ;;must be  an identifier bound  to the LAMBDA macro  supporting tagged
  ;;formal  arguments.  SYNNER must  be a  closure to  be used  to raise
  ;;syntax violations.
  ;;
  (($car mixin-parser-functions)
   (make-<mixin-spec> name-id top-id lambda-id clauses)
   (%verify-and-partially-unwrap-clauses clauses synner)
   ($cdr mixin-parser-functions)
   synner '()))


;;;; parser functions helpers

(define (parse-tag-name-spec stx synner)
  ;;Parse the first component of a class or label definition:
  ;;
  ;;  (?definer ?tag-name-spec . ?clauses)
  ;;
  ;;supported syntaxes are:
  ;;
  ;;  ?name-id
  ;;  (?name-id ?public-constructor-id ?predicate-id)
  ;;
  ;;Return  3  values:  an  identifier  representing the  tag  name;  an
  ;;identifier representing  the public constructor  name; an identifier
  ;;representing the predicate name.
  ;;
  (syntax-case stx ()
    (?name-id
     (identifier? #'?name-id)
     (let ((name-id #'?name-id))
       (values name-id
	       (tag-id->constructor-id      name-id)
	       (tag-id->public-predicate-id name-id))))

    ((?name-id ?public-constructor-id ?predicate-id)
     (and (identifier? #'?name-id)
	  (identifier? #'?public-constructor-id)
	  (identifier? #'?predicate-id))
     (values #'?name-id #'?public-constructor-id #'?predicate-id))

    (_
     (synner "invalid name specification in tag definition" stx))))


;;;; parsers entry points: mixins clauses filtering

(define filter-and-validate-mixins-clauses
  (case-lambda
   ((input-clauses synner)
    (filter-and-validate-mixins-clauses (%verify-and-partially-unwrap-clauses input-clauses synner) synner '() '()))
   ((input-clauses synner collected-mixins output-clauses)
    ;;Tail-recursive function.   Parse clauses  with keyword  MIXINS and
    ;;prepend  the  mixin  identifiers to  COLLECTED-MIXINS.   Return  2
    ;;values: a  list of identifiers  being the mixin identifiers  to be
    ;;inserted in the given order, the list of other clauses.
    ;;
    (define-syntax %recurse
      (syntax-rules ()
	((_ ?input-clauses ?output-clauses)
	 (filter-and-validate-mixins-clauses ?input-clauses synner collected-mixins ?output-clauses))
	((_ ?collected-mixins ?input-clauses ?output-clauses)
	 (filter-and-validate-mixins-clauses ?input-clauses synner ?collected-mixins ?output-clauses))))
    (cond
     ;;No more input clauses.
     ((null? input-clauses)
      (values (reverse collected-mixins)
	      (reverse output-clauses)))
     ;;Parse matching clause.
     ((free-identifier=? #'aux.mixins (caar input-clauses))
      (syntax-case ($cdar input-clauses) ()
	(()
	 (%recurse ($cdr input-clauses) output-clauses))
	((?mixin ...)
	 (let loop ((collected-mixins	collected-mixins)
		    (new-mixins		#'(?mixin ...)))
	   (syntax-case new-mixins ()
	     (()
	      (%recurse collected-mixins ($cdr input-clauses) output-clauses))
	     ;;No substitutions.
	     ((?mixin . ?other-mixins)
	      (identifier? #'?mixin)
	      (loop (cons #'(?mixin) collected-mixins) #'?other-mixins))
	     ;;With map of substitutions.
	     (((?mixin (?from ?to) ...) . ?other-mixins)
	      (and (identifier? #'?mixin)
		   (all-identifiers? #'(?from ... ?to ...)))
	      (loop (cons #'(?mixin (?from ?to) ...) collected-mixins) #'?other-mixins))
	     (_
	      (synner "expected identifier as mixin" ($car input-clauses))))))
	(_
	 (synner "invalid mixins specification" ($car input-clauses)))))

     ;;Parse non-matching clause.
     (else
      (%recurse ($cdr input-clauses)
		(cons ($car input-clauses) output-clauses)))))))


;;;; parsing finalisation

(define (final-clauses-parser parsed-spec input-clauses next-parsers synner output-clauses)
  ;;Last parser  function.  The  clause parser functions  tail-call each
  ;;other in chain until all the input clauses are parsed; at the end of
  ;;the chain this  function is used to:
  ;;
  ;;*  Raise  a  syntax  violation  if there  are  leftover  clauses  in
  ;;INPUT-CLAUSES.
  ;;
  ;;* Finalise the PARSED-SPEC record and return it.
  ;;
  (if (null? input-clauses)
      (begin
	(when (<class-spec>? parsed-spec)
	  (let ((concrete-fields (<parsed-spec>-concrete-fields parsed-spec)))
	    (unless (null? concrete-fields)
	      (<parsed-spec>-concrete-fields-set! parsed-spec (reverse concrete-fields)))))
	(let ((definitions (<parsed-spec>-definitions parsed-spec)))
	  (unless (null? definitions)
	    (<parsed-spec>-definitions-set! parsed-spec (reverse definitions))))
	(unless (<parsed-spec>-parent-id parsed-spec)
	  (<parsed-spec>-parent-id-set! parsed-spec (<parsed-spec>-top-id parsed-spec)))
	(unless (<parsed-spec>-nongenerative-uid parsed-spec)
	  ;;According to  R6RS: GENERATE-TEMPORARIES returns identifiers
	  ;;which  are guaranteed to  be unique  in the  current process
	  ;;run.
	  (<parsed-spec>-nongenerative-uid-set! parsed-spec (car (generate-temporaries '(#f)))))
	(when (and (<class-spec>? parsed-spec)
		   (<parsed-spec>-abstract? parsed-spec))
	  (when (<parsed-spec>-common-protocol parsed-spec)
	    (synner "common protocol clause forbidden in definition of abstract class"))
	  (when (<parsed-spec>-public-protocol parsed-spec)
	    (synner "public protocol clause forbidden in definition of abstract class")))
	parsed-spec)
    (synner "unknown or invalid clauses" input-clauses)))


;;;; some at-most-once clause parsers: parent, opaque, sealed, nongenerative, shadows, maker

;;Parser function for the PARENT  clause; this clause must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (parent ?tag-id)
;;
;;where ?TAG-ID is the identifier bound  to the tag syntax of the parent
;;type.
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-parent)
  (clause-keyword aux.parent)
  (flag-accessor <parsed-spec>-parent-id)
  (body
   (let ((parent-id #'?single-argument))
     (if (identifier? parent-id)
	 (<parsed-spec>-parent-id-set! parsed-spec parent-id)
       (synner "invalid tag parent type specification" ($car input-clauses))))))

;;Parser function for the SEALED  clause; this clause must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (sealed #t)
;;   (sealed #f)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-sealed)
  (clause-keyword aux.sealed)
  (flag-accessor <parsed-spec>-sealed?)
  (body
   (let ((sealed? (syntax->datum #'?single-argument)))
     (if (boolean? sealed?)
	 (<parsed-spec>-sealed?-set! parsed-spec sealed?)
       (synner "invalid tag type sealed specification" ($car input-clauses))))))

;;Parser function for the OPAQUE  clause; this clause must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (opaque #t)
;;   (opaque #f)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-opaque)
  (clause-keyword aux.opaque)
  (flag-accessor <parsed-spec>-opaque?)
  (body
   (let ((opaque? (syntax->datum #'?single-argument)))
     (if (boolean? opaque?)
	 (<parsed-spec>-opaque?-set! parsed-spec opaque?)
       (synner "invalid tag type opaque specification" ($car input-clauses))))))

;;Parser function for the SHADOWS clause; this clause must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (shadows ?id)
;;
;;where ?ID  is an identifier.  The  selected identifier is  used by the
;;syntax WITH-LABEL-SHADOWING to hide some type definition with a label.
;;For  example: this  mechanism it  allows  to shadow  a condition  type
;;definition  with a  label type  and so  to use  the tag  syntaxes with
;;condition objects.
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-shadows)
  (clause-keyword aux.shadows)
  (flag-accessor <parsed-spec>-shadowed-identifier)
  (body
   (let ((shadowed-id #'?single-argument))
     (if (identifier? shadowed-id)
	 (<parsed-spec>-shadowed-identifier-set! parsed-spec shadowed-id)
       (synner "invalid tag type shadowed identifier specification" ($car input-clauses))))))

;;Parser function for  the MAKER clause; this clause  must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (maker ?transformer-expr)
;;
;;where  ?TRANSFORMER-EXPR  is  an  expression  evaluating  to  a  macro
;;transformer to be used to parse  the maker syntax.  We can imagine the
;;definition:
;;
;;   (define-class <alpha>
;;     (fields a b)
;;     (maker (lambda (stx)
;;              (syntax-case stx ()
;;                ((?tag (?a ?b))
;;                 #'(make-<alpha> ?a ?b))))))
;;
;;to expand to:
;;
;;   (define-syntax <alpha>
;;     (let ((the-maker (lambda (stx)
;;                       (syntax-case stx ()
;;                         ((?tag (?a ?b))
;;                          #'(make-<alpha> ?a ?b))))))
;;       (lambda (stx)
;;         ---)))
;;
;;and so the following expansion happens:
;;
;;   (<alpha> (1 2))	---> (make-<alpha> 1 2)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-maker)
  (clause-keyword aux.maker)
  (flag-accessor <parsed-spec>-maker-transformer)
  (body
   (<parsed-spec>-maker-transformer-set! parsed-spec #'?single-argument)))

;;Parser function for the FINALISER  clause; this clause must be present
;;at most once.  The expected syntax for the clause is:
;;
;;   (finaliser ?function-expr)
;;
;;where ?FUNCTION-EXPR is  an expression evaluating to a  function to be
;;used by the garbage collector to finalise the record.
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-finaliser)
  (clause-keyword aux.finaliser)
  (flag-accessor <parsed-spec>-finaliser-expression)
  (body
   (<parsed-spec>-finaliser-expression-set! parsed-spec #'?single-argument)))

;;; --------------------------------------------------------------------

(define (%parse-clauses-nongenerative parsed-spec input-clauses next-parsers synner output-clauses)
  ;;Tail-recursive function.   Parse clauses with  keyword NONGENERATIVE
  ;;and mutate  the appropriate fields  of PARSED-SPEC with  the result;
  ;;raise an error if the clause is used multiple times; the clause must
  ;;have zero arguments or a single argument:
  ;;
  ;;   (nongenerative)
  ;;   (nongenerative ?unique-id)
  ;;
  ;;where ?UNIQUE-ID is the  symbol which uniquely identifies the record
  ;;type in a whole program.  If the clause has no argument: a unique id
  ;;is automatically generated.
  ;;
  ;;After parsing:
  ;;
  ;;* If no more clauses are present: return null.
  ;;
  ;;* If  more clauses are  present: tail-call the next  parser function
  ;;from ?NEXT-PARSERS to continue parsing of clauses.
  ;;
  (define-inline (%recurse input-clauses output-clauses)
    (%parse-clauses-nongenerative parsed-spec input-clauses next-parsers synner output-clauses))
  (cond
   ;;No more input clauses.
   ((null? input-clauses)
    (if (null? output-clauses)
	(final-clauses-parser parsed-spec (reverse output-clauses) '() synner '())
      ((car next-parsers) parsed-spec (reverse output-clauses)
       (cdr next-parsers) synner '())))
   ;;Parse matching clause.
   ((free-identifier=? #'aux.nongenerative (caar input-clauses))
    (if (<parsed-spec>-nongenerative-uid parsed-spec)
	(synner "clause expected at most once is used multiple times, \
                 or clauses specifying the same features are used together"
		($car input-clauses))
      (syntax-case ($cdar input-clauses) ()
	(()
	 (begin
	   (<parsed-spec>-nongenerative-uid-set! parsed-spec (car (generate-temporaries '(#f))))
	   (%recurse ($cdr input-clauses) output-clauses)))
	((?unique-id)
	 (begin
	   (let ((uid #'?unique-id))
	     (if (identifier? uid)
		 (<parsed-spec>-nongenerative-uid-set! parsed-spec uid)
	       (synner "invalid tag type nongenerative specification" ($car input-clauses))))
	   (%recurse ($cdr input-clauses) output-clauses)))
	(_
	 (synner "a single-argument clause was expected" ($car input-clauses))))))
   ;;Parse non-matching clause.
   (else
    (%recurse ($cdr input-clauses)
	      (cons ($car input-clauses) output-clauses)))))


;;;; some at-most-once clause parsers: protocols, abstract

;;Parser function for  the PROTOCOL clause; this clause  must be present
;;at most once.  The expected syntax for the clause is:
;;
;;   (protocol ?expr)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-common-protocol)
  (clause-keyword aux.protocol)
  (flag-accessor <parsed-spec>-common-protocol)
  (body
   (<parsed-spec>-common-protocol-set! parsed-spec #'?single-argument)))

;;Parser function  for the PUBLIC-PROTOCOL  clause; this clause  must be
;;present at most once.  The expected syntax for the clause is:
;;
;;   (public-protocol ?expr)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-public-protocol)
  (clause-keyword aux.public-protocol)
  (flag-accessor <parsed-spec>-public-protocol)
  (body
   (<parsed-spec>-public-protocol-set! parsed-spec #'?single-argument)))

;;Parser  function for the  SUPER-PROTOCOL clause;  this clause  must be
;;present at most once.  The expected syntax for the clause is:
;;
;;   (super-protocol ?expr)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-super-protocol)
  (clause-keyword aux.super-protocol)
  (flag-accessor <parsed-spec>-super-protocol)
  (body
   (<parsed-spec>-super-protocol-set! parsed-spec #'?single-argument)))

;;Parser function for  the ABSTRACT clause; this clause  must be present
;;at most once and only in a DEFINE-CLASS or DEFINE-MIXIN.  The expected
;;syntax for the clause is:
;;
;;   (abstract)
;;
(define-clause-parser/at-most-once-clause/no-argument
    (parser-name %parse-clauses-abstract)
  (clause-keyword aux.abstract)
  (flag-accessor <parsed-spec>-abstract?)
  (body
   (<parsed-spec>-abstract?-set! parsed-spec #t)))


;;;; some at-most-once clause parsers: predicate

;;Parser function for the PREDICATE  clause; this clause must be present
;;at most once.  The expected syntax for the clause is:
;;
;;   (predicate ?function-expr)
;;
;;When a function predicate expression is specified as an identifier:
;;
;;  (define-label <list>
;;    (parent <pair>)
;;    (predicate list?))
;;
;;the tag definition expands to:
;;
;;  (define (<list>? obj)
;;    (and (<pair> :is-a? obj)
;;         (list? obj)))
;;
;;and the following tag syntax expansion happen:
;;
;;  (<list> is-a? ?obj)		---> (<list>? ?obj)
;;
;;When a function predicate expression is specified as an expression:
;;
;;  (define-label <list-of-numbers>
;;    (parent <pair>)
;;    (predicate (lambda (obj)
;;                 (and (list? obj)
;;                      (for-all number? obj)))))
;;
;;the tag definition expands to:
;;
;;  (define <list-of-numbers>-private-predicate
;;    (lambda (obj)
;;      (and (list? obj)
;;           (for-all number? obj))))
;;  (define (<list-of-numbers>? obj)
;;    (and (<pair> :is-a? obj)
;;         (<list-of-numbers>-private-predicate obj)))
;;
;;and the following tag syntax expansion happen:
;;
;;  (<list-of-numbers> is-a? ?obj)	---> (<list-of-numbers>? ?obj)
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-predicate)
  (clause-keyword aux.predicate)
  (flag-accessor <parsed-spec>-private-predicate-id)
  (body
   (let ((pred-expr-stx #'?single-argument))
     (if (identifier? pred-expr-stx)
	 (<parsed-spec>-private-predicate-id-set! parsed-spec pred-expr-stx)
       (let* ((name-id (<parsed-spec>-name-id parsed-spec))
	      (pred-id (tag-id->private-predicate-id name-id)))
	 (<parsed-spec>-definitions-cons! parsed-spec
					  (list #'define pred-id pred-expr-stx))
	 (<parsed-spec>-private-predicate-id-set! parsed-spec pred-id))))))


;;;; single method clauses

;;Parser function for METHOD clauses; this clause can be present zero or
;;more times.  A METHOD clause has one of the following syntaxes:
;;
;;  (method (?method-name ?arg ...) ?body0 ?body ...)
;;  (method ?method-name ?lambda-expr)
;;
(define-clause-parser/zero-or-more-clauses/multiple-mandatory-arguments
    (parser-name %parse-clauses-single-method)
  (clause-keyword aux.method)
  (body
   (let ((name-id (<parsed-spec>-name-id parsed-spec)))

     (define (%add-method method-name-id method-rv-tag-id method-implementation-id method-expr)
       (<parsed-spec>-member-identifiers-cons! parsed-spec method-name-id "method name" synner)
       (<parsed-spec>-methods-table-cons! parsed-spec method-name-id method-rv-tag-id method-implementation-id)
       (<parsed-spec>-definitions-cons! parsed-spec `(,#'define ,method-implementation-id ,method-expr)))

     (syntax-case #'?arguments ()
       ;;Untagged return value method definition.
       (((?method-name . ?formals) ?body0 ?body ...)
	(identifier? #'?method-name)
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#f)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id
		       #`(#,(<parsed-spec>-lambda-id parsed-spec) ?formals ?body0 ?body ...))))

       ;;Tagged  single  return  value   method  definition.   List  tag
       ;;specification.
       ((((?method-name ?rv-tag) . ?formals) ?body0 ?body ...)
	(and (identifier? #'?method-name)
	     (identifier? #'?rv-tag))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#'?rv-tag)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id
		       #`(#,(<parsed-spec>-lambda-id parsed-spec)
			  ((_ ?rv-tag) . ?formals) ?body0 ?body ...))))

       ;;Tagged  single  return  value method  definition.   Vector  tag
       ;;specification.
       (((#(?method-name ?rv-tag) . ?formals) ?body0 ?body ...)
	(and (identifier? #'?method-name)
	     (identifier? #'?rv-tag))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#'?rv-tag)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id
		       #`(#,(<parsed-spec>-lambda-id parsed-spec)
			  ((_ ?rv-tag) . ?formals) ?body0 ?body ...))))

       ;;Tagged  multiple return  values  method  definition.  List  tag
       ;;specification.
       ((((?method-name ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...)
	(and (identifier? #'?method-name)
	     (all-identifiers? #'(?rv-tag0 ?rv-tag ...)))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#f)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id
		       #`(#,(<parsed-spec>-lambda-id parsed-spec)
			  ((_ ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...))))

       ;;Tagged multiple  return values  method definition.   Vector tag
       ;;specification.
       (((#(?method-name ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...)
	(and (identifier? #'?method-name)
	     (all-identifiers? #'(?rv-tag0 ?rv-tag ...)))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#f)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id
		       #`(#,(<parsed-spec>-lambda-id parsed-spec)
			  ((_ ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...))))

       ;;Untagged external lambda method definition.
       ((?method-name ?lambda-expr)
	(identifier? #'?method-name)
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#f)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id #'?lambda-expr)))

       ;;Tagged external lambda method definition.
       ((#(?method-name ?rv-tag) ?lambda-expr)
	(and (identifier? #'?method-name)
	     (identifier? #'?rv-tag))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#'?rv-tag)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id #'?lambda-expr)))

       ;;Syntax violation.
       (_
	(synner "invalid method specification" ($car input-clauses)))))))


;;;; single method syntax clauses

;;Parser function for METHOD-SYNTAX  clauses; this clause can be present
;;zero or more times.  A METHOD-SYNTAX clause has the following syntax:
;;
;;  (method-syntax ?method-name ?transformer-expr)
;;
(define-clause-parser/zero-or-more-clauses/multiple-mandatory-arguments
    (parser-name %parse-clauses-single-method-syntax)
  (clause-keyword aux.method-syntax)
  (body
   (let ((name-id (<parsed-spec>-name-id parsed-spec)))

     (define (%add-method method-name-id method-rv-tag-id method-implementation-id method-expr)
       (<parsed-spec>-member-identifiers-cons! parsed-spec method-name-id "method name" synner)
       (<parsed-spec>-methods-table-cons! parsed-spec method-name-id method-rv-tag-id method-implementation-id)
       (<parsed-spec>-definitions-cons! parsed-spec
					`(,#'define-syntax ,method-implementation-id ,method-expr)))

     (syntax-case #'?arguments ()
       ;;Untagged return value method definition.
       ((?method-name ?lambda-expr)
	(identifier? #'?method-name)
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#f)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id #'?lambda-expr)))

       ;;Tagged return value method definition.  List tag specification.
       (((?method-name ?rv-tag) ?lambda-expr)
	(and (identifier? #'?method-name)
	     (identifier? #'?rv-tag))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#'?rv-tag)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id #'?lambda-expr)))

       ;;Tagged return value method definition.  Vector tag specification.
       ((#(?method-name ?rv-tag) ?lambda-expr)
	(and (identifier? #'?method-name)
	     (identifier? #'?rv-tag))
	(let* ((method-name-id			#'?method-name)
	       (method-rv-tag-id		#'?rv-tag)
	       (method-implementation-id	(make-method-identifier name-id method-name-id)))
	  (%add-method method-name-id method-rv-tag-id method-implementation-id #'?lambda-expr)))

       ;;Syntax violation.
       (_
	(synner "invalid method syntax specification" ($car input-clauses)))))))


;;;; multiple methods clauses

;;Parser function for  METHODS clauses; this clause can  be present zero
;;or more times.  A METHODS clause has the following syntaxe:
;;
;;  (methods ?method ...)
;;
;;where ?METHOD has one of the following syntaxes:
;;
;;  ?method-name-id
;;  (?method-name-id ?method-implementation-id)
;;
(define-clause-parser/zero-or-more-clauses/multiple-mandatory-arguments
    (parser-name %parse-clauses-multiple-methods)
  (clause-keyword aux.methods)
  (body
   (let ((name-id (<parsed-spec>-name-id parsed-spec)))

     (define (%add-method method-name-id method-rv-tag-id method-implementation-id)
       (<parsed-spec>-member-identifiers-cons! parsed-spec method-name-id "method name" synner)
       (<parsed-spec>-methods-table-cons! parsed-spec method-name-id method-rv-tag-id method-implementation-id))

     (let loop ((methods #'?arguments))
       (syntax-case methods ()
	 (()
	  (values))

	 ((?method-name . ?other-methods)
	  (identifier? #'?method-name)
	  (let ((method-name-id #'?method-name))
	    (%add-method method-name-id #f (make-method-identifier name-id method-name-id))
	    (loop #'?other-methods)))

	 (((?method-name) . ?other-methods)
	  (identifier? #'?method-name)
	  (let ((method-name-id #'?method-name))
	    (%add-method method-name-id #f (make-method-identifier name-id method-name-id))
	    (loop #'?other-methods)))

	 ;;Untagged return value method definition.
	 (((?method-name ?method-implementation) . ?other-methods)
	  (and (identifier? #'?method-name)
	       (identifier? #'?method-implementation))
	  (begin
	    (%add-method #'?method-name #f #'?method-implementation)
	    (loop #'?other-methods)))

	 ;;Tagged   return   value    method   definition.    List   tag
	 ;;specification.
	 ((((?method-name ?rv-tag) ?method-implementation) . ?other-methods)
	  (and (identifier? #'?method-name)
	       (identifier? #'?rv-tag)
	       (identifier? #'?method-implementation))
	  (begin
	    (%add-method #'?method-name #'?rv-tag #'?method-implementation)
	    (loop #'?other-methods)))

	 ;;Tagged   return   value   method  definition.    Vector   tag
	 ;;specification.
	 (((#(?method-name ?rv-tag) ?method-implementation) . ?other-methods)
	  (and (identifier? #'?method-name)
	       (identifier? #'?rv-tag)
	       (identifier? #'?method-implementation))
	  (begin
	    (%add-method #'?method-name #'?rv-tag #'?method-implementation)
	    (loop #'?other-methods)))

	 ;;Syntax violation.
	 (_
	  (synner "invalid method specification" ($car input-clauses))))))))


;;;; concrete fields clauses

;;Parser function for FIELDS clauses; this clause can be present zero or
;;more  times; this  clause accepts  zero or  more arguments.   A FIELDS
;;clause has the following syntax:
;;
;;  (fields ?field-spec ...)
;;
;;where ?FIELD-SPEC has one of the following syntaxes:
;;
;;  ?field-name-id
;;  (mutable   ?field)
;;  (mutable   ?field ?accessor-id ?mutator-id)
;;  (immutable ?field)
;;  (immutable ?field ?accessor-id)
;;
;;where ?FIELD has one of the following syntaxes:
;;
;;  ?field-name-id
;;  (?field-name-id ?type-tag-id)
;;  (?field-name-id)
;;
(define-clause-parser/zero-or-more-clauses/zero-or-more-arguments
    (parser-name %parse-clauses-concrete-fields)
  (clause-keyword aux.fields)
  (body
   (let ((name-id	(<parsed-spec>-name-id parsed-spec))
	 (top-id	(<parsed-spec>-top-id  parsed-spec)))

     (define (%main field-specs)
       ;;Recursive function.  Parse  a FIELDS clause's arguments.  Return
       ;;unspecified values.
       ;;
       (syntax-case field-specs ()
	 ;;No more field specs.
	 (() (values))
	 ;;A field spec.
	 ((?field-spec . ?other-specs)
	  (begin
	    (%add-field-record (%parse-single-field-spec #'?field-spec))
	    (%main #'?other-specs)))
	 ;;Invalid field specification
	 (_
	  (synner "invalid field specification" ($car input-clauses)))))

     (define-inline (%add-field-record ?field-record)
       ;;Add  a  record  representing   a  field  specification  to  the
       ;;appropriate field in PARSED-SPEC.  Check for duplicate names in
       ;;members.
       ;;
       (let ((field-record ?field-record))
	 (<parsed-spec>-member-identifiers-cons! parsed-spec (<field-spec>-name-id field-record)
						 "field name" synner)
	 (<parsed-spec>-concrete-fields-cons! parsed-spec field-record)))

     (define-inline (%parse-single-field-spec ?field-spec)
       ;;Parse a  single field specification argument and  return a field
       ;;record describing it.
       ;;
       (let ((field-spec ?field-spec))
	 (syntax-case field-spec (aux.mutable aux.immutable)

	   ;;Immutable field, without accessor name.
	   ((aux.immutable ?field)
	    (%make-immutable-field-record field-spec #'?field #f))

	   ;;Immutable field, with accessor name.
	   ((aux.immutable ?field ?accessor-name)
	    (identifier? #'?accessor-name)
	    (%make-immutable-field-record field-spec #'?field #'?accessor-name))

	   ;;Mutable field, without accessor and mutator names.
	   ((aux.mutable ?field)
	    (%make-mutable-field-record field-spec #'?field #f #f))

	   ;;Mutable field, with accessor and mutator names.
	   ((aux.mutable ?field ?accessor-name ?mutator-name)
	    (and (identifier? #'?accessor-name)
		 (identifier? #'?mutator-name))
	    (%make-mutable-field-record field-spec #'?field #'?accessor-name #'?mutator-name))

	   ;;Immutable field, without IMMUTABLE keyword.
	   (?field-name
	    (identifier? #'?field-name)
	    (%make-immutable-field-record field-spec field-spec #f))

	   ;;Immutable field, without IMMUTABLE keyword, with tag.
	   ((?field-name ?tag-id)
	    (and (identifier? #'?field-name)
		 (identifier? #'?tag-id))
	    (%make-immutable-field-record field-spec field-spec #f))

	   ;;Invalid field specification.
	   (_
	    (synner "invalid field specification" field-spec)))))

     (define (%make-immutable-field-record field-spec field-name-and-tag-stx
					   accessor-id)
       ;;Parse  the field-and-tags  syntax  object.  Build  and return  a
       ;;field record specifying an immutable field.
       ;;
       (let-values (((field-id tag-id)
		     (if (identifier? field-name-and-tag-stx)
			 (values field-name-and-tag-stx top-id)
		       (%parse-field-id-and-tag field-spec field-name-and-tag-stx))))
	 (make-<concrete-field-spec> field-id
				     (or accessor-id (%make-accessor-id field-id))
				     #f tag-id)))

     (define (%make-mutable-field-record field-spec field-name-and-tag-stx
					 accessor-id mutator-id)
       ;;Parse  the field-and-tags  syntax  object.  Build  and return  a
       ;;field record specifying a mutable field.
       ;;
       (let-values (((field-id tag-id)
		     (if (identifier? field-name-and-tag-stx)
			 (values field-name-and-tag-stx top-id)
		       (%parse-field-id-and-tag field-spec field-name-and-tag-stx))))
	 (make-<concrete-field-spec> field-id
				     (or accessor-id (%make-accessor-id field-id))
				     (or mutator-id  (%make-mutator-id  field-id))
				     tag-id)))

     (define (%parse-field-id-and-tag field-spec-stx field-name-and-tag-stx)
       ;;Parse  a syntax  object representing  a list  whose car  is the
       ;;field name identifier and  whose optional cdr is the identifier
       ;;of the  field's type  tag.  Return two  values: the  field name
       ;;identifier, the tag type identifier or false.
       ;;
       (syntax-case field-name-and-tag-stx ()
	 ((?field-name ?tag-id)
	  (and (identifier? #'?field-name)
	       (identifier? #'?tag-id))
	  (values #'?field-name #'?tag-id))
	 ((?field-name)
	  (identifier? #'?field-name)
	  (values #'?field-name #f))
	 (_
	  (synner "invalid field specification" field-spec-stx))))

     (define-inline (%make-accessor-id field-id)
       (identifier-record-field-accessor name-id field-id))

     (define-inline (%make-mutator-id field-id)
       (identifier-record-field-mutator name-id field-id))

     (%main #'?arguments))))


;;;; virtual fields clauses

;;Parser function for VIRTUAL-FIELDS clauses; this clause can be present
;;zero or  more times;  this clause accepts  zero or more  arguments.  A
;;VIRTUAL-FIELDS clause has the following syntax:
;;
;;  (fields ?field-spec ...)
;;
;;where ?FIELD-SPEC has one of the following syntaxes:
;;
;;  ?field-name-id
;;  (mutable   ?field)
;;  (mutable   ?field ?accessor ?mutator)
;;  (immutable ?field)
;;  (immutable ?field ?accessor)
;;
;;where ?FIELD has one of the following syntaxes:
;;
;;  ?field-name-id
;;  (?field-name-id ?type-tag-id)
;;  (?field-name-id)
;;
;;and ?ACCESSOR has one of the following syntaxes:
;;
;;  ?accessor-id
;;  ?accessor-expr
;;
;;and ?MUTATOR has one of the following syntaxes:
;;
;;  ?mutator-id
;;  ?mutator-expr
;;
(define-clause-parser/zero-or-more-clauses/zero-or-more-arguments
    (parser-name %parse-clauses-virtual-fields)
  (clause-keyword aux.virtual-fields)
  (body
   (let ((name-id	(<parsed-spec>-name-id parsed-spec))
	 (top-id	(<parsed-spec>-top-id  parsed-spec)))

     (define (%main field-specs)
       ;;Recursive function.  Parse  a FIELDS clause's arguments.  Return
       ;;unspecified values.
       ;;
       (syntax-case field-specs ()
	 ;;No more field specs.
	 (() (values))
	 ;;A field spec.
	 ((?field-spec . ?other-specs)
	  (begin
	    (%add-field-record (%parse-single-field-spec #'?field-spec))
	    (%main #'?other-specs)))
	 ;;Invalid field specification
	 (_
	  (synner "invalid field specification" ($car input-clauses)))))

     (define-inline (%add-field-record ?field-record)
       ;;Add  a  record  representing   a  field  specification  to  the
       ;;appropriate field in PARSED-SPEC.  Check for duplicate names in
       ;;members.
       ;;
       (let ((field-record ?field-record))
	 (<parsed-spec>-member-identifiers-cons! parsed-spec (<field-spec>-name-id field-record)
						 "field name" synner)
	 (<parsed-spec>-virtual-fields-cons! parsed-spec field-record)))

     (define-inline (%parse-single-field-spec ?field-spec)
       ;;Parse a  single field specification argument and  return a field
       ;;record describing it.
       ;;
       (let ((field-spec ?field-spec))
	 (syntax-case field-spec (aux.mutable aux.immutable)

	   ;;Immutable field, without accessor name.
	   ((aux.immutable ?field)
	    (%make-immutable-field-record field-spec #'?field #f))

	   ;;Immutable field, with accessor name.
	   ((aux.immutable ?field ?accessor-expr)
	    (%make-immutable-field-record field-spec #'?field #'?accessor-expr))

	   ;;Mutable field, without accessor and mutator names.
	   ((aux.mutable ?field)
	    (%make-mutable-field-record field-spec #'?field #f #f))

	   ;;Mutable field, with accessor and mutator names.
	   ((aux.mutable ?field ?accessor-expr ?mutator-expr)
	    (%make-mutable-field-record field-spec #'?field #'?accessor-expr #'?mutator-expr))

	   ;;Immutable field, without IMMUTABLE keyword.
	   (?field-name
	    (identifier? #'?field-name)
	    (%make-immutable-field-record field-spec field-spec #f))

	   ;;Immutable field, without IMMUTABLE keyword, with tag.
	   ((?field-name ?tag-id)
	    (and (identifier? #'?field-name)
		 (identifier? #'?tag-id))
	    (%make-immutable-field-record field-spec field-spec #f))

	   ;;Invalid field specification.
	   (_
	    (synner "invalid field specification" field-spec)))))

     (define (%make-immutable-field-record field-spec field-name-and-tag-stx
					   accessor-expr)
       ;;Parse  the field-and-tags  syntax  object.  Build  and return  a
       ;;field record specifying an immutable field.
       ;;
       (let-values (((field-id tag-id)
		     (if (identifier? field-name-and-tag-stx)
			 (values field-name-and-tag-stx top-id)
		       (%parse-field-id-and-tag field-spec field-name-and-tag-stx))))
	 (make-<virtual-field-spec> field-id
				    (if (identifier? accessor-expr)
					accessor-expr
				      (let ((accessor-id (%make-accessor-id field-id)))
					(when accessor-expr
					  (%add-definition (list #'define accessor-id accessor-expr)))
					accessor-id))
				    #f tag-id)))

     (define (%make-mutable-field-record field-spec field-name-and-tag-stx
					 accessor-expr mutator-expr)
       ;;Parse  the field-and-tag  syntax  object.  Build  and return  a
       ;;field record specifying a mutable field.
       ;;
       (let-values (((field-id tag-id)
		     (if (identifier? field-name-and-tag-stx)
			 (values field-name-and-tag-stx top-id)
		       (%parse-field-id-and-tag field-spec field-name-and-tag-stx))))
	 ;;We need to  impose the correct order to  definitions added to
	 ;;the parsed spec record.
	 (let ((accessor-id	(if (identifier? accessor-expr)
				    accessor-expr
				  (let ((accessor-id (%make-accessor-id field-id)))
				    (when accessor-expr
				      (%add-definition (list #'define accessor-id accessor-expr)))
				    accessor-id)))
	       (mutator-id	(if (identifier? mutator-expr)
				    mutator-expr
				  (let ((mutator-id (%make-mutator-id field-id)))
				    (when mutator-expr
				      (%add-definition (list #'define mutator-id mutator-expr)))
				    mutator-id))))
	   (make-<virtual-field-spec> field-id accessor-id mutator-id tag-id))))

     (define (%parse-field-id-and-tag field-spec-stx field-name-and-tag-stx)
       ;;Parse  a syntax  object representing  a list  whose car  is the
       ;;field name identifier and  whose optional cdr is the identifier
       ;;of the  field's type  tag.  Return two  values: the  field name
       ;;identifier, the tag type identifier or false.
       ;;
       (syntax-case field-name-and-tag-stx ()
	 ((?field-name ?tag-id)
	  (and (identifier? #'?field-name)
	       (identifier? #'?tag-id))
	  (values #'?field-name #'?tag-id))
	 ((?field-name)
	  (identifier? #'?field-name)
	  (values #'?field-name #f))
	 (_
	  (synner "invalid field specification" field-spec-stx))))

     (define (%add-definition definition-form)
       (<parsed-spec>-definitions-cons! parsed-spec definition-form))

     (define-inline (%make-accessor-id field-id)
       (identifier-record-field-accessor name-id field-id))

     (define-inline (%make-mutator-id field-id)
       (identifier-record-field-mutator name-id field-id))

     (%main #'?arguments))))


;;;; satisfactions

(define-clause-parser/zero-or-more-clauses/zero-or-more-arguments
    (parser-name %parse-clauses-satisfies)
  (clause-keyword aux.satisfies)
  (body
   (let ()

     (define (%main satisfactions)
       ;;Recursive  function.   Parse  a SATISFIES  clause's  arguments.
       ;;Return unspecified values.
       ;;
       (syntax-case satisfactions ()
	 (() (values))

	 ((?satisfaction . ?other-satisfactions)
	  (begin
	    (when config.enable-satisfactions
	      (let ((satis-stx #'?satisfaction))
		(if (identifier? satis-stx)
		    (%add-satisfaction satis-stx)
		  (with-syntax (((ID) (generate-temporaries '(#f))))
		    (%add-satisfaction #'ID)
		    (%add-definition #'(define-syntax ID ?satisfaction))))))
	    (%main #'?other-satisfactions)))

	 (_
	  (synner "invalid satisfaction specification" ($car input-clauses)))))

     (define-inline (%add-satisfaction id)
       (<parsed-spec>-satisfactions-set! parsed-spec (cons id (<parsed-spec>-satisfactions parsed-spec))))

     (define-inline (%add-definition definition-form)
       (<parsed-spec>-definitions-cons! parsed-spec definition-form))

     (%main #'?arguments))))


;;;; setter and getter clauses

;;Parser function for the GETTER  clause; this clause must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (getter ?expr)
;;
;;where ?EXPR is an expression evaluating to a getter syntax function.
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-getter)
  (clause-keyword aux.getter)
  (flag-accessor <parsed-spec>-getter)
  (body
   (<parsed-spec>-getter-set! parsed-spec #'?single-argument)))

;;Parser function for the SETTER  clause; this clause must be present at
;;most once.  The expected syntax for the clause is:
;;
;;   (setter ?expr)
;;
;;where ?EXPR is an expression evaluating to a setter syntax function.
;;
(define-clause-parser/at-most-once-clause/single-argument
    (parser-name %parse-clauses-setter)
  (clause-keyword aux.setter)
  (flag-accessor <parsed-spec>-setter)
  (body
   (<parsed-spec>-setter-set! parsed-spec #'?single-argument)))


;;;; done

;;Parser functions for the clauses of the DEFINE-LABEL syntax.
;;
;;The  order of  invocation matters  only  in terms  of efficiency:  the
;;clauses that appear most should be parsed first.
;;
(define-constant label-parser-functions
  (list
   %parse-clauses-single-method
   %parse-clauses-virtual-fields
   %parse-clauses-multiple-methods
   %parse-clauses-single-method-syntax
   %parse-clauses-parent
   %parse-clauses-common-protocol
   %parse-clauses-public-protocol
   %parse-clauses-predicate
   %parse-clauses-getter
   %parse-clauses-setter
   %parse-clauses-nongenerative
   %parse-clauses-maker
   %parse-clauses-satisfies
   %parse-clauses-shadows
   final-clauses-parser))

;;Parser functions for the clauses of the DEFINE-CLASS syntax.
;;
;;The  order of  invocation matters  only  in terms  of efficiency:  the
;;clauses that appear most should be parsed first.
;;
(define-constant class-parser-functions
  (list
   %parse-clauses-single-method
   %parse-clauses-concrete-fields
   %parse-clauses-virtual-fields
   %parse-clauses-multiple-methods
   %parse-clauses-single-method-syntax
   %parse-clauses-nongenerative
   %parse-clauses-parent
   %parse-clauses-common-protocol
   %parse-clauses-public-protocol
   %parse-clauses-super-protocol
   %parse-clauses-getter
   %parse-clauses-setter
   %parse-clauses-maker
   %parse-clauses-finaliser
   %parse-clauses-satisfies
   %parse-clauses-sealed
   %parse-clauses-opaque
   %parse-clauses-abstract
   final-clauses-parser))

;;Parser functions for the clauses of the DEFINE-MIXIN syntax.
;;
;;The  order of  invocation matters  only  in terms  of efficiency:  the
;;clauses that appear most should be parsed first.
;;
(define-constant mixin-parser-functions
  (list
   %parse-clauses-single-method
   %parse-clauses-concrete-fields
   %parse-clauses-virtual-fields
   %parse-clauses-multiple-methods
   %parse-clauses-parent
   %parse-clauses-common-protocol
   %parse-clauses-public-protocol
   %parse-clauses-getter
   %parse-clauses-setter
   %parse-clauses-nongenerative
   %parse-clauses-maker
   %parse-clauses-finaliser
   %parse-clauses-abstract
   %parse-clauses-opaque
   %parse-clauses-predicate
   %parse-clauses-satisfies
   %parse-clauses-sealed
   %parse-clauses-shadows
   %parse-clauses-single-method-syntax
   %parse-clauses-super-protocol
   final-clauses-parser))

)

;;; end of file
;; Local Variables:
;; coding: utf-8
;; eval: (put 'define-clause-parser/at-most-once-clause/single-argument 'scheme-indent-function 1)
;; eval: (put 'define-clause-parser/at-most-once-clause/no-argument 'scheme-indent-function 1)
;; eval: (put 'define-clause-parser/zero-or-more-clauses/zero-or-more-arguments 'scheme-indent-function 1)
;; eval: (put 'define-clause-parser/zero-or-more-clauses/single-argument 'scheme-indent-function 1)
;; End:
