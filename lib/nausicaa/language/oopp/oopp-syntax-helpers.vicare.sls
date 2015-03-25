;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helper functions for OOPP syntax
;;;Date: Fri Jan 17, 2014
;;;
;;;Abstract
;;;
;;;	This  library defines  functions used  as macro  tranformers, or
;;;	portions of macro transformers, implementing the OOPP syntax.
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (nausicaa language oopp oopp-syntax-helpers (0 4))
  (export
    parse-with-tags-bindings		parse-let-bindings
    parse-let-values-bindings		parse-formals-bindings

    make-tagged-variable-transformer
    tag-public-syntax-transformer	tag-private-common-syntax-transformer
    make-accessor-transformer		make-mutator-transformer
    process-method-application		process-shadowed-identifier)
  (import (vicare (0 4))
    (prefix (only (nausicaa language oopp configuration (0 4))
		  validate-tagged-values?)
	    config.)
    (for (nausicaa language oopp auxiliary-syntaxes (0 4))
      (meta -1))
    (for (prefix (only (nausicaa language auxiliary-syntaxes (0 4))
		       <>)
		 aux.)
      (meta -1))
    (for (only (nausicaa language oopp conditions (0 4))
	       tagged-binding-violation)
      (meta -1))
    (only (vicare language-extensions identifier-substitutions)
	  single-identifier-subst)
    (for (prefix (nausicaa language oopp definition-parser-helpers (0 4))
		 parser-help.)
      expand))


(define* (make-tagged-variable-transformer {tag-id identifier?} {src-var-id identifier?})
  ;;Build  and   return  the   transformer  function   implementing  the
  ;;identifier syntax  for tagged  variables.  When  we define  a tagged
  ;;variable with:
  ;;
  ;;   (define {a <integer>} 123)
  ;;
  ;;we can imagine the following expansion:
  ;;
  ;;   (define src-var 123)
  ;;   (define-syntax a
  ;;     (make-tagged-variable-transformer #'<integer> #'src-var))
  ;;
  ;;and when we define a tagged variable with:
  ;;
  ;;   (let/tags (({a <integer>} 123)) ---)
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

       ;;This  rule matches  always, whatever  the syntax  in which  the
       ;;tagged identifier is used.  Examples:
       ;;
       ;;   (?var ?field-name)
       ;;   (?var (?key0) (?key) ...)
       ;;   (set!/tags (?var ?field-name) ?new-value)
       ;;   (set!/tags (?var (?key0) (?key) ...) ?new-value)
       ;;   (set!/tags ?var (?key0) (?key) ... ?new-value)
       ;;
       (_
	#`(#,tag-id #:oopp-syntax #,stx))
       ))))


(define (tag-private-common-syntax-transformer stx abstract? the-public-constructor the-public-predicate the-list-of-uids
					       the-getter the-setter kont)
  ;;Transformer function  for the  private syntaxes available  through a
  ;;tag identifier,  only the ones  common for both labels  and classes.
  ;;STX is a syntax object representing the use of a tag identifier.
  ;;
  ;;KONT is  a continuation thunk to  be invoked if none  of the clauses
  ;;defined here match.
  ;;
  ;;Notice that  "<procedure>" and "<top>" define  some special variants
  ;;of these syntaxes; such variants are matched before this function is
  ;;called.
  ;;
  (syntax-case stx ( ;;
		    :define :make :is-a? :list-of-unique-ids :predicate-function
		    :setter :getter
		    :assert-type-and-return :assert-procedure-argument
		    :assert-expression-return-value
		    aux.<>)

    ;;Define  internal   bindings  for   a  tagged   variable.   Without
    ;;initialisation expression.
    ((?tag :define ?var)
     (identifier? #'?var)
     #'(begin
	 (define src-var)
	 (define-syntax ?var
	   (make-tagged-variable-transformer #'?tag #'src-var))))

    ;;Define   internal   bindings   for  a   tagged   variable.    With
    ;;initialisation expression.
    ((?tag :define ?var ?expr)
     (identifier? #'?var)
     #'(begin
	 (define src-var (?tag :assert-type-and-return ?expr))
	 (define-syntax ?var
	   (make-tagged-variable-transformer #'?tag #'src-var))))

    ((?tag :make . ??args)
     (if abstract?
	 (syntax-violation (syntax->datum #'?tag)
	   "attempt to instantiate abstract syntax" stx)
       #`(#,the-public-constructor . ??args)))

    ((_ :is-a? aux.<>)
     the-public-predicate)
    ((_ :is-a? . ??args)
     #`(#,the-public-predicate . ??args))

    ((_ :list-of-unique-ids)
     the-list-of-uids)

    ((_ :predicate-function)
     the-public-predicate)

    ((_ :getter (?expr ((?key0 ...) (?key ...) ...)))
     (the-getter #'(?expr ((?key0 ...) (?key  ...) ...))))

    ((_ :setter (?expr ((?key0 ...) (?key ...) ...) ?value))
     (the-setter #'(?expr ((?key0 ...) (?key ...) ...) ?value)))

    ((?tag :assert-type-and-return ?expr)
     (if config.validate-tagged-values?
	 #'(receive-and-return (val)
	       ?expr
	     (unless (?tag :is-a? val)
	       (tagged-binding-violation '?tag
		 (string-append "invalid expression result, expected value of type "
				(symbol->string '?tag))
		 '(expression: ?expr)
		 `(result: ,val))))
       #'?expr))

    ((?tag :assert-procedure-argument ?id)
     (identifier? #'?id)
     ;;This DOES NOT return the value.
     (if config.validate-tagged-values?
	 #'(unless (?tag :is-a? ?id)
	     (procedure-argument-violation '?tag
	       "tagged procedure argument of invalid type" ?id))
       #'(void)))

    ((?tag :assert-expression-return-value ?expr)
     ;;This DOES return the value.
     (if config.validate-tagged-values?
	 #'(receive-and-return (val)
	       ?expr
	     (unless (?tag :is-a? val)
	       (expression-return-value-violation '?tag
		 "tagged expression return value of invalid type" val)))
       #'?expr))

    (_
     (kont))))


(define (tag-public-syntax-transformer stx the-maker set-tags-id synner)
  ;;Transformer function for the public syntaxes available through a tag
  ;;identifier.  STX  is a syntax object  representing the use of  a tag
  ;;identifier.
  ;;
  ;;THE-MAKER is  the maker  transformer function or  false if  no maker
  ;;transformer was defined.
  ;;
  ;;SET-TAGS-ID must be the keyword  identifier of the syntax SET!/TAGS.
  ;;It is used to process the syntax with keyword #:oopp-syntax.
  ;;
  ;;Notice that  "<procedure>" and "<top>" define  some special variants
  ;;of these syntaxes; such variants are matched before this function is
  ;;called.
  ;;
  (syntax-case stx (:make :define :flat-oopp-syntax aux.<>)

    ;;NOTE Put the clauses with literals and keyword objects first!!!

    ;;OOPP syntax  for arbitrary  expressions.  This  syntax is  used to
    ;;process  all  the OOPP  expressions  in  which a  tagged  variable
    ;;appears.  Examples:
    ;;
    ;;   (?tag #:oopp-syntax (?var ?field-name))
    ;;   (?tag #:oopp-syntax (?var (?key0) (?key) ...))
    ;;   (?tag #:oopp-syntax (set!/tags (?var ?field-name) ?new-value))
    ;;   (?tag #:oopp-syntax (set!/tags (?var (?key0) (?key) ...) ?new-value))
    ;;   (?tag #:oopp-syntax (set!/tags ?var (?key0) (?key) ... ?new-value))
    ;;
    ((?tag #:oopp-syntax (?expr0 ?expr ...))
     (%oopp-syntax-transformer #'?tag #'(?expr0 ?expr ...) set-tags-id synner))

    ;;OOPP syntax  for arbitrary expression spliced  when first subform.
    ;;?EXPR must be an expression evaluating to an object of type ?TAG.
    ;;
    ((?tag #:nested-oopp-syntax ?expr)
     #'(splice-first-expand (?tag :flat-oopp-syntax ?expr)))

    ;;Clauses to process spliced OOPP syntax forms.  These rules must be
    ;;invoked only by the expansion of:
    ;;
    ;;   (?tag #:nested-oopp-syntax ?expr)
    ;;
    ;;if it  appears as  first subform  and there  are arguments  in the
    ;;enclosing form, we want the expansion:
    ;;
    ;;   ((?tag #:nested-oopp-syntax ?expr) ?arg ...)
    ;;   ==> ((splice-first-expand (?tag :flat-oopp-syntax ?expr)) ?arg ...)
    ;;   ==> (?tag :flat-oopp-syntax ?expr ?arg ...)
    ;;   ==> (?tag #:oopp-syntax (?expr ?arg ...))
    ;;
    ;;otherwise we want the expansion:
    ;;
    ;;   (begin
    ;;     (?tag #:nested-oopp-syntax ?expr))
    ;;   ==> (begin
    ;;         ((splice-first-expand (?tag :flat-oopp-syntax ?expr))))
    ;;   ==> (begin
    ;;         (?tag :flat-oopp-syntax ?expr))
    ;;   ==> (begin ?expr)
    ;;
    ((?tag :flat-oopp-syntax ?expr)
     #'?expr)
    ((?tag :flat-oopp-syntax ?expr ?arg ...)
     #'(?tag #:oopp-syntax (?expr ?arg ...)))

    ;;Predicate application.
    ;;
    ((?tag #:is-a? ?expr)
     #'(?tag :is-a? ?expr))

    ;;Reference to predicate function.
    ;;
    ((?tag #:predicate)
     #'(?tag :predicate-function))

    ;; ------------------------------------------------------------

    ;;Internal definition with initialisation expression.
    ((?tag ?var ?expr)
     (identifier? #'?var)
     #'(?tag :define ?var (fluid-let-syntax
			      ((aux.<> (make-synonym-transformer #'?tag)))
			    ?expr)))

    ;;Internal definition without initialisation expression.
    ((?tag ?var)
     (identifier? #'?var)
     #'(?tag :define ?var))

    ;;Constructor call.   If a  maker transformer  was defined:  use it,
    ;;otherwise default to the public constructor.
    ((?tag (?arg ...))
     #`(?tag #:nested-oopp-syntax #,(if the-maker
					(the-maker stx)
				      #'(?tag :make ?arg ...))))

    ;;Cast operator.  It is meant to be used as:
    ;;
    ;;  ((?tag) '#())
    ;;  ==> ((splice-first-expand (?tag #:nested-oopp-syntax)) '#())
    ;;  ==> (?tag #:nested-oopp-syntax '#())
    ;;
    ((?tag)
     #'(splice-first-expand (?tag #:nested-oopp-syntax)))

    (_
     (synner "invalid tag syntax" #f))))

(define (%oopp-syntax-transformer tag-id form-stx set-bang-id synner)
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
  ;;Notice setter syntaxes are supported, too.
  ;;
  (syntax-case form-stx (:mutator :setter)

    ;;Setter syntax.   Main syntax to invoke  the setter for the  tag of
    ;;?VAR;  it  supports  multiple  sets  of  keys  for  nested  setter
    ;;invocations.
    ((?set (?expr (?key0 ...) (?key ...) ...) ?new-value)
     (and (identifier? #'?set)
    	  (free-identifier=? #'?set set-bang-id))
     #`(#,tag-id :setter (?expr ((?key0 ...) (?key ...) ...) ?new-value)))

    ;;Setter syntax.   Alternative syntax to  invoke the setter  for the
    ;;tag of ?EXPR; it supports multiple  sets of keys for nested setter
    ;;invocations.
    ((?set ?expr (?key0 ...) (?key ...) ... ?new-value)
     (and (identifier? #'?set)
    	  (free-identifier=? #'?set set-bang-id))
     #`(#,tag-id :setter (?expr ((?key0 ...) (?key ...) ...) ?new-value)))

    ;;Setter syntax.  Syntax to invoke the  field mutator for the tag of
    ;;?EXPR.
    ((?set (?expr ?field-name) ?new-value)
     (and (identifier? #'?set)
    	  (free-identifier=? #'?set set-bang-id))
     (if (identifier? #'?field-name)
	 #`(#,tag-id :mutator ?expr ?field-name ?new-value)
       (synner "expected identifier as field name for mutator" #'?field-name)))

    ;;Syntax to apply the field mutator for the tag of ?EXPR.
    ((?expr :mutator ?field-name ?new-value)
     (if (identifier? #'?field-name)
	 #`(#,tag-id :mutator ?expr ?field-name ?new-value)
       (synner "expected identifier as field name for mutator" #'?field-name)))

    ;;Syntax to apply the setter for the tag of ?EXPR.
    ((?expr :setter ((?key ...) ...) ?new-value)
     #`(#,tag-id :setter (?expr ((?key ...) ...) ?new-value)))

    ;;Syntax to  apply the getter for  the tag of ?EXPR.   A plain getter
    ;;syntax is as follows:
    ;;
    ;;  (?expr (?key0 ...) (?key ...) ...)
    ;;
    ;;where "<tag>"  is the  tag assigned to  the getter  runtime return
    ;;value from the getter transformer function.
    ;;
    ((?expr (?key0 ...) (?key ...) ...)
     #`(#,tag-id :getter (?expr ((?key0 ...) (?key ...) ...))))

    ;;Syntax to apply a method or reference a field of the tag of ?EXPR.
    ((?expr . ?stuff)
     #`(#,tag-id :dispatch (?expr . ?stuff)))

    (_
     (synner "invalid OOPP syntax" form-stx))))


(define* (make-accessor-transformer {spec parser-help.<parsed-spec>?})
  ;;Given  the "<parsed-spec>"  instance  SPEC: return  a syntax  object
  ;;representing the accessor transformer function for the tag.
  ;;
  ;;Whenever a tagged variable is referenced in a form like:
  ;;
  ;;   (?var ?member-id ?arg ...)
  ;;
  ;;first the  symbol ?MEMBER-ID  is compared  to the  names of  the tag
  ;;methods:
  ;;
  ;;1. If it matches, the form is a method call.  This step is performed
  ;;   elsewhere.
  ;;
  ;;2. If  no method name  matches, the form  is handed to  the accessor
  ;;   transformer function to attempt  a match between ?MEMBER-ID and a
  ;;   field name.
  ;;
  ;;3. If no field name matches, the form is handed to the parent tag to
  ;;   attempt a match with the parent tag's members.
  ;;
  ;;Let's consider the example:
  ;;
  ;;   (define-label <alpha>
  ;;     (fields a)
  ;;     (getter ---)
  ;;     (method (do O x y)
  ;;       ---))
  ;;
  ;;   (define-label <beta>
  ;;     (fields {b <alpha>}))
  ;;
  ;;   (define-label <gamma>
  ;;     (fields {c <beta>}))
  ;;
  ;;   (<gamma> O ---)
  ;;
  ;;where  O is  the tagged  variable; the  following syntax  expansions
  ;;should happen:
  ;;
  ;;   (O c)
  ;;   ==> (<gamma>-c O)
  ;;
  ;;   ((O c) b)
  ;;   ==> (<beta>-b (<gamma>-c O))
  ;;
  ;;   (((O c) b) a)
  ;;   ==> (<alpha>-a (<beta>-b (<gamma>-c O)))
  ;;
  ;;we  also want  to support  nested  method invocations,  that is  the
  ;;following expansion should hapen:
  ;;
  ;;   (((O c) b) do 1 2)
  ;;   ==> (<alpha> :dispatch ((<beta>-b (<gamma>-c O)) do 1 2))
  ;;   ==> (<alpha>-do (<beta>-b (<gamma>-c O)) 1 2)
  ;;
  ;;We  also want  to support  nested  getter invocations,  that is  the
  ;;following expansion should happen:
  ;;
  ;;   (((O c) b) [123])
  ;;   ==> (<alpha> :getter ((<beta>-b (<gamma>-c O)) ([123])))
  ;;
  ;;but this case is not processed here.
  ;;
  ;;Notice that the  getter syntax is handled by  the getter transformer
  ;;function, not by the accessor function.
  ;;
  (with-syntax
      ((THE-TAG
	(parser-help.<parsed-spec>-name-id spec))
       (THE-PARENT
	(parser-help.<parsed-spec>-parent-id spec))
       (THE-RECORD-TYPE
	(if (parser-help.<class-spec>? spec)
	    (parser-help.<class-spec>-record-type-id spec)
	  #f))
       (((IMMUTABLE-FIELD IMMUTABLE-ACCESSOR IMMUTABLE-TAG) ...)
	(parser-help.<parsed-spec>-immutable-fields-data spec))
       (((MUTABLE-FIELD MUTABLE-ACCESSOR MUTABLE-MUTATOR MUTABLE-TAG) ...)
	(parser-help.<parsed-spec>-mutable-fields-data spec))
       (((IMMUTABLE-CONCRETE-FIELD UNSAFE-IMMUTABLE-CONCRETE-FIELD IMMUTABLE-CONCRETE-TAG) ...)
	(parser-help.<parsed-spec>-unsafe-immutable-fields-data spec))
       (((MUTABLE-CONCRETE-FIELD UNSAFE-MUTABLE-CONCRETE-FIELD MUTABLE-CONCRETE-TAG) ...)
	(parser-help.<parsed-spec>-unsafe-mutable-fields-data spec)))
    #'(lambda (original-stx expr-stx member-id args-stx synner)
	;;Process  a  field  accessor  form in  which:  EXPR-STX  is  an
	;;expression  evaluating to  a  Scheme object  of type  THE-TAG;
	;;MEMBER-ID is  an identifier representing the  tag member name;
	;;ARGS-STX is the list of additional arguments.
	;;
	;;ORIGINAL-STX is  a syntax  object holding the  original syntax
	;;use that generated the accessor call.
	;;
	;;We try to  match MEMBER-ID agains the field  names of THE-TAG,
	;;if  there is  a  match:  this is  a  field  accessor call  and
	;;ARGS-STX  must  be  null.   If  there  is  no  match  we  hand
	;;everything to the parent tag.
	;;
	;;In the latter case  it may be that MEMBER-ID is  the name of a
	;;parent's method, and ARGS-STX are the arguments for the method
	;;call.
	;;
	(define (%assert-null-args)
	  (unless (null? (syntax->datum args-stx))
	    (synner "invalid additional arguments in reference to tag field" args-stx)))
	(case (syntax->datum member-id)
	  ;;Safe accessors.
	  ((IMMUTABLE-FIELD)
	   (%assert-null-args)
	   #`(IMMUTABLE-TAG #:nested-oopp-syntax (IMMUTABLE-ACCESSOR #,expr-stx)))
	  ...
	  ((MUTABLE-FIELD)
	   (%assert-null-args)
	   #`(MUTABLE-TAG   #:nested-oopp-syntax (MUTABLE-ACCESSOR   #,expr-stx)))
	  ...
	  ;;Unsafe accessors.
	  ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
	   (%assert-null-args)
	   #`(IMMUTABLE-CONCRETE-TAG #:nested-oopp-syntax
				     ($record-type-field-ref THE-RECORD-TYPE IMMUTABLE-CONCRETE-FIELD #,expr-stx)))
	  ...
	  ((UNSAFE-MUTABLE-CONCRETE-FIELD)
	   (%assert-null-args)
	   #`(MUTABLE-CONCRETE-TAG #:nested-oopp-syntax
				   ($record-type-field-ref THE-RECORD-TYPE   MUTABLE-CONCRETE-FIELD #,expr-stx)))
	  ...
	  (else
	   #`(THE-PARENT :dispatch (#,expr-stx #,member-id . #,args-stx)))))))


(define* (make-mutator-transformer {spec parser-help.<parsed-spec>?})
  ;;Given the "parser-help.<parsed-spec>" instance SPEC: return a syntax
  ;;object representing the mutator transformer function for the tag.
  ;;
  ;;Whenever a tagged variable is referenced in a form like:
  ;;
  ;;   (set!/tags (?var ?field-name) ?val)
  ;;
  ;;the  syntax  object  holding  the  form is  handed  to  the  mutator
  ;;transformer function  to attempt a  match between ?FIELD-NAME  and a
  ;;field name;  if no  field name  matches, the form  is handed  to the
  ;;parent tag's mutator transformer.
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
  ;;   (<gamma> O ---)
  ;;
  ;;where  O is  the tagged  variable; the  following  syntax expansions
  ;;should happen:
  ;;
  ;;   (set!/tags (O c) 99)
  ;;   ==> (<gamma>-c-set! O 99)
  ;;
  ;;   (set!/tags ((O c) b) 99)
  ;;   ==> (<beta>-b-set! (<gamma>-c O) 99)
  ;;
  ;;   (set!/tags (((O c) b) a) 99)
  ;;   ==> (<alpha>-a-set! (<beta>-b (<gamma>-c O)) 99)
  ;;
  ;;We  also want  to support  nested  setter invocations,  that is  the
  ;;following expansion should happen:
  ;;
  ;;   (set!/tags (((O c) b)[77]) 99)
  ;;   ==> (<alpha> :setter ((<gamma>-c (<beta>-b O)) ([77]) 99))
  ;;
  ;;but this case is not processed here.
  ;;
  (with-syntax
      ((THE-TAG
	(parser-help.<parsed-spec>-name-id spec))
       (THE-PARENT
	(parser-help.<parsed-spec>-parent-id spec))
       (THE-RECORD-TYPE
	(if (parser-help.<class-spec>? spec)
	    (parser-help.<class-spec>-record-type-id spec)
	  #f))
       (((IMMUTABLE-FIELD IMMUTABLE-ACCESSOR IMMUTABLE-TAG) ...)
	(parser-help.<parsed-spec>-immutable-fields-data spec))
       (((MUTABLE-FIELD MUTABLE-ACCESSOR MUTABLE-MUTATOR MUTABLE-TAG) ...)
	(parser-help.<parsed-spec>-mutable-fields-data spec))
       (((IMMUTABLE-CONCRETE-FIELD UNSAFE-IMMUTABLE-CONCRETE-FIELD IMMUTABLE-CONCRETE-TAG) ...)
	(parser-help.<parsed-spec>-unsafe-immutable-fields-data spec))
       (((MUTABLE-CONCRETE-FIELD UNSAFE-MUTABLE-CONCRETE-FIELD MUTABLE-CONCRETE-TAG) ...)
	(parser-help.<parsed-spec>-unsafe-mutable-fields-data spec)))
    #'(lambda (original-stx expr-stx field-name-id new-value-stx)
	;;Process  a   mutator  form.   EXPR-STX  is   a  syntax  object
	;;representing an expression that will  evaluate to an object of
	;;type  THE-TAG.  FIELD-NAME-ID  is an  identifier that  must be
	;;matched against  the field  names.  NEW-VALUE-STX is  a syntax
	;;object representing  an expression  that will evaluate  to the
	;;new field's value.
	;;
	;;ORIGINAL-STX is  a syntax  object holding the  original syntax
	;;use that generated the mutator call.
	;;
	(case (syntax->datum field-name-id)
	  ;;Safe mutators.
	  ((MUTABLE-FIELD)
	   #`(MUTABLE-MUTATOR #,expr-stx (MUTABLE-TAG :assert-type-and-return #,new-value-stx)))
	  ...
	  ((IMMUTABLE-FIELD)
	   (syntax-violation 'THE-TAG "attempt to mutate immutable field" original-stx #'IMMUTABLE-FIELD))
	  ...
	  ;;Unsafe mutators.
	  ((UNSAFE-MUTABLE-CONCRETE-FIELD)
	   #`($record-type-field-set! THE-RECORD-TYPE MUTABLE-CONCRETE-FIELD #,expr-stx
				      (MUTABLE-CONCRETE-TAG :assert-type-and-return #,new-value-stx)))
	  ...
	  ((UNSAFE-IMMUTABLE-CONCRETE-FIELD)
	   (syntax-violation 'THE-TAG "attempt to mutate immutable field" original-stx #'IMMUTABLE-CONCRETE-FIELD))
	  ...
	  (else
	   #`(THE-PARENT :mutator #,expr-stx #,field-name-id #,new-value-stx))))))


(define (process-method-application rv-tag-id application-stx)
  ;;Process a  tag's method application  to support spliced  OOPP syntax
  ;;using the tag of a method's single return value.
  ;;
  ;;RV-TAG-ID must  be false to  indicate a method with  untagged return
  ;;value or a  method with multiple return values; RV-TAG-ID  must be a
  ;;tag identifier to indicate a method  with a single and tagged return
  ;;value.
  ;;
  ;;APPLICATION-STX  must be  a  syntax object  representing the  method
  ;;application.
  ;;
  ;;When there  is no  return-value tag or  the method  returns multiple
  ;;values: RV-TAG-ID is false and we just return the application syntax
  ;;object.
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
  ;;  ==> (<vector> #:nested-oopp-syntax (subvector V 0 1)))
  ;;  ==> (splice-first-expand
  ;;       (<vector> :flat-oopp-syntax (subvector V 0 1)))
  ;;
  ;;so that  if the  application is  the first  subform of  an enclosing
  ;;subform and there are arguments, the full expansion is:
  ;;
  ;;  ((V subvector 0 2) length)
  ;;  ==> ((<vector> #:nested-oopp-syntax (subvector V 0 1)) length)
  ;;  ==> ((splice-first-expand
  ;;        (<vector> :flat-oopp-syntax (subvector V 0 1))) length)
  ;;  ==> (<vector> :flat-oopp-syntax (subvector V 0 1) length)
  ;;  ==> (<vector> #:oopp-syntax ((subvector V 0 1) length))
  ;;  ==> (vector-length (subvector V 0 1))
  ;;
  ;;otherwise the expansion is just:
  ;;
  ;;  (begin (V subvector 0 2))
  ;;  ==> (begin
  ;;       (<vector> #:nested-oopp-syntax (subvector V 0 1)))
  ;;  ==> (begin
  ;;       (splice-first-expand
  ;;        (<vector> :flat-oopp-syntax (subvector V 0 1))))
  ;;  ==> (begin
  ;;       (<vector> :flat-oopp-syntax (subvector V 0 1)))
  ;;  ==> (begin (subvector V 0 1))
  ;;
  ;; (debug-print 'process-method-application
  ;; 	       'return-value-tag (syntax->datum rv-tag-id)
  ;; 	       'method-call (syntax->datum (if (syntax->datum rv-tag-id)
  ;; 					       #`(#,rv-tag-id #:nested-oopp-syntax #,application-stx)
  ;; 					     application-stx)))
  (if (syntax->datum rv-tag-id)
      #`(#,rv-tag-id #:nested-oopp-syntax #,application-stx)
    application-stx))


(define (process-shadowed-identifier tag-id shadowed-id body-stx)
  ;;Replace all the occurrences of TAG-ID in the BODY-STX forms with the
  ;;identifier selected by the SHADOWS clause: SHADOWED-ID.  This allows
  ;;a label tag to be used to handle some other entity type.
  ;;
  ;;SHADOWED-ID has #f as datum  if no shadowed identifier was specified
  ;;in the label definition.
  ;;
  (if (syntax->datum shadowed-id)
      (single-identifier-subst tag-id shadowed-id body-stx)
    body-stx))


(case-define parse-with-tags-bindings
  ((bindings-stx synner)
   (parse-with-tags-bindings bindings-stx synner '() '() '()))
  ((bindings-stx synner vars tags syntax-bindings)
   ;;Recursive function.  Parse the syntax object BINDINGS-STX expecting
   ;;it to  be a list  of tagged WITH-TAGS bindings;  supported syntaxes
   ;;for the bindings are:
   ;;
   ;;   ()
   ;;   (?var0 ?var ...)
   ;;
   ;;where each ?VAR must have the following syntax:
   ;;
   ;;   (brace ?var-id ?tag-id)
   ;;
   ;;The return value is a syntax object with the structure:
   ;;
   ;;   ((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
   ;;
   ;;where each  VAR is an  identifier to be  used to create  a binding,
   ;;each TAG is the identifier of  the type tag and each SYNTAX-BINDING
   ;;is the associated LET-SYNTAX binding.
   ;;
   ;;SYNNER must be a closure to be used to raise syntax violations.
   ;;
   (syntax-case bindings-stx (brace)
     ;;No more bindings.
     (()
      (list (reverse vars) (reverse tags) (reverse syntax-bindings)))

     ;;Tagged binding, brace envelope.
     (((brace ?var ?tag) . ?other-bindings)
      (and (identifier? #'?var)
	   (identifier? #'?tag))
      (let ((tag-id #'?tag))
	(parse-with-tags-bindings #'?other-bindings synner
				  (cons #'?var vars)
				  (cons tag-id tags)
				  (cons #'(?var (make-tagged-variable-transformer #'?tag #'?var))
					syntax-bindings))))

     (_
      (synner "invalid bindings syntax" bindings-stx)))))

(case-define parse-let-bindings
  ((bindings-stx top-id synner)
   (parse-let-bindings bindings-stx top-id synner '() '() '()))
  ((bindings-stx top-id synner vars tags syntax-bindings)
   ;;Recursive function.  Parse the syntax object BINDINGS-STX expecting
   ;;it to be a list of  tagged LET bindings; supported syntaxes for the
   ;;bindings are:
   ;;
   ;;   ()
   ;;   (?var0 ?var ...)
   ;;
   ;;where each ?VAR must have one of the following syntaxes:
   ;;
   ;;   ?var-id
   ;;   (brace ?var-id ?tag-id)
   ;;
   ;;The return value is a syntax object with the structure:
   ;;
   ;;   ((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
   ;;
   ;;where each  VAR is an  identifier to be  used to create  a binding,
   ;;each TAG is the identifier of  the type tag and each SYNTAX-BINDING
   ;;is the associated LET-SYNTAX binding.  If a variable is tagged with
   ;;TOP-ID: no syntax binding is generated.
   ;;
   ;;When the BINDINGS-STX comes from  a LET, the returned syntax object
   ;;should be used to compose an output form as:
   ;;
   ;;   #'(let ((VAR (TAG :assert-type-and-return ?init)) ...)
   ;;       (let-syntax (SYNTAX-BINDING ...)
   ;;         ?body0 ?body ...))
   ;;
   ;;TOP-ID must the  the identifier bound to the "<top>"  tag; this tag
   ;;is used as default when no tag is given for a binding.  SYNNER must
   ;;be a closure to be used to raise syntax violations.
   ;;
   (syntax-case bindings-stx (brace)
     ;;No more bindings.
     (()
      (list (reverse vars) (reverse tags) (reverse syntax-bindings)))

     ;;Tagged binding, brace envelope.
     (((brace ?var ?tag) . ?other-bindings)
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

     (_
      (synner "invalid bindings syntax" bindings-stx)))))

(case-define parse-let-values-bindings
  ((bindings-stx top-id synner)
   (parse-let-values-bindings bindings-stx top-id synner '() '()))
  ((bindings-stx top-id synner values-vars syntax-bindings)
   ;;Recursive function.  Parse the syntax object BINDINGS-STX expecting
   ;;it to be  a list of tagged LET-VALUES  bindings; supported syntaxes
   ;;for the bindings are:
   ;;
   ;;   ()
   ;;   (?vars0 ?vars ...)
   ;;
   ;;where each ?VARS must have the syntax of the tagged LAMBDA formals:
   ;;
   ;;   (?var0 ?var ...)
   ;;   (?var0 ?var ... . ?rest)
   ;;   ?var-id
   ;;
   ;;and each ?VAR must have one of the following syntaxes:
   ;;
   ;;   ?var-id
   ;;   (brace ?var-id ?tag-id)
   ;;
   ;;The return value is a syntax object with the structure:
   ;;
   ;;   ((VARS ...) (SYNTAX-BINDING ...))
   ;;
   ;;where  each VARS  is a  list of  identifiers to  be used  to create
   ;;bindings  and  the  SYNTAX-BINDING are  the  associated  LET-SYNTAX
   ;;bindings.  If a  variable is tagged with TOP-ID:  no syntax binding
   ;;is generated.
   ;;
   ;;When the BINDINGS-STX comes from  a LET-VALUES, the returned syntax
   ;;object should be used to compose an output form as:
   ;;
   ;;   #'(let-values ((VARS ?init) ...)
   ;;       (let-syntax (SYNTAX-BINDING ...)
   ;;         ?body0 ?body ...))
   ;;
   ;;TOP-ID must the  the identifier bound to the "<top>"  tag; this tag
   ;;is used as default when no tag is given for a binding.  SYNNER must
   ;;be a closure to be used to raise syntax violations.
   ;;
   (define-syntax-rule (%final ?stx)
     (reverse (syntax->list ?stx)))
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
      (synner "invalid bindings syntax" bindings-stx)))))

(define (parse-formals-bindings formals-stx top-id synner)
  ;;Parse the  syntax object  FORMALS-STX expecting it  to be a  list of
  ;;tagged LAMBDA formals; supported syntaxes for the formals are:
  ;;
  ;;   ()
  ;;   ?args-id
  ;;   (brace ?args-id ?tag-id)
  ;;   (?var0 ?var ...)
  ;;   (?var0 ?var ... . ?rest)
  ;;
  ;;where  ?ARGS-ID is an  identifier, each  ?VAR must  have one  of the
  ;;following syntaxes:
  ;;
  ;;   ?var-id
  ;;   (brace ?var-id ?tag-id)
  ;;
  ;;and ?REST must have one of the following syntaxes:
  ;;
  ;;   ?rest-id
  ;;   (brace ?rest-id ?tag-id)
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
  (syntax-case formals-stx (brace)
    ;;No arguments.
    (()
     #'(() () ()))

    ;;List of all the arguments, with tag.
    ((brace ?args-id ?tag-id)
     (and (identifier? #'?args-id)
	  (identifier? #'?tag-id))
     (with-syntax ((((VAR) (TAG) (BINDING))
		    (parse-let-bindings #'((brace ?args-id ?tag-id)) top-id synner)))
       #'(VAR ((TAG VAR)) (BINDING))))

    ;;List of all the arguments, no tag.
    (?args-id
     (identifier? #'?args-id)
     #'(?args-id () ()))

    ;;Mandatory arguments plus untagged rest argument.
    ((?var0 ?var ... . ?rest)
     (identifier? #'?rest)
     (with-syntax ((((REST-VAR VAR ...) (REST-TAG TAG ...) (BINDING ...))
		    (parse-let-bindings #'(?rest ?var0 ?var ...) top-id synner)))
       #'((VAR ... . REST-VAR) ((TAG VAR) ... (REST-TAG REST-VAR)) (BINDING ...))))

    ;;Mandatory arguments plus tagged rest argument.
    ((?var0 ?var ... . (brace ?rest ?rest-tag))
     (with-syntax ((((REST-VAR VAR ...) (REST-TAG TAG ...) (BINDING ...))
		    (parse-let-bindings #'((brace ?rest ?rest-tag) ?var0 ?var ...) top-id synner)))
       #'((VAR ... . REST-VAR) ((TAG VAR) ... (REST-TAG REST-VAR)) (BINDING ...))))

    ;;Fixed number of arguments.
    ((?var ...)
     (with-syntax ((((VAR ...) (TAG ...) (BINDING ...))
		    (parse-let-bindings #'(?var ...) top-id synner)))
       #'((VAR ...) ((TAG VAR) ...) (BINDING ...))))
    ))


;;;; done

)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
