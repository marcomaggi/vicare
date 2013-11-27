;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tags implementation
;;;Date: Fri May 18, 2012
;;;
;;;Abstract
;;;
;;;	This library implements the syntaxes defining classes and labels
;;;	for  "object-oriented perfumed  programming" (OOPP).   With this
;;;	library  Scheme does  not  really become  an  OOP language;  the
;;;	coding style  resulting from using these features  is similar to
;;;	using "void  *" pointers in the  C language and  casting them to
;;;	some structure pointer type when needed.
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


#!r6rs
(library (nausicaa language oopp)
  (export
    define-label		define-class		define-mixin
    make-from-fields		is-a?
    slot-set!			slot-ref
    tag-unique-identifiers

    define/tags			define-values/tags
    lambda/tags			case-lambda/tags
    with-tags			begin/tags
    let/tags			let*/tags
    letrec/tags			letrec*/tags
    let-values/tags		let*-values/tags
    receive/tags		receive-and-return/tags
    do/tags			do*/tags
    set!/tags
    with-label-shadowing
    with-tagged-arguments-validation

    <top> <top>? <top>-unique-identifiers

    ;; conditions
    &tagged-binding-violation
    make-tagged-binding-violation
    tagged-binding-violation?

    ;; auxiliary keywords
    (rename (aux.parent			parent)
	    (aux.nongenerative		nongenerative)
	    (aux.abstract		abstract)
	    (aux.sealed			sealed)
	    (aux.opaque			opaque)
	    (aux.predicate		predicate)
	    (aux.fields			fields)
	    (aux.virtual-fields		virtual-fields)
	    (aux.mutable		mutable)
	    (aux.immutable		immutable)
	    (aux.method			method)
	    (aux.method-syntax		method-syntax)
	    (aux.methods		methods)
	    (aux.protocol		protocol)
	    (aux.public-protocol	public-protocol)
	    (aux.super-protocol		super-protocol)
	    (aux.maker			maker)
	    (aux.finaliser		finaliser)
	    (aux.getter			getter)
	    (aux.setter			setter)
	    (aux.shadows		shadows)
	    (aux.satisfies		satisfies)
	    (aux.mixins			mixins)
	    (aux.<>			<>)
	    (aux.<-			<-)))
  (import (except (vicare)
		  define-syntax)
    (prefix (only (rnrs)
		  define-syntax)
	    rnrs.)
    (nausicaa language oopp auxiliary-syntaxes)
    (for (prefix (nausicaa language oopp helpers)
		 help.)
	 expand)
    (for (only (nausicaa language oopp helpers)
	       case-symbol
	       case-identifier)
	 expand)
    (for (prefix (only (nausicaa language oopp configuration)
		       validate-tagged-values?)
		 config.)
	 expand)
    (prefix (only (nausicaa language auxiliary-syntaxes)
		  <>			<-
		  parent		nongenerative
		  sealed		opaque
		  predicate		abstract
		  fields		virtual-fields
		  mutable		immutable
		  method		method-syntax		methods
		  protocol		public-protocol		super-protocol
		  getter		setter
		  shadows		satisfies
		  mixins		maker			finaliser)
	    aux.)
    (vicare unsafe operations))


;;;; helpers

(rnrs.define-syntax define-syntax
  ;;This is  like DEFINE-SYNTAX from  (vicare), but in  addition defines
  ;;the function SYNNER as helper to raise syntax violations.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?name)
       (identifier? #'?name)
       #'(rnrs.define-syntax ?name))
      ((_ ?name ?lambda)
       (identifier? #'?name)
       #'(rnrs.define-syntax ?name ?lambda))
      ((_ (?name ?stx) ?body0 ?body ...)
       (and (identifier? #'?name)
	    (identifier? #'?stx))
       (with-syntax ((SYNNER (datum->syntax #'?name 'synner)))
	 #'(rnrs.define-syntax ?name
	     (lambda (?stx)
	       (define SYNNER
		 (case-lambda
		  ((message)
		   (SYNNER message #f))
		  ((message subform)
		   (syntax-violation '?name message ?stx subform))))
	       ?body0 ?body ...))))
      )))

(define-condition-type &tagged-binding-violation
    &assertion
  make-tagged-binding-violation
  tagged-binding-violation?)

(define (tagged-binding-violation who message . irritants)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      (make-tagged-binding-violation)
	      (make-irritants-condition irritants))))


(define-record-type (<top>-record-type make-<top> <top>?)
  ;;This is the super class of all the classes and labels.
  ;;
  ;;This type has  fields, but their use is restricted  to this library;
  ;;their existence must be hidden  from the subclasses of "<top>".  For
  ;;this reason we cannot use the  default protocol defined by this form
  ;;to build instances  of "<top>"; rather we  use the "<top>-super-rcd"
  ;;defined below which defines default values for the fields.
  ;;
  (nongenerative nausicaa:builtin:<top>)
  (fields (mutable unique-identifiers <top>-unique-identifiers <top>-unique-identifiers-set!)
		;List  of  UIDs identifying  the  subclass  to which  an
		;instance  belongs.   This  field is  used  to  speed-up
		;dispatching  of  multimethods:  we accept  the  greater
		;memory  usage  to  allow  some more  speed  in  generic
		;functions.
	  ))

(define <top>-super-rcd
  ;;We need a  constructor for "<top>", to be  used as super-constructor
  ;;by   subclasses    of   "<top>",   that   initialises    the   field
  ;;UNIQUE-IDENTIFIERS, hiding it from the subclass's constructors.
  ;;
  (make-record-constructor-descriptor
   (record-type-descriptor <top>-record-type) #f
   (lambda (make-instance)
     (lambda ()
       (make-instance '())))))

(define <top>-unique-ids
  ;;This is the list of UIDs for the type "<top>".
  ;;
  '(nausicaa:builtin:<top>))

(define (<top>-predicate obj)
  ;;This  function is  used as  predicate for  "<top>" in  place of  the
  ;;predefined  "<top>?".   By  convention:   all  the  objects  in  the
  ;;(nausicaa) language are instances of "<top>".
  ;;
  #t)

(define-syntax (<top> stx)
  ;;Tag syntax  for "<top>",  all the operations  involving this  tag go
  ;;through this syntax.  This tag is  the supertag of all the class and
  ;;label tags.
  ;;
  ;;In all the branches:
  ;;
  ;;* ?EXPR must be an expression  to be evaluated only once; its result
  ;;must be an instance of the  subtag type.  ?EXPR can be an identifier
  ;;but also the application of an accessor to an instance.
  ;;
  ;;*  ?VAR  must  be  the  identifier  bound  to  the  instance  syntax
  ;;dispatcher.
  ;;
  (syntax-case stx ( ;;
		    :define :let :make :make-from-fields :is-a?
		    :dispatch :accessor :mutator :getter :setter
		    :insert-parent-clause define-record-type
		    :insert-constructor-fields lambda
		    :super-constructor-descriptor :assert-type-and-return
		    :assert-procedure-argument :assert-expression-return-value
		    :append-unique-id :list-of-unique-ids
		    :predicate-function :accessor-function :mutator-function
		    aux.<>)
    ((_ :define ?var ?expr)
     (identifier? #'?var)
     #'(define ?var ?expr))

    ((_ :define ?var)
     (identifier? #'?var)
     #'(define ?var))

    ((_ :let ??expr ??var ??body0 ??body ...)
     #'(let ((??var ??expr))
	 ??body0 ??body ...))

    ((_ :make . ?args)
     (synner "invalid maker call syntax for <top> tag"))

    ((_ :make-from-fields . ?args)
     (synner "invalid :make-from-fields call syntax for <top> tag"))

    ;;Every  object  is of  type  "<top>"  by  definition.  We  have  to
    ;;evaluate the given expression for its side effects.
    ((_ :is-a? ?expr)
     #'(begin
	 ?expr
	 #t))

    ;;If a "<top>" value receives a dispatch request: what do we do?
    ;;
    ;;Creative solution: just interpret it as a function application.
    ;;
    ;; ((_ :dispatch ?expr (?var . ?args))
    ;;  (identifier? #'?var)
    ;;  #'(?expr . ?args))
    ;;
    ;;Logic solution: raise an error.
    ((_ :dispatch ?expr (?var . ?args))
     (identifier? #'?var)
     (synner "invalid tag member"))

    ((_ :accessor ?expr (?var . ?args))
     (synner "invalid tag-syntax field-accessor request"))

    ((_ :mutator ?expr ?keys ?value)
     (synner "invalid tag-syntax field-mutator request"))

    ((_ :getter ?expr (?var . ?args))
     (synner "invalid tag-syntax getter request"))

    ((_ :setter ?expr (?var . ?args))
     (synner "invalid tag-syntax setter request"))

    ;;Given an R6RS record type definition: insert an appropriate PARENT
    ;;clause so that the type is derived from "<top>-record-type".
    ((_ :insert-parent-clause (define-record-type ?name . ?clauses))
     #'(define-record-type ?name (parent <top>-record-type) . ?clauses))

    ;;For common  tags: this rule should  insert the field names  in the
    ;;appropriate  position  in  the  definition of  a  custom  protocol
    ;;function.   But this  is the  "<top>" tag  which keeps  hidden its
    ;;fields, so just return the input expression.
    ((_ :insert-constructor-fields (lambda (make-parent-1)
				     (lambda (V ...)
				       ((make-parent-2 W ...) Z ...))))
     #'(lambda (make-parent-1)
	 (lambda (V ...)
	   ((make-parent-2 W ...) Z ...))))

    ((_ :super-constructor-descriptor)
     #'<top>-super-rcd)

    ;;This is  used for values  validation by tagged variables;  it must
    ;;work like the  R6RS ASSERT syntax.  By convention:  all the values
    ;;are of type "<top>", so we just evaluate the expression and return
    ;;its value.
    ((_ :assert-type-and-return ?expr)
     #'?expr)
    ((_ :assert-procedure-argument ?expr)
     #'(void))
    ((_ :assert-expression-return-value ?expr)
     #'?expr)

    ((_ :append-unique-id (?id ...))
     #'(quote (?id ... nausicaa:builtin:<top>)))

    ((_ :list-of-unique-ids)
     #'<top>-unique-ids)

    ((_ :predicate-function)
     #'<top>-predicate)

    ;;The tag "<top>" has no accessible fields.
    ((_ :accessor-function ?field-name)
     (identifier? #'?field-name)
     (synner "invalid tag-syntax field accessor function request" #'?field-name))

    ;;The tag "<top>" has no accessible fields.
    ((_ :mutator-function ?field-name)
     (identifier? #'?field-name)
     (synner "invalid tag-syntax field mutator function request" #'?field-name))

    ;;Define an  internal variable with initialisation  expression using
    ;;the tag constructor.  The syntax use:
    ;;
    ;;   (<top> ?var (<> (?arg ...)))
    ;;
    ;;is equivalent to:
    ;;
    ;;   (define ?var (<top> (?arg ...)))
    ;;
    ;;and the constructor of "<top>" will raise an error.
    ((?tag ?var (aux.<> (?arg ...)))
     (identifier? #'?var)
     #'(define ?var (?tag (?arg ...))))

    ;;Define an  internal variable with initialisation  expression.  The
    ;;syntax use:
    ;;
    ;;   (<top> ?var ?expr)
    ;;
    ;;just defines a common internal binding.
    ((_ ?var ?expr)
     (identifier? #'?var)
     #'(define ?var ?expr))

    ;;Define an  internal variable  without initialisation.   The syntax
    ;;use:
    ;;
    ;;   (<top> ?var)
    ;;
    ;;just defines a common internal binding.
    ((_ ?var)
     (identifier? #'?var)
     #'(define ?var))

    ;;Constructor call syntax.
    ((_ (?arg ...))
     (synner "invalid maker call syntax for <top> tag"))

    ;;Predicate reference.  It is meant to be used as:
    ;;
    ;;  ((<top>) ?expr) => #t
    ;;
    ((_)
     #'<top>-predicate)

    (_
     (synner "invalid tag syntax"))))


(define-syntax (define-label stx)
  ;;Define a new label type.  After  all the processing: expand to a set
  ;;of syntax definitions and  miscellaneous bindings for predicates and
  ;;constructors.
  ;;
  ;;In this  macro transformer we  only process the MIXINS  clauses, the
  ;;actual label definition is performed by another macro.
  ;;
  (syntax-case stx ()
    ((_ ?name ?clause ...)
     (receive (mixins-clauses other-clauses)
	 (help.filter-and-validate-mixins-clauses #'(?clause ...) synner)
       (if (null? mixins-clauses)
	   #`(define-label/after-mixins-processing ?name . #,other-clauses)
	 (with-syntax ((((MIXIN (FROM TO) ...) MIXIN-SPEC ...) mixins-clauses))
	   #`(MIXIN :insert-mixin-clauses
		    (define-label/after-mixins-processing ?name . #,other-clauses)
		    ((FROM TO) ...)
		    (MIXIN-SPEC ...))))))
    (_
     (synner "invalid syntax in label definition"))))

(define-syntax (define-label/after-mixins-processing stx)
  (define spec   (help.parse-label-definition stx #'<top> #'lambda/tags synner))
  (define tag-id (help.<parsed-spec>-name-id spec))
  (with-syntax
      ((THE-TAG			tag-id)
       (THE-PARENT		(help.<parsed-spec>-parent-id spec))
       (THE-PUBLIC-CONSTRUCTOR	(help.<parsed-spec>-public-constructor-id spec))
       (THE-PUBLIC-PREDICATE	(help.<parsed-spec>-public-predicate-id   spec))
       (THE-PRIVATE-PREDICATE	(help.<parsed-spec>-private-predicate-id  spec))

       (THE-LIST-OF-UIDS	(help.tag-id->list-of-uids-id tag-id))
       (NONGENERATIVE-UID	(help.<parsed-spec>-nongenerative-uid     spec))

       (((IMMUTABLE-FIELD IMMUTABLE-ACCESSOR IMMUTABLE-TAG) ...)
	(help.<parsed-spec>-immutable-fields-data spec))

       (((MUTABLE-FIELD MUTABLE-ACCESSOR MUTABLE-MUTATOR MUTABLE-TAG) ...)
	(help.<parsed-spec>-mutable-fields-data spec))

       (((METHOD-NAME METHOD-RV-TAG METHOD-IMPLEMENTATION) ...)
	(help.<parsed-spec>-methods-table spec))

       (SHADOWED-IDENTIFIER
	(help.<parsed-spec>-shadowed-identifier spec))

       ((DEFINITION ...)
	(help.<parsed-spec>-definitions spec))

       (ACCESSOR-TRANSFORMER
	(help.<parsed-spec>-accessor-transformer spec))

       (MUTATOR-TRANSFORMER
	(help.<parsed-spec>-mutator-transformer  spec))

       (MAKER-TRANSFORMER
	(help.<parsed-spec>-maker-transformer spec))

       ((SATISFACTION ...)
	(help.<parsed-spec>-satisfactions spec))
       (SATISFACTION-CLAUSES
	(help.<label-spec>-satisfaction-clauses spec))

       (WRONG-TYPE-ERROR-MESSAGE
	(string-append "invalid expression result, expected value of type "
		       (symbol->string (syntax->datum tag-id)))))
    (with-syntax
	((THE-PUBLIC-PROTOCOL-EXPR
	  ;;Labels   have  no   record-type  descriptor,   so,  strictly
	  ;;speaking,  we do not  need to  follow the  same construction
	  ;;protocol of R6RS records; but uniformity makes understanding
	  ;;easier, so  the label construction protocols  are similar to
	  ;;those of proper records.
	  (or (help.<parsed-spec>-public-protocol spec)
	      (help.<parsed-spec>-common-protocol spec)
	      #'(lambda ()
		  (lambda args
		    (assertion-violation 'THE-TAG
		      "no constructor defined for label" 'THE-TAG args)))))

	 (PREDICATE-EXPR
	  ;;The  predicate  expression  must   evaluate  to  a  function
	  ;;predicate for  instances of this  tag.  We must  check first
	  ;;the  parent tag's  predicate,  then the  predicate for  this
	  ;;label; the "<top>" tag has no predicate.
	  ;;
	  ;;NOTE Remember that the PREDICATE  clause can also select the
	  ;;binding  of a  syntax;  we  have no  way  to distinguish  if
	  ;;THE-PRIVATE-PREDICATE  is a  syntax's  keyword or  something
	  ;;else, so we always have to create a function here, we cannot
	  ;;just return THE-PRIVATE-PREDICATE when it is an identifier.
	  (let ((has-priv-id? (syntax->datum #'THE-PRIVATE-PREDICATE)))
	    (cond ((free-identifier=? #'THE-PARENT #'<top>)
		   ;;The parent is  "<top>": we just use  for this tag's
		   ;;predicate, if any was selected.
		   (if has-priv-id?
		       #'(lambda (obj)
			   (THE-PRIVATE-PREDICATE obj))
		     #'(lambda (obj) #t)))
		  (has-priv-id?
		   ;;The parent  is not "<top>" and  this tag definition
		   ;;selects a  predicate: apply the  parent's predicate
		   ;;first, than this tag's one.
		   #'(lambda (obj)
		       (and (THE-PARENT :is-a? obj)
			    (THE-PRIVATE-PREDICATE obj))))
		  (else
		   ;;The parent  is not "<top>" and  this tag definition
		   ;;does  not  select  a   predicate:  just  apply  the
		   ;;parent's predicate.
		   #'(lambda (obj)
		       (THE-PARENT :is-a? obj))))))

	 (GETTER-TRANSFORMER
	  ;;If no getter  syntax is defined for this  label: use the one
	  ;;of  the  parent.  The  getter  of  "<top>"  raises a  syntax
	  ;;violation error.
	  (or (help.<parsed-spec>-getter spec)
	      #`(lambda (stx)
		  (syntax-case stx ()
		    (?form
		     #'(THE-PARENT :getter #f ?form))))))

	 (SETTER-TRANSFORMER
	  ;;If no setter  syntax is defined for this  label: use the one
	  ;;of  the  parent.  The  setter  of  "<top>"  raises a  syntax
	  ;;violation error.
	  (or (help.<parsed-spec>-setter spec)
	      #`(lambda (stx)
		  (syntax-case stx ()
		    (?form
		     #'(THE-PARENT :setter #f ?form)))))))
      #'(begin

	  (define THE-PUBLIC-CONSTRUCTOR
	    (THE-PUBLIC-PROTOCOL-EXPR))

	  (define THE-PUBLIC-PREDICATE PREDICATE-EXPR)

	  (define THE-LIST-OF-UIDS
	    (THE-PARENT :append-unique-id (NONGENERATIVE-UID)))

	  (define-syntax THE-TAG
	    ;;Tag  syntax,  all the  operations  involving  this tag  go
	    ;;through this syntax.  For all the patterns:
	    ;;
	    ;;* ??EXPR must be an  expression to be evaluated only once;
	    ;;its result  must be an  instance of the tag  type.  ??EXPR
	    ;;can  be  an identifier  but  also  the  application of  an
	    ;;accessor to an instance.
	    ;;
	    ;;*  ??VAR must  be  the identifier  bound  to the  instance
	    ;;syntax dispatcher.
	    ;;
	    (let ((%the-getter		GETTER-TRANSFORMER)
		  (%the-setter		SETTER-TRANSFORMER)
		  (%the-accessor	ACCESSOR-TRANSFORMER)
		  (%the-mutator		MUTATOR-TRANSFORMER)
		  (%the-maker		MAKER-TRANSFORMER))
	      (lambda (stx)
		(define (synner message subform)
		  (syntax-violation 'THE-TAG message stx subform))

		(syntax-case stx ( ;;
				  :define :let :make :is-a?
				  :dispatch :accessor :mutator :getter :setter
				  :assert-type-and-return
				  :assert-procedure-argument :assert-expression-return-value
				  :append-unique-id :list-of-unique-ids
				  :predicate-function :accessor-function :mutator-function
				  :process-shadowed-identifier
				  aux.<>)

		  ;;Try  to match  the tagged-variable  use to  a method
		  ;;call for  the tag; if  no method name  matches ??ID,
		  ;;try to match a field name.
		  ((_ :dispatch ??expr (??var ??id . ??args))
		   (and (identifier? #'??var)
			(identifier? #'??id))
		   (case-symbol (syntax->datum #'??id)
		     ((METHOD-NAME)
		      (help.process-method-application #'let/tags #'METHOD-RV-TAG
						       #'(METHOD-IMPLEMENTATION ??expr . ??args)))
		     ...
		     (else
		      (%the-accessor stx #'??expr #'??var (cons #'??id #'??args)))))

		  ((_ :accessor ??expr (??var . ??args))
		   (identifier? #'??var)
		   (%the-accessor stx #'??expr #'??var #'??args))

		  ;;Invoke the  mutator syntax transformer.   The syntax
		  ;;use:
		  ;;
		  ;;   (set!/tags (?var ?field-name ?arg ...) ?val)
		  ;;
		  ;;is expanded to:
		  ;;
		  ;;   (?var :mutator (?field-name ?arg ...) ?val)
		  ;;
		  ;;and then to:
		  ;;
		  ;;   (THE-TAG :mutator SRC-VAR (?field-name ?arg ...) ?val)
		  ;;
		  ;;where SRC-VAR is the  identifier bound to the actual
		  ;;instance.  Then,  depending on  the ?ARG  forms, the
		  ;;mutator  can  expand  to:  a  simple  field  mutator
		  ;;invocation, or to a  subfield mutator invocation, or
		  ;;to a submethod invocation.
		  ;;
		  ((_ :mutator ??expr ??keys ??value)
		   (%the-mutator stx #'??expr #'??keys #'??value))

		  ((_ :getter ??expr (??var ((??key0 (... ...))
					     (??key (... ...))
					     (... ...))))
		   (identifier? #'??var)
		   (%the-getter #'(??var ((??key0 (... ...))
					  (??key  (... ...))
					  (... ...)))))

		  ((_ :setter ??expr (??var ((??key0 (... ...))
					     (??key (... ...))
					     (... ...))
					    ??value))
		   (identifier? #'??var)
		   (%the-setter #'(??var ((??key0 (... ...))
					  (??key (... ...))
					  (... ...))
					 ??value)))

		  ((_ :assert-type-and-return ??expr)
		   (if config.validate-tagged-values?
		       #'(receive-and-return (val)
			     ??expr
			   (unless (THE-TAG :is-a? val)
			     (tagged-binding-violation 'THE-TAG
			       WRONG-TYPE-ERROR-MESSAGE
			       '(expression: ??expr)
			       `(result: ,val))))
		     #'??expr))

		  ((_ :assert-procedure-argument ??id)
		   (identifier? #'??id)
		   ;;This DOES NOT return the value.
		   (if config.validate-tagged-values?
		       #'(unless (THE-TAG :is-a? ??id)
			   (procedure-argument-violation 'THE-TAG
			     "tagged procedure argument of invalid type" ??id))
		     #'(void)))

		  ((_ :assert-expression-return-value ??expr)
		   ;;This DOES return the value.
		   (if config.validate-tagged-values?
		       #'(receive-and-return (val)
			     ??expr
			   (unless (THE-TAG :is-a? val)
			     (expression-return-value-violation 'THE-TAG
			       "tagged expression return value of invalid type" val)))
		     #'??expr))

		  ;; public API: auxiliary syntaxes

		  ;;Define  internal  bindings  for a  tagged  variable.
		  ;;Without initialisation expression.
		  ((_ :define ??var)
		   (identifier? #'??var)
		   #'(begin
		       (define src-var)
		       (define-syntax ??var
			 (help.make-tagged-variable-transformer #'THE-TAG #'src-var))))

		  ;;Define  internal  bindings  for a  tagged  variable.
		  ;;With initialisation expression.
		  ((_ :define ??var ??expr)
		   (identifier? #'??var)
		   #'(begin
		       (define src-var (THE-TAG :assert-type-and-return ??expr))
		       (define-syntax ??var
			 (help.make-tagged-variable-transformer #'THE-TAG #'src-var))))

		  ((_ :let ??expr ??var ??body0 ??body (... ...))
		   #'(let ((src-var ??expr))
		       (let-syntax ((??var (help.make-tagged-variable-transformer #'THE-TAG #'src-var)))
			 ??body0 ??body (... ...))))

		  ((_ :make . ??args)
		   #'(THE-PUBLIC-CONSTRUCTOR . ??args))

		  ((_ :is-a? . ??args)
		   #'(THE-PUBLIC-PREDICATE . ??args))

		  ((_ :append-unique-id (??id (... ...)))
		   #'(THE-PARENT :append-unique-id (??id (... ...) NONGENERATIVE-UID)))

		  ((_ :list-of-unique-ids)
		   #'THE-LIST-OF-UIDS)

		  ((_ :predicate-function)
		   #'THE-PUBLIC-PREDICATE)

		  ((_ :accessor-function ??field-name)
		   (identifier? #'??field-name)
		   (case-symbol (syntax->datum #'??field-name)
		     ((IMMUTABLE-FIELD)	#'(lambda (obj) (IMMUTABLE-ACCESSOR obj))) ...
		     ((MUTABLE-FIELD)	#'(lambda (obj) (MUTABLE-ACCESSOR   obj))) ...
		     (else
		      #'(THE-PARENT :mutator-function ??field-name))))

		  ((_ :mutator-function ??field-name)
		   (identifier? #'??field-name)
		   (case-symbol (syntax->datum #'??field-name)
		     ((MUTABLE-FIELD)
		      #'(lambda (obj val) (MUTABLE-MUTATOR obj val)))
		     ...
		     ((IMMUTABLE-FIELD)
		      (synner "request of mutator function for immutable field" #'IMMUTABLE-FIELD))
		     ...
		     (else
		      #'(THE-PARENT :mutator-function ??field-name))))

		  ;;Replace  all  the  occurrences of  ??SRC-ID  in  the
		  ;;??BODY  forms with  the identifier  selected by  the
		  ;;SHADOWS clause.  This allows a  label tag to be used
		  ;;to handle some other entity type.
		  ;;
		  ;;Notice  that we really  substitute ??SRC-ID  from the
		  ;;input form rather than THE-TAG.
		  ((??src-id :process-shadowed-identifier ??body0 ??body (... ...))
		   (let ((dst-id	#'SHADOWED-IDENTIFIER)
			 (body		#'(begin ??body0 ??body (... ...))))
		     (if (syntax->datum dst-id)
			 (help.single-identifier-subst #'??src-id dst-id body)
		       body)))

		  ;;Define  an  internal  variable  with  initialisation
		  ;;expression using the tag constructor.
		  ((??tag ??var (aux.<> (??arg (... ...))))
		   (identifier? #'??var)
		   #'(??tag ??var (??tag (??arg (... ...)))))

		  ;;Internal definition with initialisation expression.
		  ((_ ??var ??expr)
		   (identifier? #'??var)
		   #'(THE-TAG :define ??var ??expr))

		  ;;Internal     definition    without    initialisation
		  ;;expression.
		  ((_ ??var)
		   (identifier? #'??var)
		   #'(THE-TAG :define ??var))

		  ;;Constructor  call.   If   a  maker  transformer  was
		  ;;defined:  use it,  otherwise default  to the  public
		  ;;constructor.
		  ((_ (??arg (... ...)))
		   (if %the-maker
		       (%the-maker stx)
		     #'(THE-PUBLIC-CONSTRUCTOR ??arg (... ...))))

		  ;;Predicate reference.  It is meant to be used as:
		  ;;
		  ;;  ((<vector>) '#()) => #t
		  ;;  ((<vector>) 1234) => #f
		  ;;
		  ((_)
		   #'THE-PUBLIC-PREDICATE)

		  (_
		   (synner "invalid tag syntax" #f))))
	      ))

	  DEFINITION ...
	  (module () (SATISFACTION . SATISFACTION-CLAUSES) ...)
	  ;;NOTE Just in case putting  the satisfactions into a module's
	  ;;body turns  out not to  be right,  we can use  the following
	  ;;definition  instead.  What's  important  is  that the  whole
	  ;;sequence  of   forms  resulting  from  a   label  definition
	  ;;expansion is  a sequence of definitions.   (Marco Maggi; Tue
	  ;;Jul 16, 2013)
	  ;;
	  ;; (define dummy-for-satisfactions
	  ;;   (let ()
	  ;;     (SATISFACTION . SATISFACTION-CLAUSES) ...
	  ;;     #f))

	  ))))


(define-syntax (define-class stx)
  ;;Define a new class type.  After  all the processing: expand to a set
  ;;of syntax definitions and  miscellaneous bindings for predicates and
  ;;constructors.
  ;;
  ;;In this  macro transformer we  only process the MIXINS  clauses, the
  ;;actual class definition is performed by another macro.
  ;;
  (syntax-case stx ()
    ((_ ?name ?clause ...)
     (receive (mixins-clauses other-clauses)
	 (help.filter-and-validate-mixins-clauses #'(?clause ...) synner)
       (if (null? mixins-clauses)
	   #`(define-class/after-mixins-processing ?name . #,other-clauses)
	 (with-syntax ((((MIXIN (FROM TO) ...) MIXIN-SPEC ...) mixins-clauses))
	   #`(MIXIN :insert-mixin-clauses
		    (define-class/after-mixins-processing ?name . #,other-clauses)
		    ((FROM TO) ...)
		    (MIXIN-SPEC ...))))))
    (_
     (synner "invalid syntax in class definition"))))

(define-syntax (define-class/after-mixins-processing stx)
  (define spec		(help.parse-class-definition stx #'<top> #'lambda/tags synner))
  (define tag-id	(help.<parsed-spec>-name-id spec))
  (define abstract?	(help.<parsed-spec>-abstract? spec))
  (with-syntax
      ((THE-TAG					tag-id)
       (THE-RECORD-TYPE				(help.<class-spec>-record-type-id spec))
       (THE-PREDICATE				(help.<parsed-spec>-public-predicate-id spec))
       (THE-PARENT				(help.<parsed-spec>-parent-id spec))
       (THE-DEFAULT-PROTOCOL			(help.tag-id->default-protocol-id tag-id))
       (THE-FROM-FIELDS-CONSTRUCTOR		(help.tag-id->from-fields-constructor-id tag-id))
       (THE-PUBLIC-CONSTRUCTOR			(help.<parsed-spec>-public-constructor-id spec))

       (THE-LIST-OF-UIDS			(help.tag-id->list-of-uids-id tag-id))
       (NONGENERATIVE-UID			(help.<parsed-spec>-nongenerative-uid spec))

       (SEALED?
	(help.<parsed-spec>-sealed? spec))

       (OPAQUE?
	(help.<parsed-spec>-opaque? spec))

       (((CONCRETE-FIELD-SPEC ...) ...)
	(help.<parsed-spec>-concrete-fields-data spec))

       ((CONCRETE-FIELD-NAME ...)
	(help.<parsed-spec>-concrete-fields-names spec))

       (((IMMUTABLE-FIELD IMMUTABLE-ACCESSOR IMMUTABLE-TAG) ...)
	(help.<parsed-spec>-immutable-fields-data spec))

       (((MUTABLE-FIELD MUTABLE-ACCESSOR MUTABLE-MUTATOR MUTABLE-TAG) ...)
	(help.<parsed-spec>-mutable-fields-data spec))

       (((METHOD-NAME METHOD-RV-TAG METHOD-IMPLEMENTATION) ...)
	(help.<parsed-spec>-methods-table spec))

       ((DEFINITION ...)
	(help.<parsed-spec>-definitions spec))

       (ACCESSOR-TRANSFORMER
	(help.<parsed-spec>-accessor-transformer spec))

       (MUTATOR-TRANSFORMER
	(help.<parsed-spec>-mutator-transformer  spec))

       (MAKER-TRANSFORMER
	(help.<parsed-spec>-maker-transformer spec))

       (FINALISER-EXPRESSION
	(help.<parsed-spec>-finaliser-expression spec))

       ((SATISFACTION ...)
	(help.<parsed-spec>-satisfactions spec))
       (SATISFACTION-CLAUSES
	(help.<class-spec>-satisfaction-clauses spec))

       (WRONG-TYPE-ERROR-MESSAGE
	(string-append "invalid expression result, expected value of type "
		       (symbol->string (syntax->datum tag-id)))))
    (define (%compose-parent-rcd-with-proto proto)
      #`(make-record-constructor-descriptor (record-type-descriptor THE-RECORD-TYPE)
					    (THE-PARENT :super-constructor-descriptor)
					    #,proto))
    (with-syntax
	((THE-COMMON-CONSTRUCTOR-EXPR
	  ;;The common  protocol is the default  one to be used  when no
	  ;;specialised  protocols  are   defined  by  the  DEFINE-CLASS
	  ;;clauses.  Abstract classes cannot have a common protocol.
	  ;;
	  ;;If the class is concrete  and no protocol is defined, we use
	  ;;the appropriately  built default protocol;  R6RS states that
	  ;;when the parent's RCD has a custom protocol, the derived RCD
	  ;;must have a custom protocol too.
	  (%compose-parent-rcd-with-proto (cond (abstract?
						 #'(lambda (make-superclass)
						     (lambda args
						       (assertion-violation 'THE-TAG
							 "attempt to instantiate abstract class"))))
						((help.<parsed-spec>-common-protocol spec))
						(else
						 #'THE-DEFAULT-PROTOCOL))))

	 (THE-PUBLIC-CONSTRUCTOR-EXPR
	  ;;The public protocol is to be used by the maker of this class
	  ;;type; when not defined,  it defaults to the common protocol;
	  ;;abstract classes cannot have a public protocol.
	  ;;
	  ;;If the class is concrete  and no protocol is defined, we use
	  ;;the appropriately  built default protocol;  R6RS states that
	  ;;when the parent's RCD has a custom protocol, the derived RCD
	  ;;must have a custom protocol too.
	  (let ((proto (help.<parsed-spec>-public-protocol spec)))
	    (if (or abstract? (not proto))
		#'the-common-constructor-descriptor
	      (%compose-parent-rcd-with-proto proto))))

	 (THE-SUPER-CONSTRUCTOR-EXPR
	  ;;The  super  protocol is  to  be  used  when instantiating  a
	  ;;subclass of this class type; when not defined, and the class
	  ;;is concrete,  it defaults to the  common protocol.  Abstract
	  ;;classes can have a super protocol.
	  ;;
	  ;;If the class is concrete  and no protocol is defined, we use
	  ;;the appropriately  built default protocol;  R6RS states that
	  ;;when the parent's RCD has a custom protocol, the derived RCD
	  ;;must have a custom protocol too.
	  (let ((proto (help.<parsed-spec>-super-protocol spec)))
	    (cond (abstract?
		   (%compose-parent-rcd-with-proto (or proto #'THE-DEFAULT-PROTOCOL)))
		  (proto
		   (%compose-parent-rcd-with-proto proto))
		  (else
		   #'the-common-constructor-descriptor))))

	 (GETTER-TRANSFORMER
	  (or (help.<parsed-spec>-getter spec)
	      #`(lambda (stx)
	      	  (syntax-case stx ()
	      	    (?form
	      	     #'(THE-PARENT :getter #f ?form))))))

	 (SETTER-TRANSFORMER
	  (or (help.<parsed-spec>-setter spec)
	      #`(lambda (stx)
		  (syntax-case stx ()
		    (?form
		     #'(THE-PARENT :setter #f ?form))))))
	 )
      #'(begin

	  (THE-PARENT :insert-parent-clause
	    (define-record-type (THE-RECORD-TYPE the-automatic-constructor THE-PREDICATE)
	      (nongenerative NONGENERATIVE-UID)
	      (sealed SEALED?)
	      (opaque OPAQUE?)
	      (fields (CONCRETE-FIELD-SPEC ...)
		      ...)))

	  (define (THE-FROM-FIELDS-CONSTRUCTOR . args)
	    (apply the-automatic-constructor
		   (THE-TAG :list-of-unique-ids) ;this is the value of the "<top>" field
		   args))

	  (define THE-DEFAULT-PROTOCOL
	    (THE-PARENT :insert-constructor-fields
	      (lambda (make-parent)
		(lambda (CONCRETE-FIELD-NAME ...)
		  ((make-parent) CONCRETE-FIELD-NAME ...)))))

	  (module ()
	    (cond (FINALISER-EXPRESSION
		   => (lambda (finaliser)
			(record-destructor-set! (record-type-descriptor THE-RECORD-TYPE)
						finaliser)))))

	  (define THE-LIST-OF-UIDS
	    (THE-PARENT :append-unique-id (NONGENERATIVE-UID)))

	  ;;*NOTE*  (Marco  Maggi;  Thu  May  17, 2012)  Once  a  record
	  ;;constructor  descriptor  (RCD)  has been  built:  everything
	  ;;needed  to build  a  record constructor  function is  known;
	  ;;applying RECORD-CONSTRUCTOR  to the RCD can  return the same
	  ;;constructor function, memoised in  an internal field.  It is
	  ;;not   clear   if,  according   to   R6RS,   every  call   to
	  ;;RECORD-CONSTRUCTOR causes  all the protocol  functions to be
	  ;;called a new time.
	  ;;
	  ;;This   is   the   business    of   the   underlying   Scheme
	  ;;implementation,    so,   here,    we   shamelessly    invoke
	  ;;RECORD-CONSTRUCTOR multiple times even  though we known that
	  ;;all the RCDs may be the same (the common RCD).

	  (define the-common-constructor-descriptor THE-COMMON-CONSTRUCTOR-EXPR)
	  ;;This is commented out because unused at present.  The common
	  ;;constructor  descriptor  is   used  when  other  constructor
	  ;;descriptors are  not customised, but the  common constructor
	  ;;function does not need to be defined.
	  ;;
	  ;; (define THE-COMMON-CONSTRUCTOR
	  ;;   (record-constructor the-common-constructor-descriptor))

	  (define the-public-constructor-descriptor THE-PUBLIC-CONSTRUCTOR-EXPR)
	  (define THE-PUBLIC-CONSTRUCTOR
	    (let ((constructor (record-constructor the-public-constructor-descriptor)))
	      (lambda args
		(receive-and-return (instance)
		    (apply constructor args)
		  (<top>-unique-identifiers-set! instance (THE-TAG :list-of-unique-ids))))))

	  (define the-super-constructor-descriptor THE-SUPER-CONSTRUCTOR-EXPR)
	  ;;This is commented out because  unused at present.  The super
	  ;;constructor descriptor  is used by subclass  constructors to
	  ;;build instances  of type THE-TAG, but  the super constructor
	  ;;function does not need to be defined.
	  ;;
	  ;; (define the-super-constructor
	  ;;   (record-constructor the-super-constructor-descriptor))

	  (define-syntax THE-TAG
	    ;;Tag  syntax,  all the  operations  involving  this tag  go
	    ;;through  this   syntax.   The  only   reason  this  syntax
	    ;;dispatches to sub-syntaxes it to keep the code readable.
	    ;;
	    ;;??EXPR must  be an expression  to be evaluated  only once;
	    ;;its result  must be an  instance of the tag  type.  ??EXPR
	    ;;can  be  an identifier  but  also  the  application of  an
	    ;;accessor to an instance.
	    ;;
	    ;;??VAR must be the  identifier bound to the instance syntax
	    ;;dispatcher.
	    ;;
	    (let ((%the-getter		GETTER-TRANSFORMER)
		  (%the-setter		SETTER-TRANSFORMER)
		  (%the-accessor	ACCESSOR-TRANSFORMER)
		  (%the-mutator		MUTATOR-TRANSFORMER)
		  (%the-maker		MAKER-TRANSFORMER))

	      (lambda (stx)
		(define (synner message subform)
		  (syntax-violation 'THE-TAG message stx subform))

		(syntax-case stx ( ;;
				  :define :let :is-a? :make :make-from-fields
				  :dispatch :accessor :mutator :getter :setter
				  :insert-parent-clause define-record-type
				  :insert-constructor-fields
				  :super-constructor-descriptor lambda
				  :assert-type-and-return
				  :assert-procedure-argument :assert-expression-return-value
				  :append-unique-id :list-of-unique-ids
				  :predicate-function :accessor-function :mutator-function
				  aux.<>)

		  ;; private API

		  ;;Given  an  R6RS record  type  definition: insert  an
		  ;;appropriate  PARENT  clause  so  that  the  type  is
		  ;;derived from this tag's record type.  This is needed
		  ;;because  only  the tag  identifier  is  part of  the
		  ;;public interface, but we still need to define record
		  ;;types derived from this one.
		  ((_ :insert-parent-clause (define-record-type ??name . ??clauses))
		   #'(define-record-type ??name (parent THE-RECORD-TYPE) . ??clauses))

		  ;;Insert the  field names in  the appropriate position
		  ;;in  the definition  of a  custom  protocol function.
		  ;;This  is  used  by  subclasses to  build  their  own
		  ;;default  protocol which  does  not need  to set  the
		  ;;fields in "<top>".
		  ((_ :insert-constructor-fields
		      (lambda (make-parent1)
			(lambda (V (... ...))
			  ((make-parent2 W (... ...)) Z (... ...)))))
		   #'(THE-PARENT :insert-constructor-fields
		       (lambda (make-parent1)
			 (lambda (CONCRETE-FIELD-NAME ... V (... ...))
			   ((make-parent2 CONCRETE-FIELD-NAME ... W (... ...)) Z (... ...))))))

		  ((_ :super-constructor-descriptor)
		   #'the-super-constructor-descriptor)

		  ((_ :define ??var)
		   (identifier? #'??var)
		   #'(begin
		       (define src-var)
		       (define-syntax ??var
			 (help.make-tagged-variable-transformer #'THE-TAG #'src-var))))

		  ((_ :define ??var ??expr)
		   (identifier? #'??var)
		   #'(begin
		       (define src-var (THE-TAG :assert-type-and-return ??expr))
		       (define-syntax ??var
			 (help.make-tagged-variable-transformer #'THE-TAG #'src-var))))

		  ((_ :let ??expr ??var ??body0 ??body (... ...))
		   #'(let ((src-var ??expr))
		       (let-syntax ((??var (help.make-tagged-variable-transformer #'THE-TAG #'src-var)))
			 ??body0 ??body (... ...))))

		  ;;Try  to match  the tagged-variable  use to  a method
		  ;;call for  the tag; if  no method name  matches ??ID,
		  ;;try to match a field name.
		  ((_ :dispatch ??expr (??var ??id . ??args))
		   (and (identifier? #'??var)
			(identifier? #'??id))
		   (case-symbol (syntax->datum #'??id)
		     ((METHOD-NAME)
		      (help.process-method-application #'let/tags #'METHOD-RV-TAG
						       #'(METHOD-IMPLEMENTATION ??expr . ??args)))
		     ...
		     (else
		      (%the-accessor stx #'??expr #'??var (cons #'??id #'??args)))))

		  ((_ :accessor ??expr (??var . ??args))
		   (identifier? #'??var)
		   (%the-accessor stx #'??expr #'??var #'??args))

		  ((_ :mutator ??expr ??keys ??value)
		   (%the-mutator stx #'??expr #'??keys #'??value))

		  ((_ :getter ??expr (??var ((??key0 (... ...))
					     (??key (... ...))
					     (... ...))))
		   (identifier? #'??var)
		   (%the-getter #'(??var ((??key0 (... ...))
					  (??key  (... ...))
					  (... ...)))))

		  ((_ :setter ??expr (??var ((??key0 (... ...))
					     (??key (... ...))
					     (... ...))
					    ??value))
		   (identifier? #'??var)
		   (%the-setter #'(??var ((??key0 (... ...))
					  (??key (... ...))
					  (... ...))
					 ??value)))

		  ((_ :assert-type-and-return ??expr)
		   (if config.validate-tagged-values?
		       #'(receive-and-return (val)
			     ??expr
			   (unless (THE-TAG :is-a? val)
			     (tagged-binding-violation 'THE-TAG
			       WRONG-TYPE-ERROR-MESSAGE
			       '(expression: ??expr)
			       `(result: ,val))))
		     #'??expr))

		  ((_ :assert-procedure-argument ??id)
		   (identifier? #'??id)
		   ;;This DOES NOT return the value.
		   (if config.validate-tagged-values?
		       #'(unless (THE-TAG :is-a? ??id)
			   (procedure-argument-violation 'THE-TAG WRONG-TYPE-ERROR-MESSAGE ??id))
		     #'(void)))

		  ((_ :assert-expression-return-value ??expr)
		   ;;This DOES return the value.
		   (if config.validate-tagged-values?
		       #'(receive-and-return (val)
			     ??expr
			   (unless (THE-TAG :is-a? val)
			     (expression-return-value-violation 'THE-TAG WRONG-TYPE-ERROR-MESSAGE val)))
		     #'??expr))

		  ;; public API: auxiliary syntaxes

		  ((_ :make . ??args)
		   #'(THE-PUBLIC-CONSTRUCTOR . ??args))

		  ((_ :make-from-fields . ??args)
		   #'(THE-FROM-FIELDS-CONSTRUCTOR . ??args))

		  ((_ :is-a? . ??args)
		   #'(THE-PREDICATE . ??args))

		  ((_ :append-unique-id (??id (... ...)))
		   #'(THE-PARENT :append-unique-id (??id (... ...) NONGENERATIVE-UID)))

		  ((_ :list-of-unique-ids)
		   #'THE-LIST-OF-UIDS)

		  ((_ :predicate-function)
		   #'THE-PREDICATE)

		  ((_ :accessor-function ??field-name)
		   (identifier? #'??field-name)
		   (case-symbol (syntax->datum #'??field-name)
		     ((IMMUTABLE-FIELD)	#'(lambda (obj) (IMMUTABLE-ACCESSOR obj))) ...
		     ((MUTABLE-FIELD)	#'(lambda (obj) (MUTABLE-ACCESSOR   obj))) ...
		     (else
		      #'(THE-PARENT :accessor-function ??field-name))))

		  ((_ :mutator-function ??field-name)
		   (identifier? #'??field-name)
		   (case-symbol (syntax->datum #'??field-name)
		     ((MUTABLE-FIELD)
		      #'(lambda (obj val) (MUTABLE-MUTATOR obj val)))
		     ...
		     ((IMMUTABLE-FIELD)
		      (synner "request of mutator function for immutable field" #'IMMUTABLE-FIELD))
		     ...
		     (else
		      #'(THE-PARENT :mutator-function ??field-name))))

		  ;; public API: binding definition

		  ;;Define  an  internal  variable  with  initialisation
		  ;;expression using the tag constructor.
		  ((??tag ??var (aux.<> (??arg (... ...))))
		   (identifier? #'??var)
		   #'(??tag ??var (??tag (??arg (... ...)))))

		  ;;Internal definition with initialisation expression.
		  ((_ ??var ??expr)
		   (identifier? #'??var)
		   #'(THE-TAG :define ??var ??expr))

		  ;;Internal definition without initialisation expression.
		  ((_ ??var)
		   (identifier? #'??var)
		   #'(THE-TAG :define ??var))

		  ;;Constructor call.
		  ((_ (??arg (... ...)))
		   (if %the-maker
		       (%the-maker stx)
		     #'(THE-PUBLIC-CONSTRUCTOR ??arg (... ...))))

		  ;;Predicate reference.  It is meant to be used as:
		  ;;
		  ;;  ((<vector>) '#()) => #t
		  ;;  ((<vector>) 1234) => #f
		  ;;
		  ((_)
		   #'THE-PREDICATE)

		  (_
		   (synner "invalid tag syntax" #f))))))

	  DEFINITION ...

	  (module () (SATISFACTION . SATISFACTION-CLAUSES) ...)
	  ;;NOTE Just in case putting  the satisfactions into a module's
	  ;;body turns  out not to  be right,  we can use  the following
	  ;;definition  instead.  What's  important  is  that the  whole
	  ;;sequence  of   forms  resulting  from  a   class  definition
	  ;;expansion is  a sequence of definitions.   (Marco Maggi; Tue
	  ;;Jul 16, 2013)
	  ;;
	  ;; (define dummy-for-satisfactions
	  ;;   (let ()
	  ;;     (SATISFACTION . SATISFACTION-CLAUSES) ...
	  ;;     #f))

	  ))))


;;;; mixins
;;
;;Mixins are collections of clauses that  can be added to a label, class
;;or other mixin definition.  For example:
;;
;;  (define-mixin <alpha>
;;    (fields d e f))
;;
;;  (define-class <beta>
;;    (fields a b c)
;;    (mixins <alpha>))
;;
;;is equivalent to:
;;
;;  (define-class <beta>
;;    (fields a b c)
;;    (fields d e f))
;;
;;and so equivalent to:
;;
;;  (define-class <beta>
;;    (fields a b c d e f))
;;
;;mixin clauses are added to the end of the enclosing entity definition.
;;
;;Continuing  with  the  example  above  (and  simplifying  by  removing
;;processing of the substitution maps) we can imagine that the mixin and
;;class definitions are first expanded to:
;;
;;   (define-syntax <alpha>
;;     (syntax-rules (:insert-mixin-clauses)
;;       ((_ :insert-mixin-clauses
;;           (?definer ?name ?clause ...)
;;           ())
;;        (?definer ?name ?clause ... (fields d e f)))
;;       ((_ :insert-mixin-clauses
;;           (?definer ?name ?clause ...)
;;           (?mixin0 ?mixin ...))
;;        (?mixin0 :insert-mixin-clauses
;;           (?definer ?name ?clause ... (fields d e f))
;;           (?mixin ...)))
;;       ))
;;
;;  (<alpha> :insert-mixin-clauses
;;    (define-class <beta> (fields a b c))
;;    ())
;;
;;from this we can understand how multiple mixin insertion is performed.
;;

(define-syntax (define-mixin stx)
  (syntax-case stx ()
    ((_ ?name ?clause ...)
     (receive (mixins-clauses other-clauses)
	 (help.filter-and-validate-mixins-clauses #'(?clause ...) synner)
       (if (null? mixins-clauses)
	   #`(define-mixin/after-mixins-processing ?name . #,other-clauses)
	 (with-syntax ((((MIXIN (FROM TO) ...) MIXIN-SPEC ...) mixins-clauses))
	   #`(MIXIN :insert-mixin-clauses
		    (define-mixin/after-mixins-processing ?name . #,other-clauses)
		    ((FROM TO) ...)
		    (MIXIN-SPEC ...))))))
    (_
     (synner "invalid syntax in mixin definition"))))

(define-syntax (define-mixin/after-mixins-processing stx)
  (define spec (help.parse-mixin-definition stx #'<top> #'lambda/tags synner))
  (with-syntax
      ((MIXIN-ID	(help.<parsed-spec>-name-id spec))
       (CLAUSES		(help.<mixin-spec>-clauses spec)))
    #'(define-syntax (MIXIN-ID stx)
	(define (synner message subform)
	  (syntax-violation 'MIXIN-ID message stx subform))
	(syntax-case stx (:insert-mixin-clauses)
	  ((_ :insert-mixin-clauses
	      (??definer ??name ??clause (... ...))
	      ((??from ??to) (... ...))
	      ())
	   (receive (entity-id unused-constructor unused-predicate)
	       (help.parse-tag-name-spec #'??name synner)
	     (with-syntax
		 ((SPECIALISED-CLAUSES (help.multi-identifier-subst #`((MIXIN-ID #,entity-id)
								       (??from ??to)
								       (... ...))
								    #'CLAUSES)))
	       #'(??definer ??name ??clause (... ...) . SPECIALISED-CLAUSES))))
	  ((_ :insert-mixin-clauses
	      (??definer ??name ??clause (... ...))
	      ((??from ??to) (... ...))
	      ((??next-mixin (??next-from ??next-to) (... ...))
	       ??mixin-spec
	       (... ...)))
	   (receive (entity-id unused-constructor unused-predicate)
	       (help.parse-tag-name-spec #'??name synner)
	     (with-syntax
		 ((SPECIALISED-CLAUSES (help.multi-identifier-subst #`((MIXIN-ID #,entity-id)
								       (??from ??to)
								       (... ...))
								    #'CLAUSES)))
	       #'(??next-mixin :insert-mixin-clauses
			       (??definer ??name ??clause (... ...) . SPECIALISED-CLAUSES)
			       ((??next-from ??next-to) (... ...))
			       (??mixin-spec (... ...))))))
	  (_
	   (synner "invalid syntax in mixin use" #f))
	  ))
    ))


;;;; companion syntaxes

(define-syntax (make-from-fields stx)
  (syntax-case stx ()
    ((_ ?tag . ?args)
     (identifier? #'?tag)
     #'(?tag :make-from-fields . ?args))
    (_
     (synner "invalid syntax in use of from-fields maker call"))))

;; (define-syntax (make stx)
;;   (syntax-case stx ()
;;     ((_ ?tag . ?args)
;;      (identifier? #'?tag)
;;      #'(?tag :make . ?args))
;;     (_
;;      (synner "invalid syntax in use of public maker call"))))

(define-syntax (is-a? stx)
  ;;Test if  a given object  matches a class  type using the  :is-a? tag
  ;;syntax.
  ;;
  (syntax-case stx (<top> aux.<>)
    ;;All the objects are "<top>" instances by definition.
    ((_ ?obj <top>)
     (syntax #t))

    ;;Expand to the predicate to be used as standalone function.
    ((_ aux.<> ?class-name)
     #'(?class-name :predicate-function))

    ;;Apply the predicate function to the object.
    ((_ ?obj ?tag)
     (identifier? #'?tag)
     #'(?tag :is-a? ?obj))

    (_
     (synner "invalid syntax in use of tag type predicate"))))

(define-syntax (slot-ref stx)
  (syntax-case stx (aux.<>)
    ((_ ?object-expr ?slot-name ?class)
     (not (identifier? #'?slot-name))
     (synner "expected identifier as slot name" #'?slot-name))

    ((_ ?object-expr ?slot-name ?class)
     (not (identifier? #'?class))
     (synner "expected identifier as class name" #'?class))

    ((_ aux.<> ?slot-name ?class)
     #'(?class :accessor-function ?slot-name))

    ((_ ?object-expr ?slot-name ?class)
     #'((?class :accessor-function ?slot-name) ?object-expr))

    (_
     (synner "invalid syntax in slot-ref form"))))

(define-syntax (slot-set! stx)
  (syntax-case stx (aux.<>)
    ((_ ?object-expr ?slot-name ?class ?value-expr)
     (not (identifier? #'?slot-name))
     (synner "expected identifier as slot name" #'?slot-name))

    ((_ ?object-expr ?slot-name ?class ?value-expr)
     (not (identifier? #'?class))
     (synner "expected identifier as class name" #'?class))

    ((_ aux.<> ?slot-name ?class aux.<>)
     #'(?class :mutator-function ?slot-name))

    ((_ ?object-expr ?slot-name ?class ?value-expr)
     #'((?class :mutator-function ?slot-name) ?object-expr ?value-expr))

    (_
     (synner "invalid syntax in slot-set! form"))))

(define-syntax (tag-unique-identifiers stx)
  (syntax-case stx ()
    ((_ ?tag)
     (identifier? #'?tag)
     #'(?tag :list-of-unique-ids))
    (_
     (synner "invalid syntax in use of tag list of UIDs"))))

(define-syntax (with-label-shadowing stx)
  (syntax-case stx (:process-shadowed-identifier)
    ((_ () ?body0 ?body ...)
     #'(begin ?body0 ?body ...))
    ((_ (?label0 ?label ...) ?body0 ?body ...)
     (all-identifiers? #'(?label0 ?label ...))
     #'(?label0 :process-shadowed-identifier
		(with-label-shadowing (?label ...)
		  ?body0 ?body ...)))
    (_
     (synner "invalid syntax in request for labels shadowing"))))


(define-syntax (set!/tags stx)
  (syntax-case stx ()

    ;;Main syntax to invoke the setter  for the tag of ?VAR; it supports
    ;;multiple sets of keys for nested setter invocations.
    ((_ (?var (?key0 ...) (?key ...) ...) ?value)
     (identifier? #'?var)
     #'(?var :setter ((?key0 ...) (?key ...) ...) ?value))

    ;;Alternative syntax  to invoke the setter  for the tag  of ?VAR; it
    ;;supports multiple sets of keys for nested setter invocations.
    ((_ ?var (?key0 ...) (?key ...) ... ?value)
     (identifier? #'?var)
     #'(?var :setter ((?key0 ...) (?key ...) ...) ?value))

    ;;Syntax to  invoke the field mutator  for the tag  of ?VAR.  Notice
    ;;that this may also be a nested setter invocation, as in:
    ;;
    ;;   (set!/tags (O a b c[777]) 999)
    ;;
    ;;for this reason we do not validate ?ARG in any way.
    ;;
    ((_ (?var ?field-name ?arg ...) ?val)
     (and (identifier? #'?var)
	  (identifier? #'?field-name))
     #'(?var :mutator (?field-name ?arg ...) ?val))

    ;;Syntax to mutate a binding with R6RS's SET!.
    ((_ ?var ?val)
     (identifier? #'?var)
     #'(set! ?var ?val))
    ))


(define-syntax (with-tagged-arguments-validation stx)
  ;;Transform:
  ;;
  ;;  (with-tagged-arguments-validation (who)
  ;;       ((<fixnum>  X)
  ;;        (<integer> Y))
  ;;    (do-this)
  ;;    (do-that))
  ;;
  ;;into:
  ;;
  ;;  (if ((<fixnum>) X)
  ;;      (if ((<integer>) Y)
  ;;          (begin
  ;;            (do-this)
  ;;            (do-that))
  ;;        (procedure-argument-violation who "invalid tagged argument" '<integer> Y))
  ;;    (procedure-argument-violation who "invalid tagged argument" '<fixnum> X))
  ;;
  ;;As a special case:
  ;;
  ;;  (with-tagged-arguments-validation (who)
  ;;       ((<top>  X))
  ;;    (do-this)
  ;;    (do-that))
  ;;
  ;;expands to:
  ;;
  ;;  (begin
  ;;    (do-this)
  ;;    (do-that))
  ;;
  (define (main stx)
    (syntax-case stx ()
      ((_ (?who) ((?validator ?arg) ...) . ?body)
       (identifier? #'?who)
       (let* ((body		#'(begin . ?body))
	      (output-form	(%build-output-form #'?who
						    #'(?validator ...)
						    (syntax->list #'(?arg ...))
						    body)))
	 (if config.validate-tagged-values?
	     output-form
	   body)))
      (_
       (%synner "invalid input form" #f))))

  (define (%build-output-form who validators args body)
    (syntax-case validators (<top>)
      (()
       #`(let () #,body))

      ;;Accept "<top>" as special validator meaning "always valid".
      ((<top> . ?other-validators)
       (%build-output-form who #'?other-validators (cdr args) body))

      ((?validator . ?other-validators)
       (identifier? #'?validator)
       (%generate-validation-form who #'?validator (car args) #'?other-validators (cdr args) body))

      ((?validator . ?others)
       (%synner "invalid argument-validator selector" #'?validator))))

  (define (%generate-validation-form who validator-id arg-id other-validators-stx args body)
    #`(begin
	(#,validator-id :assert-procedure-argument #,arg-id)
	#,(%build-output-form who other-validators-stx args body)))

  (define (%synner msg subform)
    (syntax-violation 'with-tagged-arguments-validation msg stx subform))

  (main stx))


;;;; tagged return value

(define-syntax (begin/tags stx)
  (syntax-case stx (aux.<-)
    ((_ (aux.<- ?tag) ?body0 ?body ...)
     (identifier? #'?tag)
     (if config.validate-tagged-values?
	 #'(let ((retval (begin ?body0 ?body ...)))
	     (?tag :assert-expression-return-value retval))
       #'(begin ?body0 ?body ...)))

    ((_ (aux.<- ?tag0 ?tag ...) ?body0 ?body ...)
     (all-identifiers? #'(?tag0 ?tag ...))
     (if config.validate-tagged-values?
	 (with-syntax
	     (((RETVAL ...) (generate-temporaries #'(?tag ...))))
	   #'(receive (retval0 RETVAL ...)
		 (begin ?body0 ?body ...)
	       (values (?tag0 :assert-expression-return-value retval0)
		       (?tag  :assert-expression-return-value RETVAL)
		       ...)))
       #'(begin ?body0 ?body ...)))

    ((_ (aux.<-) ?body0 ?body ...)
     #'(begin ?body0 ?body ...))

    ((_ ?body0 ?body ...)
     #'(begin ?body0 ?body ...))
    ))


;;;; convenience syntaxes with tags: LAMBDA

(define-syntax (lambda/tags stx)
  (syntax-case stx ()

    ;;Thunk definition.
    ;;
    ((_ () ?body0 ?body ...)
     #'(lambda () ?body0 ?body ...))

    ;;Function with tagged return values.
    ((_ ((?who ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...)
     (and (all-identifiers? #'(?who ?rv-tag0 ?rv-tag ...))
	  (identifier=symbol? #'?who '_))
     #'(lambda/tags ?formals (begin/tags (aux.<- ?rv-tag0 ?rv-tag ...) ?body0 ?body ...)))

    ;;Function with untagged args argument.
    ;;
    ((_ ?formals ?body0 ?body ...)
     (identifier? #'?formals)
     #'(lambda ?formals ?body0 ?body ...))

    ;;Function with tagged args argument.
    ;;
    ((_ #(?args-id ?tag-id) ?body0 ?body ...)
     (and (identifier? #'?args-id)
	  (identifier? #'?tag-id))
     (with-syntax ((((FORMAL) VALIDATIONS (SYNTAX-BINDING ...))
		    (help.parse-formals-bindings #'(#(?args-id ?tag-id)) #'<top> synner)))
       #`(lambda FORMAL
	   (define who 'lambda/tags)
	   (with-tagged-arguments-validation (who)
	       VALIDATIONS
	     (let-syntax (SYNTAX-BINDING ...)
	       ?body0 ?body ...)))))

    ;;Mandatory arguments and untagged rest argument.
    ;;
    ((_ (?var0 ?var ... . ?args) ?body0 ?body ...)
     (identifier? #'?args)
     (with-syntax (((FORMALS VALIDATIONS (SYNTAX-BINDING ...))
		    (help.parse-formals-bindings #'(?var0 ?var ... . ?args) #'<top> synner)))
       #`(lambda FORMALS
	   (define who 'lambda/tags)
	   (with-tagged-arguments-validation (who)
	       VALIDATIONS
	     (let-syntax (SYNTAX-BINDING ...)
	       ?body0 ?body ...)))))

    ;;Mandatory arguments and tagged rest argument.
    ;;
    ((_ (?var0 ?var ... . #(?args-id ?tag-id)) ?body0 ?body ...)
     (and (identifier? #'?args)
	  (identifier? #'?tag))
     (with-syntax (((FORMALS VALIDATIONS (SYNTAX-BINDING ...))
		    (help.parse-formals-bindings #'(?var0 ?var ... . #(?args-id ?tag-id)) #'<top> synner)))
       #`(lambda FORMALS
	   (define who 'lambda/tags)
	   (with-tagged-arguments-validation (who)
	       VALIDATIONS
	     (let-syntax (SYNTAX-BINDING ...)
	       ?body0 ?body ...)))))

    ;;Mandatory arguments and no rest argument.
    ;;
    ((_ (?var0 ?var ...) ?body0 ?body ...)
     (with-syntax (((FORMALS VALIDATIONS (SYNTAX-BINDING ...))
		    (help.parse-formals-bindings #'(?var0 ?var ...) #'<top> synner)))
       #`(lambda FORMALS
	   (define who 'lambda/tags)
	   (with-tagged-arguments-validation (who)
	       VALIDATIONS
	     (let-syntax (SYNTAX-BINDING ...)
	       ?body0 ?body ...)))))

    (_
     (synner "syntax error in LAMBDA/TAGS"))))

(define-syntax (case-lambda/tags stx)
  (syntax-case stx ()
    ((_)
     #'(case-lambda))

    ((_ ?clause ...)
     (let loop ((in-clauses #'(?clause ...))
		(ou-clauses '()))
       (syntax-case in-clauses ()
	 (()
	  (cons #'case-lambda (reverse ou-clauses)))

	 ;;Function with tagged return values.
	 (((((?who ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...) . ?other-clauses)
	  (and (all-identifiers? #'(?who ?rv-tag0 ?rv-tag ...))
	       (identifier=symbol? #'?who '_))
	  (loop #'((?formals
		    (begin/tags (aux.<- ?rv-tag0 ?rv-tag ...) ?body0 ?body ...))
		   . ?other-clauses)
		ou-clauses))

	 (((?formals ?body0 ?body ...) . ?other-clauses)
	  (begin
	    (with-syntax (((FORMALS VALIDATIONS (SYNTAX-BINDING ...))
			   (help.parse-formals-bindings #'?formals #'<top> synner)))
	      (loop #'?other-clauses
		    (cons #'(FORMALS
			     (define who 'case-lambda/tags)
			     (with-tagged-arguments-validation (who)
				 VALIDATIONS
			       (let-syntax (SYNTAX-BINDING ...)
				 ?body0 ?body ...)))
			  ou-clauses)))))

	 ((?clause . ?other-clauses)
	  (synner "invalid clause syntax" #'?clause)))))

    (_
     (synner "invalid syntax in case-lambda definition"))))


;;;; convenience syntaxes with tags: DEFINE and LAMBDA

(define-syntax (define/tags stx)
  (syntax-case stx ()

    ;;Untagged, uninitialised variable.
    ;;
    ((_ ?who)
     (identifier? #'?who)
     #'(define ?who))

    ;;Tagged, uninitialised variable.
    ;;
    ((_ #(?who ?tag))
     (and (identifier? #'?who)
	  (identifier? #'?tag))
     #'(?tag ?who))

    ;;Untagged, initialised variable.
    ;;
    ((_ ?who ?expr)
     (identifier? #'?who)
     #'(define ?who ?expr))

    ;;Tagged, initialised variable.
    ;;
    ((_ #(?who ?tag) ?expr)
     (and (identifier? #'?who)
	  (identifier? #'?tag))
     #'(?tag ?who ?expr))

    ;;Function definition with tagged return values through tagged who.
    ;;
    ((_ ((?who ?tag0 ?tag ...) . ?formals) ?body0 ?body ...)
     (all-identifiers? #'(?who ?tag0 ?tag ...))
     (with-syntax
	 ((WHO (datum->syntax #'?who '__who__)))
       #'(define ?who
	   (lambda/tags ((_ ?tag0 ?tag ...) . ?formals)
	     (let-constants ((WHO '?who))
	       ?body0 ?body ...)))))

    ;;Function definition.
    ;;
    ((_ (?who . ?formals) ?body0 ?body ...)
     (identifier? #'?who)
     (with-syntax
	 ((WHO (datum->syntax #'?who '__who__)))
       #'(define ?who
	   (lambda/tags ?formals
	     (let-constants ((WHO '?who))
	       ?body0 ?body ...)))))

    (_
     (synner "syntax error in DEFINE/TAGS"))))

(define-syntax (define-values/tags stx)
  (define who 'define-values/tags)

  (define (%main stx)
    (syntax-case stx ()
      ((_ (?var ... ?var0) ?form ... ?form0)
       (let ((vars-stx #'(?var ... ?var0)))
	 (with-syntax
	     (((VAR ... VAR0) (%process-vars vars-stx (lambda (id tag) (vector id tag))))
	      ((ID  ... ID0)  (%process-vars vars-stx (lambda (id tag) id)))
	      ((TMP ...)      (generate-temporaries #'(?var ...))))
	   #'(begin
	       ;;We must make sure that the ?FORMs do not capture the ?VARs.
	       (define (return-multiple-values)
		 ?form ... ?form0)
	       (define/tags VAR)
	       ...
	       (define/tags VAR0
		 (call-with-values
		     return-multiple-values
		   (lambda (TMP ... TMP0)
		     (set! ID TMP)
		     ...
		     TMP0)))))))))

  (define (%process-vars vars-stx maker)
    (syntax-case vars-stx ()
      (() '())
      ((?id ?var ...)
       (identifier? #'?id)
       (cons (maker #'?id #'<top>) (%process-vars #'(?var ...) maker)))
      (((?id ?tag) ?var ...)
       (and (identifier? #'?id)
	    (identifier? #'?tag))
       (cons (maker #'?id #'?tag) (%process-vars #'(?var ...) maker)))
      ((?var0 ?var ...)
       (syntax-violation who "invalid binding definition syntax" stx #'?var0))))

  (%main stx))


;;;; convenience syntaxes with tags: LET and similar

(define-syntax (with-tags stx)
  (syntax-case stx ()
    ((_ (?var ...) ?body0 ?body ...)
     (with-syntax
	 ((((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
	   (help.parse-with-tags-bindings #'(?var ...) synner)))
       #`(let-syntax (SYNTAX-BINDING ...) ?body0 ?body ...)))
    (_
     (synner "syntax error"))))

(define-syntax (let/tags stx)
  (syntax-case stx ()
    ;; no bindings
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ;; common LET with possibly tagged bindings
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (with-syntax
	 ((((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
	   (help.parse-let-bindings #'(?var ...) #'<top> synner)))
       #`(let ((VAR (TAG :assert-type-and-return ?init)) ...)
	   (let-syntax (SYNTAX-BINDING ...) ?body0 ?body ...))))

    ;; named let, no bindings
    ((_ ?name () ?body0 ?body ...)
     (identifier? #'?name)
     #'(let ?name () ?body0 ?body ...))

    ;; named let, with possibly tagged bindings
    ((_ ?name ((?var ?init) ...) ?body0 ?body ...)
     (identifier? #'?name)
     (with-syntax
	 ((((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
	   (help.parse-let-bindings #'(?var ...) #'<top> synner)))
       #`(let ?name ((VAR (TAG :assert-type-and-return ?init)) ...)
	   (let-syntax (SYNTAX-BINDING ...) ?body0 ?body ...))))

    (_
     (synner "syntax error"))))

(define-syntax (let*/tags stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?var0 ?init0) (?var ?init) ...) ?body0 ?body ...)
     #`(let/tags ((?var0 ?init0))
	 (let*/tags ((?var ?init) ...)
	   ?body0 ?body ...)))

    (_
     (syntax-violation 'let*/tags "syntax error in let*/tags input form" stx #f))))

(define-syntax (letrec/tags stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (with-syntax
	 (((TMP ...)
	   (generate-temporaries #'(?var ...)))
	  (((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
	   (help.parse-let-bindings #'(?var ...) #'<top> synner)))
       #`(let ((VAR #f) ...)
	   (let-syntax (SYNTAX-BINDING ...)
	     ;;Do not enforce the order of evaluation of ?INIT.
	     (let ((TMP (TAG :assert-type-and-return ?init)) ...)
	       (set! VAR TMP) ...
	       (let () ?body0 ?body ...))))))

    (_
     (syntax-violation 'letrec/tags "syntax error in letrec/tags input form" stx #f))))

(define-syntax (letrec*/tags stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (with-syntax
	 ((((VAR ...) (TAG ...) (SYNTAX-BINDING ...))
	   (help.parse-let-bindings #'(?var ...) #'<top> synner)))
       #`(let ((VAR #f) ...)
	   (let-syntax (SYNTAX-BINDING ...)
	     ;;do enforce the order of evaluation of ?INIT
	     (set! VAR (TAG :assert-type-and-return ?init))
	     ...
	     (let () ?body0 ?body ...)))))

    (_
     (syntax-violation 'letrec*/tags "syntax error in letrec*/tags input form" stx #f))))


;;;; convenience syntaxes with tags: LET-VALUES and similar

(define-syntax receive/tags
  (syntax-rules ()
    ((_ ?formals ?expression ?body0 ?body ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda/tags ?formals ?body0 ?body ...)))))

(define-syntax (receive-and-return/tags stx)
  (syntax-case stx ()
    ((_ ?vars ?inits ?body0 ?body ...)
     (with-syntax
	 ((((VARS) (BINDING ...))
	   (help.parse-let-values-bindings #'(?vars) #'<top> synner)))
       (if (identifier? #'VARS)
	   #`(let-values ((VARS ?inits))
	       (let-syntax (BINDING ...) ?body0 ?body ... (apply values VARS)))
	 #`(let-values ((VARS ?inits))
	     (let-syntax (BINDING ...) ?body0 ?body ... (values . VARS))))))
    ))

(define-syntax (let-values/tags stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?vars ?inits) ...) ?body0 ?body ...)
     (with-syntax
	 ((((VARS ...) (BINDING ...))
	   (help.parse-let-values-bindings #'(?vars ...) #'<top> synner)))
       #`(let-values ((VARS ?inits) ...)
	   (let-syntax (BINDING ...) ?body0 ?body ...))))

    ((_ ?bindings ?body0 ?body ...)
     (synner "syntax error in bindings" #'?bindings))
    (_
     (synner "syntax error"))))

(define-syntax (let*-values/tags stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?vars0 ?inits0) (?vars ?inits) ...) ?body0 ?body ...)
     #`(let-values/tags ((?vars0 ?inits0))
	 (let*-values/tags ((?vars ?inits) ...)
	   ?body0 ?body ...)))

    ((_ ?bindings ?body0 ?body ...)
     (synner "syntax error in bindings" #'?bindings))
    (_
     (synner "syntax error"))))


;;;; convenience syntaxes with tags: DO and similar

(define-syntax do/tags
  (syntax-rules ()
    ((_ ((?var ?init ?step ...) ...)
	(?test ?expr ...)
	?form ...)
     (let-syntax ((the-expr (syntax-rules ()
			      ((_)
			       (values))
			      ((_ ??expr0 ??expr (... ...))
			       (begin ??expr0 ??expr (... ...)))))
		  (the-step (syntax-rules ()
			      ((_ ??var)
			       ??var)
			      ((_ ??var ??step)
			       ??step)
			      ((_ ??var ??step0 ??step (... ...))
			       (syntax-violation 'do/tags
				 "invalid step specification"
				 '(??step0 ??step (... ...)))))))
       (let/tags loop ((?var ?init) ...)
		 (if ?test
		     (the-expr ?expr ...)
		   (begin
		     ?form ...
		     (loop (the-step ?var ?step ...) ...))))))))

(define-syntax (do*/tags stx)
  (define (%parse-var stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'?id)
      ((?id ?tag ...)
       (all-identifiers? #'(?id ?tag ...))
       #'?id)
      (_
       (synner "invalid binding declaration"))))
  (syntax-case stx ()
    ((_ ((?var ?init ?step ...) ...)
	(?test ?expr ...)
	?form ...)
     (with-syntax (((ID ...) (map %parse-var (syntax->list #'(?var ...)))))
       #'(let-syntax ((the-expr (syntax-rules ()
				  ((_)
				   (values))
				  ((_ ??expr0 ??expr (... ...))
				   (begin ??expr0 ??expr (... ...)))))
		      (the-step (syntax-rules ()
				  ((_ ??var)
				   ??var)
				  ((_ ??var ??step)
				   ??step)
				  ((_ ??var ??step0 ??step (... ...))
				   (syntax-violation 'do/tags
				     "invalid step specification"
				     '(??step0 ??step (... ...)))))))
	   (let*/tags ((?var ?init) ...)
	     (let/tags loop ((?var ID) ...)
		       (if ?test
			   (the-expr ?expr ...)
			 (begin
			   ?form ...
			   (loop (the-step ID ?step ...) ...))))))
       ))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'rnrs.define-syntax 'scheme-indent-function 1)
;; eval: (put 'aux.method-syntax 'scheme-indent-function 1)
;; eval: (put 'aux.method 'scheme-indent-function 1)
;; eval: (put 'case-symbol 'scheme-indent-function 1)
;; eval: (put 'case-identifier 'scheme-indent-function 1)
;; eval: (put 'THE-PARENT 'scheme-indent-function 1)
;; eval: (put 'receive-and-return/tags 'scheme-indent-function 2)
;; End:
