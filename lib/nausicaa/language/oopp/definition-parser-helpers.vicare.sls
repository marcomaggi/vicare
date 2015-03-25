;;;
;;;Part of: Vicare Scheme
;;;Contents: helpers for OOPP
;;;Date: Tue May  1, 2012
;;;
;;;Abstract
;;;
;;;	This library implements parser functions to be used when parsing
;;;	DEFINE-CLASS, DEFINE-LABEL and DEFINE-MIXIN  syntaxes.  It is to
;;;	be imported for the expand phase by "(nausicaa language oopp)".
;;;
;;;Copyright (C) 2012-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language oopp definition-parser-helpers (0 4))
  (export
    parse-label-definition		parse-class-definition
    parse-mixin-definition

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
    <parsed-spec>-nongenerative-uid
    <parsed-spec>-concrete-fields-names
    <parsed-spec>-common-protocol	<parsed-spec>-public-protocol
    <parsed-spec>-super-protocol	<parsed-spec>-satisfactions
    <parsed-spec>-list-of-uids-id

    <parsed-spec>-concrete-fields-data
    <parsed-spec>-mutable-fields-data		<parsed-spec>-immutable-fields-data
    <parsed-spec>-unsafe-mutable-fields-data	<parsed-spec>-unsafe-immutable-fields-data

    <class-spec>?
    <class-spec>-record-type-id		<class-spec>-satisfaction-clauses
    <class-spec>-default-protocol-id	<class-spec>-from-fields-constructor-id

    <label-spec>?
    <label-spec>-satisfaction-clauses)
  (import (for (vicare (0 4)) run expand)
    (for (only (rnrs)
	       lambda define define-syntax set!)
      (meta -1))
    (vicare unsafe operations)
    (only (vicare language-extensions identifier-substitutions)
	  multi-identifier-subst)
    (prefix (only (nausicaa language oopp configuration (0 4))
		  enable-satisfactions)
	    config.)
    (for (nausicaa language oopp auxiliary-syntaxes (0 4))
      (meta -1))
    (for (prefix (only (nausicaa language auxiliary-syntaxes (0 4))
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
		       maker		finaliser
		       <>)
		 aux.)
      (meta -1)))


;;;; private helpers

(define (%false-or-identifier? obj)
  (or (not obj)
      (identifier? obj)))


;;;; public helpers: visible identifiers composition

(define (make-method-identifier tag-id method-id)
  (identifier-append tag-id
		     (identifier->string tag-id)
		     "-"
		     (identifier->string method-id)))

;;; --------------------------------------------------------------------

(define-inline (tag-id->record-type-id id)
  (identifier-suffix id "-record-type"))

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


;;;; data types: parsed clauses representation

;;This record type represents the parsing results of all the clauses for
;;all the tag definitions.  Some clauses  apply only to classes and some
;;clauses apply  only to  labels, but  we do not  represent them  in the
;;class and label specification record type: the mixins must support all
;;the clauses, so  to use this record  type as base for  the mixin type:
;;all the parsing results go here for simplicity.
;;
(define-record-type <parsed-spec>
  (nongenerative nausicaa:language:oopp:<parsed-spec>)
  (protocol
   (lambda (make-instance)
     (lambda* ({name-id identifier?} {top-id identifier?} {lambda-id identifier?})
       (make-instance name-id top-id lambda-id
	 '() #;member-identifiers	'() #;definitions
	 #f  #;abstract?		#f  #;public-constructor-id
	 #f  #;public-predicate-id	#f  #;private-predicate-id
	 #f  #;common-protocol		#f  #;public-protocol
	 #f  #;super-protocol
	 top-id #;parent-id
	 '() #;concrete-fields		'() #;virtual-fields
	 '() #;methods-table
	 #f  #;sealed?			#f  #;opaque?
	 #f  #;getter			#f  #;setter
	 #f  #;nongenerative-uid	#f  #;maker-transformer
	 #f  #;finaliser-expression
	 #f  #;shadowed-identifier	'() #;satisfactions
	 '() #;mixins-inclusions	'() #;mixins-ids
	 ))))
  (fields (immutable name-id)
		;The identifier representing the type name.
	  (immutable top-id)
		;An identifier bound to the "<top>" tag.
	  (immutable lambda-id)
		;An  identifier bound  to  the  LAMBDA macro  supporting
		;tagged formal arguments.

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
		;Boolean  value.  It  is  used for  classes and  mixins.
		;When  true  the class  is  abstract  and it  cannot  be
		;instantiated; when  false the class is  concrete and it
		;can be instantiated.
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
		;Identifier bound to the parent tag syntax.  Initialised
		;to "<top>".

	  (mutable concrete-fields)
		;Null or a proper list of <CONCRETE-FIELD-SPEC> records.
		;The  order  of the  records  in  the  list matches  the
		;definition order.   Notice that fields  from the mixins
		;come last.

	  (mutable virtual-fields)
		;Null or a proper  list of <VIRTUAL-FIELD-SPEC> records.
		;The  order  of  the  records  is  the  reverse  of  the
		;definition order.

	  (mutable methods-table)
		;Null  or   an  associative  list   having:  identifiers
		;representing method names as keys, lists as values.
		;
		;  ((?method-name-id . (?rv-tag ?method-implementation-id))
		;   ...)
		;
		;The first item in the  list value is the tag identifier
		;of the single  return value from the adddress  or #f if
		;there  is no  such tag.   The second  item in  the list
		;value is  an identifier  representing the  method name,
		;which is bound to a function or syntax.
		;
		;The  order  of  the  entries  is  the  reverse  of  the
		;definition order.

	  (mutable sealed?)
		;Boolean  value.  It  is  used for  classes and  mixins.
		;When true the R6RS record  type associated to the class
		;is sealed.

	  (mutable opaque?)
		;Boolean  value.    When  true  the  R6RS   record  type
		;associated to the class is opaque.

	  (mutable getter)
		;False or a syntax  object representing an expression to
		;evaluate once,  at expand  time, to acquire  the getter
		;syntax transformer function.

	  (mutable setter)
		;False or a syntax  object representing an expression to
		;evaluate once,  at expand  time, to acquire  the setter
		;syntax transformer function.

	  (mutable nongenerative-uid)
		;A symbol  uniquely identifying  this type in  the whole
		;program.   It is  used  for classes  and mixins.   When
		;non-false: the R6RS record type associated to the class
		;is nongenerative.

	  (mutable maker-transformer)
		;False  or   a  syntax  object   holding  an  expression
		;evaluating to  a macro transformer to be  used to parse
		;the maker syntax.

	  (mutable finaliser-expression)
		;False or a  syntax object.  It is used  for classes and
		;mixins.  When  a syntax object: it  holds an expression
		;evaluating to  a function to  be used as  destructor by
		;the garbage  collector when finalising the  R6RS record
		;instance representing the class instance.

	  (mutable shadowed-identifier)
		;False  or  identifier.   It  is  used  for  labels  and
		;classes.  It  is the identifier  to insert in  place of
		;the     label     tag     identifier     when     using
		;WITH-LABEL-SHADOWING.   This  field  is false  when  no
		;shadowed   identifier  is   specified   in  the   label
		;definition.

	  (mutable satisfactions)
		;Null or  list of identifiers being  syntax keywords for
		;satisfactions.

	  (mutable mixins-inclusions)
		;Null or  list of mixin inclusion  specifications.  Each
		;spec is itself a list with format:
		;
		;   (?mixin-name-id . #'((?from-id ?to-id) ...))

	  (mutable mixins-ids)
		;Null or list of mixin name identifiers representing all
		;the mixins included in  this definition, at any nesting
		;level.
	  ))

(define-record-type <class-spec>
  (nongenerative nausicaa:language:oopp:<class-spec>)
  (parent <parsed-spec>)
  (fields (immutable record-type-id)
		;Identifier to be used as  name for the R6RS record type
		;associated to the class,  in the automatically composed
		;DEFINE-RECORD-TYPE form.
	  #| end of fields |# )
  (protocol
   (lambda (make-spec)
     (lambda (name-id top-id lambda-id)
       ((make-spec name-id top-id lambda-id)
	(tag-id->record-type-id name-id))))))

(define-record-type <label-spec>
  (nongenerative nausicaa:language:oopp:<label-spec>)
  (parent <parsed-spec>))

(define-record-type <mixin-spec>
  (nongenerative nausicaa:language:oopp:<mixin-spec>)
  (parent <parsed-spec>))


;;;; data types: mixin clauses compile-time value

(define-record-type <mixin-clauses-ctv>
  (nongenerative nausicaa:language:oopp:<mixin-clauses-ctv>)
  (fields name-id
		;Identifier representing the name of the mixin.

	  clauses
		;A  fully  unwrapped   syntax  object  representing  the
		;definition clauses of the mixin.

	  nested-mixins-ids
		;Null or  a list of identifiers  representing the mixins
		;that were nested into this one.
	  ))


;;;; data types: field specification

(define-record-type <field-spec>
  (nongenerative nausicaa:language:oopp:helpers:<field-spec>)
  (protocol
   (lambda (make-record)
     (lambda* ({name identifier?} {acc identifier?} {mut %false-or-identifier?} {tag %false-or-identifier?})
       (make-record name acc mut tag))))
  (fields (immutable name-id)
		;Identifier representing the field name.
	  (immutable accessor-id)
		;Identifier  representing  the   accessor  name;  it  is
		;automatically built when not specified.
	  (immutable mutator-id)
		;For an  immutable field:  false.  For a  mutable field:
		;identifier  representing   the  mutator  name;   it  is
		;automatically built when not specified.
	  (immutable tag-id)
		;Identifier  representing the type  tag for  this field.
		;If  no type  tag is  specified  in the  label or  class
		;definition: the parser functions must set this field to
		;the top tag identifier.
	  ))

(define-record-type <concrete-field-spec>
  (nongenerative nausicaa:language:oopp:helpers:<concrete-field-spec>)
  (parent <field-spec>))

(define-record-type <virtual-field-spec>
  (nongenerative nausicaa:language:oopp:helpers:<virtual-field-spec>)
  (parent <field-spec>))


;;;; data type methods: small functions

(define* (<parsed-spec>-member-identifiers-cons! {parsed-spec <parsed-spec>?} id what-string synner)
  ;;Add  the  identifier  ID  to  the  list  of  member  identifiers  in
  ;;PARSED-SPEC.  If such identifier  is already present: raise a syntax
  ;;violation.
  ;;
  (let ((member-identifiers ($<parsed-spec>-member-identifiers parsed-spec)))
    (cond ((identifier-memq id member-identifiers free-identifier=?)
	   => (lambda (dummy)
		(synner (string-append what-string " conflicts with other member name") id)))
	  (else
	   ($<parsed-spec>-member-identifiers-set! parsed-spec (cons id member-identifiers))))))

(define* (<parsed-spec>-definitions-cons! {parsed-spec <parsed-spec>?} definition)
  ;;Prepend a definition form to the list of definitions in PARSED-SPEC.
  ;;
  ($<parsed-spec>-definitions-set! parsed-spec (cons definition ($<parsed-spec>-definitions parsed-spec))))

(define* (<parsed-spec>-concrete-fields-cons! {parsed-spec <parsed-spec>?} field-record)
  ;;Prepend a  field record  to the  list of  concrete field  records in
  ;;PARSED-SPEC.
  ;;
  ($<parsed-spec>-concrete-fields-set! parsed-spec (cons field-record ($<parsed-spec>-concrete-fields parsed-spec))))

(define* (<parsed-spec>-virtual-fields-cons! {parsed-spec <parsed-spec>?} field-record)
  ;;Prepend  a field  record to  the list  of virtual  field  records in
  ;;PARSED-SPEC.
  ;;
  ($<parsed-spec>-virtual-fields-set! parsed-spec (cons field-record ($<parsed-spec>-virtual-fields parsed-spec))))

(define* (<parsed-spec>-methods-table-cons! {parsed-spec <parsed-spec>?}
					    method-name-id method-rv-tag-id method-implementation-id)
  ;;Prepend an entry in the methods table.
  ;;
  ($<parsed-spec>-methods-table-set! parsed-spec (cons (list method-name-id method-rv-tag-id method-implementation-id)
						       ($<parsed-spec>-methods-table parsed-spec))))

(define* (<parsed-spec>-satisfactions-cons! {parsed-spec <parsed-spec>?} id)
  (when config.enable-satisfactions
    ($<parsed-spec>-satisfactions-set! parsed-spec (cons id ($<parsed-spec>-satisfactions parsed-spec)))))

(define* (<parsed-spec>-mixins-inclusions-cons! {parsed-spec <parsed-spec>?} mixin-inclusion-spec synner)
  ($<parsed-spec>-mixins-inclusions-set! parsed-spec (cons mixin-inclusion-spec
							   ($<parsed-spec>-mixins-inclusions parsed-spec))))

(define (generate-unique-id parsed-spec)
  (datum->syntax (<parsed-spec>-name-id parsed-spec) (gensym)))

;;; --------------------------------------------------------------------

(define* (<parsed-spec>-list-of-uids-id {parsed-spec <parsed-spec>?})
  (tag-id->list-of-uids-id ($<parsed-spec>-name-id parsed-spec)))

(define* (<class-spec>-default-protocol-id {class-spec <class-spec>?})
  (tag-id->default-protocol-id ($<parsed-spec>-name-id class-spec)))

(define* (<class-spec>-from-fields-constructor-id {class-spec <class-spec>?})
  (tag-id->from-fields-constructor-id ($<parsed-spec>-name-id class-spec)))

;;; --------------------------------------------------------------------

(define* (<parsed-spec>-mutable-fields-data {spec <parsed-spec>?})
  ;;Select the mutable fields among  the concrete and virtual fields and
  ;;return a list of lists with the format:
  ;;
  ;;	((?field-name ?accessor ?mutator ?tag) ...)
  ;;
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

(define* (<parsed-spec>-unsafe-mutable-fields-data {spec <parsed-spec>?})
  ;;Select the  mutable fields  among the concrete  fields and  return a
  ;;list of lists with the format:
  ;;
  ;;	((?field-name ?unsafe-field-name ?tag) ...)
  ;;
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

(define* (<parsed-spec>-immutable-fields-data {spec <parsed-spec>?})
  ;;Select the  immutable fields among  the concrete and  virtual fields
  ;;and return a list of lists with the format:
  ;;
  ;;	((?field-name ?unsafe-field-name ?accessor ?tag) ...)
  ;;
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

(define* (<parsed-spec>-unsafe-immutable-fields-data {spec <parsed-spec>?})
  ;;Select the immutable  fields among the concrete fields  and return a
  ;;list of lists with the format:
  ;;
  ;;	((?field-name ?unsafe-field-name ?tag) ...)
  ;;
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

(define* (<parsed-spec>-concrete-fields-data {spec <parsed-spec>?})
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
  (map (lambda (field-record)
	 (let ((name-id     ($<field-spec>-name-id     field-record))
	       (accessor-id ($<field-spec>-accessor-id field-record))
	       (mutator     ($<field-spec>-mutator-id  field-record)))
	   (if mutator
	       (list #'aux.mutable name-id accessor-id mutator)
	     (list #'aux.immutable name-id accessor-id))))
    ($<parsed-spec>-concrete-fields spec)))

(define* (<parsed-spec>-concrete-fields-names {spec <parsed-spec>?})
  ;;Take the concrete fields and return a list with the format:
  ;;
  ;;   (?field-name ...)
  ;;
  ;;where ?FIELD-NAME is an identifier representing a field name.
  ;;
  (map (lambda (field-record)
	 ($<field-spec>-name-id field-record))
    ($<parsed-spec>-concrete-fields spec)))

;;; --------------------------------------------------------------------

(module (<label-spec>-satisfaction-clauses
	 <class-spec>-satisfaction-clauses)

  (define* (<label-spec>-satisfaction-clauses {spec <parsed-spec>?})
    (receive (virtual-mutable-fields virtual-immutable-fields)
	(%field-spec-satisfaction-clauses ($<parsed-spec>-virtual-fields spec))
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

  (define* (<class-spec>-satisfaction-clauses {spec <parsed-spec>?})
    (let-values (((concrete-mutable-fields concrete-immutable-fields)
		  (%field-spec-satisfaction-clauses ($<parsed-spec>-concrete-fields spec)))
		 ((virtual-mutable-fields virtual-immutable-fields)
		  (%field-spec-satisfaction-clauses ($<parsed-spec>-virtual-fields  spec))))
      (list (list ($<parsed-spec>-name-id spec)
		  ($<parsed-spec>-public-constructor-id	spec)
		  ($<parsed-spec>-public-predicate-id	spec)
		  ($<class-spec>-record-type-id		spec))
	    (list #'aux.parent		($<parsed-spec>-parent-id spec))
	    (cons #'aux.fields		concrete-mutable-fields)
	    (cons #'aux.fields		concrete-immutable-fields)
	    (cons #'aux.virtual-fields	virtual-mutable-fields)
	    (cons #'aux.virtual-fields	virtual-immutable-fields)
	    (cons #'aux.methods		($<parsed-spec>-methods-table spec))
	    (list #'aux.getter		($<parsed-spec>-getter spec))
	    (list #'aux.setter		($<parsed-spec>-setter spec))
	    (list #'aux.nongenerative	($<parsed-spec>-nongenerative-uid spec))
	    (list #'aux.sealed		($<parsed-spec>-sealed? spec))
	    (list #'aux.opaque		($<parsed-spec>-opaque? spec))
	    (list #'aux.abstract	($<parsed-spec>-abstract? spec))
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

  #| end of module |# )


;;;; parsers entry points

(module (parse-class-definition
	 parse-label-definition
	 parse-mixin-definition)

  (define (parse-class-definition stx top-id lambda-id ctv-retriever synner)
    ;;Parse the full  DEFINE-CLASS form in the syntax  object STX (after
    ;;mixin clauses  insertion) and  return an  instance of  record type
    ;;"<class-spec>".
    ;;
    ;;TOP-ID must be an identifier  bound to the "<top>" tag.  LAMBDA-ID
    ;;must be an identifier bound  to the LAMBDA macro supporting tagged
    ;;formal arguments.   SYNNER must be a  closure to be used  to raise
    ;;syntax violations.
    ;;
    (define (%combine class-spec clause-spec args)
      ((syntax-clause-spec-custom-data clause-spec) class-spec args synner)
      class-spec)
    (syntax-case stx ()
      ((_ ?tag-spec ?clause ...)
       (receive (name-id public-constructor-id predicate-id)
	   (%parse-tag-name-spec #'?tag-spec synner)
	 (receive-and-return (class-spec)
	     (make-<class-spec> name-id top-id lambda-id)
	   ($<parsed-spec>-public-constructor-id-set! class-spec public-constructor-id)
	   ($<parsed-spec>-public-predicate-id-set!   class-spec predicate-id)
	   (syntax-clauses-fold-specs %combine class-spec CLASS-CLAUSES-SPECS
				      (syntax-clauses-unwrap #'(?clause ...) synner)
				      synner)
	   (syntax-clauses-fold-specs %combine class-spec CLASS-CLAUSES-SPECS
				      (%process-mixin-inclusion-requests! class-spec ctv-retriever synner)
				      synner)
	   (%finalise-clauses-parsing! class-spec synner))))
      (_
       (synner "syntax error in class definition"))))

  (define (parse-label-definition stx top-id lambda-id ctv-retriever synner)
    ;;Parse the full  DEFINE-LABEL form in the syntax  object STX (after
    ;;mixin clauses  insertion) and  return an  instance of  record type
    ;;"<label-spec>".
    ;;
    ;;TOP-ID must be an identifier  bound to the "<top>" tag.  LAMBDA-ID
    ;;must be an identifier bound  to the LAMBDA macro supporting tagged
    ;;formal arguments.   SYNNER must be a  closure to be used  to raise
    ;;syntax violations.
    ;;
    (define (%combine label-spec clause-spec args)
      ((syntax-clause-spec-custom-data clause-spec) label-spec args synner)
      label-spec)
    (syntax-case stx ()
      ((_ ?tag-spec ?clause ...)
       (receive (name-id public-constructor-id predicate-id)
	   (%parse-tag-name-spec #'?tag-spec synner)
	 (receive-and-return (label-spec)
	     (make-<label-spec> name-id top-id lambda-id)
	   ($<parsed-spec>-public-constructor-id-set! label-spec public-constructor-id)
	   ($<parsed-spec>-public-predicate-id-set!   label-spec predicate-id)
	   (syntax-clauses-fold-specs %combine label-spec LABEL-CLAUSES-SPECS
				      (syntax-clauses-unwrap #'(?clause ...) synner)
				      synner)
	   (syntax-clauses-fold-specs %combine label-spec LABEL-CLAUSES-SPECS
				      (%process-mixin-inclusion-requests! label-spec ctv-retriever synner)
				      synner)
	   (%finalise-clauses-parsing! label-spec synner))))
      (_
       (synner "syntax error in label definition"))))

  (define (parse-mixin-definition stx top-id lambda-id ctv-retriever synner)
    ;;Parse  the full  DEFINE-MIXIN form  in the  syntax object  STX and
    ;;build  an instance  of record  type "<mixin-spec>"  for validation
    ;;purposes.  Finally  return two values: the  mixin name identifier;
    ;;an instance  of "<mixin-clauses-ctv>" to  be used later  for mixin
    ;;inclusion into other definitions.
    ;;
    ;;TOP-ID must be an identifier  bound to the "<top>" tag.  LAMBDA-ID
    ;;must be an identifier bound  to the LAMBDA macro supporting tagged
    ;;formal arguments.   SYNNER must be a  closure to be used  to raise
    ;;syntax violations.
    ;;
    ;;We  perform the  full parsing  of the  definition to  catch errors
    ;;early, but we discard the results  of such parsing: the clauses in
    ;;a mixin definition are used by  the label or class definition that
    ;;imports them.
    ;;
    (define (%combine mixin-spec clause-spec args)
      ((syntax-clause-spec-custom-data clause-spec) mixin-spec args synner)
      mixin-spec)
    (syntax-case stx ()
      ((_ ?mixin-name ?clause ...)
       (identifier? #'?mixin-name)
       (let* ((clauses     (syntax-clauses-unwrap #'(?clause ...) synner))
	      (mixin-spec  (make-<mixin-spec> #'?mixin-name top-id lambda-id)))
	 (syntax-clauses-fold-specs %combine mixin-spec MIXIN-CLAUSES-SPECS clauses synner)
	 (let ((included-clauses (%process-mixin-inclusion-requests! mixin-spec ctv-retriever synner)))
	   (syntax-clauses-fold-specs %combine mixin-spec CLASS-CLAUSES-SPECS
				      included-clauses synner)
	   (%finalise-clauses-parsing! mixin-spec synner)
	   (values #'?mixin-name
		   (make-<mixin-clauses-ctv> #'?mixin-name
		   			     (append clauses included-clauses)
		   			     (<parsed-spec>-mixins-ids mixin-spec))))))
      (_
       (synner "syntax error in mixin definition"))))

  (define (%parse-tag-name-spec stx synner)
    ;;Parse the first component of a class or label definition:
    ;;
    ;;  (define-class ?tag-name-spec . ?clauses)
    ;;  (define-label ?tag-name-spec . ?clauses)
    ;;
    ;;supported syntaxes for ?TAG-NAME-SPEC are:
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

  (define* (%process-mixin-inclusion-requests! {parsed-spec <parsed-spec>?} ctv-retriever synner)
    ;;For  each mixin  inclusion  request in  PARSED-SPEC: retrieve  the
    ;;corresponding compile-time  value (CTV),  which is an  instance of
    ;;"<mixin-clauses-ctv>",  and  apply  the  identifiers  map  to  the
    ;;mixin's clauses.
    ;;
    ;;Return the list  of fully unwrapped clauses ready to  be parsed to
    ;;update PARSED-SPEC.
    ;;
    (fold-left
	(lambda (clauses mixin-inclusion-request)
	  (let* ((mixin-name-id  (car mixin-inclusion-request))
		 (mixin-id-map   (cdr mixin-inclusion-request))
		 (mixin-ctv      (ctv-retriever mixin-name-id)))
	    (cond ((<mixin-clauses-ctv>? mixin-ctv)
		   ;;Check that a mixin is  not included twice, then add
		   ;;the imported mixin identifiers in the parsed spec.
		   (let ((imported-ids (cons mixin-name-id
					     ($<mixin-clauses-ctv>-nested-mixins-ids mixin-ctv))))
		     (for-each (lambda (id1)
				 (for-each (lambda (id2)
					     (when (free-identifier=? id1 id2)
					       (synner "multiple inclusion of mixin" id1)))
				   ($<parsed-spec>-mixins-ids parsed-spec)))
		       imported-ids)
		     ($<parsed-spec>-mixins-ids-set! parsed-spec
						     (append ($<parsed-spec>-mixins-ids parsed-spec)
							     imported-ids)))
		   ;;Build and return the processed clauses.
		   (append (multi-identifier-subst
			    (cons (list mixin-name-id ($<parsed-spec>-name-id parsed-spec))
				  mixin-id-map)
			    ($<mixin-clauses-ctv>-clauses mixin-ctv))
			   clauses))
		  (mixin-ctv
		   (synner "identifier is not a mixin name" (list mixin-name-id mixin-ctv)))
		  (else
		   (synner "unknown mixin name" mixin-name-id)))))
      '()
      ;;Remember that  these are in  reversed order with respect  to the
      ;;order in which they appear in the MIXINS clauses.
      ($<parsed-spec>-mixins-inclusions parsed-spec)))

  (define* (%finalise-clauses-parsing! {parsed-spec <parsed-spec>?} synner)
    ;;Normalise  the results  of parsing  class, label  or mixin  clauses.
    ;;Mutate PARSED-SPEC.  Return unspecified values.
    ;;
    (when (<class-spec>? parsed-spec)
      (let ((concrete-fields ($<parsed-spec>-concrete-fields parsed-spec)))
	(unless (null? concrete-fields)
	  ($<parsed-spec>-concrete-fields-set! parsed-spec (reverse concrete-fields))))
      (when ($<parsed-spec>-abstract? parsed-spec)
	(when (<parsed-spec>-common-protocol parsed-spec)
	  (synner "common protocol clause forbidden in definition of abstract class"))
	(when (<parsed-spec>-public-protocol parsed-spec)
	  (synner "public protocol clause forbidden in definition of abstract class"))))
    (let ((definitions ($<parsed-spec>-definitions parsed-spec)))
      (unless (null? definitions)
	($<parsed-spec>-definitions-set! parsed-spec (reverse definitions))))
    (unless ($<parsed-spec>-nongenerative-uid parsed-spec)
      ($<parsed-spec>-nongenerative-uid-set! parsed-spec (generate-unique-id parsed-spec))))

  #| end of module |# )


;;;; some at-most-once clause parsers: parent, opaque, sealed, nongenerative, shadows, maker

(define (clause-arguments-parser:parent parsed-spec args synner)
  ;;Parser function for  the PARENT clause; this clause  must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (parent ?parent-tag-id)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?parent-tag-id ))
  ;;
  ;;where ?parent-TAG-ID  is the identifier  bound to the tag  syntax of
  ;;the parent type.
  ;;
  (let ((parent-tag-id ($vector-ref ($vector-ref args 0) 0)))
    (if (identifier? parent-tag-id)
	($<parsed-spec>-parent-id-set! parsed-spec parent-tag-id)
      (synner "invalid tag parent type specification" parent-tag-id))))

(define (clause-arguments-parser:sealed parsed-spec args synner)
  ;;Parser function for  the SEALED clause; this clause  must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (sealed #t)
  ;;   (sealed #f)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?bool ))
  ;;
  (let* ((bool-stx ($vector-ref ($vector-ref args 0) 0))
	 (bool     (syntax->datum bool-stx)))
    (if (boolean? bool)
	($<parsed-spec>-sealed?-set! parsed-spec bool)
      (synner "invalid tag type sealed specification" bool-stx))))

(define (clause-arguments-parser:opaque parsed-spec args synner)
  ;;Parser function for  the OPAQUE clause; this clause  must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (opaque #t)
  ;;   (opaque #f)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?bool ))
  ;;
  (let* ((bool-stx ($vector-ref ($vector-ref args 0) 0))
	 (bool     (syntax->datum bool-stx)))
    (if (boolean? bool)
	($<parsed-spec>-opaque?-set! parsed-spec bool)
      (synner "invalid tag type opaque specification" bool-stx))))

(define (clause-arguments-parser:shadows parsed-spec args synner)
  ;;Parser function for the SHADOWS  clause; this clause must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (shadows ?shadowed-id)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?shadowed-id ))
  ;;
  ;;where  ?SHADOWED-ID is  an identifier.   The selected  identifier is
  ;;used by the syntax WITH-LABEL-SHADOWING to hide some type definition
  ;;with  a label.   For  example:  this mechanism  allows  to shadow  a
  ;;condition type  definition with a label  type and so to  use the tag
  ;;syntaxes with condition objects.
  ;;
  (let ((shadowed-id ($vector-ref ($vector-ref args 0) 0)))
    (if (identifier? shadowed-id)
	($<parsed-spec>-shadowed-identifier-set! parsed-spec shadowed-id)
      (synner "invalid tag type shadowed identifier specification" shadowed-id))))

(define (clause-arguments-parser:maker parsed-spec args synner)
  ;;Parser function for the MAKER clause; this clause must be present at
  ;;most once.  The expected syntax for the clause is:
  ;;
  ;;   (maker ?transformer-expr)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?transformer-expr ))
  ;;
  ;;where  ?TRANSFORMER-EXPR  is an  expression  evaluating  to a  macro
  ;;transformer to be used to parse the maker syntax.
  ;;
  ;;We can imagine the definition:
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
  ($<parsed-spec>-maker-transformer-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:finaliser parsed-spec args synner)
  ;;Parser  function  for the  FINALISER  clause;  this clause  must  be
  ;;present at most once.  The expected syntax for the clause is:
  ;;
  ;;   (finaliser ?lambda-expr)
  ;;
  ;;and the correspoding ARGS is:
  ;;
  ;;   #(#( #'?lambda-expr ))
  ;;
  ;;where ?LAMBDA-EXPR is  an expression evaluating to a  function to be
  ;;used by the garbage collector to finalise the record.
  ;;
  ($<parsed-spec>-finaliser-expression-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:nongenerative parsed-spec args synner)
  ;;Parser function  for the NONGENERATIVE  clause; this clause  must be
  ;;present at  most once;  this clause  must have  zero arguments  or a
  ;;single argument:
  ;;
  ;;   (nongenerative)
  ;;   (nongenerative ?unique-id)
  ;;
  ;;and the corresponding ARGS are:
  ;;
  ;;   #(#())
  ;;   #(#( #'?unique-id ))
  ;;
  ;;where ?UNIQUE-ID is the symbol  which uniquely identifies the record
  ;;type in a whole program.  If the clause has no argument: a unique id
  ;;is automatically generated.
  ;;
  (let ((inner ($vector-ref args 0)))
    (if ($fxzero? ($vector-length inner))
	($<parsed-spec>-nongenerative-uid-set! parsed-spec (generate-unique-id parsed-spec))
      (let ((unique-id ($vector-ref inner 0)))
	(if (identifier? unique-id)
	    ($<parsed-spec>-nongenerative-uid-set! parsed-spec unique-id)
	  (synner "expected identifier as NONGENERATIVE clause argument" unique-id))))))


;;;; some at-most-once clause parsers: protocols, abstract, predicate

(define (clause-arguments-parser:common-protocol parsed-spec args synner)
  ;;Parser function for the PROTOCOL clause; this clause must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (protocol ?protocol-expr)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?protocol-expr ))
  ;;
  ($<parsed-spec>-common-protocol-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:public-protocol parsed-spec args synner)
  ;;Parser function for the PUBLIC-PROTOCOL  clause; this clause must be
  ;;present at most once.  The expected syntax for the clause is:
  ;;
  ;;   (public-protocol ?protocol-expr)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?protocol-expr ))
  ;;
  ($<parsed-spec>-public-protocol-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:super-protocol parsed-spec args synner)
  ;;Parser function for  the SUPER-PROTOCOL clause; this  clause must be
  ;;present at most once.  The expected syntax for the clause is:
  ;;
  ;;   (super-protocol ?protocol-expr)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?protocol-expr ))
  ;;
  ($<parsed-spec>-super-protocol-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:abstract parsed-spec args synner)
  ;;Parser function for the ABSTRACT clause; this clause must be present
  ;;at  most once  and  only  in a  DEFINE-CLASS  or DEFINE-MIXIN.   The
  ;;expected syntax for the clause is:
  ;;
  ;;   (abstract)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#())
  ;;
  ;;Notice that this function is called only when the ABSTRACT clause is
  ;;present.
  ;;
  ($<parsed-spec>-abstract?-set! parsed-spec #t))

(define (clause-arguments-parser:predicate parsed-spec args synner)
  ;;Parser  function  for the  PREDICATE  clause;  this clause  must  be
  ;;present at most once.  The expected syntax for the clause is:
  ;;
  ;;   (predicate ?predicate)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?predicate ))
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
  ;;and the following tag syntax expansion happens:
  ;;
  ;;  (<list> is-a? ?obj) ==> (<list>? ?obj)
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
  ;;
  ;;  (define (<list-of-numbers>? obj)
  ;;    (and (<pair> :is-a? obj)
  ;;         (<list-of-numbers>-private-predicate obj)))
  ;;
  ;;and the following tag syntax expansion happens:
  ;;
  ;;  (<list-of-numbers> is-a? ?obj) ==> (<list-of-numbers>? ?obj)
  ;;
  (let ((predicate-stx ($vector-ref ($vector-ref args 0) 0)))
    (if (identifier? predicate-stx)
	($<parsed-spec>-private-predicate-id-set! parsed-spec predicate-stx)
      (let ((pred-id (tag-id->private-predicate-id ($<parsed-spec>-name-id parsed-spec))))
	($<parsed-spec>-private-predicate-id-set! parsed-spec pred-id)
	(<parsed-spec>-definitions-cons!          parsed-spec (list #'define pred-id predicate-stx))))))


;;;; single method function clauses

(module (clause-arguments-parser:single-method)
  ;;Parser function for METHOD clauses;  this clause can be present zero
  ;;or more times.  A METHOD clause has one of the following syntaxes:
  ;;
  ;;  (method (?method-int ?arg ... . ?rest) ?body0 ?body ...)
  ;;  (method ?method-ext ?lambda-expr)
  ;;
  ;;where ?METHOD-INT has one of the syntaxes:
  ;;
  ;;  ?method-name-id
  ;;  (brace ?method-name-id ?rv-tag0 ?rv-tag ...)
  ;;
  ;;and ?METHOD-EXT has one of the syntaxes:
  ;;
  ;;  ?method-name-id
  ;;  (brace ?method-name-id ?rv-tag)
  ;;
  ;;?ARG has one of the syntaxes:
  ;;
  ;;  ?arg-id
  ;;  (brace ?arg-id ?arg-tag-id)
  ;;
  ;;?REST has one of the syntaxes:
  ;;
  ;;  ()
  ;;  ?rest-id
  ;;  (brace ?rest-id ?rest-tag-id)
  ;;
  (define (clause-arguments-parser:single-method parsed-spec args synner)
    ;;We expect ARGS to have the format:
    ;;
    ;;  #(#( #'?method-spec-arg ...) ...)
    ;;
    (vector-for-each
	(lambda (method-spec-stx)
	  (%parse-method-spec method-spec-stx parsed-spec synner))
      args))

  (define (%parse-method-spec method-spec-stx parsed-spec synner)
    (define-syntax TOP-ID	(identifier-syntax (<parsed-spec>-top-id    parsed-spec)))
    (define-syntax NAME-ID	(identifier-syntax (<parsed-spec>-name-id   parsed-spec)))
    (define-syntax LAMBDA-ID	(identifier-syntax (<parsed-spec>-lambda-id parsed-spec)))
    (syntax-case method-spec-stx (brace)

      ;;Tagged single return value method definition.  The clause is:
      ;;
      ;;   (method ((brace ?method-name-id ?rv-tag) . ?formals)
      ;;     ?body0 ?body ...)
      ;;
      (#(((brace ?method-name-id ?rv-tag) . ?formals) ?body0 ?body ...)
       (and (identifier? #'?method-name-id)
	    (identifier? #'?rv-tag))
       (%add-method parsed-spec #'?method-name-id #'?rv-tag
		    (make-method-identifier NAME-ID #'?method-name-id)
		    #`(#,LAMBDA-ID ((brace _ ?rv-tag) . ?formals) ?body0 ?body ...)
		    synner))

      ;;Tagged multiple return values method definition.  The clause is:
      ;;
      ;;   (method ((brace ?method-name-id ?rv-tag0 ?rv-tag ...) . ?formals)
      ;;     ?body0 ?body ...)
      ;;
      (#(((brace ?method-name-id ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...)
       (and (identifier? #'?method-name-id)
	    (all-identifiers? #'(?rv-tag0 ?rv-tag ...)))
       (%add-method parsed-spec #'?method-name-id TOP-ID
		    (make-method-identifier NAME-ID #'?method-name-id)
		    #`(#,LAMBDA-ID ((brace _ ?rv-tag0 ?rv-tag ...) . ?formals) ?body0 ?body ...)
		    synner))

      ;;Tagged external lambda method definition.  The clause is:
      ;;
      ;;   (method (brace ?method-name-id ?rv-tag)
      ;;     ?lambda-expr))
      ;;
      (#((brace ?method-name-id ?rv-tag) ?lambda-expr)
       (and (identifier? #'?method-name-id)
	    (identifier? #'?rv-tag))
       (%add-method parsed-spec #'?method-name-id #'?rv-tag
		    (make-method-identifier NAME-ID #'?method-name-id)
		    #'?lambda-expr synner))

      ;;Untagged method definition.  The clause is:
      ;;
      ;;   (method (?method-name-id . ?formals)
      ;;     ?body0 ?body ...)
      ;;
      (#((?method-name-id . ?formals) ?body0 ?body ...)
       (identifier? #'?method-name-id)
       (%add-method parsed-spec #'?method-name-id TOP-ID
		    (make-method-identifier NAME-ID #'?method-name-id)
		    #`(#,LAMBDA-ID ?formals ?body0 ?body ...)
		    synner))

      ;;Untagged external lambda method definition.  The clause is:
      ;;
      ;;   (method ?method-name-id ?lambda-expr)
      ;;
      (#(?method-name-id ?lambda-expr)
       (identifier? #'?method-name-id)
       (%add-method parsed-spec #'?method-name-id TOP-ID
		    (make-method-identifier NAME-ID #'?method-name-id)
		    #'?lambda-expr synner))

      (_
       (synner "invalid method specification in SINGLE-METHOD clause" method-spec-stx))))

  (define (%add-method parsed-spec method-name-id method-rv-tag-id method-implementation-id method-expr synner)
    (<parsed-spec>-member-identifiers-cons! parsed-spec method-name-id "method name" synner)
    (<parsed-spec>-methods-table-cons!      parsed-spec method-name-id method-rv-tag-id method-implementation-id)
    (<parsed-spec>-definitions-cons!        parsed-spec `(,#'define ,method-implementation-id ,method-expr)))

  #| end of module: clause-arguments-parser:single-method |# )


;;;; single method syntax clauses

(module (clause-arguments-parser:method-syntax)
  ;;Parser  function  for  METHOD-SYNTAX  clauses; this  clause  can  be
  ;;present  zero  or  more  times.   A  METHOD-SYNTAX  clause  has  the
  ;;following syntax:
  ;;
  ;;  (method-syntax ?method ?transformer-expr)
  ;;
  ;;and ?METHOD has one of the following syntaxes:
  ;;
  ;;  ?method-name-id
  ;;  (brace ?method-name-id ?rv-tag)
  ;;
  ;;where:  ?METHOD-NAME-ID is  an  identifier  representing the  method
  ;;name;  ?RV-TAG is  an identifier  representing the  type tag  of the
  ;;method  single  return  value; ?TRANSFORMER-EXPR  is  an  expression
  ;;which, evaluated at expand-time, must return the a macro transformer
  ;;representing the method implementation.
  ;;
  (define (clause-arguments-parser:method-syntax parsed-spec args synner)
    ;;We expect ARGS to be:
    ;;
    ;;  #(#( #'?method #'?transformer-expr ...) ...)
    ;;
    (vector-for-each
	(lambda (method-spec-stx)
	  (%parse-method-spec method-spec-stx parsed-spec synner))
      args))

  (define (%parse-method-spec method-spec-stx parsed-spec synner)
    (define-syntax TOP-ID
      (identifier-syntax ($<parsed-spec>-top-id parsed-spec)))
    (define-syntax NAME-ID
      (identifier-syntax ($<parsed-spec>-name-id parsed-spec)))
    (syntax-case method-spec-stx (brace)
      ;;Tagged return value method definition.
      ;;
      (#((brace ?method-name ?rv-tag) ?transformer-expr)
       (and (identifier? #'?method-name)
	    (identifier? #'?rv-tag))
       (%add-method parsed-spec #'?method-name #'?rv-tag (make-method-identifier NAME-ID #'?method-name) #'?transformer-expr synner))

      ;;Untagged return value method definition.
      ;;
      (#(?method-name ?transformer-expr)
       (identifier? #'?method-name)
       (%add-method parsed-spec #'?method-name TOP-ID (make-method-identifier NAME-ID #'?method-name) #'?transformer-expr synner))

      (_
       (synner "invalid method specification in METHOD-SYNTAX clause" method-spec-stx))))

  (define (%add-method parsed-spec method-name-id method-rv-tag-id method-implementation-id transformer-expr-stx synner)
    (<parsed-spec>-member-identifiers-cons! parsed-spec method-name-id "method name" synner)
    (<parsed-spec>-methods-table-cons!      parsed-spec method-name-id method-rv-tag-id method-implementation-id)
    (<parsed-spec>-definitions-cons!        parsed-spec `(,#'define-syntax ,method-implementation-id ,transformer-expr-stx)))

  #| end of module: CLAUSE-ARGUMENTS-PARSER:MULTIPLE-METHODS |# )


;;;; multiple methods clauses

(module (clause-arguments-parser:multiple-methods)
  ;;Parser function for METHODS clauses; this clause can be present zero
  ;;or more times.  A METHODS clause has the following syntaxe:
  ;;
  ;;  (methods ?method-spec ...)
  ;;
  ;;where ?METHOD has one of the following syntaxes:
  ;;
  ;;  ?method
  ;;  (?method ?invocable-name)
  ;;
  ;;and ?METHOD has one of the following syntaxes:
  ;;
  ;;  ?method-name-id
  ;;  (brace ?method-name-id ?rv-tag)
  ;;
  ;;where:  ?METHOD-NAME-ID is  an  identifier  representing the  method
  ;;name;  ?RV-TAG is  an identifier  representing the  type tag  of the
  ;;method single  return value; ?INVOCABLE-NAME is  an identifier bound
  ;;to the method implementation, a function or macro.
  ;;
  ;;When ?INVOCABLE-NAME is not specified  a default identifier is built
  ;;and used in its place.
  ;;
  (define (clause-arguments-parser:multiple-methods parsed-spec args synner)
    ;;We expect ARGS to be:
    ;;
    ;;  #(#( #'?method-spec ...) ...)
    ;;
    (vector-for-each
	(lambda (methods-clause-stx)
	  (vector-for-each
	      (lambda (method-spec-stx)
		(%parse-method-spec method-spec-stx parsed-spec synner))
	    methods-clause-stx))
      args))

  (define (%parse-method-spec method-spec-stx parsed-spec synner)
    (define-syntax TOP-ID
      (identifier-syntax (<parsed-spec>-top-id parsed-spec)))
    (define-syntax NAME-ID
      (identifier-syntax (<parsed-spec>-name-id parsed-spec)))
    (syntax-case method-spec-stx (brace)
      (?method-name
       (identifier? #'?method-name)
       (%add-method parsed-spec #'?method-name TOP-ID (make-method-identifier NAME-ID #'?method-name) synner))

      ;;Untagged return value method definition.
      ((?method-name ?invocable-name)
       (and (identifier? #'?method-name)
	    (identifier? #'?invocable-name))
       (%add-method parsed-spec #'?method-name TOP-ID #'?invocable-name synner))

      ;;Tagged return value method definition.
      (((brace ?method-name ?rv-tag) ?invocable-name)
       (and (identifier? #'?method-name)
	    (identifier? #'?rv-tag)
	    (identifier? #'?invocable-name))
       (%add-method parsed-spec #'?method-name #'?rv-tag #'?invocable-name synner))

      (_
       (synner "invalid method specification in METHODS clause" method-spec-stx))))

  (define (%add-method parsed-spec method-name-id method-rv-tag-id method-implementation-id synner)
    (<parsed-spec>-member-identifiers-cons! parsed-spec method-name-id "method name" synner)
    (<parsed-spec>-methods-table-cons!      parsed-spec method-name-id method-rv-tag-id method-implementation-id))

  #| end of module: CLAUSE-ARGUMENTS-PARSER:MULTIPLE-METHODS |# )


;;;; concrete and virtual fields helpers

(module FIELD-SPEC-PARSER
  (%parse-field-spec)

  (define* (%parse-field-spec field-spec-stx
			      {parsed-spec <parsed-spec>?}
			      {register-mutable-field   procedure?}
			      {register-immutable-field procedure?}
			      {synner procedure?})
    ;;Parse  a concrete  or virtual  field specification  and apply  the
    ;;proper function to the result.
    ;;
    (syntax-case field-spec-stx (aux.mutable aux.immutable)
      ((aux.mutable ?field ?accessor ?mutator)
       (receive (field-name-id type-tag-id)
	   (%parse-field #'?field parsed-spec synner)
	 (register-mutable-field field-name-id type-tag-id #'?accessor #'?mutator parsed-spec synner)))

      ((aux.immutable ?field ?accessor)
       (receive (field-name-id type-tag-id)
	   (%parse-field #'?field parsed-spec synner)
	 (register-immutable-field field-name-id type-tag-id #'?accessor parsed-spec synner)))

      ((aux.mutable ?field)
       (receive (field-name-id type-tag-id)
	   (%parse-field #'?field parsed-spec synner)
	 (register-mutable-field field-name-id type-tag-id #f #f parsed-spec synner)))

      ((aux.immutable ?field)
       (receive (field-name-id type-tag-id)
	   (%parse-field #'?field parsed-spec synner)
	 (register-immutable-field field-name-id type-tag-id #f parsed-spec synner)))

      (?field
       (receive (field-name-id type-tag-id)
	   (%parse-field #'?field parsed-spec synner)
	 (register-immutable-field field-name-id type-tag-id #f parsed-spec synner)))

      (_
       (synner "invalid virtual-field specification" field-spec-stx))))

  (define (%parse-field field-stx parsed-spec synner)
    (syntax-case field-stx (brace)
      (?field-name-id
       (identifier? #'?field-name-id)
       (values #'?field-name-id (<parsed-spec>-top-id parsed-spec)))

      ((brace ?field-name-id ?type-tag-id)
       (and (identifier? #'?field-name-id)
	    (identifier? #'?type-tag-id))
       (values #'?field-name-id #'?type-tag-id))

      (_
       (synner "invalid field name and type tag specification" field-stx))))

  #| end of module: %PARSE-FIELD-SPEC |# )


;;;; concrete fields clauses

(module (clause-arguments-parser:concrete-fields)
  ;;Parser function for FIELDS clauses;  this clause can be present zero
  ;;or more times; this clause accepts zero or more arguments.  A FIELDS
  ;;clause has the following syntax:
  ;;
  ;;  (fields ?field-spec ...)
  ;;
  ;;where ?FIELD-SPEC has one of the following syntaxes:
  ;;
  ;;  (mutable   ?field)
  ;;  (mutable   ?field ?accessor ?mutator)
  ;;  (immutable ?field)
  ;;  (immutable ?field ?accessor)
  ;;  ?field
  ;;
  ;;where ?FIELD has one of the following syntaxes:
  ;;
  ;;  ?field-name-id
  ;;  (brace ?field-name-id ?type-tag-id)
  ;;
  ;;both ?ACCESSOR and ?MUTATOR must be identifiers.
  ;;
  (define (clause-arguments-parser:concrete-fields parsed-spec args synner)
    ;;We expect ARGS to be:
    ;;
    ;;  #(#( #'?field-spec ...) ...)
    ;;
    (vector-for-each
	(lambda (fields-clause-stx)
	  (vector-for-each
	      (lambda (field-spec-stx)
		(import FIELD-SPEC-PARSER)
		(%parse-field-spec field-spec-stx parsed-spec
				   %register-mutable-field %register-immutable-field synner))
	    fields-clause-stx))
      args))

  (define (%register-mutable-field field-name-id type-tag-id accessor-stx mutator-stx parsed-spec synner)
    (define accessor-id
      (%parse-concrete-field-acc/mut-spec field-name-id accessor-stx parsed-spec
					  identifier-record-field-accessor synner))
    (define mutator-id
      (%parse-concrete-field-acc/mut-spec field-name-id mutator-stx parsed-spec
					  identifier-record-field-mutator synner))
    (define field-spec
      (make-<concrete-field-spec> field-name-id accessor-id mutator-id type-tag-id))
    (%add-field-record parsed-spec field-spec synner))

  (define (%register-immutable-field field-name-id type-tag-id accessor-stx parsed-spec synner)
    (define accessor-id
      (%parse-concrete-field-acc/mut-spec field-name-id accessor-stx parsed-spec
					  identifier-record-field-accessor synner))
    (define field-spec
      (make-<concrete-field-spec> field-name-id accessor-id #f type-tag-id))
    (%add-field-record parsed-spec field-spec synner))

  (define* (%parse-concrete-field-acc/mut-spec {field-name-id identifier?}
					       acc/mut-stx
					       {parsed-spec <parsed-spec>?}
					       make-default-id synner)
    ;;Arguments:  the  field  name identifier  FIELD-NAME-ID,  a  syntax
    ;;object   ACC/MUT-STX   representing   the  accessor   or   mutator
    ;;specification,  the parsed  spec  record.   Support the  following
    ;;cases:
    ;;
    ;;If ACC/MUT-STX is an identifier:  such identifier will be bound to
    ;;the accessor or mutator function.
    ;;
    ;;If  ACC/MUT-STX  is  false:  it means  no  specification  for  the
    ;;accessor or  mutator was present.   A default accessor  or mutator
    ;;identifier is built  and returned.  Such identifier  will be bound
    ;;to the accessor or mutator function.
    ;;
    (cond ((identifier? acc/mut-stx)
	   acc/mut-stx)
	  ((not acc/mut-stx)
	   (make-default-id (<parsed-spec>-name-id parsed-spec) field-name-id))
	  (else
	   (synner "expected identifier as field accessor or mutator specification"
		   acc/mut-stx))))

  (define* (%add-field-record {parsed-spec <parsed-spec>?} {field-record <field-spec>?} synner)
    ;;Add a record representing a field specification to the appropriate
    ;;field in PARSED-SPEC.  Check for duplicate names in members.
    ;;
    (<parsed-spec>-member-identifiers-cons! parsed-spec (<field-spec>-name-id field-record)
					    "field name" synner)
    (<parsed-spec>-concrete-fields-cons! parsed-spec field-record))

  #| end of module |# )


;;;; virtual fields clauses

(module (clause-arguments-parser:virtual-fields)
  ;;Parser  function  for VIRTUAL-FIELDS  clauses;  this  clause can  be
  ;;present  zero  or more  times;  this  clause  accepts zero  or  more
  ;;arguments.  A VIRTUAL-FIELDS clause has the following syntax:
  ;;
  ;;  (virtual-fields ?field-spec ...)
  ;;
  ;;where ?FIELD-SPEC has one of the following syntaxes:
  ;;
  ;;  (mutable   ?field)
  ;;  (mutable   ?field ?accessor ?mutator)
  ;;  (immutable ?field)
  ;;  (immutable ?field ?accessor)
  ;;  ?field
  ;;
  ;;where ?FIELD has one of the following syntaxes:
  ;;
  ;;  ?field-name-id
  ;;  (brace ?field-name-id ?type-tag-id)
  ;;
  ;;both ?ACCESSOR  and ?MUTATOR can  be identifiers bound  to functions
  ;;and syntaxes or arbitrary expressions evaluating to the accessor and
  ;;mutator functions or syntax keyword.
  ;;
  (define (clause-arguments-parser:virtual-fields parsed-spec args synner)
    ;;We expect ARGS to be:
    ;;
    ;;  #(#( #'?field-spec ...) ...)
    ;;
    (vector-for-each
	(lambda (virtual-fields-clause-stx)
	  (vector-for-each
	      (lambda (field-spec-stx)
		(import FIELD-SPEC-PARSER)
		(%parse-field-spec field-spec-stx parsed-spec
				   %register-mutable-field %register-immutable-field synner))
	    virtual-fields-clause-stx))
      args))

  (define (%register-mutable-field field-name-id type-tag-id accessor-stx mutator-stx parsed-spec synner)
    (define accessor-id
      (%parse-virtual-field-acc/mut-spec field-name-id accessor-stx parsed-spec
					 identifier-record-field-accessor synner))
    (define mutator-id
      (%parse-virtual-field-acc/mut-spec field-name-id mutator-stx parsed-spec
					 identifier-record-field-mutator synner))
    (define field-spec
      (make-<virtual-field-spec> field-name-id accessor-id mutator-id type-tag-id))
    (%add-field-record parsed-spec field-spec synner))

  (define (%register-immutable-field field-name-id type-tag-id accessor-stx parsed-spec synner)
    (define accessor-id
      (%parse-virtual-field-acc/mut-spec field-name-id accessor-stx parsed-spec
					 identifier-record-field-accessor synner))
    (define field-spec
      (make-<virtual-field-spec> field-name-id accessor-id #f type-tag-id))
    (%add-field-record parsed-spec field-spec synner))

  (define* (%parse-virtual-field-acc/mut-spec {field-name-id identifier?}
					      acc/mut-stx
					      {parsed-spec <parsed-spec>?}
					      make-default-id synner)
    ;;Arguments:  the  field  name identifier  FIELD-NAME-ID,  a  syntax
    ;;object   ACC/MUT-STX   representing   the  accessor   or   mutator
    ;;specification,  the parsed  spec  record.   Support the  following
    ;;cases:
    ;;
    ;;If ACC/MUT-STX  is an identifier:  such identifier is meant  to be
    ;;bound to a function or macro  performing the access or mutation on
    ;;the instance.
    ;;
    ;;If  ACC/MUT-STX  is  false:  it means  no  specification  for  the
    ;;accessor or mutator  was present.  We build a  default accessor or
    ;;mutator identifier  and return  it.  It  is responsibility  of the
    ;;user code to  define a function or macro performing  the access or
    ;;mutation  on  the  instance,  and   to  bind  it  to  the  default
    ;;identifier.
    ;;
    ;;Otherwise  ACC/MUT-STX  must  represent  an  arbitrary  expression
    ;;which, evaluated at run-time, will  return a function.  We build a
    ;;default accessor or mutator identifier  and return it; in addition
    ;;we register in PARSED-SPEC a  definition binding the expression to
    ;;the default identifier.
    ;;
    (if (identifier? acc/mut-stx)
	acc/mut-stx
      (let ((acc/mut-id (make-default-id (<parsed-spec>-name-id parsed-spec)
					 field-name-id)))
	(when acc/mut-stx
	  (<parsed-spec>-definitions-cons! parsed-spec (list #'define acc/mut-id acc/mut-stx)))
	acc/mut-id)))

  (define* (%add-field-record {parsed-spec <parsed-spec>?} {field-record <field-spec>?} synner)
    ;;Add a record representing a field specification to the appropriate
    ;;field in PARSED-SPEC.  Check for duplicate names in members.
    ;;
    (<parsed-spec>-member-identifiers-cons! parsed-spec (<field-spec>-name-id field-record)
					    "field name" synner)
    (<parsed-spec>-virtual-fields-cons! parsed-spec field-record))

  #| end of module |# )


;;;; more clause parsers

(define (clause-arguments-parser:satisfies parsed-spec args synner)
  ;;Parser function  for SATISFIES clauses;  this clause can  be present
  ;;any  number of  times and  can have  any number  of arguments.   The
  ;;expected syntax for the clause is:
  ;;
  ;;   (satisfies ?satisfaction-id ...)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?satisfaction-id ...) ...)
  ;;
  (vector-for-each
      (lambda (satisfaction-clause-stx)
	(vector-for-each
	    (lambda (satisfaction-id)
	      (if (identifier? satisfaction-id)
		  (<parsed-spec>-satisfactions-cons! parsed-spec satisfaction-id)
		(synner "expected identifier as satisfaction clause argument" satisfaction-id)))
	  satisfaction-clause-stx))
    args))

(define (clause-arguments-parser:getter parsed-spec args synner)
  ;;Parser function for  the GETTER clause; this clause  must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (getter ?transformer-expr)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?transformer-expr ))
  ;;
  ;;where  ?TRANSFORMER-EXPR is  an  expression evaluating  to a  getter
  ;;syntax function.
  ;;
  ($<parsed-spec>-getter-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:setter parsed-spec args synner)
  ;;Parser function for  the SETTER clause; this clause  must be present
  ;;at most once.  The expected syntax for the clause is:
  ;;
  ;;   (setter ?transformer-expr)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?transformer-expr ))
  ;;
  ;;where  ?TRANSFORMER-EXPR is  an  expression evaluating  to a  setter
  ;;syntax function.
  ;;
  ($<parsed-spec>-setter-set! parsed-spec ($vector-ref ($vector-ref args 0) 0)))

(define (clause-arguments-parser:mixins parsed-spec args synner)
  ;;Parser function  for the MIXINS  clause; this clause can  be present
  ;;any number of times and have  any number of arguments.  The expected
  ;;syntax for the clause is:
  ;;
  ;;   (mixins ?mixin-spec ...)
  ;;
  ;;and the corresponding ARGS is:
  ;;
  ;;   #(#( #'?mixin-spec ... ) ...)
  ;;
  ;;where ?MIXIN-SPEC has one of the forms:
  ;;
  ;;  ?mixin-name-id
  ;;  (?mixin-name-id (?from-id ?to-id) ...)
  ;;
  (vector-for-each
      (lambda (mixin-clause-stx)
	(vector-for-each
	    (lambda (mixin-spec-stx)
	      (syntax-case mixin-spec-stx ()
		(?mixin-name-id
		 (identifier? #'?mixin-name-id)
		 (<parsed-spec>-mixins-inclusions-cons! parsed-spec (list #'?mixin-name-id) synner))
		((?mixin-name-id (?from-id ?to-id) ...)
		 (and (identifier? #'?mixin-name-id)
		      (all-identifiers? #'(?from-id ...))
		      (all-identifiers? #'(?to-id   ...)))
		 (<parsed-spec>-mixins-inclusions-cons! parsed-spec (cons #'?mixin-name-id #'((?from-id ?to-id) ...))
							synner))
		(_
		 (synner "invalid inclusion mixin specification" mixin-spec-stx))))
	  mixin-clause-stx))
    args))


;;;; clause specifications

(module (CLASS-CLAUSES-SPECS
	 LABEL-CLAUSES-SPECS
	 MIXIN-CLAUSES-SPECS)

  ;;Remember the arguments of MAKE-SYNTAX-CLAUSE-SPEC:
  ;;
  ;; (make-syntax-clause-spec keyword
  ;;    min-occur max-occur
  ;;    min-args max-args
  ;;    mutually-inclusive mutually-exclusive
  ;;    custom-data)
  ;;

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-NONGENERATIVE
    (make-syntax-clause-spec #'aux.nongenerative	0 1 0 1 '() '()	clause-arguments-parser:nongenerative))

  (define-constant CLAUSE-SPEC-SEALED
    (make-syntax-clause-spec #'aux.sealed		0 1 1 1 '() '()	clause-arguments-parser:sealed))

  (define-constant CLAUSE-SPEC-OPAQUE
    (make-syntax-clause-spec #'aux.opaque		0 1 1 1 '() '()	clause-arguments-parser:opaque))

  (define-constant CLAUSE-SPEC-ABSTRACT
    (make-syntax-clause-spec #'aux.abstract		0 1 0 0 '() '()	clause-arguments-parser:abstract))

  (define-constant CLAUSE-SPEC-PARENT
    (make-syntax-clause-spec #'aux.parent		0 1 1 1 '() '()	clause-arguments-parser:parent))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-COMMON-PROTOCOL
    (make-syntax-clause-spec #'aux.protocol		0 1 1 1 '() '()	clause-arguments-parser:common-protocol))

  (define-constant CLAUSE-SPEC-PUBLIC-PROTOCOL
    (make-syntax-clause-spec #'aux.public-protocol	0 1 1 1 '() '()	clause-arguments-parser:public-protocol))

  (define-constant CLAUSE-SPEC-SUPER-PROTOCOL
    (make-syntax-clause-spec #'aux.super-protocol	0 1 1 1 '() '()	clause-arguments-parser:super-protocol))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-PREDICATE
    (make-syntax-clause-spec #'aux.predicate		0 1 1 1 '() '()	clause-arguments-parser:predicate))

  (define-constant CLAUSE-SPEC-MAKER
    (make-syntax-clause-spec #'aux.maker		0 1 1 1 '() '()	clause-arguments-parser:maker))

  (define-constant CLAUSE-SPEC-FINALISER
    (make-syntax-clause-spec #'aux.finaliser		0 1 1 1 '() '()	clause-arguments-parser:finaliser))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-CONCRETE-FIELDS
    (make-syntax-clause-spec #'aux.fields		0 +inf.0 0 +inf.0 '() '()	clause-arguments-parser:concrete-fields))

  (define-constant CLAUSE-SPEC-VIRTUAL-FIELDS
    (make-syntax-clause-spec #'aux.virtual-fields	0 +inf.0 0 +inf.0 '() '()	clause-arguments-parser:virtual-fields))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-SINGLE-METHOD
    (make-syntax-clause-spec #'aux.method		0 +inf.0 2 +inf.0 '() '()	clause-arguments-parser:single-method))

  (define-constant CLAUSE-SPEC-SINGLE-METHOD-SYNTAX
    (make-syntax-clause-spec #'aux.method-syntax	0 +inf.0 2 +inf.0 '() '()	clause-arguments-parser:method-syntax))

  (define-constant CLAUSE-SPEC-MULTIPLE-METHODS
    (make-syntax-clause-spec #'aux.methods		0 +inf.0 0 +inf.0 '() '()	clause-arguments-parser:multiple-methods))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-GETTER
    (make-syntax-clause-spec #'aux.getter		0 1 1 1 '() '()	clause-arguments-parser:getter))

  (define-constant CLAUSE-SPEC-SETTER
    (make-syntax-clause-spec #'aux.setter		0 1 1 1 '() '()	clause-arguments-parser:setter))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC-SATISFIES
    (make-syntax-clause-spec #'aux.satisfies		0 +inf.0 0 +inf.0 '() '()	clause-arguments-parser:satisfies))

  (define-constant CLAUSE-SPEC-SHADOWS
    (make-syntax-clause-spec #'aux.shadows		0 1 1 1 '() '()	clause-arguments-parser:shadows))

  (define-constant CLAUSE-SPEC-MIXINS
    (make-syntax-clause-spec #'aux.mixins		0 +inf.0 0 +inf.0 '() '()	clause-arguments-parser:mixins))

;;; --------------------------------------------------------------------

  ;;Parser functions for the clauses of the DEFINE-LABEL syntax.
  ;;
  (define-constant LABEL-CLAUSES-SPECS
    (syntax-clauses-validate-specs
     (list CLAUSE-SPEC-SINGLE-METHOD
	   CLAUSE-SPEC-VIRTUAL-FIELDS
	   CLAUSE-SPEC-MULTIPLE-METHODS
	   CLAUSE-SPEC-SINGLE-METHOD-SYNTAX
	   CLAUSE-SPEC-COMMON-PROTOCOL
	   CLAUSE-SPEC-PUBLIC-PROTOCOL
	   CLAUSE-SPEC-PREDICATE
	   CLAUSE-SPEC-GETTER
	   CLAUSE-SPEC-SETTER
	   CLAUSE-SPEC-PARENT
	   CLAUSE-SPEC-NONGENERATIVE
	   CLAUSE-SPEC-MAKER
	   CLAUSE-SPEC-SHADOWS
	   CLAUSE-SPEC-SATISFIES
	   CLAUSE-SPEC-MIXINS)))

  ;;Parser functions for the clauses of the DEFINE-class syntax.
  ;;
  (define-constant CLASS-CLAUSES-SPECS
    (syntax-clauses-validate-specs
     (list
      CLAUSE-SPEC-SINGLE-METHOD
      CLAUSE-SPEC-CONCRETE-FIELDS
      CLAUSE-SPEC-VIRTUAL-FIELDS
      CLAUSE-SPEC-MULTIPLE-METHODS
      CLAUSE-SPEC-SINGLE-METHOD-SYNTAX
      CLAUSE-SPEC-NONGENERATIVE
      CLAUSE-SPEC-PARENT
      CLAUSE-SPEC-COMMON-PROTOCOL
      CLAUSE-SPEC-PUBLIC-PROTOCOL
      CLAUSE-SPEC-SUPER-PROTOCOL
      CLAUSE-SPEC-GETTER
      CLAUSE-SPEC-SETTER
      CLAUSE-SPEC-MAKER
      CLAUSE-SPEC-FINALISER
      CLAUSE-SPEC-SATISFIES
      CLAUSE-SPEC-SEALED
      CLAUSE-SPEC-OPAQUE
      CLAUSE-SPEC-ABSTRACT
      CLAUSE-SPEC-MIXINS)))

  ;;Parser functions for the clauses of the DEFINE-MIXIN syntax.
  ;;
  (define-constant MIXIN-CLAUSES-SPECS
    (syntax-clauses-validate-specs
     (list
      CLAUSE-SPEC-SINGLE-METHOD
      CLAUSE-SPEC-CONCRETE-FIELDS
      CLAUSE-SPEC-VIRTUAL-FIELDS
      CLAUSE-SPEC-MULTIPLE-METHODS
      CLAUSE-SPEC-PARENT
      CLAUSE-SPEC-COMMON-PROTOCOL
      CLAUSE-SPEC-PUBLIC-PROTOCOL
      CLAUSE-SPEC-GETTER
      CLAUSE-SPEC-SETTER
      CLAUSE-SPEC-NONGENERATIVE
      CLAUSE-SPEC-MAKER
      CLAUSE-SPEC-FINALISER
      CLAUSE-SPEC-ABSTRACT
      CLAUSE-SPEC-OPAQUE
      CLAUSE-SPEC-PREDICATE
      CLAUSE-SPEC-SATISFIES
      CLAUSE-SPEC-SEALED
      CLAUSE-SPEC-SHADOWS
      CLAUSE-SPEC-SINGLE-METHOD-SYNTAX
      CLAUSE-SPEC-SUPER-PROTOCOL
      CLAUSE-SPEC-MIXINS)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
