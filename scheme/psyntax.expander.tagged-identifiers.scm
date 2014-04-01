;;;Copyright (c) 2010-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
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


;;;; basic typed language concepts
;;
;;Tag identifiers
;;---------------
;;
;;A "tag identifier" is a bound identifier whose syntactic binding label gensym has a
;;specific   entry  in   its  property   list;  such   entry  has   an  instance   of
;;"object-type-spec" as value.  Tag identifiers must  be bound (otherwise they do not
;;have a  syntactic binding label), but  it does not  matter to what they  are bound.
;;Typical examples of tag identifiers are:
;;
;;* Struct type identifiers defined by DEFINE-STRUCT; they are automatically made tag
;;  identifiers by Vicare.
;;
;;*  R6RS   record  type   identifiers  defined   by  DEFINE-RECORD-TYPE;   they  are
;;  automatically made tag identifiers by Vicare.
;;
;;* A  set of non-core macro  identifiers (whose implementation is  integrated in the
;;  expander) are exported by the library "(vicare  expander tags)" to be the tags of
;;  built-in Vicare objects.   Some of them are:  "<fixnum>", "<string>", "<vector>",
;;  "<textual-input-port>".
;;
;;We can easily create a tag identifier as:
#|
     (import (vicare)
       (vicare expander tags)
       (for (prefix (vicare expander object-type-specs) typ.)
         expand))

     (define-syntax <my-tag>
       (let ()
         (set-identifier-tag! #'<my-tag>
           (make-object-type-spec #'<my-tag> #'<top> ...))
         (lambda (x) #f)))
|#
;;in which "<top>" is used as parent tag.
;;
;;
;;Tagged binding
;;--------------
;;
;;A "tagged binding" is a bound identifier whose syntactic binding label gensym has a
;;specific entry  in its  property list; such  entry has a  tag identifier  as value.
;;Tagged identifiers  must be bound (otherwise  they do not have  a syntactic binding
;;label).   Tagged bindings  are created  by  the built-in  binding syntaxes  LAMBDA,
;;DEFINE, LET, LETREC, LET-VALUES, etc.
;;
;;An example of tagged binding creation follows:
#|
     #!vicare
     (import (vicare)
       (vicare expander tags))

     (define {O <fixnum>}
       123)
|#
;;the braces are used to tag the first identifier with the second identifier.  At the
;;time  the tagged  binding is  created: the  tag identifier  must already  be a  tag
;;identifier.
;;


;;;; formals, retvals, signatures predicates

(define (standard-formals-syntax? stx)
  ;;Return  true if  STX is  a syntax  object representing  R6RS standard  LAMBDA and
  ;;LET-VALUES formals; otherwise return false.  The return value is true if STX is a
  ;;proper or  improper list of  identifiers, with  null and a  standalone identifier
  ;;being acceptable.  Examples:
  ;;
  ;;   (standard-formals-syntax #'args)		=> #t
  ;;   (standard-formals-syntax #'())		=> #t
  ;;   (standard-formals-syntax #'(a b))	=> #t
  ;;   (standard-formals-syntax #'(a b . rest))	=> #t
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (identifier? ?id)
     (standard-formals-syntax? ?rest))
    (?rest
     (identifier? ?rest)
     #t)
    (_ #f)))

(define (formals-signature-syntax? stx)
  ;;Return true if STX is a syntax  object representing the tag signature of a tagged
  ;;formals syntax;  otherwise return false.   The return value is  true if STX  is a
  ;;proper  or improper  list of  tag  identifiers, with  null and  a standalone  tag
  ;;identifier being acceptable.  Examples:
  ;;
  ;;   (formals-signature-syntax #'<list>)				=> #t
  ;;   (formals-signature-syntax #'())					=> #t
  ;;   (formals-signature-syntax #'(<fixnum> <string>))			=> #t
  ;;   (formals-signature-syntax #'(<fixnum> <string> . <list>))	=> #t
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (tag-identifier? ?id)
     (formals-signature-syntax? ?rest))
    (?rest
     (tag-identifier? ?rest)
     #t)
    (_ #f)))

(define (retvals-signature-syntax? stx)
  ;;Return true if  STX is a syntax  object representing the tag  signature of tagged
  ;;return values; otherwise return  false.  The return value is true  if STX is null
  ;;or a proper or improper list of tag identifiers, with a standalone tag identifier
  ;;being acceptable.  Examples:
  ;;
  ;;   (retvals-signature-syntax #'<list>)				=> #t
  ;;   (retvals-signature-syntax #'())					=> #t
  ;;   (retvals-signature-syntax #'(<fixnum> <string>))			=> #t
  ;;   (retvals-signature-syntax #'(<fixnum> <string> . <list>))	=> #t
  ;;
  (or (not (syntax->datum stx))
      (let loop ((stx stx))
	(syntax-match stx ()
	  (() #t)
	  ((?id . ?rest)
	   (tag-identifier? ?id)
	   (loop ?rest))
	  (?rest
	   (tag-identifier? ?rest)
	   #t)
	  (_ #f)))))

;;; --------------------------------------------------------------------

(define* (formals-signature-partially-untagged-syntax? {stx formals-signature-syntax?})
  ;;The argument STX must be a syntax object representing a formals signature syntax:
  ;;null or a proper  or improper list of tag identifiers.  Return true  if STX as at
  ;;least one "<untagged>" tag identifier; otherwise return false.
  ;;
  ($formals-signature-partially-untagged-syntax? stx))

(define ($formals-signature-partially-untagged-syntax? stx)
  (syntax-match stx ()
    (() #f)
    ((?id . ?rest)
     (tag-identifier? ?id)
     (or ($untagged-tag-id? ?id)
	 ($formals-signature-partially-untagged-syntax? ?rest)))
    (?rest
     (tag-identifier? ?rest)
     ($untagged-tag-id? ?rest))))

(define* (retvals-signature-partially-unspecified-syntax? {stx retvals-signature-syntax?})
  ;;The argument STX must be a syntax object representing a retvals signature syntax:
  ;;null or a proper or improper list of  tag identifiers.  Return true if STX if the
  ;;signature as at least one "<untagged>" tag identifier; otherwise return false.
  ;;
  ($retvals-signature-partially-unspecified-syntax? stx))

(define $retvals-signature-partially-unspecified-syntax?
  $formals-signature-partially-untagged-syntax?)

;;; --------------------------------------------------------------------

(define* (formals-signature-super-and-sub-syntax? {super-signature formals-signature-syntax?}
						  {sub-signature   formals-signature-syntax?})
  ;;Return  true  if the  super  signature  and  the  sub signature  have  compatible
  ;;structure and the  tags from the super  signature are supertags of  the tags from
  ;;the sub signature; otherwise return false.
  ;;
  ;;This function can be used to determine:  if a tuple of arguments matches a lambda
  ;;formals's signaure; if a tuple or return values matches the receiver signature.
  ;;
  ($formals-signature-super-and-sub-syntax? super-signature sub-signature))

(define ($formals-signature-super-and-sub-syntax? super-signature sub-signature)
  (syntax-match super-signature ()
    (()
     (syntax-match sub-signature ()
       ;;Both the signatures are proper lists with  the same number of items, and all
       ;;the items are correct super and sub: success!
       (() #t)
       ;;The signature do not match.
       (_  #f)))

    ((?super-tag . ?super-rest-tags)
     (syntax-match sub-signature ()
       ((?sub-tag . ?sub-rest-tags)
	(and (or ($untagged-tag-id? ?super-tag)
		 ($tag-super-and-sub? ?super-tag ?sub-tag))
	     ($formals-signature-super-and-sub-syntax? ?super-rest-tags ?sub-rest-tags)))
       (_ #f)))

    (?super-rest-tag
     (syntax-match sub-signature ()
       ;;The super signature is an improper list with rest item and the sub signature
       ;;is finished.  We want the following signatures to match:
       ;;
       ;;  #'(<number> <fixnum> . <top>)      #'(<complex> <fixnum>)
       ;;  #'(<number> <fixnum> . <list>)     #'(<complex> <fixnum>)
       ;;  #'(<number> <fixnum> . <untagged>) #'(<complex> <fixnum>)
       ;;
       ;;because "<top>", "<list>" and "<untagged>"  in rest position mean any number
       ;;of objects of any type.
       (()
	(or ($untagged-tag-id? ?super-rest-tag)
            ($top-tag-id?      ?super-rest-tag)
	    ($tag-super-and-sub? (list-tag-id) ?super-rest-tag)))

       ;;The super signature is an improper  list shorter than the sub signature.  We
       ;;want the following signatures to match:
       ;;
       ;;  #'(<number> . <top>)   #'(<complex> <fixnum> <fixnum>)
       ;;  #'(<number> . <list>)  #'(<complex> <fixnum> <fixnum>)
       ;;
       ;;because "<top>", "<list>" and "<untagged>"  in rest position mean any number
       ;;of objects of any type.
       ((?sub-tag . ?sub-rest-tags)
	(or ($untagged-tag-id? ?super-rest-tag)
            ($top-tag-id?      ?super-rest-tag)
	    ($tag-super-and-sub? (list-tag-id) ?super-rest-tag)))

       ;;Both the  signatures are improper lists  with the same number  of items, and
       ;;all the items are  correct super and sub; if the rest  tags are proper super
       ;;and subs: success!
       (?sub-rest-tag
	(identifier? ?sub-rest-tag)
	($tag-super-and-sub? ?super-rest-tag ?sub-rest-tag))

       ;;Everything else is wrong.
       (_ #f)))

    ;;Everything else is wrong.
    (_ #f)))

(define* (retvals-signature-super-and-sub-syntax? {super-signature retvals-signature-syntax?}
						  {sub-signature   retvals-signature-syntax?})
  ;;Return  true  if the  super  signature  and  the  sub signature  have  compatible
  ;;structure and the  tags from the super  signature are supertags of  the tags from
  ;;the sub signature; otherwise return false.
  ;;
  ;;This function can be used to determine:  if a tuple of arguments matches a lambda
  ;;retvals's signaure; if a tuple or return values matches the receiver signature.
  ;;
  ($retvals-signature-super-and-sub-syntax? super-signature sub-signature))

(define $retvals-signature-super-and-sub-syntax?
  $formals-signature-super-and-sub-syntax?)


;;;; callable syntax, callable signature, return values signature, formals signature
;;
;;A "callable  spec" is a syntax  object representing the name,  arguments and return
;;values of a callable object: a function or macro.  A callable can be used as:
;;
;;   (?callable ?arg ...)
;;
;;but not as:
;;
;;   (apply ?callable ?arg ...)
;;
;;A "callable  signature" is an object  representing the number and  tags of callable
;;arguments  and return  values.  A  "return values  signature" is  false, null  or a
;;proper or  improper list  of tag  identifiers representing the  number and  tags of
;;return values;  by convention,  when the number  and tags of  return values  is not
;;known, #f is used.   A "formals signature" is null or a proper  or improper list of
;;tag identifiers representing the number and  tags of callable arguments; null and a
;;standalone tag identifier are valid.
;;
;;Let's  state the  format of  a  callable spec.   We use  the conventions:  ?ARG-ID,
;;?REST-ID  and  ?ARGS-ID are  argument  identifiers;  ?TAG  and  ?RV-TAG are  a  tag
;;identifiers.  We accept the following standard R6RS formals:
;;
;;   ?args-id
;;   (?arg-id ...)
;;   (?arg-id0 ?arg-id ... . ?rest-id)
;;
;;and in addition the following tagged specs:
;;
;;   (brace ?args-id ?args-tag)
;;   (?arg ...)
;;   (?arg0 ?arg ... . ?rest-arg)
;;   (?retvals ?arg ...)
;;   (?retvals ?arg0 ?arg ... . ?rest-arg)
;;
;;where ?ARG is a tagged argument with one of the formats:
;;
;;   ?arg-id
;;   (brace ?arg-id ?arg-tag)
;;
;;RETVALS is a special syntax that allows the specification of the number and tags of
;;expression return values; it has one of the formats:
;;
;;   (brace _ ?rv-tag ...)
;;   (brace _ ?rv-tag ... . ?rv-rest-tag)
;;
;;where the identifier "_" is the binding exported by (vicare).
;;
;;The number  of ?RV-TAG identifiers specifies  the number of return  values; when no
;;?RV-TAG is present: the callable returns no values.
;;

(define-record (retvals-signature %make-retvals-signature retvals-signature?)
  (tags
		;False,  null  or  a  proper  or improper  list  of  tag  identifiers
		;representing the object types of a tuple of return values.  False is
		;used when the  number and type of returned values  is unknown.  Null
		;is used when the number of returned values is zero.
   ))

(define-record (formals-signature %make-formals-signature formals-signature?)
  (tags
		;Null or  a proper or  improper list of tag  identifiers representing
		;the object types of a tuple of LET-VALUES formals.
   ))

(define-record (lambda-signature %make-lambda-signature lambda-signature?)
  (retvals
		;An instance of "retvals-signature".
   formals
		;An instance of "formals-signature".
   ))

(define-record (clambda-compound %make-clambda-compound clambda-compound?)
  (common-retvals-signature
		;An instance of "retvals-signature".   It represents the signature of
		;the common retvals from all  the clambda clauses represented by this
		;struct.  For example:
		;
		;   (case-lambda
		;    (({_ <fixnum>}) . ?body)
		;     ({_ <fixnum>}) . ?body)))
		;
		;has common retvals "(<fixnum>)", while:
		;
		;   (case-lambda
		;    (({_ <fixnum>}) . ?body)
		;     ({_ <bignum>}) . ?body)))
		;
		;has common retvals "(<exact-integer>)".  When  it is not possible to
		;determine  a   common  retvals  signature:  the   default  value  is
		;"<untagged>", which means any number of objects of any type.
		;
   lambda-signatures
		;A  proper  list  of "lambda-signature"  instances  representing  the
		;signatures of the CASE-LAMBDA clauses.
   ))

(define* (make-formals-signature {tags formals-signature-syntax?})
  (%make-formals-signature tags))

(define* (make-retvals-signature {tags retvals-signature-syntax?})
  (%make-retvals-signature tags))

(define* (make-lambda-signature {rv retvals-signature?} {formals formals-signature?})
  (%make-lambda-signature rv formals))

(define* (make-clambda-compound {signatures list-of-lambda-signatures?})
  (%make-clambda-compound (make-fully-unspecified-retvals-signature) signatures))

;;; --------------------------------------------------------------------
;;; special constructors

(define (make-fully-unspecified-retvals-signature)
  (%make-retvals-signature (untagged-tag-id)))

;;; --------------------------------------------------------------------
;;; special accessors

(define* (lambda-signature-formals-tags {signature lambda-signature?})
  ($formals-signature-tags ($lambda-signature-formals signature)))

(define* (lambda-signature-retvals-tags {signature lambda-signature?})
  ($retvals-signature-tags ($lambda-signature-retvals signature)))

;;; --------------------------------------------------------------------

(define (list-of-lambda-signatures? obj)
  (and (list? obj)
       (for-all lambda-signature? obj)))

;;; --------------------------------------------------------------------

(define* (retvals-signature-fully-unspecified? {signature retvals-signature?})
  (untagged-tag-id? ($retvals-signature-tags signature)))

(define* (retvals-signature-partially-unspecified? {signature retvals-signature?})
  ($retvals-signature-partially-unspecified-syntax? ($retvals-signature-tags signature)))

(define* (retvals-signature-super-and-sub? {super-signature retvals-signature?}
					   {sub-signature   retvals-signature?})
  ($retvals-signature-super-and-sub-syntax? ($retvals-signature-tags super-signature)
					    ($retvals-signature-tags sub-signature)))

(define* (retvals-signature-single-tag? {signature retvals-signature?})
  (let ((tags ($retvals-signature-tags signature)))
    (and (pair? tags)
	 (null? ($cdr tags)))))

(define* (retvals-signature-single-tag-or-fully-unspecified? {signature retvals-signature?})
  (let ((tags ($retvals-signature-tags signature)))
    (or (untagged-tag-id? tags)
	(and (pair? tags)
	     (null? ($cdr tags))))))

;;; --------------------------------------------------------------------

(define* (lambda-signature=? {signature1 lambda-signature?} {signature2 lambda-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (and (syntax=? ($formals-signature-tags ($lambda-signature-formals signature1))
		 ($formals-signature-tags ($lambda-signature-formals signature2)))
       (syntax=? ($retvals-signature-tags ($lambda-signature-retvals signature1))
		 ($retvals-signature-tags ($lambda-signature-retvals signature2)))))

(define* (formals-signature=? {signature1 formals-signature?} {signature2 formals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? ($formals-signature-tags signature1)
	    ($formals-signature-tags signature2)))

(define* (retvals-signature=? {signature1 retvals-signature?} {signature2 retvals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? ($retvals-signature-tags signature1)
	    ($retvals-signature-tags signature2)))


;;;; tagged binding parsing: standalone identifiers

(define (tagged-identifier-syntax? stx)
  ;;Return  true if  STX is  a syntax  object representing  a tagged  or
  ;;untagged identifier, otherwise return false.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (tag-identifier? ?tag)))
    (?id
     (identifier? ?id))
    ))

(define (parse-tagged-identifier-syntax stx)
  ;;If  STX is  a tagged  or  untagged identifier,  return 2  values: the  identifier
  ;;representing the binding name and  the identifier representing the tag; otherwise
  ;;raise  an exception.   When no  tag is  present: the  tag identifier  defaults to
  ;;"<untagged>".
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (begin
       (assert-tag-identifier? ?tag)
       (values ?id ?tag)))
    (?id
     (identifier? ?id)
     (values ?id (untagged-tag-id)))
    ))


;;;; tagged binding parsing: proper lists of bindings left-hand sides
;;
;;The predicate and parser functions for lists of bindings are used to parse bindings
;;from LET, DO and similar syntaxes.  For example, when expanding the syntax:
;;
;;   (let (({a <fixnum>} 1)
;;         ({b <string>} "b")
;;         (c            #t))
;;     . ?body)
;;
;;the argument STX is:
;;
;;   (#'(brace a <fixnum>) #'(brace b <string>) #'c)
;;
;;and the return values are:
;;
;;   (#'a #'b #'c) (#'<fixnum> #'<string> #'<untagged>)
;;

(case-define* parse-list-of-tagged-bindings
  ((stx)
   (parse-list-of-tagged-bindings stx #f))
  ((stx input-form.stx)
   ;;Assume STX  is a  syntax object  representing a proper  list of  possibly tagged
   ;;binding identifiers; parse  the list and return 2 values:  a list of identifiers
   ;;representing the  binding identifiers,  a list  of identifiers  representing the
   ;;type tags; "<untagged>" is used when no tag is present.  The identifiers must be
   ;;distinct.
   ;;
   (define (%invalid-tagged-bindings-syntax form subform)
     (syntax-violation __who__ "invalid tagged bindings syntax" form subform))
   (define (%duplicate-identifiers-in-bindings-specification form subform)
     (syntax-violation __who__
       "duplicate identifiers in bindings specification" form subform))
   (receive-and-return (id* tag*)
       (let recur ((bind* stx))
	 (syntax-match bind* (brace)
	   (()
	    (values '() '()))
	   (((brace ?id ?tag) . ?other-id*)
	    (begin
	      (assert-tag-identifier? ?tag)
	      (receive (id* tag*)
		  (recur ?other-id*)
		(values (cons ?id id*) (cons ?tag tag*)))))
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (receive (id* tag*)
		(recur ?other-id*)
	      (values (cons ?id id*) (cons (untagged-tag-id) tag*))))
	   (_
	    (if input-form.stx
		(%invalid-tagged-bindings-syntax input-form.stx stx)
	      (%invalid-tagged-bindings-syntax stx #f)))
	   ))
     (unless (distinct-bound-ids? id*)
       (if input-form.stx
	   (%duplicate-identifiers-in-bindings-specification input-form.stx stx)
	 (%duplicate-identifiers-in-bindings-specification stx #f))))))

(define* (list-of-tagged-bindings? lhs*)
  ;;Return  true if  lhs*  is  a list  of  possibly tagged  identifiers;
  ;;otherwise return false.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (id* tag*)
	(parse-list-of-tagged-bindings lhs*)
      #t)))


;;;; tagged binding parsing: let-values formals

(module (parse-tagged-formals-syntax)
  ;;Given a syntax object representing  tagged LET-VALUES formals: split formals from
  ;;tags.  Do test for duplicate bindings.
  ;;
  ;;Return 2 values:
  ;;
  ;;1..A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2..An object representing the LET-VALUES tagging signature.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'parse-tagged-formals-syntax))

  (case-define* parse-tagged-formals-syntax
    (({_ standard-formals-syntax? formals-signature?} original-formals.stx)
     (parse-tagged-formals-syntax original-formals.stx #f))
    (({_ standard-formals-syntax? formals-signature?} original-formals.stx input-form.stx)
     (receive (standard-formals formals-tags)
	 (%parse-formals input-form.stx original-formals.stx original-formals.stx)
       (values standard-formals (make-formals-signature formals-tags)))))

  (define (%parse-formals input-form.stx original-formals.stx formals.stx)
    (syntax-match formals.stx (brace)

      ;;Tagged args, as in:
      ;;
      ;;   (let-values (({args list-of-fixnums} ?expr)) . ?body)
      ;;
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (begin
	 (assert-tag-identifier? ?args-tag)
	 (values ?args-id ?args-tag)))

      ;;Possibly tagged identifiers with tagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . {rest list-of-fixnums}) ?expr)) . ?body)
      ;;
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (begin
	 (unless (and (identifier? ?rest-id)
		      (identifier? ?rest-tag))
	   (syntax-violation __who__
	     "invalid rest argument specification" original-formals.stx (cons 'brace ?rest-id ?rest-tag)))
	 (assert-tag-identifier? ?rest-tag)
	 (receive-and-return (standard-formals.stx tags)
	     (let recur ((?arg* ?arg*))
	       (if (pair? ?arg*)
		   (%process-args input-form.stx original-formals.stx recur ?arg*)
		 ;;Process rest argument.
		 (values ?rest-id ?rest-tag)))
	   (%validate-formals input-form.stx original-formals.stx standard-formals.stx))))

      ;;Possibly tagged identifiers with UNtagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . rest) ?expr)) . ?body)
      ;;
      ((?arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (receive-and-return (standard-formals.stx tags)
	   (let recur ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-args input-form.stx original-formals.stx recur ?arg*)
	       (values ?rest-id (untagged-tag-id))))
	 (%validate-formals input-form.stx original-formals.stx standard-formals.stx)))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (begin
	 (%validate-formals input-form.stx original-formals.stx ?id*)
	 (values ?id* (map (lambda (id) (untagged-tag-id)) ?id*))))

      ;;Standard formals: untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-formals input-form.stx original-formals.stx (append ?id* ?rest-id))
	 (values formals.stx (cons* (map (lambda (id) (untagged-tag-id)) ?id*) (untagged-tag-id)))))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (untagged-tag-id)))

      ;;Possibly tagged identifiers without rest argument.
      ;;
      ((?arg* ...)
       (receive-and-return (standard-formals.stx tags)
	   (let recur ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-args input-form.stx original-formals.stx recur ?arg*)
	       (values '() '())))
	 (%validate-formals input-form.stx original-formals.stx standard-formals.stx)))
      ))

  (define (%process-args input-form.stx original-formals.stx recur args-stx)
    (receive (standard-formals tags)
	(recur (cdr args-stx))
      (let ((arg-stx (car args-stx)))
	(syntax-match arg-stx (brace)
	  ;;Untagged argument.
	  (?id
	   (identifier? ?id)
	   (values (cons ?id standard-formals) (cons (untagged-tag-id) tags)))
	  ;;Tagged argument.
	  ((brace ?id ?tag)
	   (and (identifier? ?id)
		(identifier? ?tag))
	   (begin
	     (assert-tag-identifier? ?tag)
	     (values (cons ?id standard-formals) (cons ?tag tags))))
	  (else
	   (syntax-violation __who__
	     "invalid argument specification"
	     (or input-form.stx original-formals.stx) arg-stx))))))

  (define (%validate-formals input-form.stx original-formals.stx standard-formals.stx)
    (cond ((duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(syntax-violation __who__
		  "duplicate identifiers in formals specification"
		  (or input-form.stx original-formals.stx)
		  duplicate-id)))))

  #| end of module |# )

(define* (tagged-formals-syntax? formals.stx)
  ;;Return true if  FORMALS.STX is a syntax object representing  valid tagged formals
  ;;for a LET-VALUES syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature)
	(parse-tagged-formals-syntax formals.stx)
      #t)))


;;;; tagged binding parsing: callable signature

(case-define* parse-tagged-lambda-proto-syntax
  ;;Given a  syntax object representing  a tagged  callable spec: split  the standard
  ;;formals from the tags; do test for duplicate bindings.  Return 2 values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2. An instance of "lambda-signature".
  ;;
  (({_ standard-formals-syntax? lambda-signature?} {callable-spec.stx syntax-object?})
   (parse-tagged-lambda-proto-syntax callable-spec.stx #f))
  (({_ standard-formals-syntax? lambda-signature?} {callable-spec.stx syntax-object?} {input-form.stx syntax-object?})
   ;;First we parse  and extract the return  values tagging, if any;  then we parse
   ;;the rest of the formals.
   (syntax-match callable-spec.stx (brace)
     ;;With return values tagging.
     (((brace ?who ?rv-tag* ... . ?rv-rest-tag) . ?formals)
      (underscore-id? ?who)
      (let ((retvals.stx (append ?rv-tag* ?rv-rest-tag)))
	(unless (retvals-signature-syntax? retvals.stx)
	  (syntax-violation __who__
	    "invalid return values signature syntax" input-form.stx retvals.stx))
	(receive (standard-formals.stx formals-signature)
	    (parse-tagged-formals-syntax ?formals input-form.stx)
	  (values standard-formals.stx
		  (make-lambda-signature (make-retvals-signature retvals.stx) formals-signature)))))
     ;;Without return values tagging.
     (?formals
      (receive (standard-formals.stx formals-signature)
	  (parse-tagged-formals-syntax ?formals input-form.stx)
	(values standard-formals.stx
		(make-lambda-signature (make-retvals-signature (untagged-tag-id)) formals-signature)))))))

(define* (tagged-lambda-proto-syntax? formals-stx)
  ;;Return true if  FORMALS-STX is a syntax object representing  valid tagged formals
  ;;for a LAMBDA syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature-tags)
	(parse-tagged-lambda-proto-syntax formals-stx)
      #t)))


;;;; expand-time object type specification

(define-record (object-type-spec %make-object-type-spec object-type-spec?)
  ;;A type representing  the object type to which expressions  in syntax objects will
  ;;evaluate.  All the Scheme objects are meant to be representable with this type.
  ;;
  (uids
		;A non-empty proper list of  symbols uniquely identifying this object
		;type  specification.    The  first  symbol  in   the  list  uniquely
		;identifies this record instance.
   type-id
		;The  bound identifier  representing  the name  of  this type.   This
		;identifier has this very instance  in its syntactic binding property
		;list.
   pred-stx
		;A syntax  object (wrapped  or unwrapped) representing  an expression
		;which will evaluate to a type predicate.
   accessor-maker
		;False or an accessor maker procedure.
   mutator-maker
		;False or a mutator maker procedure.
   getter-maker
		;False or a getter maker procedure.
   setter-maker
		;False or a setter maker procedure.
   caster-maker
		;False or a caster maker procedure.
   dispatcher
		;False or a method dispatcher procedure.
   parent-spec
		;False or an instance of  "object-type-spec" describing the parent of
		;this type.   Only "<top>"  and "<untagged>" have  this field  set to
		;false; every other "object-type-spec" has a parent spec.  "<top>" is
		;the implicit parent of all the  type specs.  "<untagged>" is the tag
		;of untagged bindings.
   ))

(case-define* make-object-type-spec
  (({uid	symbol?}
    {type-id	identifier-bound?}
    {parent-id	tag-identifier?}
    {pred-stx	syntax-object?})
   (when (free-id=? parent-id (untagged-tag-id))
     (procedure-argument-violation __who__
       "<untagged> cannot be a parent tag" uid type-id))
   (let* ((parent-spec (identifier-object-type-spec parent-id))
	  (uids        (list uid (object-type-spec-uids parent-spec))))
     (%make-object-type-spec uids type-id pred-stx
			     #f ;accessor-maker
			     #f ;mutator-maker
			     #f ;getter-maker
			     #f ;setter-maker
			     #f ;cast-maker
			     #f ;method-dispatcher
			     parent-spec)))

  (({uid	symbol?}
    {type-id	identifier-bound?}
    {parent-id	tag-identifier?}
    {pred-stx	syntax-object?}
    {accessor	false-or-procedure?}
    {mutator	false-or-procedure?}
    {getter	false-or-procedure?}
    {setter	false-or-procedure?}
    {caster	false-or-procedure?}
    {dispatcher	false-or-procedure?})
   (when (free-id=? parent-id (untagged-tag-id))
     (procedure-argument-violation __who__
       "<untagged> cannot be a parent tag" uid type-id))
   (let* ((parent-spec (identifier-object-type-spec parent-id))
	  (uids        (list uid (object-type-spec-uids parent-spec))))
     (%make-object-type-spec uids type-id pred-stx
			     accessor mutator getter setter caster dispatcher parent-spec))))

(define (false-or-object-type-spec? obj)
  (or (not obj)
      (object-type-spec? obj)))


;;;; object type specification queries

(case-define* tag-identifier-predicate
  ;;Given  a tag  identifier:  retrieve from  the  associated "object-type-spec"  the
  ;;predicate syntax object.   If successful: return a syntax  object representing an
  ;;expression  which,  expanded  by  itself  and  evaluated,  will  return  the  tag
  ;;predicate.
  ;;
  ((tag-id)
   (tag-identifier-predicate tag-id #f))
  (({tag-id tag-identifier?} input-form.stx)
   (cond ((identifier-object-type-spec tag-id)
	  => (lambda (spec)
	       (or (object-type-spec-pred-stx spec)
		   ;;This   should    never   happen    because   an    instance   of
		   ;;"object-type-spec" always has a defined predicate.
		   (syntax-violation __who__
		     "internal error: undefined tag predicate" input-form.stx tag-id))))
	 (else
	  ;;This should never happen because we  have validated the identifier in the
	  ;;fender.
	  (syntax-violation __who__
	    "internal error: tag identifier without object-type-spec" input-form.stx tag-id)))))

(case-define* tag-identifier-accessor
  ;;Given   a  tag   identifier  and   a  field   name:  search   the  hierarchy   of
  ;;"object-type-spec" associated  to TAG-ID for  an accessor of the  selected field.
  ;;If successful: return a syntax  object representing an expression which, expanded
  ;;by itself and evaluated, will return the field accessor; if no accessor is found:
  ;;raise an exception.
  ;;
  ((tag-id field-name-id)
   (tag-identifier-accessor tag-id field-name-id #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} input-form.stx)
   (let loop ((spec ($identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;object-type-specs  until an  object-type-spec without  parent has  been
	    ;;found.  The serach for the field accessor has failed.
	    (syntax-violation __who__
	      "object type does not provide selected field accessor"
	      input-form.stx field-name-id))
	   (($object-type-spec-accessor-maker spec)
	    => (lambda (accessor-maker)
		 (or (accessor-maker (syntax->datum field-name-id) input-form.stx)
		     ;;The field is unknown: try with the parent.
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no accessor maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-mutator
  ;;Given   a  tag   identifier  and   a  field   name:  search   the  hierarchy   of
  ;;"object-type-spec" associated to TAG-ID for an mutator of the selected field.  If
  ;;successful: return a syntax object  representing an expression which, expanded by
  ;;itself and  evaluated, will  return the  field mutator; if  no mutator  is found:
  ;;raise an exception.
  ;;
  ((tag-id field-name-id)
   (tag-identifier-mutator tag-id field-name-id #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} input-form.stx)
   (let loop ((spec ($identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field mutator has failed.
	    (syntax-violation __who__
	      "object type does not provide selected field mutator"
	      input-form.stx field-name-id))
	   (($object-type-spec-mutator-maker spec)
	    => (lambda (mutator-maker)
		 (or (mutator-maker (syntax->datum field-name-id) input-form.stx)
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no mutator maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-getter
  ;;Given  a   tag  identifier  and   a  set  of   keys:  search  the   hierarchy  of
  ;;"object-type-spec"  associated to  TAG-ID for  a getter  accepting the  keys.  If
  ;;successful: return a syntax object  representing an expression which, expanded by
  ;;itself and  evaluated, will return  the getter; if no  getter is found:  raise an
  ;;exception.
  ;;
  ((tag-id keys.stx)
   (tag-identifier-getter tag-id keys.stx #f))
  (({tag-id tag-identifier?} {keys.stx syntax-object?} input-form.stx)
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field getter has failed.
	    (syntax-violation __who__
	      "object type does not provide getter syntax" input-form.stx))
	   (($object-type-spec-getter-maker spec)
	    => (lambda (getter-maker)
		 (or (getter-maker keys.stx input-form.stx)
		     ;;The keys are unknown: try with the parent.
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no getter maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-setter
  ;;Given  a   tag  identifier  and   a  set  of   keys:  search  the   hierarchy  of
  ;;"object-type-spec"  associated to  TAG-ID for  a setter  accepting the  keys.  If
  ;;successful: return a syntax object  representing an expression which, expanded by
  ;;itself and  evaluated, will return  the setter; if no  setter is found:  raise an
  ;;exception.
  ;;
  ((tag-id keys.stx)
   (tag-identifier-setter tag-id keys.stx #f))
  (({tag-id tag-identifier?} {keys.stx syntax-object?} input-form.stx)
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field setter has failed.
	    (syntax-violation __who__
	      "object type does not provide setter syntax" input-form.stx))
	   (($object-type-spec-setter-maker spec)
	    => (lambda (setter-maker)
		 (or (setter-maker keys.stx input-form.stx)
		     ;;The keys are unknown: try with the parent.
		     (loop ($object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec has no setter maker: try with the parent.
	    (loop ($object-type-spec-parent-spec spec)))))))

(module (tag-identifier-dispatch)
  (define-fluid-override __who__
    (identifier-syntax 'tag-identifier-dispatch))

  (define* (tag-identifier-dispatch {tag tag-identifier?} {member.id identifier?} arg*.stx {input-form.stx syntax-object?})
    ;;Given  a   tag  identifier   and  an  identifier:   search  the   hierarchy  of
    ;;"object-type-spec" associated to TAG-ID for a dispatcher accepting MEMBER.ID as
    ;;method name  or, if not found,  an accessor maker accepting  MEMBER.ID as field
    ;;name.  If successful: return a  syntax object representing an expression which,
    ;;expanded by  itself and evaluated, will  return the method or  the accessor; if
    ;;neither a method nor an accessor is found: raise an exception.
    ;;
    ;;We expect INPUT-FORM.STX to have the format:
    ;;
    ;;   (?expr ?member ?arg ...)
    ;;
    ;;where: ?EXPR  is an expression of  type TAG; ?MEMBER is  an identifier matching
    ;;the name  of a method or  field of TAG or  one of its supertags  (the MEMBER.ID
    ;;argument); the ?ARG are additional operands (the ARG*.STX argument).
    ;;
    (cond (($tag-super-and-sub? (procedure-tag-id) tag)
	   input-form.stx)
	  ((or (free-id=? tag (untagged-tag-id))
	       (free-id=? tag (top-tag-id)))
	   (%error-invalid-tagged-syntax input-form.stx))
	  (else
	   (%try-dispatcher (identifier-object-type-spec tag) (syntax->datum member.id)
			    arg*.stx input-form.stx))))

  (define (%try-dispatcher spec member.sym arg*.stx input-form.stx)
    (cond ((not spec)
	   (%error-invalid-tagged-syntax input-form.stx))
	  (($object-type-spec-dispatcher spec)
	   => (lambda (dispatcher)
		(or (dispatcher member.sym arg*.stx input-form.stx)
		    (%try-accessor spec member.sym arg*.stx input-form.stx))))
	  (else
	   (%try-accessor spec member.sym arg*.stx input-form.stx))))

  (define (%try-accessor spec member.sym arg*.stx input-form.stx)
    (define (%try-parent-dispatcher)
      (%try-dispatcher ($object-type-spec-parent-spec spec) member.sym arg*.stx input-form.stx))
    (cond ((not spec)
	   (syntax-violation __who__ "invalid tagged"))
	  (($object-type-spec-accessor-maker spec)
	   => (lambda (accessor-maker)
		(cond ((accessor-maker member.sym input-form.stx)
		       => (lambda (accessor-stx)
			    (syntax-match arg*.stx ()
			      (()
			       accessor-stx)
			      (_
			       (syntax-violation __who__
				 "invalid additional operands for field accessor"
				 input-form.stx arg*.stx)))))
		      (else
		       (%try-parent-dispatcher)))))
	  (else
	   ;;There is no accessor maker, try the parent's dispatcher.
	   (%try-parent-dispatcher))))

  (define (%error-invalid-tagged-syntax input-form.stx)
    (syntax-violation __who__ "invalid tagged syntax" input-form.stx))

  #| end of module: TAG-DISPATCH |# )


;;;; tag identifiers

(define-constant *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*
  'vicare:expander:object-type-spec)

;;; --------------------------------------------------------------------

(define* (set-identifier-object-type-spec! {type-id identifier-bound?} {spec object-type-spec?})
  ;;Add to  the syntactic binding  label property list  an entry representing  a type
  ;;specification.  When this call succeeds: TYPE-ID becomes a tag identifier.
  ;;
  (if ($syntactic-binding-getprop type-id *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
      (syntax-violation __who__
	"object specification already defined" type-id spec)
    ($syntactic-binding-putprop type-id *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE* spec)))

(define* ({identifier-object-type-spec false-or-object-type-spec?} {tag identifier-bound?})
  ;;Retrieve from  the syntactic binding  label property list  the "object-type-spec"
  ;;describing the type specification; return false if no such entry exists.
  ;;
  ($identifier-object-type-spec tag))

(define ($identifier-object-type-spec tag)
  ;;Retrieve from  the syntactic binding  label property list  the "object-type-spec"
  ;;describing the type specification; return false if no such entry exists.
  ;;
  ($syntactic-binding-getprop tag *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-label-object-type-spec! {label symbol?} {spec object-type-spec?})
  ;;Add to LABEL's property list an entry representing a type specification; LABEL is
  ;;meant to be  a syntactic binding label.  When this  call succeeds: the associated
  ;;identifier becomes a tag identifier.
  ;;
  (cond (($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	 => (lambda (old-spec)
	      (syntax-violation __who__
		"object specification already defined" label old-spec spec)))
	(else
	 ($putprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE* spec))))

(define* ({label-object-type-spec false-or-object-type-spec?} {label symbol?})
  ;;Retrieve from  LABEL's property list  the "object-type-spec" describing  the type
  ;;specification; return  false if  no such entry  exists.  LABEL is  meant to  be a
  ;;syntactic binding label.
  ;;
  ($getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*))

;;; --------------------------------------------------------------------

(define (tag-identifier? obj)
  ;;Return true  if OBJ is a  bound identifier with "object-type-spec"  property set;
  ;;otherwise return false.
  ;;
  (and (identifier? obj)
       ($identifier-bound? obj)
       (and ($identifier-object-type-spec obj)
	    #t)))

(define (false-or-tag-identifier? obj)
  (or (not obj)
      (tag-identifier? obj)))

(define (assert-tag-identifier? obj)
  (unless (tag-identifier? obj)
    (syntax-violation #f
      "expected tag identifier, identifier with object-type-spec set" obj)))

(define* (tag-super-and-sub? {super-tag tag-identifier?} {sub-tag tag-identifier?})
  ;;Given  two tag  identifiers: return  true  if SUPER-TAG  is FREE-IDENTIFIER=?  to
  ;;SUB-TAG or one of its ancestors.
  ;;
  ($tag-super-and-sub? super-tag sub-tag))

(define ($tag-super-and-sub? super-tag sub-tag)
  (or (free-id=? super-tag sub-tag)
      (free-id=? (top-tag-id) super-tag)
      (let ((pspec ($object-type-spec-parent-spec ($identifier-object-type-spec sub-tag))))
	(and pspec
	     (let ((sub-ptag ($object-type-spec-type-id pspec)))
	       (and (not (free-id=? (top-tag-id) sub-ptag))
		    ($tag-super-and-sub? super-tag sub-ptag)))))))

(define (all-tag-identifiers? stx)
  ;;Return true  if STX is  a proper or improper  list of tag  identifiers; otherwise
  ;;return false.
  ;;
  (syntax-match stx ()
    (() #t)
    ((?arg . ?rest)
     (tag-identifier? ?arg)
     (all-tag-identifiers? ?rest))
    (?rest
     (tag-identifier? ?rest))
    (_ #f)))


;;;; tagged identifiers: expand-time binding type tagging

(define-constant *EXPAND-TIME-BINDING-TAG-COOKIE*
  'vicare:expander:binding-type-tagging)

;;; --------------------------------------------------------------------

(define* (set-identifier-tag! {binding-id identifier-bound?} {tag tag-identifier?})
  ;;Given a  syntactic binding identifier:  add TAG to  its property list  as binding
  ;;type  tagging.  This  tag  should represent  the object  type  referenced by  the
  ;;binding.
  ;;
  (cond (($syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE*)
	 => (lambda (old-tag)
	      (syntax-violation __who__
		"identifier binding tag already defined"
		binding-id old-tag tag)))
	(else
	 ($syntactic-binding-putprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE* tag))))

(define* (identifier-tag {binding-id identifier-bound?})
  ;;Given  a  syntactic binding  identifier:  retrieve  from  its property  list  the
  ;;identifier representing  the binding  type tagging.   This tag  identifier should
  ;;represent the object type referenced by the binding.
  ;;
  ($syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-TAG-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-label-tag! {label symbol?} {tag tag-identifier?})
  ;;Given a  syntactic binding LABEL:  add TAG  to its property  list as
  ;;binding type  tagging.  This  tag should  represent the  object type
  ;;referenced by the binding.
  ;;
  (cond (($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*)
	 => (lambda (old-tag)
	      (syntax-violation __who__
		"label binding tag already defined" label old-tag tag)))
	(else
	 ($putprop label *EXPAND-TIME-BINDING-TAG-COOKIE* tag))))

(define* (label-tag {label identifier?})
  ;;Given a syntactic binding LABEL: retrieve from its property list the
  ;;identifier  representing   the  binding  type  tagging.    This  tag
  ;;identifier  should  represent  the  object type  referenced  by  the
  ;;binding.
  ;;
  ($getprop label *EXPAND-TIME-BINDING-TAG-COOKIE*))

;;; --------------------------------------------------------------------

(define* (tagged-identifier? {id identifier-bound?})
  ;;Return #t  if ID is an  identifier having a type  tagging; otherwise
  ;;return false.  If the return value is true: ID is a bound identifier
  ;;created by some binding syntaxes (define, let, letrec, ...).
  ;;
  (and (identifier-tag id)
       #t))

(define* (%tagged-identifier-with-dispatcher? id)
  ;;Return #t  if ID is an  identifier, it is bound  identifier, it has a  tag in its
  ;;property  list,  and  the  tag  identifier has  a  dispatcher  procedure  in  its
  ;;"object-type-spec"; otherwise return false.  If the return value is true: ID is a
  ;;bound identifier created by some binding syntaxes (define, let, letrec, ...)  and
  ;;it can be used in forms like:
  ;;
  ;;   (?id ?arg ...)
  ;;
  (and (identifier? id)
       ($identifier-bound? id)
       (cond ((identifier-tag id)
	      => (lambda (tag-id)
		   (let ((spec (identifier-object-type-spec tag-id)))
		     (and spec
			  (object-type-spec-dispatcher spec)
			  #t))))
	     (else #f))))


;;;; tagged identifiers: callable signature

(define-constant *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE*
  'vicare:expander:binding-callable-signature)

;;; --------------------------------------------------------------------

(define* (set-identifier-callable-signature! {binding-id identifier?} {signature lambda-signature?})
  ;;Given a syntactic binding identifier for a function binding: add SIGNATURE to its
  ;;property list as type tagging for the function.
  ;;
  (cond (($syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE*)
	 => (lambda (old-signature)
	      (syntax-violation __who__
		"identifier binding function tagging signature already defined"
		binding-id old-signature signature)))
	(else
	 ($syntactic-binding-putprop binding-id *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE* signature))))

(define* (identifier-callable-signature {binding-id identifier?})
  ;;Given a  syntactic binding identifier for  a function binding: retrieve  from its
  ;;property  list  the tagging  signature  of  the  function.   Return false  if  no
  ;;signature is defined.
  ;;
  ($syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-label-callable-signature! {label symbol?} {signature lambda-signature?})
  ;;Given a  syntactic binding  LABEL for  a function binding:  add SIGNATURE  to its
  ;;property list as type tagging for the function.
  ;;
  (cond (($getprop label *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE*)
	 => (lambda (old-signature)
	      (syntax-violation __who__
		"label binding function tagging signature already defined"
		label signature signature)))
	(else
	 ($putprop label *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE* signature))))

(define* (label-callable-signature {label symbol?})
  ;;Given  a syntactic  binding  LABEL  for a  function  binding:  retrieve from  its
  ;;property  list  the tagging  signature  of  the  function.   Return false  if  no
  ;;signature is defined.
  ;;
  ($getprop label *EXPAND-TIME-BINDING-CALLABLE-SIGNATURE-COOKIE*))


;;;; identifiers: expand-time callable object type specification

(define-constant *EXPAND-TIME-CALLABLE-SPEC-COOKIE*
  'vicare:expander:callable-spec)

(define-record callable-spec
  ;;A struct type representing a callable form binding, either procedure
  ;;or macro.
  ;;
  (name
		;A symbol  representing this callable name.   To be used
		;for meaningful error reporting.
   min-arity
		;A fixnum  representing the minimum number  of arguments
		;this callable must be applied to.
   max-arity
		;A fixnum or positive  infinity representing the maximum
		;number of arguments this callable must be applied to.
   dispatcher
		;False  or  a  procedure  to be  called  with  the  type
		;signature of a specific callable application.  The type
		;signature  of  a tuple  of  arguments  is the  list  of
		;instances   of  type   object-type-spec  matching   the
		;arguments.
		;
		;It is meant  to return two values: the  identifier of a
		;specialised  version  of  this callable  that  is  more
		;suited to be  applied to a tuple of  arguments with the
		;given type  signature; an instance  of object-type-spec
		;representing the type of the return value.
   ))

;;; --------------------------------------------------------------------

(define* (set-identifier-callable-spec! {type-id identifier?} {spec callable-spec?})
  (if ($syntactic-binding-getprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE*)
      (syntax-violation __who__
	"callable specification already defined" type-id spec)
    ($syntactic-binding-putprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE* spec)))

(define* (identifier-callable-spec {type-id identifier?})
  ($syntactic-binding-getprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE*))

;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; End:
