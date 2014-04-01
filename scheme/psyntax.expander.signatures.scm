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


;;;; formals, retvals, signature syntaxes predicates

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


;;;; done


;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; End:
