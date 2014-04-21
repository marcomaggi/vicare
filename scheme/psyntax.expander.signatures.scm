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
     (identifier? ?rest))
    ))

(define (formals-signature-syntax? stx)
  ;;Return true if STX is a syntax  object representing the tag signature of a tagged
  ;;formals syntax;  otherwise return false.   The return value is  true if STX  is a
  ;;proper  or improper  list of  tag  identifiers, with  null and  a standalone  tag
  ;;identifier being  acceptable; when the  signature is  an improper list:  the tail
  ;;standalone identifier must be "<list>" or a sub-tag of "<list>".  Examples:
  ;;
  ;;   (formals-signature-syntax #'<list>)				=> #t
  ;;   (formals-signature-syntax #'())					=> #t
  ;;   (formals-signature-syntax #'(<fixnum> <string>))			=> #t
  ;;   (formals-signature-syntax #'(<fixnum> <string> . <list>))	=> #t
  ;;
  ;;A standalone "<list>" identifier means: any number of values of any type.
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (tag-identifier? ?id)
     (formals-signature-syntax? ?rest))
    (?rest
     (tag-identifier-and-list-sub-tag? ?rest))
    ))

(define (retvals-signature-syntax? stx)
  ;;Return true if  STX is a syntax  object representing the tag  signature of tagged
  ;;return values; otherwise return  false.  The return value is true  if STX is null
  ;;or a proper or improper list of tag identifiers, with a standalone tag identifier
  ;;being acceptable;  when the signature  is an  improper list: the  tail standalone
  ;;identifier must be "<list>" or a sub-tag of "<list>".  Examples:
  ;;
  ;;   (retvals-signature-syntax #'<list>)				=> #t
  ;;   (retvals-signature-syntax #'())					=> #t
  ;;   (retvals-signature-syntax #'(<fixnum> <string>))			=> #t
  ;;   (retvals-signature-syntax #'(<fixnum> <string> . <list>))	=> #t
  ;;
  ;;A standalone "<list>" identifier means: any number of values of any type.
  ;;
  (syntax-match stx ()
    (() #t)
    ((?id . ?rest)
     (tag-identifier? ?id)
     (retvals-signature-syntax? ?rest))
    (?rest
     (tag-identifier-and-list-sub-tag? ?rest))
    ))

;;; --------------------------------------------------------------------

(define* (formals-signature-partially-untagged-syntax? {stx formals-signature-syntax?})
  ;;The argument STX must be a  syntax object representing a formals signature syntax
  ;;according  to FORMALS-SIGNATURE-SYNTAX?.   Return true  if STX  has at  least one
  ;;"<top>" tag identifier; otherwise return false.
  ;;
  ($formals-signature-partially-untagged-syntax? stx))

(define ($formals-signature-partially-untagged-syntax? stx)
  (syntax-match stx ()
    (() #f)
    ((?tag . ?rest-tags)
     (or (top-tag-id? ?tag)
	 ($formals-signature-partially-untagged-syntax? ?rest-tags)))
    (?rest
     #t)))

(define* (retvals-signature-partially-unspecified-syntax? {stx retvals-signature-syntax?})
  ;;The argument STX must be a  syntax object representing a retvals signature syntax
  ;;according to RETVALS-SIGNATURE-SYNTAX?.  Return true  if STX if the signature has
  ;;at least one "<top>" tag identifier; otherwise return false.
  ;;
  ($retvals-signature-partially-unspecified-syntax? stx))

(define $retvals-signature-partially-unspecified-syntax?
  $formals-signature-partially-untagged-syntax?)

;;; --------------------------------------------------------------------

(define* (formals-signature-super-and-sub-syntax? {super-signature formals-signature-syntax?}
						  {sub-signature   formals-signature-syntax?})
  ;;Return  true if  the super  signature syntax  and the  sub signature  syntax have
  ;;compatible structure and  the tags from the super signature  are supertags of the
  ;;tags from the sub signature; otherwise return false.
  ;;
  ;;This function can be used to determine:  if a tuple of arguments matches a lambda
  ;;formals's signature; if a tuple or return values matches the receiver signature.
  ;;
  ($formals-signature-super-and-sub-syntax? super-signature sub-signature))

(define ($formals-signature-super-and-sub-syntax? super-signature sub-signature)
  (syntax-match super-signature ()
    (()
     (syntax-match sub-signature ()
       ;;Both the signatures are proper lists with  the same number of items, and all
       ;;the items are correct super and sub: success!
       (() #t)
       ;;The signatures do not match.
       (_  #f)))

    ((?super-tag . ?super-rest-tags)
     (syntax-match sub-signature ()
       ((?sub-tag . ?sub-rest-tags)
	(and (or (top-tag-id? ?super-tag)
		 (tag-super-and-sub? ?super-tag ?sub-tag))
	     ($formals-signature-super-and-sub-syntax? ?super-rest-tags ?sub-rest-tags)))
       (_ #f)))

    (?super-rest-tag
     (syntax-match sub-signature ()
       ;;The super signature is an improper list with rest item and the sub signature
       ;;is finished.  We want the following signature to match:
       ;;
       ;;  #'(<number> <fixnum> . <list>)     #'(<complex> <fixnum>)
       ;;
       ;;because "<list>" in rest position means any number of objects of any type.
       (() #t)

       ;;The super signature is an improper  list shorter than the sub signature.  We
       ;;want the following signature to match:
       ;;
       ;;  #'(<number> . <list>)  #'(<complex> <fixnum> <fixnum>)
       ;;
       ;;because "<list>" in  rest position means any number of  objects of any type.
       ;;If ?SUPER-REST-TAG is a sub-tag of "<list>": we do not want it to match.
       ((?sub-tag . ?sub-rest-tags)
	(list-tag-id?      ?super-rest-tag))

       ;;Both the  signatures are improper lists  with the same number  of items, and
       ;;all the items are  correct super and sub; if the rest  tags are proper super
       ;;and subs: success!
       (?sub-rest-tag
	(tag-super-and-sub? ?super-rest-tag ?sub-rest-tag))
       ))
    ))

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

;;; --------------------------------------------------------------------

(module (retvals-signature-syntax-common-ancestor)

  (define* (retvals-signature-syntax-common-ancestor {sig1 retvals-signature-syntax?}
						     {sig2 retvals-signature-syntax?})
    ($retvals-signature-syntax-common-ancestor sig1 sig2))

  (define ($retvals-signature-syntax-common-ancestor sig1 sig2)
    ;;
    ;;We assume that  both SIG1 and SIG2 are retvals  signature syntaxes according to
    ;;RETVALS-SIGNATURE-SYNTAX?.
    ;;
    (if (or (list-tag-id? sig1)
	    (list-tag-id? sig2))
	(list-tag-id)
      (syntax-match sig1 ()
	(()
	 (syntax-match sig2 ()
	   (() '())
	   ((?tag2 . ?rest-tags2)
	    (list-tag-id))
	   (_
	    (list-tag-id))))

	((?tag1 . ?rest-tags1)
	 (syntax-match sig2 ()
	   (()
	    (list-tag-id))
	   ((?tag2 . ?rest-tags2)
	    (cons (tag-common-ancestor ?tag1 ?tag2)
		  ($retvals-signature-syntax-common-ancestor ?rest-tags1 ?rest-tags2)))
	   (?rest-tag2
	    (list-tag-id))))

	(?rest-tag1
	 (syntax-match sig2 ()
	   (()
	    (list-tag-id))
	   ((?tag2 . ?rest-tags2)
	    (list-tag-id))
	   (?rest-tag2
	    (tag-common-ancestor ?rest-tag1 ?rest-tag2))))
	)))

  #| end of module |# )


;;;; callable syntax, callable signature, return values signature, formals signature

(define-record (retvals-signature %make-retvals-signature retvals-signature?)
  (tags
		;A syntax object representing a retvals signature syntax according to
		;RETVALS-SIGNATURE-SYNTAX?.
   ))

(define-record (formals-signature %make-formals-signature formals-signature?)
  (tags
		;A syntax object representing a formals signature syntax according to
		;FORMALS-SIGNATURE-SYNTAX?.
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
		;determine a common retvals signature: the default value is "<list>",
		;which means any number of objects of any type.
		;
   lambda-signatures
		;A  proper  list  of "lambda-signature"  instances  representing  the
		;signatures of the CASE-LAMBDA clauses.
   ))

(define* (make-formals-signature {tags formals-signature-syntax?})
  (%make-formals-signature (syntax-unwrap tags)))

(define* (make-retvals-signature {tags retvals-signature-syntax?})
  (%make-retvals-signature (syntax-unwrap tags)))

(define* (make-lambda-signature {retvals retvals-signature?} {formals formals-signature?})
  (%make-lambda-signature retvals formals))

(define* (make-clambda-compound {signatures list-of-lambda-signatures?})
  (%make-clambda-compound (if (null? signatures) ;this is REDUCE
			      (make-retvals-signature-standalone-list)
			    (fold-left retvals-signature-common-ancestor
			      (lambda-signature-retvals ($car signatures))
			      (map lambda-signature-retvals ($cdr signatures))))
			  signatures))

;;; --------------------------------------------------------------------
;;; special constructors

(let-syntax
    ((define-single-tag-retvals-maker (syntax-rules ()
					((_ ?who ?tag-maker)
					 (define ?who
					   (let ((sign #f))
					     (lambda ()
					       (or sign
						   (receive-and-return (S)
						       (make-retvals-signature-single-value (?tag-maker))
						     (set! sign S))))))))))
  (define-single-tag-retvals-maker make-retvals-signature-single-top		top-tag-id)
  (define-single-tag-retvals-maker make-retvals-signature-single-procedure	procedure-tag-id)
  #| end of let-syntax |# )

(define make-retvals-signature-standalone-list
  (let ((sign #f))
    (lambda ()
      (or sign
	  (receive-and-return (S)
	      (make-retvals-signature (list-tag-id))
	    (set! sign S))))))

(define-syntax-rule (make-retvals-signature-fully-unspecified)
  (make-retvals-signature-standalone-list))

(define* (make-retvals-signature-single-value {tag tag-identifier?})
  (make-retvals-signature (list tag)))


;;;; lambda-signature stuff

(define* (lambda-signature=? {signature1 lambda-signature?} {signature2 lambda-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (and (syntax=? ($formals-signature-tags ($lambda-signature-formals signature1))
		 ($formals-signature-tags ($lambda-signature-formals signature2)))
       (syntax=? ($retvals-signature-tags ($lambda-signature-retvals signature1))
		 ($retvals-signature-tags ($lambda-signature-retvals signature2)))))

;;; --------------------------------------------------------------------

(define* (lambda-signature-formals-tags {signature lambda-signature?})
  ($formals-signature-tags ($lambda-signature-formals signature)))

(define* (lambda-signature-retvals-tags {signature lambda-signature?})
  ($retvals-signature-tags ($lambda-signature-retvals signature)))

;;; --------------------------------------------------------------------

(define (list-of-lambda-signatures? obj)
  (and (list? obj)
       (for-all lambda-signature? obj)))

(define* (lambda-signature-fully-unspecified? {signature lambda-signature?})
  ;;A LAMBDA  signature has fully unspecified  types if its retvals  tag signature is
  ;;the standalone "<list>" tag and its formals signature is a proper list of "<top>"
  ;;tags:
  ;;
  ;;   (<top> ...)
  ;;
  ;;or an improper list like:
  ;;
  ;;   (<top> ... . <list>)
  ;;
  (and (formals-signature-fully-unspecified? ($lambda-signature-formals signature))
       (retvals-signature-fully-unspecified? ($lambda-signature-retvals signature))))


;;;; formals-signature stuff

(define* (formals-signature=? {signature1 formals-signature?} {signature2 formals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? ($formals-signature-tags signature1)
	    ($formals-signature-tags signature2)))

;;; --------------------------------------------------------------------

(define* (formals-signature-super-and-sub? {super-signature formals-signature?}
					   {sub-signature   formals-signature?})
  ($formals-signature-super-and-sub-syntax? ($formals-signature-tags super-signature)
					    ($formals-signature-tags sub-signature)))

(define* (formals-signature-fully-unspecified? {signature formals-signature?})
  ;;Return  true  if the  formals  signature  does  not  specify any  argument  type,
  ;;otherwise  return false;  in other  words, return  true if  the signature  is one
  ;;among:
  ;;
  ;;   (<top> ...)
  ;;   (<top> ... . <list>)
  ;;
  (let ((formals.stx ($formals-signature-tags signature)))
    (if (list? formals.stx)
	(for-all top-tag-id? formals.stx)
      (receive (head tail)
	  (improper-list->list-and-rest formals.stx)
	(and (for-all top-tag-id? head)
	     (list-tag-id? tail))))))


;;;; retvals-signature stuff

(define* (retvals-signature-fully-unspecified? {signature retvals-signature?})
  (list-tag-id? ($retvals-signature-tags signature)))

(define* (retvals-signature-partially-unspecified? {signature retvals-signature?})
  ($retvals-signature-partially-unspecified-syntax? ($retvals-signature-tags signature)))

(define* (retvals-signature-super-and-sub? {super-signature retvals-signature?}
					   {sub-signature   retvals-signature?})
  ($retvals-signature-super-and-sub-syntax? ($retvals-signature-tags super-signature)
					    ($retvals-signature-tags sub-signature)))

(define* (retvals-signature-single-tag? {signature retvals-signature?})
  ;;Return  true if  SIGNATURE represents  a  single return  value, otherwise  return
  ;;false.  We have to remember that a signature syntax can be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (syntax-match ($retvals-signature-tags signature) ()
    ((?tag)	#t)
    (_		#f)))

(define* (retvals-signature-single-top-tag? {signature retvals-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.   We have to remember that, after  parsing syntax objects
  ;;with SYNTAX-MATCH, a signature syntax can result to be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (syntax-match ($retvals-signature-tags signature) ()
    ((?tag)
     (top-tag-id? ?tag))
    (_ #f)))

(define* (retvals-signature-single-tag-or-fully-unspecified? {signature retvals-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier,  otherwise return  false.  We have  to remember  that, after
  ;;parsing syntax objects with SYNTAX-MATCH, a signature syntax can result to be:
  ;;
  ;;   (#'?tag . #'())
  ;;
  ;;with the last element being a syntax  object representing null; so we really need
  ;;to use SYNTAX-MATCH here to inspect the tags.
  ;;
  (let ((tags ($retvals-signature-tags signature)))
    (or (list-tag-id? tags)
	(syntax-match tags ()
	  ((?tag)	#t)
	  (_		#f)))))

;;; --------------------------------------------------------------------

(define* (retvals-signature=? {signature1 retvals-signature?} {signature2 retvals-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (syntax=? ($retvals-signature-tags signature1)
	    ($retvals-signature-tags signature2)))

;;; --------------------------------------------------------------------

(define* (retvals-signature-common-ancestor {sig1 retvals-signature?}
					    {sig2 retvals-signature?})
  (make-retvals-signature
   (retvals-signature-syntax-common-ancestor ($retvals-signature-tags sig1)
					     ($retvals-signature-tags sig2))))


;;;; tagged binding parsing: standalone identifiers

(define (tagged-identifier-syntax? stx)
  ;;Return  true  if  STX is  a  syntax  object  representing  a tagged  or  untagged
  ;;identifier, otherwise return false.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (tag-identifier? ?tag)))
    (?id
     (identifier? ?id))))

(define (parse-tagged-identifier-syntax stx)
  ;;If  STX is  a tagged  or  untagged identifier,  return 2  values: the  identifier
  ;;representing the binding name and  the identifier representing the tag; otherwise
  ;;raise  an exception.   When no  tag is  present: the  tag identifier  defaults to
  ;;"<top>".
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (begin
       (assert-tag-identifier? ?tag)
       (values ?id ?tag)))
    (?id
     (identifier? ?id)
     (values ?id (top-tag-id)))))


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
;;   (#'a #'b #'c) (#'<fixnum> #'<string> #'<top>)
;;

(case-define* parse-list-of-tagged-bindings
  ((stx)
   (parse-list-of-tagged-bindings stx #f))
  ((stx input-form.stx)
   ;;Assume STX  is a  syntax object  representing a proper  list of  possibly tagged
   ;;binding identifiers; parse  the list and return 2 values:  a list of identifiers
   ;;representing the  binding identifiers,  a list  of identifiers  representing the
   ;;type tags;  "<top>" is  used when no  tag is present.   The identifiers  must be
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
	      (values (cons ?id id*) (cons (top-tag-id) tag*))))
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
  ;;Return true  if LHS* is a  list of possibly tagged  identifiers; otherwise return
  ;;false.
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
      ;;only a sub-tag of "<list>" is acceptable here.
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (begin
	 (assert-list-sub-tag-identifier? ?args-tag)
	 (values ?args-id ?args-tag)))

      ;;Possibly tagged identifiers with tagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . {rest list-of-fixnums}) ?expr)) . ?body)
      ;;
      ;;only a sub-tag of "<list>" is accepted in rest position.
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (begin
	 (unless (and (identifier? ?rest-id)
		      (identifier? ?rest-tag)
		      (tag-super-and-sub? (list-tag-id) ?rest-tag))
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
	       (values ?rest-id (list-tag-id))))
	 (%validate-formals input-form.stx original-formals.stx standard-formals.stx)))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (begin
	 (%validate-formals input-form.stx original-formals.stx ?id*)
	 (values ?id* (map (lambda (id) (top-tag-id)) ?id*))))

      ;;Standard formals: untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-formals input-form.stx original-formals.stx (append ?id* ?rest-id))
	 (values formals.stx (cons* (map (lambda (id) (top-tag-id)) ?id*) (list-tag-id)))))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (list-tag-id)))

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
	   (values (cons ?id standard-formals) (cons (top-tag-id) tags)))
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
  (({_ standard-formals-syntax? lambda-signature?} {callable-signature.stx syntax-object?})
   (parse-tagged-lambda-proto-syntax callable-signature.stx #f))
  (({_ standard-formals-syntax? lambda-signature?} {callable-signature.stx syntax-object?} {input-form.stx syntax-object?})
   ;;First we parse  and extract the return  values tagging, if any;  then we parse
   ;;the rest of the formals.
   (syntax-match callable-signature.stx (brace)
     ;;With return values tagging.
     (((brace ?who ?rv-tag* ... . ?rv-rest-tag) . ?formals)
      (underscore-id? ?who)
      (let ((retvals.stx (append ?rv-tag*
				 ;;We  want  a  proper  list when  possible,  not  an
				 ;;improper list with the syntax object #'() as tail.
				 (if (null? (syntax->datum ?rv-rest-tag))
				     '()
				   ?rv-rest-tag))))
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
		(make-lambda-signature (make-retvals-signature-standalone-list) formals-signature)))))))

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
