;;;Copyright (c) 2010-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


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


;;;; binding parsers: standard syntaxes, signature syntaxes

(define (standard-formals-syntax? stx)
  ;;Return  true if  STX is  a syntax  object representing  R6RS standard  LAMBDA and
  ;;LET-VALUES formals; otherwise return false.  The return value is true if STX is a
  ;;proper or  improper list of  identifiers, with  null and a  standalone identifier
  ;;being acceptable.
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
  ;;identifier being acceptable.
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
  ;;return values; otherwise return false.  The return value is true if STX is false,
  ;;null or  a proper  or improper  list of  tag identifiers,  with a  standalone tag
  ;;identifier being acceptable.
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

(define* (formals-signature-super-and-sub? {super-signature formals-signature-syntax?}
					   {sub-signature   formals-signature-syntax?})
  ($formals-signature-super-and-sub? super-signature sub-signature))

(define ($formals-signature-super-and-sub? super-signature sub-signature)
  (syntax-match super-signature ()
    (()
     (syntax-match sub-signature ()
       (() #t)
       (_  #f)))

    ((?super-tag . ?super-rest-tags)
     (syntax-match sub-signature ()
       ((?sub-tag . ?sub-rest-tags)
	($tag-super-and-sub? ?super-tag ?sub-tag)
	($formals-signature-super-and-sub? ?super-rest-tags ?sub-rest-tags))
       (_ #f)))

    (?super-rest-tag
     (syntax-match sub-signature ()
       ;;We want the following signatures to match:
       ;;
       ;;  #'(<number> <fixnum> <fixnum> . <top>)   #'(<complex> <fixnum> <fixnum>)
       ;;  #'(<number> <fixnum> <fixnum> . <list>)  #'(<complex> <fixnum> <fixnum>)
       ;;
       ;;because "<top>" and "<list>" in rest position means any number of objects of
       ;;any type.
       ;;
       (()
	(or (free-id=? ?super-rest-tag <top>)
	    (free-id=? ?super-rest-tag (scheme-stx '<list>))))
       ;;We want the following signatures to match:
       ;;
       ;;  #'(<number> . <top>)   #'(<complex> <fixnum> <fixnum>)
       ;;  #'(<number> . <list>)  #'(<complex> <fixnum> <fixnum>)
       ;;
       ;;because "<top>" and "<list>" in rest position means any number of objects of
       ;;any type.
       ;;
       ((?sub-tag . ?sub-rest-tags)
	(or (free-id=? ?super-rest-tag <top>)
	    (free-id=? ?super-rest-tag (scheme-stx '<list>))))
       (?sub-rest-tag
	($tag-super-and-sub? ?super-rest-tag ?sub-rest-tag))
       (_ #f)))

    (_ #f)
    ))


;;;; callable spec, callable signature, return values signature, formals signature
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

(define-record (callable-signature %make-callable-signature callable-signature?)
  (return-values
		;An instance of "retvals-signature".
   formals
		;An instance of "formals-signature".
   ))

(define* (make-formals-signature {tags formals-signature-syntax?})
  (%make-formals-signature tags))

(define* (make-retvals-signature {tags retvals-signature-syntax?})
  (%make-retvals-signature tags))

(define* (make-callable-signature {rv retvals-signature?} {formals formals-signature?})
  (%make-callable-signature rv formals))

(define* (callable-signature-formals-tags {signature callable-signature?})
  ($formals-signature-tags ($callable-signature-formals signature)))

(define* (callable-signature-return-values-tags {signature callable-signature?})
  ($retvals-signature-tags ($callable-signature-return-values signature)))

;;; --------------------------------------------------------------------

(define* (callable-signature=? {signature1 callable-signature?} {signature2 callable-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  ;;Remember that SYNTAX=? compares identifiers with FREE-IDENTIFIER=?.
  ;;
  (and (formals-signature=? ($callable-signature-formals signature1)
			    ($callable-signature-formals signature2))
       (retvals-signature=? ($callable-signature-return-values signature1)
				  ($callable-signature-return-values signature2))))

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
  ;;If STX  is a  tagged or  untagged identifier,  return 2  values: the
  ;;identifier  representing   the  binding  name  and   the  identifier
  ;;representing the tag; otherwise raise  an exception.  When no tag is
  ;;present: the tag identifier defaults to <top>.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (begin
       (assert-tag-identifier? ?tag)
       (values ?id ?tag)))
    (?id
     (identifier? ?id)
     (values ?id <top>))
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
;;   (#'a #'b #'c) (#'<fixnum> #'<string> #'<top>)
;;

(case-define* parse-list-of-tagged-bindings
  ((stx)
   (parse-list-of-tagged-bindings stx #f))
  ((stx input-form-stx)
   ;;Assume STX  is a  syntax object  representing a proper  list of  possibly tagged
   ;;binding identifiers; parse  the list and return 2 values:  a list of identifiers
   ;;representing the  binding identifiers,  a list  of identifiers  representing the
   ;;type  tags; <top>  is used  when no  tag is  present.  The  identifiers must  be
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
	      (values (cons ?id id*) (cons <top> tag*))))
	   (_
	    (if input-form-stx
		(%invalid-tagged-bindings-syntax input-form-stx stx)
	      (%invalid-tagged-bindings-syntax stx #f)))
	   ))
     (unless (distinct-bound-ids? id*)
       (if input-form-stx
	   (%duplicate-identifiers-in-bindings-specification input-form-stx stx)
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
    (({_ standard-formals-syntax? formals-signature?} original-formals-stx)
     (parse-tagged-formals-syntax original-formals-stx #f))
    (({_ standard-formals-syntax? formals-signature?} original-formals-stx input-form-stx)
     (receive (standard-formals formals-tags)
	 (%parse-formals input-form-stx original-formals-stx original-formals-stx)
       (values standard-formals (make-formals-signature formals-tags)))))

  (define (%parse-formals input-form-stx original-formals-stx formals-stx)
    (syntax-match formals-stx (brace)

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
	     "invalid rest argument specification" original-formals-stx (cons 'brace ?rest-id ?rest-tag)))
	 (assert-tag-identifier? ?rest-tag)
	 (receive-and-return (standard-formals-stx tags)
	     (let recur ((?arg* ?arg*))
	       (if (pair? ?arg*)
		   (%process-args input-form-stx original-formals-stx recur ?arg*)
		 ;;Process rest argument.
		 (values ?rest-id ?rest-tag)))
	   (%validate-formals input-form-stx original-formals-stx standard-formals-stx))))

      ;;Possibly tagged identifiers with UNtagged rest argument, as in:
      ;;
      ;;   (let-values (((?arg ... . rest) ?expr)) . ?body)
      ;;
      ((?arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (receive-and-return (standard-formals-stx tags)
	   (let recur ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-args input-form-stx original-formals-stx recur ?arg*)
	       (values ?rest-id <top>)))
	 (%validate-formals input-form-stx original-formals-stx standard-formals-stx)))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (begin
	 (%validate-formals input-form-stx original-formals-stx ?id*)
	 (values ?id* (map (lambda (id) <top>) ?id*))))

      ;;Standard formals: untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-formals input-form-stx original-formals-stx (append ?id* ?rest-id))
	 (values formals-stx (cons* (map (lambda (id) <top>) ?id*) <top>))))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id <top>))

      ;;Possibly tagged identifiers without rest argument.
      ;;
      ((?arg* ...)
       (receive-and-return (standard-formals-stx tags)
	   (let recur ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-args input-form-stx original-formals-stx recur ?arg*)
	       (values '() '())))
	 (%validate-formals input-form-stx original-formals-stx standard-formals-stx)))
      ))

  (define (%process-args input-form-stx original-formals-stx recur args-stx)
    (receive (standard-formals tags)
	(recur (cdr args-stx))
      (let ((arg-stx (car args-stx)))
	(syntax-match arg-stx (brace)
	  ;;Untagged argument.
	  (?id
	   (identifier? ?id)
	   (values (cons ?id standard-formals) (cons <top> tags)))
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
	     (or input-form-stx original-formals-stx) arg-stx))))))

  (define (%validate-formals input-form-stx original-formals-stx standard-formals-stx)
    (cond ((duplicate-bound-formals? standard-formals-stx)
	   => (lambda (duplicate-id)
		(syntax-violation __who__
		  "duplicate identifiers in formals specification"
		  (or input-form-stx original-formals-stx)
		  duplicate-id)))))

  #| end of module |# )

(define* (tagged-formals-syntax? formals-stx)
  ;;Return true if  FORMALS-STX is a syntax object representing  valid tagged formals
  ;;for a LET-VALUES syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature)
	(parse-tagged-formals-syntax formals-stx)
      #t)))


;;;; tagged binding parsing: callable signature

(case-define* parse-tagged-callable-spec-syntax
  ;;Given a  syntax object representing  a tagged  callable spec: split  the standard
  ;;formals from the tags; do test for duplicate bindings.  Return 2 values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2. An instance of "callable-signature".
  ;;
  (({_ standard-formals-syntax? callable-signature?} {callable-spec-stx syntax-object?})
   (parse-tagged-callable-spec-syntax callable-spec-stx #f))
  (({_ standard-formals-syntax? callable-signature?} {callable-spec-stx syntax-object?} {input-form-stx syntax-object?})
   ;;First we parse  and extract the return  values tagging, if any;  then we parse
   ;;the rest of the formals.
   (syntax-match callable-spec-stx (brace _)
     ;;With return values tagging.
     (((brace _ ?rv-tag* ... . ?rv-rest-tag) . ?formals)
      (let ((retvals.stx (append ?rv-tag* ?rv-rest-tag)))
	(unless (retvals-signature-syntax? retvals.stx)
	  (syntax-violation __who__
	    "invalid return values signature syntax" input-form-stx retvals.stx))
	(receive (standard-formals-stx formals-signature)
	    (parse-tagged-formals-syntax ?formals input-form-stx)
	  (values standard-formals-stx
		  (make-callable-signature (make-retvals-signature retvals.stx) formals-signature)))))
     ;;Without return values tagging.
     (?formals
      (receive (standard-formals-stx formals-signature)
	  (parse-tagged-formals-syntax ?formals input-form-stx)
	(values standard-formals-stx
		(make-callable-signature (make-retvals-signature #f) formals-signature)))))))

(define* (tagged-callable-spec-syntax? formals-stx)
  ;;Return true if  FORMALS-STX is a syntax object representing  valid tagged formals
  ;;for a LAMBDA syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature-tags)
	(parse-tagged-callable-spec-syntax formals-stx)
      #t)))


;;;; expand-time object type specification
;;
;;Records of  type "object-type-spec"  are used  to describe the  object type  of tag
;;identifier;  a bound  identifier is  a tag  identiier if,  and only  if, it  has an
;;"object-type-spec" in the property list of its syntactic binding label.
;;
;;Below we discuss the fields of "object-type-spec" and we assume the definitions:
#|
    (define-record-type <color>
      (fields (mutable   red)
              (immutable green)
              (mutable   blue)))

    (define ({<color>-string <string>} {O <color>})
      (format "[~a, ~a, ~a]"
              (<color>-red   O)
              (<color>-green O)
              (<color>-blue  O)))

    (define ({<color>-shift <color>} {O <color>} {R <fixnum>} {G <fixnum>} {B <fixnum>})
      (make-<color> (fx+ (<color>-red   O) R)
                    (fx+ (<color>-green O) G)
                    (fx+ (<color>-blue  O) B)))

    (define {O <color>}
      (make-<color> 1 2 3))
|#
;;and the fields are meant to hold fixnums.
;;
;;
;;Predicate syntax object
;;-----------------------
;;
;;It is  a syntax object PRED-STX  (wrapped or unwrapped) representing  an expression
;;which will evaluate to  a type predicate.  It is used by  the built-in syntax IS-A?
;;to implement the expansion:
;;
;;   (is-a? O <color>) ==> (<color>? O)
;;
;;the syntax object can just be:
;;
;;   #'<color>?
;;
;;Notice that it is perfectly fine to define this syntax object as:
;;
;;   #'(lambda (X) (and (fixnum? X) (fxpositive? X)))
;;
;;
;;Field accessor maker
;;--------------------
;;
;;When the "object-type-spec" has fields: it  is a function accepting 3 arguments and
;;returning 2 values; otherwise it is false.  The accessor maker arguments are:
;;
;;1..An identifier representing a field name.
;;
;;2..A boolean, true if the requested accessor is safe, false if it is unsafe.
;;
;;3..A syntax object representing the form which originated this accessor maker call.
;;   It must be used as "form" value in "&syntax" condition object.
;;
;;When successful, the returned values must be:
;;
;;1..A syntax object representing an expression evaluating to a field accessor.
;;
;;2..A tag identifier representing the object type returned by the accessor.  When no
;;   type is defined this return value must be #'<top>.
;;
;;The accessor maker is used to implement the expansions:
;;
;;   (slot-ref O red   <color>) ==> (tag-dispatch <fixnum> (<color>-red   (tag-assert-and-return <color> O)))
;;   (slot-ref O green <color>) ==> (tag-dispatch <fixnum> (<color>-green (tag-assert-and-return <color> O)))
;;   (slot-ref O blue  <color>) ==> (tag-dispatch <fixnum> (<color>-blue  (tag-assert-and-return <color> O)))
;;
;;and its implementation can be:
;;
#|
   (define (accessor-maker field-name-id safe? input-form-stx)
     (case (syntax->datum field-name-id)
       ((red)
        (values (if safe? #'<color>-red   #'$<color>-red)   #'<fixnum>))

       ((green)
        (values (if safe? #'<color>-green #'$<color>-green) #'<fixnum>))

       ((blue)
        (values (if safe? #'<color>-blue  #'$<color>-blue)  #'<fixnum>))

       (_
        (values #f #f))))
|#
;;If the field name is not recognised the  return values must be #f: in this case the
;;field will be searched in the parent "object-type-spec", if any.
;;
;;If the  field name is  invalid for  some reason: the  accessor maker must  raise an
;;exception with condition object "&syntax".
;;
;;
;;Field mutator maker
;;-------------------
;;
;;When the "object-type-spec" has fields: it  is a function accepting 3 arguments and
;;returning 2 values; otherwise it is false.  The mutator maker arguments are:
;;
;;1..An identifier representing a field name.
;;
;;2..A boolean, true if the requested mutator is safe, false if it is unsafe.
;;
;;3..A syntax object representing the form which originated this accessor maker call.
;;   It must be used as "form" value in "&syntax" condition object.
;;
;;When successful, the returned values must be:
;;
;;1..A syntax object representing an expression evaluating to a field mutator.
;;
;;2..A tag identifier representing  the object type of the new  field value.  When no
;;   type is defined this return value must be #'<top>.
;;
;;The mutator maker is used to implement the expansions:
;;
;;   (slot-set! O red   <color> 1) ==> (<color>-red-set!  (tag-assert-and-return <color> O) (tag-assert-and-return <fixnum> 1))
;;   (slot-set! O green <color> 1) error--> &syntax
;;   (slot-set! O blue  <color> 1) ==> (<color>-blue-set! (tag-assert-and-return <color> O) (tag-assert-and-return <fixnum> 1))
;;   (set! (O red)   1)            ==> (<color>-red-set!  (tag-assert-and-return <color> O) (tag-assert-and-return <fixnum> 1))
;;   (set! (O green) 1)            error--> &syntax
;;   (set! (O blue)  1)            ==> (<color>-blue-set! (tag-assert-and-return <color> O) (tag-assert-and-return <fixnum> 1))
;;
;;and its implementation can be:
#|
   (define (mutator-maker field-name-id safe? input-form-stx)
     (case (syntax->datum field-name-id)
       ((red)
        (values (if safe? #'<color>-red-set!   #'$<color>-red-set!)   #'<fixnum>))

       ((green)
        (syntax-violation '<color>
          "requested mutator of immutable field" input-form-stx field-name-id))

       ((blue)
        (values (if safe? #'<color>-blue-set!  #'$<color>-blue-set!)  #'<fixnum>))

       (_
        (values #f #f))))
|#
;;If the field name is not recognised the  return values must be #f: in this case the
;;field will be searched in the parent "object-type-spec", if any.
;;
;;If the  field name  is invalid  for some reason:  the mutator  maker must  raise an
;;exception with condition object "&syntax".
;;
;;
;;Setter maker
;;------------
;;
;;When the "object-type-spec" supports the setter  syntax: it is a function accepting
;;2  arguments and  returning 2  values;  otherwise it  is false.   The setter  maker
;;arguments are:
;;
;;1..A syntax object representing the setter keys.  The syntax object's expression is
;;   a list of lists.
;;
;;2..A syntax object representing the form which originated this accessor maker call.
;;   It must be used as "form" value in "&syntax" condition object.
;;
;;When successful, the returned values must be:
;;
;;1..A syntax object  representing an expression evaluating to  an attribute mutator;
;;   it can be a field mutator or a call to a function that mutates the object.
;;
;;2..A tag identifier representing the object  type of the new attribute value.  When
;;   no type is defined this return value must be #'<top>.
;;
;;The SET! setter syntax is used to implement the expansions:
;;
;;   (set! O[red] 1)
;;   ==> (tag-setter <color> O ([red]) 1)
;;
;;   (set! O[green] 1)
;;   ==> (tag-setter <color> O ([green]) 1)
;;
;;   (set! O[blue] 1)
;;   ==> (tag-setter <color> O ([blue]) 1)
;;
;;   (set! (O[red]) 1)
;;   ==> (tag-setter <color> O ([red]) 1)
;;
;;   (set! (O[green]) 1)
;;   ==> (tag-setter <color> O ([green]) 1)
;;
;;   (set! (O[blue]) 1)
;;   ==> (tag-setter <color> O ([blue]) 1)
;;
;;which in turn will use the setter maker to implement the expansions:
;;
;;   (tag-setter <color> O ([red])   1) ==> (<color>-red-set!  (tag-assert-and-return <color> O) (tag-assert-and-return <fixnum> 1))
;;   (tag-setter <color> O ([green]) 1) error--> &syntax
;;   (tag-setter <color> O ([blue])  1) ==> (<color>-blue-set! (tag-assert-and-return <color> O) (tag-assert-and-return <fixnum> 1))
;;
;;The setter maker implementation can be:
#|
   (define (setter-maker keys-stx input-form-stx)
     (syntax-case keys-stx ()
       (([?field-name])
        (identifier? #'?field-name)
        (mutator-maker #'?field-name #t input-form-stx))
       (_
        (syntax-violation '<color>
          "invalid setter syntax" input-form-stx keys))))
|#
;;If the  keys are  not recognised the  return values  must be #f:  in this  case the
;;parent's "object-type-spec" setter will be used, if any.
;;
;;If the keys are  invalid for some reason: the setter maker  must raise an exception
;;with condition object "&syntax".
;;
;;
;;Method dispatcher
;;-----------------
;;
;;When the "object-type-spec" has methods: it is a function accepting 2 arguments and
;;returning 2 values; otherwise it is false.  The accessor maker arguments are:
;;
;;1..An identifier representing a method name.
;;
;;2..A syntax  object representing the  form which originated this  method dispatcher
;;   call.  It must be used as "form" value in "&syntax" condition object.
;;
;;When successful, the returned values must be:
;;
;;1..A syntax object representing an applicable method.
;;
;;2..A  "return-value-signature" object  or  false  if the  number  and  type of  the
;;   returned values is unknown.
;;
;;The dispatcher syntax is used to implement the expansions:
;;
;;   (O string) ==> (tag-dispatch <string> (<color>-string (tag-assert-and-return <color> O)))
;;   (O red)    ==> (tag-dispatch <fixnum> (<color>-red    (tag-assert-and-return <color> O)))
;;   (O green)  ==> (tag-dispatch <fixnum> (<color>-green  (tag-assert-and-return <color> O)))
;;   (O blue)   ==> (tag-dispatch <fixnum> (<color>-blue   (tag-assert-and-return <color> O)))
;;   (O shift 1 2 3) ==> (<color>-shift O 1 2 3)
;;
;;and its implementation can be:
#|
   (define (dispatcher method-id input-form-stx)
     (case (syntax->datum method-id)
       ((string)
        (values #'<color>-string #'<string>))
       ((shift)
        (values #'<color>-shift #'<color>))
       (else
        (values #f #f))))
|#
;;If the method name is not recognised the return values must be #f; in this case:
;;
;;1..If there are  no additional arguments in the dispatcher  syntax: the method name
;;   is handed to the mutator maker first (if any).
;;
;;2..If there are additional arguments in the dispatcher syntax, or the mutator maker
;;   returns  #f:  the method  name  is  handed  to the  parent's  "object-type-spec"
;;   dispatcher (if any).
;;
;;3..And so on recursively in the hierarchy of "object-type-spec".
;;
;;If the method name is invalid for  some reason: the method dispatcher must raise an
;;exception with condition object "&syntax".
;;

(define-record (object-type-spec %make-object-type-spec object-type-spec?)
  ;;A type representing  the object type to which expressions  in syntax objects will
  ;;evaluate.  All the Scheme objects are meant to be representable with this type.
  ;;
  ;;Instances of  this type  are meant  to be compared  with EQ?,  with #f  acting as
  ;;wildcard: it represents any object type.
  ;;
  (type-id
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
   setter-maker
		;False or a setter maker procedure.
   dispatcher
		;False or a dispatcher procedure.
   parent-spec
		;False or an instance of  "object-type-spec" describing the parent of
		;this type.   Only "<top>" has this  field set to false;  every other
		;"object-type-spec" has a parent spec; "<top>" is the implicit parent
		;of all the type specs.
   ))

(case-define* make-object-type-spec
  (({type-id	identifier-bound?}
    {parent-id	identifier-bound?}
    {pred-stx	syntax-object?})
   (make-object-type-spec type-id parent-id pred-stx #f #f #f #f))

  (({type-id	identifier-bound?}
    {parent-id	identifier-bound?}
    {pred-stx	syntax-object?}
    {accessor	false-or-procedure?}
    {mutator	false-or-procedure?}
    {setter	false-or-procedure?}
    {dispatcher	false-or-procedure?})
   (let ((parent-spec (cond ((identifier-object-type-spec parent-id))
			    (else
			     ;;FIXME  When  Nausicaa  is  used and  the  record  type
			     ;;created  by   classes  is   selected  as   parent  the
			     ;;object-type-spec appears  to be unset.   (Marco Maggi;
			     ;;Sun Mar 16, 2014)
			     ;;
			     ;; (syntax-violation 'make-object-type-spec
			     ;;   "selected parent tag identifier has no object-type-spec"
			     ;;   parent-id)
			     ;; (debug-print type-id
			     ;; 		  parent-id
			     ;; 		  (identifier-bound? parent-id)
			     ;; 		  (and (identifier-bound? parent-id)
			     ;; 		       (property-list (id->label parent-id))))
			     #f))))
     (%make-object-type-spec type-id pred-stx accessor mutator setter dispatcher parent-spec))))

(define (false-or-object-type-spec? obj)
  (or (not obj)
      (object-type-spec? obj)))

;;; --------------------------------------------------------------------

(case-define* tag-identifier-accessor
  ((tag-id field-name-id safe-accessor?)
   (tag-identifier-accessor tag-id field-name-id safe-accessor? #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} safe-accessor? input-form-stx)
   ;;Given  a   tag  identifier   and  a   field  name:   search  the   hierarchy  of
   ;;object-type-spec associated to TAG-ID for an accessor of the selected field.  If
   ;;successful:  return  a syntax  object  representing  an expression  which,  when
   ;;evaluated, will  return the field  accessor; if no  accessor is found:  raise an
   ;;exception.
   ;;
   (let loop ((spec ($identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;object-type-specs  until an  object-type-spec without  parent has  been
	    ;;found.  The serach for the field accessor has failed.
	    (syntax-violation __who__
	      "object type does not provide selected field accessor"
	       input-form-stx field-name-id))
	   ((object-type-spec-accessor-maker spec)
	    => (lambda (accessor-maker)
		 (receive (accessor-stx rv-tag)
		     (accessor-maker field-name-id safe-accessor? input-form-stx)
		   (if accessor-stx
		       (values accessor-stx rv-tag)
		     ;;The field is unknown: try with the parent.
		     (loop (object-type-spec-parent-spec spec))))))
	   (else
	    ;;The object-type-spec has no accessor maker: try with the parent.
	    (loop (object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-mutator
  ((tag-id field-name-id safe-accessor?)
   (tag-identifier-mutator tag-id field-name-id safe-accessor? #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} safe-mutator? input-form-stx)
   ;;Given  a   tag  identifier   and  a   field  name:   search  the   hierarchy  of
   ;;object-type-spec associated to TAG-ID for an  mutator of the selected field.  If
   ;;successful:  return  a syntax  object  representing  an expression  which,  when
   ;;evaluated,  will return  the field  mutator; if  no mutator  is found:  raise an
   ;;exception.
   ;;
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field mutator has failed.
	    (syntax-violation __who__
	      "object type does not provide selected field mutator"
	      input-form-stx field-name-id))
	   ((object-type-spec-mutator-maker spec)
	    => (lambda (mutator-maker)
		 (receive (mutator-stx new-value-tag)
		     (mutator-maker field-name-id safe-mutator? input-form-stx)
		   (if mutator-stx
		       (values mutator-stx new-value-tag)
		     ;;The field is unknown: try with the parent.
		     (loop (object-type-spec-parent-spec spec))))))
	   (else
	    ;;The object-type-spec has no mutator maker: try with the parent.
	    (loop (object-type-spec-parent-spec spec)))))))

(case-define* tag-identifier-setter
  ((tag-id keys-stx)
   (tag-identifier-setter tag-id keys-stx #f))
  (({tag-id tag-identifier?} {keys-stx syntax-object?} input-form-stx)
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If  we   are  here:  we   have  traversed  upwards  the   hierarchy  of
	    ;;"object-type-specs" until an "object-type-spec" without parent has been
	    ;;found.  The serach for the field setter has failed.
	    (syntax-violation __who__
	      "object type does not provide setter syntax" input-form-stx))
	   ((object-type-spec-setter-maker spec)
	    => (lambda (setter-maker)
		 (receive (setter-stx new-value-tag)
		     (setter-maker keys-stx input-form-stx)
		   (if setter-stx
		       (values setter-stx new-value-tag)
		     ;;The keys are unknown: try with the parent.
		     (loop (object-type-spec-parent-spec spec))))))
	   (else
	    ;;The object-type-spec has no setter maker: try with the parent.
	    (loop (object-type-spec-parent-spec spec)))))))

(module (tag-identifier-dispatch)
  (define-fluid-override __who__
    (identifier-syntax 'tag-identifier-dispatch))

  (define* (tag-identifier-dispatch {tag tag-identifier?} {input-form-stx syntax-object?})
    ;;Apply the dispatcher of the "object-type-spec" from the property list of TAG to
    ;;the expression in INPUT-FORM-STX; return the fully expanded expression or raise
    ;;an exception.  We expect INPUT-FORM-STX to have the format:
    ;;
    ;;   (?expr ?arg0 ?arg ...)
    ;;
    ;;where ?EXPR is  an expression of type  TAG and ?ARG0 is  an identifier matching
    ;;the name of a method or field of TAG.
    ;;
    (if (tag-super-and-sub? <procedure> tag)
	input-form-stx
      (syntax-match input-form-stx ()
	((?expr ?arg0 ?arg* ...)
	 (identifier? ?arg0)
	 (%try-dispatcher (identifier-object-type-spec tag) input-form-stx))
	(_
	 (%error-invalid-tagged-syntax input-form-stx)))))

  (define (%try-dispatcher spec input-form-stx)
    (cond ((not spec)
	   (%error-invalid-tagged-syntax input-form-stx))
	  ((object-type-spec-dispatcher spec)
	   => (lambda (dispatcher)
		(syntax-match input-form-stx ()
		  ((?expr ?arg0 ?arg* ...)
		   (receive (method-stx rv-tag)
		       (dispatcher ?arg0 input-form-stx)
		     (if method-stx
			 (bless
			  `(,method-stx ,?expr ,?arg0 ,?arg*))
		       (%try-accessor spec input-form-stx)))))))
	  (else
	   (%try-accessor spec input-form-stx))))

  (define (%try-accessor spec input-form-stx)
    (define (%try-parent-dispatcher)
      (%try-dispatcher (object-type-spec-parent-spec spec) input-form-stx))
    (cond ((not spec)
	   (syntax-violation __who__ "invalid tagged"))
	  ((object-type-spec-accessor-maker spec)
	   => (lambda (accessor-maker)
		(syntax-match input-form-stx ()
		  ((?expr ?field-name)
		   (receive (accessor-stx rv-tag)
		       (accessor-maker ?field-name #t input-form-stx)
		     (if accessor-stx
			 (bless
			  `(,accessor-stx ,?expr))
		       ;;No field matched, try the parent's dispatcher.
		       (%try-parent-dispatcher))))
		  (else
		   ;;The  syntax  is  invalid  for the  accessor,  try  the  parent's
		   ;;dispatcher.
		   (%try-parent-dispatcher)))))
	  (else
	   ;;There is no accessor maker, try the parent's dispatcher.
	   (%try-parent-dispatcher))))

  (define (%error-invalid-tagged-syntax input-form-stx)
    (syntax-violation __who__ "invalid tagged syntax" input-form-stx))

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
      (free-id=? <top> super-tag)
      (let ((pspec ($object-type-spec-parent-spec ($identifier-object-type-spec sub-tag))))
	(and pspec
	     (let ((sub-ptag ($object-type-spec-type-id pspec)))
	       (and (not (free-id=? <top> sub-ptag))
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

(define* (set-identifier-callable-signature! {binding-id identifier?} {signature callable-signature?})
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

(define* (set-label-callable-signature! {label symbol?} {signature callable-signature?})
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
