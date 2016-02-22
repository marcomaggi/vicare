;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module PSYNTAX-TYPE-SYNTAX-OBJECTS
    (
     syntax-object.typed-argument?			syntax-object.parse-typed-argument

     syntax-object.type-signature?			syntax-object.type-signature.single-identifier?
     syntax-object.type-signature.fully-untyped?	syntax-object.type-signature.partially-untyped?
     syntax-object.type-signature.untyped?		syntax-object.type-signature.no-return?
     syntax-object.type-signature.super-and-sub?
     syntax-object.type-signature.common-ancestor
     syntax-object.type-signature.min-and-max-count

     syntax-object.parse-standard-formals		syntax-object.parse-typed-formals
     syntax-object.parse-standard-list-of-bindings	syntax-object.parse-typed-list-of-bindings
     syntax-object.standard-formals?			syntax-object.typed-formals?

     #| end of exports |# )

(import PSYNTAX-TYPE-IDENTIFIERS)


;;;; type signature syntaxes predicates

(case-define syntax-object.type-signature?
  ;;Return true  if STX  is a  syntax object  representing the  type signature  of an
  ;;expression's return values; otherwise return false.   The return value is true if
  ;;STX is null or  a proper or improper list of type  identifiers, with a standalone
  ;;type identifier being acceptable if it is "<list>" or one of its sub-types.
  ;;
  ;;Examples:
  ;;
  ;;   (syntax-object.type-signature? #'<list>)				=> #t
  ;;   (syntax-object.type-signature? #'())				=> #t
  ;;   (syntax-object.type-signature? #'(<fixnum> <string>))		=> #t
  ;;   (syntax-object.type-signature? #'(<fixnum> <string> . <list>))	=> #t
  ;;
  ;;A standalone "<list>" identifier means: any number of values of any type.
  ;;
  ((stx)
   (syntax-object.type-signature? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx (<list> <no-return>)
     (() #t)
     ((?id . ?rest)
      (and (type-identifier? ?id lexenv)
	   (syntax-object.type-signature? ?rest lexenv)))
     (<list>
      #t)
     (<no-return>
      #t)
     (?rest
      (identifier? ?rest)
      (type-identifier-is-list-sub-type? ?rest lexenv))
     (_ #f))))

;;; --------------------------------------------------------------------

(define (syntax-object.type-signature.single-identifier? stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature of an expression returning a single value; otherwise return false.
  ;;
  (syntax-match stx ()
    ((?type)	#t)
    (else	#f)))

(define* (syntax-object.type-signature.fully-untyped? stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature of an expression returning any number of values of any types; otherwise
  ;;return false.
  ;;
  ;;In other words, return true if STX is a standalone "<list>".
  ;;
  (list-type-id? stx))

(define* (syntax-object.type-signature.no-return? stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature  of   an  expression  that,   rather  than  returning,  will   raise  a
  ;;non-continuable exception.
  ;;
  ;;In other words, return true if STX is a standalone "<no-return>".
  ;;
  (no-return-type-id? stx))

;;; --------------------------------------------------------------------

(case-define* syntax-object.type-signature.partially-untyped?
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.   Return true  if  STX  is a  syntax  object  representing the  type
  ;;signature of  an expression's  return values, and  at least one  of the  types is
  ;;unspecified; otherwise return false.
  ;;
  ;;In other words, return true if at least  one type identifier is "<top>" or STX is
  ;;an improper list with "<list>" or one of its sub-types in "improper" position.
  ;;
  ((stx)
   (syntax-object.type-signature.partially-untyped? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (let loop ((stx stx)
	      (rv  #f))
     (syntax-match stx (<top> <list>)
       (() rv)
       ((<top> . ?rest)
	(loop ?rest #t))
       ((?id . ?rest)
	(type-identifier? ?id lexenv)
	(loop ?rest rv))
       (<list>
	#t)
       (?rest
	(and (type-identifier-is-list-sub-type? ?rest lexenv)
	     rv))
       ))))

(case-define* syntax-object.type-signature.untyped?
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.  Return  true if STX  is an "untyped"  type signature: all  the type
  ;;identifiers are either "<top>" or "<list>"; otherwise return false.
  ;;
  ((stx)
   (syntax-object.type-signature.untyped? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (define-syntax-rule (recur ?stx)
     (syntax-object.type-signature.untyped? ?stx lexenv))
   (syntax-match stx (<top> <list>)
     (()			#t)
     ((<top>     . ?rest)	(recur ?rest))
     ((?id   . ?rest)		#f)
     (<list>			#t)
     (?rest			#f))))

;;; --------------------------------------------------------------------

(define* (syntax-object.type-signature.min-and-max-count stx)
  ;;The argument STX must be a  syntax object representing a type signature according
  ;;to  SYNTAX-OBJECT.TYPE-SIGNATURE?, otherwise  the behaviour  of this  function is
  ;;unspecified.  Return two non-negative real  numbers: the minimum number of values
  ;;that can match  the type signature; the  maximum number of values  that can match
  ;;the type signature, possibly infinite.
  ;;
  (let recur ((stx  stx)
	      (min  0)
	      (max  0))
    (syntax-match stx ()
      (()
       (values min max))
      ((?type . ?rest)
       (recur ?rest (fxadd1 min) (fxadd1 max)))
      (?type
       (values min +inf.0)))))

;;; --------------------------------------------------------------------

(case-define* syntax-object.type-signature.super-and-sub?
  ;;The  arguments   SUPER-SIGNATURE  and   SUB-SIGNATURE  must  be   syntax  objects
  ;;representing   type   signatures  according   to   SYNTAX-OBJECT.TYPE-SIGNATURE?,
  ;;otherwise the behaviour of this function is unspecified.
  ;;
  ;;Return true if: SUPER-SIGNATURE and  SUB-SIGNATURE have compatible structure; the
  ;;type identifiers from  SUPER-SIGNATURE are super-types of  the corresponding type
  ;;identifiers from SUB-SIGNATURE.  Otherwise return false.
  ;;
  ((super-signature sub-signature)
   (syntax-object.type-signature.super-and-sub? super-signature sub-signature (current-inferior-lexenv)))
  ((super-signature sub-signature lexenv)
   (define-syntax-rule (recur ?super ?sub)
     (syntax-object.type-signature.super-and-sub? ?super ?sub lexenv))
   (syntax-match super-signature (<top> <list>)
     (()
      (syntax-match sub-signature ()
	;;Both the signatures are proper lists with the same number of items, and all
	;;the items are correct super and sub: success!
	(() #t)
	;;The signatures do not match.
	(_  #f)))

     ((<top> . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     ((?super-type . ?super-rest-types)
      (syntax-match sub-signature ()
	((?sub-type . ?sub-rest-types)
	 (type-identifier-super-and-sub? ?super-type ?sub-type lexenv)
	 (recur ?super-rest-types ?sub-rest-types))
	(_ #f)))

     (<list>
      ;;The super signature is an improper list accepting any object as rest.
      #t)

     (?super-rest-type
      (type-identifier-is-list-sub-type? ?super-rest-type lexenv)
      (let ((item-id (typed-list-type-spec.type-id (id->object-type-specification __who__ #f ?super-rest-type lexenv))))
	(or (top-type-id? item-id)
	    (syntax-match sub-signature (<list>)
	      ;;The super  signature is an improper  list with rest item  and the sub
	      ;;signature is finished.  We want the following signatures to match:
	      ;;
	      ;;  super-signature == #'(<number>  <fixnum> . <list>)
	      ;;  sub-signature   == #'(<complex> <fixnum>)
	      ;;
	      ;;because "<list>" in rest position means  any number of objects of any
	      ;;type.
	      (() #t)

	      ;;The  super  signature  is  an  improper list  shorter  than  the  sub
	      ;;signature.  We want the following signatures to match:
	      ;;
	      ;;  super-signature == #'(<number>  . <list-of-fixnums>)
	      ;;  sub-signature   == #'(<complex> <fixnum> <fixnum>)
	      ;;
	      ((?sub-type . ?sub-rest-types)
	       (and (type-identifier-super-and-sub? item-id ?sub-type lexenv)
		    (recur ?super-rest-type ?sub-rest-types)))

	      (<list>
	       ;;Both  the signatures  are improper  lists  with the  same number  of
	       ;;items, and all the items are  correct super and sub.  The rest types
	       ;;are mismatching.
	       #f)

	      ;;Both the signatures are improper lists with the same number of items,
	      ;;and all the  items are correct super  and sub; if the  rest types are
	      ;;proper super and  subs: success!  For example, we  want the following
	      ;;signatures to match:
	      ;;
	      ;;  super-signature == #'(<string> <string> . <list-of-numbers>)
	      ;;  sub-signature   == #'(<string> <string> . <list-of-fixnums>)
	      ;;
	      (?sub-rest-type
	       (type-identifier-is-list-sub-type? ?sub-rest-type lexenv)
	       (type-identifier-super-and-sub? ?super-rest-type ?sub-rest-type lexenv))
	      ))))
     )))

;;; --------------------------------------------------------------------

(case-define syntax-object.type-signature.common-ancestor
  ;;Given two syntax objects representing type signatures: return a new syntax object
  ;;representing the type signature being their common ancestor.  Examples:
  ;;
  ;;  (syntax-object.type-signature.common-ancestor #'(<fixnum>) #'(<fixnum>))
  ;;  => #'(<fixnum>)
  ;;
  ;;  (syntax-object.type-signature.common-ancestor #'(<fixnum>) #'(<flonum>))
  ;;  => #'(<real>)
  ;;
  ;;  (syntax-object.type-signature.common-ancestor #'(<fixnum>) #'(<string>))
  ;;  => #'(<top>)
  ;;
  ;;NOTE The arguments  SIG1 and SIG2 are  *not* validated here: they  must have been
  ;;previously validated.
  ;;
  ((sig1 sig2)
   (syntax-object.type-signature.common-ancestor sig1 sig2 (current-inferior-lexenv)))
  ((sig1 sig2 lexenv)
   (define-syntax-rule (recur ?sig1 ?sig2)
     (syntax-object.type-signature.common-ancestor ?sig1 ?sig2 lexenv))
   (syntax-match sig1 ()
     (()
      (syntax-match sig2 ()
	;;Both  the signatures  are  proper  lists with  the  same  number of  items:
	;;success!
	(() '())
	;;SIG1 is a proper list shorter that SIG2.
	(_
	 (list-type-id))))

     ((?type1 . ?rest1)
      (syntax-match sig2 ()
	;;SIG2 is a proper list shorter that SIG1.
	(()
	 (list-type-id))
	;;SIG1 and SIG2 have matching type identifiers ?TYPE1 and ?TYPE2.
	((?type2 . ?rest2)
	 (cons (type-identifier-common-ancestor ?type1 ?type2 lexenv)
	       (recur ?rest1 ?rest2)))
	;;SIG2 is an improper list shorter that SIG1.
	(?rest2
	 (list-type-id))))

     (?rest1
      (syntax-match sig2 ()
	;;Both SIG1 and SIG2 are improper lists with the same number of items.
	(?rest2
	 (identifier? ?rest2)
	 (type-identifier-common-ancestor ?rest1 ?rest2 lexenv))
	(_
	 (list-type-id))))
     )))


;;;; tagged binding parsing: standalone identifiers

(define (syntax-object.typed-argument? stx)
  ;;Return true if STX is a syntax object representing a typed or untyped identifier;
  ;;otherwise return false.
  ;;
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (type-identifier? ?tag)))
    (?id
     (identifier? ?id))))

(case-define* syntax-object.parse-typed-argument
  ;;If  STX  is a  typed  or  untyped identifier,  return  2  values: the  identifier
  ;;representing the syntactic binding name and the identifier representing the type;
  ;;otherwise  raise an  exception.  When  no type  is present:  the type  identifier
  ;;defaults to "<top>".
  ;;
  ((stx)
   (syntax-object.parse-typed-argument stx (current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx (brace)
     ((brace ?id ?tag)
      (begin
	;;We retrieve the object-type specification to  validate ?TAG: if ?TAG is not
	;;a type identifier, an exception is raised.
	(id->object-type-specification __who__ stx ?tag lexenv)
	(values ?id ?tag)))
     (?id
      (identifier? ?id)
      (values ?id (top-type-id))))))


;;;; standard binding parsing: proper lists of bindings left-hand sides

(case-define* syntax-object.parse-standard-list-of-bindings
  ;;Parser function  for lists of standard  syntactic bindings.  It is  used to parse
  ;;bindings from  LET, DO  and similar  syntaxes.  For  example, when  expanding the
  ;;syntax:
  ;;
  ;;   (let ((a 1)
  ;;         (b "b")
  ;;         (c #t))
  ;;     . ?body)
  ;;
  ;;the argument BINDING* is:
  ;;
  ;;   (#'a #'b #'c)
  ;;
  ;;and the return value is:
  ;;
  ;;   (#'a #'b #'c)
  ;;
  ;;Assume BINDING* is a syntax object representing a proper list of standard binding
  ;;identifiers; parse  the list and a  list of identifiers representing  the binding
  ;;identifiers.  The identifiers must be distinct.
  ;;
  ((binding*)
   (syntax-object.parse-standard-list-of-bindings binding* #f))
  ((binding* input-form.stx)
   (define (%error message)
     (syntax-violation __who__ message (or input-form.stx binding*) (if input-form.stx binding* #f)))
   (define lexenv
     (current-inferior-lexenv))
   (receive-and-return (id*)
       (let recur ((bind* binding*))
	 (syntax-match bind* (brace)
	   (() '())
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (cons ?id (recur ?other-id*)))
	   (_
	    (%error "invalid standard syntactic bindings syntax"))))
     (unless (distinct-bound-ids? id*)
       (%error "duplicate identifiers in syntactic bindings specification")))))


;;;; tagged binding parsing: proper lists of bindings left-hand sides

(case-define* syntax-object.parse-typed-list-of-bindings
  ;;Parser  function for  lists of  typed syntactic  bindings.  It  is used  to parse
  ;;bindings from  LET, DO  and similar  syntaxes.  For  example, when  expanding the
  ;;syntax:
  ;;
  ;;   (let (({a <fixnum>} 1)
  ;;         ({b <string>} "b")
  ;;         (c            #t))
  ;;     . ?body)
  ;;
  ;;the argument BINDING* is:
  ;;
  ;;   (#'(brace a <fixnum>) #'(brace b <string>) #'c)
  ;;
  ;;and the return values are:
  ;;
  ;;   (#'a #'b #'c) (#'<fixnum> #'<string> #'<top>)
  ;;
  ;;Assume BINDING* is a syntax object  representing a proper list of possibly tagged
  ;;binding identifiers;  parse the list and  return 2 values: a  list of identifiers
  ;;representing the binding identifiers, a list of identifiers representing the type
  ;;tags; "<top>" is used when no tag is present.  The identifiers must be distinct.
  ;;
  ((binding*)
   (syntax-object.parse-typed-list-of-bindings binding* #f))
  ((binding* input-form.stx)
   (define (%error message)
     (syntax-violation __who__ message (or input-form.stx binding*) (if input-form.stx binding* #f)))
   (define lexenv
     (current-inferior-lexenv))
   (receive-and-return (id* tag*)
       (let recur ((bind* binding*))
	 (syntax-match bind* (brace)
	   (()
	    (values '() '()))
	   (((brace ?id ?tag) . ?other-id*)
	    (begin
	      (id->object-type-specification __who__ input-form.stx ?tag lexenv)
	      (receive (id* tag*)
		  (recur ?other-id*)
		(values (cons ?id id*) (cons ?tag tag*)))))
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (receive (id* tag*)
		(recur ?other-id*)
	      (values (cons ?id id*) (cons (top-type-id) tag*))))
	   (_
	    (%error "invalid tagged bindings syntax"))))
     (unless (distinct-bound-ids? id*)
       (%error "duplicate identifiers in bindings specification")))))


;;;; standard binding parsing: standard LAMBDA formals

(define* (syntax-object.parse-standard-formals formals.stx input-form.stx)
  ;;Parse the  given syntax  object as standard  (untyped) LET-VALUES  formals (these
  ;;formals are  equal to the ones  of standard lambda clauses).   Test for duplicate
  ;;bindings.  If the syntax is invalid: raise an exception.
  ;;
  ;;When successful return the following values:
  ;;
  ;;1. A  proper or improper list  of identifiers representing the  standard formals.
  ;;It is the argument FORMALS.STX itself, but fully unwrapped.
  ;;
  ;;2.   A  syntax   object   representing   the  type   signature   as  defined   by
  ;;SYNTAX-OBJECT.TYPE-SIGNATURE?.
  ;;
  ;;NOTE We return two values (including FORMALS.STX  itself) to make the API of this
  ;;function equal to  the one of SYNTAX-OBJECT.PARSE-TYPED-FORMALS, so  that the two
  ;;can be used as:
  ;;
  ;;   (if (options::strict-r6rs)
  ;;       (syntax-object.parse-standard-formals formals.stx input-form.stx)
  ;;     (syntax-object.parse-typed-formals formals.stx input-form.stx))
  ;;
  ;;it makes the code simpler to read.  (Marco Maggi; Wed Feb  3, 2016)
  ;;
  (define-syntax __func_who__
    (identifier-syntax (quote syntax-object.parse-standard-formals)))
  (define (%synner message subform)
    (syntax-violation __func_who__ message input-form.stx subform))
  (define (%one-untyped-for-each item*)
    (map (lambda (x) (top-type-id)) item*))
  (define (%validate-standard-formals standard-formals.stx %synner)
    (cond ((duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(%synner "duplicate identifiers in formals specification" duplicate-id)))))
  (syntax-match formals.stx (brace)
    (?args-id
     (identifier? ?args-id)
     (values ?args-id (list-type-id)))

    ((?arg* ...)
     (for-all identifier? ?arg*)
     (begin
       (%validate-standard-formals ?arg* %synner)
       (values ?arg* (%one-untyped-for-each ?arg*))))

    ((?arg* ... . ?rest-id)
     (and (for-all identifier? ?arg*)
	  (identifier? ?rest-id))
     (begin
       (%validate-standard-formals (append ?arg* ?rest-id) %synner)
       ;;These APPEND applications return an improper list.
       (values (append ?arg* ?rest-id)
	       (append (%one-untyped-for-each ?arg*) (list-type-id)))))

    (_
     (%synner "invalid standard formals specification" formals.stx))))

(define (syntax-object.standard-formals? stx)
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
     (syntax-object.standard-formals? ?rest))
    (?rest
     (identifier? ?rest))
    ))


;;;; tagged binding parsing: typed LAMBDA formals

(module (syntax-object.parse-typed-formals)
  ;;Parse a  syntax object as  possibly typed  LET-VALUES formals (these  formals are
  ;;different from  the one  of lambda  clauses because they  have no  return values'
  ;;types).   Test for  duplicate  bindings.   If the  syntax  is  invalid: raise  an
  ;;exception.
  ;;
  ;;When successful return the following values:
  ;;
  ;;1. A proper or improper list of identifiers representing the standard formals.
  ;;
  ;;2.   A  syntax   object   representing   the  type   signature   as  defined   by
  ;;SYNTAX-OBJECT.TYPE-SIGNATURE?.
  ;;
  (define-module-who syntax-object.parse-typed-formals)

  (define (syntax-object.parse-typed-formals formals.stx input-form.stx)
    (define (%synner message subform)
      (syntax-violation __module_who__ message input-form.stx subform))
    (syntax-match formals.stx (brace)

      ;;Non-standard formals: typed args, as in: (lambda (brace args <list>) ---)
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (if (type-identifier-is-list-or-list-sub-type? ?args-tag)
	   (values ?args-id ?args-tag)
	 (%synner "expected \"<list>\" or its sub-type as type identifier for the args argument" formals.stx)))

      ;;Standard formals, UNtyped args as in: (lambda args ---)
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (list-type-id)))

      ;;Non-standard formals: possibly typed arguments with typed rest argument.
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (begin
		 (unless (and (identifier? ?rest-id)
			      (identifier? ?rest-tag))
		   (%synner "invalid rest argument specification" (list (brace-id) ?rest-id ?rest-tag)))
		 (unless (type-identifier-is-list-or-list-sub-type? ?rest-tag)
		   (%synner "expected \"<list>\" or its sub-type as type identifier for the rest argument"
			    (list (brace-id) ?rest-id ?rest-tag)))
		 (values ?rest-id ?rest-tag))))
	 (%validate-standard-formals standard-formals.stx %synner)))

      ;;Standard formals: UNtyped identifiers without rest argument.
      ((?arg* ...)
       (for-all identifier? ?arg*)
       (begin
	 (%validate-standard-formals ?arg* %synner)
	 (values ?arg* (%one-untyped-for-each ?arg*))))

      ;;Standard formals: UNtyped identifiers with UNtyped rest argument.
      ((?arg* ... . ?rest-id)
       (and (for-all identifier? ?arg*)
	    (identifier? ?rest-id))
       (begin
	 (%validate-standard-formals (append ?arg* ?rest-id) %synner)
	 (values formals.stx (append (%one-untyped-for-each ?arg*) (list-type-id)))))

      ;;Non-standard formals: possibly typed identifiers with UNtyped rest argument.
      ((?arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (if (identifier? ?rest-id)
		   (values ?rest-id (list-type-id))
		 (%synner "invalid rest argument specification" ?rest-id))))
	 (%validate-standard-formals standard-formals.stx %synner)))

      ;;Non-standard formals: possibly typed identifiers without rest argument.
      ;;
      ((?arg* ...)
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (values '() '())))
	 (%validate-standard-formals standard-formals.stx %synner)))

      (_
       (%synner "invalid formals specification" formals.stx))))

  (define (%process-arg* arg*.stx process-next-arg input-form.stx %synner)
    (receive (standard-formals.stx type-signature.stx)
	(process-next-arg (cdr arg*.stx))
      (let ((arg.stx (car arg*.stx)))
	(syntax-match arg.stx (brace)
	  ;;Untyped argument.
	  (?id
	   (identifier? ?id)
	   (values (cons ?id standard-formals.stx) (cons (top-type-id) type-signature.stx)))
	  ;;Typed argument.
	  ((brace ?id ?tag)
	   (and (identifier? ?id)
		(identifier? ?tag))
	   (begin
	     (id->object-type-specification __module_who__ input-form.stx ?tag (current-inferior-lexenv))
	     (values (cons ?id standard-formals.stx) (cons ?tag type-signature.stx))))
	  (else
	   (%synner "invalid argument specification" arg.stx))))))

  (define (%validate-standard-formals standard-formals.stx %synner)
    (cond ((duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(%synner "duplicate identifiers in formals specification" duplicate-id)))))

  (define (%one-untyped-for-each item*)
    (map (lambda (x) (top-type-id)) item*))

  #| end of module |# )

(define (syntax-object.typed-formals? formals.stx)
  ;;Return true  if FORMALS.STX is a  syntax object representing valid  typed formals
  ;;for a LAMBDA or LET-VALUES syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals.stx formals-signature.stx)
	(syntax-object.parse-typed-formals formals.stx #f)
      #t)))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
