;;; -*- coding: utf-8-unix -*-
;;;
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
  (syntax-object.type-signature?
   syntax-object.typed-argument?			syntax-object.parse-typed-argument
   syntax-object.parse-standard-formals			syntax-object.parse-typed-formals
   syntax-object.parse-standard-list-of-bindings	syntax-object.parse-typed-list-of-bindings
   syntax-object.standard-formals?			syntax-object.typed-formals?
   #| end of exports |# )


;;;; type signature syntaxes predicates

(case-define* syntax-object.type-signature?
  ;;Return true  if STX  is a  syntax object  representing the  type signature  of an
  ;;expression's return values; otherwise return false.   The return value is true if
  ;;STX is null or  a proper or improper list of type  identifiers, with a standalone
  ;;type identifier being acceptable if it is "<list>" or one of its sub-types.
  ;;
  ;;Examples:
  ;;
  ;;   (syntax-object.type-signature? #'<no-return>)			=> #t
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
   (syntax-match stx (<no-return> <list> list-of)
     (<no-return>
      #t)
     (<list>
      #t)
     ((list-of ?type-annotation)
      (syntax-object.type-annotation? ?type-annotation))
     (else
      (let recur ((stx stx))
	(syntax-match stx (<list> list-of)
	  (() #t)
	  (<list>
	   #t)
	  ((list-of ?type-annotation)
	   (syntax-object.type-annotation? ?type-annotation))
	  (?rest-id
	   (identifier? ?rest-id)
	   ;;This is to allow type identifiers defined as:
	   ;;
	   ;;   (define-type <list-of-fixnums> (list-of <fixnum>))
	   ;;   (define-type <some-list> <list>)
	   ;;
	   (let ((ots (id->object-type-specification __who__ #f ?rest-id lexenv)))
	     (or (<list>-ots? ots)
		 (list-of-type-spec? ots))))
	  ((?thing . ?rest)
	   (and (syntax-object.type-annotation? ?thing)
		(recur ?rest)))
	  (_ #f)))))))


;;;; tagged binding parsing: standalone identifiers

(case-define syntax-object.typed-argument?
  ;;Return true if STX is a syntax object representing a typed or untyped identifier;
  ;;otherwise return false.
  ;;
  ((stx)
   (syntax-object.typed-argument? stx (current-inferior-lexenv)))
  ((stx lexenv)
   (guard (E (else #f))
     (receive (id type)
	 (syntax-object.parse-typed-argument stx lexenv)
       #t))))

(case-define* syntax-object.parse-typed-argument
  ;;If  STX  is a  typed  or  untyped identifier,  return  2  values: the  identifier
  ;;representing  the syntactic  binding name  and the  "<object-type-spec>" instance
  ;;representing the  type; otherwise raise an  exception.  When no type  is present:
  ;;the type defaults to "<top>".
  ;;
  ((stx)
   (syntax-object.parse-typed-argument stx (current-inferior-lexenv)))
  ((stx lexenv)
   (syntax-match stx (brace)
     ((brace ?id ?type)
      (values ?id (type-annotation->object-type-specification ?type lexenv)))
     (?id
      (identifier? ?id)
      (values ?id (<top>-ots))))))


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
  ;;Assume BINDING* is  a syntax object representing a proper  list of possibly typed
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
	      (with-exception-handler
		  (lambda (E)
		    (raise (condition (make-who-condition __who__)
				      (make-message-condition "invalid typed binding")
				      E)))
		(lambda ()
		  (type-annotation->object-type-specification ?tag lexenv ?tag)))
	      (receive (id* tag*)
		  (recur ?other-id*)
		(values (cons ?id id*) (cons ?tag tag*)))))
	   ((?id . ?other-id*)
	    (identifier? ?id)
	    (receive (id* tag*)
		(recur ?other-id*)
	      (values (cons ?id id*) (cons (<top>-type-id) tag*))))
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
    (map (lambda (x) (<top>-type-id)) item*))
  (define (%validate-standard-formals standard-formals.stx %synner)
    (cond ((duplicate-bound-formals? standard-formals.stx)
	   => (lambda (duplicate-id)
		(%synner "duplicate identifiers in formals specification" duplicate-id)))))
  (syntax-match formals.stx (brace)
    (?args-id
     (identifier? ?args-id)
     (values ?args-id (<list>-type-id)))

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
	       (append (%one-untyped-for-each ?arg*) (<list>-type-id)))))

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
       (identifier? ?args-id)
       (if (let ((ots (type-annotation->object-type-specification ?args-tag (current-inferior-lexenv) ?args-tag)))
	     (or (<list>-ots? ots)
		 (list-of-type-spec? ots)))
	   (values ?args-id ?args-tag)
	 (%synner "expected \"<list>\" or \"(list-of ?type)\" as type annotation for the args argument" formals.stx)))

      ;;Standard formals, UNtyped args as in: (lambda args ---)
      (?args-id
       (identifier? ?args-id)
       (values ?args-id (<list>-type-id)))

      ;;Non-standard formals: possibly typed arguments with typed rest argument.
      ((?arg* ... . (brace ?rest-id ?rest-tag))
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (begin
		 (unless (identifier? ?rest-id)
		   (%synner "invalid rest argument specification" (list (brace-id) ?rest-id ?rest-tag)))
		 (unless (let ((ots (type-annotation->object-type-specification ?rest-tag (current-inferior-lexenv) ?rest-tag)))
			   (or (<list>-ots? ots)
			       (list-of-type-spec? ots)))
		   (%synner "expected \"<list>\" or \"(list-of ?type)\" as type annotation for the args argument"
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
	 (values formals.stx (append (%one-untyped-for-each ?arg*) (<list>-type-id)))))

      ;;Non-standard formals: possibly typed identifiers with UNtyped rest argument.
      ((?arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (receive-and-return (standard-formals.stx type-signature.stx)
	   (let process-next-arg ((?arg* ?arg*))
	     (if (pair? ?arg*)
		 (%process-arg* ?arg* process-next-arg input-form.stx %synner)
	       (if (identifier? ?rest-id)
		   (values ?rest-id (<list>-type-id))
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
	   (values (cons ?id standard-formals.stx) (cons (<top>-type-id) type-signature.stx)))
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
    (map (lambda (x) (<top>-type-id)) item*))

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
;; End:
