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


;;;; identifiers: tagged identifiers handling

(define (tagged-identifier-syntax? x)
  (syntax-match x (brace)
    ((brace ?id ?tag)
     (and (identifier? ?id)
	  (identifier? ?tag)))
    (?id
     (identifier? ?id))
    ))

(define (parse-tagged-identifier-syntax stx)
  (syntax-match stx (brace)
    ((brace ?id ?tag)
     (begin
       (assert-tag-identifier? ?tag)
       (values ?id ?tag)))
    (?id
     (identifier? ?id)
     (values ?id <top>))
    ))

;;; --------------------------------------------------------------------

(case-define* parse-tagged-bindings-syntax
  ((stx)
   (parse-tagged-bindings-syntax stx #f))
  ((stx input-form-stx)
   ;;Assume STX is a syntax object representing a proper list of possibly
   ;;tagged binding  identifiers; parse the  list and return 2  values: a
   ;;list of identifiers representing the  binding identifiers, a list of
   ;;identifiers and  #f representing the type  tags; #f is used  when no
   ;;tag is present.
   ;;
   (define (%parse bind*)
     (syntax-match bind* (brace)
       (()
	(values '() '()))
       (((brace ?id ?tag) . ?other-id*)
	(begin
	  (assert-tag-identifier? ?tag)
	  (receive (id* tag*)
	      (%parse ?other-id*)
	    (values (cons ?id id*) (cons ?tag tag*)))))
       ((?id . ?other-id*)
	(identifier? ?id)
	(receive (id* tag*)
	    (%parse ?other-id*)
	  (values (cons ?id id*) (cons <top> tag*))))
       (_
	(if input-form-stx
	    (syntax-violation __who__ "invalid tagged bindings syntax" input-form-stx stx)
	  (syntax-violation __who__ "invalid tagged bindings syntax" stx)))))
   (receive-and-return (id* tag*)
       (%parse stx)
     (unless (distinct-bound-ids? id*)
       (if input-form-stx
	   (syntax-violation __who__
	     "duplicate identifiers in bindings specification"
	     input-form-stx stx)
	 (syntax-violation __who__
	   "duplicate identifiers in bindings specification"
	   stx))))))

(define* (tagged-bindings-syntax? lhs*)
  ;;Return  true if  lhs*  is  a list  of  possibly tagged  identifiers;
  ;;otherwise return false.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (id* tag*)
	(parse-tagged-bindings-syntax lhs*)
      #t)))

;;; --------------------------------------------------------------------

(module (parse-tagged-lambda-formals-syntax)
  ;;Given  a syntax  object  representing tagged  LAMBDA formals:  split
  ;;formals from tags.  Do *not* test for duplicate bindings.
  ;;
  ;;We  use the  conventions: ?ID,  ?REST-ID and  ?ARGS-ID are  argument
  ;;identifiers;  ?PRED  is  a  predicate  identifier.   We  accept  the
  ;;following standard formals formats:
  ;;
  ;;   ?args-id
  ;;   (?id ...)
  ;;   (?id0 ?id ... . ?rest-id)
  ;;
  ;;and in addition the following tagged formals:
  ;;
  ;;   (brace ?args-id ?args-pred)
  ;;   (?arg ...)
  ;;   (?arg0 ?arg ... . ?rest-arg)
  ;;   ((brace _ ?rv-tag0 ?rv-tag ...) ?arg ...)
  ;;   ((brace _ ?rv-tag0 ?rv-tag ...) ?arg ... . ?rest-arg)
  ;;
  ;;where ?ARG is a tagged argument with one of the formats:
  ;;
  ;;   ?arg-id
  ;;   (brace ?arg-id ?arg-tag)
  ;;
  ;;and the first item with identifier _ is a special syntax that allows
  ;;to specify the tags of the LAMBDA return values.
  ;;
  ;;Return 2 values:
  ;;
  ;;1.  A  proper  or  improper list  of  identifiers  representing  the
  ;;   standard formals.
  ;;
  ;;2. An object representing the LAMBDA tagging signature.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'parse-tagged-lambda-formals-syntax))

  (case-define* parse-tagged-lambda-formals-syntax
    (({_ standard-lambda-formals? function-tagging-signature?} original-formals-stx)
     (parse-tagged-lambda-formals-syntax original-formals-stx #f))
    (({_ standard-lambda-formals? function-tagging-signature?} original-formals-stx input-form-stx)
     ;;First we  parse and  extract the return  values tagging,  if any;
     ;;then we parse the rest of the formals.
     (syntax-match original-formals-stx (brace _)
       ;;With return values tagging.
       (((brace _ ?rv-tag0 ?rv-tag* ...) . ?formals)
	(let ((rv-tag* (cons ?rv-tag0 ?rv-tag*)))
	  (for-each assert-tag-identifier? rv-tag*)
	  (receive (standard-formals formals-tags)
	      (%parse-formals input-form-stx original-formals-stx ?formals)
	    (values standard-formals (cons rv-tag* formals-tags)))))
       ;;Without return values tagging.
       (?formals
	(receive (standard-formals formals-tags)
	    (%parse-formals input-form-stx original-formals-stx ?formals)
	  (values standard-formals (cons '()                formals-tags)))))))

  (define (%parse-formals input-form-stx original-formals-stx formals-stx)
    (syntax-match formals-stx (brace)

      ;;Tagged args, as in:
      ;;
      ;;   (lambda {args list-of-fixnums} . ?body)
      ;;
      ((brace ?args-id ?args-tag)
       (and (identifier? ?args-id)
	    (identifier? ?args-tag))
       (begin
	 (assert-tag-identifier? ?args-tag)
	 (values ?args-id ?args-tag)))

      ;;Possibly tagged identifiers with tagged rest argument, as in:
      ;;
      ;;   (lambda (?arg ... . {rest list-of-fixnums}) . ?body)
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
      ;;   (lambda (?arg ... . rest) . ?body)
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

(define* (tagged-lambda-formals-syntax? formals-stx)
  ;;Return true  if FORMALS-STX  is a  syntax object  representing valid
  ;;tagged formals for a LAMBDA syntax.
  ;;
  (guard (E ((syntax-violation? E)
	     #f))
    (receive (standard-formals signature-tags)
	(parse-tagged-lambda-formals-syntax formals-stx)
      #t)))

(define (standard-lambda-formals? stx)
  ;;Return true  if STX  is a syntax  object representing  R6RS standard
  ;;LAMBDA formals; otherwise return false.  The return value is true if
  ;;STX is a  proper or improper list of identifiers,  with a standalone
  ;;identifier being acceptable.
  ;;
  (syntax-match stx ()
    (()
     #t)
    ((?id . ?rest)
     (identifier? ?id)
     (standard-lambda-formals? ?rest))
    (?rest
     (identifier? ?rest)
     #t)))


;;;; identifiers: expand-time object type specification

(define-record (object-type-spec %make-object-type-spec object-type-spec?)
  ;;A type representing  the object type to which  expressions in syntax
  ;;objects  will evaluate.   All the  Scheme  objects are  meant to  be
  ;;representable with this type.
  ;;
  ;;Instances of  this type are meant  to be compared with  EQ?, with #f
  ;;acting as wildcard: it represents any object type.
  ;;
  (type-id
		;The  bound identifier  representing  the  name of  this
		;type.  This  identifier has  this very instance  in its
		;syntactic binding property list.
   pred-id
		;An identifier bound to the type predicate.
   accessor-maker
		;False  or  an  accessor  maker  procedure  accepting  2
		;arguments and returning 1 value.
		;
		;* The arguments are:  an identifier representing a slot
		;  name;  a boolean, true  if the requested  accessor is
		;  safe, false if it is unsafe.
		;
		;* The return  value is a syntax object  evaluating to a
		;  slot accessor, or false if this object-type-spec does
		;  not provide an accessor for the selected slot.
		;
		;If  the  slot name  is  invalid  for some  reason  (for
		;example the slot is hidden): the proceure must raise an
		;exception.  If this object-type-spec does not implement
		;the selected slot:  the return value must  be false and
		;the   slot    will   be   searched   in    the   parent
		;object-type-spec, if any.
   mutator-maker
		;False  or   a  mutator  maker  procedure   accepting  2
		;arguments and returning 1 value.
		;
		;* The arguments are:  an identifier representing a slot
		;  name;  a boolean,  true if  the requested  mutator is
		;  safe, false  if it is unsafe.
		;
		;* The return  value is a syntax object  evaluating to a
		;  slot mutator, or  false if this object-type-spec does
		;  not provide a mutator for the selected slot.
		;
		;If  the  slot name  is  invalid  for some  reason  (for
		;example the slot is immutable): the proceure must raise
		;an  exception.   If   this  object-type-spec  does  not
		;implement the  selected slot: the return  value must be
		;false  and the  slot  will be  searched  in the  parent
		;object-type-spec, if any.
   dispatcher
		;False  or a  dispatcher  procedure  accepting a  single
		;argument and returning 1 value, and acting like a macro
		;transformer.
		;
		;The argument must be  a syntax object representing with
		;the format:
		;
		;   (?var ?arg0 ?arg ...)
		;
		;where  ?VAR is  an  expression evaluating  to a  single
		;object whose type is  described by this structure.  The
		;return value  must be a syntax  object representing the
		;output form: an expression to be further expanded.
		;
   parent-spec
		;False or an instance of object-type-spec describing the
		;parent of this type.
   ))

(case-define* make-object-type-spec
  (({type-id identifier?} {pred-id identifier?})
   (%make-object-type-spec type-id pred-id #f #f #f #f))

  (({type-id identifier?} {pred-id identifier?}
    {accessor-maker false-or-procedure?}
    {mutator-maker  false-or-procedure?})
   (%make-object-type-spec type-id pred-id accessor-maker mutator-maker #f #f))

  (({type-id identifier?} {pred-id identifier?}
    {accessor-maker false-or-procedure?}
    {mutator-maker  false-or-procedure?}
    {dispatcher     false-or-procedure?}
    {parent-id      false-or-identifier?})
;; (debug-print type-id pred-id accessor-maker mutator-maker parent-id
;; 	     (and parent-id (identifier-object-type-spec parent-id)))
   (let ((parent-spec (cond ((not parent-id)
			     #f)
			    ((identifier-object-type-spec parent-id))
			    (else
			     ;;FIXME  When  Nausicaa  is  used  and  the
			     ;;record   type  created   by  classes   is
			     ;;selected  as parent  the object-type-spec
			     ;;appears to  be unset.  (Marco  Maggi; Sun
			     ;;Mar 16, 2014)
			     ;;
			     ;; (syntax-violation 'make-object-type-spec
			     ;;   "selected parent tag identifier has no object-type-spec"
			     ;;   parent-id)
			     #f))))
     (%make-object-type-spec type-id pred-id accessor-maker mutator-maker dispatcher parent-spec))))

(define (false-or-object-type-spec? obj)
  (or (not obj)
      (object-type-spec? obj)))

;;; --------------------------------------------------------------------

(define-constant *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*
  'vicare:expander:object-type-spec)

(define* (set-identifier-object-type-spec! {type-id identifier?} {spec object-type-spec?})
  (if (syntactic-binding-getprop type-id *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
      (syntax-violation __who__
	"object specification already defined" type-id spec)
    (syntactic-binding-putprop type-id *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE* spec)))

(define* ({identifier-object-type-spec false-or-object-type-spec?} {type-id identifier?})
  (syntactic-binding-getprop type-id *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*))

(define* (set-label-object-type-spec! {label symbol?} {spec object-type-spec?})
  (cond ((getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*)
	 => (lambda (old-spec)
	      (syntax-violation __who__
		"object specification already defined" label old-spec spec)))
	(else
	 (putprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE* spec))))

(define* ({label-object-type-spec false-or-object-type-spec?} {label symbol?})
  (getprop label *EXPAND-TIME-OBJECT-TYPE-SPEC-COOKIE*))

;;; --------------------------------------------------------------------

(define (tag-identifier? obj)
  ;;Return true if  OBJ is an identifier  with object-type-spec property
  ;;set; otherwise return false.
  ;;
  (and (identifier? obj)
       (and (identifier-object-type-spec obj)
	    #t)))

(define (assert-tag-identifier? obj)
  (unless (tag-identifier? obj)
    (syntax-violation #f
      "expected tag identifier, identifier with object-type-spec set" obj)))

;;; --------------------------------------------------------------------

(case-define* identifier-object-type-spec-accessor
  ((tag-id field-name-id safe-accessor?)
   (identifier-object-type-spec-accessor tag-id field-name-id safe-accessor? #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} safe-accessor? input-form-stx)
   ;;Given a  tag identifier and a  field name: search the  hierarchy of
   ;;object-type-spec  associated  to  TAG-ID  for an  accessor  of  the
   ;;selected field.  If successful: return a syntax object representing
   ;;an  expression  which,  when   evaluated,  will  return  the  field
   ;;accessor; if no accessor is found: raise an exception.
   ;;
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If we are here: we have traversed upwards the hierarchy of
	    ;;object-type-specs until an object-type-spec without parent
	    ;;has been  found.  The  serach for  the field  accessor has
	    ;;failed.
	    (syntax-violation __who__
	      "object type does not provide selected field accessor"
	       input-form-stx field-name-id))
	   ((object-type-spec-accessor-maker spec)
	    => (lambda (accessor-maker)
		 (or (accessor-maker field-name-id safe-accessor?)
		     ;;The field is unknown: try with the parent.
		     (loop (object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec  has no accessor maker:  try with the
	    ;;parent.
	    (loop (object-type-spec-parent-spec spec)))))))

(case-define* identifier-object-type-spec-mutator
  ((tag-id field-name-id safe-accessor?)
   (identifier-object-type-spec-mutator tag-id field-name-id safe-accessor? #f))
  (({tag-id tag-identifier?} {field-name-id identifier?} safe-mutator? input-form-stx)
   ;;Given a  tag identifier and a  field name: search the  hierarchy of
   ;;object-type-spec  associated  to  TAG-ID  for  an  mutator  of  the
   ;;selected field.  If successful: return a syntax object representing
   ;;an expression which, when evaluated, will return the field mutator;
   ;;if no mutator is found: raise an exception.
   ;;
   (let loop ((spec (identifier-object-type-spec tag-id)))
     (cond ((not spec)
	    ;;If we are here: we have traversed upwards the hierarchy of
	    ;;object-type-specs until an object-type-spec without parent
	    ;;has  been found.   The serach  for the  field mutator  has
	    ;;failed.
	    (syntax-violation __who__
	      "object type does not provide selected field mutator"
	      input-form-stx field-name-id))
	   ((object-type-spec-mutator-maker spec)
	    => (lambda (mutator-maker)
		 (or (mutator-maker field-name-id safe-mutator?)
		     ;;The field is unknown: try with the parent.
		     (loop (object-type-spec-parent-spec spec)))))
	   (else
	    ;;The object-type-spec  has no  mutator maker: try  with the
	    ;;parent.
	    (loop (object-type-spec-parent-spec spec)))))))

;;; --------------------------------------------------------------------

(define (initialise-type-spec-for-built-in-object-types)
  (define (%register name-sym pred-sym)
    (let ((name-id (scheme-stx name-sym))
	  (pred-id (scheme-stx pred-sym)))
      (set-identifier-object-type-spec! name-id
	(make-object-type-spec name-id pred-id))))
  (%register '&condition				'condition?)
  (%register '&message					'message-condition?)
  (%register '&warning					'warning?)
  (%register '&serious					'serious-condition?)
  (%register '&error					'error?)
  (%register '&violation				'violation?)
  (%register '&assertion				'assertion-violation?)
  (%register '&irritants				'irritants-condition?)
  (%register '&who					'who-condition?)
  (%register '&non-continuable				'non-continuable-violation?)
  (%register '&implementation-restriction		'implementation-restriction-violation?)
  (%register '&lexical					'lexical-violation?)
  (%register '&syntax					'syntax-violation?)
  (%register '&undefined				'undefined-violation?)
  (%register '&i/o					'i/o-error?)
  (%register '&i/o-read					'i/o-read-error?)
  (%register '&i/o-write				'i/o-write-error?)
  (%register '&i/o-invalid-position			'i/o-invalid-position-error?)
  (%register '&i/o-filename				'i/o-filename-error?)
  (%register '&i/o-file-protection			'i/o-file-protection-error?)
  (%register '&i/o-file-is-read-only			'i/o-file-is-read-only-error?)
  (%register '&i/o-file-already-exists			'i/o-file-already-exists-error?)
  (%register '&i/o-file-does-not-exist			'i/o-file-does-not-exist-error?)
  (%register '&i/o-port					'i/o-port-error?)
  (%register '&i/o-decoding				'i/o-decoding-error?)
  (%register '&i/o-encoding				'i/o-encoding-error?)
  (%register '&i/o-eagain				'i/o-eagain-error?)
  (%register '&errno					'errno-condition?)
  (%register '&out-of-memory-error			'out-of-memory-error?)
  (%register '&h_errno					'h_errno-condition?)
  (%register '&no-infinities				'no-infinities-violation?)
  (%register '&no-nans					'no-nans-violation?)
  (%register '&interrupted				'interrupted-condition?)
  (%register '&source-position				'source-position-condition?)
  (%register '&procedure-argument-violation		'procedure-argument-violation?)
  (%register '&expression-return-value-violation	'expression-return-value-violation?)
  (void))


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
  (if (syntactic-binding-getprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE*)
      (syntax-violation __who__
	"callable specification already defined" type-id spec)
    (syntactic-binding-putprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE* spec)))

(define* (identifier-callable-spec {type-id identifier?})
  (syntactic-binding-getprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE*))


;;;; identifiers: expand-time binding type tagging

(define-constant *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE*
  'vicare:expander:binding-type-tagging)

(define-constant *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE*
  'vicare:expander:binding-function-signature)

;;; --------------------------------------------------------------------

(define* (set-label-type-tagging! {label symbol?} {tag identifier?})
  ;;Given a  syntactic binding LABEL:  add TAG  to its property  list as
  ;;binding type  tagging.  This  tag should  represent the  object type
  ;;referenced by the binding.
  ;;
  (cond ((getprop label *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE*)
	 => (lambda (old-tag)
	      (syntax-violation __who__
		"label binding tag already defined" label old-tag tag)))
	(else
	 (putprop label *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE* tag))))

(define* (label-type-tagging {label identifier?})
  ;;Given a syntactic binding LABEL: retrieve from its property list the
  ;;identifier  representing   the  binding  type  tagging.    This  tag
  ;;identifier  should  represent  the  object type  referenced  by  the
  ;;binding.
  ;;
  (getprop label *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-identifier-type-tagging! {binding-id identifier?} {tag identifier?})
  ;;Given a syntactic  binding identifier: add TAG to  its property list
  ;;as binding type tagging.  This  tag should represent the object type
  ;;referenced by the binding.
  ;;
  (cond ((syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE*)
	 => (lambda (old-tag)
	      (syntax-violation __who__
		"identifier binding tag already defined"
		binding-id old-tag tag)))
	(else
	 (syntactic-binding-putprop binding-id *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE* tag))))

(define* (identifier-type-tagging {binding-id identifier?})
  ;;Given  a syntactic  binding identifier:  retrieve from  its property
  ;;list the identifier representing the binding type tagging.  This tag
  ;;identifier  should  represent  the  object type  referenced  by  the
  ;;binding.
  ;;
  (syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-TYPE-TAGGING-COOKIE*))

(define* (identifier-with-tagging? {id identifier?})
  ;;Return #t  if ID is an  identifier having a type  tagging; otherwise
  ;;return false.  If the return value is true: ID is a bound identifier
  ;;created by some binding syntaxes (define, let, letrec, ...).
  ;;
  (and (identifier-type-tagging id)
       #t))

(define* (identifier-with-tagging-dispatcher? {id identifier?})
  ;;Return #t if ID  is an identifier having a type  tagging and the tag
  ;;identifier  has a  dispatcher transformer  in its  object-type-spec;
  ;;otherwise return false.  If the return  value is true: ID is a bound
  ;;identifier created  by some  binding syntaxes (define,  let, letrec,
  ;;...)  and it can be used in forms like:
  ;;
  ;;   (?id ?arg ...)
  ;;
  (cond ((identifier-type-tagging id)
	 => (lambda (tag-id)
	      (let ((spec (identifier-object-type-spec tag-id)))
		(and spec
		     (object-type-spec-dispatcher spec)
		     #t))))
	(else #f)))

(define* (identifier-tagging-apply-dispatcher {id identifier-with-tagging-dispatcher?}
					      input-form-stx)
  (let* ((tag-id (identifier-type-tagging id))
	 (spec   (identifier-object-type-spec tag-id)))
    ((object-type-spec-dispatcher spec) input-form-stx)))

;;; --------------------------------------------------------------------

(define* (set-label-function-signature! {label symbol?} {signature-tags function-tagging-signature?})
  ;;Given  a  syntactic  binding  LABEL  for  a  function  binding:  add
  ;;SIGNATURE-TAGS  to  its  property  list  as  type  tagging  for  the
  ;;function.
  ;;
  (cond ((getprop label *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE*)
	 => (lambda (old-signature-tags)
	      (syntax-violation __who__
		"label binding function tagging signature already defined"
		label signature-tags signature-tags)))
	(else
	 (putprop label *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE* signature-tags))))

(define* (label-function-signature {label identifier?})
  ;;Given a  syntactic binding  LABEL for  a function  binding: retrieve
  ;;from  its  property list  the  tagging  signature of  the  function.
  ;;Return false if no signature is defined.
  ;;
  (getprop label *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-identifier-function-signature! {binding-id identifier?} {signature-tags function-tagging-signature?})
  ;;Given a  syntactic binding  identifier for  a function  binding: add
  ;;SIGNATURE-TAGS  to  its  property  list  as  type  tagging  for  the
  ;;function.
  ;;
  (cond ((syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE*)
	 => (lambda (old-signature-tags)
	      (syntax-violation __who__
		"identifier binding function tagging signature already defined"
		binding-id old-signature-tags signature-tags)))
	(else
	 (syntactic-binding-putprop binding-id *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE* signature-tags))))

(define* (identifier-function-signature {binding-id identifier?})
  ;;Given  a  syntactic  binding  identifier  for  a  function  binding:
  ;;retrieve  from  its  property  list the  tagging  signature  of  the
  ;;function.  Return false if no signature is defined.
  ;;
  (syntactic-binding-getprop binding-id *EXPAND-TIME-BINDING-FUNCTION-SIGNATURE-COOKIE*))

;;; --------------------------------------------------------------------

(define (function-tagging-signature? obj)
  ;;Return  true  if  OBJ   represents  a  function  tagging  signature;
  ;;otherwise return false.
  ;;
  ;;OBJ is a function tagging signature if it is a pair and:
  ;;
  ;;*  Its  car  is  null  or a  possibly  proper  list  of  identifiers
  ;;  representing the return value tags.
  ;;
  ;;* Its cdr  is a proper or improper list  of identifiers representing
  ;;  the tags of the arguments.
  ;;
  ;;Examples of functions and associated tagging signature:
  ;;
  ;;   (define (doit) ---)			==> (() . ())
  ;;   (define (doit . args) ---)		==> (() . <top>)
  ;;   (define (doit . {args <list>}) ---)	==> (() . <list>)
  ;;
  ;;   (define ({doit <fixnum>} {a <fixnum>})
  ;;     ---)
  ;;   ==> ((<fixnum>) . (<fixnum>))
  ;;
  ;;   (define ({doit <fixnum>} {a <fixnum>} {b <fixnum>})
  ;;     ---)
  ;;   ==> ((<fixnum>) . (<fixnum> <fixnum>))
  ;;
  ;;   (define ({doit <fixnum>} {a <fixnum>} {b <fixnum>} . rest)
  ;;     ---)
  ;;   ==> ((<fixnum>) . (<fixnum> <fixnum> . <top>))
  ;;
  ;;   (define ({doit <fixnum>} {a <fixnum>} {b <fixnum>} . {rest <list>})
  ;;     ---)
  ;;   ==> ((<fixnum>) . (<fixnum> <fixnum> . <list>))
  ;;
  ;;   (define ({doit <fixnum> <flonum>} a)
  ;;     ---)
  ;;   ==> ((<fixnum> <flonum>) . (<top>))
  ;;
  (syntax-match obj ()
    ((?rv-tag* . ?formals-tags)
     (and (or (not ?rv-tag*)
	      (function-return-values-tags? ?rv-tag*))
	  (or (not ?formals-tags)
	      (function-arguments-tags?     ?formals-tags))))
    (_
     #f)))

(define (function-return-values-tags? rv-tag*)
  ;;Return  true  if RV-TAG*  represents  the  tags of  function  return
  ;;values; otherwise return false.
  ;;
  ;;RV-TAG* represents  the tags of  function return  values if it  is a
  ;;proper list of identifiers.
  ;;
  (all-identifiers? rv-tag*))

(define (function-arguments-tags? formals-tags)
  ;;Return  true  if  FORMALS-TAGS   represents  the  tags  of  function
  ;;arguments; otherwise return false.
  ;;
  ;;FORMALS-TAGS  represents the  tags of  function formals  if it  is a
  ;;proper or improper list of identifiers.
  ;;
  (let loop ((fmls formals-tags))
    (syntax-match fmls ()
      ((?tag . ?rest)
       (identifier? ?tag)
       (loop ?rest))
      (?rest
       (identifier? ?rest)
       #t)
      (() #t)
      (_  #f))))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
