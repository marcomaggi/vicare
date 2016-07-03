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


(module PSYNTAX-TYPE-SIGNATURES
  (
   <type-signature>
   <type-signature>-rtd					<type-signature>-rcd
   make-type-signature					type-signature?
   type-signature.object-type-specs			type-signature.object-type-specs-set!
   type-signature.syntax-object

   syntax-object->type-signature-specs			type-signature-specs?
   tail-type-annotation->object-type-spec		type-signature.syntax-object-list-and-rest
   syntax-object.type-signature?

;;; special constructors
   make-type-signature/single-void			make-type-signature/single-untyped
   make-type-signature/single-top
   make-type-signature/single-null			make-type-signature/single-list
   make-type-signature/single-boolean
   make-type-signature/single-true			make-type-signature/single-false
   make-type-signature/single-procedure			make-type-signature/single-symbol
   make-type-signature/single-stx			make-type-signature/single-syntactic-identifier
   make-type-signature/single-value
   make-type-signature/standalone-list			make-type-signature/fully-unspecified
   make-type-signature/no-return

;;; comparison
   type-signature=?

;;; predicates
   list-of-type-signatures?				type-signature.empty?
   type-signature.fully-unspecified?
   type-signature.matching-super-and-sub?		type-signature.compatible-super-and-sub?
   type-signature.single-type?				type-signature.single-top-tag?
   type-signature.single-type-or-fully-untyped?		type-signature.no-return?
   type-signature.only-<untyped>-and-<list>?
   type-signature.only-<untyped>-and-<top>-and-<list>?

   type-signature.match-formals-against-operands	type-signature.type-propagation

;;; accessors
   type-signature.min-count				type-signature.max-count
   type-signature.min-and-max-counts
   type-signature.untyped-to-top			type-signature.untyped-to-top!

   type-signature.common-ancestor			type-signature.union
   type-signature.union-same-number-of-operands
   datum-type-signature

;;; helpers
   case-type-signature-full-structure			case-type-signature-full-structure*
   case-type-signature-structure			case-type-signature-structure*
   single-value <list>/<list-of-spec> <list-of-spec> <list-spec> <closure> <pair-spec>

   #| end of exports |# )


;;;; syntax helpers: type signature specs matching

(define-auxiliary-syntaxes single-value <list>/<list-of-spec> <closure>
  <list-of-spec> <list-spec> <pair-spec>)

(define-syntax (case-type-signature-full-structure input-form.stx)
  ;;A typical use for an expression used as operand or similar:
  ;;
  ;;  (case-type-signature-full-structure expr.sig
  ;;    (<list>/<list-of-spec>
  ;;     ;;The expression returns an unspecified number of values.
  ;;     ?body0 ?body ...)
  ;;
  ;;    (<bottom>
  ;;     ;;The expression is typed as not-returning.
  ;;     ?body0 ?body ...)
  ;;
  ;;    ((<void>)
  ;;     ;;The expression is typed as returning void.
  ;;     ?body0 ?body ...)
  ;;
  ;;    ((single-value)
  ;;     ;;The expression returns a single value.
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    (else
  ;;     ;;The expression returns zero, two or more values.
  ;;     ?body0 ?body ...))
  ;;
  ;;A typical use for an expression used as operator in an application form:
  ;;
  ;;  (case-type-signature-full-structure expr.sig
  ;;    ((<closure>)
  ;;     ;;The expression returns a single value, typed as closure object.
  ;;     => (lambda (closure.ots) ?body0 ?body ...))
  ;;
  ;;    ((<procedure>)
  ;;     ;;The expression returns a single value, typed as "<procedure>".
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    (<bottom>
  ;;     ;;The expression is typed as not-returning.
  ;;     ?body0 ?body ...)
  ;;
  ;;    ((<void>)
  ;;     ;;The expression is typed as returning void.
  ;;     ?body0 ?body ...)
  ;;
  ;;    ((single-value)
  ;;     ;;The expression returns a single value, but not a procedure.
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    (<list>/<list-of-spec>
  ;;     ;;The expression returns an unspecified number of values.
  ;;     ?body0 ?body ...)
  ;;
  ;;    (else
  ;;     ;;The expression returns zero, two or more values.
  ;;     ?body0 ?body ...))
  ;;
  (case-define synner
    ((message)
     (sys::syntax-violation (quote case-type-signature-full-structure) message input-form.stx #f))
    ((message subform)
     (sys::syntax-violation (quote case-type-signature-full-structure) message input-form.stx subform)))

  (define (main input-form.stx)
    (sys::syntax-case input-form.stx ()
      ((_ ?signature . ?clause*)
       (sys::with-syntax
	   (((CLAUSE ...)	(%parse-clauses (sys::syntax ?clause*))))
	 (sys::syntax
	  (let* ((signature		?signature)
		 (signature.specs	(type-signature.object-type-specs signature))
		 (single-item?		(list-of-single-item? signature.specs)))
	    ;;
	    (let-syntax
		((declare (syntax-rules ()
			    ((_ ?who ?pred)
			     (define (?who)
			       (and single-item? (?pred (car signature.specs)))))
			    )))
	      (declare signature.single-void?		<void>-ots?)
	      (declare signature.single-top?		<top>-ots?)
	      (declare signature.single-untyped?	<untyped>-ots?)
	      (declare signature.single-procedure?	<procedure>-ots?)
	      (declare signature.single-closure?	closure-type-spec?)
	      #| end of LET-SYNTAX |# )
	    ;;
	    (let-syntax
		((declare (syntax-rules ()
			    ((_ ?who ?pred)
			     (define (?who)
			       (?pred signature.specs)))
			    )))
	      (declare signature.no-return?	<bottom>-ots?)
	      (declare signature.empty?		null?)
	      (declare signature.null?		<null>-ots?)
	      (declare signature.list?		<list>-ots?)
	      (declare signature.nelist?	<nelist>-ots?)
	      (declare signature.list-of-spec?	list-of-type-spec?)
	      (declare signature.list-spec?	list-type-spec?)
	      (declare signature.pair-spec?	pair-type-spec?)
	      #| end of LET-SYNTAX |# )
	    ;;
	    (define (signature.single-value?)
	      single-item?)
	    (define (signature.unspecified-values?)
	      (and (not single-item?)
		   (not (<bottom>-ots? signature.specs))
		   (not (<void>-ots?      signature.specs))
		   (or (list-of-type-spec? signature.specs)
		       (<list>-ots? signature.specs))))
	    ;;
	    (cond CLAUSE ...)))))
      ))

  (define (%parse-clauses clause*.stx)
    (sys::syntax-case clause*.stx (else)
      (((else ?body0 ?body ...))
       (sys::syntax
	((else ?body0 ?body ...))))
      ((?clause . ?clause*)
       (cons (%parse-single-clause	(sys::syntax ?clause))
	     (%parse-clauses		(sys::syntax ?clause*))))
      (()
       (synner "list of clauses must end with an ELSE clause"))
      (_
       (synner "invalid input clauses" clause*.stx))))

  (define (%parse-single-clause clause.stx)
    (sys::syntax-case clause.stx (=> single-value <list>/<list-of-spec> <null>
				     <top> <untyped> <closure> <procedure>
				     <bottom> <void> <list> <nelist>
				     <list-of-spec> <list-spec> <pair-spec>)
      ((() ?body0 ?body ...)
       (sys::syntax ((signature.empty?)			?body0 ?body ...)))

      ;;;

      (((<void>) ?body0 ?body ...)
       (sys::syntax ((signature.single-void?)		?body0 ?body ...)))

      (((<untyped>) ?body0 ?body ...)
       (sys::syntax ((signature.single-untyped?)	?body0 ?body ...)))

      (((<top>) ?body0 ?body ...)
       (sys::syntax ((signature.single-top?)		?body0 ?body ...)))

      (((<closure>) => ?body)
       (sys::syntax ((signature.single-closure?)	(?body (car signature.specs)))))
      (((<closure>) ?body0 ?body ...)
       (sys::syntax ((signature.single-closure?)	?body0 ?body ...)))

      (((<procedure>) ?body0 ?body ...)
       (sys::syntax ((signature.single-procedure?)	?body0 ?body ...)))

      (((single-value) => ?body)
       (sys::syntax ((signature.single-value?)		(?body (car signature.specs)))))
      (((single-value) ?body0 ?body ...)
       (sys::syntax ((signature.single-value?)		?body0 ?body ...)))

      ;;;

      ((<bottom> ?body0 ?body ...)
       (sys::syntax ((signature.no-return?)		?body0 ?body ...)))

      ((<list>/<list-of-spec> => ?body)
       (sys::syntax ((signature.unspecified-values?)	(?body signature.specs))))
      ((<list>/<list-of-spec> ?body0 ?body ...)
       (sys::syntax ((signature.unspecified-values?)	?body0 ?body ...)))

      ((<list-of-spec> => ?body)
       (sys::syntax ((signature.list-of-spec?)		(?body signature.specs))))
      ((<list-of-spec> ?body0 ?body ...)
       (sys::syntax ((signature.list-of-spec?)		?body0 ?body ...)))

      ((<list-spec> => ?body)
       (sys::syntax ((signature.list-spec?)		(?body signature.specs))))
      ((<list-spec> ?body0 ?body ...)
       (sys::syntax ((signature.list-spec?)		?body0 ?body ...)))

      ((<list> ?body0 ?body ...)
       (sys::syntax ((signature.list?)			?body0 ?body ...)))

      ((<null> ?body0 ?body ...)
       (sys::syntax ((signature.null?)			?body0 ?body ...)))

      ((<nelist> ?body0 ?body ...)
       (sys::syntax ((signature.nelist?)		?body0 ?body ...)))

      ((<pair-spec> => ?body)
       (sys::syntax ((signature.pair-spec?)		(?body signature.specs))))
      ((<pair-spec> ?body0 ?body ...)
       (sys::syntax ((signature.pair-spec?)		?body0 ?body ...)))

      (_
       (synner "invalid input clause" clause.stx))))

  (main input-form.stx))


;;;; syntax helpers: type annotation specs matching

(define-syntax (case-type-signature-structure input-form.stx)
  (define (main input-form.stx)
    (sys::syntax-case input-form.stx ()
      ((_ ?specification . ?clause*)
       (sys::with-syntax
	   (((CLAUSE ...)	(%parse-clauses (sys::syntax ?clause*))))
	 (sys::syntax
	  (let* ((specification ?specification)
		 (specification (if (label-type-spec? specification)
				    (object-type-spec.parent-ots specification)
				  specification)))
	    (cond CLAUSE ...)))))
       ))

  (define (%parse-clauses clause*.stx)
    (sys::syntax-case clause*.stx ()
      (()
       '())
      ((?clause0 . ?other-clauses)
       (cons (%parse-single-clause (sys::syntax ?clause0))
	     (%parse-clauses       (sys::syntax ?other-clauses))))
      (_
       (sys::syntax-violation __who__ "invalid syntax in input clauses" input-form.stx clause*.stx))))

  (define (%parse-single-clause clause.stx)
    (sys::syntax-case clause.stx (else => null? <null>
				       pair? <pair-spec>
				       <list> <nelist> <list-of-spec> <list-spec>
				       <list>/<list-of-spec>)
      ((else . ?body)
       clause.stx)

      ((null? . ?body)
       (sys::syntax ((null? specification) . ?body)))

      ((<null> => ?expr)
       (sys::syntax ((<null>-ots? specification) (?expr specification))))
      ((<null> . ?body)
       (sys::syntax ((<null>-ots? specification) . ?body)))

      ((pair? => ?expr)
       (sys::syntax ((pair? specification) (?expr specification))))
      ((pair? . ?body)
       (sys::syntax ((pair? specification) . ?body)))

      ((<pair-spec> => ?expr)
       (sys::syntax ((pair-type-spec? specification) (?expr specification))))
      ((<pair-spec> . ?body)
       (sys::syntax ((pair-type-spec? specification) . ?body)))

      ((<list> => ?expr)
       (sys::syntax ((<list>-ots? specification) (?expr specification))))
      ((<list> . ?body)
       (sys::syntax ((<list>-ots? specification) . ?body)))

      ((<nelist> => ?expr)
       (sys::syntax ((<nelist>-ots? specification) (?expr specification))))
      ((<nelist> . ?body)
       (sys::syntax ((<nelist>-ots? specification) . ?body)))

      ((<list-of-spec> => ?expr)
       (sys::syntax ((list-of-type-spec? specification) (?expr specification))))
      ((<list-of-spec> . ?body)
       (sys::syntax ((list-of-type-spec? specification) . ?body)))

      ((<list-spec> => ?expr)
       (sys::syntax ((list-type-spec? specification) (?expr specification))))
      ((<list-spec> . ?body)
       (sys::syntax ((list-type-spec? specification) . ?body)))

      ((<list>/<list-of-spec> => ?expr)
       (sys::syntax ((or (<list>-ots?     specification)
			 (list-of-type-spec? specification))
		     (?expr specification))))
      ((<list>/<list-of-spec> . ?body)
       (sys::syntax ((or (<list>-ots?     specification)
			 (list-of-type-spec? specification))
		     . ?body)))

      (_
       (sys::syntax-violation __who__ "invalid syntax in input clauses" input-form.stx clause.stx))))

  (main input-form.stx))


;;;; syntax helpers

(define-syntax case-type-signature-full-structure*
  ;;This is like CASE-TYPE-SIGNATURE-FULL-STRUCTURE but it has all the clauses.
  ;;
  (syntax-rules (<bottom>
		 single-value <void>
		 <list> <null> <list-of-spec>
		 <nelist> <pair-spec> <list-spec>
		 else)
    ((_ ?specs
	(<bottom>		. ?body-no-return)
	((<void>)		. ?body-single-void)
	((single-value)		. ?body-single-value)
	;;
	(<list>			. ?body-<list>)
	(<null>			. ?body-<null>)
	(<list-of-spec>		. ?body-<list-of-spec>)
	;;
	(<pair-spec>		. ?body-<pair-spec>)
	(<nelist>		. ?body-<nelist>)
	(<list-spec>		. ?body-<list-spec>)
	;;
	(else			. ?body-else))
     (case-type-signature-full-structure ?specs
	(<bottom>		. ?body-no-return)
	((<void>)		. ?body-single-void)
	((single-value)		. ?body-single-value)
	;;
	(<list>			. ?body-<list>)
	(<null>			. ?body-<null>)
	(<list-of-spec>		. ?body-<list-of-spec>)
	;;
	(<pair-spec>		. ?body-<pair-spec>)
	(<nelist>		. ?body-<nelist>)
	(<list-spec>		. ?body-<list-spec>)
	;;
	(else			. ?body-else)))
    ))

(define-syntax case-type-signature-structure*
  ;;This is like CASE-TYPE-SIGNATURE-STRUCTURE but it has all the clauses.
  ;;
  (syntax-rules (else null? pair?
		      <pair-spec> <null>
		      <list> <nelist> <list-of-spec> <list-spec>)
    ((_ ?specs
	(pair? . ?body-pair)
	(<pair-spec> . ?body-<pair-spec>)
	(null? . ?body-null)
	(<null> . ?body-<null>)
	(<list> . ?body-<list>)
	(<list-of-spec> . ?body-<list-of-spec>)
	(<nelist> . ?body-<nelist>)
	(<list-spec> . ?body-<list-spec>)
	(else . ?body-else))
     (case-type-signature-structure ?specs
       (pair? . ?body-pair)
       (<pair-spec> . ?body-<pair-spec>)
       (null? . ?body-null)
       (<null> . ?body-<null>)
       (<list> . ?body-<list>)
       (<list-of-spec> . ?body-<list-of-spec>)
       (<nelist> . ?body-<nelist>)
       (<list-spec> . ?body-<list-spec>)
       (else . ?body-else)))
    ))


;;;; type signature syntax object parser

(module (syntax-object->type-signature-specs
	 syntax-object.type-signature?
	 tail-type-annotation->object-type-spec
	 type-signature-specs?)

  (define* (syntax-object->type-signature-specs input-signature.stx lexenv synner)
    ;;Parse  the   syntax  object  INPUT-SIGNATURE.STX   and  as  a   type  signature
    ;;specification   and  convert   it   into   a  proper   or   improper  list   of
    ;;"<object-type-spec>"  instances.   If the  input  is  invalid: raise  a  syntax
    ;;violation.
    ;;
    ;;We can always use "(current-inferior-lexenv)" as value for the LEXENV argument.
    ;;
    ;;SYNNER must be a function called, in tail position, as:
    ;;
    ;;   (SYNNER ?message ?subform)
    ;;
    ;;for example:
    ;;
    ;;   (define (synner message subform)
    ;;     (syntax-violation #f message input-signature.stx subform))
    ;;
    ;;Examples of acceptable type signatures:
    ;;
    ;;   <bottom>
    ;;   <list>
    ;;   <nelist>
    ;;   <null>
    ;;   ()
    ;;   (list-of ?type)
    ;;   (list)
    ;;   (list ?type0 ?type ...)
    ;;   (pair ?car-type ?list-type-signature)
    ;;   (?car-type . ?list-type-signature)
    ;;
    (define (ta->ots type.stx)
      (try
	  (type-annotation->object-type-spec type.stx lexenv)
	(catch E
	  (else
	   (%synner type.stx)))))
    (define (%synner subform)
      (synner "invalid syntax object as type signature" subform))
    (syntax-match input-signature.stx (<bottom> <list> <nelist> <null> list list-of pair pair-of nelist-of)
      (<bottom>
       (<bottom>-ots))

      (<list>
       (<list>-ots))

      (<nelist>
       (<nelist>-ots))

      (<null>
       (<null>-ots))

      ((list)
       (<null>-ots))

      ((list-of ?type)
       (make-list-of-type-spec (ta->ots ?type)))

      ((list ?type ?type* ...)
       (make-list-type-spec (map ta->ots (cons ?type ?type*))))

      ((pair ?car ?cdr)
       (let ((cdr.ots (ta->ots ?cdr)))
	 (if (%acceptable-list-ots? cdr.ots)
	     (make-pair-type-spec (ta->ots ?car) cdr.ots)
	   (%synner ?cdr))))

      ((pair-of ?item)
       (let ((item.ots (ta->ots ?item)))
	 (if (%acceptable-list-ots? item.ots)
	     (make-pair-of-type-spec item.ots)
	   (%synner ?item))))

      ((nelist-of ?type)
       (let ((type.ots (ta->ots ?type)))
	 (make-pair-type-spec type.ots (make-list-of-type-spec type.ots))))

      (?type
       (identifier? ?type)
       ;;For example, here we want to allow syntactic identifiers bound with:
       ;;
       ;;   (define-type ?type <list>)
       ;;
       (let ((type.ots (ta->ots ?type)))
	 (if (or (<bottom>-ots?		type.ots)
		 (%acceptable-list-ots?	type.ots))
	     type.ots
	   (%synner ?type))))

      (()
       ;;This is good: an empty type signature means no values.
       '())

      ((?car . ?cdr)
       ;;INPUT-SIGNATURE.STX must be a proper or improper list of type annotations.
       (cons (ta->ots ?car)
	     (let recur ((stx ?cdr))
	       (syntax-match stx (<bottom> <list> <nelist> <null> list list-of pair pair-of nelist-of)
		 ((list-of ?type)
		  (make-list-of-type-spec (ta->ots ?type)))

		 ((list)
		  (<null>-ots))

		 ((list ?type ?type* ...)
		  (make-list-type-spec (map ta->ots (cons ?type ?type*))))

		 ((pair ?car ?cdr)
		  (let ((cdr.ots (ta->ots ?cdr)))
		    (if (%acceptable-list-ots? cdr.ots)
			(make-pair-type-spec (ta->ots ?car) cdr.ots)
		      (%synner ?cdr))))

		 ((pair-of ?item)
		  (let ((item.ots (ta->ots ?item)))
		    (if (%acceptable-list-ots? item.ots)
			(make-pair-of-type-spec (ta->ots ?item))
		      (%synner ?item))))

		 ((nelist-of ?type)
		  (let ((type.ots (ta->ots ?type)))
		    (make-pair-type-spec type.ots (make-list-of-type-spec type.ots))))

		 ((?car . ?cdr)
		  (cons (ta->ots ?car) (recur ?cdr)))

		 (()
		  '())

;;; the ones below are less probable

		 (?type
		  (identifier? ?type)
		  ;;For example, here we want to allow syntactic identifiers bound with:
		  ;;
		  ;;   (define-type ?type <list>)
		  ;;
		  (let ((type.ots (ta->ots ?type)))
		    (if (%acceptable-list-ots? type.ots)
			type.ots
		      (%synner ?type))))

		 (<list>
		  (<list>-ots))

		 (<nelist>
		  (<nelist>-ots))

		 (<null>
		  (<null>-ots))

		 (_
		  (%synner stx))))))

      (_
       (%synner #f))))

;;; --------------------------------------------------------------------

  (case-define* syntax-object.type-signature?
    ;;Return true  if STX is  a syntax object representing  the type signature  of an
    ;;expression's return values;  otherwise return false.  The return  value is true
    ;;if  STX is  null or  a proper  or  improper list  of type  identifiers, with  a
    ;;standalone type  identifier being acceptable  if it is  "<list>" or one  of its
    ;;sub-types.
    ;;
    ;;Examples:
    ;;
    ;;   (syntax-object.type-signature? #'<bottom>)			=> #t
    ;;   (syntax-object.type-signature? #'<list>)			=> #t
    ;;   (syntax-object.type-signature? #'())				=> #t
    ;;   (syntax-object.type-signature? #'(<fixnum> <string>))		=> #t
    ;;   (syntax-object.type-signature? #'(<fixnum> <string> . <list>))	=> #t
    ;;
    ;;A standalone "<list>" identifier means: any number of values of any type.
    ;;
    ((stx)
     (syntax-object.type-signature? stx (current-inferior-lexenv)))
    ((stx lexenv)
     (syntax-object->type-signature-specs stx lexenv (lambda (message subform) #f))))

;;; --------------------------------------------------------------------

  (case-define* tail-type-annotation->object-type-spec
    ;;Let's consider the following LAMBDA syntaxes:
    ;;
    ;;   (lambda/typed {args <list>}					?body)
    ;;   (lambda/typed {args (list-of <fixnum>)}			?body)
    ;;   (lambda/typed ({a <fixnum>} . {rest <list>})			?body)
    ;;   (lambda/typed ({a <fixnum>} . {rest (list-of <fixnum>)})	?body)
    ;;
    ;;the type  annotations for the  ARGS and REST  arguments are special:  they must
    ;;represent lists.   This function  parses such type  annotations and  returns an
    ;;instance of  "<object-type-spce>" representing it.   If the type  annotation is
    ;;invalid: an exception is raised.
    ;;
    ((annotation.stx lexenv)
     (tail-type-annotation->object-type-spec annotation.stx lexenv annotation.stx))
    ((annotation.stx lexenv name.stx)
     (define (ta->ots type.stx)
       (try
	   (type-annotation->object-type-spec type.stx lexenv)
	 (catch E
	   (else
	    (%synner type.stx)))))
     (define (%synner subform)
       (syntax-violation __who__ "invalid syntax object as type signature" annotation.stx subform))
     (syntax-match annotation.stx (<list> <nelist> <null> list list-of pair pair-of nelist-of)
       (<list>
	(<list>-ots))

       (<nelist>
	(<nelist>-ots))

       (<null>
	(<null>-ots))

       ((list-of ?type)
	(make-list-of-type-spec (ta->ots ?type)))

       ((list)
	(<null>-ots))

       ((list ?type ?type* ...)
	(make-list-type-spec (map ta->ots (cons ?type ?type*))))

       ((pair ?car ?cdr)
	(let ((cdr.ots (ta->ots ?car)))
	  (if (%acceptable-list-ots? cdr.ots)
	      (make-pair-type-spec (ta->ots ?car) cdr.ots)
	    (%synner ?cdr))))

       ((pair-of ?item)
	(let ((item.ots (ta->ots ?item)))
	  (if (%acceptable-list-ots? item.ots)
	      (make-pair-of-type-spec item.ots)
	    (%synner ?item))))

       ((nelist-of ?type)
	(let ((type.ots (ta->ots ?type)))
	  (make-pair-type-spec type.ots (make-list-of-type-spec type.ots))))

       (?type
	(identifier? ?type)
	;;For example, here we want to allow syntactic identifiers bound with:
	;;
	;;   (define-type ?type <list>)
	;;
	(let ((type.ots (ta->ots ?type)))
	  (if (%acceptable-list-ots? type.ots)
	      type.ots
	    (%synner ?type))))

       (()
	;;This is good: an empty type signature means no values.
	'())

       (else
	(%synner #f)))))

;;; --------------------------------------------------------------------

  (define* (type-signature-specs? input-signature.specs)
    ;;Return  true  if  INPUT-SIGNATURE.SPECS  is   a  proper  or  improper  list  of
    ;;"<object-type-spec>" instances representing a  valid type signature.  Otherwise
    ;;return false.
    ;;
    (cond ((pair? input-signature.specs)
	   (let loop ((specs (cdr input-signature.specs)))
	     (cond ((pair? specs)
		    (loop (cdr specs)))
		   ((null? specs))
		   ((%acceptable-list-ots? specs))
		   (else #f))))
	  ((<bottom>-ots?		input-signature.specs))
	  ((%acceptable-list-ots?	input-signature.specs))
	  ;;This is good: an empty type signature means no values.
	  ((null?			input-signature.specs))
	  (else				#f)))

;;; --------------------------------------------------------------------

  (define (%acceptable-list-ots? type.ots)
    (or (<list>-ots?		type.ots)
	(<nelist>-ots?		type.ots)
	(<null>-ots?		type.ots)
	(list-of-type-spec?	type.ots)
	(list-type-spec?	type.ots)
	(and (pair-type-spec?		type.ots) (%acceptable-list-ots? (pair-type-spec.cdr-ots      type.ots)))
	(and (pair-of-type-spec?	type.ots) (%acceptable-list-ots? (pair-of-type-spec.item-ots  type.ots)))
	(and (label-type-spec?		type.ots) (%acceptable-list-ots? (object-type-spec.parent-ots type.ots)))))

  #| end of module |# )


;;;; type signature: type definition

(define-core-record-type <type-signature>
  (nongenerative *0*vicare:expander:<type-signature>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (fields
    (mutable specs
	     type-signature.object-type-specs
	     type-signature.object-type-specs-set!)
		;A  proper   or  improper  list  of   "<object-type-spec>"  instances
		;representing the types of values matching this signature.
    (mutable memoised-tags			type-signature.memoised-tags		      type-signature.memoised-tags-set!)
    (mutable memoised-fully-untyped?		type-signature.memoised-fully-untyped?	      type-signature.memoised-fully-untyped?-set!)
    (mutable memoised-partially-untyped?	type-signature.memoised-partially-untyped?    type-signature.memoised-partially-untyped?-set!)
    (mutable memoised-untyped?			type-signature.memoised-untyped?	      type-signature.memoised-untyped?-set!)
    (mutable memoised-min-count			type-signature.memoised-min-count	      type-signature.memoised-min-count-set!)
		;Memoised minimum number of values matching this signature.
    (mutable memoised-max-count			type-signature.memoised-max-count	      type-signature.memoised-max-count-set!)
		;Memoised maximum number of values matching this signature.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-type-signature {specs type-signature-specs?})
	(make-record specs (void) (void) (void) (void) #f #f))
      make-type-signature))
  ;; (custom-printer
  ;;   (lambda (S port sub-printer)
  ;;     (sub-printer `(<type-signature> ,(type-signature.syntax-object S)))))
  (custom-printer
    (lambda (S port sub-printer)
      (define-syntax-rule (%display ?thing)
	(display ?thing port))
      (define-syntax-rule (%write ?thing)
	(write ?thing port))
      (%display "#[signature ")
      (%display (syntax->datum (type-signature.syntax-object S)))
      (%display "]"))))

(define-list-of-type-predicate list-of-type-signatures? type-signature?)


;;;; type signature: special constructors

(let*-syntax
    ((define-cached-signature-maker
       (syntax-rules ()
	 ((_ ?who ?ots-maker)
	  (define ?who
	    (let ((rvs #f))
	      (lambda ()
		(or rvs
		    (receive-and-return (S)
			(make-type-signature ?ots-maker)
		      (set! rvs S)))))))))
     (define-single-type-signature-maker
       (syntax-rules ()
	 ((_ ?who ?type-ots-maker)
	  (define-cached-signature-maker ?who (list (?type-ots-maker)))))))
  (define-single-type-signature-maker make-type-signature/single-void			<void>-ots)
  (define-single-type-signature-maker make-type-signature/single-untyped		<untyped>-ots)
  (define-single-type-signature-maker make-type-signature/single-top			<top>-ots)
  (define-single-type-signature-maker make-type-signature/single-null			<null>-ots)
  (define-single-type-signature-maker make-type-signature/single-list			<list>-ots)
  (define-single-type-signature-maker make-type-signature/single-boolean		<boolean>-ots)
  (define-single-type-signature-maker make-type-signature/single-true			<true>-ots)
  (define-single-type-signature-maker make-type-signature/single-false			<false>-ots)
  (define-single-type-signature-maker make-type-signature/single-symbol			<symbol>-ots)
  (define-single-type-signature-maker make-type-signature/single-procedure		<procedure>-ots)
  (define-single-type-signature-maker make-type-signature/single-stx			<stx>-ots)
  (define-single-type-signature-maker make-type-signature/single-syntactic-identifier	<syntactic-identifier>-ots)
  (define-cached-signature-maker make-type-signature/standalone-list			(<list>-ots))
  (define-cached-signature-maker make-type-signature/no-return				(<bottom>-ots))
  #| end of LET-SYNTAX |# )

(define-syntax-rule (make-type-signature/fully-unspecified)
  (make-type-signature/standalone-list))

(define* (make-type-signature/single-value {type-annotation object-type-spec?})
  (make-type-signature (list type-annotation)))


;;;; type signature: predicates

(define-syntax-rule (%type-signature-memoised-body ?signature ?memoised-getter ?memoised-setter . ?body)
  (let ((obj (?memoised-getter ?signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (begin . ?body)
	  (?memoised-setter ?signature bool))
      obj)))

(define* (type-signature.empty? {signature type-signature?})
  (null? (type-signature.object-type-specs signature)))

(define* (type-signature.fully-unspecified? {signature type-signature?})
  ;;Return true  if the type  signature specifies  neither object types,  nor objects
  ;;count; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-fully-untyped? type-signature.memoised-fully-untyped?-set!
   (<list>-ots? (type-signature.object-type-specs signature))))

(define* (type-signature.only-<untyped>-and-<list>? {signature type-signature?})
  ;;Return true if  the type signature has only untyped  items, either "<untyped>" or
  ;;"<list>"; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-untyped? type-signature.memoised-untyped?-set!
   (let loop ((specs (type-signature.object-type-specs signature)))
     (cond ((pair? specs)
	    (and (<untyped>-ots? (car specs))
		 (loop (cdr specs))))
	   ((null? specs)
	    ;;End of proper list.
	    #t)
	   ((<list>-ots? specs)
	    ;;End of improper list with an UNtyped OTS.
	    #t)
	   (else
	    ;;End of improper list with a typed OTS.
	    #f)))))

(define* (type-signature.only-<untyped>-and-<top>-and-<list>? {signature type-signature?})
  ;;Return true  if the type  signature has  only: "<untyped>", "<top>"  and "<list>"
  ;;items; otherwise return false.
  ;;
  (let loop ((specs (type-signature.object-type-specs signature)))
    (cond ((pair? specs)
	   (and (let ((ots (car specs)))
		  (or (<top>-ots?     ots)
		      (<untyped>-ots? ots)))
		(loop (cdr specs))))
	  ((null? specs)
	   ;;End of proper list.
	   #t)
	  ((<list>-ots? specs)
	   ;;End of improper list with an UNtyped OTS.
	   #t)
	  (else
	   ;;End of improper list with a typed OTS.
	   #f))))

(define* (type-signature.single-type? {signature type-signature?})
  ;;Return true if SIGNATURE represents a single value; otherwise return false.
  ;;
  (list-of-single-item? (type-signature.object-type-specs signature)))

(define* (type-signature.single-top-tag? {signature type-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.
  ;;
  (let ((specs (type-signature.object-type-specs signature)))
    (and (list-of-single-item? specs)
	 (<top>-ots? (car specs)))))

(define* (type-signature.single-type-or-fully-untyped? {signature type-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier, otherwise return false.
  ;;
  (let ((specs (type-signature.object-type-specs signature)))
    (or (list-of-single-item? specs)
	(<list>-ots? specs))))

(define* (type-signature.no-return? {signature type-signature?})
  (let ((specs (type-signature.object-type-specs signature)))
    (and (pair? specs)
	 (null? (cdr specs))
	 (<bottom>-ots? specs))))


;;;; type signature: accessors

(define* (type-signature.syntax-object {signature type-signature?})
  ;;Return a proper  or improper list of identifiers and  syntax objects representing
  ;;the names of the types in the signature.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-tags type-signature.memoised-tags-set!
   (let recur ((specs (type-signature.object-type-specs signature)))
     (cond ((pair? specs)
	    (cons (object-type-spec.name (car specs))
		  (recur (cdr specs))))
	   ((null? specs)
	    '())
	   (else
	    (object-type-spec.name specs))))))

(define* (type-signature.syntax-object-list-and-rest {sig type-signature?})
  ;;Return two values:
  ;;
  ;;1. A proper  list of syntax objects  representing the proper portion  of the type
  ;;signature.
  ;;
  ;;2.  Null  or a  syntax  object  representing the  improper  portion  of the  type
  ;;signature.
  ;;
  ;;Examples:
  ;;
  ;;  (type-signature.syntax-object-list-and-rest #[sig (<top> <top>)])
  ;;  => (<top> <top>) ()
  ;;
  ;;  (type-signature.syntax-object-list-and-rest #[sig (<top> <top> . <list>)])
  ;;  => (<top> <top>) <list>
  ;;
  ;;  (type-signature.syntax-object-list-and-rest #[sig <list>])
  ;;  => () <list>
  ;;
  ;;  (type-signature.syntax-object-list-and-rest #[sig ()])
  ;;  => () ()
  ;;
  (let loop ((stx*	'())
	     (specs	(type-signature.object-type-specs sig)))
    (cond ((pair? specs)
	   (loop (cons (object-type-spec.name (car specs)) stx*)
		 (cdr specs)))
	  ((null? specs)
	   (values (reverse stx*) '()))
	  (else
	   (assert (object-type-spec? specs))
	   (values (reverse stx*) (object-type-spec.name specs))))))

;;; --------------------------------------------------------------------

(define* (type-signature.min-count {signature type-signature?})
  ;;Return a non-negative  fixnum representing the minimum number of  values that can
  ;;match the signature.
  ;;
  (or (type-signature.memoised-min-count signature)
      (receive (min-count max-count)
	  (type-signature.min-and-max-counts signature)
	min-count)))

(define* (type-signature.max-count {signature type-signature?})
  ;;Return a non-negative  fixnum representing the maximum number of  values that can
  ;;match the signature; if the number of values is infinite: return +inf.0.
  ;;
  (or (type-signature.memoised-max-count signature)
      (receive (min-count max-count)
	  (type-signature.min-and-max-counts signature)
	max-count)))

(define (type-signature.min-and-max-counts signature)
  (receive-and-return (min-count max-count)
      (let recur ((specs	(type-signature.object-type-specs signature))
		  (min		0)
		  (max		0))
	(cond ((pair? specs)
	       (recur (cdr specs) (fxadd1 min) (fxadd1 max)))
	      ((null? specs)
	       ;;End of proper list.
	       (values min max))
	      (else
	       ;;End of IMproper list.
	       (values min +inf.0))))
    (type-signature.memoised-min-count-set! signature min-count)
    (type-signature.memoised-max-count-set! signature max-count)))

(define* (type-signature.untyped-to-top {sig type-signature?})
  (make-type-signature
   (let recur ((specs (type-signature.object-type-specs sig)))
     (cond ((pair? specs)
	    (cons (if (<untyped>-ots? (car specs))
		      (<top>-ots)
		    (car specs))
		  (recur (cdr specs))))
	   ((null? specs)
	    '())
	   (else
	    #;(assert (or (<list>-ots? specs) (list-of-type-spec? specs)))
	    specs)))))

(define* (type-signature.untyped-to-top! {sig type-signature?})
  (type-signature.object-type-specs-set!
   sig (let recur ((specs (type-signature.object-type-specs sig)))
	 (cond ((pair? specs)
		(cons (if (<untyped>-ots? (car specs))
			  (<top>-ots)
			(car specs))
		      (recur (cdr specs))))
	       ((null? specs)
		'())
	       (else
		#;(assert (or (<list>-ots? specs) (list-of-type-spec? specs)))
		specs)))))


;;;; matching: signatures exact matching

(define-equality/sorting-predicate type-signature=?	$type-signature=?	type-signature?)

(define ($type-signature=? signature1 signature2)
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (let loop ((specs1 (type-signature.object-type-specs signature1))
	     (specs2 (type-signature.object-type-specs signature2)))
    (cond ((pair? specs1)
	   (and (pair? specs2)
		(object-type-spec=? (car specs1) (car specs2))
		(loop               (cdr specs1) (cdr specs2))))
	  ((pair? specs2)
	   #f)
	  ((null? specs1)
	   (null? specs2))
	  ((null? specs2)
	   #f)
	  (else
	   (object-type-spec=? specs1 specs2)))))


;;;; matching: signatures hierarchy matching super-type and sub-type

(define* (type-signature.matching-super-and-sub? formals.sig operands.sig)
  ;;Return true if FORMALS.SIG and OPERANDS.SIG  have the same structure and the type
  ;;in  the homologous  positions  are matching  super-type  and sub-type;  otherwise
  ;;return false.
  ;;
  (%type-signature.criterion-super-and-sub? __who__ formals.sig operands.sig
					    object-type-spec.matching-super-and-sub?))

(define* (type-signature.compatible-super-and-sub? formals.sig operands.sig)
  ;;Return true if FORMALS.SIG and OPERANDS.SIG  have the same structure and the type
  ;;in the  homologous positions  are compatible  super-type and  sub-type; otherwise
  ;;return false.
  ;;
  (%type-signature.criterion-super-and-sub? __who__ formals.sig operands.sig
					    (lambda (formal.ots operand.ots)
					      (or (object-type-spec.matching-super-and-sub?   formal.ots operand.ots)
						  (object-type-spec.compatible-super-and-sub? formal.ots operand.ots)))))

(define (%type-signature.criterion-super-and-sub? caller-who formals.sig operands.sig
						  super-and-sub?)
  (let  ((formals.specs		(type-signature.object-type-specs formals.sig))
	 (operands.specs	(type-signature.object-type-specs operands.sig)))
    (cond
     ((<bottom>-ots? formals.specs)
      (<bottom>-ots? operands.specs))
     ((<bottom>-ots? operands.specs)
      #t)
     (else
      (let recur ((formals.specs	formals.specs)
		  (operands.specs	operands.specs))
	(case-type-signature-structure* formals.specs
	  (pair?
	   (case-type-signature-structure operands.specs
	     (pair?
	      ;;If the formal is actually a super-type of the operand: good.
	      (and (super-and-sub? (car formals.specs) (car operands.specs))
		   (recur (cdr formals.specs) (cdr operands.specs))))
	     (<pair-spec>
	      (and (super-and-sub? (car formals.specs) (pair-type-spec.car-ots operands.specs))
		   (recur (cdr formals.specs) (pair-type-spec.cdr-ots operands.specs))))
	     (<list-spec>
	      ;;Splice the operand OTSs.
	      (recur formals.specs (list-type-spec.item-ots* operands.specs)))
	     (else
	      ;;It is possible that there are more formals than operands.  Examples:
	      ;;
	      ;;  formals.sig  == (<number>  <fixnum> <string)
	      ;;  operands.sig == (<complex> <fixnum> . ())
	      ;;
	      ;;  formals.sig  == (<number>  <fixnum> <string)
	      ;;  operands.sig == (<complex> <fixnum> . <list>)
	      ;;
	      ;;  formals.sig  == (<number>  <fixnum> <string)
	      ;;  operands.sig == (<complex> <fixnum> . <null>)
	      ;;
	      ;;we want these cases to fail matching, in this function.
	      #f)))

	  (<pair-spec>
	   (let ((formal-car.ots (pair-type-spec.car-ots formals.specs))
		 (formal-cdr.ots (pair-type-spec.cdr-ots formals.specs)))
	     (case-type-signature-structure operands.specs
	       (pair?
		;;If the formal is actually a super-type of the operand good.
		(and (super-and-sub? formal-car.ots (car operands.specs))
		     (recur formal-cdr.ots (cdr operands.specs))))
	       (<pair-spec>
		(and (super-and-sub? formal-car.ots (pair-type-spec.car-ots operands.specs))
		     (recur formal-cdr.ots (pair-type-spec.cdr-ots operands.specs))))
	       (<list-spec>
		(let ((operand-item*.ots (list-type-spec.item-ots* operands.specs)))
		  (case-type-signature-structure operand-item*.ots
		    (pair?
		     (and (super-and-sub? formal-car.ots (car operand-item*.ots))
			  ;;Splice the operand OTSs.
			  (recur formal-cdr.ots (cdr operand-item*.ots))))
		    (else
		     #f))))
	       (else #f))))

	  (null?
	   ;;Return true if both the signatures are proper lists with the same number
	   ;;of items, and all the items are correct super and sub.
	   (case-type-signature-structure operands.specs
	     (null?	#t)
	     (<null>	#t)
	     (else	#f)))

	  (<null>
	   (case-type-signature-structure operands.specs
	     (null?	#t)
	     (<null>	#t)
	     (else	#f)))

	  (<list>
	   ;;The formals signature matches any number operands of any type.
	   #t)

	  (<list-of-spec>
	   ;;The formals  signature matches any  number of operands of  the specified
	   ;;type.  Examples:
	   ;;
	   ;;  formals.sig  == (... . (list-of <number>))
	   ;;  operands.sig == (... . <fixnum>)
	   ;;
	   ;;  formals.sig  == (... . (list-of <string>))
	   ;;  operands.sig == (... . (list-of <string>))
	   ;;
	   ;;  formals.sig  == (... . (list-of <string>))
	   ;;  operands.sig == (... . (list <string>))
	   ;;
	   (let ((formal-item.ots (list-of-type-spec.item-ots formals.specs)))
	     ;;If the formal's type is "<top>": any type of operands is matched.
	     (or (<top>-ots? formal-item.ots)
		 (let item-recur ((operands.specs operands.specs))
		   (case-type-signature-structure* operands.specs
		     (pair?
		      ;;formals.sig  == (... . (list-of <fixnum>))
		      ;;operands.sig == (... <fixnum> <fixnum>)
		      (and (super-and-sub? formal-item.ots (car operands.specs))
			   (item-recur (cdr operands.specs))))

		     (<pair-spec>
		      ;;formals.sig  == (... . (list-of <number>))
		      ;;operands.sig == (... . (pair <fixnum> . ?list-type))
		      (and (super-and-sub? formal-item.ots (pair-type-spec.car-ots operands.specs))
			   (item-recur (pair-type-spec.cdr-ots operands.specs))))

		     (null?
		      ;;formals.sig  == (... . (list-of <string>))
		      ;;operands.sig == (... . ())
		      #t)

		     (<null>
		      ;;formals.sig  == (... . (list-of <string>))
		      ;;operands.sig == (... . <null>)
		      #t)

		     (<list>
		      ;;formals.sig  == (... . (list-of <fixnum>))
		      ;;operands.sig == (... . <list>)
		      #f)

		     (<list-of-spec>
		      ;;formals.sig  == (... . (list-of <number>))
		      ;;operands.sig == (... . (list-of <fixnum>))
		      (super-and-sub? formal-item.ots (list-of-type-spec.item-ots operands.specs)))

		     (<nelist>
		      ;;formals.sig  == (... . (list-of <fixnum>))
		      ;;operands.sig == (... . <nelist>)
		      #f)

		     (<list-spec>
		      ;;formals.sig  == (... . (list-of <number>))
		      ;;operands.sig == (... . (list <fixnum>))
		      (item-recur (list-type-spec.item-ots* operands.specs)))

		     (else
		      (assertion-violation caller-who "invalid operands signature" operands.sig)))))))

	  (<nelist>
	   ;;The formals signature matches one or more operands of any type.
	   (case-type-signature-structure operands.specs
	     (pair?		#t)
	     (<nelist>		#t)
	     (<list-spec>	#t)
	     (<pair-spec>	#t)
	     (else		#f)))

	  (<list-spec>
	   ;;Splice the formals OTSs.
	   (recur (list-type-spec.item-ots* formals.specs) operands.specs))

	  (else
	   (assertion-violation caller-who "invalid formals signature" formals.sig))))))))


;;;; matching: arguments and operands

(module (type-signature.match-formals-against-operands)
  (define-module-who type-signature.match-formals-against-operands)

  (define* (type-signature.match-formals-against-operands formals.sig operands.sig)
    ;;In the context of a closure object application to fixed operands:
    ;;
    ;;   (?operator ?operand ...)
    ;;
    ;;compare the type signature of the operator's arguments to the type signature of
    ;;the  given  operands.   Return  a symbol  among:  exact-match,  possible-match,
    ;;no-match.
    ;;
    ;;FORMALS.SIG is a "<type-signature>" instance representing the type signature of
    ;;a closure object's clause arguments.
    ;;
    ;;OPERANDS.SIG is  a "<type-signature>" instance representing  the type signature
    ;;of the given operands.
    ;;
    (define the-who __who__)
    (define (%error-invalid-formals-signature)
      (assertion-violation the-who "invalid formals signature" formals.sig))
    (define (%error-invalid-operands-signature)
      (assertion-violation the-who "invalid operands signature" operands.sig))
    (let loop ((state		'exact-match)
	       (formals.specs	(type-signature.object-type-specs formals.sig))
	       (operands.specs	(type-signature.object-type-specs operands.sig)))
      ;;In  this loop  the  variable  STATE always  degrades:  from "exact-match"  to
      ;;"possible-match"  or "no-match";  from  "possible-match"  to "no-match".   It
      ;;never upgrades.
      (case-type-signature-structure* formals.specs
	;;The operator accepts one more mandatory operand.
	(pair?
	 (%match-formals-pair-against-operands loop state
					       (car formals.specs) (cdr formals.specs)
					       operands.specs
					       %error-invalid-formals-signature %error-invalid-operands-signature))
	(<pair-spec>
	 (%match-formals-pair-against-operands loop state
					       (pair-type-spec.car-ots formals.specs) (pair-type-spec.cdr-ots formals.specs)
					       operands.specs
					       %error-invalid-formals-signature %error-invalid-operands-signature))

	;;The operator accepts no more operands.
	(null?
	 (%match-formals-null-against-operands state operands.specs %error-invalid-operands-signature))
	(<null>
	 (%match-formals-null-against-operands state operands.specs %error-invalid-operands-signature))

	(<list>
	 ;;The operator accepts zero or more operands of any type.
	 ;;
	 ;;   formals.sig  == (... . <list>)
	 ;;   operands.sig == (... ?type ...)
	 ;;
	 ;;Good.  And we are done here, let's return the final state.
	 state)

	(<list-of-spec>
	 ;;The operator accepts zero or more operands of a known type.
	 ;;
	 ;;   formals.sig  == (... . (list-of ?type))
	 ;;   operands.sig == (... ?type ...)
	 ;;
	 (%match-formals-list-of-against-operands state (list-of-type-spec.item-ots formals.specs) operands.specs
						  %error-invalid-operands-signature))

	(<nelist>
	 ;;The operator accepts one or more operands of any type.
	 (case-type-signature-structure* operands.specs
	   ;;There is at least one more operand.  Good.
	   (pair?		state)
	   (<pair-spec>		state)
	   ;;No more operands.  Bad.
	   (null?		'no-match)
	   (<null>		'no-match)
	   ;;There is an unspecified number of rest operands.
	   (<list>		'possible-match)
	   (<list-of-spec>	'possible-match)
	   ;;There are one or more operands.
	   (<nelist>		state)
	   (<list-spec>		state)
	   (else
	    (%error-invalid-formals-signature))))

	(<list-spec>
	 ;;The operator  accepts a known  number of  operands, of known  type.  Let's
	 ;;splice the specifications.
	 (loop state (list-type-spec.item-ots* formals.specs) operands.specs))

	(else
	 (%error-invalid-formals-signature)))))

;;; --------------------------------------------------------------------

  (module (%match-formals-pair-against-operands)

    (define (%match-formals-pair-against-operands loop state
						  formals-car.ots formals-cdr.specs operands.specs
						  %error-invalid-formals-signature %error-invalid-operands-signature)
      (case-type-signature-structure* operands.specs
	(pair?
	 (%match-formals-pair-against-operands-pair loop state
						    formals-car.ots formals-cdr.specs
						    (car operands.specs) (cdr operands.specs)))
	(<pair-spec>
	 (%match-formals-pair-against-operands-pair loop state
						    formals-car.ots formals-cdr.specs
						    (pair-type-spec.car-ots operands.specs)
						    (pair-type-spec.cdr-ots operands.specs)))
	;;At least one more formal and no more operands.  Bad.
	(null?			'no-match)
	(<null>			'no-match)
	;;There is an unspecified number of operands, of unspecified type.
	(<list>			'possible-match)
	;;There is an unspecified number of operands, of known type.
	(<list-of-spec>
	 (%match-formals-pair-against-operands-list-of formals-car.ots formals-cdr.specs
						       (list-of-type-spec.item-ots operands.specs)
						       %error-invalid-formals-signature))
	;;There is at least one more operand, of unknown type.
	(<nelist>		'possible-match)
	;;There  is a  known number  of operands,  of known  type.  Let's  splice the
	;;operands.
	(<list-spec>
	 (%match-formals-pair-against-operands loop state
					       formals-car.ots formals-cdr.specs
					       (list-type-spec.item-ots* operands.specs)
					       %error-invalid-formals-signature %error-invalid-operands-signature))
	(else
	 (%error-invalid-operands-signature))))

    (define (%match-formals-pair-against-operands-pair loop state
						       formals-car.ots formals-cdr.specs
						       operands-car.ots operands-cdr.specs)
      (cond ((object-type-spec.matching-super-and-sub? formals-car.ots operands-car.ots)
	     (loop state formals-cdr.specs operands-cdr.specs))
	    ((object-type-spec.compatible-super-and-sub? formals-car.ots operands-car.ots)
	     (loop 'possible-match formals-cdr.specs operands-cdr.specs))
	    (else
	     'no-match)))

    (module (%match-formals-pair-against-operands-list-of)

      (define (%match-formals-pair-against-operands-list-of formals-car.ots formals-cdr.specs operand.ots
							    %error-invalid-formals-signature)
	;;This       is      a       mutually      recursive       function      with
	;;%MATCH-FORMALS-AGAINST-OPERANDS-LIST-OF.
	;;
	(cond ((object-type-spec.matching-super-and-sub? formals-car.ots operand.ots)
	       (%match-formals-against-operands-list-of formals-cdr.specs operand.ots %error-invalid-formals-signature))
	      ((object-type-spec.compatible-super-and-sub? formals-car.ots operand.ots)
	       (%match-formals-against-operands-list-of formals-cdr.specs operand.ots %error-invalid-formals-signature))
	      (else 'no-match)))

      (define (%match-formals-against-operands-list-of formals.specs operand.ots %error-invalid-formals-signature)
	;;This  is   a  recursive  function   a  mutually  recursive   function  with
	;;%MATCH-FORMALS-PAIR-AGAINST-OPERANDS-LIST-OF.   The  operator accepts  more
	;;arguments of  a specified type and  there is an unspecified  number of rest
	;;operands of known type.
	;;
	;;   formals.sig  == (... ?type ...)
	;;   operands.sig == (... . (list-of ?type))
	;;
	;;This  function returns  a  symbol among:  possible-match, no-match.   Exact
	;;match is already excluded.
	;;
	;;The   argument   FORMALS.SPECS   is   a  proper   or   improper   list   of
	;;"<object-type-spec>" representing the requested type for all rest operands.
	;;The argument  OPERAND.OTS is an "<object-type-spec>"  instance representing
	;;the type of all the given rest operands.
	;;
	(case-type-signature-structure* formals.specs
	  ;;At least one more formal.
	  (pair?
	   (%match-formals-pair-against-operands-list-of (car formals.specs) (cdr formals.specs) operand.ots
							 %error-invalid-formals-signature))
	  (<pair-spec>
	   (%match-formals-pair-against-operands-list-of (pair-type-spec.car-ots formals.specs)
							 (pair-type-spec.cdr-ots formals.specs)
							 operand.ots
							 %error-invalid-formals-signature))
	  ;;No more formals.
	  (null?			'possible-match)
	  (<null>			'possible-match)

	  ;;There is an unspecified number of formals, of unspecified type.
	  (<list>			'possible-match)

	  ;;There is an unspecified number of formals, with a known type.
	  (<list-of-spec>
	   (let ((formal.ots (list-of-type-spec.item-ots formals.specs)))
	     (cond ((object-type-spec.matching-super-and-sub? formal.ots operand.ots)
		    'possible-match)
		   ((object-type-spec.compatible-super-and-sub? formal.ots operand.ots)
		    'possible-match)
		   (else 'no-match))))

	  ;;There is at least one more formals, of unspecified type.
	  (<nelist>		'possible-match)

	  ;;There  is a  known number  of  formals, of  known type.   Let's splice  the
	  ;;specifications.
	  (<list-spec>
	   (%match-formals-against-operands-list-of (list-type-spec.item-ots* formals.specs)
						    operand.ots %error-invalid-formals-signature))

	  (else
	   (%error-invalid-formals-signature))))

      #| end of module: %MATCH-FORMALS-PAIR-AGAINST-OPERANDS-LIST-OF |# )

    #| end of module: %MATCH-FORMALS-PAIR-AGAINST-OPERANDS |# )

;;; --------------------------------------------------------------------

  (define (%match-formals-null-against-operands state operands.specs %error-invalid-operands-signature)
    (case-type-signature-structure* operands.specs
      ;;No more arguments and leftover operands.  Bad.
      (pair?		'no-match)
      (<pair-spec>	'no-match)

      ;;No more arguments and  no more operands.  Good.  And we  are done here, let's
      ;;return the final state.
      (null?		state)
      (<null>		state)

      ;;There may be other operands.
      (<list>		'possible-match)
      (<list-of-spec>	'possible-match)

      ;;There is at least one other operand.  Bad.
      (<nelist>		'no-match)
      (<list-spec>	'no-match)

      (else
       (%error-invalid-operands-signature))))

;;; --------------------------------------------------------------------

  (module (%match-formals-list-of-against-operands)

    (define (%match-formals-list-of-against-operands state formal.ots operands.specs %error-invalid-operands-signature)
      ;;Recursive function.  We  use this function when the operator  accepts zero or
      ;;more operands of a specified type.
      ;;
      ;;   formals.sig  == (... . (list-of ?type))
      ;;   operands.sig == (... ?type ...)
      ;;
      ;;The argument  FORMAL.OTS is an instance  of "<object-type-spec>" representing
      ;;the requested type for all the rest operands.  The argument OPERANDS.SPECS is
      ;;a proper or improper list  of "<object-type-spec>" instances representing the
      ;;types of the given rest operands.
      ;;
      (case-type-signature-structure* operands.specs
	;;At least one more operand.  Let's match it against the argument.
	(pair?
	 (%match-formals-list-of-against-operands-pair state formal.ots (car operands.specs) (cdr operands.specs)
						       %error-invalid-operands-signature))
	(<pair-spec>
	 (%match-formals-list-of-against-operands-pair state formal.ots
						       (pair-type-spec.car-ots operands.specs)
						       (pair-type-spec.cdr-ots operands.specs)
						       %error-invalid-operands-signature))
	;;No more operands.  Good.
	(null?		state)
	(<null>		state)

	;;There is an unspecified number of rest operands, with unspecified type.
	(<list>		'possible-match)

	;;There is an unspecified number of rest operands, with a known type.
	(<list-of-spec>
	 (let ((operand.ots (list-of-type-spec.item-ots operands.specs)))
	   (cond ((object-type-spec.matching-super-and-sub? formal.ots operand.ots)
		  state)
		 ((object-type-spec.compatible-super-and-sub? formal.ots operand.ots)
		  'possible-match)
		 (else 'no-match))))

	;;There is at least one more operand, with unspecified type.
	(<nelist>	'possible-match)

	;;There is  a known number  of operands, with  known type.  Let's  splice the
	;;specifications.
	(<list-spec>
	 (%match-formals-list-of-against-operands state formal.ots (list-type-spec.item-ots* operands.specs)
						  %error-invalid-operands-signature))

	(else
	 (%error-invalid-operands-signature))))

    (define (%match-formals-list-of-against-operands-pair state formal.ots operands-car.ots operands-cdr.specs
							  %error-invalid-operands-signature)
      (cond ((object-type-spec.matching-super-and-sub? formal.ots operands-car.ots)
	     (%match-formals-list-of-against-operands state formal.ots operands-cdr.specs
						      %error-invalid-operands-signature))
	    ((object-type-spec.compatible-super-and-sub? formal.ots operands-car.ots)
	     (%match-formals-list-of-against-operands 'possible-match formal.ots operands-cdr.specs
						      %error-invalid-operands-signature))
	    (else
	     ;;The formal is INcompatible with the operand.  Bad.
	     'no-match)))

    #| end of module: %MATCH-FORMALS-LIST-OF-AGAINST-OPERANDS |# )

  #| end of module: TYPE-SIGNATURE.MATCH-FORMALS-AGAINST-OPERANDS |# )


;;;; matching: common ancestor

(case-define* type-signature.common-ancestor
  ;;Given a  type signature arguments: return  a new type signature  representing the
  ;;common ancestor.
  ;;
  (()
   (make-type-signature/fully-unspecified))
  (({sig type-signature?})
   sig)
  (({sig1 type-signature?} {sig2 type-signature?})
   ($type-signature.common-ancestor2 sig1 sig2))
  (({sig1 type-signature?} {sig2 type-signature?} {sig3 type-signature?} . {sig* type-signature?})
   (fold-left $type-signature.common-ancestor2
     ($type-signature.common-ancestor2 ($type-signature.common-ancestor2 sig1 sig2) sig3)
     sig*)))

(define ($type-signature.common-ancestor2 sig1 sig2)
  ;;Given two type signatures: return a  new type signature representing their common
  ;;ancestor.
  ;;
  (make-type-signature
   (let recur ((specs1 (type-signature.object-type-specs sig1))
	       (specs2 (type-signature.object-type-specs sig2)))
     (cond ((null? specs1)
	    (if (null? specs2)
		;;Both the signatures are proper lists with the same number of items:
		;;success!
		'()
	      ;;SIG1 is a proper list shorter that SIG2.
	      (<list>-ots)))

	   ((pair? specs1)
	    (cond ((pair? specs2)
		   (cons (object-type-spec.common-ancestor (car specs1) (car specs2))
			 (recur (cdr specs1) (cdr specs2))))
		  ((null? specs2)
		   ;;SIG2 is a proper list shorter that SIG1.
		   (<list>-ots))
		  (else
		   ;;SIG2 is an improper list shorter that SIG1.
		   (<list>-ots))))

	   (else
	    ;;SIG1 is an improper list.
	    (cond ((null? specs2)
		   ;;SIG2 is an proper list shorter that SIG1.
		   (<list>-ots))
		  ((pair? specs2)
		   ;;SIG2 is an proper list longer that SIG1.
		   (<list>-ots))
		  (else
		   ;;Both SIG1  and SIG2 are improper  lists with the same  number of
		   ;;items.   Since  both  SPECS1  and   SPECS2  come  from  a  valid
		   ;;"<type-signature>" instance: here we know that SPECS1 and SPECS2
		   ;;are    either    "<bottom>"    OTSs,   "<list>"    OTSs    or
		   ;;"<list-of-type-spec>"  instances; so  their  common ancestor  is
		   ;;either the "<list>" OTS or a "<list-of-type-spec>" instance.
		   (object-type-spec.common-ancestor specs1 specs2))))))))


;;;; matching: union

(case-define* type-signature.union
  ;;Given a  type signature arguments: return  a new type signature  representing the
  ;;union.
  ;;
  (()
   (make-type-signature/fully-unspecified))
  (({sig type-signature?})
   sig)
  (({sig1 type-signature?} {sig2 type-signature?})
   ($type-signature.union2 sig1 sig2))
  (({sig1 type-signature?} {sig2 type-signature?} {sig3 type-signature?} . {sig* type-signature?})
   (fold-left $type-signature.union2
     ($type-signature.union2 ($type-signature.union2 sig1 sig2) sig3)
     sig*)))

(define ($type-signature.union2 sig1 sig2)
  ;;Given two  type signatures: return  a new  type signature representing  the union
  ;;between the two.
  ;;
  (make-type-signature
   (let ((specs1 (type-signature.object-type-specs sig1))
	 (specs2 (type-signature.object-type-specs sig2)))
     (cond
      ;;Here we imagine this situation:
      ;;
      ;;   (or (do-something)
      ;;       (error #f "error doing something"))
      ;;
      ;;the type signature of  the returned values is the one  of the expression that
      ;;returns.
      ((<bottom>-ots? specs1)	specs2)
      ((<bottom>-ots? specs2)	specs1)

      ;;If  a  signature  is  fully   unspecified:  just  consider  the  union  fully
      ;;unspecified.  This is useful with the standard language.
      ((<list>-ots? specs1)	(<list>-ots))
      ((<list>-ots? specs2)	(<list>-ots))

      (else
       (let recur ((specs1 specs1) (specs2 specs2))
	 (case-type-signature-structure specs1
	   (pair?
	    (case-type-signature-structure specs2
	      (pair?
	       (cons (union-of-type-specs (car specs1) (car specs2))
		     (recur (cdr specs1) (cdr specs2))))
	      (null?
	       ;;SIG2 is a proper list shorter that SIG1.
	       (<list>-ots))
	      (else
	       ;;SIG2 is an improper list shorter that SIG1.
	       (<list>-ots))))

	   (null?
	    (case-type-signature-structure specs2
	      (null?
	       ;;Both the signatures are proper lists  with the same number of items:
	       ;;success!
	       '())
	      (else
	       ;;SIG1 is a proper list shorter that SIG2.
	       (<list>-ots))))

	   (else
	    ;;SIG1 is an improper list.
	    (case-type-signature-structure specs2
	      (null?
	       ;;SIG2 is an proper list shorter that SIG1.
	       (<list>-ots))
	      (pair?
	       ;;SIG2 is an proper list longer that SIG1.
	       (<list>-ots))
	      (else
	       ;;Both SIG1 and SIG2 are improper lists with the same number of items.
	       (union-of-type-specs specs1 specs2)))))))))))


;;;; matching: union with same number of operands

(case-define* type-signature.union-same-number-of-operands
  ;;Given a  type signature arguments: return  a new type signature  representing the
  ;;union.  UNION-SYNNER must be a procedure used to signal errors.
  ;;
  (({union-synner procedure?})
   (make-type-signature/fully-unspecified))
  (({union-synner procedure?} {sig type-signature?})
   sig)
  (({union-synner procedure?} {sig1 type-signature?} {sig2 type-signature?})
   ($type-signature.union-same-number-of-operands2 union-synner sig1 sig2))
  (({union-synner procedure?} {sig1 type-signature?} {sig2 type-signature?} {sig3 type-signature?} . {sig* type-signature?})
   (fold-left (lambda (sig1 sig2)
		($type-signature.union-same-number-of-operands2 union-synner sig1 sig2))
     ($type-signature.union-same-number-of-operands2
      union-synner
      ($type-signature.union-same-number-of-operands2 union-synner sig1 sig2)
      sig3)
     sig*)))

(define ($type-signature.union-same-number-of-operands2 union-synner sig1 sig2)
  ;;Given two  type signatures: return  a new  type signature representing  the union
  ;;between the two.
  ;;
  (define (%error-mismatching-length)
    (union-synner "type signatures of different length"
		  (condition (make-type-signature-condition sig1)
			     (make-type-signature-condition sig2))))
  (make-type-signature
   (let ((specs1 (type-signature.object-type-specs sig1))
	 (specs2 (type-signature.object-type-specs sig2)))
     (cond
      ;;Here we imagine this situation:
      ;;
      ;;   (or (do-something)
      ;;       (error #f "error doing something"))
      ;;
      ;;the type signature of  the returned values is the one  of the expression that
      ;;returns.
      ((<bottom>-ots? specs1)	specs2)
      ((<bottom>-ots? specs2)	specs1)

      ;;If  a  signature  is  fully   unspecified:  just  consider  the  union  fully
      ;;unspecified.  This is useful with the standard language.
      ((<list>-ots? specs1)	(<list>-ots))
      ((<list>-ots? specs2)	(<list>-ots))

      (else
       (let recur ((specs1 specs1) (specs2 specs2))
	 (cond ((pair? specs1)
		(cond ((pair? specs2)
		       (cons (union-of-type-specs (car specs1) (car specs2))
			     (recur (cdr specs1) (cdr specs2))))
		      ((null? specs2)
		       ;;SIG2 is a proper list shorter that SIG1.
		       (%error-mismatching-length))
		      (else
		       ;;SIG2 is an improper list shorter that SIG1.
		       (<list>-ots))))

	       ((null? specs1)
		(if (null? specs2)
		    ;;Both the  signatures are proper  lists with the same  number of
		    ;;items: success!
		    '()
		  ;;SIG1 is a proper list shorter that SIG2.
		  (%error-mismatching-length)))

	       (else
		;;SIG1 is an improper list.
		(cond ((null? specs2)
		       ;;SIG2 is an proper list shorter that SIG1.
		       (%error-mismatching-length))
		      ((pair? specs2)
		       ;;SIG2 is an proper list longer that SIG1.
		       (%error-mismatching-length))
		      (else
		       ;;Both SIG1 and  SIG2 are improper lists with  the same number
		       ;;of items.
		       (union-of-type-specs specs1 specs2)))))))))))


;;;; type propagation

(module (type-signature.type-propagation)

  (define* (type-signature.type-propagation formals.sig operands.sig)
    ;;Assume we have this situation:
    ;;
    ;;   (receive ?formals
    ;;       ?operands
    ;;     ---)
    ;;
    ;;FORMALS.SIG is a "<type-signature>" instance representing the type signature of
    ;;?FORMALS; OPERANDS.SIG  is a "<type-signature>" instance  representing the type
    ;;signature of ?OPERANDS.
    ;;
    ;;We want to perform type propagation  from the values returned by the expression
    ;;?OPERANDS to the variables in the ?FORMALS, so that:
    ;;
    ;;   (receive (a b c)
    ;;       (values 1 2.3 "ciao")
    ;;     ---)
    ;;
    ;;becomes equivalent to:
    ;;
    ;;   (receive ({a <positive-fixnum>} {b <positive-flonum>} {c <nestring>})
    ;;       (values 1 2.3 "ciao")
    ;;     ---)
    ;;
    ;;We want to propagate  the type for all the untyped variables  in ?FORMALS; if a
    ;;variable  in  ?formals already  has  a  type: we  leave  it  alone.  We  assume
    ;;FORMALS.SIG and OPERANDS.SIG have already been matched and the result is either
    ;;"exact-match" or "possible-match".
    ;;
    ;;Build  and return  a new  "<type-signature>" instance  representing FORMALS.SIG
    ;;with propagated types.  If an error occurs: raise an exception.
    ;;
    ;;Examples:
    ;;
    ;;   formals.sig    == (<untyped> <untyped> . <list>)
    ;;   operands.sig   == (<fixnum>  <fixnum>  . (list-of <fixnum>))
    ;;   propagated.sig == (<fixnum>  <fixnum>  . (list-of <fixnum>))
    ;;
    ;;   formals.sig    == (<untyped> . <list>)
    ;;   operands.sig   == (<fixnum>  <fixnum>  . (list-of <fixnum>))
    ;;   propagated.sig == (<fixnum>  . (pair <fixnum> (list-of <fixnum>)))
    ;;
    ;;   formals.sig    == (<untyped> <untyped> . <list>)
    ;;   operands.sig   == (<fixnum>  . <list>)
    ;;   propagated.sig == (<fixnum>  <top>     . <list>)
    ;;
    (define (%error-mismatch message)
      (raise
       (condition (make-expand-time-type-signature-violation)
		  (make-who-condition 'type-signature.type-propagation)
		  (make-message-condition message)
		  (make-expected-type-signature-condition formals.sig)
		  (make-returned-type-signature-condition operands.sig))))
    (define (%error-invalid-formals-signature)
      (assertion-violation 'type-signature.type-propagation "internal error, invalid formals signature" formals.sig))
    (define (%error-invalid-operands-signature)
      (assertion-violation 'type-signature.type-propagation "internal error, invalid operands signature" operands.sig))
    (make-type-signature
     (let recur ((formals.specs		(type-signature.object-type-specs formals.sig))
		 (operands.specs	(type-signature.object-type-specs operands.sig)))
       (case-type-signature-structure* formals.specs
	 (pair?
	  (%process-pair-formals (car formals.specs) (cdr formals.specs)
				 operands.specs
				 recur %error-invalid-operands-signature %error-mismatch))

	 (<pair-spec>
	  (%process-pair-formals (pair-type-spec.car-ots formals.specs) (pair-type-spec.cdr-ots formals.specs)
				 operands.specs
				 recur %error-invalid-operands-signature %error-mismatch))

	 (null?
	  (%process-null-formals formals.specs operands.specs %error-mismatch))

	 (<null>
	  (%process-null-formals formals.specs operands.specs %error-mismatch))

	 (<list>
	  (%process-list-formals operands.specs %error-invalid-operands-signature))

	 (<list-of-spec>
	  ;;We give precedence to the formals type.
	  ;;
	  ;;formals.sig    == (... . (list-of <fixnum>))
	  ;;operands.sig   == (... . ?operand-rest)
	  ;;propagated.sig == (... . (list-of <fixnum>))
	  formals.specs)

	 (<nelist>
	  (%process-nelist-formals formals.specs operands.specs %error-invalid-operands-signature))

	 (<list-spec>
	  ;;We give precedence to the formals type.
	  ;;
	  ;;formals.sig    == (... . (list <fixnum> ...))
	  ;;operands.sig   == (... . ?operand-rest)
	  ;;propagated.sig == (... . (list <fixnum> ...))
	  formals.specs)

	 (else
	  (%error-invalid-formals-signature))))))

;;; --------------------------------------------------------------------

  (define (%process-pair-formals formals-car.ots formals-cdr.ots operands.specs recur
				 error-invalid-operands-signature error-mismatch)
    (define untyped? (<untyped>-ots? formals-car.ots))
    (case-type-signature-structure* operands.specs
      (pair?	;there is at least one more operand
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... <fixnum> ...)
       ;;propagated.sig == (... <fixnum> ...)
       (cons (if untyped? (car operands.specs) formals-car.ots)
	     (recur formals-cdr.ots (cdr operands.specs))))

      (<pair-spec>
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . (pair <fixnum> ?list-type))
       ;;propagated.sig == (... <fixnum> ...)
       (cons (if untyped? (pair-type-spec.car-ots operands.specs) formals-car.ots)
	     (recur formals-cdr.ots (pair-type-spec.cdr-ots operands.specs))))

      (null?	;no more operands
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . ())
       (error-mismatch "more operands than formals"))

      (<null>	;no more operands
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . <null>)
       (error-mismatch "more operands than formals"))

      (<list>
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . <list>)
       ;;propagated.sig == (... <top> ...)
       (cons (if untyped? (<top>-ots) formals-car.ots)
	     (recur formals-cdr.ots operands.specs)))

      (<list-of-spec>
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . (list-of <fixnum>))
       ;;propagated.sig == (... <fixnum> ...)
       (cons (if untyped? (list-of-type-spec.item-ots operands.specs) formals-car.ots)
	     (recur formals-cdr.ots operands.specs)))

      (<nelist>
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . <nelist>)
       ;;propagated.sig == (... <top> ...)
       (cons (if untyped? (<top>-ots) formals-car.ots)
	     ;;We replace  "<nelist>" with  "<list>", because we  have consumed
	     ;;the mandatory item.
	     (recur formals-cdr.ots (<list>-ots))))

      (<list-spec>
       ;;formals.sig    == (... <untyped> ...)
       ;;operands.sig   == (... . (list <fixnum> ...))
       ;;propagated.sig == (... <fixnum> ...)
       (let ((operands.specs (list-type-spec.item-ots* operands.specs)))
	 (cons (if untyped? (car operands.specs) formals-car.ots)
	       (recur formals-cdr.ots (cdr operands.specs)))))

      (else
       (error-invalid-operands-signature))))

;;; --------------------------------------------------------------------

  (define (%process-null-formals formals.specs operands.specs error-mismatch)
    (case-type-signature-structure operands.specs
      (null?	;no more operands
       '())
      (<null>	;no more operands
       '())
      (<list>/<list-of-spec>
       ;;formals.sig    == (... . ())
       ;;operands.sig   == (... . <list>)
       ;;propagated.sig == (... . ())
       formals.specs)
      (else
       (error-mismatch "more operands than formals"))))

;;; --------------------------------------------------------------------

  (define (%process-list-formals operands.specs error-invalid-operands-signature)
    (case-type-signature-structure* operands.specs
      (pair?
       (if (list? operands.specs)
	   ;;Good, it is a proper list.
	   ;;
	   ;;formals.sig    == (... . <list>)
	   ;;operands.sig   == (... <fixnum> ...)
	   ;;propagated.sig == (... . (list <fixnum> ...))
	   (make-list-type-spec operands.specs)
	 ;;It is an improper list: let's build a compound of pair specs.
	 ;;
	 ;;formals.sig    == (... . <list>)
	 ;;operands.sig   == (... <fixnum> <flonum> . <list>)
	 ;;propagated.sig == (... . (pair <fixnum> (pair <flonum> <list>)))
	 (let recur ((specs operands.specs))
	   (if (pair? specs)
	       (make-pair-type-spec (car specs) (recur (cdr specs)))
	     specs))))

      (<pair-spec>
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . (pair <fixnum> ?list-type))
       ;;propagated.sig == (... . (pair <fixnum> ?list-type))
       operands.specs)

      (null?	;no more operands
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . ())
       ;;propagated.sig == (... . <null>)
       (<null>-ots))

      (<null>
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . <null>)
       ;;propagated.sig == (... . <null>)
       operands.specs)

      (<list>
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . <list>)
       ;;propagated.sig == (... . <list>)
       operands.specs)

      (<list-of-spec>
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . (list-of <fixnum>))
       ;;propagated.sig == (... . (list-of <fixnum>))
       operands.specs)

      (<nelist>
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . <nelist>)
       ;;propagated.sig == (... . <nelist>)
       operands.specs)

      (<list-spec>
       ;;formals.sig    == (... . <list>)
       ;;operands.sig   == (... . (list <fixnum>))
       ;;propagated.sig == (... . (list <fixnum>))
       operands.specs)

      (else
       (error-invalid-operands-signature))))

;;; --------------------------------------------------------------------

  (define (%process-nelist-formals formals.specs operands.specs error-invalid-operands-signature)
    ;;We have to remember  that we have already decided that  the type signatures are
    ;;compatible, so  any mismatch in  the number of  values is checked  at run-time.
    ;;Here we force the propagated type signature to be possible.
    ;;
    (case-type-signature-structure* operands.specs
      (pair?
       (if (list? operands.specs)
	   ;;Good, it is a proper list.
	   ;;
	   ;;formals.sig    == (... . <nelist>)
	   ;;operands.sig   == (... <fixnum> ...)
	   ;;propagated.sig == (... . (list <fixnum> ...))
	   (make-list-type-spec operands.specs)
	 ;;It is an improper list: let's build a compound of pair specs.
	 ;;
	 ;;formals.sig    == (... . <nelist>)
	 ;;operands.sig   == (... <fixnum> <flonum> . <list>)
	 ;;propagated.sig == (... . (pair <fixnum> (pair <flonum> <list>)))
	 (let recur ((specs operands.specs))
	   (if (pair? specs)
	       (make-pair-type-spec (car specs) (recur (cdr specs)))
	     specs))))

      (<pair-spec>
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . (pair <fixnum> ?list-type))
       ;;propagated.sig == (... . (pair <fixnum> ?list-type))
       operands.specs)

      (null?	;no more operands
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . ())
       ;;propagated.sig == (... . <nelist>)
       formals.specs)

      (<null>
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . <null>)
       ;;propagated.sig == (... . <nelist>)
       formals.specs)

      (<list>
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . <list>)
       ;;propagated.sig == (... . <nelist>)
       formals.specs)

      (<list-of-spec>
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . (list-of <fixnum>))
       ;;propagated.sig == (... . (pair <fixnum> (list-of <fixnum>)))
       (make-pair-type-spec (list-of-type-spec.item-ots operands.specs)
			    operands.specs))

      (<nelist>
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . <nelist>)
       ;;propagated.sig == (... . <nelist>)
       operands.specs)

      (<list-spec>
       ;;formals.sig    == (... . <nelist>)
       ;;operands.sig   == (... . (list <fixnum> ...))
       ;;propagated.sig == (... . (list <fixnum> ...))
       operands.specs)

      (else
       (error-invalid-operands-signature))))

  #| end of module: TYPE-SIGNATURE.TYPE-PROPAGATION |# )


;;;; helpers and utilities

(case-define datum-type-signature
  ((datum)
   (datum-type-signature datum (current-inferior-lexenv)))
  ((datum lexenv)
   ;;Build and return  a new instance of "<type-signature>" representing  the type of
   ;;the single value returned by the expression DATUM, which must be a Scheme object
   ;;extracted from a syntax object representing a literal expression.
   ;;
   (make-type-signature/single-value (datum-type-annotation datum lexenv))))

(define (datum-type-annotation datum lexenv)
  ;;Recursive  function.   Build  and  return  an  instance  of  "<object-type-spec>"
  ;;representing  the  type annotation  of  DATUM,  which  must  be a  Scheme  object
  ;;extracted from a syntax object representing a literal expression.
  ;;
  ;;We use a hashtable  to detect circular structures in DATUM; we  put in here pairs
  ;;and vectors.
  (define table (make-eq-hashtable))
  (let recur ((datum datum))
    (cond ((boolean? datum)		(cond (datum
					       (<true>-ots))
					      (else
					       (<false>-ots))))
	  ((char?    datum)		(core-prim-spec '<char> lexenv))
	  ((symbol?  datum)		(make-enumeration-type-spec (list datum)))
	  ((keyword? datum)		(core-prim-spec '<keyword> lexenv))

	  ((fixnum?  datum)		(cond ((fxpositive? datum)
					       (core-prim-spec '<positive-fixnum> lexenv))
					      ((fxnegative? datum)
					       (core-prim-spec '<negative-fixnum> lexenv))
					      ((fxzero? datum)
					       (core-prim-spec '<zero-fixnum> lexenv))
					      (else
					       ;;This should never happen.
					       (core-prim-spec '<fixnum> lexenv))))
	  ((flonum?  datum)		(cond ((flpositive? datum)
					       (core-prim-spec '<positive-flonum> lexenv))
					      ((flnegative? datum)
					       (core-prim-spec '<negative-flonum> lexenv))
					      ((flzero?/positive datum)
					       (core-prim-spec '<positive-zero-flonum> lexenv))
					      ((flzero?/negative datum)
					       (core-prim-spec '<negative-zero-flonum> lexenv))
					      (else
					       ;;This  happens  when  the  flonum  is
					       ;;not-a-number.
					       (core-prim-spec '<flonum> lexenv))))
	  ((ratnum?  datum)		(cond ((ratnum-positive? datum)
					       (core-prim-spec '<positive-ratnum> lexenv))
					      ((ratnum-negative? datum)
					       (core-prim-spec '<negative-ratnum> lexenv))
					      (else
					       ;;This should never happen.
					       (core-prim-spec '<ratnum> lexenv))))
	  ((bignum?  datum)		(cond ((bignum-positive? datum)
					       (core-prim-spec '<positive-bignum> lexenv))
					      ((bignum-negative? datum)
					       (core-prim-spec '<negative-bignum> lexenv))
					      (else
					       ;;This should never happen.
					       (core-prim-spec '<bignum> lexenv))))
	  ((compnum? datum)		(cond ((exact-compnum? datum)
					       (core-prim-spec '<exact-compnum> lexenv))
					      ((zero-compnum? datum)
					       (core-prim-spec '<zero-compnum> lexenv))
					      (else
					       (core-prim-spec '<non-zero-inexact-compnum> lexenv))))
	  ((cflonum? datum)		(cond ((zero-cflonum? datum)
					       (core-prim-spec '<zero-cflonum> lexenv))
					      (else
					       (core-prim-spec '<non-zero-cflonum> lexenv))))

	  ((string?  datum)		(cond ((string-empty? datum)
					       (core-prim-spec '<empty-string> lexenv))
					      (else
					       (core-prim-spec '<nestring> lexenv))))

	  ((null? datum)		(<null>-ots))

	  ((list? datum)		(if (hashtable-ref table datum #f)
					    (<nelist>-ots)
					  (begin
					    (let pair-recur ((P datum))
					      (when (pair? P)
						(hashtable-set! table P #t)
						(pair-recur (cdr P))))
					    (make-list-type-spec (map recur datum)))))

	  ((pair? datum)		(if (hashtable-ref table datum #f)
					    (<pair>-ots)
					  (begin
					    (hashtable-set! table datum #t)
					    (make-pair-type-spec (recur (car datum))
								 (recur (cdr datum))))))

	  ((vector?  datum)		(cond ((vector-empty? datum)
					       (<empty-vector>-ots))
					      (else
					       (if (hashtable-ref table datum #f)
						   (<nevector>-ots)
						 (begin
						   (hashtable-set! table datum #t)
						   (make-vector-type-spec (map recur (vector->list datum))))))))

	  ((bytevector? datum)		(cond ((bytevector-empty? datum)
					       (core-prim-spec '<empty-bytevector> lexenv))
					      (else
					       (core-prim-spec '<nebytevector> lexenv))))

	  ((eq? datum (void))		(<void>-ots))
	  (else				(<top>-ots)))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
