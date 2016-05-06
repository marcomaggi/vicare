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
   type-signature.object-type-specs			type-signature.syntax-object

;;; special constructors
   make-type-signature/single-top			make-type-signature/single-void
   make-type-signature/single-null			make-type-signature/single-list
   make-type-signature/single-boolean
   make-type-signature/single-true			make-type-signature/single-false
   make-type-signature/single-procedure			make-type-signature/single-symbol
   make-type-signature/single-stx			make-type-signature/single-syntactic-identifier
   make-type-signature/single-value
   make-type-signature/standalone-list			make-type-signature/fully-untyped
   make-type-signature/no-return

;;; comparison
   type-signature=?

;;; predicates
   list-of-type-signatures?
   type-signature.fully-untyped?			type-signature.partially-untyped?
   type-signature.untyped?				type-signature.empty?
   type-signature.super-and-sub?			type-signature.compatible-super-and-sub?
   type-signature.single-type?				type-signature.single-top-tag?
   type-signature.single-type-or-fully-untyped?		type-signature.no-return?

   type-signature.match-arguments-against-operands

;;; accessors
   type-signature.min-count				type-signature.max-count
   type-signature.min-and-max-counts

   type-signature.common-ancestor			type-signature.union
   type-signature.union-same-number-of-operands
   datum-type-signature

;;; helpers
   case-signature-specs
   single-value unspecified-values <list-of> <closure>

   #| end of exports |# )


;;;; syntax helpers

(define-auxiliary-syntaxes single-value unspecified-values <closure> <list-of>)

(define-syntax* (case-signature-specs input-form.stx)
  ;;A typical use for an expression used as operand or similar:
  ;;
  ;;  (case-signature-specs expr.sig
  ;;    ((unspecified-values)
  ;;     ;;The expression returns an unspecified number of values.
  ;;     ?body0 ?body ...)
  ;;
  ;;    (<no-return>
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
  ;;  (case-signature-specs expr.sig
  ;;    ((<closure>)
  ;;     ;;The expression returns a single value, typed as closure object.
  ;;     => (lambda (closure.ots) ?body0 ?body ...))
  ;;
  ;;    ((<procedure>)
  ;;     ;;The expression returns a single value, typed as "<procedure>".
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    (<no-return>
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
  ;;    ((unspecified-values)
  ;;     ;;The expression returns an unspecified number of values.
  ;;     ?body0 ?body ...)
  ;;
  ;;    (else
  ;;     ;;The expression returns zero, two or more values.
  ;;     ?body0 ?body ...))
  ;;
  (define (main input-form.stx)
    (sys::syntax-case input-form.stx ()
      ((?kwd ?signature . ?clause*)
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
	      (declare signature.no-return?	<no-return>-ots?)
	      (declare signature.empty?		null?)
	      (declare signature.list?		<list>-ots?)
	      (declare signature.list-of?	list-of-type-spec?)
	      #| end of LET-SYNTAX |# )
	    ;;
	    (define (signature.single-value?)
	      single-item?)
	    (define (signature.unspecified-values?)
	      (and (not single-item?)
		   (not (<no-return>-ots? signature.specs))
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
    (sys::syntax-case clause.stx (=> single-value unspecified-values
				     <top> <closure> <procedure> <no-return> <void> <list> <list-of>)
      ((() ?body0 ?body ...)
       (sys::syntax ((signature.empty?)			?body0 ?body ...)))

      ;;;

      (((<void>) ?body0 ?body ...)
       (sys::syntax ((signature.single-void?)		?body0 ?body ...)))

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

      ((<no-return> ?body0 ?body ...)
       (sys::syntax ((signature.no-return?)		?body0 ?body ...)))

      (((unspecified-values) => ?body)
       (sys::syntax ((signature.unspecified-values?)	(?body signature.specs))))
      (((unspecified-values) ?body0 ?body ...)
       (sys::syntax ((signature.unspecified-values?)	?body0 ?body ...)))

      ((<list-of> => ?body)
       (sys::syntax ((signature.list-of?)		(?body signature.specs))))
      ((<list-of> ?body0 ?body ...)
       (sys::syntax ((signature.list-of?)		?body0 ?body ...)))

      ((<list> ?body0 ?body ...)
       (sys::syntax ((signature.list?)			?body0 ?body ...)))

      (_
       (synner "invalid input clause" clause.stx))))

  (main input-form.stx))




;;;; type signature: type definition

(define-record-type (<type-signature> make-type-signature type-signature?)
  (nongenerative vicare:expander:<type-signature>)
  (fields
    (immutable specs	type-signature.object-type-specs)
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
      (case-define* make-type-signature
	((stx)
	 (make-type-signature stx (current-inferior-lexenv)))
	((stx lexenv)
	 (let ((specs (with-exception-handler
			  (lambda (E)
			    (raise
			     (condition E
					(make-who-condition __who__)
					(make-message-condition "invalid type signature")
					(make-irritants-condition (list stx)))))
			(lambda ()
			  (%parse-input-signature __who__ stx lexenv)))))
	   (make-record specs (void) (void) (void) (void) #f #f))))

      (define (%parse-input-signature caller-who input-signature lexenv)
	;;Accept as input:
	;;
	;;* A standalone "<no-return>" identifier.
	;;
	;;*  A   standalone  "<object-type-spec>"  instance  representing   the  type
	;;"<no-return>".
	;;
	;;*  A   proper  or  improper   list  of  type  identifiers,   compound  type
	;;specifications,  instances  of  "<object-type-spec>";  in  standalone  tail
	;;position there must be a list type.
	;;
	#;(debug-print __who__ input-signature)
	(syntax-match input-signature (<no-return> <void>)
	  (<no-return>
	   ;;INPUT-SIGNATURE is a standalone "<no-return>" type identifier.
	   (<no-return>-ots))
	  (?item
	   (<no-return>-ots? ?item)
	   ;;INPUT-SIGNATURE   is   a    standalone   "<object-type-spec>"   instance
	   ;;representing the type "<no-return>".
	   input-signature)
          ;; ((<void>)
          ;;  ;;"<void>" is fine if it is the single component of a proper list.
          ;;  (list (<void>-ots)))
          ;; ((?item)
          ;;  (<void>-ots? ?item)
          ;;  (list (<void>-ots)))
	  (_
	   ;;INPUT-SIGNATURE must  be a proper  or improper list of  type identifiers
	   ;;and/or instances of "<object-type-spec>".
	   (let recur ((stx input-signature))
	     (syntax-match stx (<list> <no-return> list-of)
	       (()
		;;STX is a proper list.  Good.
		'())

	       (<list>
		(<list>-ots))

	       ((list-of ?item-type)
		(make-list-of-type-spec (type-annotation->object-type-spec ?item-type lexenv)))

	       (<no-return>
		(syntax-violation caller-who
		  "type signature component \"<no-return>\" in invalid position"
		  input-signature stx))

	       (?rest-ots
		(object-type-spec? ?rest-ots)
		(cond ((or (list-of-type-spec? ?rest-ots)
			   (<list>-ots? ?rest-ots))
		       ?rest-ots)
		      (else
		       (syntax-violation caller-who
			 "expected list sub-type object-type specification as signature component in tail position"
			 input-signature ?rest-ots))))

	       (?rest-id
		(identifier? ?rest-id)
		;;This is meant to match type identifiers defined as:
		;;
		;;   (define-type <list-of-fixnums> (list-of <fixnum>))
		;;   (define-type <some-list> <list>)
		;;
		(let ((rest.ots (with-exception-handler
				    (lambda (E)
				      (raise-continuable (condition E (make-who-condition 'make-type-signature))))
				  (lambda ()
				    (id->object-type-spec ?rest-id lexenv)))))
		  (cond ((or (list-of-type-spec? rest.ots)
			     (<list>-ots?        rest.ots))
			 rest.ots)
			(else
			 (raise
			  (condition (make-who-condition caller-who)
				     (make-message-condition "expected list type identifier as signature component in tail position")
				     (make-syntax-violation input-signature ?rest-id)))))))

	       ((?thing . ?rest)
		(cons (let ((ots (if (object-type-spec? ?thing)
				     ?thing
				   (type-annotation->object-type-spec ?thing lexenv))))
			(cond
			 ;; ((<no-return>-ots? ots)
			 ;;  (syntax-violation caller-who
			 ;;    "type signature component \"<no-return>\" in invalid position"
			 ;;    input-signature ?thing))
			 ;; ((<void>-ots? ots)
			 ;;  (syntax-violation caller-who
			 ;; 	 "type signature component \"<void>\" in invalid position"
			 ;; 	 input-signature ?thing))
			 (else ots)))
		      (recur ?rest)))

	       (_
		(syntax-violation caller-who
		  "expected type identifier or object-type specification as signature component"
		  input-signature stx)))))))

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

(define <type-signature>-rtd
  (record-type-descriptor <type-signature>))

(define <type-signature>-rcd
  (record-constructor-descriptor <type-signature>))

(define-list-of-type-predicate list-of-type-signatures? type-signature?)


;;;; type signature: special constructors

(let*-syntax
    ((define-cached-signature-maker
       (syntax-rules ()
	 ((_ ?who ?stx-maker)
	  (define ?who
	    (let ((rvs #f))
	      (lambda ()
		(or rvs
		    (receive-and-return (S)
			(make-type-signature ?stx-maker)
		      (set! rvs S)))))))))
     (define-single-type-signature-maker
       (syntax-rules ()
	 ((_ ?who ?type-id-maker)
	  (define-cached-signature-maker ?who (list (?type-id-maker)))))))
  (define-single-type-signature-maker make-type-signature/single-top			<top>-ots)
  (define-single-type-signature-maker make-type-signature/single-void			<void>-ots)
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
  (define-cached-signature-maker make-type-signature/no-return				(<no-return>-ots))
  #| end of LET-SYNTAX |# )

(define-syntax-rule (make-type-signature/fully-untyped)
  (make-type-signature/standalone-list))

(define* (make-type-signature/single-value type-annotation)
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

(define* (type-signature.fully-untyped? {signature type-signature?})
  ;;Return true  if the type  signature specifies  neither object types,  nor objects
  ;;count; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-fully-untyped? type-signature.memoised-fully-untyped?-set!
   (<list>-ots? (type-signature.object-type-specs signature))))

(define* (type-signature.partially-untyped? {signature type-signature?})
  ;;Return true if the  type signature has at least one  untyped item, either "<top>"
  ;;or "<list>"; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-partially-untyped? type-signature.memoised-partially-untyped?-set!
   (let loop ((specs (type-signature.object-type-specs signature))
	      (rv    #f))
     (cond ((pair? specs)
	    (loop (cdr specs) (<top>-ots? (car specs))))
	   ((<list>-ots? specs)
	    ;;End of IMproper list.
	    #t)
	   (else
	    ;;End of proper list.
	    rv)))))

(define* (type-signature.untyped? {signature type-signature?})
  ;;Return  true if  the type  signature has  only untyped  items, either  "<top>" or
  ;;"<list>"; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-untyped? type-signature.memoised-untyped?-set!
   (let loop ((specs (type-signature.object-type-specs signature)))
     (cond ((pair? specs)
	    (and (<top>-ots? (car specs))
		 (loop (cdr specs))))
	   ((<list>-ots? specs))
	   (else #f)))))

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
	 (<no-return>-ots? specs))))


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


;;;; matching: signatures super-type and sub-type matching

(define* (type-signature.super-and-sub? super-signature sub-signature)
  ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE  have the same structure and the
  ;;identifiers in  the homologous  position are  super-type and  sub-type; otherwise
  ;;return false.
  ;;
  (define-constant the-who __who__)
  (let  ((super-specs	(type-signature.object-type-specs super-signature))
	 (sub-specs	(type-signature.object-type-specs sub-signature)))
    (cond
     ((<no-return>-ots? super-specs)
      (<no-return>-ots? sub-specs))
     ((<no-return>-ots? sub-specs)
      #f)
     (else
      (let recur ((super-specs	super-specs)
		  (sub-specs	sub-specs))
	(cond
	 ((pair? super-specs)
	  (cond ((pair? sub-specs)
		 ;;If the super-type is actually a super type of the sub-type good.
		 (and (object-type-spec.matching-super-and-sub? (car super-specs) (car sub-specs))
		      (recur (cdr super-specs) (cdr sub-specs))))
		(else
		 ;;There are more super-types than sub-types, for example:
		 ;;
		 ;;  super-signature == #'(<number>  <fixnum> <string)
		 ;;  sub-signature   == #'(<complex> <fixnum>)
		 ;;
		 ;;or the sub signature is an improper list:
		 ;;
		 ;;  super-signature == #'(<number>  <fixnum> <string)
		 ;;  sub-signature   == #'(<complex> <fixnum> . <list>)
		 ;;
		 ;;we want these cases to fail matching.
		 #f)))

	 ((null? super-specs)
	  ;;Return true if both the signatures  are proper lists with the same number
	  ;;of items, and all the items are correct super and sub.
	  (null? sub-specs))

	 ((<list>-ots? super-specs)
	  ;;The super signature is an improper  list: either a standalone "<list>" or
	  ;;a  list with  a  "<list>" in  tail  position.  As  example,  we want  the
	  ;;following to match successfully:
	  ;;
	  ;;  super-signature == #'(<number>  <fixnum> . <list>)
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'<list>
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'(<fixnum> <fixnum> . <list>)
	  ;;  sub-signature   == #'(<fixnum> <fixnum> <string> <vector>)
	  ;;
	  #t)

	 ((list-of-type-spec? super-specs)
	  ;;The  super   signature  is   an  improper   list:  either   a  standalone
	  ;;"<list-of-type-spec>"  or a  list  with a  "<list-of-type-spec>" in  tail
	  ;;position.   The super-signature  accepts  any number  of  arguments of  a
	  ;;specified type.
	  ;;
	  ;;As example, we want the following to match successfully:
	  ;;
	  ;;  super-signature == #'(<number>  <fixnum> . (list-of <string>))
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'(list-of <number>)
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'(<fixnum> <fixnum> . (list-of <number>))
	  ;;  sub-signature   == #'(<fixnum> <fixnum> <integer> <rational>)
	  ;;
	  (let ((super-item.ots (list-of-type-spec.item-ots super-specs)))
	    (or (<top>-ots? super-item.ots)
		(let item-recur ((sub-specs sub-specs))
		  (cond
		   ;;This is the case:
		   ;;
		   ;;  super-signature == #'(<number>  <fixnum> . (list-of <string>))
		   ;;  sub-signature   == #'(<complex> <fixnum>)
		   ;;
		   ;;and we want it to match successfully.
		   ((null? sub-specs))

		   ((pair? sub-specs)
		    ;;We want the following signatures to match:
		    ;;
		    ;;  super-signature == #'(<number>  . (list-of <fixnum>))
		    ;;  sub-signature   == #'(<complex> <fixnum> <fixnum>)
		    ;;
		    (and (object-type-spec.matching-super-and-sub? super-item.ots (car sub-specs))
			 (item-recur (cdr sub-specs))))

		   ((<list>-ots? sub-specs)
		    ;;This is the case:
		    ;;
		    ;;  super-signature == #'(<number>  . (list-of <fixnum>))
		    ;;  sub-signature   == #'(<complex> . <list>)
		    ;;
		    ;;and we want it to fail matching.
		    #f)

		   ((list-of-type-spec? sub-specs)
		    ;;This is the case:
		    ;;
		    ;;  super-signature == #'(<string> <string> . (list-of <number>))
		    ;;  sub-signature   == #'(<string> <string> . (list-of <fixnum>))
		    ;;
		    ;;and we want it to match if the item OTSs match.
		    (let ((sub-item.ots (list-of-type-spec.item-ots sub-specs)))
		      (object-type-spec.matching-super-and-sub? super-item.ots sub-item.ots)))

		   (else
		    (assertion-violation the-who "invalid sub-signature" sub-signature)))))))

	 (else
	  (assertion-violation the-who "invalid super-signature" super-signature))))))))

(define* (type-signature.compatible-super-and-sub? super-signature sub-signature)
  ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE  have the same structure and the
  ;;identifiers in  the homologous position  are compatible super-type  and sub-type;
  ;;otherwise return false.
  ;;
  (define-constant the-who __who__)
  (let  ((super-specs	(type-signature.object-type-specs super-signature))
	 (sub-specs	(type-signature.object-type-specs sub-signature)))
    (cond
     ((<no-return>-ots? super-specs)
      (<no-return>-ots? sub-specs))
     ((<no-return>-ots? sub-specs)
      #f)
     (else
      (let recur ((super-specs	super-specs)
		  (sub-specs		sub-specs))
	(cond
	 ((pair? super-specs)
	  (cond ((pair? sub-specs)
		 ;;If the super-type is actually a super type of the sub-type good.
		 (and (or (object-type-spec.matching-super-and-sub?   (car super-specs) (car sub-specs))
			  (object-type-spec.compatible-super-and-sub? (car super-specs) (car sub-specs)))
		      (recur (cdr super-specs) (cdr sub-specs))))
		(else
		 ;;There are more super-types than sub-types, for example:
		 ;;
		 ;;  super-signature == #'(<number>  <fixnum> <string)
		 ;;  sub-signature   == #'(<complex> <fixnum>)
		 ;;
		 ;;or the sub signature is an improper list:
		 ;;
		 ;;  super-signature == #'(<number>  <fixnum> <string)
		 ;;  sub-signature   == #'(<complex> <fixnum> . <list>)
		 ;;
		 ;;we want these cases to fail matching.
		 #f)))

	 ((null? super-specs)
	  ;;Return true if both the signatures  are proper lists with the same number
	  ;;of items, and all the items are correct super and sub.
	  (null? sub-specs))

	 ((<list>-ots? super-specs)
	  ;;The super signature is an improper  list: either a standalone "<list>" or
	  ;;a  list with  a  "<list>" in  tail  position.  As  example,  we want  the
	  ;;following to match successfully:
	  ;;
	  ;;  super-signature == #'(<number>  <fixnum> . <list>)
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'<list>
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'(<fixnum> <fixnum> . <list>)
	  ;;  sub-signature   == #'(<fixnum> <fixnum> <string> <vector>)
	  ;;
	  #t)

	 ((list-of-type-spec? super-specs)
	  ;;The  super   signature  is   an  improper   list:  either   a  standalone
	  ;;"<list-of-type-spec>"  or a  list  with a  "<list-of-type-spec>" in  tail
	  ;;position.   The super-signature  accepts  any number  of  arguments of  a
	  ;;specified type.
	  ;;
	  ;;As example, we want the following to match successfully:
	  ;;
	  ;;  super-signature == #'(<number>  <fixnum> . (list-of <string>))
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'(list-of <number>)
	  ;;  sub-signature   == #'(<complex> <fixnum>)
	  ;;
	  ;;and the following to match successfully:
	  ;;
	  ;;  super-signature == #'(<fixnum> <fixnum> . (list-of <number>))
	  ;;  sub-signature   == #'(<fixnum> <fixnum> <integer> <rational>)
	  ;;
	  (let ((super-item.ots (list-of-type-spec.item-ots super-specs)))
	    (or (<top>-ots? super-item.ots)
		(let item-recur ((sub-specs sub-specs))
		  (cond
		   ;;This is the case:
		   ;;
		   ;;  super-signature == #'(<number>  <fixnum> . (list-of <string>))
		   ;;  sub-signature   == #'(<complex> <fixnum>)
		   ;;
		   ;;and we want it to match successfully.
		   ((null? sub-specs))

		   ((pair? sub-specs)
		    ;;We want the following signatures to match:
		    ;;
		    ;;  super-signature == #'(<number>  . (list-of <fixnum>))
		    ;;  sub-signature   == #'(<complex> <fixnum> <fixnum>)
		    ;;
		    (and (or (object-type-spec.matching-super-and-sub?   super-item.ots (car sub-specs))
			     (object-type-spec.compatible-super-and-sub? super-item.ots (car sub-specs)))
			 (item-recur (cdr sub-specs))))

		   ((<list>-ots? sub-specs)
		    ;;This is the case:
		    ;;
		    ;;  super-signature == #'(<number>  . (list-of <fixnum>))
		    ;;  sub-signature   == #'(<complex> . <list>)
		    ;;
		    ;;and we want it to fail matching.
		    #f)

		   ((list-of-type-spec? sub-specs)
		    ;;This is the case:
		    ;;
		    ;;  super-signature == #'(<string> <string> . (list-of <number>))
		    ;;  sub-signature   == #'(<string> <string> . (list-of <fixnum>))
		    ;;
		    ;;and we want it to match if the item OTSs match.
		    (let ((sub-item.ots (list-of-type-spec.item-ots sub-specs)))
		      (or (object-type-spec.matching-super-and-sub?   super-item.ots sub-item.ots)
			  (object-type-spec.compatible-super-and-sub? super-item.ots sub-item.ots))))

		   (else
		    (assertion-violation the-who "invalid sub-signature" sub-signature)))))))

	 (else
	  (assertion-violation the-who "invalid super-signature" super-signature))))))))


;;;; matching: arguments and operands

(module (type-signature.match-arguments-against-operands)
  (define-module-who type-signature.match-arguments-against-operands)

  (define* (type-signature.match-arguments-against-operands args.sig rands.sig)
    ;;In the context of a closure object application to fixed operands:
    ;;
    ;;   (?operator ?operand ...)
    ;;
    ;;compare the type signature of the operator's arguments to the type signature of
    ;;the  given  operands.   Return  a symbol  among:  exact-match,  possible-match,
    ;;no-match.
    ;;
    ;;ARGS.SIG is a "<type-signature>" instance  representing the type signature of a
    ;;closure object's clause arguments.
    ;;
    ;;RANDS.SIG is a  "<type-signature>" instance representing the  type signature of
    ;;the given operands.
    ;;
    (let loop ((state		'exact-match)
	       (args.ots	(type-signature.object-type-specs args.sig))
	       (rands.ots	(type-signature.object-type-specs rands.sig)))
      ;;In  this loop  the  variable  STATE always  degrades:  from "exact-match"  to
      ;;"possible-match"  or "no-match";  from  "possible-match"  to "no-match".   It
      ;;never upgrades.
      (cond

       ((pair? args.ots)
	;;The operator accepts one more mandatory operand.
	(cond ((pair? rands.ots)
	       ;;One more argument and one more operand.  Good, let's inspect them.
	       (let ((arg.ots	(car args.ots))
		     (rand.ots	(car rands.ots)))
		 (cond ((object-type-spec.matching-super-and-sub? arg.ots rand.ots)
			;;The argument matches the operand.  Good.
			(loop state (cdr args.ots) (cdr rands.ots)))
		       ((object-type-spec.compatible-super-and-sub? arg.ots rand.ots)
			;;The argument is compatible with the operand.  Good.
			(loop 'possible-match (cdr args.ots) (cdr rands.ots)))
		       (else
			;;The argument is INcompatible with the operand.  Bad.
			'no-match))))
	      ((null? rands.ots)
	       ;;One more argument and no more operands.  Bad.
	       'no-match)
	      ((<list>-ots? rands.ots)
	       ;;There is an unspecified number of rest operands, of unknown type.
	       'possible-match)
	      (else
	       ;;There is an unspecified number of rest operands, of known type.
	       (%match-arguments-against-rest-operands args.ots (list-of-type-spec.item-ots rands.ots)))))

       ((null? args.ots)
	;;The operator accepts no more operands.
	(cond ((pair? rands.ots)
	       ;;No more arguments and leftover operands.  Bad.
	       'no-match)
	      ((null? rands.ots)
	       ;;No more  arguments and  no more  operands.  Good.   And we  are done
	       ;;here, let's return the final state.
	       state)
	      (else
	       ;;There is an unspecified number of rest operands, of unknown type.
	       (assert (or (<list>-ots? rands.ots) (list-of-type-spec? rands.ots)))
	       'possible-match)))

       ((<list>-ots? args.ots)
	;;The operator accepts zero or more operands of any type.  Example:
	;;
	;;   ((lambda (arg . {rest <list>}) ?rator-body)
	;;    ?rand ...)
	;;
	;;another example:
	;;
	;;   ((lambda {args <list>} ?rator-body)
	;;    ?rand ...)
	;;
	;;Good.  And we are done here, let's return the final state.
	state)

       (else
	;;The operator accepts zero or more operands of a specified type.  Example:
	;;
	;;   ((lambda (arg . {rest (list-of <fixnum>)}) ?rator-body)
	;;    ?rand ...)
	;;
	;;another example:
	;;
	;;   ((lambda {args (list-of <fixnum>)} ?rator-body)
	;;    ?rand ...)
	;;
	(%match-rest-argument-against-operands state (list-of-type-spec.item-ots args.ots) rands.ots)))))

  (define (%match-rest-argument-against-operands state item.ots rands.ots)
    ;;Recursive function.   We use this  function when  the operator accepts  zero or
    ;;more operands of a specified type.  Example:
    ;;
    ;;   ((lambda (arg . {rest (list-of <fixnum>)}) ?rator-body)
    ;;    ?rand ...)
    ;;
    ;;another example:
    ;;
    ;;   ((lambda {args (list-of <fixnum>)} ?rator-body)
    ;;    ?rand ...)
    ;;
    ;;The argument ITEM.OTS  is an instance of  "<object-type-spec>" representing the
    ;;requested type  for all  rest operands.   The argument RANDS.OTS  is a  list of
    ;;"<object-type-spec>"  instances  representing  the  types  of  the  given  rest
    ;;operands.
    ;;
    (cond ((pair? rands.ots)
	   ;;At least one more operand.  Let's match it against the argument.
	   (cond ((object-type-spec.matching-super-and-sub?   item.ots (car rands.ots))
		  ;;The argument matches the operand.  Good.
		  (%match-rest-argument-against-operands state item.ots (cdr rands.ots)))
		 ((object-type-spec.compatible-super-and-sub? item.ots (car rands.ots))
		  ;;The argument is compatible with the operand.  Good.
		  (%match-rest-argument-against-operands 'possible-match item.ots (cdr rands.ots)))
		 (else
		  ;;The argument is INcompatible with the operand.  Bad.
		  'no-match)))
	  ((null? rands.ots)
	   ;;No more operands.  Good.
	   state)
	  ((<list>-ots? rands.ots)
	   ;;There is an unspecified number of rest operands, with an unknown type.
	   'possible-match)
	  (else
	   ;;There is an unspecified number of rest operands, with a known type.
	   (let ((rand.ots (list-of-type-spec.item-ots rands.ots)))
	     (cond ((object-type-spec.matching-super-and-sub? item.ots rand.ots)
		    ;;The rest argument matches the rest operands.  Good.
		    state)
		   ((object-type-spec.compatible-super-and-sub? item.ots rand.ots)
		    ;;The rest argument is compatible with the rest operands.  Good.
		    'possible-match)
		   (else
		    ;;The rest argument is INcompatible with the rest operand.  Bad.
		    'no-match))))))

  (define (%match-arguments-against-rest-operands args.ots rand.ots)
    ;;Recursive  function.  We  use  this  function when  the  operator accepts  more
    ;;arguments  of a  specified type  and  there is  an unspecified  number of  rest
    ;;operands of known type.
    ;;
    ;;This function returns a symbol among: possible-match, no-match.  Exact match is
    ;;already excluded.
    ;;
    ;;The  argument ARGS.OTS  is a  proper or  improper list  of "<object-type-spec>"
    ;;representing the requested  type for all rest operands.   The argument RAND.OTS
    ;;is an "<object-type-spec>" instance representing the type of all the given rest
    ;;operands.
    ;;
    (cond ((pair? args.ots)
	   ;;At least one more argument.  Let's match it against the argument.
	   (cond ((object-type-spec.matching-super-and-sub? (car args.ots) rand.ots)
		  ;;The argument matches the operand.  Good.
		  (%match-arguments-against-rest-operands (cdr args.ots) rand.ots))
		 ((object-type-spec.compatible-super-and-sub? (car args.ots) rand.ots)
		  ;;The argument is compatible with the operand.  Good.
		  (%match-arguments-against-rest-operands (cdr args.ots) rand.ots))
		 (else
		  ;;The argument is INcompatible with the operand.  Bad.
		  'no-match)))
	  ((null? args.ots)
	   ;;No more arguments.  Good.
	   'possible-match)
	  ((<list>-ots? args.ots)
	   ;;There is an unspecified number of rest arguments, with an unknown type.
	   'possible-match)
	  (else
	   ;;There is an unspecified number of rest arguments, with a known type.
	   (let ((arg.ots (list-of-type-spec.item-ots args.ots)))
	     (cond ((object-type-spec.matching-super-and-sub? arg.ots rand.ots)
		    ;;The rest argument matches the rest operand.  Good.
		    'possible-match)
		   ((object-type-spec.compatible-super-and-sub? arg.ots rand.ots)
		    ;;The rest argument is compatible with the rest operand.  Good.
		    'possible-match)
		   (else
		    ;;The rest argument is INcompatible with the rest operand.  Bad.
		    'no-match))))))

  #| end of module: TYPE-SIGNATURE.MATCH-ARGUMENTS-AGAINST-OPERANDS |# )


;;;; matching: common ancestor

(case-define* type-signature.common-ancestor
  ;;Given a  type signature arguments: return  a new type signature  representing the
  ;;common ancestor.
  ;;
  (()
   (make-type-signature/fully-untyped))
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
		   ;;are    either    "<no-return>"    OTSs,   "<list>"    OTSs    or
		   ;;"<list-of-type-spec>"  instances; so  their  common ancestor  is
		   ;;either the "<list>" OTS or a "<list-of-type-spec>" instance.
		   (object-type-spec.common-ancestor specs1 specs2))))))))


;;;; matching: union

(case-define* type-signature.union
  ;;Given a  type signature arguments: return  a new type signature  representing the
  ;;union.
  ;;
  (()
   (make-type-signature/fully-untyped))
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
      ((<no-return>-ots? specs1)	specs2)
      ((<no-return>-ots? specs2)	specs1)

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
		       (<list>-ots))
		      (else
		       ;;SIG2 is an improper list shorter that SIG1.
		       (<list>-ots))))

	       ((null? specs1)
		(if (null? specs2)
		    ;;Both the  signatures are proper  lists with the same  number of
		    ;;items: success!
		    '()
		  ;;SIG1 is a proper list shorter that SIG2.
		  (<list>-ots)))

	       (else
		;;SIG1 is an improper list.
		(cond ((null? specs2)
		       ;;SIG2 is an proper list shorter that SIG1.
		       (<list>-ots))
		      ((pair? specs2)
		       ;;SIG2 is an proper list longer that SIG1.
		       (<list>-ots))
		      (else
		       ;;Both SIG1 and  SIG2 are improper lists with  the same number
		       ;;of items.
		       (union-of-type-specs specs1 specs2)))))))))))


;;;; matching: union with same number of operands

(case-define* type-signature.union-same-number-of-operands
  ;;Given a  type signature arguments: return  a new type signature  representing the
  ;;union.  UNION-SYNNER must be a procedure used to signal errors.
  ;;
  (({union-synner procedure?})
   (make-type-signature/fully-untyped))
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
      ((<no-return>-ots? specs1)	specs2)
      ((<no-return>-ots? specs2)	specs1)

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


;;;; helpers and utilities

(define (datum-type-signature datum)
  ;;Build and  return a new instance  of "<type-signature>" representing the  type of
  ;;the single value returned by the expression  DATUM, which must be a Scheme object
  ;;extracted from a syntax object representing a literal expression.
  ;;
  (make-type-signature/single-value (datum-type-annotation datum)))

(define (datum-type-annotation datum)
  ;;Recursive  function.  Build  and return  a  syntax object  representing the  type
  ;;annotation of DATUM, which must be a Scheme object extracted from a syntax object
  ;;representing a literal expression.
  ;;
  ;;We use a hashtable  to detect circular structures in DATUM; we  put in here pairs
  ;;and vectors.
  (define table (make-eq-hashtable))
  (let recur ((datum datum))
    (cond ((boolean? datum)		(cond (datum
					       (<true>-type-id))
					      (else
					       (<false>-type-id))))
	  ((char?    datum)		(core-prim-id '<char>))
	  ((symbol?  datum)		(list (enumeration-id) (make-syntactic-identifier-for-quoted-symbol datum)))
	  ((keyword? datum)		(core-prim-id '<keyword>))

	  ((fixnum?  datum)		(cond ((fxpositive? datum)
					       (core-prim-id '<positive-fixnum>))
					      ((fxnegative? datum)
					       (core-prim-id '<negative-fixnum>))
					      ((fxzero? datum)
					       (core-prim-id '<zero-fixnum>))
					      (else
					       ;;This should never happen.
					       (core-prim-id '<fixnum>))))
	  ((flonum?  datum)		(cond ((flpositive? datum)
					       (core-prim-id '<positive-flonum>))
					      ((flnegative? datum)
					       (core-prim-id '<negative-flonum>))
					      ((flzero?/positive datum)
					       (core-prim-id '<positive-zero-flonum>))
					      ((flzero?/negative datum)
					       (core-prim-id '<negative-zero-flonum>))
					      (else
					       ;;This  happens  when  the  flonum  is
					       ;;not-a-number.
					       (core-prim-id '<flonum>))))
	  ((ratnum?  datum)		(cond ((ratnum-positive? datum)
					       (core-prim-id '<positive-ratnum>))
					      ((ratnum-negative? datum)
					       (core-prim-id '<negative-ratnum>))
					      (else
					       ;;This should never happen.
					       (core-prim-id '<ratnum>))))
	  ((bignum?  datum)		(cond ((bignum-positive? datum)
					       (core-prim-id '<positive-bignum>))
					      ((bignum-negative? datum)
					       (core-prim-id '<negative-bignum>))
					      (else
					       ;;This should never happen.
					       (core-prim-id '<bignum>))))
	  ((compnum? datum)		(cond ((exact-compnum? datum)
					       (core-prim-id '<exact-compnum>))
					      ((zero-compnum? datum)
					       (core-prim-id '<zero-compnum>))
					      (else
					       (core-prim-id '<non-zero-inexact-compnum>))))
	  ((cflonum? datum)		(cond ((zero-cflonum? datum)
					       (core-prim-id '<zero-cflonum>))
					      (else
					       (core-prim-id '<non-zero-cflonum>))))
	  ((string?  datum)		(core-prim-id '<string>))

	  ((null? datum)		(<null>-type-id))

	  ((list? datum)		(if (hashtable-ref table datum #f)
					    (<nelist>-type-id)
					  (begin
					    (let pair-recur ((P datum))
					      (when (pair? P)
						(hashtable-set! table P #t)
						(pair-recur (cdr P))))
					    (cons (core-prim-id 'list)
						  (map recur datum)))))

	  ((pair? datum)		(if (hashtable-ref table datum #f)
					    (<pair>-type-id)
					  (begin
					    (hashtable-set! table datum #t)
					    (list (core-prim-id 'pair)
						  (recur (car datum))
						  (recur (cdr datum))))))

	  ((vector?  datum)		(if (hashtable-ref table datum #f)
					    (<nevector>-type-id)
					  (begin
					    (hashtable-set! table datum #t)
					    (cond ((vector-empty? datum)
						   (<empty-vector>-type-id))
						  (else
						   (cons (core-prim-id 'vector)
							 (map recur (vector->list datum))))))))

	  ((bytevector? datum)		(core-prim-id '<bytevector>))

	  ((eq? datum (void))		(<void>-type-id))
	  (else				(<top>-type-id)))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
