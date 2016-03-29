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
   type-signature.specs					type-signature.tags

;;; special constructors
   make-type-signature/single-top			make-type-signature/single-void
   make-type-signature/single-null
   make-type-signature/single-boolean
   make-type-signature/single-true			make-type-signature/single-false
   make-type-signature/single-procedure
   make-type-signature/single-stx			make-type-signature/single-syntactic-identifier
   make-type-signature/standalone-list			make-type-signature/fully-untyped
   make-type-signature/single-value

;;; comparison
   type-signature=?

;;; predicates
   type-signature.fully-untyped?			type-signature.partially-untyped?
   type-signature.untyped?				type-signature.empty?
   type-signature.super-and-sub?			type-signature.compatible-super-and-sub?
   type-signature.single-type?				type-signature.single-top-tag?
   type-signature.single-type-or-fully-untyped?		type-signature.no-return?

   type-signature.match-arguments-against-fixed-operands

;;; accessors
   type-signature.min-count				type-signature.max-count
   type-signature.min-and-max-counts

   type-signature.common-ancestor			datum-type-signature

;;; helpers
   case-signature-specs
   single-value unspecified-values <list-of> <closure>

   #| end of exports |# )

  (import PSYNTAX-TYPE-SYNTAX-OBJECTS)


;;;; syntax helpers

(define-auxiliary-syntaxes single-value unspecified-values <closure> <list-of>)

(define-syntax* (case-signature-specs input-form.stx)
  ;;A typical use for an expression used as operand or similar:
  ;;
  ;;  (case-signature-specs expr.sig
  ;;    ((single-value)
  ;;     ;;The expression returns a single value.
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    ((unspecified-values)
  ;;     ;;The expression returns an unspecified number of values.
  ;;     ?body0 ?body ...))
  ;;
  ;;    ((<no-return>)
  ;;     ;;The expression is marked as not-returning.
  ;;     ?body0 ?body ...)
  ;;
  ;;    (else
  ;;     ;;The expression returns zero, two or more values.
  ;;     ?body0 ?body ...))
  ;;
  ;;A typical use for an expression used as operator in an application form:
  ;;
  ;;  (case-signature-specs expr.sig
  ;;    ((<closure>)
  ;;     ;;The expression returns a single value, marked as closure object.
  ;;     => (lambda (closure.ots) ?body0 ?body ...))
  ;;
  ;;    ((<procedure>)
  ;;     ;;The expression returns a single value, marked as "<procedure>".
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    ((single-value)
  ;;     ;;The expression returns a single value, but not a procedure.
  ;;     => (lambda (obj.ots) ?body0 ?body ...))
  ;;
  ;;    ((unspecified-values)
  ;;     ;;The expression returns an unspecified number of values.
  ;;     ?body0 ?body ...))
  ;;
  ;;    ((<no-return>)
  ;;     ;;The expression is marked as not-returning.
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
		 (signature.specs	(type-signature.specs signature))
		 (single-item?		(list-of-single-item? signature.specs)))
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
    (sys::syntax-case clause.stx (=> single-value unspecified-values <top> <closure> <procedure> <no-return> <list> <list-of>)
      (((<top>) ?body0 ?body ...)
       (sys::syntax
	((and single-item? (<top>-ots? (car signature.specs))) ?body0 ?body ...)))

      (((<closure>) => ?body)
       ;;Here we do need the OTS of the single value type.
       (sys::syntax
	((and single-item? (closure-type-spec? (car signature.specs))) (?body (car signature.specs)))))

      (((<closure>) ?body0 ?body ...)
       ;;Here we do not need the OTS of the single value type.
       (sys::syntax
	((and single-item? (closure-type-spec? (car signature.specs))) ?body0 ?body ...)))

      (((<procedure>) ?body0 ?body ...)
       ;;Here we do not need the OTS of the single value type.
       (sys::syntax
	((and single-item? (<procedure>-ots? (car signature.specs))) ?body0 ?body ...)))

      (((single-value) => ?body)
       ;;Here we do need the OTS of the single value type.
       (sys::syntax
	(single-item? (?body (car signature.specs)))))

      (((single-value) ?body0 ?body ...)
       ;;Here we do not need the OTS of the single value type.
       (sys::syntax
	(single-item? ?body0 ?body ...)))

      (((unspecified-values) ?body0 ?body ...)
       (sys::syntax
	((and (not single-item?)
	      (not (<no-return>-ots? signature.specs))
	      (or (list-of-type-spec? signature.specs)
		  (<list>-ots? signature.specs)))
	 ?body0 ?body ...)))

      ((<no-return> ?body0 ?body ...)
       (sys::syntax
	((<no-return>-ots? signature.specs) ?body0 ?body ...)))

      ((<list-of> => ?body)
       (sys::syntax
	((list-of-type-spec? signature.specs) (?body signature.specs))))

      ((<list-of> ?body0 ?body ...)
       (sys::syntax
	((list-of-type-spec? signature.specs) ?body0 ?body ...)))

      ((<list> ?body0 ?body ...)
       (sys::syntax
	((<list>-ots? signature.specs) ?body0 ?body ...)))

      (_
       (synner "invalid input clause" clause.stx))))

  (main input-form.stx))




;;;; type signature: type definition

(define-record-type (<type-signature> make-type-signature type-signature?)
  (nongenerative vicare:expander:<type-signature>)
  (fields
    (immutable specs	type-signature.specs)
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
	;;The compound type specifications accepted are:
	;;
	;;   (pair ?car-thing ?cdr-thing)
	;;   (list   ?item-thing ...)
	;;   (vector ?item-thing ...)
	;;   (pair-of   ?item-thing)
	;;   (list-of   ?item-thing)
	;;   (vector-of ?item-thing)
	;;
	#;(debug-print __who__ input-signature)
	(cond ((<no-return>-type-id? input-signature)
	       ;;INPUT-SIGNATURE is a standalone "<no-return>" type identifier.
	       (<no-return>-ots))
	      ((<no-return>-ots? input-signature)
	       ;;INPUT-SIGNATURE  is   a  standalone   "<object-type-spec>"  instance
	       ;;representing the type "<no-return>".
	       input-signature)
	      (else
	       ;;INPUT-SIGNATURE  must  be   a  proper  or  improper   list  of  type
	       ;;identifiers and/or instances of "<object-type-spec>".
	       (let recur ((stx input-signature))
		 (syntax-match stx (<list> list-of)
		   (()
		    ;;STX is a proper list.  Good.
		    '())

		   (<list>
		    (<list>-ots))

		   ((list-of ?item-type)
		    (make-list-of-type-spec (type-annotation->object-type-specification ?item-type lexenv)))

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
		    (cons (if (object-type-spec? ?thing)
			      ?thing
			    (type-annotation->object-type-specification ?thing lexenv))
			  (recur ?rest)))

		   (_
		    (syntax-violation caller-who
		      "expected type identifier or object-type specification as signature component"
		      input-signature stx)))))))

      make-type-signature))
  ;; (custom-printer
  ;;   (lambda (S port sub-printer)
  ;;     (sub-printer `(<type-signature> ,(type-signature.tags S)))))
  (custom-printer
    (lambda (S port sub-printer)
      (define-syntax-rule (%display ?thing)
	(display ?thing port))
      (define-syntax-rule (%write ?thing)
	(write ?thing port))
      (%display "#[signature ")
      (%display (syntax->datum (type-signature.tags S)))
      (%display "]"))))

(define <type-signature>-rtd
  (record-type-descriptor <type-signature>))

(define <type-signature>-rcd
  (record-constructor-descriptor <type-signature>))


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
  (define-single-type-signature-maker make-type-signature/single-boolean		<boolean>-ots)
  (define-single-type-signature-maker make-type-signature/single-true			<true>-ots)
  (define-single-type-signature-maker make-type-signature/single-false			<false>-ots)
  (define-single-type-signature-maker make-type-signature/single-procedure		<procedure>-ots)
  (define-single-type-signature-maker make-type-signature/single-stx			<stx>-ots)
  (define-single-type-signature-maker make-type-signature/single-syntactic-identifier	<syntactic-identifier>-ots)
  (define-cached-signature-maker make-type-signature/standalone-list			(<list>-ots))
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
  (null? (type-signature.specs signature)))

(define* (type-signature.fully-untyped? {signature type-signature?})
  ;;Return true  if the type  signature specifies  neither object types,  nor objects
  ;;count; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-fully-untyped? type-signature.memoised-fully-untyped?-set!
   (<list>-ots? (type-signature.specs signature))))

(define* (type-signature.partially-untyped? {signature type-signature?})
  ;;Return true if the  type signature has at least one  untyped item, either "<top>"
  ;;or "<list>"; otherwise return false.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-partially-untyped? type-signature.memoised-partially-untyped?-set!
   (let loop ((specs (type-signature.specs signature))
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
   (let loop ((specs (type-signature.specs signature)))
     (cond ((pair? specs)
	    (and (<top>-ots? (car specs))
		 (loop (cdr specs))))
	   ((<list>-ots? specs))
	   (else #t)))))

(define* (type-signature.single-type? {signature type-signature?})
  ;;Return true if SIGNATURE represents a single value; otherwise return false.
  ;;
  (list-of-single-item? (type-signature.specs signature)))

(define* (type-signature.single-top-tag? {signature type-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.
  ;;
  (let ((specs (type-signature.specs signature)))
    (and (list-of-single-item? specs)
	 (<top>-ots? (car specs)))))

(define* (type-signature.single-type-or-fully-untyped? {signature type-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier, otherwise return false.
  ;;
  (let ((specs (type-signature.specs signature)))
    (or (list-of-single-item? specs)
	(<list>-ots? specs))))

(define* (type-signature.no-return? {signature type-signature?})
  (let ((specs (type-signature.specs signature)))
    (and (pair? specs)
	 (null? (cdr specs))
	 (<no-return>-ots? specs))))


;;;; type signature: accessors

(define* (type-signature.tags {signature type-signature?})
  ;;Return a proper  or improper list of identifiers and  syntax objects representing
  ;;the names of the types in the signature.
  ;;
  (%type-signature-memoised-body
   signature type-signature.memoised-tags type-signature.memoised-tags-set!
   (let recur ((specs (type-signature.specs signature)))
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
      (let recur ((specs	(type-signature.specs signature))
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

(define* (type-signature=? {signature1 type-signature?} {signature2 type-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (define (%syntax=? specs1 specs2)
    (cond ((and (pair? specs1)
		(pair? specs2))
	   (and (eq? (car specs1)
		     (car specs2))
		(%syntax=?         (cdr specs1) (cdr specs2))))
	  ((and (null? specs1)
		(null? specs2)))
	  ((eq? specs1 specs2))
	  (else #f)))
  (%syntax=? (type-signature.specs signature1)
	     (type-signature.specs signature2)))


;;;; matching: signatures super-type and sub-type matching

(define* (type-signature.super-and-sub? super-signature sub-signature)
  ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE  have the same structure and the
  ;;identifiers in  the homologous  position are  super-type and  sub-type; otherwise
  ;;return false.
  ;;
  (let recur ((super-specs	(type-signature.specs super-signature))
	      (sub-specs	(type-signature.specs sub-signature)))
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
      ;;Return true if both  the signatures are proper lists with  the same number of
      ;;items, and all the items are correct super and sub.
      (null? sub-specs))

     ((<list>-ots? super-specs)
      ;;The super  signature is an improper  list: either a standalone  "<list>" or a
      ;;list with a "<list>" in tail position.   As example, we want the following to
      ;;match successfully:
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
      ;;The   super   signature   is   an  improper   list:   either   a   standalone
      ;;"<list-of-type-spec>"  or  a  list   with  a  "<list-of-type-spec>"  in  tail
      ;;position.  The super-signature accepts any number of arguments of a specified
      ;;type.
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
		(assertion-violation __who__ "invalid super-signature" sub-signature)))))))

     (else
      (assertion-violation __who__ "invalid super-signature" super-signature)))))

(define* (type-signature.compatible-super-and-sub? super-signature sub-signature)
  ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE  have the same structure and the
  ;;identifiers in  the homologous position  are compatible super-type  and sub-type;
  ;;otherwise return false.
  ;;
  (let recur ((super-specs	(type-signature.specs super-signature))
	      (sub-specs	(type-signature.specs sub-signature)))
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
      ;;Return true if both  the signatures are proper lists with  the same number of
      ;;items, and all the items are correct super and sub.
      (null? sub-specs))

     ((<list>-ots? super-specs)
      ;;The super  signature is an improper  list: either a standalone  "<list>" or a
      ;;list with a "<list>" in tail position.   As example, we want the following to
      ;;match successfully:
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
      ;;The   super   signature   is   an  improper   list:   either   a   standalone
      ;;"<list-of-type-spec>"  or  a  list   with  a  "<list-of-type-spec>"  in  tail
      ;;position.  The super-signature accepts any number of arguments of a specified
      ;;type.
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
		(assertion-violation __who__ "invalid super-signature" sub-signature)))))))

     (else
      (assertion-violation __who__ "invalid super-signature" super-signature)))))


;;;; matching: arguments and operands

(module (type-signature.match-arguments-against-fixed-operands)

  (define* (type-signature.match-arguments-against-fixed-operands args.sig rand*.sig)
    ;;In the context of a closure object application to fixed operands:
    ;;
    ;;   (?operator ?operand ...)
    ;;
    ;;compare the type  signature of the operator's arguments to  the type signatures
    ;;of the operands.  Return a symbol among: exact-match, possible-match, no-match.
    ;;
    ;;ARGS.SIG is a "<type-signature>" instance  representing the type signature of a
    ;;closure object's clause arguments.
    ;;
    ;;RAND*.SIG must be a list  of "<type-signature>" instances representing the type
    ;;signatures of the operands.
    ;;
    (let loop ((state		'exact-match)
	       (args.ots	(type-signature.specs args.sig))
	       (rand*.sig	rand*.sig))
      ;;In  this loop  the  variable  STATE always  degrades:  from "exact-match"  to
      ;;"possible-match"  or "no-match";  from  "possible-match"  to "no-match".   It
      ;;never upgrades.
      (cond

       ((pair? args.ots)
	;;The operator accepts one more mandatory operand.
	(cond ((pair? rand*.sig)
	       ;;One more argument and one more operand.  Good, let's inspect them.
	       (%match-one-argument-against-one-operand state (car args.ots)
							(type-signature.specs (car rand*.sig))
							;;Success continuation.
							(lambda (state)
							  (loop state (cdr args.ots) (cdr rand*.sig)))))
	      ((null? rand*.sig)
	       ;;One more argument and no more operand.  Bad.
	       'no-match)))

       ((null? args.ots)
	;;The operator accepts no more operands.
	(cond ((null? rand*.sig)
	       ;;No more  arguments and  no more  operands.  Good.   And we  are done
	       ;;here, let's return the final state.
	       state)
	      (else
	       ;;No more arguments and leftover operands.  Bad.
	       'no-match)))

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
	(%match-rest-argument-against-operands state args.ots rand*.sig)))))

  (define (%match-one-argument-against-one-operand state arg.ots rand.specs success-kont)
    ;;The argument  ARG.OTS is an  instance of "<object-type-spec>"  representing the
    ;;expected type of the operand.
    ;;
    ;;The argument  RAND.SPECS is a  proper or improper list  of "<object-type-spec>"
    ;;instances representing the types of the operand's expression return values.
    ;;
    (cond ((list-of-single-item? rand.specs)
	   ;;The operand's expression  returns a single value.  Good,  let's match it
	   ;;against the argument.
	   (let ((rand.ots (car rand.specs)))
	     (cond ((object-type-spec.matching-super-and-sub? arg.ots rand.ots)
		    ;;The argument matches the operand.  Good.
		    (success-kont state))
		   ((object-type-spec.compatible-super-and-sub? arg.ots rand.ots)
		    ;;The argument is compatible with the operand.  Good.
		    (success-kont 'possible-match))
		   (else
		    ;;The argument is INcompatible with the operand.  Bad.
		    'no-match))))

	  ((null? rand.specs)
	   ;;The operand's expression returns zero values.  Bad.
	   'no-match)

	  (else
	   ;;The operand's expression  returns an unspecified number  of values.  For
	   ;;example:
	   ;;
	   ;;   (?operator (values ?sub-rand ...))
	   ;;
	   (let ((rand.ots rand.specs))
	     (cond ((<list>-ots? rand.ots)
		    ;;Operand with unspecified return values.  Example:
		    ;;
		    ;;   (?operator (lambda ({_ . <list>}) ?rand-body))
		    ;;
		    (success-kont 'possible-match))
		   (else
		    ;;The  operand  returns  an  unspecified number  of  values  with
		    ;;specified type.  Example:
		    ;;
		    ;;   (?operator (lambda ({_ . <list-of-fixnums>}) ?rand-body))
		    ;;
		    (if (and (list-of-type-spec? rand.ots)
			     (let ((item.ots (list-of-type-spec.item-ots rand.ots)))
			       (or (object-type-spec.matching-super-and-sub? arg.ots item.ots)
				   (object-type-spec.compatible-super-and-sub? arg.ots item.ots))))
			;;If  the  operand  returns  a  single  value:  its  type  is
			;;compatible with the expected argument.  Example:
			;;
			;;   ((lambda ({arg <fixnum>}) ?rator-body)
			;;    (lambda ({_ . (list-of <fixnums>)}) ?rand-body))
			;;
			(success-kont 'possible-match)
		      ;;Even  if the  operand returns  a  single value:  its type  is
		      ;;INcompatible with the expected argument.  Example:
		      ;;
		      ;;   ((lambda ({arg <string>}) ?rator-body)
		      ;;    (lambda ({_ . (list-of <fixnums>)}) ?rand-body))
		      ;;
		      'no-match)))))))

  (define (%match-rest-argument-against-operands state rest.ots rand*.sig)
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
    ;;The argument REST.OTS  is an instance of  "<object-type-spec>" representing the
    ;;requested type  for all  the leftover  operands.  The  argument RAND*.SIG  is a
    ;;proper  list of  "<type-signature>"  instances representing  the  types of  the
    ;;leftover operand's expressions.
    ;;
    (cond ((list-of-type-spec? rest.ots)
	   (let loop ((item.ots		(list-of-type-spec.item-ots rest.ots))
		      (rand*.sig	rand*.sig)
		      (state		state))
	     (if (pair? rand*.sig)
		 ;;At least one more operand.  Let's match it against the argument.
		 (%match-one-argument-against-one-operand state item.ots
							  (type-signature.specs (car rand*.sig))
							  ;;Success continuation.
							  (lambda (state)
							    (loop item.ots (cdr rand*.sig) state)))
	       ;;No more operands.  Good.
	       state)))
	  (else
	   (assertion-violation __who__ "invalid object-type specification for rest argument" rest.ots))))

  #| end of module: type-signature.match-arguments-against-fixed-operands |# )


;;;; matching: common ancestor

(define* (type-signature.common-ancestor {sig1 type-signature?} {sig2 type-signature?})
  ;;Given  a two  type signatures:  return a  new type  signature representing  their
  ;;common ancestor.
  ;;
  (make-type-signature
   (let recur ((specs1 (type-signature.specs sig1))
	       (specs2 (type-signature.specs sig2)))
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
		   ;;are either "<list>" OTSs  or "<list-of-type-spec>" instances; so
		   ;;their  common  ancestor   is  either  the  "<list>"   OTS  or  a
		   ;;"<list-of-type-spec>" instance.
		   (object-type-spec.common-ancestor specs1 specs2))))))))


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
	  ((symbol?  datum)		(core-prim-id '<symbol>))
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
					      (else
					       (core-prim-id '<inexact-compnum>))))
	  ((cflonum? datum)		(core-prim-id '<cflonum>))

	  ((string?  datum)		(core-prim-id '<string>))

	  ((null? datum)		(<null>-type-id))

	  ((list? datum)		(if (hashtable-ref table datum #f)
					    (<list>-type-id)
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
					    (<vector>-type-id)
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
