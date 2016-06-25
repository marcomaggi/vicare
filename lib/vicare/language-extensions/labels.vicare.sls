;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of DEFINE-LABEL
;;;Date: Mon Apr 25, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare language-extensions labels (0 4 2016 6 1))
  (options typed-language)
  (export
    define-label
    parent
    constructor
    destructor
    type-predicate
    equality-predicate
    comparison-procedure
    hash-function
    method
    case-method
    method/overload
    nongenerative)
  (import (vicare)
    (vicare language-extensions mixins (0 4)))


(define-syntax define-label
  (internal-body
    (import (only (psyntax system $all)
		  make-label-type-spec))

    (define-constant __module_who__
      'define-label)

    (define-constant CLAUSE-SPEC*
      (syntax-clauses-validate-specs
       (list
	;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
	(new <syntax-clause-spec> #'parent			1 1      1 +inf.0 '() '())
	(new <syntax-clause-spec> #'constructor			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'destructor			0 1      2 +inf.0 '() '())
	(new <syntax-clause-spec> #'type-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'equality-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'comparison-procedure	0 1      1 1      '() '())
	(new <syntax-clause-spec> #'hash-function		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'case-method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'method/overload		0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'nongenerative		0 1      1 1      '() '())
	#| end of LIST |# )))

    (define-record-type <parsed-clauses>
      (fields
	(immutable type-name)
		;Identifier representing the label type name.
	(mutable uid)
		;Unique identifier associated to this type.
	(mutable parent)
		;After  parsing:  a  syntax   object  representing  the  parent  type
		;annotation.
	(mutable constructor)
		;After parsing:  null or a list  of CASE-DEFINE clauses for  the type
		;constructor.
	(mutable destructor)
		;After parsing: false  of or a list representin  a single CASE-DEFINE
		;clause for the type destructor.
	(mutable type-predicate)
		;After  parsing:  false or  a  syntax  object representing  a  Scheme
		;expression  which,   expanded  and   evaluated,  returns   the  type
		;predicate.
	(mutable equality-predicate)
		;After  parsing:  false or  a  syntax  object representing  a  Scheme
		;expression  which,  expanded  and evaluated,  returns  the  equality
		;predicate.
	(mutable comparison-procedure)
		;After  parsing:  false or  a  syntax  object representing  a  Scheme
		;expression  which, expanded  and evaluated,  returns the  comparison
		;procedure.
	(mutable hash-function)
		;After  parsing:  false or  a  syntax  object representing  a  Scheme
		;expression which, expanded and evaluated, returns the hash function.
	(mutable method*)
		;A list of syntax objects  representing METHOD clauses.  Each item in
		;the list has the format:
		;
		;   #(?method-name ?method-proc ?method-def)
		;
		;where: ?METHOD-NAME  is an identifier representing  the method name;
		;?METHOD-PROC is  an identifier representing  the name of  the method
		;implementation   procedure;   ?METHOD-DEF   is   a   syntax   object
		;representing the definition of the method procedure.
	#| end of FIELDS |# )
      (protocol
	(lambda (make-record)
	  (lambda (type-name)
	    (make-record type-name #f #f '() #f #f #f #f #f '())))))

    (define (main stx synner)
      (syntax-case stx ()
	((_ ?type-name . ?clauses)
	 (if (identifier? #'?type-name)
	     (%parse-clauses #'?type-name #'?clauses synner)
	   (synner "expected identifier as label type name" #'?type-name)))
	(_
	 (synner "invalid DEFINE-LABEL syntax use"))))

    (define (%parse-clauses type-name.id clauses.stx synner)
      (let* ((clause*.stx	(syntax-clauses-unwrap clauses.stx synner))
	     (clause*.stx	(%merge-mixins-clauses type-name.id clause*.stx synner))
	     ({parsed <parsed-clauses>} (syntax-clauses-fold-specs
					 (lambda (parsed spec args)
					   (combine type-name.id parsed spec args synner)
					   parsed)
					 (new <parsed-clauses> type-name.id) CLAUSE-SPEC*
					 clause*.stx synner))
	     (parent.stx	(.parent parsed)))
	(with-syntax
	    ((TYPE-NAME type-name.id))
	  (with-syntax
	      ((UID (or (.uid parsed)
			(datum->syntax type-name.id (gensym (syntax->datum type-name.id)))))
	       ((CONSTRUCTOR-ID CONSTRUCTOR-DEF ...)
		(let ((clause*.stx (.constructor parsed)))
		  (if (null? clause*.stx)
		      '(#f)
		    (let ((clause*.stx (map (lambda (clause.stx)
					      (cons (cons #'(brace _ TYPE-NAME) (car clause.stx))
						    (cdr clause.stx)))
					 (reverse clause*.stx))))
		      (with-syntax
			  ((FUNC (identifier-record-field-accessor type-name.id #'constructor)))
			(list #'(syntax FUNC)
			      #`(case-define/checked FUNC . #,(reverse clause*.stx))))))))

	       ((DESTRUCTOR-ID DESTRUCTOR-DEF ...)
		(cond ((.destructor parsed)
		       => (lambda (clause.stx)
			    (with-syntax
				((FUNC (identifier-record-field-accessor type-name.id #'destructor)))
			      (list #'(syntax FUNC)
				    #`(case-define/checked FUNC #,clause.stx)))))
		      (else '(#f))))

	       ((TYPE-PREDICATE-ID TYPE-PREDICATE-DEF ...)
		(cond ((.type-predicate parsed)
		       => (lambda (stx)
			    (with-syntax
				((FUNC (identifier-record-field-accessor type-name.id #'type-predicate)))
			      (list #'(syntax FUNC)
				    ;;NOTE  We  do   *not*  use  a  TYPE-PREDICATE
				    ;;annotation here because, at present,
				    #`(define/typed {FUNC <type-predicate>}
					(#,stx (is-a? _ #,parent.stx)))))))
		      (else
		       (list #f))))

	       ((EQUALITY-PREDICATE-ID EQUALITY-PREDICATE-DEF ...)
		(cond ((.equality-predicate parsed)
		       => (lambda (stx)
			    (with-syntax
				((FUNC (identifier-record-field-accessor type-name.id #'equality-predicate)))
			      (list #'(syntax FUNC)
				    #`(define/typed {FUNC (equality-predicate TYPE-NAME)}
					(#,stx (equality-predicate #,parent.stx)))))))
		      (else
		       (list #f))))

	       ((COMPARISON-PROCEDURE-ID COMPARISON-PROCEDURE-DEF ...)
		(cond ((.comparison-procedure parsed)
		       => (lambda (stx)
			    (with-syntax
				((FUNC (identifier-record-field-accessor type-name.id #'comparison-procedure)))
			      (list #'(syntax FUNC)
				    #`(define/typed {FUNC (comparison-procedure TYPE-NAME)}
					(#,stx (comparison-procedure #,parent.stx)))))))
		      (else
		       (list #f))))

	       ((HASH-FUNCTION-ID HASH-FUNCTION-DEF ...)
		(cond ((.hash-function parsed)
		       => (lambda (stx)
			    (with-syntax
				((FUNC (identifier-record-field-accessor type-name.id #'hash-function)))
			      (list #'(syntax FUNC)
				    #`(define/typed {FUNC (hash-function TYPE-NAME)}
					(#,stx (hash-function #,parent.stx)))))))
		      (else
		       (list #f))))

	       ((#(METHOD-NAME METHOD-PROC METHOD-DEFINITION) ...)
		(.method* parsed)))
	    #`(module (TYPE-NAME)
		(define-syntax TYPE-NAME
		  (make-label-type-spec
		   (syntax TYPE-NAME)
		   (quote UID)
		   (syntax #,parent.stx)
		   CONSTRUCTOR-ID
		   DESTRUCTOR-ID
		   TYPE-PREDICATE-ID
		   EQUALITY-PREDICATE-ID
		   COMPARISON-PROCEDURE-ID
		   HASH-FUNCTION-ID
		   ;;This methods table must be an alist.
		   (list (cons (quote METHOD-NAME) (syntax METHOD-PROC)) ...)))
		CONSTRUCTOR-DEF			...
		DESTRUCTOR-DEF			...
		TYPE-PREDICATE-DEF		...
		EQUALITY-PREDICATE-DEF		...
		COMPARISON-PROCEDURE-DEF	...
		HASH-FUNCTION-DEF		...
		METHOD-DEFINITION		...)))))

;;; --------------------------------------------------------------------

    (module (%merge-mixins-clauses)

      (define (%merge-mixins-clauses type-name.id input-clause*.stx synner)
	;;The MIXINS clause can be present multiple times.  Each clause must have one
	;;of the formats:
	;;
	;;   (mixins ?mixin-name ...)
	;;
	;;NOTE  We  cannot  process  the MIXINS  clauses  with  the  SYNTAX-CLAUSES-*
	;;facilities because  such facilities change  the order in which  the clauses
	;;are  processed; instead,  the MIXINS  clauses  need to  be spliced  without
	;;changing the order.
	;;
	(fold-right
	    (lambda (input-clause.stx parsed-clause*.stx)
	      (syntax-case input-clause.stx (mixins)
		((mixins . ?mixins)
		 (%splice-mixins type-name.id input-clause.stx #'?mixins parsed-clause*.stx synner))
		(_
		 (cons input-clause.stx parsed-clause*.stx))))
	  '() input-clause*.stx))

      (define (%splice-mixins type-name.id input-clause.stx
			      mixins.stx parsed-clause*.stx synner)
	(syntax-case mixins.stx ()
	  ((?mixin-name . ?mixins)
	   (if (identifier? #'?mixin-name)
	       (append (%splice-single-mixin type-name.id #'?mixin-name synner)
		       (%splice-mixins type-name.id input-clause.stx #'?mixins parsed-clause*.stx synner))
	     (synner "expected identifier as mixin name in MIXINS clause" input-clause.stx)))
	  (()
	   parsed-clause*.stx)
	  (_
	   (synner "invalid syntax in MIXINS clause" input-clause.stx))))

      (define (%splice-single-mixin type-name.id mixin-name.id synner)
	(let ((obj (retrieve-expand-time-value mixin-name.id)))
	  (syntax-case obj (define-mixin)
	    ((define-mixin ?mixin-name . ?clauses)
	     (syntax-replace-id #'?clauses mixin-name.id type-name.id))
	    (_
	     (synner "identifier in MIXINS clause is not a mixin name" mixin-name.id)))))

      #| end of module: %MERGE-MIXINS-CLAUSES |# )

;;; --------------------------------------------------------------------

    (define (combine type-name.id {parsed <parsed-clauses>} {spec <syntax-clause-spec>} args synner)
      (case-identifiers (.keyword spec)
	((method)
	 ;;This clause  can be present multiple  times.  Each input clause  must have
	 ;;the format:
	 ;;
	 ;;   (method (?who . ?args) . ?body)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#((?who . ?args) . ?body) ...)
	 ;;
	 (vector-for-each (lambda (arg)
			    (%process-method-spec parsed arg synner))
	   args))

	((case-method)
	 ;;This clause  can be present multiple  times.  Each input clause  must have
	 ;;the format:
	 ;;
	 ;;   (case-method ?who . ?case-method-clauses)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?who ?case-method-clause0 ?case-method-clause ...))
	 ;;
	 (vector-for-each (lambda (arg)
			    (%process-case-method-spec parsed arg synner))
	   args))

	((method/overload)
	 ;;This clause  can be present multiple  times.  Each input clause  must have
	 ;;the format:
	 ;;
	 ;;   (method/overload (?who . ?args) . ?body)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#((?who . ?args) . ?body) ...)
	 ;;
	 (vector-for-each (lambda (arg)
			    (%process-method-overload-spec parsed arg synner))
	   args))

	((parent)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (parent ?parent-id)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?parent-id))
	 ;;
         (.parent parsed (vector-ref (vector-ref args 0) 0)))

	((constructor)
	 ;;An input clause must have the format:
	 ;;
	 ;;   (constructor ?args ?body0 ?body ...)
	 ;;
	 ;;and there can be any number of them; we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?args ?body0 ?body ...) ...)
	 ;;
	 (vector-for-each
	     (lambda (spec)
	       (syntax-case spec ()
		 (#(?args ?body0 ?body ...)
		  (.constructor parsed (cons #'(?args ?body0 ?body ...)
					     (.constructor parsed))))
		 (#(?stuff ...)
		  (synner "invalid constructor specification" #'(constructor ?stuff ...)))))
	   args))

	((destructor)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (destructor ?args ?body0 ?body ...)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?args ?body0 ?body ...))
	 ;;
	 (syntax-case (vector-ref args 0) ()
	   (#(?args ?body0 ?body ...)
	    (.destructor parsed #'(?args ?body0 ?body ...)))
	   (#(?stuff ...)
	    (synner "invalid destructor specification" #'(destructor ?stuff ...)))))

	((type-predicate)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (predicate ?predicate-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?predicate-stx))
	 ;;
	 (.type-predicate parsed (vector-ref (vector-ref args 0) 0)))

	((equality-predicate)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (equality-predicate ?func-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?func-stx))
	 ;;
	 (.equality-predicate parsed (vector-ref (vector-ref args 0) 0)))

	((comparison-procedure)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (comparison-procedure ?func-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?func-stx))
	 ;;
	 (.comparison-procedure parsed (vector-ref (vector-ref args 0) 0)))

	((hash-function)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (hash-function ?func-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?func-stx))
	 ;;
	 (.hash-function parsed (vector-ref (vector-ref args 0) 0)))

	((nongenerative)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (nongenerative ?uid)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?uid))
	 ;;
	 (.uid parsed (vector-ref (vector-ref args 0) 0)))

	(else
	 (assertion-violation __module_who__ "invalid clause spec" spec))))

    (define (%process-method-spec {parsed <parsed-clauses>} spec synner)
      ;;The METHOD clause can be present multiple times.  Each input clause must have
      ;;the format:
      ;;
      ;;   (method (?who . ?args) . ?body)
      ;;
      ;;and we expect SPEC to have the format:
      ;;
      ;;   #((?who . ?args) . ?body)
      ;;
      (syntax-case spec ()
	(#((?who . ?args) ?body0 ?body ...)
	 (receive (method-name.id method-proc.id method-who.stx)
	     (syntax-case #'?who (brace)
	       (?method-name
		(identifier? #'?method-name)
		(let ((method-proc.id (identifier-record-field-accessor (.type-name parsed) #'?method-name)))
		  (values #'?method-name method-proc.id method-proc.id)))
	       ((brace ?method-name . ?rv-types)
		(identifier? #'?method-name)
		(let ((method-proc.id (identifier-record-field-accessor (.type-name parsed) #'?method-name)))
		  (values #'?method-name method-proc.id #`(brace #,method-proc.id . ?rv-types))))
	       (_
		(synner "invalid method name specification" #'?who)))
	   (.method* parsed (cons (vector method-name.id method-proc.id
					  #`(define/checked (#,method-who.stx . ?args) ?body0 ?body ...))
				  (.method* parsed)))))
	(#(?stuff ...)
	 (synner "invalid method specification" #'(method ?stuff ...)))))

    (define (%process-case-method-spec {parsed <parsed-clauses>} spec synner)
      ;;The CASE-METHOD clause can be present multiple times.  Each input clause must
      ;;have the format:
      ;;
      ;;   (case-method ?who . ?case-method-clauses)
      ;;
      ;;and we expect the SPEC argument to have the format:
      ;;
      ;;   #(?who ?case-method-clause0 ?case-method-clause ...)
      ;;
      (syntax-case spec ()
	(#(?who ?case-method-clause0 ?case-method-clause ...)
	 (.method* parsed (cons (let ((method-proc.id (identifier-record-field-accessor (.type-name parsed) #'?method-name)))
				  (vector #'?who method-proc.id
					  #`(case-define #,method-proc.id ?case-method-clause0 ?case-method-clause ...)))
				(.method* parsed))))
	(#(?stuff ...)
	 (synner "invalid method specification" #'(case-method ?stuff ...)))))

    (define (%process-method-overload-spec {parsed <parsed-clauses>} spec synner)
      ;;The METHOD/OVERLOAD clause can be  present multiple times.  Each input clause
      ;;must have the format:
      ;;
      ;;   (method/overload (?who . ?args) . ?body)
      ;;
      ;;and we expect SPEC to have the format:
      ;;
      ;;   #((?who . ?args) . ?body)
      ;;
      (syntax-case spec ()
	(#((?who . ?args) ?body0 ?body ...)
	 (receive (method-name.id method-proc.id method-who.stx)
	     (syntax-case #'?who (brace)
	       (?method-name
		(identifier? #'?method-name)
		(let ((method-proc.id (identifier-record-field-accessor (.type-name parsed) #'?method-name)))
		  (values #'?method-name method-proc.id method-proc.id)))
	       ((brace ?method-name . ?rv-types)
		(identifier? #'?method-name)
		(let ((method-proc.id (identifier-record-field-accessor (.type-name parsed) #'?method-name)))
		  (values #'?method-name method-proc.id #`(brace #,method-proc.id . ?rv-types))))
	       (_
		(synner "invalid method name specification" #'?who)))
	   (.method* parsed (cons (vector method-name.id method-proc.id
					  #`(define/overload (#,method-who.stx . ?args) ?body0 ?body ...))
				  (.method* parsed)))))
	(#(?stuff ...)
	 (synner "invalid method specification" #'(method ?stuff ...)))))

    (lambda (input-form.stx)
      (case-define synner
	((message)
	 (syntax-violation __module_who__ message input-form.stx #f))
	((message subform)
	 (syntax-violation __module_who__ message input-form.stx subform)))

      (main input-form.stx synner))))


;;;; done

#| end of library |# )

;;; end of file
