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
(library (vicare language-extensions labels)
  (options typed-language)
  (export
    define-label
    parent
    type-predicate
    equality-predicate
    comparison-procedure
    hash-function
    method
    case-method)
  (import (vicare))


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
	(new <syntax-clause-spec> #'type-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'equality-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'comparison-procedure	0 1      1 1      '() '())
	(new <syntax-clause-spec> #'hash-function		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'case-method			0 +inf.0 2 +inf.0 '() '())
	#| end of LIST |# )))

    (define-record-type <parsed-clauses>
      (fields
	(mutable parent)
		;After  parsing:  a  syntax   object  representing  the  parent  type
		;annotation.
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
	  (lambda ()
	    (make-record #f #f #f #f #f '())))))

    (define (main stx synner)
      (syntax-case stx ()
	((_ ?type-name . ?clauses)
	 (let ((type-name.id	#'?type-name))
	   (unless (identifier? type-name.id)
	     (synner "expected identifier as label type name" type-name.id))
	   (let* (({parsed <parsed-clauses>} (syntax-clauses-fold-specs
					      (lambda (parsed spec args)
						(combine type-name.id parsed spec args synner)
						parsed)
					      (new <parsed-clauses>) CLAUSE-SPEC*
					      (syntax-clauses-unwrap #'?clauses synner) synner))
		  (parent.stx (.parent parsed)))
	     (with-syntax
		 (((TYPE-PREDICATE-ID TYPE-PREDICATE-DEF ...)
		   (cond ((.type-predicate parsed)
			  => (lambda (stx)
			       (with-syntax
				   ((FUNC (identifier-record-field-accessor type-name.id #'type-predicate)))
				 (list #'(syntax FUNC)
				       #`(define/typed {FUNC (type-predicate ?type-name)}
					   (#,stx (is-a? _ #,parent.stx)))))))
			 (else
			  (list #f))))

		  ((EQUALITY-PREDICATE-ID EQUALITY-PREDICATE-DEF ...)
		   (cond ((.equality-predicate parsed)
			  => (lambda (stx)
			       (with-syntax
				   ((FUNC (identifier-record-field-accessor type-name.id #'equality-predicate)))
				 (list #'(syntax FUNC)
				       #`(define/typed {FUNC (equality-predicate ?type-name)}
					   (#,stx (equality-predicate #,parent.stx)))))))
			 (else
			  (list #f))))

		  ((COMPARISON-PROCEDURE-ID COMPARISON-PROCEDURE-DEF ...)
		   (cond ((.comparison-procedure parsed)
			  => (lambda (stx)
			       (with-syntax
				   ((FUNC (identifier-record-field-accessor type-name.id #'comparison-procedure)))
				 (list #'(syntax FUNC)
				       #`(define/typed {FUNC (comparison-procedure ?type-name)}
					   (#,stx (comparison-procedure #,parent.stx)))))))
			 (else
			  (list #f))))

		  ((HASH-FUNCTION-ID HASH-FUNCTION-DEF ...)
		   (cond ((.hash-function parsed)
			  => (lambda (stx)
			       (with-syntax
				   ((FUNC (identifier-record-field-accessor type-name.id #'hash-function)))
				 (list #'(syntax FUNC)
				       #`(define/typed {FUNC (hash-function ?type-name)}
					   (#,stx (hash-function #,parent.stx)))))))
			 (else
			  (list #f))))

		  ((#(METHOD-NAME METHOD-PROC METHOD-DEFINITION) ...)
		   (.method* parsed)))
	       #`(module (?type-name)
		   (define-syntax ?type-name
		     (make-label-type-spec
		      (syntax ?type-name)
		      (syntax #,parent.stx)
		      TYPE-PREDICATE-ID
		      EQUALITY-PREDICATE-ID
		      COMPARISON-PROCEDURE-ID
		      HASH-FUNCTION-ID
		      ;;This methods table must be an alist.
		      (quasiquote ((METHOD-NAME . (unquote (syntax METHOD-PROC)))
				   ...))))
		   TYPE-PREDICATE-DEF		...
		   EQUALITY-PREDICATE-DEF	...
		   COMPARISON-PROCEDURE-DEF	...
		   HASH-FUNCTION-DEF		...
		   METHOD-DEFINITION		...)))))
	(_
	 (synner "invalid DEFINE-LABEL syntax use"))))

    (define (combine type-name.id {parsed <parsed-clauses>} {spec <syntax-clause-spec>} args synner)
      (case-identifiers (.keyword spec)
	((parent)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (parent ?parent-id)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?parent-id))
	 ;;
         (set! (.parent parsed) (vector-ref (vector-ref args 0) 0)))
	((type-predicate)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (predicate ?predicate-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?predicate-stx))
	 ;;
	 (set! (.type-predicate parsed) (vector-ref (vector-ref args 0) 0)))
	((equality-predicate)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (equality-predicate ?func-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?func-stx))
	 ;;
	 (set! (.equality-predicate parsed) (vector-ref (vector-ref args 0) 0)))
	((comparison-procedure)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (comparison-procedure ?func-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?func-stx))
	 ;;
	 (set! (.comparison-procedure parsed) (vector-ref (vector-ref args 0) 0)))
	((hash-function)
	 ;;The input clause must have the format:
	 ;;
	 ;;   (hash-function ?func-stx)
	 ;;
	 ;;and we expect ARGS to have the format:
	 ;;
	 ;;   #(#(?func-stx))
	 ;;
	 (set! (.hash-function parsed) (vector-ref (vector-ref args 0) 0)))
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
	 (vector-for-each
	     (lambda (method-def)
	       (syntax-case method-def ()
		 (#((?who . ?args) ?body0 ?body ...)
		  (receive (method-name.id method-proc.id method-who.stx)
		      (syntax-case #'?who (brace)
			(?method-name
			 (identifier? #'?method-name)
			 (let ((method-proc.id (identifier-record-field-accessor type-name.id #'?method-name)))
			   (values #'?method-name method-proc.id method-proc.id)))
			((brace ?method-name ?rv-type0 ?rv-type ...)
			 (identifier? #'?method-name)
			 (let ((method-proc.id (identifier-record-field-accessor type-name.id #'?method-name)))
			   (values #'?method-name method-proc.id #`(brace #,method-proc.id ?rv-type0 ?rv-type ...))))
			(_
			 (synner "invalid method name specification" #'?who)))
		    (set! (.method* parsed)
			  (cons (vector method-name.id method-proc.id
					#`(define/checked (#,method-who.stx . ?args) ?body0 ?body ...))
				(.method* parsed)))))
		 (_
		  (synner "invalid method specification" (cons #'method method-def)))))
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
	 (vector-for-each
	     (lambda (cmethod-def)
	       (syntax-case cmethod-def ()
		 (#(?who ?case-method-clause0 ?case-method-clause ...)
		  (set! (.method* parsed)
			(cons (let ((method-proc.id (identifier-record-field-accessor type-name.id #'?method-name)))
				(vector #'?who method-proc.id
					#`(case-define #,method-proc.id ?case-method-clause0 ?case-method-clause ...)))
			      (.method* parsed))))
		 (_
		  (synner "invalid method specification" (cons #'case-method cmethod-def)))))
	   args))
	(else
	 (assertion-violation __module_who__ "invalid clause spec" spec))))

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
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
