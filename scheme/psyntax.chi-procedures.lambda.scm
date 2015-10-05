;;;Copyright (c) 2010-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(module (chi-defun chi-lambda chi-case-lambda)


;;;; lambda clause attributes

(define (attributes.safe-formals? attributes.sexp)
  (or (memq 'safe         attributes.sexp)
      (memq 'safe-formals attributes.sexp)))

(define (attributes.safe-retvals? attributes.sexp)
  (or (memq 'safe         attributes.sexp)
      (memq 'safe-retvals attributes.sexp)))


;;;; chi procedures: function definitions and lambda syntaxes

(module (chi-defun)

  (define (chi-defun qrhs lexenv.run lexenv.expand)
    ;;Expand a qualified RHS representing an  INTERNAL-DEFINE syntax use for the case
    ;;of function  definition.  Return  an expanded language  expression representing
    ;;the expanded definition.
    ;;
    ;;The  returned expression  will  be  coupled (by  the  caller)  with an  already
    ;;generated  lex gensym  serving as  lexical variable  name; for  this reason  we
    ;;return a lambda core form rather than a define core form.
    ;;
    ;;NOTE This function assumes the INPUT-FORM.STX  has already been parsed, and the
    ;;binding for ?CTXT has already been added to LEXENV by the caller.
    ;;
    (define input-form.stx
      (qualified-rhs.stx qrhs))
    (syntax-match input-form.stx (brace)
      ((_ ?attributes ((brace ?ctxt ?rv-tag* ... . ?rest-rv-tag) . ?fmls) . ?body-form*)
       (let ((formals.stx (bless
			   `((brace _ ,@?rv-tag* . ,?rest-rv-tag) . ,?fmls))))
	 (%expand qrhs input-form.stx lexenv.run lexenv.expand
		  (syntax->datum ?attributes) ?ctxt formals.stx ?body-form*)))

      ((_ ?attributes (?ctxt . ?fmls) . ?body-form*)
       (%expand qrhs input-form.stx lexenv.run lexenv.expand
		(syntax->datum ?attributes) ?ctxt ?fmls ?body-form*))
      ))

  (define (%expand qrhs input-form.stx lexenv.run lexenv.expand
		   attributes.sexp who.id formals.stx body*.stx)
    ;;This procedure is like CHI-LAMBDA, but, in addition, it puts WHO.ID in the core
    ;;language LAMBDA sexp's annotation.
    (receive (formals.core lambda-signature body.psi)
	(%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			    attributes.sexp who.id formals.stx body*.stx)
      ;;FORMALS.CORE is composed of lex gensyms.
      (make-psi input-form.stx
		(build-lambda (syntax-annotation who.id)
		  formals.core
		  (psi.core-expr body.psi))
		(cond ((qualified-rhs.type-id qrhs)
		       => make-retvals-signature/single-value)
		      (else
		       (make-retvals-signature/single-procedure))))))

  #| end of module: CHI-DEFUN |# )


(define* (chi-lambda input-form.stx lexenv.run lexenv.expand
		     attributes.stx who.id formals.stx body*.stx)
  ;;Expand the contents of a LAMBDA syntax and return a "psi" struct.
  ;;
  ;;INPUT-FORM.STX is a syntax object representing the original LAMBDA expression.
  ;;
  ;;FORMALS.STX is a syntax object representing the formals of the LAMBDA syntax.
  ;;
  ;;BODY*.STX is  a list of syntax  objects representing the body  expressions in the
  ;;LAMBDA syntax.
  ;;
  (define attributes.sexp (syntax->datum attributes.stx))
  (receive (formals.lex lambda-signature body.psi)
      (%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			  attributes.sexp who.id formals.stx body*.stx)
    (let ((who.sym (and who.id (identifier->symbol who.id))))
      (make-psi input-form.stx
		(build-lambda (syntax-annotation input-form.stx)
		  formals.lex
		  (psi.core-expr body.psi))
		(make-retvals-signature/single-value
		 ;;If we fabricate a type identifier for this closure: it is possible
		 ;;to leak the type identifier out of the local lexical context where
		 ;;it is defined.  This is an error  we can fix in the typed language
		 ;;with a  cast operator; for the  untyped language we want  to avoid
		 ;;it.
		 (if (option.typed-language?)
		     (fabricate-closure-type-identifier who.sym lambda-signature)
		   (procedure-tag-id)))))))


(define* (chi-case-lambda input-form.stx lexenv.run lexenv.expand
			  attributes.stx who.id formals*.stx body**.stx)
  ;;Expand the clauses of a CASE-LAMBDA syntax and return a "psi" struct.
  ;;
  ;;INPUT-FORM.STX  is   a  syntax  object  representing   the  original  CASE-LAMBDA
  ;;expression.
  ;;
  ;;FORMALS*.STX is  a list  of syntax  objects whose  items are  the formals  of the
  ;;CASE-LAMBDA clauses.
  ;;
  ;;BODY**.STX  is a  list  of syntax  objects  whose  items are  the  bodies of  the
  ;;CASE-LAMBDA clauses.
  ;;
  ;;Example, for the input form:
  ;;
  ;;   (case-lambda ((a b c) body1) ((d e f) body2))
  ;;
  ;;this function is invoked as:
  ;;
  ;;   (chi-case-lambda
  ;;    #'(case-lambda ((a b c) body1) ((d e f) body2))
  ;;    (list #'(a b c) #'(d e f))
  ;;    (list #'(body1) #'(body2))
  ;;    lexenv.run lexenv.expand)
  ;;
  (define attributes.sexp (syntax->datum attributes.stx))
  (receive (formals*.lex lambda-signature* body**.psi)
      (%chi-lambda-clause* input-form.stx lexenv.run lexenv.expand
			   attributes.sexp who.id formals*.stx body**.stx)
    (let ((signature (make-clambda-compound lambda-signature*))
	  (who.sym   (and who.id (identifier->symbol who.id))))
      (make-psi input-form.stx
		(build-case-lambda (syntax-annotation input-form.stx)
		  formals*.lex
		  (map psi.core-expr body**.psi))
		(make-retvals-signature/single-value
		 ;;If we fabricate a type identifier for this closure: it is possible
		 ;;to leak the type identifier out of the local lexical context where
		 ;;it is defined.  This is an error  we can fix in the typed language
		 ;;with a  cast operator; for the  untyped language we want  to avoid
		 ;;it.
		 (if (option.typed-language?)
		     (fabricate-closure-type-identifier who.sym signature)
		   (procedure-tag-id)))))))


;;;; chi procedures: lambda clauses

(module (%chi-lambda-clause %chi-lambda-clause*)

  (define* (%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			       attributes.sexp who.id formals.stx body-form*.stx)
    ;;Expand  the components  of  a LAMBDA  syntax or  a  single CASE-LAMBDA  clause.
    ;;Return 3  values: a  proper or  improper list of  lex gensyms  representing the
    ;;formals; an instance  of "lambda-signature" representing the  tag signature for
    ;;this  LAMBDA   clause;  a  PSI   struct  containing  the   language  expression
    ;;representing the body of the clause.
    ;;
    ;;A LAMBDA or CASE-LAMBDA clause defines a lexical contour; so we build a new rib
    ;;for it, initialised with the id/label  associations of the formals; we push new
    ;;lexical bindings on LEXENV.RUN.
    ;;
    ;;NOTE The expander for the internal body will create yet another lexical contour
    ;;to hold the body's internal definitions.
    ;;
    ;;When the formals are tagged, we want to transform:
    ;;
    ;;   (lambda ({_ <symbol>} {a <fixnum>} {b <string>})
    ;;     ?body ... ?last-body)
    ;;
    ;;into:
    ;;
    ;;   (lambda (a b)
    ;;     (validate-typed-procedure-argument <fixnum> a)
    ;;     (validate-typed-procedure-argument <string> b)
    ;;     (internal-body
    ;;       ?body ...
    ;;       (assert-retvals-signature-and-return (<symbol>) ?last-body)))
    ;;
    (case-define %synner
      ((message)
       (%synner message #f))
      ((message subform)
       (syntax-violation __who__ message input-form.stx subform)))
    ;;STANDARD-FORMALS.STX is a syntax object representing the formal argument of the
    ;;LAMBDA   clause  as   required  by   R6RS.    SIGNATURE  is   an  instance   of
    ;;"<lambda-signature>"  representing the  types  of arguments  and return  values
    ;;specified in the extended Vicare syntax.
    (define-values (standard-formals.stx signature)
      (syntax-object.parse-lambda-clause-signature formals.stx input-form.stx))
    (define formals-signature.tags
      (lambda-signature.formals.tags signature))
    (cond
     ((list? standard-formals.stx)
      ;;Without  rest argument.   Here  we know  that  both STANDARD-FORMALS.STX  and
      ;;FORMALS-SIGNATURE.TAGS are proper lists with equal length.
      (receive (rib lexenv.run formals*.lex)
	  (%process-syntactic-bindings standard-formals.stx formals-signature.tags lexenv.run)
	(define validation*.stx
	  (if (attributes.safe-formals? attributes.sexp)
	      (%build-formals-validation-form* standard-formals.stx formals-signature.tags #f #f)
	    '()))
	(define has-arguments-validators?
	  (not (null? validation*.stx)))
	(define body-form^*.stx
	  (push-lexical-contour
	      rib
	    ;;Build a list of syntax objects representing the internal body.
	    (append validation*.stx
		    (if (attributes.safe-retvals? attributes.sexp)
			(%build-retvals-validation-form has-arguments-validators?
							(callable-signature.retvals signature)
							body-form*.stx)
		      body-form*.stx))))
	(define-values (lexenv.run^ lexenv.expand^)
	  (%push-who-fluid-syntax-on-lexenv __who__ input-form.stx lexenv.run lexenv.expand
					    who.id %synner))
	(let* ((body.psi  (chi-internal-body #f lexenv.run^ lexenv.expand^ body-form^*.stx))
	       (signature (%override-retvals-signature signature body.psi)))
	  (values formals*.lex signature body.psi))))

     (else
      ;;With  rest  argument.   Here  we  know  that  both  STANDARD-FORMALS.STX  and
      ;;FORMALS-SIGNATURE.TAGS are improper lists with equal length.
      (let*-values
	  (((arg*.id  rest.id)
	    (improper-list->list-and-rest standard-formals.stx))
	   ((arg*.tag rest.tag)
	    (improper-list->list-and-rest formals-signature.tags))
	   ((rib lexenv.run ghost*.lex)
	    (%process-syntactic-bindings (cons rest.id arg*.id) (cons rest.tag arg*.tag) lexenv.run)))
	(define formals.lex
	  ;;Yes, this call to APPEND builds an improper list.
	  (append (cdr ghost*.lex) (car ghost*.lex)))
	(define validation*.stx
	  (if (attributes.safe-formals? attributes.sexp)
	      (%build-formals-validation-form* arg*.id arg*.tag rest.id rest.tag)
	    '()))
	(define has-arguments-validators?
	  (not (null? validation*.stx)))
	(define body-form^*.stx
	  (push-lexical-contour
	      rib
	    ;;Build a list of syntax objects representing the internal body.
	    (append validation*.stx
		    (if (attributes.safe-retvals? attributes.sexp)
			(%build-retvals-validation-form has-arguments-validators?
							(callable-signature.retvals signature)
							body-form*.stx)
		      body-form*.stx))))
	(define-values (lexenv.run^ lexenv.expand^)
	  (%push-who-fluid-syntax-on-lexenv __who__ input-form.stx lexenv.run lexenv.expand
					    who.id %synner))
	(let* ((body.psi  (chi-internal-body #f lexenv.run^ lexenv.expand^ body-form^*.stx))
	       (signature (%override-retvals-signature signature body.psi)))
	  (values formals.lex signature body.psi))))))

;;; --------------------------------------------------------------------

  (define* (%chi-lambda-clause* input-form.stx lexenv.run lexenv.expand
				attributes.sexp who.id formals*.stx body-form**.stx)
    ;;Expand all the clauses of a CASE-LAMBDA syntax, return 2 values:
    ;;
    ;;1. A  list of subslist,  each sublist  being a proper  or improper list  of lex
    ;;gensyms representing the formals.
    ;;
    ;;2. A list of "<lambda-signature>" instances representing the signatures of each
    ;;clause.
    ;;
    ;;3.  A  list  of  PSI  structs   each  containing  a  core  language  expression
    ;;representing the body of a clause.
    ;;
    (if (null? formals*.stx)
	(values '() '() '())
      (receive (formals-lex signature body.psi)
	  (%chi-lambda-clause input-form.stx lexenv.run lexenv.expand
			      attributes.sexp who.id (car formals*.stx) (car body-form**.stx))
	(receive (formals-lex* signature* body*.psi)
	    (%chi-lambda-clause* input-form.stx lexenv.run lexenv.expand
				 attributes.sexp who.id (cdr formals*.stx) (cdr body-form**.stx))
	  (values (cons formals-lex  formals-lex*)
		  (cons signature    signature*)
		  (cons body.psi     body*.psi))))))

;;; --------------------------------------------------------------------

  (define (%build-formals-validation-form* arg* tag* rest-arg rest-tag)
    ;;Build  and  return a  list  of  syntax  objects representing  expressions  that
    ;;validate the  arguments, excluding  the formals  in which  the tag  is "<top>",
    ;;whose argument are always valid.  When  there is no rest argument: REST-ARG and
    ;;REST-TAG must be #f.
    ;;
    (let recur ((arg* arg*)
		(tag* tag*)
		(idx  0))
      (cond ((pair? arg*)
	     (if (top-tag-id? (car tag*))
		 ;;Insert no validation for an argument typed "<top>".
		 (recur (cdr arg*) (cdr tag*) (fxadd1 idx))
	       (cons (bless
		      `(validate-typed-procedure-argument ,(car tag*) ,idx ,(car arg*)))
		     (recur (cdr arg*) (cdr tag*) (fxadd1 idx)))))
	    ((or (not rest-tag)
		 (top-tag-id? rest-tag))
	     '())
	    (else
	     ;;Build a validation form for the objects in the rest argument.
	     (if (top-tag-id? rest-tag)
		 '()
	       (bless
		(let ((obj.sym (gensym))
		      (idx.sym (gensym)))
		  `((fold-left (lambda (,idx.sym ,obj.sym)
				 (validate-typed-procedure-argument ,rest-arg ,idx.sym ,obj.sym)
				 (fxadd1 ,idx.sym))
		      ,idx ,rest-arg)))))))))

  (define* (%build-retvals-validation-form has-arguments-validators? retvals-signature body-form*.stx)
    ;;Add the return values validation to the last form in the body; return a list of
    ;;body forms.
    ;;
    ;;When  there  are  arguments  validators:  the body  forms  are  wrapped  in  an
    ;;INTERNAL-BODY to  create an internal  lexical scope.   This is far  better than
    ;;wrapping into a LET, which would expand into a nested LAMBDA.
    ;;
    ;;The  argument HAS-ARGUMENTS-VALIDATORS?   is  required  to avoid  INTERNAL-BODY
    ;;wrapping when not needed; this gains a bit of speed when expanding the body.
    ;;
    (cond (has-arguments-validators?
	   (bless
	    (if (retvals-signature.fully-unspecified? retvals-signature)
		;;The number and type of return values is unknown.
		`((internal-body . ,body-form*.stx))
	      (receive (head*.stx last.stx)
		  (proper-list->head-and-last body-form*.stx)
		`((internal-body
		    ,@head*.stx
		    (assert-retvals-signature-and-return ,(retvals-signature.tags retvals-signature) ,last.stx)))))))
	  (else
	   (if (retvals-signature.fully-unspecified? retvals-signature)
	       ;;The number and type of return values is unknown.
	       body-form*.stx
	     (receive (head*.stx last.stx)
		 (proper-list->head-and-last body-form*.stx)
	       (append head*.stx
		       (bless
			`((assert-retvals-signature-and-return ,(retvals-signature.tags retvals-signature) ,last.stx)))))))))

;;; --------------------------------------------------------------------

  (define* (%push-who-fluid-syntax-on-lexenv who input-form.stx lexenv.run lexenv.expand
					     lhs.id %synner)
    ;;It is  very important to do  this only if  LHS.ID is true: the  internal syntax
    ;;INTERNAL-LAMBDA sets LHS.ID to false, so we  do not bind "__who__" in its body.
    ;;We would go into infinite  loop otherwise, because IDENTIFIER-SYNTAX expands to
    ;;an INTERNAL-LAMBDA use.
    ;;
    (import CORE-MACRO-TRANSFORMER) ;for PUSH-FLUID-SYNTAX
    (if lhs.id
	(push-fluid-syntax who input-form.stx lexenv.run lexenv.expand
			   (core-prim-id '__who__) (bless `(identifier-syntax (quote ,lhs.id)))
			   %synner)
      (values lexenv.run lexenv.expand)))

  (define (%override-retvals-signature signature body.psi)
    ;;SIGNATURE must  be an  instance of  "<lambda-signature>" representing  the type
    ;;signature of the  LAMBDA clause.  BODY.PSI must be the  result of expanding the
    ;;body of the LAMBDA clause.
    ;;
    ;;If the SIGNATURE does not specify any type for the return values of the clause:
    ;;build  and return  a new  "<lambda-signature>" instance  having the  retvals of
    ;;BODY.PSI.  Otherwise return SIGNATURE itself.
    ;;
    ;;FIXME Can we  do better here?  If SIGNATURE partially  specifies the types: can
    ;;we merge its  specification with the specification of  BODY.PSI?  (Marco Maggi;
    ;;Wed Sep 23, 2015)
    ;;
    (if (retvals-signature.fully-unspecified? (callable-signature.retvals signature))
	(make-lambda-signature (psi.retvals-signature body.psi)
			       (lambda-signature.formals signature))
      signature))

  #| end of module: CHI-LAMBDA-CLAUSES |# )


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'with-exception-handler/input-form		'scheme-indent-function 1)
;;eval: (put 'raise-compound-condition-object		'scheme-indent-function 1)
;;eval: (put 'assertion-violation/internal-error	'scheme-indent-function 1)
;;eval: (put 'with-who					'scheme-indent-function 1)
;;End:
