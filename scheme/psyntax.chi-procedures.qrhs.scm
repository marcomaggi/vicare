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


;;;; chi procedures: lexical bindings qualified right-hand sides
;;
;;A "qualified right-hand side expression" (QRHS)  is an object of type QUALIFIED-RHS
;;(more  precisely:  one of  its  subtypes);  these  objects  are generated  only  by
;;CHI-BODY*.
;;
;;For  example, when  CHI-BODY*  expands a  body that  allows  mixed definitions  and
;;expressions:
;;
;;   (define/standard (fun x) (list x 1))
;;   (define/standard name)
;;   (define/typed {red tag} (+ 1 2))
;;   (define/standard blue (+ 3 4))
;;   (display 5)
;;
;;all the forms are parsed and the following QUALIFIED-RHS objects are created:
;;
;;   #<qualified-rhs/standard-defun  #'fun    #'(define/standard ?attributes (fun x) (list x 1))>
;;   #<qualified-rhs/standard-defvar #'name   #'(void)>
;;   #<qualified-rhs/typed-defvar    #'red    #'(+ 1 2)>
;;   #<qualified-rhs/standard-defvar #'blue   #'(+ 3 4)>
;;   #<qualified-rhs/top-expr        #'dummy  #'(display 5)>
;;
;;the definitions are not immediately expanded.  This allows to expand the macros and
;;macro definitions  first, and  to expand the  variable definitions  and expressions
;;later.
;;
;;It is responsibility of the caller  to establish the appropriate variable syntactic
;;binding to represent the DEFINE syntax;  when CHI-QRHS and CHI-QRHS* are called the
;;binding has already been establish.
;;


;;This module is for  functions needed to create new QRHS objects  and to expand QRHS
;;objects already created.
;;
(module (qrhs-generate-loc
	 chi-qrhs			chi-qrhs*
	 chi-interaction-qrhs
	 qualified-rhs/standard-defun	make-qualified-rhs/standard-defun	qualified-rhs/standard-defun?
	 qualified-rhs/standard-defvar	make-qualified-rhs/standard-defvar	qualified-rhs/standard-defvar?
	 qualified-rhs/typed-defun	make-qualified-rhs/typed-defun		qualified-rhs/typed-defun?
	 qualified-rhs/typed-defvar	make-qualified-rhs/typed-defvar		qualified-rhs/typed-defvar?
	 qualified-rhs/top-expr		make-qualified-rhs/top-expr		qualified-rhs/top-expr?
	 qualified-rhs/standard-case-defun	make-qualified-rhs/standard-case-defun	qualified-rhs/standard-case-defun?
	 qualified-rhs/typed-case-defun		make-qualified-rhs/typed-case-defun	qualified-rhs/typed-case-defun?
	 ;;
	 qualified-rhs.input-form
	 qualified-rhs.lhs
	 qualified-rhs.type-id
	 qualified-rhs.lex
	 qualified-rhs/defun.standard-formals
	 qualified-rhs/defun.body*
	 qualified-rhs/defun.signature
	 qualified-rhs/defvar.rhs
	 qualified-rhs/case-defun.standard-formals*
	 qualified-rhs/case-defun.body**
	 qualified-rhs/case-defun.signature)


;;;; type definitions: qualified RHS base type

(define-record-type qualified-rhs
  (nongenerative vicare:expander:qualified-rhs)
  (fields
    (immutable input-form	qualified-rhs.input-form)
		;A syntax object representing the original input form.
    (immutable lhs		qualified-rhs.lhs)
		;The syntactic identifier bound by this expression.
    (mutable   type-id		qualified-rhs.type-id qualified-rhs.type-id-set!)
		;False  or  a  syntactic  identifier representing  the  type  of  the
		;syntactic binding defined by the QRHS.
		;
		;* When the QRHS is a  "defun": this field references the sub-type of
		;"<procedure>" representing the callable signature.
		;
		;*  When  the QRHS  is  a  "defvar":  this  field contains  the  type
		;identifier.
		;
		;*  When the  QRHS  is a  "top-expr": this  field  contains the  type
		;identifier "<top>".
    (immutable lex		qualified-rhs.lex)
		;The lexical gensym to use for this expression.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define (make-qualified-rhs input-form.stx lhs.id lhs.type)
	(make-record input-form.stx lhs.id lhs.type (generate-lexical-gensym lhs.id)))
      make-qualified-rhs)))

(define (qrhs-generate-loc qrhs)
  (generate-storage-location-gensym (qualified-rhs.lhs qrhs)))


;;;; type definitions: qualified RHS single-clause function definition

(define-record-type qualified-rhs/defun
  (nongenerative vicare:expander:qualified-rhs/defun)
  (parent qualified-rhs)
  (fields
    (immutable formals			qualified-rhs/defun.standard-formals)
		;A proper  or improper list  of syntax objects representing  a lambda
		;clause's standard formals.
    (immutable body*			qualified-rhs/defun.body*)
		;A list of syntax objects representing a lambda clause's body forms.
    (immutable signature		qualified-rhs/defun.signature)
		;An instance  of "<clambda-signature>" representing the  signature of
		;the function.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qualified-rhs)
      (define* (make-qualified-rhs/defun input-form.stx {lhs.id identifier?} standard-formals.stx {body*.stx list?}
					 {signature-type.id identifier?} {signature clambda-signature?})
	((make-qualified-rhs input-form.stx lhs.id signature-type.id) standard-formals.stx body*.stx signature))
      make-qualified-rhs/defun)))

;;; --------------------------------------------------------------------
;;; standard function definition

(define-record-type qualified-rhs/standard-defun
  ;;This type  is used to  represent standard  R6RS function definitions  from syntax
  ;;uses like:
  ;;
  ;;   (define/standard (fun arg) body)
  ;;
  (nongenerative vicare:expander:qualified-rhs/standard-defun)
  (parent qualified-rhs/defun)
  (protocol
    (lambda (make-qualified-rhs/defun)
      (define* (make-qualified-rhs/standard-defun input-form.stx {lhs.id identifier?} standard-formals.stx {body*.stx list?}
						  {signature-type.id identifier?} {signature clambda-signature?})
	((make-qualified-rhs/defun input-form.stx lhs.id standard-formals.stx body*.stx signature-type.id signature)))
      make-qualified-rhs/standard-defun)))

(define qualified-rhs/standard-defun-rtd
  (record-type-descriptor qualified-rhs/standard-defun))

;;; --------------------------------------------------------------------
;;; typed function definition

(define-record-type qualified-rhs/typed-defun
  ;;This type is used to represent typed function definitions from syntax uses like:
  ;;
  ;;   (define/typed ((brace fun <fixnum>) (brace arg <string>))
  ;;     body)
  ;;
  (nongenerative vicare:expander:qualified-rhs/typed-defun)
  (parent qualified-rhs/defun)
  (protocol
    (lambda (make-qualified-rhs/defun)
      (define* (make-qualified-rhs/typed-defun input-form.stx {lhs.id identifier?} standard-formals.stx {body*.stx list?}
					       {signature-type.id identifier?} {signature clambda-signature?})
	((make-qualified-rhs/defun input-form.stx lhs.id standard-formals.stx body*.stx signature-type.id signature)))
      make-qualified-rhs/typed-defun)))

(define qualified-rhs/typed-defun-rtd
  (record-type-descriptor qualified-rhs/typed-defun))


;;;; type definitions: qualified RHS multiple-clause function definition

(define-record-type qualified-rhs/case-defun
  (nongenerative vicare:expander:qualified-rhs/case-defun)
  (parent qualified-rhs)
  (fields
    (immutable formals*			qualified-rhs/case-defun.standard-formals*)
		;A  proper  or  improper  list   of  syntax  objects  representing  a
		;case-lambda clause's standard formals.
    (immutable body**			qualified-rhs/case-defun.body**)
		;A  list of  lists  of syntax  objects  representing lambda  clause's
		;bodies forms.
    (immutable signature		qualified-rhs/case-defun.signature)
		;An instance  of "<clambda-signature>" representing the  signature of
		;the function.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qualified-rhs)
      (define* (make-qualified-rhs/case-defun input-form.stx {lhs.id identifier?} standard-formals*.stx {body**.stx list?}
					      {signature-type.id identifier?} {signature clambda-signature?})
	((make-qualified-rhs input-form.stx lhs.id signature-type.id) standard-formals*.stx body**.stx signature))
      make-qualified-rhs/case-defun)))

;;; --------------------------------------------------------------------
;;; standard function definition, case variant

(define-record-type qualified-rhs/standard-case-defun
  ;;This type  is used to represent  non-typed function definitions from  syntax uses
  ;;like:
  ;;
  ;;   (case-define/standard ?lhs ?clause0 ?clause ...)
  ;;
  ;;which are meant to be equivalent to:
  ;;
  ;;   (define ?lhs (case-lambda/standard ?clause0 ?clause ...))
  ;;
  (nongenerative vicare:expander:qualified-rhs/standard-case-defun)
  (parent qualified-rhs/case-defun)
  (protocol
    (lambda (make-qualified-rhs/case-defun)
      (define* (make-qualified-rhs/standard-case-defun input-form.stx {lhs.id identifier?} standard-formals*.stx {body**.stx list?}
						       {signature-type.id identifier?} {signature clambda-signature?})
	((make-qualified-rhs/case-defun input-form.stx lhs.id standard-formals*.stx body**.stx signature-type.id signature)))
      make-qualified-rhs/standard-case-defun)))

(define qualified-rhs/standard-case-defun-rtd
  (record-type-descriptor qualified-rhs/standard-case-defun))

;;; --------------------------------------------------------------------
;;; typed function definition

(define-record-type qualified-rhs/typed-case-defun
  ;;This type is used to represent typed function definitions from syntax uses like:
  ;;
  ;;   (case-define/typed ?lhs ?clause0 ?clause ...)
  ;;
  ;;which are meant to be equivalent to:
  ;;
  ;;   (define ?lhs (case-lambda/typed ?clause0 ?clause ...))
  ;;
  (nongenerative vicare:expander:qualified-rhs/typed-case-defun)
  (parent qualified-rhs/case-defun)
  (protocol
    (lambda (make-qualified-rhs/case-defun)
      (define* (make-qualified-rhs/typed-case-defun input-form.stx {lhs.id identifier?} standard-formals*.stx {body**.stx list?}
						    {signature-type.id identifier?} {signature clambda-signature?})
	((make-qualified-rhs/case-defun input-form.stx lhs.id standard-formals*.stx body**.stx signature-type.id signature)))
      make-qualified-rhs/typed-case-defun)))

(define qualified-rhs/typed-case-defun-rtd
  (record-type-descriptor qualified-rhs/typed-case-defun))


;;;; type definitions: qualified RHS variable definition

(define-record-type qualified-rhs/defvar
  (nongenerative vicare:expander:qualified-rhs/defvar)
  (parent qualified-rhs)
  (fields
    (immutable rhs		qualified-rhs/defvar.rhs)
		;A syntax  object representing  the right-hand  side expression  of a
		;variable definition.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qualified-rhs)
      (define* (make-qualified-rhs/defvar input-form.stx {lhs.id identifier?} rhs.stx {lhs.type identifier?})
	((make-qualified-rhs input-form.stx lhs.id lhs.type) rhs.stx))
      make-qualified-rhs/defvar)))

;;; --------------------------------------------------------------------
;;; standard variable definition

(define-record-type qualified-rhs/standard-defvar
  ;;This type  is used to  represent standard  R6RS variable definitions  from syntax
  ;;uses like:
  ;;
  ;;   (define/standard var val)
  ;;
  (nongenerative vicare:expander:qualified-rhs/standard-defvar)
  (parent qualified-rhs/defvar)
  (protocol
    (lambda (make-qualified-rhs/defvar)
      (define* (make-qualified-rhs/standard-defvar input-form.stx {lhs.id identifier?} rhs.stx)
	((make-qualified-rhs/defvar input-form.stx lhs.id rhs.stx (top-tag-id))))
      make-qualified-rhs/standard-defvar)))

(define qualified-rhs/standard-defvar-rtd
  (record-type-descriptor qualified-rhs/standard-defvar))

;;; --------------------------------------------------------------------
;;; typed variable definition

(define-record-type qualified-rhs/typed-defvar
  ;;This type is used to represent typed variable definitions from syntax uses like:
  ;;
  ;;   (define/typed (brace var <fixnum>) val)
  ;;
  (nongenerative vicare:expander:qualified-rhs/typed-defvar)
  (parent qualified-rhs/defvar)
  (protocol
    (lambda (make-qualified-rhs/defvar)
      (define* (make-qualified-rhs/typed-defvar input-form.stx {lhs.id identifier?} rhs.stx {lhs.type identifier?})
	((make-qualified-rhs/defvar input-form.stx lhs.id rhs.stx lhs.type)))
      make-qualified-rhs/typed-defvar)))

(define qualified-rhs/typed-defvar-rtd
  (record-type-descriptor qualified-rhs/typed-defvar))


;;;; type definitions: qualified RHS top-level expression

(define-record-type qualified-rhs/top-expr
  ;;This type is used to represent  expressions mixed with definitions (when they are
  ;;allowed).  For  example, when in a  top-level program's body the  expander parses
  ;;the forms:
  ;;
  ;;   (define/standard var1 val1)
  ;;   (display 123)
  ;;   (define/standard var2 val2)
  ;;
  ;;the form "(display 123)" is represented by a QRHS of this type.  This QRHS gets a
  ;;dummy left-hand side syntactic identifier (it  exists but it cannot be referenced
  ;;by the code).
  ;;
  (nongenerative vicare:expander:qualified-rhs/top-expr)
  (parent qualified-rhs)
  (protocol
    (lambda (make-qualified-rhs)
      (define* (make-qualified-rhs/top-expr input-form.stx)
	((make-qualified-rhs input-form.stx (make-syntactic-identifier-for-temporary-variable 'dummy) (top-tag-id))))
      make-qualified-rhs/top-expr)))

(define qualified-rhs/top-expr-rtd
  (record-type-descriptor qualified-rhs/top-expr))


;;;; full QRHS expansion

(define (chi-qrhs* qrhs* lexenv.run lexenv.expand)
  ;;Fully expand the  qualified right-hand side expressions  in QRHS*, left-to-right,
  ;;in the context  of an internal body  or top-level program.  Return a  list of PSI
  ;;objects.
  ;;
  (if (pair? qrhs*)
      ;;ORDER MATTERS!!!  Make sure that first we do the car, then the rest.
      (let ((psi (chi-qrhs (car qrhs*) lexenv.run lexenv.expand)))
	(cons psi (chi-qrhs* (cdr qrhs*) lexenv.run lexenv.expand)))
    '()))

(module (chi-qrhs chi-interaction-qrhs)

  (define (chi-qrhs qrhs lexenv.run lexenv.expand)
    (%chi-qrhs #f qrhs lexenv.run lexenv.expand))

  (define (chi-interaction-qrhs qrhs lexenv.run lexenv.expand)
    (%chi-qrhs #t qrhs lexenv.run lexenv.expand))

  (define* (%chi-qrhs interaction? qrhs lexenv.run lexenv.expand)
    ;;Fully expand  a qualified right-hand side  expression and return a  PSI object.
    ;;The  generated core  language expression  represents the  standalone right-hand
    ;;side expression; the  code representing the handling of the  resulting value is
    ;;generated somewhere else.
    ;;
    (while-not-expanding-application-first-subform
     (let ((rtd (record-rtd qrhs)))
       (cond
	((eq? rtd qualified-rhs/standard-defun-rtd)		(chi-defun/standard	qrhs lexenv.run lexenv.expand))
	((eq? rtd qualified-rhs/typed-defun-rtd)		(chi-defun/typed	qrhs lexenv.run lexenv.expand))
	((eq? rtd qualified-rhs/standard-defvar-rtd)		(chi-defvar/standard	qrhs lexenv.run lexenv.expand))
	((eq? rtd qualified-rhs/typed-defvar-rtd)		(chi-defvar/typed	qrhs lexenv.run lexenv.expand))
	((eq? rtd qualified-rhs/top-expr-rtd)			(if interaction?
								    (chi-interaction-top-expr qrhs lexenv.run lexenv.expand)
								  (chi-top-expr qrhs lexenv.run lexenv.expand)))
	((eq? rtd qualified-rhs/standard-case-defun-rtd)	(chi-case-defun/standard qrhs lexenv.run lexenv.expand))
	((eq? rtd qualified-rhs/typed-case-defun-rtd)		(chi-case-defun/typed	 qrhs lexenv.run lexenv.expand))
	(else
	 (assertion-violation __who__ "invalid QRHS type" qrhs))))))

  #| end of module |# )


;;;; chi procedures: standard and typed variable definition

(define (chi-defvar/standard qrhs lexenv.run lexenv.expand)
  ;;Expand the  right-hand side expression  of a standard variable  definition; build
  ;;and return a  PSI object.  The generated core language  expression represents the
  ;;standalone right-hand side expression; the  code representing the handling of the
  ;;resulting value is generated somewhere else.
  ;;
  (let* ((rhs.stx (qualified-rhs/defvar.rhs qrhs))
	 (rhs.psi (chi-expr rhs.stx lexenv.run lexenv.expand))
	 (rhs.sig (psi.retvals-signature rhs.psi)))
    ;;All right, we  have expanded the RHS expression.  Now  let's do some validation
    ;;on the type of the expression.
    (syntax-match (type-signature-tags rhs.sig) ()
      ((?tag)
       ;;A single return value.  Good.
       rhs.psi)

      (?tag
       (list-tag-id? ?tag)
       ;;Fully  unspecified return  values: we  accept it  here and  delegate further
       ;;checks at run-time.
       rhs.psi)

      (_
       ;;Damn!!!   We have  determined  at expand-time  that  the expression  returns
       ;;multiple return values: syntax violation.
       (expand-time-retvals-signature-violation __who__
	 rhs.stx #f (make-type-signature/single-top) rhs.sig))
      )))

(define (chi-defvar/typed qrhs lexenv.run lexenv.expand)
  ;;Expand the right-hand  side expression of a typed variable  definition; build and
  ;;return  a PSI  object.  The  generated  core language  expression represents  the
  ;;standalone right-hand side expression; the  code representing the handling of the
  ;;resulting value is generated somewhere else.
  ;;
  (chi-expr (qualified-rhs/defvar.rhs qrhs) lexenv.run lexenv.expand))


;;;; chi procedures: top-level expression to be handled as dummy definition

(define (chi-top-expr qrhs lexenv.run lexenv.expand)
  ;;Expand the top  expression in the context  of a program body, build  and return a
  ;;PSI object.
  ;;
  ;;NOTE The generated core language expression discards the returned values and just
  ;;returns a single void object.  In the context of a top-level body, this is really
  ;;needed because we are handling the top expression as a dummy definition:
  ;;
  ;;   (define ?dummy ?rhs)
  ;;
  ;;so we must make sure that the RHS returns a single value.
  ;;
  (let* ((expr.stx  (qualified-rhs.input-form qrhs))
	 (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	 (expr.core (psi.core-expr expr.psi)))
    (make-psi expr.stx
	      (build-sequence no-source
		(list expr.core (build-void)))
	      (make-type-signature/single-void))))

(define (chi-interaction-top-expr qrhs lexenv.run lexenv.expand)
  ;;Expand the top  expression in an interaction environment, build  and return a PSI
  ;;object.
  ;;
  ;;NOTE We  really need the  expression to return its  return values, we  must *not*
  ;;wrap the expression into:
  ;;
  ;;  (begin ?expr (void))
  ;;
  ;;to have it  return a single value.  When evaluating  expressions at the top-level
  ;;of an interaction environment (for example  at the REPL): a top-expr is generated
  ;;and we need its return values.  (Marco Maggi; Tue Feb 9, 2016)
  ;;
  (chi-expr (qualified-rhs.input-form qrhs) lexenv.run lexenv.expand))


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'build-global-assignment			'scheme-indent-function 1)
;;eval: (put 'with-who					'scheme-indent-function 1)
;;eval: (put 'expand-time-retvals-signature-violation	'scheme-indent-function 1)
;;eval: (put 'sys::syntax-case				'scheme-indent-function 2)
;;End:
