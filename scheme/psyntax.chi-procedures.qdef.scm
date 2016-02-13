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


;;;; chi procedures: qualified definitions
;;
;;A "qualified  lexical variable  definition" (QDEF)  is an  object of  type "<qdef>"
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
;;all the forms are parsed and the following QDEF objects are created:
;;
;;   #<qdef-standard-defun  #'fun    #'(define/standard ?attributes (fun x) (list x 1))>
;;   #<qdef-standard-defvar #'name   #'(void)>
;;   #<qdef-typed-defvar    #'red    #'(+ 1 2)>
;;   #<qdef-standard-defvar #'blue   #'(+ 3 4)>
;;   #<qdef-top-expr        #'dummy  #'(display 5)>
;;
;;the definitions are not immediately expanded.  This allows to expand the macros and
;;macro definitions  first, and  to expand the  variable definitions  and expressions
;;later.
;;
;;It is responsibility of the caller  to establish the appropriate variable syntactic
;;binding to represent the DEFINE syntax;  when CHI-QDEF and CHI-QDEF* are called the
;;binding has already been establish.
;;


;;This module is for  functions needed to create new QDEF objects  and to expand QDEF
;;objects already created.
;;
(module (qdef-generate-loc
	 chi-qdef			chi-qdef*
	 chi-interaction-qdef
	 <qdef>				<qdef-defun>			<qdef-defvar>
	 <qdef-standard-defun>		make-qdef-standard-defun	qdef-standard-defun?
	 <qdef-standard-defvar>		make-qdef-standard-defvar	qdef-standard-defvar?
	 <qdef-typed-defun>		make-qdef-typed-defun		qdef-typed-defun?
	 <qdef-typed-defvar>		make-qdef-typed-defvar		qdef-typed-defvar?
	 <qdef-top-expr>		make-qdef-top-expr		qdef-top-expr?
	 <qdef-standard-case-defun>	make-qdef-standard-case-defun	qdef-standard-case-defun?
	 <qdef-typed-case-defun>	make-qdef-typed-case-defun	qdef-typed-case-defun?
	 ;;
	 qdef.input-form
	 qdef.lhs
	 qdef.type-id
	 qdef.lex
	 qdef-defun.standard-formals
	 qdef-defun.body*
	 qdef-defun.signature
	 qdef-defvar.rhs
	 qdef-case-defun.standard-formals*
	 qdef-case-defun.body**
	 qdef-case-defun.signature)


;;;; type definitions: qualified RHS base type

(define-record-type (<qdef> dummy-make-qdef qdef?)
  (nongenerative vicare:expander:qdef)
  (fields
    (immutable input-form	qdef.input-form)
		;A syntax object representing the original input form.
    (immutable lhs		qdef.lhs)
		;The syntactic identifier bound by this expression.
    (mutable   type-id		qdef.type-id qdef.type-id-set!)
		;False  or  a  syntactic  identifier representing  the  type  of  the
		;syntactic binding defined by the QDEF.
		;
		;* When the QDEF is a  "defun": this field references the sub-type of
		;"<procedure>" representing the callable signature.
		;
		;*  When  the QDEF  is  a  "defvar":  this  field contains  the  type
		;identifier.
		;
		;*  When the  QDEF  is a  "top-expr": this  field  contains the  type
		;identifier "<top>".
    (immutable lex		qdef.lex)
		;The lexical gensym to use for this expression.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define (make-qdef input-form.stx lhs.id lhs.type)
	(make-record input-form.stx lhs.id lhs.type (generate-lexical-gensym lhs.id)))
      make-qdef)))

(define (qdef-generate-loc qdef)
  (generate-storage-location-gensym (qdef.lhs qdef)))


;;;; type definitions: qualified RHS single-clause function definition

(define-record-type (<qdef-defun> make-qdef-defun qdef-defun?)
  (nongenerative vicare:expander:<qdef-defun>)
  (parent <qdef>)
  (fields
    (immutable formals			qdef-defun.standard-formals)
		;A proper  or improper list  of syntax objects representing  a lambda
		;clause's standard formals.
    (immutable body*			qdef-defun.body*)
		;A list of syntax objects representing a lambda clause's body forms.
    (immutable signature		qdef-defun.signature)
		;An instance  of "<clambda-signature>" representing the  signature of
		;the function.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-defun input-form.stx {lhs.id identifier?} standard-formals.stx {body*.stx list?}
					 {signature-type.id identifier?} {signature clambda-signature?})
	((make-qdef input-form.stx lhs.id signature-type.id) standard-formals.stx body*.stx signature))
      make-qdef-defun)))

;;; --------------------------------------------------------------------
;;; standard function definition

(define-record-type (<qdef-standard-defun> make-qdef-standard-defun qdef-standard-defun?)
  ;;This type  is used to  represent standard  R6RS function definitions  from syntax
  ;;uses like:
  ;;
  ;;   (define/standard (fun arg) body)
  ;;
  (nongenerative vicare:expander:<qdef-standard-defun>)
  (parent <qdef-defun>)
  (protocol
    (lambda (make-qdef-defun)
      (define* (make-qdef-standard-defun input-form.stx {lhs.id identifier?} standard-formals.stx {body*.stx list?}
						  {signature-type.id identifier?} {signature clambda-signature?})
	((make-qdef-defun input-form.stx lhs.id standard-formals.stx body*.stx signature-type.id signature)))
      make-qdef-standard-defun)))

;;; --------------------------------------------------------------------
;;; typed function definition

(define-record-type (<qdef-typed-defun> make-qdef-typed-defun qdef-typed-defun?)
  ;;This type is used to represent typed function definitions from syntax uses like:
  ;;
  ;;   (define/typed ((brace fun <fixnum>) (brace arg <string>))
  ;;     body)
  ;;
  (nongenerative vicare:expander:<qdef-typed-defun>)
  (parent <qdef-defun>)
  (protocol
    (lambda (make-qdef-defun)
      (define* (make-qdef-typed-defun input-form.stx {lhs.id identifier?} standard-formals.stx {body*.stx list?}
					       {signature-type.id identifier?} {signature clambda-signature?})
	((make-qdef-defun input-form.stx lhs.id standard-formals.stx body*.stx signature-type.id signature)))
      make-qdef-typed-defun)))


;;;; type definitions: qualified RHS multiple-clause function definition

(define-record-type (<qdef-case-defun> make-qdef-case-defun qdef-case-defun?)
  (nongenerative vicare:expander:<qdef-case-defun>)
  (parent <qdef>)
  (fields
    (immutable formals*			qdef-case-defun.standard-formals*)
		;A  proper  or  improper  list   of  syntax  objects  representing  a
		;case-lambda clause's standard formals.
    (immutable body**			qdef-case-defun.body**)
		;A  list of  lists  of syntax  objects  representing lambda  clause's
		;bodies forms.
    (immutable signature		qdef-case-defun.signature)
		;An instance  of "<clambda-signature>" representing the  signature of
		;the function.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-case-defun input-form.stx {lhs.id identifier?} standard-formals*.stx {body**.stx list?}
					      {signature-type.id identifier?} {signature clambda-signature?})
	((make-qdef input-form.stx lhs.id signature-type.id) standard-formals*.stx body**.stx signature))
      make-qdef-case-defun)))

;;; --------------------------------------------------------------------
;;; standard function definition, case variant

(define-record-type (<qdef-standard-case-defun> make-qdef-standard-case-defun qdef-standard-case-defun?)
  ;;This type  is used to represent  non-typed function definitions from  syntax uses
  ;;like:
  ;;
  ;;   (case-define/standard ?lhs ?clause0 ?clause ...)
  ;;
  ;;which are meant to be equivalent to:
  ;;
  ;;   (define ?lhs (case-lambda/standard ?clause0 ?clause ...))
  ;;
  (nongenerative vicare:expander:qdef-standard-case-defun)
  (parent <qdef-case-defun>)
  (protocol
    (lambda (make-qdef-case-defun)
      (define* (make-qdef-standard-case-defun input-form.stx {lhs.id identifier?} standard-formals*.stx {body**.stx list?}
						       {signature-type.id identifier?} {signature clambda-signature?})
	((make-qdef-case-defun input-form.stx lhs.id standard-formals*.stx body**.stx signature-type.id signature)))
      make-qdef-standard-case-defun)))

;;; --------------------------------------------------------------------
;;; typed function definition

(define-record-type (<qdef-typed-case-defun> make-qdef-typed-case-defun qdef-typed-case-defun?)
  ;;This type is used to represent typed function definitions from syntax uses like:
  ;;
  ;;   (case-define/typed ?lhs ?clause0 ?clause ...)
  ;;
  ;;which are meant to be equivalent to:
  ;;
  ;;   (define ?lhs (case-lambda/typed ?clause0 ?clause ...))
  ;;
  (nongenerative vicare:expander:qdef-typed-case-defun)
  (parent <qdef-case-defun>)
  (protocol
    (lambda (make-qdef-case-defun)
      (define* (make-qdef-typed-case-defun input-form.stx {lhs.id identifier?} standard-formals*.stx {body**.stx list?}
						    {signature-type.id identifier?} {signature clambda-signature?})
	((make-qdef-case-defun input-form.stx lhs.id standard-formals*.stx body**.stx signature-type.id signature)))
      make-qdef-typed-case-defun)))


;;;; type definitions: qualified RHS variable definition

(define-record-type (<qdef-defvar> make-qdef-defvar qdef-defvar?)
  (nongenerative vicare:expander:<qdef-defvar>)
  (parent <qdef>)
  (fields
    (immutable rhs		qdef-defvar.rhs)
		;A syntax  object representing  the right-hand  side expression  of a
		;variable definition.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-defvar input-form.stx {lhs.id identifier?} rhs.stx {lhs.type identifier?})
	((make-qdef input-form.stx lhs.id lhs.type) rhs.stx))
      make-qdef-defvar)))

;;; --------------------------------------------------------------------
;;; standard variable definition

(define-record-type (<qdef-standard-defvar> make-qdef-standard-defvar qdef-standard-defvar?)
  ;;This type  is used to  represent standard  R6RS variable definitions  from syntax
  ;;uses like:
  ;;
  ;;   (define/standard var val)
  ;;
  (nongenerative vicare:expander:<qdef-standard-defvar>)
  (parent <qdef-defvar>)
  (protocol
    (lambda (make-qdef-defvar)
      (define* (make-qdef-standard-defvar input-form.stx {lhs.id identifier?} rhs.stx)
	((make-qdef-defvar input-form.stx lhs.id rhs.stx (top-tag-id))))
      make-qdef-standard-defvar)))

;;; --------------------------------------------------------------------
;;; typed variable definition

(define-record-type (<qdef-typed-defvar> make-qdef-typed-defvar qdef-typed-defvar?)
  ;;This type is used to represent typed variable definitions from syntax uses like:
  ;;
  ;;   (define/typed (brace var <fixnum>) val)
  ;;
  (nongenerative vicare:expander:<qdef-typed-defvar>)
  (parent <qdef-defvar>)
  (protocol
    (lambda (make-qdef-defvar)
      (define* (make-qdef-typed-defvar input-form.stx {lhs.id identifier?} rhs.stx {lhs.type identifier?})
	((make-qdef-defvar input-form.stx lhs.id rhs.stx lhs.type)))
      make-qdef-typed-defvar)))


;;;; type definitions: qualified RHS top-level expression

(define-record-type (<qdef-top-expr> make-qdef-top-expr qdef-top-expr?)
  ;;This type is used to represent  expressions mixed with definitions (when they are
  ;;allowed).  For  example, when in a  top-level program's body the  expander parses
  ;;the forms:
  ;;
  ;;   (define/standard var1 val1)
  ;;   (display 123)
  ;;   (define/standard var2 val2)
  ;;
  ;;the form "(display 123)" is represented by a QDEF of this type.  This QDEF gets a
  ;;dummy left-hand side syntactic identifier (it  exists but it cannot be referenced
  ;;by the code).
  ;;
  (nongenerative vicare:expander:<qdef-top-expr>)
  (parent <qdef>)
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-top-expr input-form.stx)
	((make-qdef input-form.stx (make-syntactic-identifier-for-temporary-variable 'dummy) (top-tag-id))))
      make-qdef-top-expr)))


;;;; full QDEF expansion

(define (chi-qdef* qdef* lexenv.run lexenv.expand)
  ;;Fully expand the  qualified right-hand side expressions  in QDEF*, left-to-right,
  ;;in the context  of an internal body  or top-level program.  Return a  list of PSI
  ;;objects.
  ;;
  (if (pair? qdef*)
      ;;ORDER MATTERS!!!  Make sure that first we do the car, then the rest.
      (let ((psi (chi-qdef (car qdef*) lexenv.run lexenv.expand)))
	(cons psi (chi-qdef* (cdr qdef*) lexenv.run lexenv.expand)))
    '()))

(module (chi-qdef chi-interaction-qdef)

  (define (chi-qdef qdef lexenv.run lexenv.expand)
    (%chi-qdef #f qdef lexenv.run lexenv.expand))

  (define (chi-interaction-qdef qdef lexenv.run lexenv.expand)
    (%chi-qdef #t qdef lexenv.run lexenv.expand))

  (define* (%chi-qdef interaction? qdef lexenv.run lexenv.expand)
    ;;Fully expand  a qualified right-hand side  expression and return a  PSI object.
    ;;The  generated core  language expression  represents the  standalone right-hand
    ;;side expression; the  code representing the handling of the  resulting value is
    ;;generated somewhere else.
    ;;
    (while-not-expanding-application-first-subform
     (let ((rtd (record-rtd qdef)))
       (cond
	((eq? rtd (record-type-descriptor <qdef-standard-defun>))	(chi-defun/standard	qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-typed-defun>))		(chi-defun/typed	qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-standard-defvar>))	(chi-defvar/standard	qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-typed-defvar>))		(chi-defvar/typed	qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-top-expr>))		(if interaction?
									    (chi-interaction-top-expr qdef lexenv.run lexenv.expand)
									  (chi-top-expr qdef lexenv.run lexenv.expand)))
	((eq? rtd (record-type-descriptor <qdef-standard-case-defun>))	(chi-case-defun/standard qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-typed-case-defun>))	(chi-case-defun/typed	 qdef lexenv.run lexenv.expand))
	(else
	 (assertion-violation __who__ "invalid QDEF type" qdef))))))

  #| end of module |# )


;;;; chi procedures: standard and typed variable definition

(define (chi-defvar/standard qdef lexenv.run lexenv.expand)
  ;;Expand the  right-hand side expression  of a standard variable  definition; build
  ;;and return a  PSI object.  The generated core language  expression represents the
  ;;standalone right-hand side expression; the  code representing the handling of the
  ;;resulting value is generated somewhere else.
  ;;
  (let* ((rhs.stx (qdef-defvar.rhs qdef))
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

(define (chi-defvar/typed qdef lexenv.run lexenv.expand)
  ;;Expand the right-hand  side expression of a typed variable  definition; build and
  ;;return  a PSI  object.  The  generated  core language  expression represents  the
  ;;standalone right-hand side expression; the  code representing the handling of the
  ;;resulting value is generated somewhere else.
  ;;
  (chi-expr (qdef-defvar.rhs qdef) lexenv.run lexenv.expand))


;;;; chi procedures: top-level expression to be handled as dummy definition

(define (chi-top-expr qdef lexenv.run lexenv.expand)
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
  (let* ((expr.stx  (qdef.input-form qdef))
	 (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	 (expr.core (psi.core-expr expr.psi)))
    (make-psi expr.stx
	      (build-sequence no-source
		(list expr.core (build-void)))
	      (make-type-signature/single-void))))

(define (chi-interaction-top-expr qdef lexenv.run lexenv.expand)
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
  (chi-expr (qdef.input-form qdef) lexenv.run lexenv.expand))


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'build-sequence				'scheme-indent-function 1)
;;eval: (put 'build-global-assignment			'scheme-indent-function 1)
;;eval: (put 'with-who					'scheme-indent-function 1)
;;eval: (put 'expand-time-retvals-signature-violation	'scheme-indent-function 1)
;;eval: (put 'sys::syntax-case				'scheme-indent-function 2)
;;End:
