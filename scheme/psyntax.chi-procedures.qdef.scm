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
;;   (define/std (fun x) (list x 1))
;;   (define/std name)
;;   (define/typed {red tag} (+ 1 2))
;;   (define/checked {red tag} (+ 1 2))
;;   (define/std blue (+ 3 4))
;;   (display 5)
;;
;;all the forms are parsed and the following QDEF objects are created:
;;
;;   #<qdef-standard-defun  #'fun    #'(define/std ?attributes (fun x) (list x 1))>
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
	 chi-qdef			chi-qdef*			chi-interaction-qdef
	 <qdef>				<qdef-closure>
	 <qdef-defun>			<qdef-defvar>
	 <qdef-standard-defun>		make-qdef-standard-defun	qdef-standard-defun?
	 <qdef-standard-defvar>		make-qdef-standard-defvar	qdef-standard-defvar?
	 <qdef-typed-defun>		make-qdef-typed-defun		qdef-typed-defun?
	 <qdef-typed-defvar>		make-qdef-typed-defvar		qdef-typed-defvar?
	 <qdef-checked-defun>		make-qdef-checked-defun		qdef-checked-defun?
	 <qdef-checked-defvar>		make-qdef-checked-defvar	qdef-checked-defvar?
	 <qdef-standard-case-defun>	make-qdef-standard-case-defun	qdef-standard-case-defun?
	 <qdef-typed-case-defun>	make-qdef-typed-case-defun	qdef-typed-case-defun?
	 <qdef-checked-case-defun>	make-qdef-checked-case-defun	qdef-checked-case-defun?
	 <qdef-top-expr>		make-qdef-top-expr		qdef-top-expr?
	 ;;
	 qdef.input-form
	 qdef.var-id				qdef.lex
	 qdef-defvar.rhs
	 qdef-closure.ots			qdef-closure.clause-signature*
	 qdef-defun.standard-formals		qdef-defun.body*
	 qdef-case-defun.standard-formals*	qdef-case-defun.body**)


;;;; type definitions: qualified RHS base type

(define-record-type (<qdef> dummy-make-qdef qdef?)
  (fields
    (immutable input-form	qdef.input-form)
		;A syntax object representing the original input form.
    (immutable var-id		qdef.var-id)
		;The syntactic identifier bound by this definition form.
    (immutable lex		qdef.lex)
		;The lexical gensym to use for this expression.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-qdef input-form.stx {lhs.var-id identifier?})
	(make-record input-form.stx lhs.var-id (generate-lexical-gensym lhs.var-id)))
      make-qdef)))

(define (qdef-generate-loc qdef)
  (generate-storage-location-gensym (qdef.var-id qdef)))

(define* (qdef.var-sym {qdef qdef-defvar?})
  (syntax->datum (qdef.var-id qdef)))


;;;; type definitions: qualified RHS general closure definition

(define-record-type (<qdef-closure> dummy-make-qdef-closure qdef-closure?)
  (parent <qdef>)
  (fields
    (immutable ots		qdef-closure.ots)
		;An instance  of "<closure-type-spec>"  representing the type  of the
		;syntactic binding defined by the QDEF.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-closure input-form.stx {lhs.var-id identifier?} {lhs.ots closure-type-spec?})
	((make-qdef input-form.stx lhs.var-id) lhs.ots))
      make-qdef-closure))
  #| end of DEFINE-RECORD-TYPE |# )

(define* (qdef-closure.clause-signature* {qdef qdef-closure?})
  (case-lambda-signature.clause-signature* (closure-type-spec.signature (qdef-closure.ots qdef))))


;;;; type definitions: qualified RHS single-clause function definition

(define-record-type (<qdef-defun> make-qdef-defun qdef-defun?)
  (parent <qdef-closure>)
  (fields
    (immutable formals			qdef-defun.standard-formals)
		;A proper  or improper list  of syntax objects representing  a lambda
		;clause's standard formals.
    (immutable body*			qdef-defun.body*)
		;A list of syntax objects representing a lambda clause's body forms.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef-closure)
      (define* (make-qdef-defun input-form.stx {lhs.var-id identifier?} standard-formals.stx {body*.stx list?}
				{lhs.ots closure-type-spec?})
	((make-qdef-closure input-form.stx lhs.var-id lhs.ots) standard-formals.stx body*.stx))
      make-qdef-defun)))

;;; --------------------------------------------------------------------
;;; standard function definition

(define-record-type (<qdef-standard-defun> make-qdef-standard-defun qdef-standard-defun?)
  ;;This type  is used to  represent standard  R6RS function definitions  from syntax
  ;;uses like:
  ;;
  ;;   (define/std (fun arg) body)
  ;;
  (parent <qdef-defun>)
  (protocol
    (lambda (make-qdef-defun)
      (define* (make-qdef-standard-defun input-form.stx {lhs.var-id identifier?} standard-formals.stx {body*.stx list?}
					 {lhs.ots closure-type-spec?})
	((make-qdef-defun input-form.stx lhs.var-id standard-formals.stx body*.stx lhs.ots)))
      make-qdef-standard-defun)))

;;; --------------------------------------------------------------------
;;; typed function definition

(define-record-type (<qdef-typed-defun> make-qdef-typed-defun qdef-typed-defun?)
  ;;This type is used to represent typed function definitions from syntax uses like:
  ;;
  ;;   (define/typed ((brace fun <fixnum>) (brace arg <string>))
  ;;     body)
  ;;
  (parent <qdef-defun>)
  (protocol
    (lambda (make-qdef-defun)
      (define* (make-qdef-typed-defun input-form.stx {lhs.var-id identifier?} standard-formals.stx {body*.stx list?}
				      {lhs.ots object-type-spec?})
	((make-qdef-defun input-form.stx lhs.var-id standard-formals.stx body*.stx lhs.ots)))
      make-qdef-typed-defun)))

(define-record-type (<qdef-checked-defun> make-qdef-checked-defun qdef-checked-defun?)
  ;;This type  is used  to represent  checked function  definitions from  syntax uses
  ;;like:
  ;;
  ;;   (define/checked ((brace fun <fixnum>) (brace arg <string>))
  ;;     body)
  ;;
  (parent <qdef-defun>)
  (protocol
    (lambda (make-qdef-defun)
      (define* (make-qdef-checked-defun input-form.stx {lhs.var-id identifier?} standard-formals.stx {body*.stx list?}
					{lhs.ots object-type-spec?})
	((make-qdef-defun input-form.stx lhs.var-id standard-formals.stx body*.stx lhs.ots)))
      make-qdef-checked-defun)))


;;;; type definitions: qualified RHS multiple-clause function definition

(define-record-type (<qdef-case-defun> make-qdef-case-defun qdef-case-defun?)
  (parent <qdef-closure>)
  (fields
    (immutable formals*		qdef-case-defun.standard-formals*)
		;A  proper  or  improper  list   of  syntax  objects  representing  a
		;case-lambda clause's standard formals.
    (immutable body**		qdef-case-defun.body**)
		;A  list of  lists  of syntax  objects  representing lambda  clause's
		;bodies forms.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef-closure)
      (define* (make-qdef-case-defun input-form.stx {lhs.var-id identifier?} standard-formals*.stx {body**.stx list?}
				     {lhs.ots closure-type-spec?})
	((make-qdef-closure input-form.stx lhs.var-id lhs.ots) standard-formals*.stx body**.stx))
      make-qdef-case-defun)))

;;; --------------------------------------------------------------------
;;; standard function definition, case variant

(define-record-type (<qdef-standard-case-defun> make-qdef-standard-case-defun qdef-standard-case-defun?)
  ;;This type  is used to represent  non-typed function definitions from  syntax uses
  ;;like:
  ;;
  ;;   (case-define/std ?lhs ?clause0 ?clause ...)
  ;;
  ;;which are meant to be equivalent to:
  ;;
  ;;   (define ?lhs (case-lambda/std ?clause0 ?clause ...))
  ;;
  (parent <qdef-case-defun>)
  (protocol
    (lambda (make-qdef-case-defun)
      (define* (make-qdef-standard-case-defun input-form.stx {lhs.var-id identifier?} standard-formals*.stx {body**.stx list?}
					      {lhs.ots closure-type-spec?})
	((make-qdef-case-defun input-form.stx lhs.var-id standard-formals*.stx body**.stx lhs.ots)))
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
  (parent <qdef-case-defun>)
  (protocol
    (lambda (make-qdef-case-defun)
      (define* (make-qdef-typed-case-defun input-form.stx {lhs.var-id identifier?} standard-formals*.stx {body**.stx list?}
					   {lhs.ots closure-type-spec?})
	((make-qdef-case-defun input-form.stx lhs.var-id standard-formals*.stx body**.stx lhs.ots)))
      make-qdef-typed-case-defun)))

(define-record-type (<qdef-checked-case-defun> make-qdef-checked-case-defun qdef-checked-case-defun?)
  ;;This type  is used  to represent  checked function  definitions from  syntax uses
  ;;like:
  ;;
  ;;   (case-define/checked ?lhs ?clause0 ?clause ...)
  ;;
  ;;which are meant to be equivalent to:
  ;;
  ;;   (define ?lhs (case-lambda/checked ?clause0 ?clause ...))
  ;;
  (parent <qdef-case-defun>)
  (protocol
    (lambda (make-qdef-case-defun)
      (define* (make-qdef-checked-case-defun input-form.stx {lhs.var-id identifier?} standard-formals*.stx {body**.stx list?}
					     {lhs.ots closure-type-spec?})
	((make-qdef-case-defun input-form.stx lhs.var-id standard-formals*.stx body**.stx lhs.ots)))
      make-qdef-checked-case-defun)))


;;;; type definitions: qualified RHS variable definition

(define-record-type (<qdef-defvar> make-qdef-defvar qdef-defvar?)
  (parent <qdef>)
  (fields
    (mutable lhs.ots		qdef-defvar.lhs-ots qdef-defvar.lhs-ots-set!)
		;An  instance of  "<object-type-spec>" representing  the type  of the
		;variable.  It is "<untyped>" if no type annotation is present.  This
		;field is used to perform expand-time type validation.  This field is
		;mutated when RHS type propagation is performed.
    (immutable init-expr?	qdef-typed-defvar.init-expr?)
		;Boolean, true if this definition has an initialisation expression in
		;the RHS field; false otherwise.
    (immutable rhs		qdef-defvar.rhs)
		;A syntax  object representing  the right-hand  side expression  of a
		;variable definition.  When no  initialisation expression was present
		;in the variable definition: this field defaults to "(void)".
    #| end of FIELDS |# )
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-defvar input-form.stx
				 {lhs.var-id identifier?} {lhs.ots object-type-spec?}
				 init-expr? rhs.stx)
	(let* ((init-expr?	(and init-expr? #t))
	       (rhs.stx		(if init-expr? rhs.stx `(,(void-id)))))
	  ((make-qdef input-form.stx lhs.var-id) lhs.ots init-expr? rhs.stx)))
      make-qdef-defvar)))

(define* (qdef-defvar.untyped? {qdef qdef-defvar?})
  ;;Return true if QDEF has an untyped LHS; otherwise return false.
  ;;
  (<untyped>-ots? (qdef-defvar.lhs-ots qdef)))

(define (qdef-defvar.type-propagation qdef lhs.new-ots lexenv.run)
  ;;Mutate the syntactic binding's descriptor of  LHS.ID to represent a typed lexical
  ;;variable having type LHS.NEW-OTS.  Return unspecified values.
  ;;
  ;;NOTE This is very dirty...  but I  thrive in dirt.  Fuck Yeah!  (Marco Maggi; Mon
  ;;May 2, 2016)
  ;;
  (let* ((lhs.id	(qdef.var-id qdef))
	 (lhs.lab	(id->label lhs.id))
	 (lhs.descr	(label->syntactic-binding-descriptor lhs.lab lexenv.run)))
    (let ((descr (make-syntactic-binding-descriptor/lexical-typed-var/from-data lhs.new-ots (qdef.lex qdef))))
      (syntactic-binding-descriptor.type-set!  lhs.descr (syntactic-binding-descriptor.type  descr))
      (syntactic-binding-descriptor.value-set! lhs.descr (syntactic-binding-descriptor.value descr)))))

;;; --------------------------------------------------------------------
;;; standard variable definition

(define-record-type (<qdef-standard-defvar> make-qdef-standard-defvar qdef-standard-defvar?)
  ;;This type  is used to  represent standard  R6RS variable definitions  from syntax
  ;;uses like:
  ;;
  ;;   (define/std ?var ?expr)
  ;;   (define/std ?var)
  ;;
  (parent <qdef-defvar>)
  (protocol
    (lambda (make-qdef-defvar)
      (define* (make-qdef-standard-defvar input-form.stx {lhs.var-id identifier?} init-expr? rhs.stx)
	((make-qdef-defvar input-form.stx lhs.var-id (<top>-ots) init-expr? rhs.stx)))
      make-qdef-standard-defvar)))

;;; --------------------------------------------------------------------
;;; typed variable definition

(define-record-type (<qdef-typed-defvar> make-qdef-typed-defvar qdef-typed-defvar?)
  ;;This type is used to represent typed variable definitions from syntax uses like:
  ;;
  ;;   (define/typed   (brace ?var ?type) ?expr)
  ;;   (define/typed   (brace ?var ?type))
  ;;
  (parent <qdef-defvar>)
  (protocol
    (lambda (make-qdef-defvar)
      (define* (make-qdef-typed-defvar input-form.stx
				       {lhs.var-id identifier?} {lhs.ots object-type-spec?}
				       init-expr? rhs.stx)
	((make-qdef-defvar input-form.stx lhs.var-id lhs.ots init-expr? rhs.stx)))
      make-qdef-typed-defvar)))

(define-record-type (<qdef-checked-defvar> make-qdef-checked-defvar qdef-checked-defvar?)
  ;;This type  is used  to represent  checked variable  definitions from  syntax uses
  ;;like:
  ;;
  ;;   (define/checked (brace ?var ?type) ?expr)
  ;;   (define/checked (brace ?var ?type))
  ;;
  (parent <qdef-defvar>)
  (protocol
    (lambda (make-qdef-defvar)
      (define* (make-qdef-checked-defvar input-form.stx
					 {lhs.var-id identifier?} {lhs.ots object-type-spec?}
					 init-expr? rhs.stx)
	((make-qdef-defvar input-form.stx lhs.var-id lhs.ots init-expr? rhs.stx)))
      make-qdef-checked-defvar)))


;;;; type definitions: qualified RHS top-level expression

(define-record-type (<qdef-top-expr> make-qdef-top-expr qdef-top-expr?)
  ;;This type is used to represent  expressions mixed with definitions (when they are
  ;;allowed).  For  example, when in a  top-level program's body the  expander parses
  ;;the forms:
  ;;
  ;;   (define/std var1 val1)
  ;;   (display 123)
  ;;   (define/std var2 val2)
  ;;
  ;;the form "(display 123)" is represented by a QDEF of this type.  This QDEF gets a
  ;;dummy left-hand side syntactic identifier (it  exists but it cannot be referenced
  ;;by the code).
  ;;
  (parent <qdef>)
  (protocol
    (lambda (make-qdef)
      (define* (make-qdef-top-expr input-form.stx)
	((make-qdef input-form.stx (make-syntactic-identifier-for-temporary-variable))))
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
	((eq? rtd (record-type-descriptor <qdef-standard-defun>))	(chi-defun/std	      qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-typed-defun>))		(chi-defun/typed      qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-checked-defun>))	(chi-defun/checked    qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-standard-defvar>))	(chi-defvar/std	      qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-typed-defvar>))		(chi-defvar/typed     qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-checked-defvar>))	(chi-defvar/checked   qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-top-expr>))		(if interaction?
									    (chi-interaction-top-expr qdef lexenv.run lexenv.expand)
									  (chi-top-expr qdef lexenv.run lexenv.expand)))
	((eq? rtd (record-type-descriptor <qdef-standard-case-defun>))	(chi-case-defun/std     qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-typed-case-defun>))	(chi-case-defun/typed   qdef lexenv.run lexenv.expand))
	((eq? rtd (record-type-descriptor <qdef-checked-case-defun>))	(chi-case-defun/checked qdef lexenv.run lexenv.expand))
	(else
	 (assertion-violation __who__ "invalid QDEF type" qdef))))))

  #| end of module |# )


;;;; chi procedures: standard and typed variable definition

(module (chi-defvar/std chi-defvar/typed chi-defvar/checked)

  (define* (chi-defvar/std qdef lexenv.run lexenv.expand)
    ;;Expand the right-hand side expression  of a standard variable definition; build
    ;;and return a PSI object.  The generated core language expression represents the
    ;;standalone right-hand  side expression; the  code representing the  handling of
    ;;the resulting value is generated somewhere else.
    ;;
    (let* ((rhs.stx (qdef-defvar.rhs qdef))
	   (rhs.psi (chi-expr rhs.stx lexenv.run lexenv.expand))
	   (rhs.sig (psi.retvals-signature rhs.psi)))
      (define (common message)
	(condition
	  (make-who-condition 'chi-defvar/std)
	  (make-message-condition message)
	  (make-syntax-violation (qdef.input-form qdef) rhs.stx)
	  (make-type-signature-condition rhs.sig)))
      (case-signature-specs rhs.sig
	(<no-return>
	 ;;The expression is marked as not-returning.
	 (when (options::warn-about-not-returning-expressions)
	   (raise-continuable
	    (common "expression used as right-hand side in typed variable definition is typed as not returning")))
	 rhs.psi)

	((<void>)
	 ;;The expression is marked as returning void.
	 #;(assert (<top>-ots? (qdef-defvar.lhs-ots qdef)))
	 (if (qdef-typed-defvar.init-expr? qdef)
	     ;;This definition has an  initialisation expression returning void: this
	     ;;is wrong.
	     (%handle-error rhs.psi common "expression used as right-hand side in standard variable definition is typed as returning void")
	   ;;This definition has no initialisation  expression, so it is obvious that
	   ;;the RHS returns void.
	   rhs.psi))

	((single-value)
	 ;;A single return value.  Good.
	 ;;
	 ;;NOTE We could be tempted to perform RHS type propagation for this variable
	 ;;definition, even though  it is standard and so without  type.  But we have
	 ;;to remember that a  typed variable must obey rules if  it is later mutated
	 ;;with SET!; we  do not want to  enforce such rules on  a standard variable.
	 ;;(Marco Maggi; Wed May 11, 2016)
	 #;(assert (<top>-ots? (qdef-defvar.lhs-ots qdef)))
	 rhs.psi)

	((unspecified-values)
	 ;;Fully unspecified  return values: we  accept it here and  delegate further
	 ;;checks at run-time.
	 rhs.psi)

	(else
	 ;;Damn!!!  We  have determined  at expand-time  that the  expression returns
	 ;;multiple return values: syntax violation.
	 (%handle-error rhs.psi common
			"expression used as right-hand side in typed variable definition is typed as returning zero, two or more values")))))

;;; --------------------------------------------------------------------

  (module (chi-defvar/typed)

    (define* (chi-defvar/typed qdef lexenv.run lexenv.expand)
      ;;Expand the right-hand  side expression of a typed  variable definition; build
      ;;and return a  PSI object.  The generated core  language expression represents
      ;;the standalone right-hand side expression; the code representing the handling
      ;;of the resulting value is generated somewhere else.
      ;;
      (let* ((rhs.stx (qdef-defvar.rhs qdef))
	     (rhs.psi (chi-expr rhs.stx lexenv.run lexenv.expand))
	     (rhs.sig (psi.retvals-signature rhs.psi))
	     (lhs.ots (qdef-defvar.lhs-ots qdef)))
	(define (common message)
	  (condition
	    (make-who-condition 'chi-defvar/typed)
	    (make-message-condition message)
	    (make-syntax-violation (qdef.input-form qdef) rhs.stx)
	    (make-type-signature-condition rhs.sig)))
	(case-signature-specs (psi.retvals-signature rhs.psi)
	  (<no-return>
	   ;;The expression is marked as not-returning.
	   (when (options::warn-about-not-returning-expressions)
	     (raise-continuable
	      (common "expression used as right-hand side in typed variable definition is typed as not returning")))
	   (when (qdef-defvar.untyped? qdef)
	     (qdef-defvar.type-propagation qdef (<top>-ots) lexenv.run))
	   rhs.psi)

	  ((<void>)
	   ;;The expression is marked as returning void.
	   (if (qdef-typed-defvar.init-expr? qdef)
	       ;;This  definition has  an initialisation  expression returning  void:
	       ;;this is wrong.
	       (begin
		 (when (<untyped>-ots? lhs.ots)
		   (qdef-defvar.type-propagation qdef (<top>-ots) lexenv.run))
		 (%handle-error rhs.psi common "expression used as right-hand side in typed variable definition is typed as returning void"))
	     ;;This definition  has no  initialisation expression,  so it  is obvious
	     ;;that the RHS returns void.  We  propagate the type to "<void>" so that
	     ;;it is an error to use this variable without setting to some value.
	     (begin
	       (when (<untyped>-ots? lhs.ots)
		 (qdef-defvar.type-propagation qdef (<void>-ots) lexenv.run))
	       rhs.psi)))

	  ((single-value)
	   ;;The RHS expression returns a single value.  Good.
	   => (lambda (rhs.ots)
		(%process-single-value-case qdef lexenv.run lexenv.expand lhs.ots rhs.ots rhs.psi common)))

	  (<list-of-spec>
	   ;;The RHS  expression returns  an unspecified number  of values,  of known
	   ;;type.  RHS.OTS holds a "<list-of-type-spec>" OTS.
	   => (lambda (rhs.ots)
		;;We delegate  to the run-time code  the validation of the  number of
		;;returned values.
		(%process-single-value-case qdef lexenv.run lexenv.expand lhs.ots (list-of-type-spec.item-ots rhs.ots) rhs.psi common)))

	  (<list>
	   ;;The  RHS  expression  returns  an   unspecified  number  of  values,  of
	   ;;unspecified type; RHS.OTS  holds a "<list>" OTS.  This  is a non-checked
	   ;;typed variable,  so we do  not introduce  code to perform  run-time type
	   ;;validation.  Let's just accept it and  delegate to the run-time code the
	   ;;validation of the number of returned values.
	   (when (<untyped>-ots? lhs.ots)
	     ;;No type annotation  was present in the DEFINE/TYPED syntax  use, so we
	     ;;cast the value to "<top>".
	     (qdef-defvar.type-propagation qdef (<top>-ots) lexenv.run))
	   rhs.psi)

	  (else
	   ;;The expression returns zero, two or more values.
	   (%handle-error rhs.psi common
			  "expression used as right-hand side in typed variable definition is typed as returning zero, two or more values")))))

    (define (%process-single-value-case qdef lexenv.run lexenv.expand lhs.ots rhs.ots rhs.psi common)
      (if (<untyped>-ots? lhs.ots)
	  ;;No type  annotation was  present in  the DEFINE/TYPED  syntax use,  so we
	  ;;perform RHS type propagation.
	  (begin
	    (qdef-defvar.type-propagation qdef rhs.ots lexenv.run)
	    rhs.psi)
	;;A  type annotation  was  present  in the  DEFINE/TYPED  syntax  use, so  we
	;;validate RHS.OTS against it.
	(cond ((object-type-spec.matching-super-and-sub? lhs.ots rhs.ots)
	       ;;Types do match.  Good.
	       rhs.psi)
	      ((object-type-spec.compatible-super-and-sub? lhs.ots rhs.ots)
	       ;;Types  do not  match,  but they  are compatible.   Since  this is  a
	       ;;non-checked syntax: we  do not insert code to  perform run-time type
	       ;;validation.
	       rhs.psi)
	      (else
	       ;;Types do not match and are not compatible.  Bad.
	       (%error-mismatching-type 'chi-defvar/typed qdef lhs.ots rhs.psi)))))

    #| end of module: CHI-DEFVAR/TYPED |# )

;;; --------------------------------------------------------------------

  (module (chi-defvar/checked)

    (define (chi-defvar/checked qdef lexenv.run lexenv.expand)
      ;;Expand the right-hand side expression of a checked variable definition; build
      ;;and return a  PSI object.  The generated core  language expression represents
      ;;the standalone right-hand side expression; the code representing the handling
      ;;of the resulting value is generated somewhere else.
      ;;
      (let* ((rhs.stx (qdef-defvar.rhs qdef))
	     (rhs.psi (chi-expr rhs.stx lexenv.run lexenv.expand))
	     (rhs.sig (psi.retvals-signature rhs.psi))
	     (lhs.ots (qdef-defvar.lhs-ots qdef)))
	(define (common message)
	  (condition
	    (make-who-condition 'chi-defvar/checked)
	    (make-message-condition message)
	    (make-syntax-violation (qdef.input-form qdef) rhs.stx)
	    (make-type-signature-condition rhs.sig)))
	(case-signature-specs (psi.retvals-signature rhs.psi)
	  (<no-return>
	   ;;The expression is marked as not-returning.
	   (when (options::warn-about-not-returning-expressions)
	     (raise-continuable
	      (common "expression used as right-hand side in typed variable definition is typed as not returning")))
	   (when (qdef-defvar.untyped? qdef)
	     (qdef-defvar.type-propagation qdef (<top>-ots) lexenv.run))
	   rhs.psi)

	  ((<void>)
	   ;;The expression is marked as returning void.
	   (if (qdef-typed-defvar.init-expr? qdef)
	       ;;This  definition has  an initialisation  expression returning  void:
	       ;;this is wrong.
	       (begin
		 (when (<untyped>-ots? lhs.ots)
		   (qdef-defvar.type-propagation qdef (<top>-ots) lexenv.run))
		 (%handle-error rhs.psi common "expression used as right-hand side in typed variable definition is typed as returning void"))
	     ;;This definition  has no  initialisation expression,  so it  is obvious
	     ;;that the RHS returns void.  We  propagate the type to "<void>" so that
	     ;;it is an error to use this variable without setting to some value.
	     (begin
	       (when (<untyped>-ots? lhs.ots)
		 (qdef-defvar.type-propagation qdef (<void>-ots) lexenv.run))
	       rhs.psi)))

	  ((single-value)
	   ;;The RHS expression returns a single value.  Good.
	   => (lambda (rhs.ots)
		(%process-single-value-case qdef lexenv.run lexenv.expand
					    lhs.ots rhs.ots rhs.psi common)))

	  (<list-of-spec>
	   ;;The RHS  expression returns  an unspecified number  of values,  of known
	   ;;type.  RHS.OTS holds a "<list-of-type-spec>" OTS.
	   => (lambda (rhs.ots)
		;;We delegate  to the run-time code  the validation of the  number of
		;;returned values.
		(%process-single-value-case qdef lexenv.run lexenv.expand
					    lhs.ots (list-of-type-spec.item-ots rhs.ots) rhs.psi common)))

	  (<list>
	   ;;The  RHS  expression  returns  an   unspecified  number  of  values,  of
	   ;;unspecified type; RHS.OTS holds a "<list>" OTS.
	   (if (<untyped>-ots? lhs.ots)
	       ;;No type annotation was present  in the DEFINE/CHECKED syntax use, so
	       ;;we cast the value to "<top>".
	       (begin
		 (qdef-defvar.type-propagation qdef (<top>-ots) lexenv.run)
		 rhs.psi)
	     ;;A type annotation  was present in the DEFINE/CHECKED  syntax use; this
	     ;;is  a checked  typed  variable, so  we do  introduce  code to  perform
	     ;;run-time type validation.
	     (%insert-single-value-validator qdef lexenv.run lexenv.expand rhs.psi lhs.ots)))

	  (else
	   ;;The expression returns zero, two or more values.
	   (%handle-error rhs.psi common
			  "expression used as right-hand side in typed variable definition is typed as returning zero, two or more values")))))

    (define* (%process-single-value-case qdef lexenv.run lexenv.expand
					 lhs.ots rhs.ots rhs.psi common)
      (if (<untyped>-ots? lhs.ots)
	  ;;No type  annotation was  present in  the DEFINE/CHECKED  syntax use,  so we
	  ;;perform RHS type propagation.
	  (begin
	    (qdef-defvar.type-propagation qdef rhs.ots lexenv.run)
	    rhs.psi)
	;;A  type annotation  was present  in the  DEFINE/CHECKED syntax  use, so  we
	;;validate RHS.OTS against it.
	(cond ((object-type-spec.matching-super-and-sub? lhs.ots rhs.ots)
	       ;;Types do match.  Good.
	       rhs.psi)
	      ((object-type-spec.compatible-super-and-sub? lhs.ots rhs.ots)
	       ;;Types  do not  match,  but they  are compatible.   Since  this is  a
	       ;;checked  syntax:  we  do  insert   code  to  perform  run-time  type
	       ;;validation.
	       (%insert-single-value-validator qdef lexenv.run lexenv.expand rhs.psi lhs.ots))
	      (else
	       ;;Types do not match and are not compatible.  Bad.
	       (%error-mismatching-type 'chi-defvar/checked qdef lhs.ots rhs.psi)))))

    (define (%insert-single-value-validator qdef lexenv.run lexenv.expand rhs.psi lhs.ots)
      (make-psi (psi.input-form rhs.psi)
	(let* ((validator.stx (object-type-spec.single-value-validator-lambda-stx lhs.ots #t))
	       (validator.psi (chi-expr validator.stx lexenv.run lexenv.expand)))
	  (build-application no-source
	      (psi.core-expr validator.psi)
	    (list (psi.core-expr rhs.psi)		     ;value
		  (build-data no-source 1)		     ;value-index
		  (build-data no-source (qdef.var-sym qdef)) ;caller-who
		  )))
	(psi.retvals-signature rhs.psi)))

    #| end of module: CHI-DEFVAR/CHECKED |# )

;;; --------------------------------------------------------------------

  (define (%handle-error rv common message)
    (case-expander-language
      ((typed)
       (raise			(condition (make-expand-time-type-signature-violation)	(common message))))
      ((default)
       (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common message)))
       rv)
      ((strict-r6rs)
       rv)))

  (define (%error-mismatching-type caller-who qdef lhs.ots rhs.psi)
    (raise
     (condition (make-expand-time-type-signature-violation)
		(make-who-condition caller-who)
		(make-message-condition
		 "expression used as right-hand side in typed variable definition has non-matching type")
		(make-syntax-violation (qdef.input-form qdef) (psi.input-form rhs.psi))
		(make-expected-type-signature-condition (make-type-signature/single-value lhs.ots))
		(make-returned-type-signature-condition (psi.retvals-signature rhs.psi)))))

  #| end of module |# )


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
;;End:
