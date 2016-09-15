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


(module (new-transformer
	 delete-transformer
	 is-a?-transformer
	 method-call-transformer
	 typed-variable-with-private-access!-transformer
	 unsafe-cast-signature-transformer
	 case-type-transformer
	 assert-signature-transformer
	 assert-signature-and-return-transformer
	 cast-signature-transformer
	 hash-function-transformer
	 equality-predicate-transformer
	 comparison-procedure-transformer
	 hash-transformer
	 ;;
	 type-of-transformer
	 ;;
	 type-annotation=?-transformer
	 type-annotation-super-and-sub?-transformer
	 type-annotation-common-ancestor-transformer
	 type-annotation-ancestors-transformer
	 type-annotation-syntax-transformer
	 type-annotation-matching-transformer
	 ;;
	 type-signature-super-and-sub?-transformer
	 type-signature-common-ancestor-transformer
	 type-signature-matching-transformer
	 type-signature-union-transformer
	 ;;
	 type-descriptor-parent-transformer
	 type-descriptor-ancestors-transformer
	 type-descriptor=?-transformer
	 type-descriptor-super-and-sub?-transformer
	 type-descriptor-matching-transformer
	 #| end of EXPORTS |# )


;;;; helpers

(define-syntax (with-object-type-syntactic-binding stx)
  (sys::syntax-case stx ()
    ((_ (?input-form.stx ?type-annotation ?lexenv ?object-type-spec)
	. ?body)
     (and (sys::identifier? (sys::syntax ?input-form.stx))
	  (sys::identifier? (sys::syntax ?lexenv))
	  (sys::identifier? (sys::syntax ?object-type-spec)))
     (sys::syntax
      (let ((?object-type-spec (let ((type.ann ?type-annotation))
				 (try
				     (type-annotation->object-type-spec type.ann ?lexenv ?type-annotation)
				   (catch E
				     (else
				      (syntax-violation __who__ "invalid type annotation" ?input-form.stx type.ann)))))))
	. ?body)))
    ))

(define (%fxiota fx item*)
  (if (pair? item*)
      (cons fx (%fxiota (fxadd1 fx) (cdr item*)))
    '()))


;;;; module core-macro-transformer: NEW

(module (new-transformer)

  (define-module-who new)

  (define-core-transformer (new input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to expand NEW  syntaxes from the top-level  built in
    ;;environment.  Expand  the syntax  object INPUT-FORM.STX in  the context  of the
    ;;given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-name ?rand* ...)
       (with-object-type-syntactic-binding (input-form.stx ?type-name lexenv.run requested.ots)
	 (cond ((or (list-of-type-spec? requested.ots)
		    (list-type-spec?    requested.ots))
		;;LIST and LIST-OF type annotations  have no constructor, but we know
		;;they need LIST.
		(%build-object-with-constructor input-form.stx lexenv.run lexenv.expand
						requested.ots (core-prim-id 'list) ?rand*))

	       ((or (vector-of-type-spec? requested.ots)
		    (vector-type-spec?    requested.ots))
		;;VECTOR and VECTOR-OF  type annotations have no  constructor, but we
		;;know they need VECTOR.
		(%build-object-with-constructor input-form.stx lexenv.run lexenv.expand
						requested.ots (core-prim-id 'vector) ?rand*))

	       ((or (pair-of-type-spec? requested.ots)
		    (pair-type-spec?    requested.ots))
		;;PAIR and PAIR-OF type annotations  have no constructor, but we know
		;;they need PAIR.
		(%build-object-with-constructor input-form.stx lexenv.run lexenv.expand
						requested.ots (core-prim-id 'pair) ?rand*))

	       ((compound-condition-type-spec? requested.ots)
		;;CONDITION type  annotations have no  constructor, but we  know they
		;;need CONDITION.
		(%build-object-with-constructor input-form.stx lexenv.run lexenv.expand
						requested.ots (core-prim-id 'condition) ?rand*))

	       ((enumeration-type-spec? requested.ots)
		;;ENUMERATION  type  annotations  have  no constructor,  but  we  can
		;;validate the symbol.
		(%build-object-with-validator input-form.stx lexenv.run lexenv.expand
					      requested.ots ?rand*))

	       ((hashtable-type-spec? requested.ots)
		(%build-hashtable-constructor input-form.stx lexenv.run lexenv.expand
					      ?type-name ?rand* requested.ots __synner__))

	       ((interface-type-spec? requested.ots)
		(__synner__ "attempt to instantiate interface" ?type-name))

	       ((object-type-spec.constructor-stx requested.ots)
		=> (lambda (constructor.stx)
		     (if (boolean? constructor.stx)
			 (%build-object-with-validator input-form.stx lexenv.run lexenv.expand
						       requested.ots ?rand*)
		       (%build-object-with-constructor input-form.stx lexenv.run lexenv.expand
						       requested.ots constructor.stx ?rand*))))
	       (else
		(__synner__ "attempt to instantiate object-type with no constructor (abstract type?)" ?type-name)))))
      (_
       (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

  (define (%build-object-with-validator input-form.stx lexenv.run lexenv.expand requested.ots rand*.stx)
    ;;The requested type REQUESTED.OTS references an object-type specification having
    ;;*no* constructor, but having #t  in the "<object-type-spec>" field.  This means
    ;;a request to  validate the single operand for the  correct type, at expand-time
    ;;and/or run-time.  For example:
    ;;
    ;;   (new <fixnum> 123)
    ;;
    ;;Build  and return  an expression  that validates  the operand  as matching  the
    ;;requested type.
    ;;
    (if (list-of-single-item? rand*.stx)
	(let* ((constructor.psi	(chi-expr (car rand*.stx) lexenv.run lexenv.expand))
	       (constructor.ots	(%process-retvals-signature input-form.stx constructor.psi)))
	  (receive (constructor.core exact-match?)
	      (%generate-constructor-code input-form.stx lexenv.run lexenv.expand
					  requested.ots constructor.psi constructor.ots)
	    (make-psi input-form.stx
	      constructor.core
	      (make-type-signature/single-value (if exact-match? constructor.ots requested.ots)))))
      (raise
       (condition (make-who-condition __module_who__)
		  (make-message-condition "type with no constructor requires a single operand of the specified type")
		  (make-syntax-violation input-form.stx #f)))))

;;; --------------------------------------------------------------------

  (define (%build-object-with-constructor input-form.stx lexenv.run lexenv.expand requested.ots constructor.stx rand*.stx)
    ;;The requested type REQUESTED.OTS references an object-type specification having
    ;;a constructor.  Build and return a PSI representing the constructor application
    ;;to the given operands.
    ;;
    ;;About the whole expression type signature:
    ;;
    ;;* If the constructor application returns  a single value whose type matches the
    ;;requested one: the  NEW expression has CONSTRUCTOR.OTS as  return value's type.
    ;;CONSTRUCTOR.OTS might be more descriptive than REQUESTED.OTS.
    ;;
    ;;*  If  the  constructor  application  returns a  single  value  whose  type  is
    ;;compatible with the requested one: the NEW expression contains a type validator
    ;;and it has REQUESTED.OTS has as return value's type.
    ;;
    (let* ((constructor.psi	(chi-expr `(,constructor.stx . ,rand*.stx) lexenv.run lexenv.expand))
	   (constructor.ots	(%process-retvals-signature input-form.stx constructor.psi)))
      (receive (constructor.core exact-match?)
	  (%generate-constructor-code input-form.stx lexenv.run lexenv.expand
				      requested.ots constructor.psi constructor.ots)
	(make-psi input-form.stx
	  constructor.core
	  (make-type-signature/single-value (if exact-match? constructor.ots requested.ots))))))

;;; --------------------------------------------------------------------

  (module (%process-retvals-signature)

    (define (%process-retvals-signature input-form.stx constructor.psi)
      ;;Process  the  "<type-signature>"  returned  by  the  constructor  expression:
      ;;validate  that a  single return  value is  returned.  Return  an instance  of
      ;;"<object-type-spec>"  representing the  type  of the  value  returned by  the
      ;;constructor.
      ;;
      (define (common message)
	(condition
	  (make-who-condition __module_who__)
	  (make-message-condition message)
	  (make-syntax-violation input-form.stx (psi.input-form constructor.psi))
	  (make-type-signature-condition (psi.retvals-signature constructor.psi))))
      (case-type-signature-full-structure (psi.retvals-signature constructor.psi)
	((single-value)
	 ;;The expression returns a single value.  Good this OTS will become the type
	 ;;of the expression.
	 => (lambda (constructor.ots) constructor.ots))

	(<bottom>
	 ;;The expression is marked as not-returning.
	 (%handle-error common "type constructor expression is typed as not returning"))

	((<void>)
	 ;;The expression is marked as returning void.
	 (%handle-error common "type constructor expression is typed as returning void"))

	(<list>/<list-of-spec>
	 ;;The expression returns an unspecified  number of values.  Let's simulate a
	 ;;"<top>" syntactic binding  are delegate the run-time code  to validate the
	 ;;number of arguments.
	 (<top>-ots))

	(else
	 ;;The expression returns zero, two or more values.
	 (%handle-error common "type constructor expression is typed as returning zero, two or more values"))))

    (define (%handle-error common message)
      (case-expander-language
	((typed)
	 (raise			(condition (make-expand-time-type-signature-violation)	(common message))))
	((default)
	 (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common message)))
	 (<top>-ots))
	((strict-r6rs)
	 (<top>-ots))))

    #| end of module: %PROCESS-RETVALS-SIGNATURE |# )

;;; --------------------------------------------------------------------

  (define (%generate-constructor-code input-form.stx lexenv.run lexenv.expand
				      requested.ots constructor.psi constructor.ots)
    ;;The  NEW syntax  use  requested  building an  instance  of REQUESTED.OTS.   The
    ;;constructor is  the expression  CONSTRUCTOR.PSI and it  returns an  instance of
    ;;CONSTRUCTOR.OTS.
    ;;
    ;;Return 2 values:
    ;;
    ;;*  If the  CONSTRUCTOR.OTS matches  the REQUESTED.OTS:  the constructor's  core
    ;;expression and #t.
    ;;
    ;;*  If  the CONSTRUCTOR.OTS  is  compatible  with  the REQUESTED.OTS:  wrap  the
    ;;constructor's core  expression in a  validator for the requested  type.  Return
    ;;the resulting core epxression and #f.
    ;;
    ;;If the CONSTRUCTOR.OTS is does not match the REQUESTED.OTS: raise an exception.
    ;;
    (cond ((object-type-spec.matching-super-and-sub? requested.ots constructor.ots)
	   (values (psi.core-expr constructor.psi) #t))
	  ((object-type-spec.compatible-super-and-sub? requested.ots constructor.ots)
	   (let* ((validator.stx (object-type-spec.single-value-validator-lambda-stx requested.ots #t))
		  (validator.psi (chi-expr validator.stx lexenv.run lexenv.expand)))
	     (values (build-application no-source
			 (psi.core-expr validator.psi)
		       (list (psi.core-expr constructor.psi)	     ;value
			     (build-data no-source 1)		     ;value-index
			     (build-data no-source __module_who__))) ;caller-who
		     #f)))
	  (else
	   (raise
	    (condition (make-expand-time-type-signature-violation)
		       (make-who-condition __module_who__)
		       (make-message-condition
			"type constructor expression has type not matching the requested type")
		       (make-syntax-violation input-form.stx (psi.input-form constructor.psi))
		       (make-expected-type-signature-condition (make-type-signature/single-value requested.ots))
		       (make-returned-type-signature-condition (make-type-signature/single-value constructor.ots)))))))

;;; --------------------------------------------------------------------

  (define (%build-hashtable-constructor input-form.stx lexenv.run lexenv.expand
					type-name.stx rand*.stx requested.ots synner)
    (unless (null? rand*.stx)
      (synner "expected no operands to hashtable constructor" rand*.stx))
    (let ((key.ots (hashtable-type-spec.key-ots requested.ots))
	  (val.ots (hashtable-type-spec.val-ots requested.ots)))
      (let ((hash-func.stx	(object-type-spec.applicable-hash-function key.ots))
	    (equal-pred.stx	(object-type-spec.equality-predicate       key.ots)))
	(unless hash-func.stx
	  (synner "key object-type does not implement a hash function" (object-type-spec.name key.ots)))
	(unless equal-pred.stx
	  (synner "key object-type does not implement an equality predicate" (object-type-spec.name key.ots)))
	(let* ((output-form.sexp	(let ((table.id (make-syntactic-identifier-for-temporary-variable "table")))
					  `(receive-and-return (,table.id)
					       (make-hashtable ,hash-func.stx ,equal-pred.stx)
					     ($hashtable-type-descriptor-set! ,table.id (type-descriptor ,type-name.stx)))))
	       (output-form.psi		(chi-expr (bless output-form.sexp) lexenv.run lexenv.expand)))
	  (make-psi input-form.stx
	    (psi.core-expr output-form.psi)
	    (make-type-signature/single-value requested.ots))))))

  #| end of module |# )


;;;; module core-macro-transformer: DELETE

(module (delete-transformer)

  (define-core-transformer (delete input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand DELETE syntaxes from the top-level built in
    ;;environment.  Expand  the syntax  object INPUT-FORM.STX in  the context  of the
    ;;given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?expr)
       (let* ((expr.psi (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig (psi.retvals-signature expr.psi)))
	 (case-type-signature-full-structure expr.sig
	   ((single-value)
	    => (lambda (type.ots)
		 (%apply-appropriate-destructor __who__ input-form.stx lexenv.run lexenv.expand type.ots expr.psi)))
	   (<bottom>
	    (__synner__ "subject expression of destructor syntax defined to no return" ?expr))
	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature; we need to insert a run-time dispatch.
	    (%run-time-destruction input-form.stx lexenv.run lexenv.expand expr.psi))
	   (else
	    ;;The horror!!!  We  have established at expand-time  that the expression
	    ;;returns multiple values; type violation.
	    (syntax-violation __who__
	      "the expression used as destructor operand returns multiple values"
	      input-form.stx ?expr)))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define (%apply-appropriate-destructor who input-form.stx lexenv.run lexenv.expand type.ots expr.psi)
    (cond ((object-type-spec.destructor-stx type.ots)
	   => (lambda (destructor.stx)
		;;This record type has a default destructor.
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   destructor.stx expr.psi '())))
	  (else
	   ;;This  record  type has  *no*  default  destructor; default  to  run-time
	   ;;destruction.
	   ;;
	   ;;Example  of usefulness  of defaulting  to run-time  destruction: if  the
	   ;;object is  a record  with a  destructor set at  run-time, this  way that
	   ;;destructor will be called.
	   (%run-time-destruction input-form.stx lexenv.run lexenv.expand expr.psi))))

  (define* (%run-time-destruction input-form.stx lexenv.run lexenv.expand {expr.psi psi?})
    (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
				       (core-prim-id 'internal-delete) expr.psi '()))

  #| end of module: DELETE-TRANSFORMER |# )


;;;; module core-macro-transformer: IS-A?

(module (is-a?-transformer)

  (define-module-who is-a?)

  (define-core-transformer (is-a? input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand Vicare's IS-A?  syntaxes from the top-level
    ;;built in environment.   Expand the syntax object INPUT-FORM.STX  in the context
    ;;of the given LEXENV; return a PSI object.
    ;;
    (syntax-match input-form.stx (pair list vector pair-of list-of vector-of)
      ((_ ?jolly ?type-annotation)
       (underscore-id? ?jolly)
       ;;This is the case:
       ;;
       ;;   (is-a? _ <string>)
       ;;
       ;;which should expand to the predicate of "<string>".
       (chi-expr (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run type-annotation.ots)
		   (or (object-type-spec.type-predicate-stx type-annotation.ots)
		       (__synner__ "type annotation has no predicate for run-time use" ?type-annotation)))
		 lexenv.run lexenv.expand))

      ((_ ?expr ?type-annotation)
       (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run type-annotation.ots)
	 (%expand-to-single-value-predicate input-form.stx lexenv.run lexenv.expand
					    ?expr ?type-annotation type-annotation.ots __synner__)))
      (_
       (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

  (module (%expand-to-single-value-predicate)

    (define (%expand-to-single-value-predicate input-form.stx lexenv.run lexenv.expand
					       expr.stx type-annotation.stx type-annotation.ots synner)
      ;;Given the input form:
      ;;
      ;;   (is-a? ?expr ?type-annotation)
      ;;
      ;;the  expression  EXPR.STX is  expected  to  return  a  single value  of  type
      ;;TYPE-ANNOTATION.OTS.   We try  to check  this  at expand-time,  and when  not
      ;;possible at run-time.
      ;;
      (let* ((expr.psi	(chi-expr expr.stx lexenv.run lexenv.expand))
	     (expr.sig	(psi.retvals-signature expr.psi)))
	(define (%run-time-predicate)
	  (%expand-to-run-time-predicate-application input-form.stx lexenv.run lexenv.expand
						     expr.psi type-annotation.ots synner))
	(if (options::typed-language-enabled?)
	    (case-type-signature-full-structure expr.sig
	      ((single-value)
	       => (lambda (expr.ots)
		    (%match-pred-type-against-single-value-expr input-form.stx lexenv.run lexenv.expand
								expr.psi expr.ots type-annotation.ots %run-time-predicate)))

	      (<bottom>
	       (let ((common (condition
			       (make-who-condition __module_who__)
			       (make-message-condition "subject expression typed as not returning")
			       (make-syntax-violation input-form.stx expr.stx))))
		 (if (options::typed-language-enabled?)
		     (raise (condition (make-expand-time-type-signature-violation) common))
		   (begin
		     (raise-continuable (condition (make-expand-time-type-signature-warning) common))
		     (%run-time-predicate)))))

	      (<list>
	       ;;The expression  returns an unspecified  number of values  of UNspecified
	       ;;type.
	       (%run-time-predicate))

	      (<list-of-spec>
	       => (lambda (expr.ots)
		    (%match-pred-type-against-list-of input-form.stx lexenv.run lexenv.expand
						      expr.psi expr.ots type-annotation.ots %run-time-predicate)))

	      (else
	       (raise
		(condition (make-expand-time-type-signature-violation)
			   (make-who-condition __module_who__)
			   (make-message-condition "subject expression returns multiple values")
			   (make-syntax-violation input-form.stx expr.stx)))))
	  ;;We cannot trust typing when the typed language is disabled.
	  (%run-time-predicate))))

    (define (%match-pred-type-against-single-value-expr input-form.stx lexenv.run lexenv.expand
							expr.psi expr.ots type-annotation.ots %run-time-predicate)
      ;;Given the input form:
      ;;
      ;;   (is-a? ?expr ?pred-type)
      ;;
      ;;we have determined  that the expression ?EXPR returns a  single value of type
      ;;EXPR.OTS and such type  is expected to match TYPE-ANNOTATION.OTS.  We  try to check this
      ;;at expand-time, and when not possible at run-time.
      ;;
      (cond ((object-type-spec.matching-super-and-sub? type-annotation.ots expr.ots)
	     ;;The expression  returns a  single value  matching the  predicate type.
	     ;;Expand-time success!
	     (%make-true-psi input-form.stx expr.psi))

	    ((object-type-spec.compatible-super-and-sub? type-annotation.ots expr.ots)
	     ;;The expression  returns a single  value compatible with  the predicate
	     ;;type.  Let's check it at run-time.
	     (%run-time-predicate))

	    (else
	     ;;The expression returns a single  value INcompatible with the predicate
	     ;;type.  Expand-time failure!
	     (%make-false-psi input-form.stx expr.psi))))

    (define* (%match-pred-type-against-list-of input-form.stx lexenv.run lexenv.expand
					       expr.psi {expr.ots list-of-type-spec?}
					       type-annotation.ots %run-time-predicate)
      ;;Given the input form:
      ;;
      ;;   (is-a? ?expr ?pred-type)
      ;;
      ;;we have determined that the expression ?EXPR returns an unspecified number of
      ;;values of a specified type.
      ;;
      (let ((item.ots	(list-of-type-spec.item-ots expr.ots))
	    (expr.core	(psi.core-expr expr.psi)))
	(cond ((object-type-spec.matching-super-and-sub? type-annotation.ots item.ots)
	       ;;If the  expression returns  a single value:  such value  matches the
	       ;;predicate type.   Let's check at  run-time that it returns  a single
	       ;;value and let the expression return true.
	       (make-psi input-form.stx
		 (build-application input-form.stx
		     (build-primref no-source 'expect-single-argument-and-return-true)
		   (list expr.core))
		 (make-type-signature/single-true)))

	      ((object-type-spec.compatible-super-and-sub? type-annotation.ots item.ots)
	       ;;If the expression  returns a single value: such  value is compatible
	       ;;with the predicate type.  Let's check it at run-time.
	       (%run-time-predicate))

	      (else
	       ;;Even if the  expression returns a single value: such  value does not
	       ;;match the predicate type.  Let's check at run-time that it returns a
	       ;;single value and let the expression return false.
	       (make-psi input-form.stx
		 (build-application input-form.stx
		     (build-primref no-source 'expect-single-argument-and-return-false)
		   (list expr.core))
		 (make-type-signature/single-false))))))

    #| end of module: %EXPAND-TO-SINGLE-VALUE-PREDICATE |# )

;;; --------------------------------------------------------------------

  (define* (%expand-to-run-time-predicate-application input-form.stx lexenv.run lexenv.expand
						      {expr.psi psi?} {type-annotation.ots object-type-spec?} synner)
    ;;Given the input form:
    ;;
    ;;   (is-a? ?expr ?type-annotation)
    ;;
    ;;we build the output form:
    ;;
    ;;   (?type-predicate ?expr)
    ;;
    ;;where  ?TYPE-PREDICATE is  a syntax  object representing  an expression  which,
    ;;expanded and evaluated, returns the type predicate for ?TYPE-ANNOTATION.
    ;;
    (cond ((object-type-spec.type-predicate-stx type-annotation.ots)
	   => (lambda (operator.stx)
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   operator.stx expr.psi '())))
	  (else
	   (synner "type specification has no predicate for run-time use" (object-type-spec.name type-annotation.ots)))))

;;; --------------------------------------------------------------------

  (module (%make-true-psi %make-false-psi)

    (define-syntax-rule (%make-true-psi ?input-form.stx ?expr.psi)
      (%make-boolean-psi ?input-form.stx ?expr.psi #t))

    (define-syntax-rule (%make-false-psi ?input-form.stx ?expr.psi)
      (%make-boolean-psi ?input-form.stx ?expr.psi #f))

    (define* (%make-boolean-psi input-form.stx {expr.psi psi?} bool)
      ;;Build and  return a PSI object  whose core language expression  evaluates the
      ;;subject expression for its side effects and then returns a boolean.
      ;;
      (let ((expr.core (psi.core-expr expr.psi)))
	(make-psi input-form.stx
	  (build-sequence no-source (list expr.core (build-data no-source bool)))
	  (if bool
	      (make-type-signature/single-true)
	    (make-type-signature/single-false)))))

    #| end of module |# )

  #| end of module: IS-A?-TRANSFORMER |# )


;;;; module core-macro-transformer: METHOD-CALL
;;
;;About protection levels for record-type members
;;-----------------------------------------------
;;
;;There is only one  place where protection levels defined for  fields and methods in
;;uses of DEFINE-RECORD-TYPE make a difference:  right here in the METHOD-CALL syntax
;;implementation.  Of all the uses of  METHOD-CALL, the only ones in which protection
;;levels matter are  those in which METHOD-CALL  is applied to the  fluid syntax THIS
;;injected in the body of methods as alias for the first method operand.
;;
;;We can think of a method clause like:
;;
;;   (define-record-type <duo>
;;     (fields one two)
;;     (method (doit)
;;       (+ (.one this) (.two this))))
;;
;;as expanding to:
;;
;;   (define/checked (<duo>-doit {subject <tow>})
;;     (typed-variable-with-private-access! subject)
;;     (fluid-let-syntax ((this (make-synonym-transformer #'subject)))
;;       (+ (method-call one this) (method-call two this))))
;;
;;such use of METHOD-CALL causes the macro to look for methods in the private methods
;;table, rather than the public methods table as in the general case.
;;

(module (method-call-transformer)

  (define-module-who method-call)

  (define-core-transformer (method-call input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to expand  Vicare's METHOD-CALL  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?method-name ?subject-expr ?arg* ...)
       (and (identifier? ?method-name)
	    (identifier? ?subject-expr))
       ;;The case  of ?SUBJECT-EXPR  being an  identifier is may  be special:  if the
       ;;identifier is bound to a type  variable, we know the single-value type right
       ;;away  an  we can  distinguish  between  public  and  private access  to  the
       ;;object-type's members.
       (%identifier-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
					     ?method-name ?subject-expr ?arg*))

      ((_ ?method-name ?subject-expr ?arg* ...)
       (identifier? ?method-name)
       (%general-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
					  ?method-name ?subject-expr ?arg*))

      (_
       (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

  (module (%identifier-subject-expr-dispatching)

    (define (%identifier-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
						  method-name.id subject-expr.id rand*.stx)
      (define (%default-dispatching)
	(%general-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
					   method-name.id subject-expr.id rand*.stx))
      (cond ((id->label subject-expr.id)
	     => (lambda (label)
		  (let ((descr (label->syntactic-binding-descriptor label lexenv.run)))
		    (case (syntactic-binding-descriptor.type descr)
		      ((lexical-typed)
		       ;;Reference to typed lexical variable; this means EXPR.STX is an
		       ;;identifier.  The syntactic binding's descriptor has format:
		       ;;
		       ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
		       ;;
		       (%lexical-typed-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
								method-name.id subject-expr.id rand*.stx
								(syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec descr)))

		      ((global-typed)
		       ;;Reference  to  global  imported typed  lexical  variable.   We
		       ;;expect the syntactic binding's descriptor to be:
		       ;;
		       ;;   (global-typed . (#<library> . ?loc))
		       ;;
		       ;;The library  is visited by default,  so we know that  the ?LOC
		       ;;actually        references        the       instance        of
		       ;;"<global-typed-variable-spec>".
		       (%global-typed-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
							       method-name.id subject-expr.id rand*.stx
							       (syntactic-binding-descriptor.value descr)))

		      (else
		       (%default-dispatching))))))
	    (else
	     (%default-dispatching))))

    (define (%lexical-typed-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
						     method-name.id subject-expr.id rand*.stx
						     subject-expr.lts)
      (let* ((subject-expr.ots	(typed-variable-spec.ots             subject-expr.lts))
	     (private-access?	(typed-variable-spec.private-access? subject-expr.lts)))
	(%typed-variable-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
						  method-name.id subject-expr.id subject-expr.ots rand*.stx
						  private-access?)))

    (define (%global-typed-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
						    method-name.id subject-expr.id rand*.stx
						    descr.value)
      (let* ((globvar.lib	(car descr.value))
	     (globvar.type-loc	(cdr descr.value)))
	((inv-collector) globvar.lib)
	(if (symbol-bound? globvar.type-loc)
	    (let ((subject-expr.gts (symbol-value globvar.type-loc)))
	      (if (global-typed-variable-spec? subject-expr.gts)
		  (let ((subject-expr.ots	(typed-variable-spec.ots             subject-expr.gts))
			(private-access?	(typed-variable-spec.private-access? subject-expr.gts)))
		    (%typed-variable-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
							      method-name.id subject-expr.id subject-expr.ots rand*.stx
							      private-access?))
		(assertion-violation __module_who__
		  "invalid object in loc gensym's \"value\" slot of \"global-typed\" syntactic binding's descriptor"
		  subject-expr.id descr.value subject-expr.gts)))
	  (assertion-violation __module_who__
	    "unbound loc gensym of \"global-typed\" syntactic binding's descriptor"
	    subject-expr.id descr.value))))

    (define (%typed-variable-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
						      method-name.id subject-expr.id subject-expr.ots rand*.stx
						      private-access?)
      (let* ((method-name.sym	(identifier->symbol method-name.id))
	     (subject-expr.psi	(chi-expr subject-expr.id lexenv.run lexenv.expand))
	     (subject-expr.sig	(psi.retvals-signature subject-expr.psi)))
	(define-syntax-rule (%late-binding)
	  (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
					       method-name.sym subject-expr.psi rand*.stx))
	(cond ((or (<top>-ots?     subject-expr.ots)
		   (<bottom>-ots?  subject-expr.ots)
		   (<untyped>-ots? subject-expr.ots))
	       (%late-binding))
	      (else
	       (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						     method-name.id method-name.sym
						     subject-expr.id subject-expr.psi subject-expr.ots
						     rand*.stx private-access?)))))

    #| end of module: %IDENTIFIER-SUBJECT-EXPR-DISPATCHING |# )

;;; --------------------------------------------------------------------

  (define (%general-subject-expr-dispatching input-form.stx lexenv.run lexenv.expand
					     method-name.id subject-expr.stx rand*.stx)
    (let* ((method-name.sym	(identifier->symbol method-name.id))
	   (subject-expr.psi	(chi-expr subject-expr.stx lexenv.run lexenv.expand))
	   (subject-expr.sig	(psi.retvals-signature subject-expr.psi)))
      (define-syntax-rule (%late-binding)
	(%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
					     method-name.sym subject-expr.psi rand*.stx))
      (define (%error message)
	(raise
	 (condition (make-who-condition __module_who__)
		    (make-message-condition message)
		    (make-syntax-violation input-form.stx subject-expr.stx)
		    (make-irritants-condition (list subject-expr.sig)))))
      (case-type-signature-full-structure subject-expr.sig
	((<top>)
	 (%late-binding))
	((<untyped>)
	 (%late-binding))

	((single-value)
	 => (lambda (subject-expr.ots)
	      (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						    method-name.id method-name.sym
						    subject-expr.stx subject-expr.psi subject-expr.ots
						    rand*.stx #f)))

	(<bottom>
	 (let ((common (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "subject expression of method call defined to never return")
			 (make-syntax-violation input-form.stx subject-expr.stx)
			 (make-irritants-condition (list subject-expr.sig)))))
	   (if (options::typed-language-enabled?)
	       (raise (condition (make-expand-time-type-signature-violation) common))
	     (begin
	       (raise-continuable (condition (make-expand-time-type-signature-warning) common))
	       (%late-binding)))))

	(<list>
	 ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	 ;;signature; we need to insert a run-time dispatch.
	 (%late-binding))

	(else
	 ;;We have determined at expand-time  that the expression returns multiple
	 ;;values.
	 (%error "subject expression of method call returns multiple values")))))

;;; --------------------------------------------------------------------

  (define* (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						 method-name.id method-name.sym
						 subject-expr.stx subject-expr.psi subject-expr.ots
						 arg*.stx private-access?)
    (define (%error-unknown-method-name)
      (raise
       (condition (make-who-condition __module_who__)
		  (make-message-condition "unknown method name for type of subject expression")
		  (make-syntax-violation input-form.stx method-name.id)
		  (make-application-operand-signature-condition (psi.retvals-signature subject-expr.psi))
		  (make-type-method-name-condition method-name.sym))))
    (cond (private-access?
	   (cond ((object-type-spec.applicable-private-method-stx subject-expr.ots method-name.sym)
		  ;;A matching method name exists.
		  => (lambda (method.stx)
		       ;;A matching method name exists.
		       (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
							  method.stx subject-expr.psi arg*.stx)))
		 (else
		  (%error-unknown-method-name))))

	  ((object-type-spec.applicable-method-stx subject-expr.ots method-name.sym)
	   ;;A matching method name exists.
	   => (lambda (method.stx)
		;;A matching method name exists.
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   method.stx subject-expr.psi arg*.stx)))

	  (else
	   (%error-unknown-method-name))))

;;; --------------------------------------------------------------------

  (define (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
					       method-name.sym subject-expr.psi arg*.stx)
    ;;The  type of  the  values returned  by  the subject  expression  is unknown  at
    ;;expand-time; so we  expand to an expression that searches  at run-time a method
    ;;matching the  given name.  In other  words: we default to  "late binding" (also
    ;;known as "run-time dispatching" at some level of abstract reasoning).
    ;;
    (let* ((expr.core   (psi.core-expr subject-expr.psi))
	   (arg*.psi    (chi-expr* arg*.stx lexenv.run lexenv.expand))
	   (arg*.core   (map psi.core-expr arg*.psi)))
      (make-psi input-form.stx
	(build-application input-form.stx
	    (build-primref no-source 'method-call-late-binding)
	  (cons* (build-data no-source method-name.sym)
		 (build-data no-source #f) ;no predetermined type-descriptor
		 expr.core
		 arg*.core))
	(make-type-signature/fully-unspecified))))

  #| end of module |# )


;;;; module core-macro-transformer: TYPED-VARIABLE-WITH-PRIVATE-ACCESS!

(define-core-transformer (typed-variable-with-private-access! input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand Vicare's TYPED-VARIABLE-WITH-PRIVATE-ACCESS!
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?varname)
     (identifier? ?varname)
     (cond ((id->label ?varname)
	    => (lambda (label)
		 (let ((descr (label->syntactic-binding-descriptor label lexenv.run)))
		   (case (syntactic-binding-descriptor.type descr)
		     ((lexical-typed)
		      ;;Reference to  typed lexical variable; this  means EXPR.STX is
		      ;;an  identifier.   The   syntactic  binding's  descriptor  has
		      ;;format:
		      ;;
		      ;;   (lexical-typed . (#<lexical-typed-variable-spec> . ?expanded-expr))
		      ;;
		      (let ((varname.lts (syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec descr)))
			(typed-variable-spec.private-access?-set! varname.lts #t)))

		     ((global-typed)
		      ;;Reference  to global  imported  typed  lexical variable.   We
		      ;;expect the syntactic binding's descriptor to be:
		      ;;
		      ;;   (global-typed . (#<library> . ?loc))
		      ;;
		      ;;The library is  visited by default, so we know  that the ?LOC
		      ;;actually        references       the        instance       of
		      ;;"<global-typed-variable-spec>".
		      (let* ((descr.value		(syntactic-binding-descriptor.value descr))
			     (globvar.lib		(car descr.value))
			     (globvar.type-loc	(cdr descr.value)))
			((inv-collector) globvar.lib)
			(if (symbol-bound? globvar.type-loc)
			    (let ((varname.gts (symbol-value globvar.type-loc)))
			      (if (global-typed-variable-spec? varname.gts)
				  (typed-variable-spec.private-access?-set! varname.gts #t)
				(assertion-violation __who__
				  "invalid object in loc gensym's \"value\" slot of \"global-typed\" syntactic binding's descriptor"
				  ?varname descr.value varname.gts)))
			  (assertion-violation __who__
			    "unbound loc gensym of \"global-typed\" syntactic binding's descriptor"
			    ?varname descr.value))))

		     (else
		      (__synner__ "expected typed variable identifier as argument" ?varname))))
		 (make-psi input-form.stx
		   (build-void)
		   (make-type-signature/single-void))))
	   (else
	    (__synner__ "expected bound syntactic identifier as argument" ?varname))))

    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer: HASH

(module (hash-transformer)

  (define-module-who hash)

  (define-core-transformer (hash input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand Vicare's HASH syntaxes  from the top-level
    ;;built in environment.   Expand the syntax object INPUT-FORM.STX  in the context
    ;;of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?expr)
       (let* ((expr.psi	(chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig	(psi.retvals-signature expr.psi)))
	 (define-syntax-rule (%late-binding)
	   (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
						expr.psi))
	 (define (%error message)
	   (raise
	    (condition (make-who-condition __module_who__)
		       (make-message-condition message)
		       (make-syntax-violation input-form.stx ?expr)
		       (make-irritants-condition (list expr.sig)))))
	 (case-type-signature-full-structure expr.sig
	   ((<top>)
	    (%late-binding))
	   ((<untyped>)
	    (%late-binding))

	   ((single-value)
	    => (lambda (expr.ots)
		 (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						       ?expr expr.psi expr.ots)))

	   (<bottom>
	    (let ((common (condition
			    (make-who-condition __module_who__)
			    (make-message-condition "subject expression of hash syntax defined to never return")
			    (make-syntax-violation input-form.stx ?expr)
			    (make-irritants-condition (list expr.sig)))))
	      (if (options::typed-language-enabled?)
		  (raise (condition (make-expand-time-type-signature-violation) common))
		(begin
		  (raise-continuable (condition (make-expand-time-type-signature-warning) common))
		  (%late-binding)))))

	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature; we need to insert a run-time dispatch.
	    (%late-binding))

	   (else
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (%error "subject expression of method call returns multiple values")))))

      (_
       (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

  (define* (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						 expr.stx expr.psi expr.ots)
    (cond ((object-type-spec.applicable-hash-function expr.ots)
	   => (lambda (hash-function.id)
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   hash-function.id expr.psi '())))
	  (else
	   (raise
	    (condition (make-who-condition __module_who__)
		       (make-message-condition "undefined hash function for type of subject expression")
		       (make-syntax-violation input-form.stx expr.stx)
		       (make-application-operand-signature-condition (psi.retvals-signature expr.psi)))))))

;;; --------------------------------------------------------------------

  (define (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
					       expr.psi)
    ;;The  type of  the  values returned  by  the subject  expression  is unknown  at
    ;;expand-time; so we  expand to an expression that searches  at run-time a method
    ;;matching the  given name.  In other  words: we default to  "late binding" (also
    ;;known as "run-time dispatching" at some level of abstract reasoning).
    ;;
    (let ((expr.core   (psi.core-expr expr.psi)))
      (make-psi input-form.stx
	(build-application input-form.stx
	    (build-primref no-source 'hash-function-late-binding)
	  (list expr.core))
	(make-type-signature/single-value (core-prim-spec '<non-negative-fixnum>)))))

  #| end of module |# )


;;;; module core-macro-transformer: CASE-TYPE

(define-core-transformer (case-type input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  CASE-TYPE  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define (main)
    (syntax-match input-form.stx ()
      ((_ ?expr ?case-clause0 ?case-clause* ...)
       (%build-output-form input-form.stx lexenv.run lexenv.expand
			   ?expr (cons ?case-clause0 ?case-clause*)))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define-synner synner (quote case-type) input-form.stx)

  (define (%build-output-form input-form.stx lexenv.run lexenv.expand
			      expr.stx case-clause*.stx)
    (let* ((expr.psi     (chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.sig     (psi.retvals-signature expr.psi)))
      (let* ((matcher.sexp	(let ((arg.sym (gensym "arg")))
				  `(lambda/std (,arg.sym)
				     (cond ,@(%build-branches input-form.stx arg.sym case-clause*.stx)))))
	     (matcher.psi	(chi-expr (bless matcher.sexp) lexenv.run lexenv.expand)))
	(make-psi input-form.stx
	  (build-application no-source
	      (psi.core-expr matcher.psi)
	    (list (psi.core-expr expr.psi)))
	  (make-type-signature/fully-unspecified)))))

  (define (%build-branches input-form.stx arg.sym case-clause*.stx)
    ;;This loop is like MAP, but we want  to detect if the ELSE clause (when present)
    ;;is used only as last clause.
    (let recur ((case-clause*.stx case-clause*.stx))
      (if (pair? case-clause*.stx)
	  (let ((case-clause.stx (car case-clause*.stx)))
	    (cons (syntax-match case-clause.stx (=> else)
		    (((?type) => ?receiver-expr)
		     `((is-a? ,arg.sym ,?type)
		       (,?receiver-expr ,arg.sym)))

		    (((?type) ?body0 ?body* ...)
		     `((is-a? ,arg.sym ,?type)
		       ,?body0 . ,?body*))

		    ((else ?body0 ?body* ...)
		     (if (null? (cdr case-clause*.stx))
			 `(else ,?body0 . ,?body*)
		       (synner "the ELSE clause is not the last clause" case-clause.stx)))

		    (else
		     (synner "invalid clause syntax" case-clause.stx)))
		  (recur (cdr case-clause*.stx))))
	'())))

  (main))


;;;; module core-macro-transformer: ASSERT-SIGNATURE, ASSERT-SIGNATURE-AND-RETURN, CAST-SIGNATURE

(module (assert-signature-transformer assert-signature-and-return-transformer cast-signature-transformer)
  ;;Transformer   function    used   to   expand   Vicare's    ASSERT-SIGNATURE   and
  ;;ASSERT-SIGNATURE-AND-RETURN  syntaxes from  the top-level  built in  environment.
  ;;Expand  the syntax  object INPUT-FORM.STX  in the  context of  the given  LEXENV;
  ;;return a PSI struct.
  ;;
  (define-core-transformer (assert-signature input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?assert-signature ?expr)
       (begin
	 (unless (syntax-object.type-signature? ?assert-signature)
	   (__synner__ "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #f #f)))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define-core-transformer (assert-signature-and-return input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?assert-signature ?expr)
       (begin
	 (unless (syntax-object.type-signature? ?assert-signature)
	   (__synner__ "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #t #f)))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define-core-transformer (cast-signature input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?assert-signature ?expr)
       (begin
	 (unless (syntax-object.type-signature? ?assert-signature)
	   (__synner__ "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #t #t)))
      (_
       (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

  (define (%process-expression caller-who input-form.stx lexenv.run lexenv.expand
			       asrt.stx expr.stx return-values? cast-signature?)
    (let* ((asrt.stx	(syntax-unwrap asrt.stx))
	   (asrt.sig	(make-type-signature
			 (syntax-object->type-signature-specs
			  asrt.stx lexenv.run (lambda (message subform)
						(raise
						 (condition (make-who-condition caller-who)
							    (make-message-condition message)
							    (make-syntax-violation input-form.stx #f)
							    (make-syntax-violation asrt.stx subform)))))))
	   (asrt.sig	(type-signature.untyped-to-top asrt.sig))
	   (expr.psi	(chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.sig	(psi.retvals-signature expr.psi)))
      (define (%error-mismatching-signatures)
	(raise
	 (condition (make-expand-time-type-signature-violation)
		    (make-who-condition caller-who)
		    (make-message-condition "expand-time return values signature mismatch")
		    (make-syntax-violation input-form.stx expr.stx)
		    (make-expected-type-signature-condition asrt.sig)
		    (make-returned-type-signature-condition expr.sig))))
      (if (options::typed-language-enabled?)
	  (cond ((and (type-signature.empty? asrt.sig)
		      (type-signature.empty? expr.sig))
		 ;;The  uncommon case  of empty  signatures.  The  expression returns
		 ;;zero  values  and the  assertion  expects  zero values.   We  just
		 ;;evaluate the expression.
		 (%just-evaluate-the-expression asrt.sig expr.psi return-values? cast-signature?))

		((type-signature.no-return? asrt.sig)
		 (syntax-violation caller-who
		   "internal error, invalid <bottom> assertion signature"
		   input-form.stx asrt.stx))

		((type-signature.no-return? expr.sig)
		 (%just-evaluate-the-expression asrt.sig expr.psi return-values? cast-signature?))

		((type-signature.fully-unspecified? asrt.sig)
		 ;;The assertion's signature always matches expression's signature.
		 (%just-evaluate-the-expression asrt.sig expr.psi return-values? cast-signature?))

		((type-signature.fully-unspecified? expr.sig)
		 ;;When  the assertion's  signature  has types  and the  expression's
		 ;;signature is unspecified: always do a run-time validation.
		 (%run-time-validation input-form.stx lexenv.run lexenv.expand
				       caller-who asrt.stx asrt.sig expr.psi
				       return-values? cast-signature?))

		(else
		 (case (type-signature.match-formals-against-operands asrt.sig expr.sig)
		   ((exact-match)
		    ;;Good.  Everything is all right  at expand-time.  We replace the
		    ;;expression's type  signature with the asserted  type signature:
		    ;;yes,  this   is  really   useful,  especially  with   RHS  type
		    ;;propagation and mutable variables.
		    (%just-evaluate-the-expression asrt.sig expr.psi return-values? cast-signature?))

		   ((possible-match)
		    ;;Compatible signatures, let's check the values at run-time.
		    (%run-time-validation input-form.stx lexenv.run lexenv.expand
					  caller-who asrt.stx asrt.sig expr.psi
					  return-values? cast-signature?))

		   (else
		    (%error-mismatching-signatures)))))
	;;Just insert a run-time validation when using a non-typed language.
	(%run-time-validation input-form.stx lexenv.run lexenv.expand
			      caller-who asrt.stx asrt.sig expr.psi
			      return-values? cast-signature?))))

;;; --------------------------------------------------------------------

  (define (%just-evaluate-the-expression asrt.sig expr.psi return-values? cast-signature?)
    ;;This  function  is  used  when  it   is  determined  at  expand-time  that  the
    ;;expression's signature matches the asserted signature.
    ;;
    ;;When we  do not need to  return the actual  values, we return the  void object;
    ;;returning a single  value is faster and, sometimes, returning  void might allow
    ;;detection of unwanted use of return values.
    ;;
    (let ((output.psi (if cast-signature?
			  (make-psi (psi.input-form expr.psi)
			    (psi.core-expr expr.psi)
			    asrt.sig)
			expr.psi)))
      (if return-values?
	  output.psi
	(make-psi (psi.input-form output.psi)
	  (build-sequence no-source
	    (list (psi.core-expr output.psi)
		  (build-void)))
	  (make-type-signature/single-void)))))

;;; --------------------------------------------------------------------

  (module (%run-time-validation)
    ;;Read the documentation for some examples of run-time validation.
    ;;
    (define* (%run-time-validation input-form.stx lexenv.run lexenv.expand
				   caller-who asrt.stx {asrt.sig type-signature?} {expr.psi psi?}
				   return-values? cast-signature?)
      (define asrt.specs
	(type-signature.object-type-specs asrt.sig))
      (define-values (consumer-formals.sexp has-rest?)
	(%compose-validator-formals asrt.specs))
      (define number-of-validation-forms 0)
      (define validating-form*.sexp
	(let recur ((asrt.specs			asrt.specs)
		    (consumer-formals.sexp	consumer-formals.sexp)
		    (operand-index		1))
	  (cond ((pair? asrt.specs)
		 (let ((asrt.ots (car asrt.specs)))
		   (if (or (<top>-ots?       asrt.ots)
			   (<untyped>-ots?   asrt.ots)
			   (<bottom>-ots? asrt.ots))
		       ;;No validation.
		       (let ((validators (recur (cdr asrt.specs) (cdr consumer-formals.sexp) (fxadd1 operand-index))))
			 (if return-values?
			     (cons (car consumer-formals.sexp) validators)
			   validators))
		     (begin
		       (set! number-of-validation-forms (fxadd1 number-of-validation-forms))
		       (let ((validator.stx (object-type-spec.single-value-validator-lambda-stx asrt.ots return-values?)))
			 (cons `(,validator.stx ,(car consumer-formals.sexp) ,operand-index __who__)
			       (recur (cdr asrt.specs) (cdr consumer-formals.sexp) (fxadd1 operand-index))))))))

		((null? asrt.specs)
		 '())

		((<list>-ots? asrt.specs)
		 ;;There is a  rest argument, but its type is  "<list>".  Any list of
		 ;;values will match, so we generate no validator.
		 '())

		((<bottom>-ots? asrt.specs)
		 ;;There is a rest argument, but its type is "<bottom>".  Any list
		 ;;of values will match, so we generate no validator.
		 '())

		((or (list-of-type-spec? asrt.specs)
		     (pair-type-spec?    asrt.specs)
		     (<nelist>-ots?      asrt.specs))
		 (set! number-of-validation-forms (fxadd1 number-of-validation-forms))
		 (let ((validator.stx (object-type-spec.list-validator-lambda-stx asrt.specs return-values?)))
		   `(,validator.stx ,consumer-formals.sexp ,operand-index __who__)))

		(else
		 (assertion-violation caller-who
		   "internal error, invalid assertion signature"
		   input-form.stx asrt.stx asrt.sig)))))
      (case number-of-validation-forms
	((0)
	 ;;No  validation forms,  so  just  evaluate the  expression.   To check  the
	 ;;correct number of return values: we rely on the compiler-generated code.
	 expr.psi)
	((1)
	 ;;Generate a directly applied LAMBDA form.
	 (let* ((consumer-body.sexp	(car validating-form*.sexp))
		(consumer.stx		(bless `(lambda/std ,consumer-formals.sexp ,consumer-body.sexp)))
		;;We want "__who__" to be bound in the consumer expression.
		(consumer.psi		(let* ((id    (core-prim-id '__who__))
					       (label (id->label id))
					       (descr (label->syntactic-binding-descriptor label lexenv.run)))
					  (case (syntactic-binding-descriptor.type descr)
					    ((displaced-lexical)
					     ;;__who__ is unbound.
					     (receive (lexenv.run lexenv.expand)
						 (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand
										   __who__ (core-prim-id caller-who))
					       (chi-expr consumer.stx lexenv.run lexenv.expand)))
					    (else
					     ;;__who__ is bound.
					     (chi-expr consumer.stx lexenv.run lexenv.expand)))))
		(consumer.core		(psi.core-expr consumer.psi))
		(output-signature	(if return-values?
					    (if cast-signature? asrt.sig (psi.retvals-signature expr.psi))
					  (make-type-signature/single-void))))
	   (make-psi input-form.stx
	     (build-application no-source
		 consumer.core
	       (list (psi.core-expr expr.psi)))
	     output-signature)))
	(else
	 ;;Generate the full CALL-WITH-VALUES output form.
	 (let* ((producer.core		(build-lambda no-source '() (psi.core-expr expr.psi)))
		(consumer-body.sexp	(if return-values?
					    (cond (has-rest?
						   `(apply values . ,validating-form*.sexp))
						  ((list-of-single-item? validating-form*.sexp)
						   (car validating-form*.sexp))
						  (else
						   `(values . ,validating-form*.sexp)))
					  `(begin ,@validating-form*.sexp (void))))
		(consumer.stx		(bless `(lambda/std ,consumer-formals.sexp ,consumer-body.sexp)))
		;;We want "__who__" to be bound in the consumer expression.
		(consumer.psi		(let* ((id    (core-prim-id '__who__))
					       (label (id->label id))
					       (descr (label->syntactic-binding-descriptor label lexenv.run)))
					  (case (syntactic-binding-descriptor.type descr)
					    ((displaced-lexical)
					     ;;__who__ is unbound.
					     (receive (lexenv.run lexenv.expand)
						 (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand
										   __who__ (core-prim-id caller-who))
					       (chi-expr consumer.stx lexenv.run lexenv.expand)))
					    (else
					     ;;__who__ is bound.
					     (chi-expr consumer.stx lexenv.run lexenv.expand)))))
		(consumer.core		(psi.core-expr consumer.psi))
		(output-signature	(if return-values?
					    (if cast-signature? asrt.sig (psi.retvals-signature expr.psi))
					  (make-type-signature/single-void))))
	   (make-psi input-form.stx
	     (build-application no-source
		 (build-primref no-source 'call-with-values)
	       (list producer.core consumer.core))
	     output-signature)))))

    (define (%compose-validator-formals asrt.specs)
      ;;Build  and return  a  proper or  improper list  of  gensyms representing  the
      ;;validator LAMBDA formals.
      ;;
      (define has-rest? #f)
      (define formals
	(let recur ((operand-index	1)
		    (asrt.specs		asrt.specs))
	  (cond ((pair? asrt.specs)
		 (cons (gensym (string-append "arg" (number->string operand-index)))
		       (recur (fxadd1 operand-index) (cdr asrt.specs))))
		((null? asrt.specs)
		 '())
		(else
		 (set! has-rest? #t)
		 (gensym (string-append "arg" (number->string operand-index)))))))
      (values formals has-rest?))

    #| end of module: %RUN-TIME-VALIDATION |# )

  #| end of module: ASSERT-SIGNATURE-TRANSFORMER |# )


;;;; module core-macro-transformer: UNSAFE-CAST-SIGNATURE

(define-core-transformer (unsafe-cast-signature input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's UNSAFE-CAST-SIGNATURE syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?target-signature ?expr)
     (let* ((target.sig		(make-type-signature
				 (syntax-object->type-signature-specs
				  ?target-signature lexenv.run
				  (lambda (message subform)
				    (raise
				     (condition (make-who-condition __who__)
						(make-message-condition message)
						(make-syntax-violation input-form.stx #f)
						(make-syntax-violation ?target-signature subform)))))))
	    (target.sig		(type-signature.untyped-to-top target.sig))
	    (expr.psi		(chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core		(psi.core-expr expr.psi))
	    (expr.sig		(psi.retvals-signature expr.psi)))
       (define (%do-unsafe-cast-signature)
	 (make-psi input-form.stx expr.core target.sig))
       (case (type-signature.match-formals-against-operands target.sig expr.sig)
	 ((exact-match)
	  ;;Good,   matching  type   signatures:   we  are   generalising  the   type
	  ;;specification.  For example:
	  ;;
	  ;;   (unsafe-cast-signature (<number>) 123)
	  ;;
	  ;;which generalises from "<positive-fixnum>" to "<number>".
	  (%do-unsafe-cast-signature))
	 ((possible-match)
	  ;;Good, non-matching  but compatible  type signatures: we  are specialising
	  ;;the type specification.  For example:
	  ;;
	  ;;   (define ({fun <number>})
	  ;;     123)
	  ;;   (unsafe-cast-signature (<fixnum>) (fun))
	  ;;
	  ;;which specialises from "<number>" to "<fixnum>".
	  (%do-unsafe-cast-signature))
	 (else
	  ;;Bad, non-matching and incompatible signatures.  For example:
	  ;;
	  ;;   (unsafe-cast-signature (<fixnum>) "ciao")
	  ;;
	  (raise
	   (condition (make-who-condition __who__)
		      (make-message-condition "source expression's signature is incompatible with the requested target signature")
		      (make-syntax-violation input-form.stx ?expr)
		      (make-irritants-condition (list target.sig expr.sig))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer: HASH-FUNCTION, EQUALITY-PREDICATE, COMPARISON-PROCEDURE

(define-core-transformer (equality-predicate input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's EQUALITY-PREDICATE  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run ots)
       (cond ((object-type-spec.equality-predicate ots)
	      => (lambda (stx)
		   (chi-expr stx lexenv.run lexenv.expand)))
	     (else
	      (__synner__ "type annotation has no equality predicate")))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (comparison-procedure input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's COMPARISON-PROCEDURE  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run ots)
       (cond ((object-type-spec.comparison-procedure ots)
	      => (lambda (stx)
		   (chi-expr stx lexenv.run lexenv.expand)))
	     (else
	      (__synner__ "type annotation has no comparison procedure")))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (hash-function input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's HASH-FUNCTION  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run ots)
       (cond ((object-type-spec.hash-function ots)
	      => (lambda (stx)
		   (chi-expr stx lexenv.run lexenv.expand)))
	     (else
	      (__synner__ "type annotation has no hash function")))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer: TYPE-OF

(define-core-transformer (type-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's TYPE-OF syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sig (psi.retvals-signature expr.psi)))
       (make-psi input-form.stx
	 (build-data no-source
	   expr.sig)
	 (make-type-signature/single-value (core-prim-spec '<type-signature> lexenv.run)))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer:
;;
;; TYPE-ANNOTATION=?
;; TYPE-ANNOTATION-SUPER-AND-SUB?
;; TYPE-ANNOTATION-COMMON-ANCESTOR
;; TYPE-ANNOTATION-ANCESTORS
;; TYPE-ANNOTATION-SYNTAX
;; TYPE-ANNOTATION-MATCHING
;;

(define-core-transformer (type-annotation=? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's TYPE-ANNOTATION=?  syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation1 ?type-annotation2)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation1 lexenv.run one.ots)
       (with-object-type-syntactic-binding (input-form.stx ?type-annotation2 lexenv.run two.ots)
	 (let ((bool (object-type-spec=? one.ots two.ots)))
	   (make-psi input-form.stx
	     (build-data no-source bool)
	     (if bool
		 (make-type-signature/single-true)
	       (make-type-signature/single-false)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-annotation-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  TYPE-ANNOTATION-SUPER-AND-SUB?
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type-annotation ?sub-type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?super-type-annotation lexenv.run super.ots)
       (with-object-type-syntactic-binding (input-form.stx ?sub-type-annotation lexenv.run sub.ots)
	 (let ((bool (object-type-spec.matching-super-and-sub? super.ots sub.ots)))
	   (make-psi input-form.stx
	     (build-data no-source bool)
	     (if bool
		 (make-type-signature/single-true)
	       (make-type-signature/single-false)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-annotation-common-ancestor input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to expand  Vicare's  TYPE-ANNOTATION-COMMON-ANCESTOR
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type-annotation ?sub-type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?super-type-annotation lexenv.run super.ots)
       (with-object-type-syntactic-binding (input-form.stx ?sub-type-annotation lexenv.run sub.ots)
	 (let ((ots (object-type-spec.common-ancestor super.ots sub.ots)))
	   (make-psi input-form.stx
	     (build-data no-source
	       (object-type-spec.name ots))
	     (make-type-signature/single-top))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-annotation-ancestors input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  TYPE-ANNOTATION-ANCESTORS syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run type.ots)
       (let ((name* (map object-type-spec.name (object-type-spec.ancestor-ots* type.ots))))
	 (make-psi input-form.stx
	   (build-data no-source name*)
	   (make-type-signature/single-list)))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-annotation-syntax input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's TYPE-ANNOTATION-SYNTAX syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run type.ots)
       (make-psi input-form.stx
	 (build-data no-source (object-type-spec.type-annotation type.ots))
	 (make-type-signature/single-top))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-annotation-matching input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's  TYPE-ANNOTATION-MATCHING syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type-annotation ?sub-type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?super-type-annotation lexenv.run super.ots)
       (with-object-type-syntactic-binding (input-form.stx ?sub-type-annotation lexenv.run sub.ots)
	 (let ((retval.sym (cond ((object-type-spec.matching-super-and-sub? super.ots sub.ots)
				  'exact-match)
				 ((object-type-spec.compatible-super-and-sub? super.ots sub.ots)
				  'possible-match)
				 (else
				  'no-match))))
	   (make-psi input-form.stx
	     (build-data no-source retval.sym)
	     (make-type-signature/single-value (make-enumeration-type-spec (list retval.sym))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer:
;;
;; TYPE-SIGNATURE-SUPER-AND-SUB?
;; TYPE-SIGNATURE-MATCHING
;; TYPE-SIGNATURE-UNION
;; TYPE-SIGNATURE-COMMON-ANCESTOR
;;

(define-core-transformer (type-signature-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used   to  expand  Vicare's  TYPE-SIGNATURE-SUPER-AND-SUB?
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-signature ?sub-signature)
     (let* ((super.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid super signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?super-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?super-signature lexenv.run synner))))
	    (sub.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid sub signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?sub-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?sub-signature lexenv.run synner))))
	    (bool	(type-signature.matching-super-and-sub? super.sig sub.sig)))
       (make-psi input-form.stx
	 (build-data no-source bool)
	 (if bool
	     (make-type-signature/single-true)
	   (make-type-signature/single-false)))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-signature-matching input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's TYPE-SIGNATURE-MATCHING  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-signature ?sub-signature)
     (let* ((super.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid super signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?super-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?super-signature lexenv.run synner))))
	    (sub.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid sub signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?sub-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?sub-signature lexenv.run synner))))
	    (sym	(type-signature.match-formals-against-operands super.sig sub.sig)))
       (make-psi input-form.stx
	 (build-data no-source sym)
	 (make-type-signature/single-value (make-enumeration-type-spec (list sym))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-signature-union input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  TYPE-SIGNATURE-UNION syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (define (%signature-union-synner message cnd)
    (raise
     (condition (make-who-condition 'type-signature-union)
		(make-message-condition message)
		(make-syntax-violation input-form.stx #f)
		cnd)))
  (syntax-match input-form.stx ()
    ((_ ?signature* ...)
     (make-psi input-form.stx
       (build-data no-source
	 (type-signature.syntax-object
	  (apply type-signature.union-same-number-of-operands
		 %signature-union-synner
		 (map (lambda (signature.stx)
			(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid type signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation signature.stx subform))))))
			  (make-type-signature (syntax-object->type-signature-specs signature.stx lexenv.run synner))))
		   ?signature*))))
       (make-type-signature/single-top)))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-signature-common-ancestor input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  TYPE-SIGNATURE-COMMON-ANCESTOR
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-signature ?sub-signature)
     (let* ((super.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid super signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?super-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?super-signature lexenv.run synner))))
	    (sub.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid sub signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?sub-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?sub-signature lexenv.run synner))))
	    (sig	(type-signature.common-ancestor super.sig sub.sig)))
       (make-psi input-form.stx
	 (build-data no-source
	   (type-signature.syntax-object sig))
	 (make-type-signature/single-top))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer:
;;
;; TYPE-DESCRIPTOR=?
;; TYPE-DESCRIPTOR-PARENT
;; TYPE-DESCRIPTOR-ANCESTORS
;; TYPE-DESCRIPTOR-SUPER-AND-SUB?
;; TYPE-DESCRIPTOR-MATCHING
;;

(define-core-transformer (type-descriptor-parent input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's TYPE-DESCRIPTOR-PARENT syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run type.ots)
       (let ((des.core-expr (object-type-spec.type-descriptor-core-expr type.ots)))
	 (make-psi input-form.stx
	   (build-application no-source
	       (build-primref no-source 'object-type-descr.parent)
	     (list des.core-expr))
	   ;;The return value can be false or an instance of "<type-descriptor>".
	   (make-type-signature/single-value
	    (make-union-type-spec (list (<false>-ots) (core-prim-spec '<type-descriptor> lexenv.run))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-descriptor-ancestors input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  TYPE-DESCRIPTOR-ANCESTORS syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?type-annotation lexenv.run type.ots)
       (let ((des.core-expr (object-type-spec.type-descriptor-core-expr type.ots)))
	 (make-psi input-form.stx
	   (build-application no-source
	       (build-primref no-source 'object-type-descr.ancestor-des*)
	     (list des.core-expr))
	   (make-type-signature/single-value (make-list-of-type-spec (core-prim-spec '<type-descriptor> lexenv.run)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-descriptor=? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's TYPE-DESCRIPTOR=?  syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?one-type-annotation ?two-type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?one-type-annotation lexenv.run one.ots)
       (with-object-type-syntactic-binding (input-form.stx ?two-type-annotation lexenv.run two.ots)
	 (let ((one-des.core-expr	(object-type-spec.type-descriptor-core-expr one.ots))
	       (two-des.core-expr	(object-type-spec.type-descriptor-core-expr two.ots)))
	   (make-psi input-form.stx
	     (build-application no-source
		 (build-primref no-source 'object-type-descr=?)
	       (list one-des.core-expr two-des.core-expr))
	     (make-type-signature/single-boolean))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-descriptor-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  TYPE-DESCRIPTOR-SUPER-AND-SUB?
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type-annotation ?sub-type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?super-type-annotation lexenv.run super.ots)
       (with-object-type-syntactic-binding (input-form.stx ?sub-type-annotation lexenv.run sub.ots)
	 (let ((super-des.core-expr	(object-type-spec.type-descriptor-core-expr super.ots))
	       (sub-des.core-expr	(object-type-spec.type-descriptor-core-expr sub.ots)))
	   (make-psi input-form.stx
	     (build-application no-source
		 (build-primref no-source 'object-type-descr.matching-super-and-sub?)
	       (list super-des.core-expr sub-des.core-expr))
	     (make-type-signature/single-boolean))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-core-transformer (type-descriptor-matching input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's  TYPE-DESCRIPTOR-MATCHING syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type-annotation ?sub-type-annotation)
     (with-object-type-syntactic-binding (input-form.stx ?super-type-annotation lexenv.run super.ots)
       (with-object-type-syntactic-binding (input-form.stx ?sub-type-annotation lexenv.run sub.ots)
	 (let ((super-des.core-expr	(object-type-spec.type-descriptor-core-expr super.ots))
	       (sub-des.core-expr	(object-type-spec.type-descriptor-core-expr sub.ots)))
	   (make-psi input-form.stx
	     (build-application no-source
		 (build-primref no-source 'object-type-descr.matching-formal-and-operand)
	       (list super-des.core-expr sub-des.core-expr))
	     (make-type-signature/single-value (make-enumeration-type-spec '(exact-match possible-match no-match))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; module core-macro-transformer: descriptors-signature-matching

(define-core-transformer (descriptors-signature-matching input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  DESCRIPTORS-SIGNATURE-MATCHING
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-signature ?sub-signature)
     (let ((super.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid super signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?super-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?super-signature lexenv.run synner))))
	   (sub.sig	(let ((synner (lambda (message subform)
					(raise
					 (condition (make-who-condition __who__)
						    (make-message-condition
						     (string-append "invalid sub signature argument: " message))
						    (make-syntax-violation input-form.stx #f)
						    (make-syntax-violation ?sub-signature subform))))))
			  (make-type-signature (syntax-object->type-signature-specs ?sub-signature lexenv.run synner)))))
       (let ((super-des.core-expr	(type-signature.type-descriptor-core-expr super.sig))
	     (sub-des.core-expr		(type-signature.type-descriptor-core-expr sub.sig)))
	 (make-psi input-form.stx
	   (build-application no-source
	       (build-primref no-source 'descriptors-signature.match-formals-against-operands)
	     (list super-des.core-expr sub-des.core-expr))
	   (make-type-signature/single-value (make-enumeration-type-spec '(exact-match possible-match no-match)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
