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
	 slot-ref-transformer
	 slot-set!-transformer
	 method-call-transformer
	 unsafe-cast-signature-transformer
	 case-type-transformer
	 assert-signature-transformer
	 assert-signature-and-return-transformer
	 type-of-transformer
	 type-super-and-sub?-transformer
	 signature-super-and-sub?-transformer)


;;;; helpers

(define-syntax (with-object-type-syntactic-binding stx)
  (sys::syntax-case stx ()
    ((_ (?who ?input-form.stx ?type-annotation ?lexenv ?object-type-spec)
	. ?body)
     (and (sys::identifier? (sys::syntax ?who))
	  (sys::identifier? (sys::syntax ?input-form.stx))
	  (sys::identifier? (sys::syntax ?lexenv))
	  (sys::identifier? (sys::syntax ?object-type-spec)))
     (sys::syntax
      (let ((?object-type-spec (type-annotation->object-type-specification ?type-annotation ?lexenv ?type-annotation)))
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
       (identifier? ?type-name)
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-name lexenv.run type.ots)
	 (cond ((list-of-type-spec? type.ots)
		(%build-list-of-construction input-form.stx lexenv.run lexenv.expand type.ots ?rand*))

	       ((list-type-spec? type.ots)
		(assertion-violation __who__ "constructor of heterogeneous list type is not yet implemented" ?type-name))

	       ((vector-of-type-spec? type.ots)
		(assertion-violation __who__ "constructor of homogeneous vector type is not yet implemented" ?type-name))

	       ((vector-type-spec? type.ots)
		(assertion-violation __who__ "constructor of heterogeneous vector type is not yet implemented" ?type-name))

	       ((pair-of-type-spec? type.ots)
		(assertion-violation __who__ "constructor of homogeneous pair type is not yet implemented" ?type-name))

	       ((pair-type-spec? type.ots)
		(assertion-violation __who__ "constructor of heterogeneous pair type is not yet implemented" ?type-name))

	       ((closure-type-spec? type.ots)
		(%synner "cannot build a closure with NEW"))

	       ((object-type-spec.constructor-stx type.ots)
		=> (lambda (constructor.stx)
		     (if (boolean? constructor.stx)
			 (%build-object-with-validator input-form.stx lexenv.run lexenv.expand ?type-name ?rand*)
		       (%build-object-with-constructor input-form.stx lexenv.run lexenv.expand ?type-name constructor.stx ?rand*))))

	       (else
		(%synner "attempt to instantiate object-type with no constructor (abstract type?)" ?type-name)))))
      ))

;;; --------------------------------------------------------------------

  (define (%build-object-with-validator input-form.stx lexenv.run lexenv.expand type-name.id rand*.stx)
    ;;The type identifier TYPE-NAME.ID references an object-type specification having
    ;;no constructor, but requesting validation of a single operand.  For example:
    ;;
    ;;   (new <fixnum> 123)
    ;;
    ;;must expand to:
    ;;
    ;;   (assert-signature-and-return (<fixnum>) 123)
    ;;
    (chi-expr (bless
	       `(assert-signature-and-return (,type-name.id) (values . ,rand*.stx)))
	      lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (define (%build-object-with-constructor input-form.stx lexenv.run lexenv.expand type-name.id constructor.stx rand*.stx)
    ;;The type identifier TYPE-NAME.ID references an object-type specification having
    ;;a constructor.
    ;;
    (chi-expr (bless
	       `(assert-signature-and-return (,type-name.id) (,constructor.stx . ,rand*.stx)))
	      lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (module (%build-list-of-construction)
    ;;The type identifier given to NEW requests building a list of type "(list-of ?item-type)".
    ;;
    (define (%build-list-of-construction input-form.stx lexenv.run lexenv.expand list-type.ots rand*.stx)
      (if (null? rand*.stx)
	  ;;No operands, so we build an empty list: we just return null.
	  (make-psi input-form.stx
	    (build-data no-source '())
	    (make-type-signature/single-value (<null>-ots)))
	;;There is at least one operand.  We built an expression like:
	;;
	;;   (list ?rand ...)
	;;
	;;where each  ?RAND is: just  the expression itself,  if its type  matches at
	;;expand-time; a validator expression like:
	;;
	;;   ((lambda (arg)
	;;      (if (?type-pred arg)
	;; 	    arg
	;;        (procedure-signature-argument-violation ?type-name
	;; 	    "invalid object type" ?index (quote ?type-pred) arg)))
	;;    ?rand)
	;;
	;;if the operand's type needs to be validated at run-time.
	(let* ((item.ots	(list-of-type-spec.item-ots list-type.ots))
	       (rand*.core	(map (lambda (rand.stx rand.idx)
				       (%process-list-constructor-operand input-form.stx lexenv.run lexenv.expand
									  item.ots rand.stx rand.idx))
				  rand*.stx (%fxiota 0 rand*.stx))))
	  (make-psi input-form.stx
	    (build-application input-form.stx
		(build-primref no-source 'list)
	      (cons* rand*.core))
	    (make-type-signature/single-value list-type.ots)))))

    (define (%process-list-constructor-operand input-form.stx lexenv.run lexenv.expand
					       item.ots rand.stx rand.idx)
      (define rand.psi (chi-expr rand.stx lexenv.run lexenv.expand))
      (define rand.sig (psi.retvals-signature rand.psi))
      (define (%run-time-validation)
	(%build-type-run-time-validation input-form.stx lexenv.run lexenv.expand item.ots rand.psi rand.idx))
      (case-signature-specs rand.sig
	((<top>)
	 ;;Integrate a run-time validator for the single return value.
	 (%run-time-validation))

	((single-value)
	 => (lambda (operand.ots)
	      (if (object-type-spec.matching-super-and-sub? item.ots operand.ots)
		  ;;Success!!!  Just evaluate the operand's expression.
		  (psi.core-expr rand.psi)
		(raise
		 (condition
		  (make-expand-time-type-signature-violation)
		  (%make-signature-mismatch-condition "expand-time mismatch between list's item type and operand signature"
						      input-form.stx rand.stx rand.idx rand.sig item.ots))))))

	(<no-return>
	 (let ((common (%make-signature-mismatch-condition "expression used as list constructor operand is typed as not returning"
							   input-form.stx rand.stx rand.idx rand.sig item.ots)))
	   (if (options::typed-language?)
	       (raise (condition (make-expand-time-type-signature-violation) common))
	     (begin
	       (raise-continuable (condition (make-expand-time-type-signature-warning) common))
	       (%run-time-validation)))))

	(<list-of>
	 => (lambda (operand.ots)
	      (let ((operand-item.ots (list-of-type-spec.item-ots operand.ots)))
		(if (object-type-spec.matching-super-and-sub? item.ots operand-item.ots)
		    ;;We let  the integrated single-value  validation do the  rest of
		    ;;the job.
		    (psi.core-expr rand.psi)
		  (raise
		   (condition
		    (make-expand-time-type-signature-violation)
		    (%make-signature-mismatch-condition "expand-time mismatch between list's item type and operand signature"
							input-form.stx rand.stx rand.idx rand.sig item.ots)))))))

	(<list>
	 ;;The  retvals signature  is  fully unspecified.   We  integrate a  run-time
	 ;;validator  expecting a  single  return value.   We  use the  automatically
	 ;;generated machine code to validate the number of return values.
	 (%run-time-validation))

	(else
	 ;;The  horror!!!  We  have established  at expand-time  that the  expression
	 ;;returns multiple values; type violation.
	 (raise
	  (condition
	   (make-expand-time-type-signature-violation)
	   (%make-signature-mismatch-condition "the expression used as constructor's operand returns multiple values"
					       input-form.stx rand.stx rand.idx rand.sig item.ots))))))

    (define (%build-type-run-time-validation input-form.stx lexenv.run lexenv.expand
					     item.ots rand.psi rand.idx)
      ;;Build and return  a psi object representing an expression  which: at run-time
      ;;validates  the operand  RAND.PSI as  a single  value of  type ITEM.OTS;  when
      ;;successful returns the value itself.
      ;;
      (let* ((validator.stx	(let ((arg.sym		(gensym "arg"))
				      (pred.stx		(object-type-spec.type-predicate-stx item.ots))
				      (name.stx		(object-type-spec.name item.ots)))
				  (bless
				   `(lambda (,arg.sym)
				      (if (,pred.stx ,arg.sym)
					  ,arg.sym
					(procedure-signature-argument-violation (quote ,name.stx)
					  "invalid object type" ,rand.idx (quote (is-a? _ ,name.stx)) ,arg.sym))))))
	     (validator.psi	(chi-expr validator.stx lexenv.run lexenv.expand)))
	(build-application no-source
	    (psi.core-expr validator.psi)
	  (list (psi.core-expr rand.psi)))))

    (define* (%make-signature-mismatch-condition message input-form.stx rand.stx rand.idx
						 {rand.sig type-signature?} {item.ots object-type-spec?})
      (condition (make-who-condition __module_who__)
		 (make-message-condition message)
		 (make-syntax-violation input-form.stx rand.stx)
		 (make-application-argument-index-condition rand.idx)
		 (make-application-argument-type-name-condition (object-type-spec.name item.ots))
		 (make-application-operand-signature-condition rand.sig)))

    #| end of module: %BUILD-LIST-OF-CONSTRUCTION |# )

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
	 (case-signature-specs expr.sig
	   ((single-value)
	    => (lambda (type.ots)
		 (%apply-appropriate-destructor __who__ input-form.stx lexenv.run lexenv.expand type.ots expr.psi)))
	   (<no-return>
	    (%synner "subject expression of destructor syntax defined to no return" ?expr))
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
      ))

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
       (chi-expr (with-object-type-syntactic-binding (__who__ input-form.stx ?type-annotation lexenv.run type-annotation.ots)
		   (or (object-type-spec.type-predicate-stx type-annotation.ots)
		       (%synner "type annotation has no predicate for run-time use" ?type-annotation)))
		 lexenv.run lexenv.expand))

      ((_ ?expr ?type-annotation)
       (with-object-type-syntactic-binding (__module_who__ input-form.stx ?type-annotation lexenv.run type-annotation.ots)
	 (%expand-to-single-value-predicate input-form.stx lexenv.run lexenv.expand
					    ?expr ?type-annotation type-annotation.ots %synner)))
      ))

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
	(case-signature-specs expr.sig
	  ((single-value)
	   => (lambda (expr.ots)
		(%match-pred-type-against-single-value-expr input-form.stx lexenv.run lexenv.expand
							    expr.psi expr.ots type-annotation.ots %run-time-predicate)))

	  (<no-return>
	   (let ((common (condition
			  (make-who-condition __module_who__)
			  (make-message-condition "subject expression typed as not returning")
			  (make-syntax-violation input-form.stx expr.stx))))
	     (if (options::typed-language?)
		 (raise (condition (make-expand-time-type-signature-violation) common))
	       (begin
		 (raise-continuable (condition (make-expand-time-type-signature-warning) common))
		 (%run-time-predicate)))))

	  (<list>
	   ;;The expression  returns an unspecified  number of values  of UNspecified
	   ;;type.
	   (%run-time-predicate))

	  (<list-of>
	   => (lambda (expr.ots)
		(%match-pred-type-against-list-of input-form.stx lexenv.run lexenv.expand
						  expr.psi expr.ots type-annotation.ots %run-time-predicate)))

	  (else
	   (let ((common (condition
			  (make-who-condition __module_who__)
			  (make-message-condition "subject expression returns multiple values")
			  (make-syntax-violation input-form.stx expr.stx))))
	     (if (options::typed-language?)
		 (raise (condition (make-expand-time-type-signature-violation) common))
	       (begin
		 (raise-continuable (condition (make-expand-time-type-signature-warning) common))
		 (%run-time-predicate))))))))

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

	    ((object-type-spec.matching-super-and-sub? expr.ots type-annotation.ots)
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

	      ((object-type-spec.matching-super-and-sub? item.ots type-annotation.ots)
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


;;;; module core-macro-transformer: SLOT-REF

(module (slot-ref-transformer)

  (define-module-who slot-ref)

  (define-core-transformer (slot-ref input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function  used  to  expand Vicare's  SLOT-REF  syntaxes  from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ;;Everything included.  It must expand to an accessor application.
      ((_ ?expr ?field-name ?type-id)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name)
	    (identifier? ?type-id))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run type.ots)
	 (%expand-to-object-accessor-application input-form.stx lexenv.run lexenv.expand type.ots ?expr ?field-name)))

      ;;Wildcard in place of the subject expression.  It must expand to an expression
      ;;that evaluates to an accessor.
      ((_ ?jolly ?field-name ?type-id)
       (and (identifier? ?type-id)
	    (identifier? ?field-name)
	    (underscore-id? ?jolly))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run type.ots)
	 (%expand-to-object-accessor input-form.stx lexenv.run lexenv.expand type.ots ?field-name)))

      ;;Missing type identifier.  Try to retrieve  the type from the signature of the
      ;;subject expression.
      ((_ ?expr ?field-name)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name))
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi.retvals-signature expr.psi)))
	 (define (%error-unknown-type)
	   (%synner "unable to determine type of expression at expand-time" ?expr))
	 (case-signature-specs expr.sig
	   ((<top>)
	    (%error-unknown-type))
	   ((single-value)
	    => (lambda (type.ots)
		 (%expand-to-object-accessor-application-post input-form.stx lexenv.run lexenv.expand
							      type.ots expr.psi ?field-name)))
	   (<no-return>
	    (%synner "subject expression of field accessor syntax defined to no return" ?expr))
	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature.
	    (%error-unknown-type))
	   (else
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (raise
	     (condition (make-who-condition __who__)
			(make-message-condition "subject expression of slot access returns multiple values")
			(make-syntax-violation input-form.stx ?expr)
			(make-irritants-condition (list expr.sig))))))))
      ))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-accessor-application input-form.stx lexenv.run lexenv.expand
						   {type.ots object-type-spec?} {expr.stx syntax-object?}
						   field-name.id)
    (cond ((object-type-spec.safe-accessor-stx type.ots (identifier->symbol field-name.id))
	   => (lambda (accessor.stx)
		(chi-expr `(,accessor.stx ,expr.stx) lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-accessor input-form.stx lexenv.run lexenv.expand
				       {type.ots object-type-spec?} field-name.id)
    (cond ((object-type-spec.safe-accessor-stx type.ots (identifier->symbol field-name.id))
	   => (lambda (accessor.stx)
		(chi-expr accessor.stx lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-accessor-application-post input-form.stx lexenv.run lexenv.expand
							{type.ots object-type-spec?} {expr.psi psi?} field-name.id)
    (cond ((object-type-spec.safe-accessor-stx type.ots (identifier->symbol field-name.id))
	   => (lambda (accessor.stx)
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   accessor.stx expr.psi '())))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

  #| end of module: SLOT-REF-TRANSFORMER |# )


;;;; module core-macro-transformer: SLOT-SET!

(module (slot-set!-transformer)

  (define-module-who slot-set!)

  (define-core-transformer (slot-set! input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to  expand Vicare's  SLOT-SET!   syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ;;Everything included.  It must expand to an mutator application.
      ((_ ?expr ?field-name ?type-id ?new-value)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name)
	    (identifier? ?type-id))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run type.ots)
	 (%expand-to-object-mutator-application input-form.stx lexenv.run lexenv.expand type.ots ?expr ?field-name ?new-value)))

      ;;Wildcard in place of the subject expression.  It must expand to an expression
      ;;that evaluates to an mutator.
      ((_ ?jolly1 ?field-name ?type-id ?jolly2)
       (and (identifier? ?type-id)
	    (identifier? ?field-name)
	    (underscore-id? ?jolly1)
	    (underscore-id? ?jolly2))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run type.ots)
	 (%expand-to-object-mutator input-form.stx lexenv.run lexenv.expand type.ots ?field-name)))

      ;;Missing type identifier.  Try to retrieve  the type from the signature of the
      ;;subject expression.
      ((_ ?expr ?field-name ?new-value)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name))
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi.retvals-signature expr.psi)))
	 (define (%error-unknown-type)
	   (%synner "unable to determine type of expression at expand-time" ?expr))
	 (case-signature-specs expr.sig
	   ((<top>)
	    (%error-unknown-type))
	   ((single-value)
	    => (lambda (type.ots)
		 (%expand-to-object-mutator-application-post input-form.stx lexenv.run lexenv.expand
							     type.ots expr.psi ?field-name ?new-value)))
	   (<no-return>
	    (%synner "subject expression of field mutator syntax defined to no return" ?expr))
	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature.
	    (%error-unknown-type))
	   (else
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (raise
	     (condition (make-who-condition __who__)
			(make-message-condition "subject expression of slot access returns multiple values")
			(make-syntax-violation input-form.stx ?expr)
			(make-irritants-condition (list expr.sig))))))))
      ))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-mutator-application input-form.stx lexenv.run lexenv.expand
						  {type.ots object-type-spec?} {expr.stx syntax-object?}
						  field-name.id new-value.stx)
    (let ((mutator.stx (%retrieve-mutator-stx input-form.stx lexenv.run lexenv.expand type.ots field-name.id)))
      (chi-expr `(,mutator.stx ,expr.stx ,new-value.stx) lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define (%expand-to-object-mutator input-form.stx lexenv.run lexenv.expand
				     type.ots field-name.id)
    (let ((mutator.stx (%retrieve-mutator-stx input-form.stx lexenv.run lexenv.expand type.ots field-name.id)))
      (chi-expr mutator.stx lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-mutator-application-post input-form.stx lexenv.run lexenv.expand
						       {type.ots object-type-spec?} {expr.psi psi?}
						       field-name.id new-value.stx)
    (let ((mutator.stx (%retrieve-mutator-stx input-form.stx lexenv.run lexenv.expand type.ots field-name.id)))
      (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					 mutator.stx expr.psi (list new-value.stx))))

;;; --------------------------------------------------------------------

  (define (%retrieve-mutator-stx input-form.stx lexenv.run lexenv.expand
				 type.ots field-name.id)
    (define (%error message)
      (syntax-violation __module_who__ message input-form.stx field-name.id))
    (cond ((object-type-spec.safe-mutator-stx type.ots (identifier->symbol field-name.id))
	   => (lambda (mutator.stx)
		(if (boolean? mutator.stx)
		    (%error "attempt to mutate immutable field")
		  mutator.stx)))
	  (else
	   (%error "unknown field name"))))

  #| end of module: SLOT-SET!-TRANSFORMER |# )


;;;; module core-macro-transformer: METHOD-CALL

(module (method-call-transformer)

  (define-module-who method-call)

  (define-core-transformer (method-call input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to expand  Vicare's METHOD-CALL  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?method-name ?subject-expr ?arg* ...)
       (identifier? ?method-name)
       (let* ((method-name.sym	(identifier->symbol ?method-name))
	      (subject-expr.psi	(chi-expr ?subject-expr lexenv.run lexenv.expand))
	      (subject-expr.sig	(psi.retvals-signature subject-expr.psi)))
	 (define-syntax-rule (%late-binding)
	   (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
						method-name.sym subject-expr.psi ?arg*))
	 (define (%error message)
	   (raise
	    (condition (make-who-condition __module_who__)
		       (make-message-condition message)
		       (make-syntax-violation input-form.stx ?subject-expr)
		       (make-irritants-condition (list subject-expr.sig)))))
	 (case-signature-specs subject-expr.sig
	   ((<top>)
	    (%late-binding))

	   ((single-value)
	    => (lambda (subject-expr.ots)
		 (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						       ?method-name method-name.sym
						       ?subject-expr subject-expr.psi subject-expr.ots
						       ?arg*)))

	   (<no-return>
	    (let ((common (condition
			   (make-who-condition __module_who__)
			   (make-message-condition "subject expression of method call defined to never return")
			   (make-syntax-violation input-form.stx ?subject-expr)
			   (make-irritants-condition (list subject-expr.sig)))))
	      (if (options::typed-language?)
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
      ))

;;; --------------------------------------------------------------------

  (define* (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						 method-name.id method-name.sym
						 subject-expr.stx subject-expr.psi subject-expr.ots
						 arg*.stx)
    (cond ((object-type-spec.applicable-method-stx subject-expr.ots method-name.sym)
	   ;;A matching method name exists.
	   => (lambda (method.stx)
		;;A matching method name exists.
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   method.stx subject-expr.psi arg*.stx)))

	  ((and (null? arg*.stx)
		(object-type-spec.safe-accessor-stx subject-expr.ots method-name.sym))
	   ;;No additional  arguments: the input form  has the correct syntax  for an
	   ;;field accessor application.  A matching field name exists.
	   => (lambda (accessor.stx)
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   accessor.stx subject-expr.psi arg*.stx)))

	  ((and (list-of-single-item? arg*.stx)
		(object-type-spec.safe-mutator-stx subject-expr.ots method-name.sym))
	   ;;A single additional argument: the input  form has the correct syntax for
	   ;;a field mutator application.  A matching mutable field name exists.
	   => (lambda (mutator.stx)
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   mutator.stx subject-expr.psi arg*.stx)))

	  (else
	   (raise
	    (condition (make-who-condition __module_who__)
		       (make-message-condition "unknown method name for type of subject expression")
		       (make-syntax-violation input-form.stx subject-expr.stx)
		       (make-application-operand-signature-condition (psi.retvals-signature subject-expr.psi))
		       (make-type-method-name-condition method-name.sym))))))

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
		 expr.core
		 arg*.core))
	(make-type-signature/fully-untyped))))

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
      ))

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
	  (psi-application-retvals-signature input-form.stx lexenv.run matcher.psi)))))

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

  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation 'case-type message input-form.stx subform)))

  (main))


;;;; module core-macro-transformer: ASSERT-SIGNATURE, ASSERT-SIGNATURE-AND-RETURN

(module (assert-signature-transformer assert-signature-and-return-transformer)
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
	   (%synner "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #f)))
      ))

  (define-core-transformer (assert-signature-and-return input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?assert-signature ?expr)
       (begin
	 (unless (syntax-object.type-signature? ?assert-signature)
	   (%synner "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #t)))
      ))

;;; --------------------------------------------------------------------

  (define (%process-expression caller-who input-form.stx lexenv.run lexenv.expand
			       asrt.stx expr.stx return-values?)
    (let* ((asrt.stx	(syntax-unwrap asrt.stx))
	   (asrt.sig	(make-type-signature asrt.stx))
	   (expr.psi	(chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.sig	(psi.retvals-signature expr.psi)))
      (define (%error-mismatching-signatures)
	(raise
	 (condition
	  (make-expand-time-type-signature-violation)
	  (make-who-condition caller-who)
	  (make-message-condition "expand-time return values signature mismatch")
	  (make-syntax-violation input-form.stx expr.stx)
	  (make-expand-time-retvals-signature-violation asrt.sig expr.sig))))
      (cond ((and (type-signature.empty? asrt.sig)
		  (type-signature.empty? expr.sig))
	     ;;The uncommon  case of empty  signatures.  The expression  returns zero
	     ;;values and  the assertion expects  zero values.  We just  evaluate the
	     ;;expression.
	     (%just-evaluate-the-expression expr.psi return-values?))

	    ((type-signature.fully-untyped? asrt.sig)
	     ;;The assertion's signature always matches expression's signature.
	     (%just-evaluate-the-expression expr.psi return-values?))

	    ((type-signature.fully-untyped? expr.sig)
	     ;;When  the  assertion's  signature   has  types  and  the  expression's
	     ;;signature is unspecified: always do a run-time validation.
	     (%run-time-validation caller-who input-form.stx lexenv.run lexenv.expand
				   asrt.stx asrt.sig expr.psi return-values?))

	    ((type-signature.no-return? expr.sig)
	     (%just-evaluate-the-expression expr.psi return-values?))

	    ((type-signature.super-and-sub? asrt.sig expr.sig)
	     ;;Good.  Everything is all right at expand-time.
	     (%just-evaluate-the-expression expr.psi return-values?))

	    ((type-signature.super-and-sub? expr.sig asrt.sig)
	     ;;Compatible signatures, let's check the values at run-time.
	     (%run-time-validation caller-who input-form.stx lexenv.run lexenv.expand
				   asrt.stx asrt.sig expr.psi return-values?))

	    (else
	     (%error-mismatching-signatures)))))

;;; --------------------------------------------------------------------

  (define (%just-evaluate-the-expression expr.psi return-values?)
    ;;This  function  is  used  when  it   is  determined  at  expand-time  that  the
    ;;expression's signature matches the asserted signature.
    ;;
    ;;When we  do not need to  return the actual  values, we return the  void object;
    ;;returning a single  value is faster and, sometimes, returning  void might allow
    ;;detection of unwanted use of return values.
    ;;
    (if return-values?
	expr.psi
      (make-psi (psi.input-form expr.psi)
	(build-sequence no-source
	  (list (psi.core-expr expr.psi)
		(build-void)))
	(make-type-signature/single-void))))

;;; --------------------------------------------------------------------

  (module (%run-time-validation)
    ;;Let's see some examples of run-time validation.  The input form:
    ;;
    ;;   (assert-signature-and-return (<fixnum> <flonum> <string>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda/std () ?expr)
    ;;     (lambda/std (arg0 arg1 arg2)
    ;;       (values (if (fixnum? arg0)
    ;;                   arg0
    ;;                 (expression-return-value-violation __who__ "return value of invalid type" 1 '(is-a? _ <fixnum>) arg0))
    ;;               (if (flonum? arg1)
    ;;                   arg1
    ;;                 (expression-return-value-violation __who__ "return value of invalid type" 2 '(is-a? _ <flonum>) arg1))
    ;;               (if (string? arg2)
    ;;                   arg2
    ;;                 (expression-return-value-violation __who__ "return value of invalid type" 3 '(is-a? _ <string>) arg2)))))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature (<fixnum> <flonum> <string>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda/std () ?expr)
    ;;     (lambda/std (arg0 arg1 arg2)
    ;;       (begin
    ;;         (if (fixnum? arg0)
    ;;              arg0
    ;;           (expression-return-value-violation __who__ "return value of invalid type" 1 '(is-a? _ <fixnum>) arg0))
    ;;         (if (flonum? arg1)
    ;;             arg1
    ;;           (expression-return-value-violation __who__ "return value of invalid type" 2 '(is-a? _ <flonum>) arg1))
    ;;         (if (string? arg2)
    ;;             arg2
    ;;           (expression-return-value-violation __who__ "return value of invalid type" 3 '(is-a? _ <string>) arg2))
    ;;         (void))))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature-and-return (list-of <fixnum>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda/std () ?expr)
    ;;     (lambda/std/std args
    ;;       (fold-left (lambda/std (idx obj)
    ;;                    (unless (fixnum? obj)
    ;;                      (expression-return-value-violation __who__ "return value of invalid type" idx '(is-a? _ <fixnum>) obj))
    ;;                    (fxadd1 idx))
    ;;         1 args)
    ;;       (apply values args)))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature-and-return (<fixnum> <flonum> . (list-of <fixnum>)) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda/std () ?expr)
    ;;     (lambda/std (arg0 arg1 . rest)
    ;;       (apply values
    ;;         (if (fixnum? arg0)
    ;;              arg0
    ;;           (expression-return-value-violation __who__ "return value of invalid type" 1 '(is-a? _ <fixnum>) arg0))
    ;;         (if (flonum? arg1)
    ;;             arg1
    ;;           (expression-return-value-violation __who__ "return value of invalid type" 2 '(is-a? _ <flonum>) arg1))
    ;;         (begin
    ;;           (fold-left (lambda/std (idx obj)
    ;;                        (unless (fixnum? obj)
    ;;                          (expression-return-value-violation __who__ "return value of invalid type" idx '(is-a? _ <fixnum>) obj))
    ;;                        (fxadd1 idx))
    ;;             3 rest)
    ;; 	         rest))))
    ;;
    (define* (%run-time-validation caller-who input-form.stx lexenv.run lexenv.expand
				   asrt.stx {asrt.sig type-signature?} {expr.psi psi?} return-values?)
      (define asrt.specs
	(type-signature.specs asrt.sig))
      (receive (validating-form*.sexp consumer-formals.sexp has-rest?)
	  (let recur ((asrt.stx		asrt.stx)
		      (asrt.specs	asrt.specs)
		      (idx		1))
	    (cond ((pair? asrt.stx)
		   (receive (validating-form*.sexp consumer-formals.sexp has-rest?)
		       (recur (cdr asrt.stx) (cdr asrt.specs) (fxadd1 idx))
		     (receive (validating-form.sexp consumer-formal.sym)
			 (%build-arg-validator caller-who input-form.stx lexenv.run lexenv.expand
					       (car asrt.specs) idx return-values?)
		       (values (cons validating-form.sexp validating-form*.sexp)
			       (cons consumer-formal.sym  consumer-formals.sexp)
			       has-rest?))))
		  ((null? asrt.stx)
		   (values '() '() #f))
		  ((<list>-ots? asrt.specs)
		   (let ((consumer-formal.sym	(gensym "rest"))
			 (has-rest?		#t))
		     (values (list consumer-formal.sym) consumer-formal.sym has-rest?)))
		  (else
		   ;;Here ASRT.STX is a standalone identifier, sub-type of "<list>".
		   (let* ((consumer-formal.sym  (gensym "rest"))
			  (validating-form.sexp (%build-rest-validator caller-who input-form.stx lexenv.run lexenv.expand
								       asrt.stx idx consumer-formal.sym return-values?))
			  (has-rest?		#t))
		     (values (list validating-form.sexp) consumer-formal.sym has-rest?)))))
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
					    asrt.sig
					  (make-type-signature/single-void))))
	  (make-psi input-form.stx
	    (build-application no-source
		(build-primref no-source 'call-with-values)
	      (list producer.core consumer.core))
	    output-signature))))

    (define (%build-arg-validator caller-who input-form.stx lexenv.run lexenv.expand
				  asrt.ots idx return-values?)
      (let ((consumer-formal.sym	(gensym (string-append "arg" (number->string idx))))
	    (type-name.stx		(object-type-spec.name                asrt.ots))
	    (typed-predicate.sexp	(object-type-spec.type-predicate-stx asrt.ots)))
	(define validating-form.sexp
	  (if return-values?
	      `(if (,typed-predicate.sexp ,consumer-formal.sym)
		   ,consumer-formal.sym
		 (expression-return-value-violation __who__
		   "return value of invalid type"
		   ,idx '(is-a? _ ,type-name.stx) ,consumer-formal.sym))
	    `(unless (,typed-predicate.sexp ,consumer-formal.sym)
	       (expression-return-value-violation __who__
		 "return value of invalid type"
		 ,idx '(is-a? _ ,type-name.stx) ,consumer-formal.sym))))
	(values validating-form.sexp consumer-formal.sym)))

    (define (%build-rest-validator caller-who input-form.stx lexenv.run lexenv.expand
				   asrt.id idx consumer-formal.sym return-values?)
      (let* ((asrt.ots			(id->object-type-specification caller-who input-form.stx asrt.id lexenv.run))
	     (item.ots			(list-of-type-spec.item-ots asrt.ots))
	     (typed-predicate.sexp	(object-type-spec.type-predicate-stx asrt.ots))
	     (idx.sym			(gensym "idx"))
	     (obj.sym			(gensym "obj")))
	(define validator.sexp
	  `(fold-left (lambda/std (,idx.sym ,obj.sym)
			(unless (,typed-predicate.sexp ,obj.sym)
			  (expression-return-value-violation __who__
			    "return value of invalid type"
			    ,idx.sym ,obj.sym (quote ,(object-type-spec.name item.ots))))
			(fxadd1 ,idx.sym))
	     ,idx ,consumer-formal.sym))
	(if return-values?
	    `(begin ,validator.sexp ,consumer-formal.sym)
	  validator.sexp)))

    #| end of module: %RUN-TIME-VALIDATION |# )

  #| end of module: ASSERT-SIGNATURE-TRANSFORMER |# )


;;;; module core-macro-transformer: UNSAFE-CAST-SIGNATURE

(define-core-transformer (unsafe-cast-signature input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  UNSAFE-CAST-SIGNATURE syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?target-signature ?expr)
     (begin
       (unless (syntax-object.type-signature? ?target-signature lexenv.run)
	 (syntax-violation __who__ "invalid type signature" input-form.stx ?target-signature))
       (let* ((target.sig	(make-type-signature ?target-signature))
	      (expr.psi		(chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.core	(psi.core-expr expr.psi))
	      (expr.sig		(psi.retvals-signature expr.psi)))
	 (define (%do-unsafe-cast-signature)
	   (make-psi input-form.stx expr.core target.sig))
	 (cond ((type-signature.super-and-sub? target.sig expr.sig)
		;;Good,  matching  type  signatures:   we  are  generalising  the  type
		;;specification.  For example:
		;;
		;;   (unsafe-cast-signature (<number>) 123)
		;;
		;;which generalises from "<fixnum>" to "<number>".
		(%do-unsafe-cast-signature))
	       ((type-signature.super-and-sub? expr.sig target.sig)
		;;Good,   non-matching  but   compatible   type   signatures:  we   are
		;;specialising the type specification.  For example:
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
			    (make-irritants-condition (list expr.sig))))))
	 )))
    ))


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
		 (make-type-signature/single-value (core-prim-id '<type-signature>)))))
    ))


;;;; module core-macro-transformer: TYPE-SUPER-AND-SUB?, SIGNATURE-SUPER-AND-SUB?

(define-core-transformer (type-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  TYPE-SUPER-AND-SUB?  syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type-annotation ?sub-type-annotation)
     (with-object-type-syntactic-binding (__who__ input-form.stx ?super-type-annotation lexenv.run super.ots)
       (with-object-type-syntactic-binding (__who__ input-form.stx ?sub-type-annotation lexenv.run sub.ots)
	 (let ((bool (object-type-spec.matching-super-and-sub? super.ots sub.ots)))
	   (make-psi input-form.stx
	     (build-data no-source bool)
	     (if bool
		 (make-type-signature/single-true)
	       (make-type-signature/single-false)))))))
    ))

(define-core-transformer (signature-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  SIGNATURE-SUPER-AND-SUB?  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-signature ?sub-signature)
     (begin
       (unless (syntax-object.type-signature? ?super-signature lexenv.run)
	 (syntax-violation __who__ "invalid super signature argument" input-form.stx ?super-signature))
       (unless (syntax-object.type-signature? ?sub-signature lexenv.run)
	 (syntax-violation __who__ "invalid sub signature argument"   input-form.stx ?sub-signature))
       (let* ((super.sig	(make-type-signature ?super-signature lexenv.run))
	      (sub.sig		(make-type-signature ?sub-signature   lexenv.run))
	      (bool		(type-signature.super-and-sub? super.sig sub.sig)))
	 (make-psi input-form.stx
	   (build-data no-source bool)
	   (if bool
	       (make-type-signature/single-true)
	     (make-type-signature/single-false))))))
    ))


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
