;;; -*- coding: utf-8-unix -*-
;;;
;;;Copyright (c) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
;;;; overloaded functions


(module (overloaded-function-spec
	 make-overloaded-function-spec	overloaded-function-spec?
	 overloaded-function-spec.name-id
	 overloaded-function-spec.signature*
	 overloaded-function-spec.id*
	 overloaded-function-spec.add-specialised-implementation!
	 overloaded-function-spec.register-specialisation!
	 overloaded-function-spec.expanded-expr
	 #| end of exports |# )


(define-struct (overloaded-function-spec %make-overloaded-function-spec overloaded-function-spec?)
  (name-id
		;A syntactic identifier representing the overloaded function name.
   signature*
		;Null or a proper list of "<lambda-signature>" instances representing
		;the specialised functions' signatures.
   id*
		;Null  or  a  proper  list  of syntactic  identifiers  bound  to  the
		;specialised functions.
   ))

(define-syntax-rule (overloaded-function-spec.name-id ofs)	(overloaded-function-spec-name-id    ofs))
(define-syntax-rule (overloaded-function-spec.signature* ofs)	(overloaded-function-spec-signature* ofs))
(define-syntax-rule (overloaded-function-spec.id* ofs)		(overloaded-function-spec-id*        ofs))

(define* (make-overloaded-function-spec {name.id identifier?})
  (%make-overloaded-function-spec name.id '() '()))

;;; --------------------------------------------------------------------

(define* (overloaded-function-spec.add-specialised-implementation! input-form.stx
								   {lhs.ofs overloaded-function-spec?}
								   {spec.lambda-sig lambda-signature?}
								   {spec.id identifier?})
  (let ((new-formals.sig (lambda-signature.argvals spec.lambda-sig)))
    (for-each (lambda (spec.lambda-sig)
		(when (type-signature=? (lambda-signature.argvals spec.lambda-sig) new-formals.sig)
		  (raise
		   (condition (make-who-condition __who__)
			      (make-message-condition "formals type signature already exists in overloaded function")
			      (syntax-violation input-form.stx #f)))))
      (overloaded-function-spec-signature* lhs.ofs)))
  (set-overloaded-function-spec-signature*! lhs.ofs (cons spec.lambda-sig (overloaded-function-spec-signature* lhs.ofs)))
  (set-overloaded-function-spec-id*!        lhs.ofs (cons spec.id         (overloaded-function-spec-id*        lhs.ofs))))

(define* (overloaded-function-spec.register-specialisation! lhs.id spec.id spec.lambda-sig)
  ;;Register a specialisation function in the descriptor of an overloaded function.
  ;;
  ;;LHS.ID is the syntactic identifier bound to the overloaded function descriptor.
  ;;
  ;;SPEC.ID is the syntactic identifier bound to a specialisation function.
  ;;
  ;;SPEC.LAMBDA-SIG  is an  instance  of "<lambda-signature>"  representing the  type
  ;;signature of the specialisation function.
  ;;
  (let ((lhs.ofs (let* ((lhs.lab (id->label/local lhs.id))
			(lhs.des (label->syntactic-binding-descriptor lhs.lab (current-inferior-lexenv))))
		   (case (syntactic-binding-descriptor.type lhs.des)
		     ((local-overloaded-function)
		      (syntactic-binding-descriptor.value lhs.des))
		     ((global-overloaded-function)
		      (syntactic-binding-descriptor/global-overloaded-function.ofs lhs.des))
		     (else
		      ;;The syntactic binding is not an overloaded function.
		      (assertion-violation __who__
			"invalid syntactic binding's descriptor, expecting overloaded function"
			lhs.id lhs.des))))))
    (set-overloaded-function-spec-signature*! lhs.ofs (cons spec.lambda-sig (overloaded-function-spec-signature* lhs.ofs)))
    (set-overloaded-function-spec-id*!        lhs.ofs (cons spec.id         (overloaded-function-spec-id*        lhs.ofs)))))

(define (overloaded-function-spec.expanded-expr ofs)
  ;;Build and return a core language  expression that, compiled and evaluated, return
  ;;an empty copy of the OFS argument.
  ;;
  (let ((name.id (overloaded-function-spec.name-id ofs)))
    (build-application no-source
	(build-primref no-source 'make-overloaded-function-spec)
      (list (build-data no-source name.id)))))


;;;; done

#| end of module |# )

;;; end of file
