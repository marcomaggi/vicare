;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module (<typed-variable-spec>
	 typed-variable-spec?
	 typed-variable-spec.type-id
	 typed-variable-spec.unsafe-variant-sexp	typed-variable-spec.unsafe-variant-sexp-set!

	 <lexical-typed-variable-spec>
	 make-lexical-typed-variable-spec		lexical-typed-variable-spec?
	 lexical-typed-variable-spec.lex		lexical-typed-variable-spec.type-id
	 lexical-typed-variable-spec.assigned?		lexical-typed-variable-spec.assigned?-set!

	 <global-typed-variable-spec>
	 make-global-typed-variable-spec		global-typed-variable-spec?
	 global-typed-variable-spec.variable-loc	global-typed-variable-spec.type-id)


;;;; lexical variable specification: base type

(define-record-type (<typed-variable-spec> dummy typed-variable-spec?)
  (nongenerative vicare:expander:<typed-variable-spec>)
  (fields
   (immutable type-id		typed-variable-spec.type-id)
		;A syntactic identifier representing the type of this variable.
   (mutable   unsafe-variant	typed-variable-spec.unsafe-variant-sexp typed-variable-spec.unsafe-variant-sexp-set!)
		;This  field is  used only  when this  variable binding  references a
		;closure object.
		;
		;False or a symbolic expression  (to be BLESSed later) representing a
		;Scheme expression which, expanded and evaluated at run-time, returns
		;the unsafe variant of this closure object.
		;
		;The  unsafe variant  is meant  to be  used in  place of  the closure
		;object, when we know, at expand-time,  that the type of the operands
		;in a closure application is correct.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (case-define make-typed-variable-spec
	((type-id)
	 (make-record type-id #f))
	((type-id unsafe-variant.sexp)
	 (make-record type-id unsafe-variant.sexp)))
      make-typed-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; local lexical variable specification

(define-record-type (<lexical-typed-variable-spec> make-lexical-typed-variable-spec lexical-typed-variable-spec?)
  (nongenerative vicare:expander:<lexical-typed-variable-spec>)
  (parent <typed-variable-spec>)
  (fields
   (immutable lex		lexical-typed-variable-spec.lex)
		;The lex gensym of the lexical variable.
   (mutable   assigned?		lexical-typed-variable-spec.assigned? lexical-typed-variable-spec.assigned?-set!)
   #| end of fields |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-lexical-typed-variable-spec type-id lex)
	((make-typed-variable-spec type-id) lex #f))
      make-lexical-typed-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define-syntax-rule (lexical-typed-variable-spec.type-id gts)
  (typed-variable-spec.type-id gts))


;;;; global lexical variable specification

;;A global typed variable has two loc gensyms:
;;
;;* One for the global variable, which holds the variable's value; it pertains to the
;;invoke code.
;;
;;*  One for  the type  specification,  which holds  a  reference to  an instance  of
;;"<global-typed-variable-spec>"; it pertains to the invoke code.
;;
(define-record-type (<global-typed-variable-spec> make-global-typed-variable-spec global-typed-variable-spec?)
  (nongenerative vicare:expander:<global-typed-variable-spec>)
  (parent <typed-variable-spec>)
  (fields
   (immutable variable-loc	global-typed-variable-spec.variable-loc)
		;The loc gensym of the variable.
   #| end of fields |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-global-typed-variable-spec type-id unsafe-variant.sexp variable-loc)
	((make-typed-variable-spec type-id unsafe-variant.sexp) variable-loc))
      make-global-typed-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define-syntax-rule (global-typed-variable-spec.type-id gts)
  (typed-variable-spec.type-id gts))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'let-syntax-rules			'scheme-indent-function 1)
;; End:
