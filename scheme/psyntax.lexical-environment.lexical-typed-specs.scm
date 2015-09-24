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

	 <lexical-typed-spec>
	 make-lexical-typed-spec			lexical-typed-spec?
	 lexical-typed-spec.lex				lexical-typed-spec.type-id
	 lexical-typed-spec.defined-type?
	 lexical-typed-spec.assigned?			lexical-typed-spec.set-assigned!

	 <global-typed-spec>
	 make-global-typed-spec				global-typed-spec?
	 global-typed-spec.loc				global-typed-spec.type-id
	 global-typed-spec.lib				global-typed-spec.set-lib!
	 )


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

(define-record-type (<lexical-typed-spec> make-lexical-typed-spec lexical-typed-spec?)
  (nongenerative vicare:expander:<lexical-typed-spec>)
  (parent <typed-variable-spec>)
  (fields
   (immutable lex		lexical-typed-spec.lex)
		;A gensym representing the lexical gensym of the variable.
   (mutable   assigned?		lexical-typed-spec.assigned? lexical-typed-spec.set-assigned!)
		;Boolean.  True if this lexical variable is assigned at least once in
		;the code; false if it is never assigned.
   (immutable defined-type?	lexical-typed-spec.defined-type?)
		;Boolean.  True if the type of this variable has been declared in the
		;variable definition; false  if it was inferred  from the surrounding
		;code.
   #| end of fields |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-<lexical-typed-spec> {lex gensym?} {type-id identifier?} defined-type?)
	((make-typed-variable-spec type-id) lex #f (and defined-type? #t)))
      make-<lexical-typed-spec>))
  #| end of DEFINE-RECORD-TYPE |# )

(define (lexical-typed-spec.type-id spec)
  (typed-variable-spec.type-id spec))


;;;; global lexical variable specification

(define-record-type (<global-typed-spec> make-global-typed-spec global-typed-spec?)
  (nongenerative vicare:expander:<global-typed-spec>)
  (parent <typed-variable-spec>)
  (fields
   (immutable loc	global-typed-spec.loc)
		;A gensym representing the location gensym of the variable.
   (mutable   lib	global-typed-spec.lib global-typed-spec.set-lib!)
		;False or an instance of "lib" representing the library in which this
		;global syntactic binding is defined.
   #| end of fields |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-global-typed-spec {loc gensym?} {lexical-spec lexical-typed-spec?})
	;;The lex gensym of global variables acts also as loc gensym.
	((make-typed-variable-spec (lexical-typed-spec.type-id lexical-spec)
				   (typed-variable-spec.unsafe-variant-sexp lexical-spec))
	 loc #f))
      make-global-typed-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define (global-typed-spec.type-id spec)
  (typed-variable-spec.type-id spec))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'let-syntax-rules			'scheme-indent-function 1)
;; End:
