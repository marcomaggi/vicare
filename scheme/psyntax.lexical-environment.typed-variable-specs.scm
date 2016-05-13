;;; -*- coding: utf-8-unix -*-
;;;
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


(module (<typed-variable-spec>
	 typed-variable-spec?
	 typed-variable-spec.ots			typed-variable-spec.ots-set!

	 <lexical-typed-variable-spec>
	 make-lexical-typed-variable-spec		lexical-typed-variable-spec?
	 lexical-typed-variable-spec.lex
	 lexical-typed-variable-spec.assigned?		lexical-typed-variable-spec.assigned?-set!

	 <lexical-closure-variable-spec>
	 make-lexical-closure-variable-spec		lexical-closure-variable-spec?
	 lexical-closure-variable-spec.replacements

	 <global-typed-variable-spec>
	 make-global-typed-variable-spec		global-typed-variable-spec?
	 global-typed-variable-spec.variable-loc

	 <global-closure-variable-spec>
	 make-global-closure-variable-spec		global-closure-variable-spec?
	 global-closure-variable-spec.replacements

	 <core-prim-type-spec>
	 make-core-prim-type-spec			core-prim-type-spec?
	 core-prim-type-spec.name			core-prim-type-spec.safety)


;;;; lexical variable specification: base type

(define-record-type (<typed-variable-spec> dummy typed-variable-spec?)
  (nongenerative *0*vicare:expander:<typed-variable-spec>)
  (fields
    (mutable ots		typed-variable-spec.ots typed-variable-spec.ots-set!)
		;An instance  of "<object-type-spec>"  representing the type  of this
		;variable.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-typed-variable-spec {ots object-type-spec?})
	(make-record ots))
      make-typed-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; local lexical variable specification

(define-record-type (<lexical-typed-variable-spec> make-lexical-typed-variable-spec lexical-typed-variable-spec?)
  (nongenerative *0*vicare:expander:<lexical-typed-variable-spec>)
  (parent <typed-variable-spec>)
  (fields
    (immutable lex		lexical-typed-variable-spec.lex)
		;The lex gensym of the lexical variable.
    (mutable   assigned?	lexical-typed-variable-spec.assigned? lexical-typed-variable-spec.assigned?-set!)
    #| end of fields |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-lexical-typed-variable-spec {ots object-type-spec?} {lex gensym?})
	((make-typed-variable-spec ots) lex #f))
      make-lexical-typed-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define-record-type (<lexical-closure-variable-spec> make-lexical-closure-variable-spec lexical-closure-variable-spec?)
  (nongenerative *0*vicare:expander:<lexical-closure-variable-spec>)
  (parent <lexical-typed-variable-spec>)
  (fields
    (immutable replacements	lexical-closure-variable-spec.replacements)
		;False or  a vector  of syntactic identifiers  bound to  the possible
		;replacements for this closure object.
    #| end of fields |# )
  (protocol
    (lambda (make-lexical-typed-variable-spec)
      (case-define* make-lexical-closure-variable-spec
	(({ots object-type-spec?} {lex gensym?})
	 ((make-lexical-typed-variable-spec ots lex) #f))
	(({ots object-type-spec?} {lex gensym?} {replacements (or not vector?)})
	 ((make-lexical-typed-variable-spec ots lex) replacements)))
      make-lexical-closure-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; global lexical variable specification

;;A global typed variable has two loc gensyms:
;;
;;* One for the global variable, which holds the variable's value; it pertains to the
;;invoke code.
;;
;;*  One for  the type  specification,  which holds  a  reference to  an instance  of
;;"<global-typed-variable-spec>"; it pertains to the visit code.
;;
(define-record-type (<global-typed-variable-spec> make-global-typed-variable-spec global-typed-variable-spec?)
  (nongenerative *0*vicare:expander:<global-typed-variable-spec>)
  (parent <typed-variable-spec>)
  (fields
    (immutable variable.loc	global-typed-variable-spec.variable-loc)
		;The loc gensym of the variable.
    #| end of fields |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-global-typed-variable-spec {ots object-type-spec?} {variable.loc gensym?})
	((make-typed-variable-spec ots) variable.loc))
      make-global-typed-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define-record-type (<global-closure-variable-spec> make-global-closure-variable-spec global-closure-variable-spec?)
  (nongenerative *0*vicare:expander:<global-closure-variable-spec>)
  (parent <global-typed-variable-spec>)
  (fields
    (immutable replacements	global-closure-variable-spec.replacements)
		;False or  a vector  of syntactic identifiers  bound to  the possible
		;replacements for this closure object.
    #| end of fields |# )
  (protocol
    (lambda (make-global-typed-variable-spec)
      (case-define* make-global-closure-variable-spec
	(({ots object-type-spec?} {variable.loc gensym?})
	 ((make-global-typed-variable-spec ots variable.loc) #f))
	(({ots object-type-spec?} {variable.loc gensym?} {replacements (or not vector?)})
	 ((make-global-typed-variable-spec ots variable.loc) replacements)))
      make-global-closure-variable-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; typed core primitive

(define-record-type (<core-prim-type-spec> make-core-prim-type-spec core-prim-type-spec?)
  (nongenerative *0*vicare:expander:<core-prim-type-spec>)
  (parent <typed-variable-spec>)
  (fields
    (immutable name			core-prim-type-spec.name)
		;A symbol representing the public name of this core primitive.
    (immutable safety			core-prim-type-spec.safety)
		;Boolean.  True if this core primitive is safe.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-typed-variable-spec)
      (define* (make-core-prim-type-spec {core-prim.sym symbol?} safety {closure.ots closure-type-spec?})
	;;CORE-PRIM.SYM  is  a  symbol  representing  the public  name  of  the  core
	;;primitive.
	;;
	;;SAFETY is a boolean, true if this primitive is safe.
	;;
	;;TYPE-ID is a syntactic identifier representing the type of this function.
	;;
	((make-typed-variable-spec closure.ots) core-prim.sym safety))
      make-core-prim-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
