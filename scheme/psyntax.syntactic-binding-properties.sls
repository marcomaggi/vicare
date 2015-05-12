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
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(library (psyntax.syntactic-binding-properties)
  (export

    ;; identifiers: syntactic binding properties
    syntactic-binding-putprop			$syntactic-binding-putprop
    syntactic-binding-getprop			$syntactic-binding-getprop
    syntactic-binding-remprop			$syntactic-binding-remprop
    syntactic-binding-property-list		$syntactic-binding-property-list

    ;; identifiers: unsafe variants API
    set-identifier-unsafe-variant!		identifier-unsafe-variant

    ;; identifiers: predicates axiliary functions API
    set-predicate-assertion-procedure-argument-validation!	predicate-assertion-procedure-argument-validation
    set-predicate-assertion-return-value-validation!		predicate-assertion-return-value-validation)
  (import (rnrs)
    (psyntax.compat)
    (only (psyntax.lexical-environment)
	  id->label/or-error
	  syntax-object?))


;;;; identifiers: syntactic binding properties

(define* (syntactic-binding-putprop {id identifier?} {key symbol?} value)
  ($syntactic-binding-putprop id key value))

(define* (syntactic-binding-getprop {id identifier?} {key symbol?})
  ($syntactic-binding-getprop id key))

(define* (syntactic-binding-remprop {id identifier?} {key symbol?})
  ($syntactic-binding-remprop id key))

(define* (syntactic-binding-property-list {id identifier?})
  ($syntactic-binding-property-list id))

;;; --------------------------------------------------------------------

(define* ($syntactic-binding-putprop id key value)
  ($putprop (id->label/or-error __who__ id id) key value))

(define* ($syntactic-binding-getprop id key)
  ($getprop (id->label/or-error __who__ id id) key))

(define* ($syntactic-binding-remprop id key)
  ($remprop (id->label/or-error __who__ id id) key))

(define* ($syntactic-binding-property-list id)
  ($property-list (id->label/or-error __who__ id id)))


;;;; identifiers: unsafe variants API
;;
;;See the Texinfo documentation for explanations on the unsafe variants.
;;

(define-constant *UNSAFE-VARIANT-COOKIE*
  'vicare:expander:unsafe-variant)

(define* (set-identifier-unsafe-variant! {safe-id identifier?} {unsafe-expr-stx syntax-object?})
  (if (syntactic-binding-getprop safe-id *UNSAFE-VARIANT-COOKIE*)
      (syntax-violation __who__
	"unsafe variant already defined" safe-id unsafe-expr-stx)
    (syntactic-binding-putprop safe-id *UNSAFE-VARIANT-COOKIE* unsafe-expr-stx)))

(define* (identifier-unsafe-variant {safe-id identifier?})
  (syntactic-binding-getprop safe-id *UNSAFE-VARIANT-COOKIE*))


;;;; identifiers: predicates axiliary functions API

(let-syntax
    ((define-identifier-property (syntax-rules ()
				   ((_ ?cookie ?setter ?getter ?alread-defined-errmsg)
				    (begin
				      (define* (?setter {pred-id identifier?} {validation-stx syntax-object?})
					(if ($syntactic-binding-getprop pred-id '?cookie)
					    (syntax-violation __who__ ?alread-defined-errmsg pred-id validation-stx)
					  ($syntactic-binding-putprop pred-id '?cookie validation-stx)))
				      (define* (?getter {pred-id identifier?})
					($syntactic-binding-getprop pred-id '?cookie))))
				   )))

  (define-identifier-property vicare:expander:predicate-procedure-argument-validation
    set-predicate-assertion-procedure-argument-validation!
    predicate-assertion-procedure-argument-validation
    "predicate procedure argument validation already defined")

  (define-identifier-property vicare:expander:predicate-return-value-validation
    set-predicate-assertion-return-value-validation!
    predicate-assertion-return-value-validation
    "predicate procedure return value validation already defined")

  #| end of LET-SYNTAX |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
