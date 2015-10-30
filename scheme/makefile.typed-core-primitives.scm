;;; typed-core-primitives.scm --
;;
;;Typed core primitive properties.

;; Create a lexical contour.
(let ()


;;;; helpers

(define-auxiliary-syntaxes /comment)

(define-syntax comment
  (syntax-rules (/comment)
    ((comment ?form ... /comment)
     (void))))

;;; --------------------------------------------------------------------

(define-syntax /section
  (syntax-rules ()))

(define-syntax section
  ;;By enclosing code in:
  ;;
  ;;   (section ?body ... /section)
  ;;
  ;;we can comment out a section by just commenting out the form:
  ;;
  ;;   #;(section ?body ... /section)
  ;;
  ;;This is sometimes useful when debugging.
  ;;
  (syntax-rules (/section)
    ((section ?body ... /section)
     (begin ?body ...))))


;;;; basic definition syntaxes

(define-auxiliary-syntaxes signatures attributes replacements safe unsafe)

(define-syntax (declare-core-primitive stx)
  (define (main input-form.stx)
    (syntax-case input-form.stx (replacements signatures attributes)
      ;; signatures
      ((_ ?prim-name ?safety (signatures . ?signatures))
       (%output #'?prim-name #'?safety #'?signatures '()))

      ;; signatures, replacements
      ((_ ?prim-name ?safety (signatures . ?signatures) (replacements . ?replacements))
       (%output #'?prim-name #'?safety #'?signatures #'?replacements))

      ;; signatures, attributes
      ((_ ?prim-name ?safety (signatures . ?signatures) (attributes . ?stuff))
       (%output #'?prim-name #'?safety #'?signatures '()))

      ;; signatures, attributes, replacements
      ((_ ?prim-name ?safety (signatures . ?signatures) (attributes . ?stuff) (replacements . ?replacements))
       (%output #'?prim-name #'?safety #'?signatures #'?replacements))
      ))

  (define (%output prim-name.id safety.id signatures.stx replacements.stx)
    ;;The syntactic  binding's descriptor's value is  a vector: it is  more efficient
    ;;than a list, memory-wise.
    ;;
    (with-syntax
	((PRIM-NAME	prim-name.id)
	 (SAFETY	(%validate-safety safety.id))
	 (SIGNATURES	(%validate-and-format-signatures signatures.stx))
	 (REPLACEMENTS	(%validate-replacements replacements.stx)))
      #'(set-cons! VICARE-TYPED-CORE-PRIMITIVES
		   (cons* (quote PRIM-NAME)
			  (quote $core-prim-typed)
			  (quote #(PRIM-NAME SAFETY SIGNATURES REPLACEMENTS))))))

  (define (%validate-safety safety.stx)
    (syntax-case safety.stx (safe unsafe)
      ((safe)		#t)
      ((unsafe)		#f)
      (_
       (synner "invalid safety specification" safety.stx))))

  (define (%validate-and-format-signatures signatures.stx)
    ;;Validate SIGNATURES.STX  as syntax  object representing  a list  of CASE-LAMBDA
    ;;clauses type signatures:
    ;;
    ;;   ((?argvals-signature0 => ?retvals-signature0)
    ;;    (?argvals-signature  => ?retvals-signature)
    ;;    ...)
    ;;
    ;;When successful, return a syntax object representing a list with the format:
    ;;
    ;;   ((?retvals-signature0 . ?argvals-signature0)
    ;;    (?retvals-signature  . ?argvals-signature)
    ;;    ...)
    ;;
    ;;in which the order of the signatures is unchanged.
    ;;
    (syntax-case signatures.stx (=>)
      (()
       '())

      (((?argvals-signature => ?retvals-signature) . ?rest)
       (cons (cons (%validate-type-signature #'?retvals-signature)
		   (%validate-type-signature #'?argvals-signature))
	     (%validate-and-format-signatures #'?rest)))

      ((?signature . ?rest)
       (synner "invalid signature specification" #'?signature))))

  (define (%validate-type-signature type-signature.stx)
    ;;Validate TYPE-SIGNATURE.STX as syntax object representing a type signature:
    ;;
    ;;   (?type ...)
    ;;   (?type ... . ?tail-type)
    ;;   ?type
    ;;
    ;;When successful:  return a proper or  improper list representing the  same type
    ;;signature, with the identifiers "_" replaced with "<top>".
    ;;
    (define (%replace id)
      (if (free-identifier=? #'_ id) #'<top> id))
    (let recur ((sig type-signature.stx))
      (syntax-case sig ()
	(()
	 '())
	((?type . ?rest)
	 (identifier? #'?type)
	 (cons (%replace #'?type) (recur #'?rest)))
	(?type
	 (identifier? #'?type)
	 (%replace #'?type))
	(_
	 (synner "invalid type signature" type-signature.stx)))))

  (define (%validate-replacements replacements.stx)
    ;;Validate REPLACEMENTS.STX as syntax object  representing a list of identifiers.
    ;;When successful: return REPLACEMENTS.STX itself.
    ;;
    (syntax-case replacements.stx ()
      ((?replacement ...)
       (all-identifiers? replacements.stx)
       replacements.stx)
      (_
       (synner "invalid replacements specification" replacements.stx))))

  (case-define synner
    ((message subform)
     (syntax-violation 'declare-core-primitive message stx subform))
    ((message)
     (syntax-violation 'declare-core-primitive message stx)))

  (main stx))


;;;; syntax helpers: type predicates

(define-syntax declare-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate fixnum? <fixnum>)
  ;;   (declare-type-predicate vector? <vector>)
  ;;   (declare-type-predicate exact-integer? <fixnum> <bignum> <exact-integer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag ...)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((?obj-tag)	=> (<true>))
	...
	((<top>)	=> (<boolean>)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/maybe
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/maybe maybe-pointer? <pointer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag ...)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((<void>)	=> (<true>))
	((?obj-tag)	=> (<true>))
	...
	((<top>)	=> (<boolean>)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/false
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/false false-or-pointer? <pointer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag ...)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((<false>)	=> (<true>))
	((?obj-tag)	=> (<true>))
	...
	((<top>)	=> (<boolean>)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/list
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/list list-of-pointers? <pointer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag ...)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((<null>)	=> (<true>))
	((?obj-tag)	=> (<true>))
	...
	((<top>)	=> (<boolean>)))
       (attributes
	((_)		foldable effect-free))))
    ))

;;; --------------------------------------------------------------------

(module (define-object-predicate-declarer)

  (define-syntax define-object-predicate-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-predicate-declarer declare-number-predicate <number>)
    ;;   (declare-number-predicate zero?)
    ;;   (declare-number-predicate positive?)
    ;;   (declare-number-predicate negative?)
    ;;
    (syntax-rules ()
      ((_ ?declarer ?obj-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-predicate ?who ?obj-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-predicate ?who ?obj-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-predicate ?who ?obj-tag unsafe (replacements)))

	   ((_ ?who		(replacements . ?replacements))	(%define-predicate ?who ?obj-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe	(replacements . ?replacements))	(%define-predicate ?who ?obj-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe	(replacements . ?replacements))	(%define-predicate ?who ?obj-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-predicate
    (syntax-rules (replacements)
      ((_ ?who ?obj-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?obj-tag)		=> (<boolean>)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-PREDICATE-DECLARER |# )

(define-object-predicate-declarer declare-object-predicate		<top>)

(define-object-predicate-declarer declare-fixnum-predicate		<fixnum>)
(define-object-predicate-declarer declare-bignum-predicate		<bignum>)
(define-object-predicate-declarer declare-flonum-predicate		<flonum>)
(define-object-predicate-declarer declare-ratnum-predicate		<ratnum>)
(define-object-predicate-declarer declare-compnum-predicate		<compnum>)
(define-object-predicate-declarer declare-cflonum-predicate		<cflonum>)
(define-object-predicate-declarer declare-number-predicate		<number>)

(define-object-predicate-declarer declare-char-predicate		<char>)
(define-object-predicate-declarer declare-string-predicate		<string>)
(define-object-predicate-declarer declare-keyword-predicate		<keyword>)
(define-object-predicate-declarer declare-vector-predicate		<vector>)
(define-object-predicate-declarer declare-bytevector-predicate		<bytevector>)
(define-object-predicate-declarer declare-struct-predicate		<struct>)
(define-object-predicate-declarer declare-record-predicate		<record>)
(define-object-predicate-declarer declare-port-predicate		<port>)


;;;; syntax helpers: hash functions

(define-syntax-rule (declare-hash-function ?prim-name ?type ?safety)
  (declare-core-primitive ?prim-name
      (?safety)
    (signatures
     ((?type)		=> (<fixnum>)))))


;;;; syntax helpers: comparison functions

(module (define-object-binary-comparison-declarer)

  (define-syntax define-object-binary-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-comparison-declarer declare-object-binary-comparison _)
    ;;   (declare-object-binary-comparison eq?)
    ;;   (declare-object-binary-comparison eqv?)
    ;;   (declare-object-binary-comparison equal?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)	=> (<boolean>)))
	 (attributes
	  ((_ _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-COMPARISON-DECLARER |# )

(define-object-binary-comparison-declarer declare-object-binary-comparison <top>)
(define-object-binary-comparison-declarer declare-fixnum-binary-comparison <fixnum>)
(define-object-binary-comparison-declarer declare-flonum-binary-comparison <flonum>)
(define-object-binary-comparison-declarer declare-number-binary-comparison <number>)
(define-object-binary-comparison-declarer declare-pointer-binary-comparison <pointer>)
(define-object-binary-comparison-declarer declare-char-binary-comparison <char>)
(define-object-binary-comparison-declarer declare-string-binary-comparison <string>)
(define-object-binary-comparison-declarer declare-keyword-binary-comparison <keyword>)
(define-object-binary-comparison-declarer declare-bytevector-binary-comparison <bytevector>)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-comparison-declarer)

  (define-syntax define-object-unary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison <flonum>)
    ;;   (declare-flonum-unary/multi-comparison fl=?)
    ;;   (declare-flonum-unary/multi-comparison fl<?)
    ;;   (declare-flonum-unary/multi-comparison fl>?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)				=> (<boolean>))
	  ((?type-tag ?type-tag)		=> (<boolean>))
	  ((?type-tag ?type-tag . ?type-tag)	=> (<boolean>)))
	 (attributes
	  ((_)				foldable effect-free)
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-COMPARISON-DECLARER |# )

(define-object-unary/multi-comparison-declarer declare-number-unary/multi-comparison <number>)
(define-object-unary/multi-comparison-declarer declare-fixnum-unary/multi-comparison <fixnum>)
(define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison <flonum>)
(define-object-unary/multi-comparison-declarer declare-string-unary/multi-comparison <string>)

;;; --------------------------------------------------------------------

(module (define-object-binary/multi-comparison-declarer)

  (define-syntax define-object-binary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison <char>)
    ;;   (declare-char-binary/multi-comparison char=?)
    ;;   (declare-char-binary/multi-comparison char<?)
    ;;   (declare-char-binary/multi-comparison char>?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)		=> (<boolean>))
	  ((?type-tag ?type-tag . ?type-tag)	=> (<boolean>)))
	 (attributes
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY/MULTI-COMPARISON-DECLARER |# )

(define-object-binary/multi-comparison-declarer declare-number-binary/multi-comparison <number>)
(define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison <char>)
(define-object-binary/multi-comparison-declarer declare-string-binary/multi-comparison <string>)


;;;; syntax helpers: math operations

(module (define-object-unary-operation-declarer)

  (define-syntax (define-object-unary-operation-declarer stx)
    ;;Usage example:
    ;;
    ;;   (define-object-unary-operation-declarer declare-flonum-unary <flonum>)
    ;;   (declare-flonum-unary flsin)
    ;;   (declare-flonum-unary flcos)
    ;;   (declare-flonum-unary fltan)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY-OPERATION-DECLARER |# )

(define-object-unary-operation-declarer declare-number-unary <number>)
(define-object-unary-operation-declarer declare-fixnum-unary <fixnum>)
(define-object-unary-operation-declarer declare-flonum-unary <flonum>)
(define-object-unary-operation-declarer declare-exact-integer-unary <exact-integer>)
(define-object-unary-operation-declarer declare-char-unary <char>)
(define-object-unary-operation-declarer declare-string-unary <string>)

;;; --------------------------------------------------------------------

(module (define-object-binary-operation-declarer)

  (define-syntax (define-object-binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-operation-declarer declare-flonum-binary <flonum>)
    ;;   (declare-flonum-binary flexpt)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_ _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-OPERATION-DECLARER |# )

(define-object-binary-operation-declarer declare-number-binary <number>)
(define-object-binary-operation-declarer declare-fixnum-binary <fixnum>)
(define-object-binary-operation-declarer declare-flonum-binary <flonum>)
(define-object-binary-operation-declarer declare-string-binary <string>)

;;; --------------------------------------------------------------------

(module (define-object-unary/binary-operation-declarer)

  (define-syntax (define-object-unary/binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/binary-operation-declarer declare-flonum-unary/binary <flonum>)
    ;;   (declare-flonum-unary/binary fllog)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)			=> (?type-tag))
	  ((?type-tag ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)			foldable effect-free result-true)
	  ((_ _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/BINARY-OPERATION-DECLARER |# )

(define-object-unary/binary-operation-declarer declare-number-unary/binary <number>)
(define-object-unary/binary-operation-declarer declare-fixnum-unary/binary <fixnum>)
(define-object-unary/binary-operation-declarer declare-flonum-unary/binary <flonum>)
(define-object-unary/binary-operation-declarer declare-string-unary/binary <string>)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-operation-declarer)

  (define-syntax (define-object-unary/multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-operation-declarer declare-flonum-unary/multi <flonum>)
    ;;   (declare-flonum-unary/multi fl+)
    ;;   (declare-flonum-unary/multi fl-)
    ;;   (declare-flonum-unary/multi fl*)
    ;;   (declare-flonum-unary/multi fl/)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)			=> (?type-tag))
	  ((?type-tag . ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)				foldable effect-free result-true)
	  ((_ . _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-OPERATION-DECLARER |# )

(define-object-unary/multi-operation-declarer declare-number-unary/multi <number>)
(define-object-unary/multi-operation-declarer declare-fixnum-unary/multi <fixnum>)
(define-object-unary/multi-operation-declarer declare-flonum-unary/multi <flonum>)
(define-object-unary/multi-operation-declarer declare-string-unary/multi <string>)

;;; --------------------------------------------------------------------

(module (define-object-multi-operation-declarer)

  (define-syntax (define-object-multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-multi-operation-declarer declare-fixnum-multi <fixnum>)
    ;;   (declare-fixnum-multi fxior)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  (()			=> (?type-tag))
	  (?type-tag		=> (?type-tag)))
	 (attributes
	  (()			foldable effect-free result-true)
	  ((_ . _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-MULTI-OPERATION-DECLARER |# )

(define-object-multi-operation-declarer declare-number-multi <number>)
(define-object-multi-operation-declarer declare-fixnum-multi <fixnum>)
(define-object-multi-operation-declarer declare-flonum-multi <flonum>)
(define-object-multi-operation-declarer declare-string-multi <string>)

;;; --------------------------------------------------------------------

(define-syntax declare-unsafe-unary-operation
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-unary-operation $neg-fixnum <fixnum> <exact-integer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand-tag ?result-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand-tag)		=> (?result-tag)))
       (attributes
	((_)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-unary-operation/2rv
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-unary-operation/2rv $exact-integer-sqrt-fixnum <fixnum> <exact-integer> <exact-integer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand-tag ?result1-tag ?result2-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand-tag)		=> (?result1-tag ?result2-tag)))
       (attributes
	((_)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-binary-operation
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-binary-operation $add-fixnum-fixnum <fixnum> <fixnum> <exact-integer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand1-tag ?operand2-tag ?result-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand1-tag ?operand2-tag)	=> (?result-tag)))
       (attributes
	((_ _)				foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-binary-operation/2rv
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-binary-operation $quotient+remainder-fixnum-fixnum <fixnum> <fixnum> <exact-integer> <fixnum>)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand1-tag ?operand2-tag ?result1-tag ?result2-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand1-tag ?operand2-tag)	=> (?result1-tag ?result2-tag)))
       (attributes
	((_ _)				foldable effect-free result-true))))
    ))


;;;; syntax helpers: pairs, lists, alists

(module (declare-pair-accessor)

  (define-syntax declare-pair-accessor
    ;;This is for: CAR, CDR, CAAR, CADR, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-pair-accessor ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-pair-accessor ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-pair-accessor ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-pair-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-pair-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-pair-accessor ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-pair-accessor
    (syntax-rules (replacements)
      ((_ ?who ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((<pair>)		=> (_)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (declare-pair-mutator)

  (define-syntax declare-pair-mutator
    ;;This is for: SET-CAR!, SET-CDR!, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-pair-mutator ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-pair-mutator ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-pair-mutator ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-pair-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-pair-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-pair-mutator ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-pair-mutator
    (syntax-rules (replacements)
      ((_ ?who ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((<pair> _)		=> (<void>)))
	 (attributes
	  ((_ _)		result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DECLARE-PAIR-MUTATOR |# )

;;; --------------------------------------------------------------------

(module (declare-alist-accessor)

  (define-syntax declare-alist-accessor
    ;;This is for: ASSQ, ASSV, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who ?obj-tag)					(%declare-alist-accessor ?who ?obj-tag safe   (replacements)))
      ((_ ?who ?obj-tag safe)					(%declare-alist-accessor ?who ?obj-tag safe   (replacements)))
      ((_ ?who ?obj-tag unsafe)					(%declare-alist-accessor ?who ?obj-tag unsafe (replacements)))
      ((_ ?who ?obj-tag        (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag safe   (replacements . ?replacements)))
      ((_ ?who ?obj-tag safe   (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag safe   (replacements . ?replacements)))
      ((_ ?who ?obj-tag unsafe (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-alist-accessor
    (syntax-rules (replacements)
      ((_ ?who ?obj-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?obj-tag <list>)	=> (_)))
	 (attributes
	  ((_ _)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DECLARE-ALIST-ACCESSOR |# )

;;; --------------------------------------------------------------------

(define-syntax declare-list-finder
  ;;This is for: MEMQ, MEMV, ...
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag)
     (declare-list-finder ?who ?obj-tag safe))
    ((_ ?who ?obj-tag ?safety)
     (declare-core-primitive ?who
	 (?safety)
       (signatures
	((?obj-tag <null>)	=> (<false>))
	((?obj-tag <nlist>)	=> (_)))
       (attributes
	((_ ())			foldable effect-free result-false)
	((_ _)			foldable effect-free))))
    ))


;;;; syntax helpers: bytevectors

(define-syntax declare-safe-bytevector-conversion
  ;;Usage examples:
  ;;
  ;;   (declare-safe-bytevector-conversion uri-encode <bytevector>)
  ;;   (declare-safe-bytevector-conversion uri-decode <bytevector>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((<bytevector>)		=> (?return-value-tag)))
       (attributes
	;;Not foldable because it must return a new bytevector every time.
	((_)			effect-free result-true))))
    ))

;;; --------------------------------------------------------------------

(define-syntax declare-unsafe-bytevector-accessor
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-accessor $bytevector-u8-ref <octet>)
  ;;   (declare-unsafe-bytevector-accessor $bytevector-s8-ref <byte>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<bytevector> <fixnum>)	=> (?return-value-tag)))
       (attributes
	((_ _)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-bytevector-mutator
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-mutator $bytevector-set! <octet>/byte)
  ;;
  (syntax-rules ()
    ((_ ?who ?new-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<bytevector> <fixnum> ?new-value-tag)	=> (<void>)))))
    ))

(define-syntax declare-unsafe-bytevector-conversion
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-conversion $uri-encode <bytevector>)
  ;;   (declare-unsafe-bytevector-conversion $uri-decode <bytevector>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<bytevector>)		=> (?return-value-tag)))
       (attributes
	;;Not foldable because it must return a new bytevector every time.
	((_)			effect-free result-true))))
    ))


;;;; syntax helpers: miscellaneous

(define-syntax declare-parameter
  ;;Usage examples:
  ;;
  ;;   (declare-parameter current-input-port	<textual-input-port>)
  ;;   (declare-parameter native-transcoder	<transcoder>)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-parameter ?who <top>))
    ((_ ?who ?value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()			=> (?value-tag))
	((?value-tag)		=> (<void>))
	((?value-tag <boolean>)	=> (<void>)))
       (attributes
	(()			effect-free)
	((_)			result-true)
	((_ _)			result-true))))
    ))

(define-syntax declare-object-retriever
  ;;Usage examples:
  ;;
  ;;   (declare-object-retriever console-input-port <binary-input-port>)
  ;;   (declare-object-retriever native-eol-style foldable <symbol>)
  ;;
  ;;NOTE The returned object must *not* be false.
  ;;
  (syntax-rules (foldable)
    ((_ ?who foldable)
     (declare-object-retriever ?who foldable <top>))
    ((_ ?who foldable ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()		=> (?return-value-tag)))
       (attributes
	(()		foldable effect-free result-true))))

    ((_ ?who)
     (declare-object-retriever ?who <top>))
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()		=> (?return-value-tag)))
       (attributes
	;;Not foldable, we want the object built and returned at run-time.
	(()		effect-free result-true))))
    ))


;;;; core syntactic binding descriptors, typed core primitives: miscellanous primitives

(section

(declare-core-primitive void
    (safe)
  (signatures
   (() => (<void>))))

(declare-type-predicate void-object? <void>)

/section)


;;;; core syntactic binding descriptors, typed core primitives: char primitives

(section

(declare-core-primitive integer->char
    (safe)
  (signatures
   ((<non-negative-fixnum>) => (<char>))))

(declare-type-predicate char? <char>)

(declare-hash-function char-hash <char> safe)

(declare-core-primitive char->integer
    (safe)
  (signatures
   ((<char>) => (<non-negative-fixnum>))))

(declare-core-primitive char->fixnum
    (safe)
  (signatures
   ((<char>) => (<non-negative-fixnum>))))

/section)


;;;; core syntactic binding descriptors, typed core primitives: symbol primitives

(section

(declare-core-primitive string->symbol
    (safe)
  (signatures
   ((<string>) => (<symbol>))))

(declare-type-predicate symbol? <symbol>)

(declare-core-primitive symbol->string
    (safe)
  (signatures
   ((<symbol>) => (<string>))))

(declare-hash-function symbol-hash <symbol> safe)

(declare-core-primitive symbol-bound?
    (safe)
  (signatures
   ((<symbol>) => (<boolean>))))

(declare-core-primitive symbol-value
    (safe)
  (signatures
   ((<symbol>) => (<top>))))

(declare-core-primitive set-symbol-value!
    (safe)
  (signatures
   ((<symbol> <top>) => (<void>))))

(declare-core-primitive <symbol>-value
    (safe)
  (signatures
   ((<symbol>)		=> (<top>))
   ((<symbol> <top>)	=> (<void>))))

;;;

(declare-core-primitive putprop
    (safe)
  (signatures
   ((<symbol> <symbol> <top>)	=> (<void>))))

(declare-core-primitive getprop
    (safe)
  (signatures
   ((<symbol> <symbol>)		=> (<top>))))

(declare-core-primitive remprop
    (safe)
  (signatures
   ((<symbol> <symbol>)		=> (<void>))))

(declare-core-primitive property-list
    (safe)
  (signatures
   ((<symbol>)			=> (<list>))))

/section)


;;;; core syntactic binding descriptors, typed core primitives: keyword primitives

(section

(declare-core-primitive symbol->keyword
    (safe)
  (signatures
   ((<symbol>)		=> (<keyword>))))

(declare-type-predicate keyword? <keyword>)

(declare-core-primitive keyword->symbol
    (safe)
  (signatures
   ((<keyword>)		=> (<symbol>))))

(declare-core-primitive keyword->string
    (safe)
  (signatures
   ((<keyword>)		=> (<string>))))

(declare-hash-function keyword-hash <keyword> safe)

/section)


;;;; core syntactic binding descriptors, typed core primitives: pointer primitives

(section

(declare-core-primitive integer->pointer
    (safe)
  (signatures
   ((<exact-integer>)	=> (<pointer>))))

(declare-type-predicate pointer? <pointer>)

(declare-core-primitive pointer-null?
    (safe)
  (signatures
   ((<pointer>)		=> (<boolean>))))

(declare-core-primitive pointer->integer
    (safe)
  (signatures
   ((<pointer>)		=> (<exact-integer>))))

(declare-core-primitive pointer=?
    (safe)
  (signatures
   (<pointer>		=> (<boolean>))))

(declare-core-primitive pointer!=?
    (safe)
  (signatures
   (<pointer>		=> (<boolean>))))

(declare-core-primitive pointer<?
    (safe)
  (signatures
   (<pointer>		=> (<boolean>))))

(declare-core-primitive pointer>?
    (safe)
  (signatures
   (<pointer>		=> (<boolean>))))

(declare-core-primitive pointer<=?
    (safe)
  (signatures
   (<pointer>		=> (<boolean>))))

(declare-core-primitive pointer>=?
    (safe)
  (signatures
   (<pointer>		=> (<boolean>))))

(declare-hash-function pointer-hash <pointer> safe)

(declare-core-primitive pointer-add
    (safe)
  (signatures
   ((<pointer> <exact-integer>)	=> (<pointer>))))

(declare-core-primitive pointer-diff
    (safe)
  (signatures
   ((<pointer> <pointer>)	=> (<pointer>))))

(declare-core-primitive pointer-clone
    (safe)
  (signatures
   ((<pointer>)			=> (<pointer>))))

(declare-core-primitive set-pointer-null!
    (safe)
  (signatures
   ((<pointer>)			=> (<void>))))

/section)


;;;; core syntactic binding descriptors, typed core primitives: transcoders

(section

(declare-core-primitive make-transcoder
    (safe)
  (signatures
   ((<symbol> <symbol> <symbol>)	=> (<transcoder>))
   ((<symbol> <symbol>)			=> (<transcoder>))
   ((<symbol>)				=> (<transcoder>))))

(declare-type-predicate transcoder? <transcoder>)

(declare-core-primitive transcoder-codec
    (safe)
  (signatures
   ((<transcoder>)			=> (<symbol>))))

(declare-core-primitive transcoder-eol-style
    (safe)
  (signatures
   ((<transcoder>)			=> (<symbol>))))

(declare-core-primitive transcoder-error-handling-mode
    (safe)
  (signatures
   ((<transcoder>)			=> (<symbol>))))

(declare-parameter native-transcoder <transcoder>)

/section)


;;;; core syntactic binding descriptors, typed core primitives: strings

(section

;;; predicates

(declare-type-predicate string? <string>)
(declare-string-predicate string-empty?			(replacements $string-empty?))
(declare-string-predicate ascii-encoded-string?		(replacements $ascii-encoded-string?))
(declare-string-predicate latin1-encoded-string?	(replacements $latin1-encoded-string?))
(declare-string-predicate octets-encoded-string?	(replacements $octets-encoded-string?))
(declare-string-predicate uri-encoded-string?		(replacements $uri-encoded-string?))
(declare-string-predicate percent-encoded-string?	(replacements $percent-encoded-string?))

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive string
    (safe)
  (signatures
   (()			=> (<string>))
   (<char>		=> (<string>)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   (()			effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive make-string
    (safe)
  (signatures
   ((<non-negative-fixnum>)		=> (<string>))
   ((<non-negative-fixnum> <char>)	=> (<string>)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((0)			effect-free result-true)
   ((0 . _)		effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive substring
    (safe)
  (signatures
   ((<string> <non-negative-fixnum> <non-negative-fixnum>)	=> (<string>)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive string-copy
    (safe)
  (signatures
   ((<string>)		=> (<void>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string-copy!
    (safe)
  (signatures
   ((<string> <non-negative-fixnum> <string> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive string-append
    (safe)
  (signatures
   (<string>			=> (<string>)))
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive string-reverse-and-concatenate
    (safe)
  (signatures
   ((<string*>)			=> (<string>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive string-length
    (safe)
  (signatures
   ((<string>)		=> (<non-negative-fixnum>)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

(declare-core-primitive string-for-each
    (safe)
  (signatures
   ((<procedure> <string> . <string>)		=> (<void>)))
  (attributes
   ;;Not foldable and not effect-free because it applies an unknown procedure.
   ((_ _ . _)					result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $STRING-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Tue Oct 27, 2015)
(declare-core-primitive string-ref
    (safe)
  (signatures
   ((<string> <non-negative-fixnum>)	=> (<char>)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;FIXME This  cannot have  $STRING-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Tue Oct 27, 2015)
(declare-core-primitive string-set!
    (safe)
  (signatures
   ((<string> <non-negative-fixnum> <char>)	=> (<void>)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive string-fill!
    (safe)
  (signatures
   ((<string> <char>)	=> (<void>)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-string-binary/multi-comparison string<=?)

(declare-string-binary/multi-comparison string<?)
(declare-string-binary/multi-comparison string=?)
(declare-string-binary/multi-comparison string>=?)
(declare-string-binary/multi-comparison string>?)

(declare-string-binary/multi-comparison string-ci<=?)
(declare-string-binary/multi-comparison string-ci<?)
(declare-string-binary/multi-comparison string-ci=?)
(declare-string-binary/multi-comparison string-ci>=?)
(declare-string-binary/multi-comparison string-ci>?)

;;; --------------------------------------------------------------------
;;; transformation

(declare-string-unary string-titlecase)
(declare-string-unary string-upcase)
(declare-string-unary string-downcase)
(declare-string-unary string-foldcase)

(declare-string-unary string-normalize-nfc)
(declare-string-unary string-normalize-nfd)
(declare-string-unary string-normalize-nfkc)
(declare-string-unary string-normalize-nfkd)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive string->flonum
    (safe)
  (signatures
   ((<string>)		=> (<flonum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->number
    (safe)
  (signatures
   ;; ((<string>)               => ((or <number> <false>)))
   ;; ((<string> <non-negative-fixnum>)      => ((or <number> <false>)))
   ((<string>)		=> (<top>))
   ((<string> <non-negative-fixnum>)	=> (<top>)))
  (attributes
   ((_)			foldable effect-free)
   ((_ _)		foldable effect-free)))

(declare-core-primitive string->utf8
    (safe)
  (signatures
   ((<string>)			=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf16
    (safe)
  (signatures
   ((<string>)			=> (<bytevector>))
   ((<string> <symbol>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->utf32
    (safe)
  (signatures
   ((<string>)			=> (<bytevector>))
   ((<string> <symbol>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->bytevector
    (safe)
  (signatures
   ((<string> <transcoder>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(let-syntax
    ((declare-string->bytevector-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<string>)		=> (<bytevector>)))
	   (attributes
	    ;;Not  foldable  because  it  must  return  a  new  bytevector  at  every
	    ;;application.
	    ((_ )		effect-free result-true))))
	)))
  (declare-string->bytevector-conversion string->ascii)
  (declare-string->bytevector-conversion string->latin1)
  (declare-string->bytevector-conversion string->octets)
  (declare-string->bytevector-conversion string->percent-encoding)
  (declare-string->bytevector-conversion string->uri-encoding)
  (declare-string->bytevector-conversion string->utf16be)
  (declare-string->bytevector-conversion string->utf16le)
  (declare-string->bytevector-conversion string->utf16n)
  (declare-string->bytevector-conversion string-base64->bytevector)
  (declare-string->bytevector-conversion string-hex->bytevector)
  #| end of LET-SYNTAX |# )

;;;

(declare-core-primitive string->symbol
    (safe)
  (signatures
   ((<string>)			=> (<symbol>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string-or-symbol->string
    (safe)
  (signatures
   ((<string>)			=> (<string>))
   ((<symbol>)			=> (<string>)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string-or-symbol->symbol
    (safe)
  (signatures
   ((<string>)			=> (<symbol>))
   ((<symbol>)			=> (<symbol>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string->keyword
    (safe)
  (signatures
   ((<string>)			=> (<keyword>)))
  (attributes
   ;;Not foldable because keywords cannot be serialised in fasl files.
   ((_)				effect-free result-true)))

(declare-core-primitive string->list
    (safe)
  (signatures
   ((<string>)			=> (<char*>)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; strings, unsafe functions

;;; predicates

(declare-string-predicate $string-empty?)
(declare-string-predicate $octets-encoded-string?)
(declare-string-predicate $ascii-encoded-string?)
(declare-string-predicate $latin1-encoded-string?)
(declare-string-predicate $uri-encoded-string?)
(declare-string-predicate $percent-encoded-string?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive $make-string
    (unsafe)
  (signatures
   ((<non-negative-fixnum>)		=> (<string>)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string
    (unsafe)
  (signatures
   (<char>		=> (<string>)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   (_			effect-free result-true)))

(declare-core-primitive $string-concatenate
    (unsafe)
  (signatures
   ((<exact-integer> <string*>)	=> (<string>)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $string-reverse-and-concatenate
    (unsafe)
  (signatures
   ((<exact-integer> <string*>)	=> (<string>)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((<string>)		=> (<non-negative-fixnum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-total-length
    (unsafe)
  (signatures
   ((<exact-integer> <string*>)	=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $string-ref
    (unsafe)
  (signatures
   ((<string> <non-negative-fixnum>)	=> (<char>)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (unsafe)
  (signatures
   ((<string> <non-negative-fixnum> <char>)	=> (<void>)))
  (attributes
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-string-binary-comparison $string=)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $string->ascii
    (unsafe)
  (signatures
   ((<string>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->octets
    (unsafe)
  (signatures
   ((<string>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->latin1
    (unsafe)
  (signatures
   ((<string>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string-base64->bytevector
    (unsafe)
  (signatures
   ((<string>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->symbol
    (unsafe)
  (signatures
   ((<string>)		=> (<symbol>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $interned-strings
    (unsafe)
  (signatures
   (()			=> (<vector>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive <string>-for-each
    (safe)
  (signatures
   ((<string> <procedure> . <string>)	=> (<void>))))

/section)


;;;; core syntactic binding descriptors, typed core primitives: vectors

(section

;;; predicates

(declare-type-predicate vector? <vector>)

(declare-vector-predicate vector-empty?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive vector
    (safe)
  (signatures
   (()				=> (<vector>))
   (_				=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (()				effect-free result-true)
   (_				effect-free result-true)))

(declare-core-primitive subvector
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive make-vector
    (safe)
  (signatures
   ((<non-negative-fixnum>)		=> (<vector>))
   ((<non-negative-fixnum> _)		=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive vector-resize
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum>)		=> (<vector>))
   ((<vector> <non-negative-fixnum> <top>)	=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive vector-append
    (safe)
  (signatures
   (<vector>			=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive vector-copy
    (safe)
  (signatures
   ((<vector>)			=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive vector-length
    (safe)
  (signatures
   ((<vector>)			=> (<non-negative-fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $vector-length))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $VECTOR-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-ref
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum>)	=> (<top>)))
  (attributes
   ((_ _)		foldable effect-free)))

;;FIXME This  cannot have  $VECTOR-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-set!
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum> _)	=> (<void>)))
  (attributes
   ((_ _ _)			result-true)))

(declare-core-primitive vector-copy!
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum> <vector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>)))
  ;;Not foldable  because it must return  a newly allocated vector.   Not effect free
  ;;because it mutates the operand.
  (attributes
   ((_ _ _ _ _)			result-true)))

(declare-core-primitive vector-fill!
    (safe)
  (signatures
   ((<vector> <top>)		=> (<void>)))
  ;;Not effect free because it mutates the operand.
  (attributes
   ((_ _)			foldable result-true)))

;;; --------------------------------------------------------------------
;;; sorting

(declare-core-primitive vector-sort
    (safe)
  (signatures
   ((<procedure> <vector>)	=> (<vector>)))
  (attributes
   ;;Not foldable  because it  must return  a new vector  at every  application.  Not
   ;;effect-free because it invokes an unknown procedure.
   ((_ _)			result-true)))

(declare-core-primitive vector-sort!
    (safe)
  (signatures
   ((<procedure> <vector>)	=> (<void>)))
  (attributes
   ;;Not foldable and not effect-free because  it invokes an unknown procedure and it
   ;;mutates the operand.
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive vector-map
    (safe)
  (signatures
   ((<procedure> <vector> . <vector>)		=> (<vector>)))
  (attributes
   ;;Not foldable  because it  must return  a new vector  at every  application.  Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-each
    (safe)
  (signatures
   ((<procedure> <vector> . <vector>)		=> (<void>)))
  (attributes
   ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-all
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <vector> . <vector>)		=> (<top>))))

(declare-core-primitive vector-exists
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <vector> . <vector>)		=> (<top>))))

(declare-core-primitive vector-find
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <vector>)			=> (<top>))))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive vector->list
    (safe)
  (signatures
   ((<vector>)			=> (<list>)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; unsafe functions

;;; constructors

(declare-core-primitive $make-vector
    (unsafe)
  (signatures
   ((<non-negative-fixnum>)			=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-vector-predicate $vector-empty? unsafe)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $vector-length
    (unsafe)
  (signatures
   ((<vector>)			=> (<non-negative-fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $vector-ref
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum>)		=> (<top>)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $vector-set!
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> _)	=> (<void>)))
  (attributes
   ((_ _ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive $vector-map1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<vector>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-each1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-all1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<top>))))

(declare-core-primitive $vector-exists1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive <vector>-map
    (safe)
  (signatures
   ((<vector> <procedure> . <vector>)	=> (<vector>))))

(declare-core-primitive <vector>-for-each
    (safe)
  (signatures
   ((<vector> <procedure> . <vector>)	=> (<void>))))

(declare-core-primitive <vector>-for-all
    (safe)
  (signatures
   ((<vector> <procedure> . <vector>)	=> (<top>))))

(declare-core-primitive <vector>-exists
    (safe)
  (signatures
   ((<vector> <procedure> . <vector>)	=> (<top>))))

(declare-core-primitive <vector>-find
    (safe)
  (signatures
   ((<vector> <procedure>>)		=> (<top>))))

(declare-core-primitive <vector>-fold-left
    (safe)
  (signatures
   ((<vector> <procedure> <top> . <vector>)	=> (<top>))))

(declare-core-primitive <vector>-fold-right
    (safe)
  (signatures
   ((<vector> <procedure> <top> . <vector>)	=> (<top>))))

/section)


;;;; core syntactic binding descriptors, typed core primitives: pairs and lists primitives

(section

(declare-core-primitive cons
    (safe)
  (signatures
   ((<top> <list>)		=> (<nlist>))
   ((<top> <top>)		=> (<pair>))))

(declare-core-primitive list
    (safe)
  (signatures
   (()				=> (<null>))
   ((<top> . <top>)		=> (<nlist>))))

;;;

(declare-core-primitive <null>-constructor
    (safe)
  (signatures
   (()				=> (<null>))))

;;;

(declare-type-predicate pair? <pair>)

(declare-type-predicate list? <list>)

(declare-type-predicate nlist? <nlist>)

;;;

(declare-core-primitive car
    (safe)
  (signatures
   ((<pair>)		=> (<top>))
   ((<nlist>)		=> (<top>))
   ;;FIXME Strictly  speaking this is wrong:  "<list>" can also be  null, so applying
   ;;CAR is an error.   This is why the replacement is  commented out.  (Marco Maggi;
   ;;Thu Oct 22, 2015)
   ((<list>)		=> (<top>)))
  ;;(replacements $car)
  #| end of DECLARE-CORE-PRIMITIVE |# )

(declare-core-primitive cdr
    (safe)
  (signatures
   ((<pair>)		=> (<top>))
   ((<nlist>)		=> (<top>))
   ;;FIXME Strictly  speaking this is wrong:  "<list>" can also be  null, so applying
   ;;CDR is an error.   This is why the replacement is  commented out.  (Marco Maggi;
   ;;Thu Oct 22, 2015)
   ((<list>)		=> (<top>)))
  ;;(replacements $cdr)
  #| end of DECLARE-CORE-PRIMITIVE |# )

/section)


;;;; core syntactic binding descriptors, typed core primitives: utilities

(section

(declare-core-primitive <top>-constructor
    (safe)
  (signatures
   ((<top>)		=> (<top>))))

(declare-core-primitive <top>-type-predicate
    (safe)
  (signatures
   ((<top>)		=> (<true>))))

(declare-core-primitive <boolean>-constructor
    (safe)
  (signatures
   ((<top>)		=> (<boolean>))))

/section)


;;;; done

#| close lexical contour |# (void))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
