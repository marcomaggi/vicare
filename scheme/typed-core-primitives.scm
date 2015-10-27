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
     (void))
    ))


;;;; basic definition syntaxes

(define-auxiliary-syntaxes signatures attributes replacements safe unsafe)

(define-syntax (declare-core-primitive stx)
  (define (main input-form.stx)
    (syntax-case input-form.stx (=> replacements signatures attributes)
      ;; signatures
      ((_ ?prim-name ?safety
	  (signatures
	   (?argvals-signature0 => ?retvals-signature0)
	   (?argvals-signature  => ?retvals-signature)
	   ...))
       (%output #'?prim-name #'?safety
		#'((?argvals-signature0 ?retvals-signature0) (?argvals-signature ?retvals-signature) ...)
		#'()))

      ;; signatures, replacements
      ((_ ?prim-name ?safety
	  (signatures
	   (?argvals-signature0 => ?retvals-signature0)
	   (?argvals-signature  => ?retvals-signature)
	   ...)
	  (replacements ?unsafe-prim-name ...))
       (%output #'?prim-name #'?safety
		#'((?argvals-signature0 ?retvals-signature0) (?argvals-signature ?retvals-signature) ...)
		#'(?unsafe-prim-name ...)))

      ;; signatures, attributes
      ((_ ?prim-name ?safety
	  (signatures
	   (?argvals-signature0 => ?retvals-signature0)
	   (?argvals-signature  => ?retvals-signature)
	   ...)
	  (attributes . ?stuff))
       (%output #'?prim-name #'?safety
		#'((?argvals-signature0 ?retvals-signature0) (?argvals-signature ?retvals-signature) ...)
		#'()))

      ;; signatures, attributes, replacements
      ((_ ?prim-name ?safety
	  (signatures
	   (?argvals-signature0 => ?retvals-signature0)
	   (?argvals-signature  => ?retvals-signature)
	   ...)
	  (attributes . ?stuff)
	  (replacements ?unsafe-prim-name ...))
       (%output #'?prim-name #'?safety
		#'((?argvals-signature0 ?retvals-signature0) (?argvals-signature ?retvals-signature) ...)
		#'(?unsafe-prim-name ...)))
      ))

  (define (%output prim-name.id safety.id signatures.stx replacements.stx)
    (define safety.datum
      (%validate-safety safety.id))
    (%validate-signatures signatures.stx)
    (%validate-replacements replacements.stx)
    (with-syntax
	((((?argvals-signature0 ?retvals-signature0) (?argvals-signature ?retvals-signature) ...) signatures.stx))
      #`(set-cons! VICARE-TYPED-CORE-PRIMITIVES
		   (cons* (quote #,prim-name.id)
			  (quote $core-prim-typed)
			  (quote #(#,prim-name.id
				   #,safety.datum
				   ((?retvals-signature0 . ?argvals-signature0)
				    (?retvals-signature  . ?argvals-signature)
				    ...)
				   #,replacements.stx))))))

  (define (%validate-safety safety.stx)
    (syntax-case safety.stx (safe unsafe)
      ((safe)		#t)
      ((unsafe)		#t)
      (_
       (synner "invalid safety specification" safety.stx))))

  (define (%validate-signatures signatures.stx)
    (syntax-case signatures.stx ()
      (()
       (void))

      (((?argvals-signature ?retvals-signature) . ?rest)
       (begin
	 (%validate-type-signature #'?argvals-signature)
	 (%validate-type-signature #'?retvals-signature)
	 (%validate-signatures #'?rest)))

      ((?signature . ?rest)
       (synner "invalid signature specification" #'?signature))))

  (define (%validate-type-signature type-signature.stx)
    (syntax-case type-signature.stx ()
      (()
       (void))
      ((?type0 ?type ...)
       (all-identifiers? #'(?type0 ?type ...))
       (void))
      ((?type0 ?type ... . ?tail-type)
       (all-identifiers? #'(?tail-type ?type0 ?type ...))
       (void))
      (?type
       (identifier? #'?type)
       (void))
      (_
       (synner "invalida type signature" type-signature.stx))))

  (define (%validate-replacements replacements.stx)
    (unless (all-identifiers? replacements.stx)
      (synner "invalid replacements specification" replacements.stx)))

  (case-define synner
    ((message subform)
     (syntax-violation 'declare-core-primitive message stx subform))
    ((message)
     (syntax-violation 'declare-core-primitive message stx)))

  (main stx))


;;;; helpers

(define-syntax declare-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate fixnum? <fixnum>)
  ;;   (declare-type-predicate vector? <vector>)
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

;;; --------------------------------------------------------------------

(define-syntax-rule (declare-hash-function ?prim-name ?type ?safety)
  (declare-core-primitive ?prim-name
      (?safety)
    (signatures
     ((?type)		=> (<fixnum>)))))

(define-syntax-rule (declare-parameter ?prim-name ?type)
  (declare-core-primitive ?prim-name
      (safe)
    (signatures
     (()			=> (?type))
     ((?type)			=> (<void>))
     ((?type <top>)		=> (<void>)))))

;;; --------------------------------------------------------------------

(define-syntax declare-string-predicate
  (syntax-rules (replacements)
    ((_ ?prim-name)
     (declare-core-primitive ?prim-name
         (safe)
       (signatures
	((<string>) => (<boolean>)))))
    ((_ ?prim-name (replacements ?unsafe-prim-name ...))
     (declare-core-primitive ?prim-name
	 (safe)
       (signatures
	((<string>) => (<boolean>)))
       (replacements ?unsafe-prim-name ...)))
    ))


;;;; core syntactic binding descriptors, typed core primitives: miscellanous primitives

(declare-core-primitive void
    (safe)
  (signatures
   (() => (<void>))))

(declare-type-predicate void-object? <void>)


;;;; core syntactic binding descriptors, typed core primitives: char primitives

(declare-core-primitive integer->char
    (safe)
  (signatures
   ((<fixnum>) => (<char>))))

(declare-type-predicate char? <char>)

(declare-hash-function char-hash <char> safe)

(declare-core-primitive char->integer
    (safe)
  (signatures
   ((<char>) => (<fixnum>))))

(declare-core-primitive char->fixnum
    (safe)
  (signatures
   ((<char>) => (<fixnum>))))


;;;; core syntactic binding descriptors, typed core primitives: symbol primitives

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


;;;; core syntactic binding descriptors, typed core primitives: keyword primitives

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


;;;; core syntactic binding descriptors, typed core primitives: pointer primitives

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
   (<pointer*>		=> (<boolean>))))

(declare-core-primitive pointer!=?
    (safe)
  (signatures
   (<pointer*>		=> (<boolean>))))

(declare-core-primitive pointer<?
    (safe)
  (signatures
   (<pointer*>		=> (<boolean>))))

(declare-core-primitive pointer>?
    (safe)
  (signatures
   (<pointer*>		=> (<boolean>))))

(declare-core-primitive pointer<=?
    (safe)
  (signatures
   (<pointer*>		=> (<boolean>))))

(declare-core-primitive pointer>=?
    (safe)
  (signatures
   (<pointer*>		=> (<boolean>))))

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


;;;; core syntactic binding descriptors, typed core primitives: transcoders

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


;;;; core syntactic binding descriptors, typed core primitives: strings

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
   (<char*>		=> (<string>)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   (()			effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive make-string
    (safe)
  (signatures
   ((<fixnum>)		=> (<string>))
   ((<fixnum> <char>)	=> (<string>)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((0)			effect-free result-true)
   ((0 . _)		effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive substring
    (safe)
  (signatures
   ((<string> <fixnum> <fixnum>)	=> (<string>)))
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
   ((<string> <fixnum> <string> <fixnum> <fixnum>)	=> (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive string-append
    (safe)
  (signatures
   (<string*>			=> (<string>)))
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive string-reverse-and-concatenate
    (safe)
  (signatures
   ((<list>)			=> (<string>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive string-length
    (safe)
  (signatures
   ((<string>)		=> (<fixnum>)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

(declare-core-primitive string-for-each
    (safe)
  (signatures
   ((<procedure> <string> . <string*>)		=> (<void>)))
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
   ((<string> <fixnum>)	=> (<char>)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;FIXME This  cannot have  $STRING-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Tue Oct 27, 2015)
(declare-core-primitive string-set!
    (safe)
  (signatures
   ((<string> <fixnum> <char>)	=> (<void>)))
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

(comment
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
/comment)

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
   ;; ((<string> <fixnum>)      => ((or <number> <false>)))
   ((<string>)		=> (<top>))
   ((<string> <fixnum>)	=> (<top>)))
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
   ((<string>)			=> (<list>)))
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
   ((<fixnum>)		=> (<string>)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string
    (unsafe)
  (signatures
   (<char*>		=> (<string>)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   (_			effect-free result-true)))

(declare-core-primitive $string-concatenate
    (unsafe)
  (signatures
   ((<exact-integer> <list>)	=> (<string>)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $string-reverse-and-concatenate
    (unsafe)
  (signatures
   ((<exact-integer> <list>)	=> (<string>)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((<string>)		=> (<fixnum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-total-length
    (unsafe)
  (signatures
   ((<exact-integer> <list>)	=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $string-ref
    (unsafe)
  (signatures
   ((<string> <fixnum>)	=> (<char>)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (unsafe)
  (signatures
   ((<string> <fixnum> <char>)	=> (<void>)))
  (attributes
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(comment
(declare-string-binary-comparison $string=)
/comment)

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
   ((<string> <procedure> . <string*>)	=> (<void>))))


;;;; core syntactic binding descriptors, typed core primitives: pairs and lists primitives

(declare-core-primitive cons
    (safe)
  (signatures
   ((<top> <top>)		=> (<pair>))))

(declare-core-primitive list
    (safe)
  (signatures
   (<list>			=> (<list>))))

;;;

(declare-core-primitive <null>-constructor
    (safe)
  (signatures
   (()				=> (<null>))))

;;;

(declare-type-predicate pair? <pair> <nlist>)

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


;;;; core syntactic binding descriptors, typed core primitives: utilities

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


;;;; done

#| close lexical contour |# (void))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
