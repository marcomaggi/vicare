;;; typed-core-primitives.scm --
;;
;;Typed core primitive properties.


;;;; core syntactic binding descriptors, typed core primitives: miscellanous primitives

(declare-typed-core-prim void
  (signatures
   ((<void>) ())))

(declare-typed-core-prim void-object?
  (signatures
   ((<boolean>) (<top>))))


;;;; core syntactic binding descriptors, typed core primitives: char primitives

(declare-typed-core-prim integer->char
  (signatures
   ((<char>) (<fixnum>))))

(declare-typed-core-prim char?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim string
  (signatures
   ((<string>) <char*>)))

(declare-typed-core-prim char-hash
  (signatures
   ((<fixnum>) (<char>))))

(declare-typed-core-prim char->integer
  (signatures
   ((<fixnum>) (<char>))))

(declare-typed-core-prim char->fixnum
  (signatures
   ((<fixnum>) (<char>))))


;;;; core syntactic binding descriptors, typed core primitives: symbol primitives

(declare-typed-core-prim string->symbol
  (signatures
   ((<symbol>) (<string>))))

(declare-typed-core-prim symbol?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim symbol->string
  (signatures
   ((<string>) (<symbol>))))

(declare-typed-core-prim symbol-hash
  (signatures
   ((<fixnum>) (<symbol>))))

(declare-typed-core-prim symbol-bound?
  (signatures
   ((<boolean>) (<symbol>))))

(declare-typed-core-prim symbol-value
  (signatures
   ((<top>) (<symbol>))))

(declare-typed-core-prim set-symbol-value!
  (signatures
   ((<void>) (<symbol> <top>))))

(declare-typed-core-prim <symbol>-value
  (signatures
   ((<top>) (<symbol>))
   ((<void>) (<symbol> <top>))))

;;;

(declare-typed-core-prim putprop
  (signatures
   ((<void>) (<symbol> <symbol> <top>))))

(declare-typed-core-prim getprop
  (signatures
   ((<top>) (<symbol> <symbol>))))

(declare-typed-core-prim remprop
  (signatures
   ((<void>) (<symbol> <symbol>))))

(declare-typed-core-prim property-list
  (signatures
   ((<list>) (<symbol>))))


;;;; core syntactic binding descriptors, typed core primitives: keyword primitives

(declare-typed-core-prim symbol->keyword
  (signatures
   ((<keyword>) (<symbol>))))

(declare-typed-core-prim keyword?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim keyword->symbol
  (signatures
   ((<symbol>) (<keyword>))))

(declare-typed-core-prim keyword->string
  (signatures
   ((<string>) (<keyword>))))

(declare-typed-core-prim keyword-hash
  (signatures
   ((<fixnum>) (<keyword>))))


;;;; core syntactic binding descriptors, typed core primitives: pointer primitives

(declare-typed-core-prim integer->pointer
  (signatures
   ((<pointer>) (<exact-integer>))))

(declare-typed-core-prim pointer?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim pointer-null?
  (signatures
   ((<boolean>) (<pointer>))))

(declare-typed-core-prim pointer->integer
  (signatures
   ((<exact-integer>) (<pointer>))))

(declare-typed-core-prim pointer=?
  (signatures
   ((<boolean>) <list>)))

(declare-typed-core-prim pointer!=?
  (signatures
   ((<boolean>) <list>)))

(declare-typed-core-prim pointer<?
  (signatures
   ((<boolean>) <list>)))

(declare-typed-core-prim pointer>?
  (signatures
   ((<boolean>) <list>)))

(declare-typed-core-prim pointer<=?
  (signatures
   ((<boolean>) <list>)))

(declare-typed-core-prim pointer>=?
  (signatures
   ((<boolean>) <list>)))

(declare-typed-core-prim pointer-hash
  (signatures
   ((<fixnum>) (<pointer>))))

(declare-typed-core-prim pointer-add
  (signatures
   ((<pointer>) (<pointer> <exact-integer>))))

(declare-typed-core-prim pointer-diff
  (signatures
   ((<pointer>) (<pointer> <pointer>))))

(declare-typed-core-prim pointer-clone
  (signatures
   ((<pointer>) (<pointer>))))

(declare-typed-core-prim set-pointer-null!
  (signatures
   ((<void>) (<pointer>))))


;;;; core syntactic binding descriptors, typed core primitives: transcoders

(declare-typed-core-prim make-transcoder
  (signatures
   ((<transcoder>) (<symbol> <symbol> <symbol>))
   ((<transcoder>) (<symbol> <symbol>))
   ((<transcoder>) (<symbol>))))

(declare-typed-core-prim transcoder?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim transcoder-codec
  (signatures
   ((<symbol>) (<transcoder>))))

(declare-typed-core-prim transcoder-eol-style
  (signatures
   ((<symbol>) (<transcoder>))))

(declare-typed-core-prim transcoder-error-handling-mode
  (signatures
   ((<symbol>) (<transcoder>))))

(declare-typed-core-prim native-transcoder
  (signatures
   ((<transcoder>) ())
   ((<void>) (<transcoder>))
   ((<void>) (<transcoder> <top>))))


;;;; core syntactic binding descriptors, typed core primitives: strings

#|
(declare-type-predicate string? T:string)

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
   (()			=> (T:string))
   (T:char		=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   (()			effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive make-string
    (safe)
  (signatures
   ((T:fixnum)		=> (T:string))
   ((T:fixnum T:char)	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((0)			effect-free result-true)
   ((0 . _)		effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive substring
    (safe)
  (signatures
   ((T:string T:fixnum T:fixnum)	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive string-copy
    (safe)
  (signatures
   ((T:string)		=> (T:void)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string-copy!
    (safe)
  (signatures
   ((T:string T:fixnum T:string T:fixnum T:fixnum)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive string-append
    (safe)
  (signatures
   (T:string			=> (T:string)))
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive string-reverse-and-concatenate
    (safe)
  (signatures
   ((T:proper-list)		=> (T:string)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive string-length
    (safe)
  (signatures
   ((T:string)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

(declare-core-primitive string-for-each
    (safe)
  (signatures
   ((T:procedure T:string . T:string)		=> (T:void)))
  (attributes
   ;;Not foldable and not effect-free because it applies an unknown procedure.
   ((_ _ . _)					result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $STRING-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive string-ref
    (safe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;FIXME This  cannot have  $STRING-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive string-set!
    (safe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive string-fill!
    (safe)
  (signatures
   ((T:string T:char)	=> (T:void)))
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
   ((T:string)		=> (T:flonum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->number
    (safe)
  (signatures
   ((T:string)		=> (T:number/false))
   ((T:string T:fixnum)	=> (T:number/false)))
  (attributes
   ((_)			foldable effect-free)
   ((_ _)		foldable effect-free)))

(declare-core-primitive string->utf8
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf16
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector))
   ((T:string T:symbol)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->utf32
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector))
   ((T:string T:symbol)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->bytevector
    (safe)
  (signatures
   ((T:string T:transcoder)	=> (T:bytevector)))
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
	    ((T:string)		=> (T:bytevector)))
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
   ((T:string)			=> (T:symbol)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string-or-symbol->string
    (safe)
  (signatures
   ((T:string)			=> (T:string))
   ((T:symbol)			=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string-or-symbol->symbol
    (safe)
  (signatures
   ((T:string)			=> (T:symbol))
   ((T:symbol)			=> (T:symbol)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string->keyword
    (safe)
  (signatures
   ((T:string)			=> (T:other-object)))
  (attributes
   ;;Not foldable because keywords cannot be serialised in fasl files.
   ((_)				effect-free result-true)))

(declare-core-primitive string->list
    (safe)
  (signatures
   ((T:string)			=> (T:proper-list)))
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
   ((T:fixnum)		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string
    (unsafe)
  (signatures
   (T:char		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   (_			effect-free result-true)))

(declare-core-primitive $string-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:string)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $string-reverse-and-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:string)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((T:string)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-total-length
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $string-ref
    (unsafe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (unsafe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
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
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->octets
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->latin1
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string-base64->bytevector
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->symbol
    (unsafe)
  (signatures
   ((T:string)		=> (T:symbol)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $interned-strings
    (unsafe)
  (signatures
   (()			=> (T:vector)))
  (attributes
   ((_)			effect-free result-true)))


|#

;;; --------------------------------------------------------------------

(declare-typed-core-prim <string>-for-each
  (signatures
   ((<void>) (<string> <procedure> . <string*>))))


;;;; core syntactic binding descriptors, typed core primitives: pairs and lists primitives

(declare-typed-core-prim cons
  (signatures
   ((<pair>) (<top> <top>))))

(declare-typed-core-prim list
  (signatures
   ((<list>) <list>)))

;;;

(declare-typed-core-prim <null>-constructor
  (signatures
   ((<null>) ())))

;;;

(declare-typed-core-prim pair?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim list?
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim nlist?
  (signatures
   ((<boolean>) (<top>))))

;;;

(declare-typed-core-prim car
  #;(unsafe-variant $car)
  (signatures
   ((<top>) (<pair>))
   ((<top>) (<nlist>))
   ;;FIXME Strictly  speaking this is wrong:  "<list>" can also be  null, so applying
   ;;CAR is  an error.   This is  why the  unsafe variant  is commented  out.  (Marco
   ;;Maggi; Thu Oct 22, 2015)
   ((<top>) (<list>))))

(declare-typed-core-prim cdr
  #;(unsafe-variant $cdr)
  (signatures
   ((<top>) (<pair>))
   ((<top>) (<nlist>))
   ;;FIXME Strictly  speaking this is wrong:  "<list>" can also be  null, so applying
   ;;CDR is  an error.   This is  why the  unsafe variant  is commented  out.  (Marco
   ;;Maggi; Thu Oct 22, 2015)
   ((<top>) (<list>))))


;;;; core syntactic binding descriptors, typed core primitives: utilities

(declare-typed-core-prim <top>-constructor
  (signatures
   ((<top>) (<top>))))

(declare-typed-core-prim <top>-type-predicate
  (signatures
   ((<boolean>) (<top>))))

(declare-typed-core-prim <boolean>-constructor
  (signatures
   ((<boolean>) (<top>))))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
