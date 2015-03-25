;;;Copyright (c) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; helpers

(define-syntax-rule (C ?core-prim)
  (scheme-stx (quote ?core-prim)))

(define-syntax-rule (S ?retvals-signature-syntax ?formals-signature-syntax)
  (make-lambda-signature
   (make-retvals-signature ?retvals-signature-syntax)
   (make-formals-signature ?formals-signature-syntax)))

(define-syntax-rule (register-lambda-signature ?core-prim-id ?lambda-signature)
  (set-identifier-tag! ?core-prim-id (fabricate-procedure-tag-identifier (syntax->datum ?core-prim-id)
									 ?lambda-signature)))


(define (initialise-core-prims-tagging)
  ;; integer->machine-word
  ;; machine-word->integer
  ;; make-list
  ;; last-pair
  ;; bwp-object
  ;; bwp-object?
  ;; weak-cons
  ;; weak-pair?
  ;; uuid
  ;; andmap
  ;; ormap
  ;; fx<
  ;; fx<=
  ;; fx>
  ;; fx>=
  ;; fx=
  ;; fx!=
  ;; fxadd1
  ;; fxsub1
  ;; fxquotient
  ;; fxremainder
  ;; fxmodulo
  ;; fxsign
  ;; fxsll
  ;; fxsra
  ;; sra
  ;; sll
  ;; fxlogand
  ;; fxlogxor
  ;; fxlogor
  ;; fxlognot
  ;; fixnum->string
  ;; string->flonum
  ;; flonum->string
  ;; always-true
  ;; always-false
  ;; add1
  ;; sub1
  ;; bignum?
  ;; ratnum?
  ;; compnum?
  ;; cflonum?
  ;; flonum-parts
  ;; flonum-bytes
  ;; quotient+remainder
  ;; random
  ;; gensym?
  ;; getprop
  ;; putprop
  ;; remprop
  ;; property-list
  ;; gensym->unique-string
  ;; symbol-bound?
  ;; top-level-value
  ;; reset-symbol-proc!
  ;; make-guardian
  ;; port-mode
  ;; set-port-mode!
  ;; with-input-from-string
  ;; get-output-string
  ;; with-output-to-string
  ;; console-input-port
  ;; console-error-port
  ;; console-output-port
  ;; stdin
  ;; stdout
  ;; stderr
  ;; reset-input-port!
  ;; reset-output-port!
  ;; printf
  ;; fprintf
  ;; format
  ;; print-gensym
  ;; print-graph
  ;; print-unicode
  ;; printer-integer-radix
  ;; unicode-printable-char?
  ;; gensym-count
  ;; gensym-prefix
  ;; make-parameter
  ;; call/cf
  ;; print-error
  ;; interrupt-handler
  ;; engine-handler
  ;; assembler-output
  ;; optimizer-output
  ;; assembler-property-key
  ;; new-cafe
  ;; waiter-prompt-string
  ;; readline-enabled?
  ;; readline
  ;; make-readline-input-port
  ;; expand-form-to-core-language
  ;; expand-library
  ;; expand-library->sexp
  ;; expand-top-level
  ;; expand-top-level->sexp
;;;
  ;; environment?
  ;; environment-symbols
  ;; environment-libraries
  ;; environment-labels
  ;; environment-binding
;;;
  ;; time-and-gather
  ;; stats?
  ;; stats-user-secs
  ;; stats-user-usecs
  ;; stats-sys-secs
  ;; stats-sys-usecs
  ;; stats-real-secs
  ;; stats-real-usecs
  ;; stats-collection-id
  ;; stats-gc-user-secs
  ;; stats-gc-user-usecs
  ;; stats-gc-sys-secs
  ;; stats-gc-sys-usecs
  ;; stats-gc-real-secs
  ;; stats-gc-real-usecs
  ;; stats-bytes-minor
  ;; stats-bytes-major
  ;; time-it
  ;; verbose-timer
;;;
  ;; current-time
  ;; time-from-now
  ;; time?
  ;; time-second
  ;; time-nanosecond
  ;; time-gmt-offset
  ;; date-string
  ;; make-time
  ;; time-addition
  ;; time-difference
  ;; time=?
  ;; time<?
  ;; time<=?
  ;; time>?
  ;; time>=?
;;;
  ;; command-line-arguments
  ;; set-rtd-printer!
  ;; set-rtd-destructor!
  ;; struct?
  ;; make-struct-type
  ;; struct-type-descriptor?
  ;; struct-type-name
  ;; struct-type-symbol
  ;; struct-type-field-names
  ;; struct-type-destructor
  ;; default-struct-printer
  ;; struct-constructor
  ;; struct-predicate
  ;; struct-field-accessor
  ;; struct-field-mutator
  ;; struct-length
  ;; struct-ref
  ;; struct-set!
  ;; struct-printer
  ;; struct-destructor
  ;; struct-name
  ;; struct-rtd
  ;; struct=?
  ;; struct-reset
  ;; struct-guardian-logger
  ;; struct-guardian-log
  ;; code?
  ;; immediate?
  ;; pointer-value
;;;
  ;; apropos
  ;; current-primitive-locations
  ;; boot-library-expand
  ;; current-library-collection
  ;; library-name
  ;; find-library-by-name

;;; --------------------------------------------------------------------

  ;; $car
  ;; $cdr
  ;; $set-car!
  ;; $set-cdr!
;;;
  ;; $length
  ;; $map1
  ;; $for-each1
  ;; $for-all1
  ;; $exists1
  ;; $memq
  ;; $memv
;;;
  ;; $char?
  ;; $char=
  ;; $char<
  ;; $char>
  ;; $char<=
  ;; $char>=
  ;; $char->fixnum
  ;; $fixnum->char
;;;
  ;; $make-string
  ;; $string
  ;; $string-ref
  ;; $string-set!

  (let ((P (C $string-length)))
    (register-lambda-signature P (S (list (C <fixnum>))
				    (list (C <string>)))))

  ;; $string-empty?
  ;; $string=
  ;; $string-total-length
  ;; $string-concatenate
  ;; $string-reverse-and-concatenate
  ;; $interned-strings
  ;; $string->ascii
  ;; $ascii->string
  ;; $string->octets
  ;; $octets->string
  ;; $string->latin1
  ;; $latin1->string
  ;; $octets-encoded-string?
  ;; $ascii-encoded-string?
  ;; $latin1-encoded-string?
  ;; $string-base64->bytevector
  ;; $bytevector->string-base64
  ;; $uri-encoded-string?
  ;; $percent-encoded-string?
  ;;     ;;
  ;; $make-bytevector
  ;; $bytevector-length
  ;; $bytevector-empty?
  ;; $bytevector-s8-ref
  ;; $bytevector-u8-ref
  ;; $bytevector-set!
  ;; $bytevector-ieee-double-native-ref
  ;; $bytevector-ieee-double-native-set!
  ;; $bytevector-ieee-double-nonnative-ref
  ;; $bytevector-ieee-double-nonnative-set!
  ;; $bytevector-ieee-single-native-ref
  ;; $bytevector-ieee-single-native-set!
  ;; $bytevector-ieee-single-nonnative-ref
  ;; $bytevector-ieee-single-nonnative-set!
  ;; $bytevector=
  ;; $bytevector-total-length
  ;; $bytevector-concatenate
  ;; $bytevector-reverse-and-concatenate
  ;; $bytevector-copy
  ;; $uri-encode
  ;; $uri-decode
  ;; $uri-encoded-bytevector?
  ;; $uri-normalise-encoding
  ;; $octets-encoded-bytevector?
  ;; $ascii-encoded-bytevector?
  ;; $latin1-encoded-bytevector?
  ;; $percent-encode
  ;; $percent-decode
  ;; $percent-encoded-bytevector?
  ;; $percent-normalise-encoding
  ;; $bytevector->base64
  ;; $base64->bytevector
;;;
  ;; $flonum-u8-ref
  ;; $make-flonum
  ;; $flonum-set!
  ;; $flonum-signed-biased-exponent
  ;; $flonum-rational?
  ;; $flonum-integer?
  ;; $fl+
  ;; $fl-
  ;; $fl*
  ;; $fl/
  ;; $fl=
  ;; $fl<
  ;; $fl<=
  ;; $fl>
  ;; $fl>=
  ;; $fldiv
  ;; $flmod
  ;; $fldiv0
  ;; $flmod0
  ;; $fldiv-and-mod
  ;; $fldiv0-and-mod0
  ;; $fixnum->flonum
  ;; $flonum-sbe
  ;; $flonum->exact
  ;; $flzero?
  ;; $flzero?/positive
  ;; $flzero?/negative
  ;; $flpositive?
  ;; $flnegative?
  ;; $flnonpositive?
  ;; $flnonnegative?
  ;; $fleven?
  ;; $flnan?
  ;; $flfinite?
  ;; $flinfinite?
  ;; $flodd?
  ;; $flround
  ;; $flfloor
  ;; $flceiling
  ;; $fltruncate
  ;; $flnumerator
  ;; $fldenominator
  ;; $flabs
  ;; $flsin
  ;; $flcos
  ;; $fltan
  ;; $flasin
  ;; $flacos
  ;; $flatan
  ;; $flsinh
  ;; $flcosh
  ;; $fltanh
  ;; $flasinh
  ;; $flacosh
  ;; $flatanh
  ;; $flatan2
  ;; $flexp
  ;; $fllog
  ;; $fllog2
  ;; $flexpm1
  ;; $fllog1p
  ;; $flexpt
  ;; $flsqrt
  ;; $flcbrt
  ;; $flsquare
  ;; $flcube
  ;; $flhypot
  ;; $flmax
  ;; $flmin
;;;
  ;; $make-bignum
  ;; $bignum-positive?
  ;; $bignum-negative?
  ;; $bignum-non-positive?
  ;; $bignum-non-negative?
  ;; $bignum-size
  ;; $bignum-byte-ref
  ;; $bignum-byte-set!
  ;; $bignum-even?
  ;; $bignum-odd?
  ;; $bignum->flonum
;;;
  ;; $make-ratnum
  ;; $ratnum-n
  ;; $ratnum-num
  ;; $ratnum-d
  ;; $ratnum-den
  ;; $ratnum->flonum
  ;; $ratnum-positive?
  ;; $ratnum-negative?
  ;; $ratnum-non-positive?
  ;; $ratnum-non-negative?
;;;
  ;; $make-rectangular

  ;; $make-compnum
  ;; $compnum-real
  ;; $compnum-imag

  ;; $make-cflonum
  ;; $cflonum-real
  ;; $cflonum-imag

  ;; $complex-conjugate-compnum
  ;; $complex-conjugate-cflonum

  ;; $angle-fixnum
  ;; $angle-bignum
  ;; $angle-ratnum
  ;; $angle-flonum
  ;; $angle-compnum
  ;; $angle-cflonum

  ;; $magnitude-fixnum
  ;; $magnitude-bignum
  ;; $magnitude-ratnum
  ;; $magnitude-flonum
  ;; $magnitude-compnum
  ;; $magnitude-cflonum
;;;
  ;; $make-vector
  ;; $vector-length
  ;; $vector-empty?
  ;; $vector-ref
  ;; $vector-set!
  ;; $vector-map1
  ;; $vector-for-each1
  ;; $vector-for-all1
  ;; $vector-exists1
;;;
  ;; $fxzero?
  ;; $fxpositive?
  ;; $fxnegative?
  ;; $fxnonpositive?
  ;; $fxnonnegative?
  ;; $fxeven?
  ;; $fxodd?
  ;; $fxadd1
  ;; $fxsub1
  ;; $fx>=
  ;; $fx<=
  ;; $fx>
  ;; $fx<
  ;; $fx=
  ;; $fxmin
  ;; $fxmax
  ;; $fxsll
  ;; $fxsra
  ;; $fxquotient
  ;; $fxmodulo
  ;; $fxremainder
  ;; $fxsign
  ;; $int-quotient
  ;; $int-remainder
  ;; $fxlogxor
  ;; $fxlogor
  ;; $fxlognot
  ;; $fxlogand
  ;; $fx+
  ;; $fx*
  ;; $fx-
  ;; $fxinthash
  ;; $fxdiv
  ;; $fxdiv0
  ;; $fxmod
  ;; $fxmod0
  ;; $fxdiv-and-mod
  ;; $fxdiv0-and-mod0
  ;; $fxabs
  ;; $fxcopy-bit
  ;; $fxcopy-bit-field
  ;; $fxrotate-bit-field
  ;; $fxbit-field
  ;; $fixnum->string
;;;
  ;; $make-symbol
  ;; $string->symbol
  ;; $symbol->string
  ;; $symbol-unique-string
  ;; $symbol-value
  ;; $symbol-proc
  ;; $symbol-string
  ;; $symbol-plist
  ;; $set-symbol-value!
  ;; $set-symbol-proc!
  ;; $set-symbol-string!
  ;; $set-symbol-unique-string!
  ;; $set-symbol-plist!
  ;; $unintern-gensym
  ;; $init-symbol-value!)
  ;; $unbound-object?
  ;; $symbol-table-size
  ;; $log-symbol-table-status
  ;; system-value-gensym
  ;; system-label-gensym
  ;; system-value
  ;; system-label
  ;; $getprop
  ;; $putprop
  ;; $remprop
  ;; $property-list
;;;
  ;; $symbol->keyword
  ;; $keyword->symbol
  ;; $keyword->string
  ;; $keyword-hash
  ;; $keyword=?

;;; --------------------------------------------------------------------

  (let ((P (C base-rtd)))
    (register-lambda-signature P (S (list (C <struct-type-descriptor>))
				    '())))

  (let ((P (C $struct)))
    (register-lambda-signature P (S (list (C <struct>))
				    (cons (C <struct-type-descriptor>) (C <list>)))))

  (let ((P (C $make-struct)))
    (register-lambda-signature P (S (list (C <struct>))
				    (list (C <struct-type-descriptor>) (C <fixnum>)))))

  (let ((P (C $struct?)))
    (register-lambda-signature P (S (list (C <boolean>))
				    (list (C <top>)))))

  (let ((P (C $struct/rtd?)))
    (register-lambda-signature P (S (list (C <boolean>))
				    (list (C <top>) (C <struct-type-descriptor>)))))

  (let ((P (C $struct-guardian)))
    (register-lambda-signature P (S (list (C <top>))
				    (list (C <struct>)))))

  (let ((P (C $struct-rtd)))
    (register-lambda-signature P (S (list (C <struct-type-descriptor>))
				    (list (C <struct>)))))

  (let ((P (C $struct-set!)))
    (register-lambda-signature P (S (list (C <void>))
				    (list (C <struct>) (C <fixnum>) (C <top>)))))

  (let ((P (C $struct-ref)))
    (register-lambda-signature P (S (list (C <top>))
				    (list (C <struct>) (C <fixnum>)))))

  ;; $record-guardian

  ;; $std-std
  ;; $std-name
  ;; $std-length
  ;; $std-fields
  ;; $std-printer
  ;; $std-symbol
  ;; $std-destructor

  ;; $set-std-std!
  ;; $set-std-name!
  ;; $set-std-length!
  ;; $set-std-fields!
  ;; $set-std-printer!
  ;; $set-std-symbol!
  ;; $set-std-destructor!

;;; --------------------------------------------------------------------
;;; (ikarus system $pointers)

  ;; $pointer?
  ;; $pointer=
;;;
  ;; $closure-code
  ;; $code->closure
  ;; $code-reloc-vector
  ;; $code-freevars
  ;; $code-size
  ;; $code-annotation
  ;; $code-ref
  ;; $code-set!
  ;; $set-code-annotation!
  ;; procedure-annotation
  ;; $make-annotated-procedure
  ;; $annotated-procedure-annotation
  ;; $cpref
  ;; $make-tcbucket
  ;; $tcbucket-key
  ;; $tcbucket-val
  ;; $tcbucket-next
  ;; $set-tcbucket-val!
  ;; $set-tcbucket-next!
  ;; $set-tcbucket-tconc!
  ;; $arg-list
  ;; $collect-key
  ;; $$apply
  ;; $fp-at-base
  ;; $primitive-call/cc
  ;; $frame->continuation
  ;; $current-frame
  ;; $seal-frame-and-call
  ;; $make-call-with-values-procedure
  ;; $make-values-procedure
  ;; $interrupted?
  ;; $unset-interrupted!
  ;; $swap-engine-counter!
;;;
  ;; interrupted-condition?
  ;; make-interrupted-condition
  ;; source-position-condition?
  ;; make-source-position-condition
  ;; source-position-port-id
  ;; source-position-byte
  ;; source-position-character
  ;; source-position-line
  ;; source-position-column

  ;; $apply-nonprocedure-error-handler
  ;; $incorrect-args-error-handler
  ;; $multiple-values-error
  ;; $debug
  ;; $underflow-misaligned-error
  ;; top-level-value-error
  ;; car-error
  ;; cdr-error
  ;; fxadd1-error
  ;; fxsub1-error
  ;; cadr-error
  ;; fx+-type-error
  ;; fx+-types-error
  ;; fx+-overflow-error
  ;; $do-event
  ;; do-overflow
  ;; do-overflow-words
  ;; do-vararg-overflow
  ;; collect
  ;; collect-key
  ;; post-gc-hooks
  ;; register-to-avoid-collecting
  ;; forget-to-avoid-collecting
  ;; replace-to-avoid-collecting
  ;; retrieve-to-avoid-collecting
  ;; collection-avoidance-list
  ;; purge-collection-avoidance-list
  ;; do-stack-overflow
  ;; make-promise
  ;; make-traced-procedure
  ;; make-traced-macro
  ;; error@fx+
  ;; error@fxarithmetic-shift-left
  ;; error@fxarithmetic-shift-right
  ;; error@fx*
  ;; error@fx-
  ;; error@add1
  ;; error@sub1
  ;; error@fxadd1
  ;; error@fxsub1
  ;; fasl-write
  ;; fasl-read
  ;; syntax-parameter-value
  ;; <
  ;; <=
  ;; =
  ;; !=
  ;; >
  ;; >=
  ;; +
  ;; -
  ;; *
  ;; /
  ;; abs
  ;; sign
  ;; asin
  ;; acos
  ;; atan
  ;; sinh
  ;; cosh
  ;; tanh
  ;; asinh
  ;; acosh
  ;; atanh
  ;; angle
  ;; bignum->bytevector
  ;; bytevector->bignum
  ;; append
  ;; apply
  ;; assert
  ;; assertion-error
  ;; assertion-violation
  ;; boolean=?
  ;; boolean?

  ;;CAR is  both a core primitive  and a primitive  operation.  At present it  has no
  ;;unsafe variant.
  ;;
  ;;FIXME  The right  way  here is  to  have  a multimethod:  one  method for  <pair>
  ;;argument, which can have an unsafe variant; one method for <list> argument, which
  ;;must not have an unsafe variant because  a <list> can be null.  (Marco Maggi; Sat
  ;;Apr 26, 2014)
  (let ((P (C car)))
    (register-lambda-signature P (S (list (C <top>))
				    (list (C <pair>)))))

  ;;CDR is  both a core primitive  and a primitive  operation.  At present it  has no
  ;;unsafe variant.
  ;;
  ;;FIXME  The right  way  here is  to  have  a multimethod:  one  method for  <pair>
  ;;argument, which can have an unsafe variant; one method for <list> argument, which
  ;;must not have an unsafe variant because  a <list> can be null.  (Marco Maggi; Sat
  ;;Apr 26, 2014)
  (let ((P (C cdr)))
    (register-lambda-signature P (S (list (C <top>))
				    (list (C <pair>)))))

  ;; caar
  ;; cadr
  ;; cdar
  ;; cddr
  ;; caaar
  ;; caadr
  ;; cadar
  ;; caddr
  ;; cdaar
  ;; cdadr
  ;; cddar
  ;; cdddr
  ;; caaaar
  ;; caaadr
  ;; caadar
  ;; caaddr
  ;; cadaar
  ;; cadadr
  ;; caddar
  ;; cadddr
  ;; cdaaar
  ;; cdaadr
  ;; cdadar
  ;; cdaddr
  ;; cddaar
  ;; cddadr
  ;; cdddar
  ;; cddddr
  ;; call-with-current-continuation
  ;; call/cc
  ;; call-with-values
  ;; ceiling
  ;; ;;
  ;; char->integer
  ;; char<=?
  ;; char<?
  ;; char=?
  ;; char>=?
  ;; char>?
  ;; char?
  ;; char-in-ascii-range?
  ;; fixnum-in-character-range?
  ;; ;;
  ;; complex?
  ;; cons
  ;; cos
  ;; denominator
  ;; div
  ;; mod
  ;; div-and-mod
  ;; div0
  ;; mod0
  ;; div0-and-mod0
  ;; eq?
  ;; neq?
  ;; equal?
  ;; eqv?
  ;; error
  ;; warning
  ;; die
  ;; even?
  ;; exact
  ;; exact-integer-sqrt
  ;; exact?
  ;; exp
  ;; expt
  ;; finite?
  ;; floor
  ;; gcd
  ;; imag-part
  ;; inexact
  ;; inexact?
  ;; infinite?
  ;; integer->char
  ;; integer-valued?
  ;; integer?
  ;; exact-integer?
  ;; zero-exact-integer?
  ;; negative-exact-integer?
  ;; positive-exact-integer?
  ;; non-negative-exact-integer?
  ;; non-positive-exact-integer?
  ;; lcm
  ;; length
  ;; list
  ;; list->string
  ;; list->vector
  ;; list-ref
  ;; list-tail
  ;; list?
  ;; log
  ;; magnitude
  ;; make-polar
  ;; make-rectangular
  ;; complex-conjugate
  ;; make-string
  ;; make-vector
  ;; map
  ;; max
  ;; min
  ;; nan?
  ;; negative?
  ;; non-negative?
  ;; not
  ;; null?
  ;; number->string
  ;; number?
  ;; numerator
  ;; odd?
  ;; pair?
  ;; positive?
  ;; non-positive?
  ;; procedure?
  ;; rational-valued?
  ;; rational?
  ;; rationalize
  ;; real-part
  ;; real-valued?
  ;; real?
  ;; reverse
  ;; round
  ;; sin
  ;; sqrt
  ;; cbrt
  ;; square
  ;; cube
  ;; string
  ;; string->list
  ;; string->number
  ;; string->symbol
  ;; string-or-symbol->string
  ;; string-or-symbol->symbol
  ;; string-append
  ;; string-reverse-and-concatenate
  ;; string-copy
  ;; string-for-each

  (let ((P (C string-length)))
    (set-identifier-unsafe-variant! P (C $string-length))
    (register-lambda-signature P (S (list (C <fixnum>))
				    (list (C <string>)))))

  ;; string-empty?
  ;; string-ref
  ;; string<=?
  ;; string<?
  ;; string=?
  ;; string>=?
  ;; string>?
  ;; string?
  ;; substring
  ;; string->latin1
  ;; latin1->string
  ;; latin1-encoded-bytevector?
  ;; ascii-encoded-string?
  ;; latin1-encoded-string?
  ;; string->ascii
  ;; string->octets
  ;; octets-encoded-bytevector?
  ;; octets-encoded-string?
  ;; octets->string
  ;; ascii->string
  ;; ascii-encoded-bytevector?
  ;; bytevector->hex
  ;; hex->bytevector
  ;; string-hex->bytevector
  ;; bytevector->string-hex
  ;; bytevector->base64
  ;; base64->bytevector
  ;; string-base64->bytevector
  ;; bytevector->string-base64
  ;; string->uri-encoding
  ;; uri-encoding->string
  ;; string->percent-encoding
  ;; percent-encoding->string
  ;; uri-encode
  ;; uri-decode
  ;; normalise-uri-encoding
  ;; uri-encoded-bytevector?
  ;; uri-encoded-string?
  ;; percent-encoded-bytevector?
  ;; percent-encoded-string?
  ;; percent-encode
  ;; percent-decode
  ;; normalise-percent-encoding
  ;; symbol->string
  ;; symbol=?
  ;; symbol?
  ;; tan
  ;; truncate
  ;; values
  ;; values->list
  ;; vector
  ;; vector->list
  ;; vector-fill!
  ;; vector-for-each
  ;; vector-length
  ;; vector-empty?
  ;; vector-map
  ;; vector-for-all
  ;; vector-exists
  ;; vector-ref
  ;; vector-set!
  ;; subvector
  ;; vector-append
  ;; vector-copy
  ;; vector-copy!
  ;; vector-resize
  ;; vector?
  ;; zero?
  ;; ...
  ;; =>
  ;; _
  ;; else
  ;; bitwise-arithmetic-shift
  ;; bitwise-arithmetic-shift-left
  ;; bitwise-arithmetic-shift-right
  ;; bitwise-not
  ;; bitwise-and
  ;; bitwise-ior
  ;; bitwise-xor
  ;; bitwise-bit-count
  ;; bitwise-bit-field
  ;; bitwise-bit-set?
  ;; bitwise-copy-bit
  ;; bitwise-copy-bit-field
  ;; bitwise-first-bit-set
  ;; bitwise-if
  ;; bitwise-length
  ;; bitwise-reverse-bit-field
  ;; bitwise-rotate-bit-field
  ;; fixnum?
  ;; fixnum-width
  ;; least-fixnum
  ;; greatest-fixnum
  ;; fx*
  ;; fx*/carry

  ;;FX+ is  both a  core primitive  and a primitive  operation, so  it has  no unsafe
  ;;variant.
  (let ((P (C fx+)))
    (register-lambda-signature P (S (list (C <fixnum>))
				    (list (C <fixnum>) (C <fixnum>)))))

  ;; fx+/carry
  ;; fx-
  ;; fx-/carry
  ;; fx<=?
  ;; fx<?
  ;; fx=?
  ;; fx!=?
  ;; fx>=?
  ;; fx>?
  ;; fxand
  ;; fxarithmetic-shift
  ;; fxarithmetic-shift-left
  ;; fxarithmetic-shift-right
  ;; fxbit-count
  ;; fxbit-field
  ;; fxbit-set?
  ;; fxcopy-bit
  ;; fxcopy-bit-field
  ;; fxdiv
  ;; fxdiv-and-mod
  ;; fxdiv0
  ;; fxdiv0-and-mod0
  ;; fxabs
  ;; fxeven?
  ;; fxfirst-bit-set
  ;; fxif
  ;; fxior
  ;; fxlength
  ;; fxmax
  ;; fxmin
  ;; fxmod
  ;; fxmod0
  ;; fxnot
  ;; fxodd?
  ;; fxreverse-bit-field
  ;; fxrotate-bit-field
  ;; fxxor
  ;; fxzero?
  ;; fxpositive?
  ;; fxnegative?
  ;; fxnonpositive?
  ;; fxnonnegative?
  ;; fixnum->flonum
;;;
  ;; bignum-positive?
  ;; bignum-negative?
  ;; bignum-non-negative?
  ;; bignum-non-positive?
  ;; bignum-odd?
  ;; bignum-even?
  ;; least-positive-bignum
  ;; greatest-negative-bignum
;;;
  ;; fl*
  ;; fl+
  ;; fl-
  ;; fl/
  ;; fl<=?
  ;; fl<?
  ;; fl=?
  ;; fl>=?
  ;; fl>?
  ;; flabs
  ;; flceiling
  ;; fldenominator
  ;; fldiv
  ;; fldiv-and-mod
  ;; fldiv0
  ;; fldiv0-and-mod0
  ;; fleven?
  ;; flexp
  ;; flexpm1
  ;; flexpt
  ;; flfinite?
  ;; flfloor
  ;; flinfinite?
  ;; flinteger?
  ;; fllog
  ;; fllog1p
  ;; flhypot
  ;; flmax
  ;; flmin
  ;; flmod
  ;; flmod0
  ;; flnan?
  ;; flnegative?
  ;; flnumerator
  ;; flodd?
  ;; flonum?
  ;; flpositive?
  ;; flnonpositive?
  ;; flnonnegative?
  ;; flround
  ;; flsin
  ;; flcos
  ;; fltan
  ;; flacos
  ;; flasin
  ;; flatan
  ;; flsinh
  ;; flcosh
  ;; fltanh
  ;; flacosh
  ;; flasinh
  ;; flatanh
  ;; flsqrt
  ;; flcbrt
  ;; flsquare
  ;; flcube
  ;; fltruncate
  ;; flzero?
  ;; flzero?/positive
  ;; flzero?/negative
  ;; real->flonum
  ;; bytevector->flonum
  ;; flonum->bytevector
  ;; make-no-infinities-violation
  ;; make-no-nans-violation
  ;; &no-infinities
  ;; no-infinities-violation?
  ;; &no-nans
  ;; no-nans-violation?
  ;; bytevector->sint-list
  ;; bytevector->u8-list
  ;; bytevector->s8-list
  ;; bytevector->u16l-list
  ;; bytevector->u16b-list
  ;; bytevector->u16n-list
  ;; bytevector->s16l-list
  ;; bytevector->s16b-list
  ;; bytevector->s16n-list
  ;; bytevector->u32l-list
  ;; bytevector->u32b-list
  ;; bytevector->u32n-list
  ;; bytevector->s32l-list
  ;; bytevector->s32b-list
  ;; bytevector->s32n-list
  ;; bytevector->u64l-list
  ;; bytevector->u64b-list
  ;; bytevector->u64n-list
  ;; bytevector->s64l-list
  ;; bytevector->s64b-list
  ;; bytevector->s64n-list
  ;; bytevector->uint-list
  ;; bytevector->f4l-list
  ;; bytevector->f4b-list
  ;; bytevector->f4n-list
  ;; bytevector->f8l-list
  ;; bytevector->f8b-list
  ;; bytevector->f8n-list
  ;; bytevector->c4l-list
  ;; bytevector->c4b-list
  ;; bytevector->c4n-list
  ;; bytevector->c8l-list
  ;; bytevector->c8b-list
  ;; bytevector->c8n-list
  ;; bytevector-copy
  ;; string-copy!
  ;; bytevector-copy!
  ;; bytevector-fill!
  ;; bytevector-ieee-double-native-ref
  ;; bytevector-ieee-double-native-set!
  ;; bytevector-ieee-double-ref
  ;; bytevector-ieee-double-set!
  ;; bytevector-ieee-single-native-ref
  ;; bytevector-ieee-single-native-set!
  ;; bytevector-ieee-single-ref
  ;; bytevector-ieee-single-set!
  ;; bytevector-length
  ;; bytevector-length?
  ;; bytevector-index?
  ;; bytevector-word-size?
  ;; bytevector-word-count?
  ;; bytevector-index-for-word?
  ;; bytevector-index-for-word8?
  ;; bytevector-index-for-word16?
  ;; bytevector-index-for-word32?
  ;; bytevector-index-for-word64?
  ;; bytevector-start-index-and-count-for-word?
  ;; bytevector-start-index-and-count-for-word8?
  ;; bytevector-start-index-and-count-for-word16?
  ;; bytevector-start-index-and-count-for-word32?
  ;; bytevector-start-index-and-count-for-word64?
  ;; list-of-bytevectors?
  ;; bytevector-empty?
  ;; bytevector-s16-native-ref
  ;; bytevector-s16-native-set!
  ;; bytevector-s16-ref
  ;; bytevector-s16-set!
  ;; bytevector-s32-native-ref
  ;; bytevector-s32-native-set!
  ;; bytevector-s32-ref
  ;; bytevector-s32-set!
  ;; bytevector-s64-native-ref
  ;; bytevector-s64-native-set!
  ;; bytevector-s64-ref
  ;; bytevector-s64-set!
  ;; bytevector-s8-ref
  ;; bytevector-s8-set!
  ;; bytevector-sint-ref
  ;; bytevector-sint-set!
  ;; bytevector-u16-native-ref
  ;; bytevector-u16-native-set!
  ;; bytevector-u16-ref
  ;; bytevector-u16-set!
  ;; bytevector-u32-native-ref
  ;; bytevector-u32-native-set!
  ;; bytevector-u32-ref
  ;; bytevector-u32-set!
  ;; bytevector-u64-native-ref
  ;; bytevector-u64-native-set!
  ;; bytevector-u64-ref
  ;; bytevector-u64-set!
  ;; bytevector-u8-ref
  ;; bytevector-u8-set!
  ;; bytevector-uint-ref
  ;; bytevector-uint-set!
  ;; f4l-list->bytevector
  ;; f4b-list->bytevector
  ;; f4n-list->bytevector
  ;; f8l-list->bytevector
  ;; f8b-list->bytevector
  ;; f8n-list->bytevector
  ;; c4l-list->bytevector
  ;; c4b-list->bytevector
  ;; c4n-list->bytevector
  ;; c8l-list->bytevector
  ;; c8b-list->bytevector
  ;; c8n-list->bytevector
  ;; bytevector=?
  ;; bytevector?
  ;; subbytevector-u8
  ;; subbytevector-u8/count
  ;; subbytevector-s8
  ;; subbytevector-s8/count
  ;; bytevector-append
  ;; bytevector-reverse-and-concatenate
  ;; native-endianness
  ;; sint-list->bytevector
  ;; string->utf16
  ;; string->utf32
  ;; string->utf8
  ;; string->utf16le
  ;; string->utf16be
  ;; string->utf16n
  ;; u8-list->bytevector
  ;; s8-list->bytevector
  ;; u16l-list->bytevector
  ;; u16b-list->bytevector
  ;; u16n-list->bytevector
  ;; s16l-list->bytevector
  ;; s16b-list->bytevector
  ;; s16n-list->bytevector
  ;; u32l-list->bytevector
  ;; u32b-list->bytevector
  ;; u32n-list->bytevector
  ;; s32l-list->bytevector
  ;; s32b-list->bytevector
  ;; s32n-list->bytevector
  ;; u64l-list->bytevector
  ;; u64b-list->bytevector
  ;; u64n-list->bytevector
  ;; s64l-list->bytevector
  ;; s64b-list->bytevector
  ;; s64n-list->bytevector
  ;; uint-list->bytevector
  ;; utf8->string
  ;; utf16->string
  ;; utf16le->string
  ;; utf16n->string
  ;; utf16be->string
  ;; utf32->string
  ;; print-condition
  ;; condition?
  ;; &assertion
  ;; assertion-violation?
  ;; &condition
  ;; condition
  ;; condition-accessor
  ;; condition-irritants
  ;; condition-message
  ;; condition-predicate
  ;; condition-who
  ;; define-condition-type
  ;; &error
  ;; error?
  ;; &implementation-restriction
  ;; implementation-restriction-violation?
  ;; &irritants
  ;; irritants-condition?
  ;; &lexical
  ;; lexical-violation?
  ;; make-assertion-violation
  ;; make-error
  ;; make-implementation-restriction-violation
  ;; make-irritants-condition
  ;; make-lexical-violation
  ;; make-message-condition
  ;; make-non-continuable-violation
  ;; make-serious-condition
  ;; make-syntax-violation
  ;; make-undefined-violation
  ;; make-violation
  ;; make-warning
  ;; make-who-condition
  ;; &message
  ;; message-condition?
  ;; &non-continuable
  ;; non-continuable-violation?
  ;; &serious
  ;; serious-condition?
  ;; simple-conditions
  ;; &syntax
  ;; syntax-violation-form
  ;; syntax-violation-subform
  ;; syntax-violation?
  ;; &undefined
  ;; undefined-violation?
  ;; &violation
  ;; violation?
  ;; &warning
  ;; warning?
  ;; &who
  ;; who-condition?
  ;; case-lambda
  ;; do
  ;; unless
  ;; when
  ;; define-enumeration
  ;; enum-set->list
  ;; enum-set-complement
  ;; enum-set-constructor
  ;; enum-set-difference
  ;; enum-set-indexer
  ;; enum-set-intersection
  ;; enum-set-member?
  ;; enum-set-projection
  ;; enum-set-subset?
  ;; enum-set-union
  ;; enum-set-universe
  ;; enum-set=?
  ;; make-enumeration
  ;; enum-set?
  ;; environment
  ;; eval
  ;; raise
  ;; raise-continuable
  ;; with-exception-handler
  ;; guard
  ;; binary-port?
  ;; buffer-mode
  ;; buffer-mode?
  ;; bytevector->string
  ;; call-with-bytevector-output-port
  ;; call-with-port
  ;; call-with-string-output-port
  ;; assoc
  ;; assp
  ;; assq
  ;; assv
  ;; cons*
  ;; filter
  ;; find
  ;; fold-left
  ;; fold-right
  ;; for-all
  ;; exists
  ;; member
  ;; memp
  ;; memq
  ;; memv
  ;; partition
  ;; remq
  ;; remp
  ;; remv
  ;; remove
  ;; make-queue-procs

  ;;SET-CAR! is both a core primitive and  a primitive operation, so it has no unsafe
  ;;variant.
  (let ((P (C set-car!)))
    (register-lambda-signature P (S (list (C <void>))
				    (list (C <pair>) (C <top>)))))

  ;;SET-CDR! is both a core primitive and  a primitive operation, so it has no unsafe
  ;;variant.
  (let ((P (C set-cdr!)))
    (register-lambda-signature P (S (list (C <void>))
				    (list (C <pair>) (C <top>)))))

  ;; string-set!
  ;; string-fill!
  ;; command-line
  ;; exit
  ;; exit-hooks
  ;; delay
  ;; exact->inexact
  ;; force
  ;; inexact->exact
  ;; modulo
  ;; remainder
  ;; null-environment
  ;; promise?
  ;; quotient
  ;; scheme-report-environment
  ;; interaction-environment
  ;; new-interaction-environment
  ;; close-port
  ;; eol-style
  ;; error-handling-mode
  ;; file-options
  ;; flush-output-port
  ;; get-bytevector-all
  ;; get-bytevector-n
  ;; get-bytevector-n!
  ;; get-bytevector-some
  ;; get-char
  ;; get-datum
  ;; get-line
  ;; read-line
  ;; get-string-all
  ;; get-string-n
  ;; get-string-n!
  ;; get-string-some
  ;; get-u8
  ;; &i/o
  ;; &i/o-decoding
  ;; i/o-decoding-error?
  ;; &i/o-encoding
  ;; i/o-encoding-error-char
  ;; i/o-encoding-error?
  ;; i/o-error-filename
  ;; i/o-error-port
  ;; i/o-error-position
  ;; i/o-error?
  ;; &i/o-file-already-exists
  ;; i/o-file-already-exists-error?
  ;; &i/o-file-does-not-exist
  ;; i/o-file-does-not-exist-error?
  ;; &i/o-file-is-read-only
  ;; i/o-file-is-read-only-error?
  ;; &i/o-file-protection
  ;; i/o-file-protection-error?
  ;; &i/o-filename
  ;; i/o-filename-error?
  ;; &i/o-invalid-position
  ;; i/o-invalid-position-error?
  ;; &i/o-port
  ;; i/o-port-error?
  ;; &i/o-read
  ;; i/o-read-error?
  ;; &i/o-write
  ;; i/o-write-error?
  ;; &i/o-eagain
  ;; i/o-eagain-error?
  ;; &errno
  ;; errno-condition?
  ;; &h_errno
  ;; h_errno-condition?
  ;; &procedure-argument-violation
  ;; procedure-argument-violation?
  ;; &expression-return-value-violation
  ;; expression-return-value-violation?
  ;; lookahead-char
  ;; lookahead-u8
  ;; lookahead-two-u8
  ;; make-bytevector
  ;; make-custom-binary-input-port
  ;; make-custom-binary-output-port
  ;; make-custom-textual-input-port
  ;; make-custom-textual-output-port
  ;; make-custom-binary-input/output-port
  ;; make-custom-textual-input/output-port
  ;; make-binary-file-descriptor-input-port
  ;; make-binary-file-descriptor-input-port*
  ;; make-binary-file-descriptor-output-port
  ;; make-binary-file-descriptor-output-port*
  ;; make-binary-file-descriptor-input/output-port
  ;; make-binary-file-descriptor-input/output-port*
  ;; make-binary-socket-input-port
  ;; make-binary-socket-input-port*
  ;; make-binary-socket-output-port
  ;; make-binary-socket-output-port*
  ;; make-binary-socket-input/output-port
  ;; make-binary-socket-input/output-port*
  ;; make-textual-file-descriptor-input-port
  ;; make-textual-file-descriptor-input-port*
  ;; make-textual-file-descriptor-output-port
  ;; make-textual-file-descriptor-output-port*
  ;; make-textual-file-descriptor-input/output-port
  ;; make-textual-file-descriptor-input/output-port*
  ;; make-textual-socket-input-port
  ;; make-textual-socket-input-port*
  ;; make-textual-socket-output-port
  ;; make-textual-socket-output-port*
  ;; make-textual-socket-input/output-port
  ;; make-textual-socket-input/output-port*
  ;; make-i/o-decoding-error
  ;; make-i/o-encoding-error
  ;; make-i/o-error
  ;; make-i/o-file-already-exists-error
  ;; make-i/o-file-does-not-exist-error
  ;; make-i/o-file-is-read-only-error
  ;; make-i/o-file-protection-error
  ;; make-i/o-filename-error
  ;; make-i/o-invalid-position-error
  ;; make-i/o-port-error
  ;; make-i/o-read-error
  ;; make-i/o-write-error
  ;; make-i/o-eagain
  ;; make-errno-condition
  ;; condition-errno
  ;; make-h_errno-condition
  ;; condition-h_errno
  ;; make-procedure-argument-violation
  ;; procedure-argument-violation
  ;; make-expression-return-value-violation
  ;; expression-return-value-violation
  ;; latin-1-codec
  ;; make-transcoder
  ;; native-eol-style
  ;; native-transcoder
  ;; transcoder?
  ;; open-bytevector-input-port
  ;; open-bytevector-output-port
  ;; open-file-input-port
  ;; open-file-input/output-port
  ;; open-file-output-port
  ;; open-string-input-port
  ;; open-string-output-port
  ;; bytevector-port-buffer-size
  ;; string-port-buffer-size
  ;; input-file-buffer-size
  ;; output-file-buffer-size
  ;; input/output-file-buffer-size
  ;; input/output-socket-buffer-size
  ;; output-port-buffer-mode
  ;; set-port-buffer-mode!
  ;; port-eof?
  ;; port-has-port-position?
  ;; port-has-set-port-position!?
  ;; port-position
  ;; get-char-and-track-textual-position
  ;; port-textual-position
  ;; port-transcoder
  ;; port?
  ;; put-bytevector
  ;; put-char
  ;; put-datum
  ;; put-string
  ;; put-u8
  ;; set-port-position!
  ;; standard-error-port
  ;; standard-input-port
  ;; standard-output-port
  ;; string->bytevector
  ;; textual-port?
  ;; transcoded-port
  ;; transcoder-codec
  ;; transcoder-eol-style
  ;; transcoder-error-handling-mode
  ;; utf-8-codec
  ;; utf-16-codec
  ;; utf-16le-codec
  ;; utf-16be-codec
  ;; utf-16n-codec
  ;; utf-bom-codec
  ;; would-block-object
  ;; would-block-object?
  ;; input-port?
  ;; output-port?
  ;; input/output-port?
  ;; binary-input-port?
  ;; textual-input-port?
  ;; binary-output-port?
  ;; textual-output-port?
  ;; binary-input/output-port?
  ;; textual-input/output-port?
  ;; current-input-port
  ;; current-output-port
  ;; current-error-port
  ;; eof-object
  ;; eof-object?
  ;; close-input-port
  ;; close-output-port
  ;; display
  ;; newline
  ;; open-input-file
  ;; open-output-file
  ;; peek-char
  ;; read
  ;; read-char
  ;; write
  ;; write-char
  ;; call-with-input-file
  ;; call-with-output-file
  ;; hashtable-clear!
  ;; hashtable-contains?
  ;; hashtable-copy
  ;; hashtable-delete!
  ;; hashtable-entries
  ;; hashtable-keys
  ;; hashtable-mutable?
  ;; hashtable-ref
  ;; hashtable-set!
  ;; hashtable-size
  ;; hashtable-update!
  ;; hashtable?
  ;; make-eq-hashtable
  ;; make-eqv-hashtable
  ;; hashtable-hash-function
  ;; make-hashtable
  ;; hashtable-equivalence-function
  ;; equal-hash
  ;; string-hash
  ;; string-ci-hash
  ;; symbol-hash
  ;; bytevector-hash
  ;; list-sort
  ;; vector-sort
  ;; vector-sort!
  ;; file-exists?
  ;; directory-exists?
  ;; delete-file

;;; --------------------------------------------------------------------

  ;; record-field-mutable?
  ;; record-rtd
  ;; record-type-field-names
  ;; record-type-generative?
  ;; record-type-name
  ;; record-type-opaque?
  ;; record-type-parent
  ;; record-type-sealed?
  ;; record-type-uid
  ;; record?
  ;; make-record-constructor-descriptor
  ;; make-record-type-descriptor
  ;; record-constructor

  (let ((P (C record-predicate)))
    (register-lambda-signature P (S (list (C <predicate>))
				    (list (C <record-type-descriptor>)))))

  ;; record-type-descriptor?
  ;; record-destructor-set!
  ;; record-destructor
  ;; record-guardian-logger
  ;; record-guardian-log
  ;; record-reset
  ;; record-and-rtd?
  ;; record-accessor
  ;; record-mutator
  ;; unsafe-record-accessor
  ;; unsafe-record-mutator

;;; --------------------------------------------------------------------

  ;; syntax-violation
  ;; bound-identifier=?
  ;; datum->syntax
  ;; syntax->datum
  ;; free-identifier=?
  ;; generate-temporaries
  ;; identifier?
  ;; identifier-bound?
  ;; make-variable-transformer
  ;; variable-transformer?
  ;; variable-transformer-procedure
  ;; make-synonym-transformer
  ;; synonym-transformer?
  ;; synonym-transformer-identifier
  ;; make-compile-time-value
  ;; compile-time-value?
  ;; compile-time-value-object
  ;; syntactic-binding-putprop
  ;; syntactic-binding-getprop
  ;; syntactic-binding-remprop
  ;; syntactic-binding-property-list
  ;; syntax-object?
  ;; syntax-object-expression
  ;; syntax-object-marks
  ;; syntax-object-ribs
  ;; syntax-object-source-objects
  ;; char-alphabetic?
  ;; char-ci<=?
  ;; char-ci<?
  ;; char-ci=?
  ;; char-ci>=?
  ;; char-ci>?
  ;; char-downcase
  ;; char-foldcase
  ;; char-titlecase
  ;; char-upcase
  ;; char-general-category
  ;; char-lower-case?
  ;; char-numeric?
  ;; char-title-case?
  ;; char-upper-case?
  ;; char-whitespace?
  ;; string-ci<=?
  ;; string-ci<?
  ;; string-ci=?
  ;; string-ci>=?
  ;; string-ci>?
  ;; string-downcase
  ;; string-foldcase
  ;; string-normalize-nfc
  ;; string-normalize-nfd
  ;; string-normalize-nfkc
  ;; string-normalize-nfkd
  ;; string-titlecase
  ;; string-upcase
  ;; load
  ;; void
  ;; gensym
  ;; symbol-value
  ;; set-symbol-value!
  ;; unbound-object
  ;; unbound-object?
  ;; eval-core
  ;; current-core-eval
  ;; pretty-print
  ;; pretty-print*
  ;; debug-print
  ;; debug-print-enabled?
  ;; debug-print*
  ;; pretty-format
  ;; pretty-width
  ;; library
  ;; $transcoder->data
  ;; $data->transcoder
  ;; make-file-options
;;;
  ;; set-identifier-unsafe-variant!
;;;
  ;; set-predicate-procedure-argument-validation!
  ;; set-predicate-return-value-validation!
;;;
  ;; push-compensation
  ;; run-compensations
  ;; compensations
  ;; run-compensations-store
  ;; push-compensation-thunk
;;;
  ;; port-id
  ;; port-uid
  ;; port-hash
  ;; port-fd
  ;; port-set-non-blocking-mode!
  ;; port-unset-non-blocking-mode!
  ;; port-in-non-blocking-mode?
  ;; port-putprop
  ;; port-getprop
  ;; port-remprop
  ;; port-property-list
  ;; string->filename-func
  ;; filename->string-func
  ;; string->pathname-func
  ;; pathname->string-func
  ;; port-dump-status
  ;; port-closed?
;;; (ikarus system $io)
  ;; $make-port
  ;; $port-tag
  ;; $port-id
  ;; $port-cookie
  ;; $port-transcoder
  ;; $port-index
  ;; $port-size
  ;; $port-buffer
  ;; $port-get-position
  ;; $port-set-position!
  ;; $port-close
  ;; $port-read!
  ;; $port-write!
  ;; $set-port-index!
  ;; $set-port-size!
  ;; $port-attrs
  ;; $set-port-attrs!
;;;
  ;; get-annotated-datum
  ;; annotation?
  ;; annotation-expression
  ;; annotation-source
  ;; annotation-stripped
  ;; annotation-textual-position

;;; --------------------------------------------------------------------
;;; keywords

  ;; symbol->keyword
  ;; keyword->symbol
  ;; keyword->string
  ;; keyword?
  ;; keyword=?
  ;; keyword-hash

;;; --------------------------------------------------------------------
;;; configuration options

  ;; vicare-built-with-ffi-enabled
  ;; vicare-built-with-iconv-enabled
  ;; vicare-built-with-posix-enabled
  ;; vicare-built-with-glibc-enabled
  ;; vicare-built-with-linux-enabled
  ;; vicare-built-with-srfi-enabled

  ;; vicare-built-with-arguments-validation-enabled

;;; --------------------------------------------------------------------
;;; POSIX functions

  ;; strerror
  ;; errno->string
  ;; getenv
  ;; environ
  ;; mkdir
  ;; mkdir/parents
  ;; real-pathname
  ;; file-pathname?
  ;; file-string-pathname?
  ;; file-bytevector-pathname?
  ;; file-absolute-pathname?
  ;; file-relative-pathname?
  ;; file-colon-search-path?
  ;; file-string-colon-search-path?
  ;; file-bytevector-colon-search-path?
  ;; file-modification-time
  ;; split-pathname-root-and-tail
  ;; search-file-in-environment-path
  ;; search-file-in-list-path
  ;; split-pathname
  ;; split-pathname-bytevector
  ;; split-pathname-string
  ;; split-search-path
  ;; split-search-path-bytevector
  ;; split-search-path-string
  ;; vicare-argv0
  ;; vicare-argv0-string

;;; --------------------------------------------------------------------
;;; environment inquiry

  ;; uname
  ;; utsname?
  ;; utsname-sysname
  ;; utsname-nodename
  ;; utsname-release
  ;; utsname-version
  ;; utsname-machine

  ;; implementation-name
  ;; implementation-version
  ;; cpu-architecture
  ;; machine-name
  ;; os-name
  ;; os-version

  ;; host-info

;;; --------------------------------------------------------------------
;;; (ikarus system $foreign)

  ;; errno
  ;; pointer?
  ;; null-pointer
  ;; pointer->integer
  ;; integer->pointer
  ;; pointer-clone
  ;; pointer-null?
  ;; pointer-diff
  ;; pointer-add
  ;; pointer-and-offset?
  ;; pointer=?
  ;; pointer!=?
  ;; pointer<?
  ;; pointer>?
  ;; pointer<=?
  ;; pointer>=?
  ;; set-pointer-null!
;;;
  ;; make-memory-block
  ;; make-memory-block/guarded
  ;; null-memory-block
  ;; memory-block?
  ;; memory-block?/non-null
  ;; memory-block?/not-null
  ;; memory-block-pointer
  ;; memory-block-size
  ;; memory-block-reset
;;;
  ;; make-out-of-memory-error
  ;; out-of-memory-error?
  ;; out-of-memory-error.old-pointer
  ;; out-of-memory-error.number-of-bytes
  ;; out-of-memory-error.clean?
  ;; malloc
  ;; realloc
  ;; calloc
  ;; guarded-malloc
  ;; guarded-realloc
  ;; guarded-calloc
  ;; malloc*
  ;; realloc*
  ;; calloc*
  ;; guarded-malloc*
  ;; guarded-realloc*
  ;; guarded-calloc*
  ;; free
  ;; memcpy
  ;; memcmp
  ;; memmove
  ;; memset
  ;; memory-copy
  ;; memory->bytevector
  ;; bytevector->memory
  ;; bytevector->guarded-memory
  ;; bytevector->memory*
  ;; bytevector->guarded-memory*
;;;
  ;; bytevector->cstring
  ;; bytevector->guarded-cstring
  ;; cstring->bytevector
  ;; cstring16->bytevector
  ;; cstring16n->string
  ;; cstring16le->string
  ;; cstring16be->string
  ;; string->cstring
  ;; string->guarded-cstring
  ;; bytevector->cstring*
  ;; bytevector->guarded-cstring*
  ;; cstring->bytevector*
  ;; string->cstring*
  ;; string->guarded-cstring*
  ;; cstring->string
  ;; strlen
  ;; strcmp
  ;; strncmp
  ;; strdup
  ;; strndup
  ;; guarded-strdup
  ;; guarded-strndup
  ;; strdup*
  ;; strndup*
  ;; guarded-strdup*
  ;; guarded-strndup*

  ;; argv->bytevectors
  ;; argv-length
  ;; argv->strings
  ;; bytevectors->argv
  ;; bytevectors->argv*
  ;; bytevectors->guarded-argv
  ;; bytevectors->guarded-argv*
  ;; strings->argv
  ;; strings->argv*
  ;; strings->guarded-argv
  ;; strings->guarded-argv*

;;;
  ;; pointer-ref-c-uint8
  ;; pointer-ref-c-sint8
  ;; pointer-ref-c-uint16
  ;; pointer-ref-c-sint16
  ;; pointer-ref-c-uint32
  ;; pointer-ref-c-sint32
  ;; pointer-ref-c-uint64
  ;; pointer-ref-c-sint64
;;;
  ;; pointer-ref-c-signed-char
  ;; pointer-ref-c-signed-short
  ;; pointer-ref-c-signed-int
  ;; pointer-ref-c-signed-long
  ;; pointer-ref-c-signed-long-long
  ;; pointer-ref-c-unsigned-char
  ;; pointer-ref-c-unsigned-short
  ;; pointer-ref-c-unsigned-int
  ;; pointer-ref-c-unsigned-long
  ;; pointer-ref-c-unsigned-long-long
;;;
  ;; pointer-ref-c-float
  ;; pointer-ref-c-double
  ;; pointer-ref-c-pointer
;;;
  ;; pointer-ref-c-size_t
  ;; pointer-ref-c-ssize_t
  ;; pointer-ref-c-off_t
  ;; pointer-ref-c-ptrdiff_t
;;;
  ;; pointer-set-c-uint8!
  ;; pointer-set-c-sint8!
  ;; pointer-set-c-uint16!
  ;; pointer-set-c-sint16!
  ;; pointer-set-c-uint32!
  ;; pointer-set-c-sint32!
  ;; pointer-set-c-uint64!
  ;; pointer-set-c-sint64!
;;;
  ;; pointer-set-c-signed-char!
  ;; pointer-set-c-signed-short!
  ;; pointer-set-c-signed-int!
  ;; pointer-set-c-signed-long!
  ;; pointer-set-c-signed-long-long!
  ;; pointer-set-c-unsigned-char!
  ;; pointer-set-c-unsigned-short!
  ;; pointer-set-c-unsigned-int!
  ;; pointer-set-c-unsigned-long!
  ;; pointer-set-c-unsigned-long-long!
;;;
  ;; pointer-set-c-float!
  ;; pointer-set-c-double!
  ;; pointer-set-c-pointer!
;;;
  ;; pointer-set-c-size_t!
  ;; pointer-set-c-ssize_t!
  ;; pointer-set-c-off_t!
  ;; pointer-set-c-ptrdiff_t!
;;;
  ;; array-ref-c-uint8
  ;; array-ref-c-sint8
  ;; array-ref-c-uint16
  ;; array-ref-c-sint16
  ;; array-ref-c-uint32
  ;; array-ref-c-sint32
  ;; array-ref-c-uint64
  ;; array-ref-c-sint64
;;;
  ;; array-ref-c-signed-char
  ;; array-ref-c-unsigned-char
  ;; array-ref-c-signed-short
  ;; array-ref-c-unsigned-short
  ;; array-ref-c-signed-int
  ;; array-ref-c-unsigned-int
  ;; array-ref-c-signed-long
  ;; array-ref-c-unsigned-long
  ;; array-ref-c-signed-long-long
  ;; array-ref-c-unsigned-long-long
;;;
  ;; array-ref-c-float
  ;; array-ref-c-double
  ;; array-ref-c-pointer
;;;
  ;; array-ref-c-size_t
  ;; array-ref-c-ssize_t
  ;; array-ref-c-off_t
  ;; array-ref-c-ptrdiff_t
;;;
  ;; array-set-c-uint8!
  ;; array-set-c-sint8!
  ;; array-set-c-uint16!
  ;; array-set-c-sint16!
  ;; array-set-c-uint32!
  ;; array-set-c-sint32!
  ;; array-set-c-uint64!
  ;; array-set-c-sint64!
;;;
  ;; array-set-c-signed-char!
  ;; array-set-c-unsigned-char!
  ;; array-set-c-signed-short!
  ;; array-set-c-unsigned-short!
  ;; array-set-c-signed-int!
  ;; array-set-c-unsigned-int!
  ;; array-set-c-signed-long!
  ;; array-set-c-unsigned-long!
  ;; array-set-c-signed-long-long!
  ;; array-set-c-unsigned-long-long!
;;;
  ;; array-set-c-float!
  ;; array-set-c-double!
  ;; array-set-c-pointer!
;;;
  ;; array-set-c-size_t!
  ;; array-set-c-ssize_t!
  ;; array-set-c-off_t!
  ;; array-set-c-ptrdiff_t!
;;;
  ;; dlopen
  ;; dlerror
  ;; dlclose
  ;; dlsym
;;;
  ;; make-c-callout-maker
  ;; make-c-callout-maker/with-errno
  ;; make-c-callback-maker
  ;; free-c-callback

;;; --------------------------------------------------------------------
;;; syntax utilities

  ;; identifier->string
  ;; string->identifier
  ;; identifier-prefix
  ;; identifier-suffix
  ;; identifier-append
  ;; identifier-format
  ;; duplicate-identifiers?
  ;; delete-duplicate-identifiers
  ;; identifier-memq

  ;; identifier-record-constructor
  ;; identifier-record-predicate
  ;; identifier-record-field-accessor
  ;; identifier-record-field-mutator

  ;; identifier-struct-constructor
  ;; identifier-struct-predicate
  ;; identifier-struct-field-accessor
  ;; identifier-struct-field-mutator

  ;; syntax-car
  ;; syntax-cdr
  ;; syntax->list
  ;; identifiers->list
  ;; all-identifiers?

  ;; syntax->vector
  ;; syntax-unwrap
  ;; syntax=?
  ;; identifier=symbol?
  ;; #;quoted-syntax-object?

  ;; syntax-clauses-unwrap
  ;; syntax-clauses-filter
  ;; syntax-clauses-remove
  ;; syntax-clauses-partition
  ;; syntax-clauses-collapse
  ;; syntax-clauses-verify-at-least-once
  ;; syntax-clauses-verify-at-most-once
  ;; syntax-clauses-verify-exactly-once
  ;; syntax-clauses-verify-mutually-inclusive
  ;; syntax-clauses-verify-mutually-exclusive

  ;;     ;; clause specification structs
  ;; make-syntax-clause-spec
  ;; syntax-clause-spec?
  ;; syntax-clause-spec-keyword
  ;; syntax-clause-spec-min-number-of-occurrences
  ;; syntax-clause-spec-max-number-of-occurrences
  ;; syntax-clause-spec-min-number-of-arguments
  ;; syntax-clause-spec-max-number-of-arguments
  ;; syntax-clause-spec-mutually-inclusive
  ;; syntax-clause-spec-mutually-exclusive
  ;; syntax-clause-spec-custom-data
  ;; syntax-clauses-single-spec
  ;; syntax-clauses-fold-specs
  ;; syntax-clauses-validate-specs

;;; --------------------------------------------------------------------
;;; library names

  ;; library-name?
  ;; library-version-numbers?
  ;; library-version-number?
  ;; library-name-decompose
  ;; library-name->identifiers
  ;; library-name->version
  ;; library-name-identifiers=?
  ;; library-name=?
  ;; library-name<?
  ;; library-name<=?
  ;; library-version=?
  ;; library-version<?
  ;; library-version<=?

;;; --------------------------------------------------------------------
;;; library references and conformity

  ;; library-reference?
  ;; library-version-reference?
  ;; library-sub-version-reference?
  ;; library-sub-version?
  ;; library-reference-decompose
  ;; library-reference->identifiers
  ;; library-reference->version-reference
  ;; library-reference-identifiers=?
  ;; conforming-sub-version-and-sub-version-reference?
  ;; conforming-version-and-version-reference?
  ;; conforming-library-name-and-library-reference?

;;; --------------------------------------------------------------------
;;; library infrastructure

  ;; library?
  ;; library-uid
  ;; library-imp-lib*
  ;; library-vis-lib*
  ;; library-inv-lib*
  ;; library-export-subst
  ;; library-export-env
  ;; library-visit-state
  ;; library-invoke-state
  ;; library-visit-code
  ;; library-invoke-code
  ;; library-guard-code
  ;; library-guard-lib*
  ;; library-visible?
  ;; library-source-file-name
  ;; library-option*

  ;; library-source-search-path
  ;; library-extensions
  ;; fasl-directory
  ;; library-binary-search-path
  ;; fasl-path
  ;; fasl-stem+extension

  ;; current-library-locator
  ;; run-time-library-locator
  ;; compile-time-library-locator
  ;; source-library-locator
  ;; current-source-library-file-locator
  ;; current-binary-library-file-locator
  ;; default-source-library-file-locator
  ;; default-binary-library-file-locator
  ;; installed-libraries
  ;; uninstall-library

;;; --------------------------------------------------------------------
;;; compiler stuff

  ;; $current-letrec-pass
  ;; $check-for-illegal-letrec
  ;; $optimize-cp
  ;; $optimize-level
  ;; $source-optimizer-passes-count
  ;; $perform-tag-analysis
  ;; $cp0-size-limit
  ;; $cp0-effort-limit
  ;; $strip-source-info
  ;; $generate-debug-calls
  ;; $open-mvcalls

  ;; $tag-analysis-output
  ;; $assembler-output
  ;; $optimizer-output

  ;; $compile-core-expr->code
  ;; $recordize
  ;; $optimize-direct-calls
  ;; $optimize-letrec
  ;; $source-optimize
  ;; $rewrite-references-and-assignments
  ;; $introduce-tags
  ;; $introduce-vars
  ;; $sanitize-bindings
  ;; $optimize-for-direct-jumps
  ;; $insert-global-assignments
  ;; $convert-closures
  ;; $optimize-closures/lift-codes
  ;; $alt-cogen
  ;; $assemble-sources

  ;; $introduce-primcalls
  ;; $eliminate-fix
  ;; $insert-engine-checks
  ;; $insert-stack-overflow-check
  ;; $specify-representation
  ;; $impose-calling-convention/evaluation-order
  ;; $assign-frame-sizes
  ;; $color-by-chaitin
  ;; $flatten-codes

  ;; $unparse-recordized-code
  ;; $unparse-recordized-code/pretty

;;; --------------------------------------------------------------------

  ;; $compnum->cflonum

  ;; $neg-number
  ;; $neg-fixnum
  ;; $neg-bignum
  ;; $neg-flonum
  ;; $neg-ratnum
  ;; $neg-compnum
  ;; $neg-cflonum

  ;; $inv-number
  ;; $inv-fixnum
  ;; $inv-bignum
  ;; $inv-flonum
  ;; $inv-ratnum
  ;; $inv-compnum
  ;; $inv-cflonum

  ;; $add1-integer
  ;; $add1-fixnum
  ;; $add1-bignum

  ;; $sub1-integer
  ;; $sub1-fixnum
  ;; $sub1-bignum

  ;; $add-number-number
  ;; $add-fixnum-number
  ;; $add-bignum-number
  ;; $add-flonum-number
  ;; $add-ratnum-number
  ;; $add-compnum-number
  ;; $add-cflonum-number
  ;; $add-number-fixnum
  ;; $add-number-bignum
  ;; $add-number-flonum
  ;; $add-number-ratnum
  ;; $add-number-compnum
  ;; $add-number-cflonum
  ;; $add-fixnum-fixnum
  ;; $add-fixnum-bignum
  ;; $add-fixnum-flonum
  ;; $add-fixnum-ratnum
  ;; $add-fixnum-compnum
  ;; $add-fixnum-cflonum
  ;; $add-bignum-fixnum
  ;; $add-bignum-bignum
  ;; $add-bignum-flonum
  ;; $add-bignum-ratnum
  ;; $add-bignum-compnum
  ;; $add-bignum-cflonum
  ;; $add-flonum-fixnum
  ;; $add-flonum-bignum
  ;; $add-flonum-flonum
  ;; $add-flonum-ratnum
  ;; $add-flonum-compnum
  ;; $add-flonum-cflonum
  ;; $add-ratnum-fixnum
  ;; $add-ratnum-bignum
  ;; $add-ratnum-flonum
  ;; $add-ratnum-ratnum
  ;; $add-ratnum-compnum
  ;; $add-ratnum-cflonum
  ;; $add-compnum-fixnum
  ;; $add-compnum-bignum
  ;; $add-compnum-ratnum
  ;; $add-compnum-compnum
  ;; $add-compnum-flonum
  ;; $add-compnum-cflonum
  ;; $add-cflonum-fixnum
  ;; $add-cflonum-bignum
  ;; $add-cflonum-ratnum
  ;; $add-cflonum-flonum
  ;; $add-cflonum-compnum
  ;; $add-cflonum-cflonum

  ;; $sub-number-number
  ;; $sub-fixnum-number
  ;; $sub-bignum-number
  ;; $sub-flonum-number
  ;; $sub-ratnum-number
  ;; $sub-compnum-number
  ;; $sub-cflonum-number
  ;; $sub-number-fixnum
  ;; $sub-number-bignum
  ;; $sub-number-flonum
  ;; $sub-number-ratnum
  ;; $sub-number-compnum
  ;; $sub-number-cflonum
  ;; $sub-fixnum-fixnum
  ;; $sub-fixnum-bignum
  ;; $sub-fixnum-flonum
  ;; $sub-fixnum-ratnum
  ;; $sub-fixnum-compnum
  ;; $sub-fixnum-cflonum
  ;; $sub-bignum-fixnum
  ;; $sub-bignum-bignum
  ;; $sub-bignum-flonum
  ;; $sub-bignum-ratnum
  ;; $sub-bignum-compnum
  ;; $sub-bignum-cflonum
  ;; $sub-flonum-fixnum
  ;; $sub-flonum-bignum
  ;; $sub-flonum-ratnum
  ;; $sub-flonum-flonum
  ;; $sub-flonum-compnum
  ;; $sub-flonum-cflonum
  ;; $sub-ratnum-fixnum
  ;; $sub-ratnum-bignum
  ;; $sub-ratnum-flonum
  ;; $sub-ratnum-ratnum
  ;; $sub-ratnum-compnum
  ;; $sub-ratnum-cflonum
  ;; $sub-compnum-fixnum
  ;; $sub-compnum-bignum
  ;; $sub-compnum-ratnum
  ;; $sub-compnum-compnum
  ;; $sub-compnum-flonum
  ;; $sub-compnum-cflonum
  ;; $sub-cflonum-fixnum
  ;; $sub-cflonum-bignum
  ;; $sub-cflonum-ratnum
  ;; $sub-cflonum-flonum
  ;; $sub-cflonum-compnum
  ;; $sub-cflonum-cflonum

  ;; $mul-number-number
  ;; $mul-fixnum-number
  ;; $mul-bignum-number
  ;; $mul-flonum-number
  ;; $mul-ratnum-number
  ;; $mul-compnum-number
  ;; $mul-cflonum-number
  ;; $mul-number-fixnum
  ;; $mul-number-bignum
  ;; $mul-number-flonum
  ;; $mul-number-ratnum
  ;; $mul-number-compnum
  ;; $mul-number-cflonum
  ;; $mul-fixnum-fixnum
  ;; $mul-fixnum-bignum
  ;; $mul-fixnum-flonum
  ;; $mul-fixnum-ratnum
  ;; $mul-fixnum-compnum
  ;; $mul-fixnum-cflonum
  ;; $mul-bignum-fixnum
  ;; $mul-bignum-bignum
  ;; $mul-bignum-flonum
  ;; $mul-bignum-ratnum
  ;; $mul-bignum-compnum
  ;; $mul-bignum-cflonum
  ;; $mul-flonum-flonum
  ;; $mul-flonum-cflonum
  ;; $mul-flonum-fixnum
  ;; $mul-flonum-bignum
  ;; $mul-flonum-ratnum
  ;; $mul-flonum-compnum
  ;; $mul-ratnum-fixnum
  ;; $mul-ratnum-bignum
  ;; $mul-ratnum-flonum
  ;; $mul-ratnum-ratnum
  ;; $mul-ratnum-compnum
  ;; $mul-ratnum-cflonum
  ;; $mul-compnum-fixnum
  ;; $mul-compnum-bignum
  ;; $mul-compnum-ratnum
  ;; $mul-compnum-flonum
  ;; $mul-compnum-compnum
  ;; $mul-compnum-cflonum
  ;; $mul-cflonum-fixnum
  ;; $mul-cflonum-bignum
  ;; $mul-cflonum-ratnum
  ;; $mul-cflonum-flonum
  ;; $mul-cflonum-compnum
  ;; $mul-cflonum-cflonum

  ;; $div-number-number
  ;; $div-flonum-number
  ;; $div-fixnum-number
  ;; $div-bignum-number
  ;; $div-ratnum-number
  ;; $div-compnum-number
  ;; $div-cflonum-number
  ;; $div-number-flonum
  ;; $div-number-fixnum
  ;; $div-number-bignum
  ;; $div-number-ratnum
  ;; $div-number-compnum
  ;; $div-number-cflonum
  ;; $div-fixnum-flonum
  ;; $div-fixnum-fixnum
  ;; $div-fixnum-bignum
  ;; $div-fixnum-ratnum
  ;; $div-fixnum-compnum
  ;; $div-fixnum-cflonum
  ;; $div-bignum-fixnum
  ;; $div-bignum-bignum
  ;; $div-bignum-flonum
  ;; $div-bignum-ratnum
  ;; $div-bignum-compnum
  ;; $div-bignum-cflonum
  ;; $div-ratnum-fixnum
  ;; $div-ratnum-bignum
  ;; $div-ratnum-ratnum
  ;; $div-ratnum-flonum
  ;; $div-ratnum-compnum
  ;; $div-ratnum-cflonum
  ;; $div-flonum-flonum
  ;; $div-flonum-cflonum
  ;; $div-flonum-fixnum
  ;; $div-flonum-bignum
  ;; $div-flonum-ratnum
  ;; $div-flonum-compnum
  ;; $div-compnum-fixnum
  ;; $div-compnum-bignum
  ;; $div-compnum-ratnum
  ;; $div-compnum-flonum
  ;; $div-compnum-compnum
  ;; $div-compnum-cflonum
  ;; $div-cflonum-fixnum
  ;; $div-cflonum-bignum
  ;; $div-cflonum-ratnum
  ;; $div-cflonum-flonum
  ;; $div-cflonum-compnum
  ;; $div-cflonum-cflonum

  ;; $square-fixnum
  ;; $square-bignum
  ;; $square-ratnum
  ;; $square-compnum
  ;; $square-cflonum

  ;; $cube-fixnum
  ;; $cube-bignum
  ;; $cube-ratnum
  ;; $cube-compnum
  ;; $cube-cflonum

  ;; $gcd-number
  ;; $gcd-number-number
  ;; $gcd-fixnum-number
  ;; $gcd-bignum-number
  ;; $gcd-flonum-number
  ;; $gcd-number-fixnum
  ;; $gcd-number-bignum
  ;; $gcd-number-flonum
  ;; $gcd-fixnum-fixnum
  ;; $gcd-fixnum-bignum
  ;; $gcd-fixnum-flonum
  ;; $gcd-bignum-fixnum
  ;; $gcd-bignum-bignum
  ;; $gcd-bignum-flonum
  ;; $gcd-flonum-fixnum
  ;; $gcd-flonum-bignum
  ;; $gcd-flonum-flonum

  ;; $lcm-number
  ;; $lcm-number-number
  ;; $lcm-fixnum-number
  ;; $lcm-bignum-number
  ;; $lcm-flonum-number
  ;; $lcm-number-fixnum
  ;; $lcm-number-bignum
  ;; $lcm-number-flonum
  ;; $lcm-fixnum-fixnum
  ;; $lcm-fixnum-bignum
  ;; $lcm-fixnum-flonum
  ;; $lcm-bignum-fixnum
  ;; $lcm-bignum-bignum
  ;; $lcm-bignum-flonum
  ;; $lcm-flonum-fixnum
  ;; $lcm-flonum-bignum
  ;; $lcm-flonum-flonum

  ;; $quotient+remainder-fixnum-number
  ;; $quotient+remainder-number-fixnum
  ;; $quotient+remainder-bignum-number
  ;; $quotient+remainder-number-bignum
  ;; $quotient+remainder-flonum-number
  ;; $quotient+remainder-number-flonum
  ;; $quotient+remainder-fixnum-fixnum
  ;; $quotient+remainder-bignum-fixnum
  ;; $quotient+remainder-fixnum-bignum
  ;; $quotient+remainder-bignum-bignum
  ;; $quotient+remainder-fixnum-flonum
  ;; $quotient+remainder-bignum-flonum
  ;; $quotient+remainder-flonum-fixnum
  ;; $quotient+remainder-flonum-bignum
  ;; $quotient+remainder-flonum-flonum

  ;; $quotient-fixnum-number
  ;; $quotient-number-fixnum
  ;; $quotient-bignum-number
  ;; $quotient-number-bignum
  ;; $quotient-flonum-number
  ;; $quotient-number-flonum
  ;; $quotient-fixnum-fixnum
  ;; $quotient-fixnum-bignum
  ;; $quotient-fixnum-flonum
  ;; $quotient-bignum-fixnum
  ;; $quotient-bignum-bignum
  ;; $quotient-bignum-flonum
  ;; $quotient-flonum-fixnum
  ;; $quotient-flonum-bignum
  ;; $quotient-flonum-flonum

  ;; $remainder-fixnum-number
  ;; $remainder-number-fixnum
  ;; $remainder-bignum-number
  ;; $remainder-number-bignum
  ;; $remainder-flonum-number
  ;; $remainder-number-flonum
  ;; $remainder-fixnum-fixnum
  ;; $remainder-fixnum-bignum
  ;; $remainder-fixnum-flonum
  ;; $remainder-bignum-fixnum
  ;; $remainder-bignum-bignum
  ;; $remainder-bignum-flonum
  ;; $remainder-flonum-fixnum
  ;; $remainder-flonum-bignum
  ;; $remainder-flonum-flonum

  ;; $modulo-fixnum-number
  ;; $modulo-bignum-number
  ;; $modulo-flonum-number
  ;; $modulo-number-fixnum
  ;; $modulo-number-bignum
  ;; $modulo-number-flonum
  ;; $modulo-fixnum-fixnum
  ;; $modulo-fixnum-bignum
  ;; $modulo-fixnum-flonum
  ;; $modulo-bignum-fixnum
  ;; $modulo-bignum-bignum
  ;; $modulo-bignum-flonum
  ;; $modulo-flonum-fixnum
  ;; $modulo-flonum-bignum
  ;; $modulo-flonum-flonum

  ;; $max-fixnum-number
  ;; $max-bignum-number
  ;; $max-flonum-number
  ;; $max-ratnum-number
  ;; $max-number-fixnum
  ;; $max-number-bignum
  ;; $max-number-flonum
  ;; $max-number-ratnum
  ;; $max-fixnum-fixnum
  ;; $max-fixnum-bignum
  ;; $max-fixnum-flonum
  ;; $max-fixnum-ratnum
  ;; $max-bignum-fixnum
  ;; $max-bignum-bignum
  ;; $max-bignum-flonum
  ;; $max-bignum-ratnum
  ;; $max-flonum-flonum
  ;; $max-flonum-fixnum
  ;; $max-flonum-bignum
  ;; $max-flonum-ratnum
  ;; $max-ratnum-fixnum
  ;; $max-ratnum-bignum
  ;; $max-ratnum-ratnum
  ;; $max-ratnum-flonum

  ;; $min-fixnum-number
  ;; $min-bignum-number
  ;; $min-flonum-number
  ;; $min-ratnum-number
  ;; $min-number-fixnum
  ;; $min-number-bignum
  ;; $min-number-flonum
  ;; $min-number-ratnum
  ;; $min-fixnum-fixnum
  ;; $min-fixnum-bignum
  ;; $min-fixnum-flonum
  ;; $min-fixnum-ratnum
  ;; $min-bignum-fixnum
  ;; $min-bignum-bignum
  ;; $min-bignum-flonum
  ;; $min-bignum-ratnum
  ;; $min-flonum-flonum
  ;; $min-flonum-fixnum
  ;; $min-flonum-bignum
  ;; $min-flonum-ratnum
  ;; $min-ratnum-fixnum
  ;; $min-ratnum-bignum
  ;; $min-ratnum-ratnum
  ;; $min-ratnum-flonum

  ;; $abs-fixnum
  ;; $abs-bignum
  ;; $abs-flonum
  ;; $abs-ratnum

  ;; $sign-fixnum
  ;; $sign-bignum
  ;; $sign-flonum
  ;; $sign-ratnum
;;;
  ;; $expt-number-fixnum

  ;; $expt-number-zero-fixnum
  ;; $expt-fixnum-zero-fixnum
  ;; $expt-flonum-zero-fixnum
  ;; $expt-compnum-zero-fixnum
  ;; $expt-cflonum-zero-fixnum

  ;; $expt-number-negative-fixnum
  ;; $expt-fixnum-negative-fixnum
  ;; $expt-bignum-negative-fixnum
  ;; $expt-ratnum-negative-fixnum
  ;; $expt-flonum-negative-fixnum
  ;; $expt-compnum-negative-fixnum
  ;; $expt-cflonum-negative-fixnum

  ;; $expt-number-positive-fixnum
  ;; $expt-fixnum-positive-fixnum
  ;; $expt-bignum-positive-fixnum
  ;; $expt-flonum-positive-fixnum
  ;; $expt-ratnum-positive-fixnum
  ;; $expt-compnum-positive-fixnum
  ;; $expt-cflonum-positive-fixnum

  ;; $expt-fixnum-fixnum
  ;; $expt-bignum-fixnum
  ;; $expt-ratnum-fixnum
  ;; $expt-flonum-fixnum
  ;; $expt-compnum-fixnum
  ;; $expt-cflonum-fixnum

  ;; $expt-number-bignum
  ;; $expt-fixnum-bignum
  ;; $expt-bignum-bignum
  ;; $expt-ratnum-bignum
  ;; $expt-flonum-bignum
  ;; $expt-compnum-bignum
  ;; $expt-cflonum-bignum

  ;; $expt-number-flonum
  ;; $expt-number-ratnum
  ;; $expt-number-compnum
  ;; $expt-number-cflonum

  ;; $expt-fixnum-flonum
  ;; $expt-bignum-flonum
  ;; $expt-ratnum-flonum
  ;; $expt-flonum-flonum
  ;; $expt-compnum-flonum
  ;; $expt-cflonum-flonum

  ;; $expt-fixnum-ratnum
  ;; $expt-bignum-ratnum
  ;; $expt-ratnum-ratnum
  ;; $expt-flonum-ratnum
  ;; $expt-compnum-ratnum
  ;; $expt-cflonum-ratnum

  ;; $expt-fixnum-cflonum
  ;; $expt-bignum-cflonum
  ;; $expt-ratnum-cflonum
  ;; $expt-flonum-cflonum
  ;; $expt-compnum-cflonum
  ;; $expt-cflonum-cflonum

  ;; $expt-fixnum-compnum
  ;; $expt-bignum-compnum
  ;; $expt-ratnum-compnum
  ;; $expt-flonum-compnum
  ;; $expt-compnum-compnum
  ;; $expt-cflonum-compnum
;;;
  ;; $sqrt-fixnum
  ;; $sqrt-flonum
  ;; $sqrt-bignum
  ;; $sqrt-ratnum
  ;; $sqrt-compnum
  ;; $sqrt-cflonum

  ;; $exact-integer-sqrt-fixnum
  ;; $exact-integer-sqrt-bignum

  ;; $cbrt-fixnum
  ;; $cbrt-flonum
  ;; $cbrt-bignum
  ;; $cbrt-ratnum
  ;; $cbrt-compnum
  ;; $cbrt-cflonum

  ;; $log-fixnum
  ;; $log-flonum
  ;; $log-bignum
  ;; $log-ratnum
  ;; $log-compnum
  ;; $log-cflonum

  ;; $exp-fixnum
  ;; $exp-bignum
  ;; $exp-ratnum
  ;; $exp-flonum
  ;; $exp-compnum
  ;; $exp-cflonum

  ;; $sin-fixnum
  ;; $sin-bignum
  ;; $sin-ratnum
  ;; $sin-flonum
  ;; $sin-cflonum
  ;; $sin-compnum

  ;; $cos-fixnum
  ;; $cos-bignum
  ;; $cos-ratnum
  ;; $cos-flonum
  ;; $cos-cflonum
  ;; $cos-compnum

  ;; $tan-fixnum
  ;; $tan-bignum
  ;; $tan-ratnum
  ;; $tan-flonum
  ;; $tan-compnum
  ;; $tan-cflonum

  ;; $asin-fixnum
  ;; $asin-bignum
  ;; $asin-ratnum
  ;; $asin-flonum
  ;; $asin-cflonum
  ;; $asin-compnum

  ;; $acos-fixnum
  ;; $acos-bignum
  ;; $acos-ratnum
  ;; $acos-flonum
  ;; $acos-cflonum
  ;; $acos-compnum

  ;; $atan2-real-real

  ;; $atan-fixnum
  ;; $atan-ratnum
  ;; $atan-bignum
  ;; $atan-flonum
  ;; $atan-cflonum
  ;; $atan-compnum

  ;; $sinh-fixnum
  ;; $sinh-bignum
  ;; $sinh-ratnum
  ;; $sinh-flonum
  ;; $sinh-compnum
  ;; $sinh-cflonum

  ;; $cosh-fixnum
  ;; $cosh-bignum
  ;; $cosh-ratnum
  ;; $cosh-flonum
  ;; $cosh-compnum
  ;; $cosh-cflonum

  ;; $tanh-fixnum
  ;; $tanh-bignum
  ;; $tanh-ratnum
  ;; $tanh-flonum
  ;; $tanh-compnum
  ;; $tanh-cflonum

  ;; $asinh-fixnum
  ;; $asinh-bignum
  ;; $asinh-ratnum
  ;; $asinh-flonum
  ;; $asinh-cflonum
  ;; $asinh-compnum

  ;; $acosh-fixnum
  ;; $acosh-bignum
  ;; $acosh-ratnum
  ;; $acosh-flonum
  ;; $acosh-cflonum
  ;; $acosh-compnum

  ;; $atanh-fixnum
  ;; $atanh-bignum
  ;; $atanh-ratnum
  ;; $atanh-flonum
  ;; $atanh-cflonum
  ;; $atanh-compnum

  ;; $bitwise-not-fixnum
  ;; $bitwise-not-bignum

  ;; $bitwise-and-fixnum-number
  ;; $bitwise-and-bignum-number
  ;; $bitwise-and-fixnum-fixnum
  ;; $bitwise-and-fixnum-bignum
  ;; $bitwise-and-bignum-fixnum
  ;; $bitwise-and-bignum-bignum

  ;; $bitwise-ior-fixnum-number
  ;; $bitwise-ior-bignum-number
  ;; $bitwise-ior-fixnum-fixnum
  ;; $bitwise-ior-fixnum-bignum
  ;; $bitwise-ior-bignum-fixnum
  ;; $bitwise-ior-bignum-bignum

  ;; $bitwise-xor-fixnum-number
  ;; $bitwise-xor-bignum-number
  ;; $bitwise-xor-fixnum-fixnum
  ;; $bitwise-xor-fixnum-bignum
  ;; $bitwise-xor-bignum-fixnum
  ;; $bitwise-xor-bignum-bignum

;;; --------------------------------------------------------------------
;;; (vicare system $hashtables)

  ;; $string-hash
  ;; $string-ci-hash
  ;; $symbol-hash
  ;; $bytevector-hash

;;; --------------------------------------------------------------------

  ;; tagged-identifier-syntax?
  ;; list-of-tagged-bindings?
  ;; tagged-lambda-proto-syntax?
  ;; tagged-formals-syntax?
  ;; standard-formals-syntax?
  ;; formals-signature-syntax?
  ;; retvals-signature-syntax?
  ;; parse-tagged-identifier-syntax
  ;; parse-list-of-tagged-bindings
  ;; parse-tagged-lambda-proto-syntax
  ;; parse-tagged-formals-syntax

  ;; make-clambda-compound
  ;; clambda-compound?
  ;; clambda-compound-common-retvals-signature
  ;; clambda-compound-lambda-signatures

  ;; make-lambda-signature
  ;; lambda-signature?
  ;; lambda-signature-formals
  ;; lambda-signature-retvals
  ;; lambda-signature-formals-tags
  ;; lambda-signature-retvals-tags
  ;; lambda-signature=?

  ;; make-formals-signature
  ;; formals-signature?
  ;; formals-signature-tags
  ;; formals-signature=?

  ;; make-retvals-signature
  ;; retvals-signature?
  ;; retvals-signature-tags
  ;; retvals-signature=?
  ;; retvals-signature-common-ancestor

  ;; tag-identifier?
  ;; all-tag-identifiers?
  ;; tag-identifier-callable-signature
  ;; tag-super-and-sub?
  ;; tag-identifier-ancestry
  ;; tag-common-ancestor
  ;; formals-signature-super-and-sub-syntax?

  ;; set-identifier-object-type-spec!
  ;; identifier-object-type-spec
  ;; set-label-object-type-spec!
  ;; label-object-type-spec
  ;; make-object-type-spec
  ;; object-type-spec?
  ;; object-type-spec-uids
  ;; object-type-spec-type-id
  ;; object-type-spec-parent-spec
  ;; object-type-spec-pred-stx
  ;; object-type-spec-constructor-maker
  ;; object-type-spec-accessor-maker
  ;; object-type-spec-mutator-maker
  ;; object-type-spec-getter-maker
  ;; object-type-spec-setter-maker
  ;; object-type-spec-dispatcher
  ;; object-type-spec-ancestry

  ;; tagged-identifier?
  ;; set-identifier-tag!
  ;; identifier-tag
  ;; set-label-tag!
  ;; label-tag

  ;; expand-time-retvals-signature-violation?
  ;; expand-time-retvals-signature-violation-expected-signature
  ;; expand-time-retvals-signature-violation-returned-signature

  ;; top-tag-id
  ;; void-tag-id
  ;; procedure-tag-id
  ;; list-tag-id
  ;; boolean-tag-id

  #| initialise-core-prims-tagging |# )


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; End:
