;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: augmented Scheme language around (nausicaa)
;;;Date: Thu Nov 28, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (nausicaa mehve)
  (export

;;;; (rnrs base (6))
    < > <= >= =
    apply call-with-current-continuation call/cc
;;;Replaced by BEGIN/TAGS from (nausicaa language oopp).
;;;
;;; begin
    quasiquote unquote unquote-splicing
    caadar caaddr cadaar cadadr caddar cadddr cdaaar
    caar cadr cdar cddr caaar caadr cadar
    caddr cdaar cdadr cddar cdddr caaaar caaadr
    cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    char=? char<? char>? char<=? char>=?
    char? char->integer integer->char
;;;Replaced by DEFINE/TAGS from (nausicaa language oopp).
;;;
;;; define
    define-syntax
    div-and-mod div mod div0-and-mod0 div0 mod0
    else => ... _
    eq?
    equal?
    eqv?
    error assertion-violation
    exact-integer-sqrt
    exact? inexact?
    exp log sin cos tan asin acos atan
    expt
    finite? infinite? nan?
    floor ceiling truncate round
    gcd lcm numerator denominator
    identifier-syntax assert
    inexact exact
;;;Replaced with the analogous from (nausicaa language oopp).
;;;
;;; let let* letrec letrec*
;;; let-values let*-values
    let-syntax letrec-syntax syntax-rules
    list-ref map for-each
    magnitude angle
    make-rectangular make-polar real-part imag-part
    max min + * - / abs
    not boolean? boolean=?
    null? list? list length append reverse list-tail
    number->string string->number
    number? complex? real? rational? integer?
    pair? cons car cdr
    procedure?
    quote
;;;Replaced with LAMBDA/TAGS from (nausicaa language oopp).
;;;
;;; lambda set!
    if cond case and or
    rationalize
    real-valued? rational-valued? integer-valued?
    sqrt
    string=? string<? string>? string<=? string>=?
    string? make-string string string-length string-ref
    substring string-append string->list list->string string-copy string-for-each
    symbol? symbol->string string->symbol symbol=?
    values call-with-values dynamic-wind
    vector->list list->vector vector-fill!
    vector-map vector-for-each
    vector? make-vector vector vector-length vector-ref vector-set!
    zero? positive? negative? odd? even?


;;;; (rnrs arithmetic bitwise (6))
    bitwise-and
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-bit-count
    bitwise-bit-field
    bitwise-bit-set?
    bitwise-copy-bit
    bitwise-copy-bit-field
    bitwise-first-bit-set
    bitwise-if
    bitwise-ior
    bitwise-length
    bitwise-not
    bitwise-reverse-bit-field
    bitwise-rotate-bit-field
    bitwise-xor


;;;; (rnrs arithmetic fixnums (6))
    fixnum-width
    fixnum?
    fx*
    fx*/carry
    fx+
    fx+/carry
    fx-
    fx-/carry
    fx<=?
    fx<?
    fx=?
    fx>=?
    fx>?
    fxand
    fxarithmetic-shift
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxbit-count
    fxbit-field
    fxbit-set?
    fxcopy-bit
    fxcopy-bit-field
    fxdiv
    fxdiv-and-mod
    fxdiv0
    fxdiv0-and-mod0
    fxeven?
    fxfirst-bit-set
    fxif
    fxior
    fxlength
    fxmax
    fxmin
    fxmod
    fxmod0
    fxnegative?
    fxnot
    fxodd?
    fxpositive?
    fxreverse-bit-field
    fxrotate-bit-field
    fxxor
    fxzero?
    greatest-fixnum
    least-fixnum


;;;; (rnrs arithmetic flonums (6))

;;;Redefined by (nausicaa language conditions).
;;;
;;; &no-infinities
;;; &no-nans
    make-no-infinities-violation no-infinities-violation?
    make-no-nans-violation no-nans-violation?
    fixnum->flonum
    fl*
    fl+
    fl-
    fl/
    fl<=?
    fl<?
    fl=?
    fl>=?
    fl>?
    flabs
    flacos
    flasin
    flatan
    flceiling
    flcos
    fldenominator
    fldiv
    fldiv-and-mod
    fldiv0
    fldiv0-and-mod0
    fleven?
    flexp
    flexpt
    flfinite?
    flfloor
    flinfinite?
    flinteger?
    fllog
    flmax
    flmin
    flmod
    flmod0
    flnan?
    flnegative?
    flnumerator
    flodd?
    flonum?
    flpositive?
    flround
    flsin
    flsqrt
    fltan
    fltruncate
    flzero?
    real->flonum


;;;; (rnrs bytevectors (6))
    endianness				native-endianness

    make-bytevector			bytevector?
    bytevector-length			bytevector=?
    bytevector-copy!			bytevector-copy
    bytevector-fill!

    bytevector->u8-list			u8-list->bytevector
    bytevector->uint-list		uint-list->bytevector
    bytevector->sint-list		sint-list->bytevector

    string->utf8			utf8->string
    string->utf16			utf16->string
    string->utf32			utf32->string

    bytevector-u8-ref			bytevector-u8-set!
    bytevector-s8-ref			bytevector-s8-set!

    bytevector-u16-ref			bytevector-u16-set!
    bytevector-s16-ref			bytevector-s16-set!
    bytevector-u16-native-ref		bytevector-u16-native-set!
    bytevector-s16-native-ref		bytevector-s16-native-set!

    bytevector-u32-ref			bytevector-u32-set!
    bytevector-s32-ref			bytevector-s32-set!
    bytevector-u32-native-ref		bytevector-u32-native-set!
    bytevector-s32-native-ref		bytevector-s32-native-set!

    bytevector-u64-ref			bytevector-u64-set!
    bytevector-s64-ref			bytevector-s64-set!
    bytevector-u64-native-ref		bytevector-u64-native-set!
    bytevector-s64-native-ref		bytevector-s64-native-set!

    bytevector-uint-ref			bytevector-uint-set!
    bytevector-sint-ref			bytevector-sint-set!

    bytevector-ieee-single-ref		bytevector-ieee-single-set!
    bytevector-ieee-single-native-ref	bytevector-ieee-single-native-set!
    bytevector-ieee-double-ref		bytevector-ieee-double-set!
    bytevector-ieee-double-native-ref	bytevector-ieee-double-native-set!


;;;; (rnrs conditions (6))

;;;Replaced by (nausicaa language conditions).
;;;
;;; define-condition-type
    condition?
    condition			simple-conditions
    condition-predicate		condition-accessor

;;;Redefined by (nausicaa language conditions).
;;;
;;; &condition
;;; &assertion
;;; &error
;;; &implementation-restriction
;;; &irritants
;;; &lexical
;;; &message
;;; &non-continuable
;;; &serious
;;; &syntax
;;; &undefined
;;; &violation
;;; &warning
;;; &who

    make-assertion-violation			assertion-violation?
    make-error					error?
    make-implementation-restriction-violation	implementation-restriction-violation?
    make-irritants-condition			irritants-condition?
    condition-irritants
    make-lexical-violation			lexical-violation?
    make-message-condition			message-condition?
    condition-message
    make-non-continuable-violation		non-continuable-violation?
    make-serious-condition			serious-condition?
    make-syntax-violation			syntax-violation?
    syntax-violation-form			syntax-violation-subform
    make-undefined-violation			undefined-violation?
    make-violation				violation?
    make-warning				warning?
    make-who-condition				who-condition?
    condition-who


;;;; (rnrs control (6))
    when unless
;;;Replaced by DO/TAGS from (nausicaa language oopp).
;;;
;;; do

;;;Replaced   with   CASE-LAMBDA/TAGS   from  (nausicaa   language
;;;oopp).
;;;
;;; case-lambda


;;;; (rnrs enums (6))
    define-enumeration
    enum-set->list
    enum-set-complement
    enum-set-constructor
    enum-set-difference
    enum-set-indexer
    enum-set-intersection
    enum-set-member?
    enum-set-projection
    enum-set-subset?
    enum-set-union
    enum-set-universe
    enum-set=?
    make-enumeration


;;; (rnrs exceptions (6))
    raise
    raise-continuable
    with-exception-handler
    guard

;;These are also in (rnrs base (6))
;;
;; else =>


;;;; (rnrs files (6))
    file-exists? delete-file


;;;; (rnrs hashtables (6))
    equal-hash
    hashtable-clear!
    hashtable-contains?
    hashtable-copy
    hashtable-delete!
    hashtable-entries
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-keys
    hashtable-mutable?
    hashtable-ref
    hashtable-set!
    hashtable-size
    hashtable-update!
    hashtable?
    make-eq-hashtable
    make-eqv-hashtable
    make-hashtable
    string-ci-hash
    string-hash
    symbol-hash


;;;; (rnrs io ports (6))
    open-file-input-port		open-bytevector-input-port
    open-file-output-port		open-bytevector-output-port
    open-string-input-port		open-string-output-port
    open-file-input/output-port
    call-with-port
    call-with-string-output-port	call-with-bytevector-output-port
    close-port
    standard-input-port			standard-output-port
    standard-error-port
    current-input-port			current-output-port
    current-error-port

    make-custom-binary-input-port	make-custom-binary-output-port
    make-custom-textual-input-port	make-custom-textual-output-port
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port

    flush-output-port

    file-options
    utf-8-codec				utf-16-codec
    latin-1-codec
    error-handling-mode

    buffer-mode				buffer-mode?
    output-port-buffer-mode

    make-transcoder			transcoder-codec
    native-transcoder			transcoder-error-handling-mode
    port-transcoder			eol-style
    transcoder-eol-style		native-eol-style
    transcoded-port

    bytevector->string			string->bytevector

    eof-object				eof-object?
    port-eof?

    port?
    input-port?				output-port?
    textual-port?			binary-port?

    port-position			set-port-position!
    port-has-port-position?		port-has-set-port-position!?

    get-u8				lookahead-u8
    get-bytevector-n			get-bytevector-n!
    get-bytevector-some			get-bytevector-all
    get-char				lookahead-char
    get-string-n			get-string-n!
    get-string-all			get-line
    get-datum

    put-u8				put-bytevector
    put-char				put-string
    put-datum

;;;Redefined by (nausicaa language conditions).
;;;
;;; &i/o
;;; &i/o-decoding
;;; &i/o-encoding
;;; &i/o-file-already-exists
;;; &i/o-file-does-not-exist
;;; &i/o-file-is-read-only
;;; &i/o-file-protection
;;; &i/o-filename
;;; &i/o-invalid-position
;;; &i/o-port
;;; &i/o-read
;;; &i/o-write

    make-i/o-error			i/o-error?
    make-i/o-decoding-error		i/o-decoding-error?
    make-i/o-encoding-error		i/o-encoding-error?
    i/o-encoding-error-char
    make-i/o-file-already-exists-error	i/o-file-already-exists-error?
    make-i/o-file-does-not-exist-error	i/o-file-does-not-exist-error?
    make-i/o-file-is-read-only-error	i/o-file-is-read-only-error?
    make-i/o-file-protection-error	i/o-file-protection-error?
    make-i/o-filename-error		i/o-filename-error?
    i/o-error-filename
    make-i/o-invalid-position-error	i/o-invalid-position-error?
    i/o-error-position
    make-i/o-port-error			i/o-port-error?
    i/o-error-port
    make-i/o-read-error			i/o-read-error?
    make-i/o-write-error		i/o-write-error?


;;;; (rnrs io simple (6))
    call-with-input-file	call-with-output-file
    with-input-from-file	with-output-to-file
    open-input-file		open-output-file
    close-input-port		close-output-port
    read-char			peek-char
    read			write-char
    display			write
    newline


;;These are in also (rnrs io ports (6)).
;;
;; current-input-port		current-output-port
;; current-error-port
;; eof-object			eof-object?
;; input-port?			output-port?
;;
;; &i/o				make-i/o-error
;; i/o-error?
;;
;; &i/o-read			make-i/o-read-error
;; i/o-read-error?
;;
;; &i/o-write			make-i/o-write-error
;; i/o-write-error?
;;
;; &i/o-invalid-position	make-i/o-invalid-position-error
;; i/o-invalid-position-error?	i/o-error-position
;;
;; &i/o-filename		make-i/o-filename-error
;; i/o-filename-error?		i/o-error-filename
;;
;; &i/o-file-protection	make-i/o-file-protection-error
;; i/o-file-protection-error?
;;
;; &i/o-file-is-read-only	make-i/o-file-is-read-only-error
;; i/o-file-is-read-only-error?
;;
;; &i/o-file-already-exists	make-i/o-file-already-exists-error
;; i/o-file-already-exists-error?
;;
;; &i/o-file-does-not-exist	make-i/o-file-does-not-exist-error
;; i/o-file-does-not-exist-error?
;;
;; &i/o-port			make-i/o-port-error
;; i/o-port-error?		i/o-error-port
;;
;; &i/o-decoding		make-i/o-decoding-error
;; i/o-decoding-error?
;;
;; &i/o-encoding		make-i/o-encoding-error
;; i/o-encoding-error?		i/o-encoding-error-char


;;;; (rnrs lists (6))
    assoc
    assp
    assq
    assv
    cons*
    exists
    filter
    find
    fold-left
    fold-right
    for-all
    member
    memp
    memq
    memv
    partition
    remove
    remp
    remq
    remv


;;;; (rnrs programs (6))
    command-line	exit


;;;; (rnrs records inspection (6))
    record-field-mutable?
    record-rtd
    record-type-field-names
    record-type-generative?
    record-type-name
    record-type-opaque?
    record-type-parent
    record-type-sealed?
    record-type-uid
    record?


;;;; (rnrs records procedural (6))
    make-record-constructor-descriptor
    make-record-type-descriptor
    record-accessor
    record-constructor
    record-mutator
    record-predicate
    record-type-descriptor?


;;;; (rnrs records syntactic (6))
    define-record-type
    fields
    immutable
    mutable
    nongenerative
    opaque
    parent
    parent-rtd
    protocol
    record-constructor-descriptor
    record-type-descriptor
    sealed


;;;; (rnrs sorting (6))
    list-sort
    vector-sort
    vector-sort!


;;;; (rnrs syntax-case (6))
    bound-identifier=?
    datum->syntax
    free-identifier=?
    generate-temporaries
    identifier?
    make-variable-transformer
    quasisyntax
    syntax
    syntax->datum
    syntax-case
    syntax-violation
    unsyntax
    unsyntax-splicing
    with-syntax

;;These are also in (rnrs base (6)).
;;
;; ... _


;;;; (rnrs unicode (6))
    char-upcase			char-downcase
    char-titlecase		char-foldcase

    char-ci=?
    char-ci<?			char-ci>?
    char-ci<=?			char-ci>=?

    char-alphabetic?		char-numeric?
    char-whitespace?		char-upper-case?
    char-lower-case?		char-title-case?
    char-general-category

    string-upcase		string-downcase
    string-titlecase		string-foldcase

    string-ci=?
    string-ci<?			string-ci>?
    string-ci<=?		string-ci>=?

    string-normalize-nfd	string-normalize-nfkd
    string-normalize-nfc	string-normalize-nfkc


;;;; (rnrs r5rs (6))

    delay
    exact->inexact
    force
    inexact->exact
    modulo
    remainder
    null-environment
    quotient
    scheme-report-environment

;;;; (rnrs eval (6))
    eval
    environment

;;;; (rnrs mutable-pairs (6))
    set-car!
    set-cdr!

;;;; (rnrs mutable-strings (6))
    string-set!
    string-fill!


;;;; bindings from (vicare language-extensions)
;;
;;The list  of these bindings can  be obtained by running  the following
;;program:
;;
;;   #!r6rs
;;   (import (vicare))
;;   (for-each pretty-print
;;      (environment-symbols (environment '(vicare language-extensions))))
;;   (flush-output-port (current-output-port))
;;

    acosh
    add1
    andmap
    annotation?
    annotation-expression
    annotation-source
    annotation-stripped
    annotation-textual-position
    apropos
    argv->bytevectors
    argv-length
    argv->strings
    array-ref-c-double
    array-ref-c-float
    array-ref-c-off_t
    array-ref-c-pointer
    array-ref-c-ptrdiff_t
    array-ref-c-signed-char
    array-ref-c-signed-int
    array-ref-c-signed-long
    array-ref-c-signed-long-long
    array-ref-c-signed-short
    array-ref-c-sint16
    array-ref-c-sint32
    array-ref-c-sint64
    array-ref-c-sint8
    array-ref-c-size_t
    array-ref-c-ssize_t
    array-ref-c-uint16
    array-ref-c-uint32
    array-ref-c-uint64
    array-ref-c-uint8
    array-ref-c-unsigned-char
    array-ref-c-unsigned-int
    array-ref-c-unsigned-long
    array-ref-c-unsigned-long-long
    array-ref-c-unsigned-short
    array-set-c-double!
    array-set-c-float!
    array-set-c-off_t!
    array-set-c-pointer!
    array-set-c-ptrdiff_t!
    array-set-c-signed-char!
    array-set-c-signed-int!
    array-set-c-signed-long!
    array-set-c-signed-long-long!
    array-set-c-signed-short!
    array-set-c-sint16!
    array-set-c-sint32!
    array-set-c-sint64!
    array-set-c-sint8!
    array-set-c-size_t!
    array-set-c-ssize_t!
    array-set-c-uint16!
    array-set-c-uint32!
    array-set-c-uint64!
    array-set-c-uint8!
    array-set-c-unsigned-char!
    array-set-c-unsigned-int!
    array-set-c-unsigned-long!
    array-set-c-unsigned-long-long!
    array-set-c-unsigned-short!
    asinh
    assembler-output
    atanh
    base64->bytevector
    begin0
    begin-returnable
    bignum?
    bignum->bytevector
    break
    bwp-object?
    bytevector-append
    bytevector-empty?
    bytevector->base64
    bytevector->bignum
    bytevector->c4b-list
    bytevector->c4l-list
    bytevector->c4n-list
    bytevector->c8b-list
    bytevector->c8l-list
    bytevector->c8n-list
    bytevector->cstring
    bytevector->cstring*
    bytevector->f4b-list
    bytevector->f4l-list
    bytevector->f4n-list
    bytevector->f8b-list
    bytevector->f8l-list
    bytevector->f8n-list
    bytevector->flonum
    bytevector->guarded-cstring
    bytevector->guarded-cstring*
    bytevector->guarded-memory
    bytevector->guarded-memory*
    bytevector->hex
    bytevector->memory
    bytevector->memory*
    bytevector-port-buffer-size
    bytevector-reverse-and-concatenate
    bytevector->s16b-list
    bytevector->s16l-list
    bytevector->s16n-list
    bytevector->s32b-list
    bytevector->s32l-list
    bytevector->s32n-list
    bytevector->s64b-list
    bytevector->s64l-list
    bytevector->s64n-list
    bytevector->s8-list
    bytevectors->argv
    bytevectors->argv*
    bytevectors->guarded-argv
    bytevectors->guarded-argv*
    bytevector->string-base64
    bytevector->string-hex
    bytevector->u16b-list
    bytevector->u16l-list
    bytevector->u16n-list
    bytevector->u32b-list
    bytevector->u32l-list
    bytevector->u32n-list
    bytevector->u64b-list
    bytevector->u64l-list
    bytevector->u64n-list
    c4b-list->bytevector
    c4l-list->bytevector
    c4n-list->bytevector
    c8b-list->bytevector
    c8l-list->bytevector
    c8n-list->bytevector
    call/cf
    calloc
    calloc*
    cbrt
    cflonum?
    code?
    collect
    collection-avoidance-list
    collect-key
    command-line-arguments
    compensate
    complex-conjugate
    compnum?
    condition-errno
    condition-h_errno
    conforming-library-name-and-library-reference?
    conforming-sub-version-and-sub-version-reference?
    conforming-version-and-version-reference?
    console-error-port
    console-input-port
    console-output-port
    continue
    cosh
    cstring16be->string
    cstring16->bytevector
    cstring16le->string
    cstring16n->string
    cstring->bytevector
    cstring->bytevector*
    cstring->string
    cube
    current-core-eval
    current-time
    date-string
    debug-print
    debug-print*
    debug-print-enabled?
    define-auxiliary-syntaxes
    define-constant
    define-constant-values
    define-fluid-syntax
    define-inline
    define-inline-constant
    define-integrable
    define-returnable
    define-struct
    define-syntax-rule
    ;;Replaced by the binding from (nausicaa language oopp).
    ;;
    ;;define-values
    die
    engine-handler
    enum-set?
    environ
    environment?
    environment-binding
    environment-labels
    environment-libraries
    environment-symbols
    errno
;;;Redefined by (nausicaa language conditions).
;;;
;;; &errno
    errno-condition?
    exact-integer?
    exit-hooks
    expand-form-to-core-language
    expand-library
    expand-top-level
    export
    f4b-list->bytevector
    f4l-list->bytevector
    f4n-list->bytevector
    f8b-list->bytevector
    f8l-list->bytevector
    f8n-list->bytevector
    fasl-directory
    fasl-path
    fasl-read
    fasl-search-path
    fasl-write
    filename->string-func
    fixnum->string
    flacosh
    flasinh
    flatanh
    flcbrt
    flcosh
    flcube
    flexpm1
    flhypot
    fllog1p
    flnonnegative?
    flnonpositive?
    flonum-bytes
    flonum->bytevector
    flonum-parts
    flonum->string
    flsinh
    flsquare
    fltanh
    fluid-let-syntax
    flzero?/negative
    flzero?/positive
    for
    for-each-in-order
    foreign-call
    forget-to-avoid-collecting
    format
    fprintf
    free
    fx<
    fx<=
    fx=
    fx>
    fx>=
    fxabs
    fxadd1
    fxlogand
    fxlognot
    fxlogor
    fxlogxor
    fxmodulo
    fxnonnegative?
    fxnonpositive?
    fxquotient
    fxremainder
    fxsign
    fxsll
    fxsra
    fxsub1
    gensym
    gensym?
    gensym-count
    gensym-prefix
    gensym->unique-string
    get-annotated-datum
    get-char-and-track-textual-position
    getenv
    get-output-string
    getprop
    get-string-some
    guarded-calloc
    guarded-calloc*
    guarded-malloc
    guarded-malloc*
    guarded-realloc
    guarded-realloc*
    guarded-strdup
    guarded-strdup*
    guarded-strndup
    guarded-strndup*
;;;Redefined by (nausicaa language conditions).
;;;
;;; &h_errno
    h_errno-condition?
    hex->bytevector
    host-info
    immediate?
    import
    include
    input-file-buffer-size
    input/output-file-buffer-size
    input/output-socket-buffer-size
    installed-libraries
    integer->machine-word
    integer->pointer
    interaction-environment
    interrupted-condition?
    interrupt-handler
;;;Redefined by (nausicaa language conditions).
;;;
;;; &i/o-eagain
    i/o-eagain-error?
    keyword=?
    keyword?
    keyword-hash
    keyword->symbol
    lambda-returnable
    last-pair
    library
    library-extensions
    library-name<=?
    library-name<?
    library-name=?
    library-name?
    library-name-decompose
    library-name->identifiers
    library-name-identifiers=?
    library-name->version
    library-path
    library-reference?
    library-reference-decompose
    library-reference->identifiers
    library-reference-identifiers=?
    library-reference->version-reference
    library-sub-version-reference?
    library-version<=?
    library-version<?
    library-version=?
    library-version-number?
    library-version-numbers?
    library-version-reference?
    load
    load-r6rs-script
    lookahead-two-u8
    machine-word->integer
    make-binary-file-descriptor-input/output-port
    make-binary-file-descriptor-input/output-port*
    make-binary-file-descriptor-input-port
    make-binary-file-descriptor-input-port*
    make-binary-file-descriptor-output-port
    make-binary-file-descriptor-output-port*
    make-binary-socket-input/output-port
    make-binary-socket-input/output-port*
    make-compile-time-value
    make-errno-condition
    make-file-options
    make-guardian
    make-h_errno-condition
    make-interrupted-condition
    make-i/o-eagain
    make-list
    make-memory-block
    make-memory-block/guarded
    make-out-of-memory-error
    make-parameter
    make-queue-procs
    make-readline-input-port
    make-source-position-condition
    make-struct-type
    make-textual-file-descriptor-input/output-port
    make-textual-file-descriptor-input/output-port*
    make-textual-file-descriptor-input-port
    make-textual-file-descriptor-input-port*
    make-textual-file-descriptor-output-port
    make-textual-file-descriptor-output-port*
    make-textual-socket-input/output-port
    make-textual-socket-input/output-port*
    make-time
    make-traced-macro
    make-traced-procedure
    malloc
    malloc*
    memcmp
    memcpy
    memmove
    memory-block?
    memory-block?/non-null
    memory-block?/not-null
    memory-block-pointer
    memory-block-reset
    memory-block-size
    memory->bytevector
    memory-copy
    memset
    module
    neq?
    new-cafe
    new-interaction-environment
    non-negative?
    non-positive?
    null-memory-block
    null-pointer
    optimizer-output
    ormap
;;;Redefined by (nausicaa language conditions).
;;;
;;; &out-of-memory-error
    out-of-memory-error?
    out-of-memory-error.clean?
    out-of-memory-error.number-of-bytes
    out-of-memory-error.old-pointer
    output-file-buffer-size
    parameterize
    parameterise
    parametrise
    pathname->string-func
    pointer<=?
    pointer<>?
    pointer<?
    pointer=?
    pointer>=?
    pointer>?
    pointer?
    pointer-add
    pointer-and-offset?
    pointer-clone
    pointer-diff
    pointer->integer
    pointer-null?
    pointer-ref-c-double
    pointer-ref-c-float
    pointer-ref-c-off_t
    pointer-ref-c-pointer
    pointer-ref-c-ptrdiff_t
    pointer-ref-c-signed-char
    pointer-ref-c-signed-int
    pointer-ref-c-signed-long
    pointer-ref-c-signed-long-long
    pointer-ref-c-signed-short
    pointer-ref-c-sint16
    pointer-ref-c-sint32
    pointer-ref-c-sint64
    pointer-ref-c-sint8
    pointer-ref-c-size_t
    pointer-ref-c-ssize_t
    pointer-ref-c-uint16
    pointer-ref-c-uint32
    pointer-ref-c-uint64
    pointer-ref-c-uint8
    pointer-ref-c-unsigned-char
    pointer-ref-c-unsigned-int
    pointer-ref-c-unsigned-long
    pointer-ref-c-unsigned-long-long
    pointer-ref-c-unsigned-short
    pointer-set-c-double!
    pointer-set-c-float!
    pointer-set-c-off_t!
    pointer-set-c-pointer!
    pointer-set-c-ptrdiff_t!
    pointer-set-c-signed-char!
    pointer-set-c-signed-int!
    pointer-set-c-signed-long!
    pointer-set-c-signed-long-long!
    pointer-set-c-signed-short!
    pointer-set-c-sint16!
    pointer-set-c-sint32!
    pointer-set-c-sint64!
    pointer-set-c-sint8!
    pointer-set-c-size_t!
    pointer-set-c-ssize_t!
    pointer-set-c-uint16!
    pointer-set-c-uint32!
    pointer-set-c-uint64!
    pointer-set-c-uint8!
    pointer-set-c-unsigned-char!
    pointer-set-c-unsigned-int!
    pointer-set-c-unsigned-long!
    pointer-set-c-unsigned-long-long!
    pointer-set-c-unsigned-short!
    pointer-value
    port-closed?
    port-dump-status
    port-fd
    port-getprop
    port-hash
    port-id
    port-in-non-blocking-mode?
    port-mode
    port-property-list
    port-putprop
    port-remprop
    port-set-non-blocking-mode!
    port-textual-position
    port-uid
    port-unset-non-blocking-mode!
    post-gc-hooks
    pretty-format
    pretty-print
    pretty-print*
    pretty-width
    print-condition
    printer-integer-radix
    print-error
    printf
    print-gensym
    print-graph
    print-unicode
    procedure-annotation
    promise?
    property-list
    purge-collection-avoidance-list
    push-compensation
    push-compensation-thunk
    putprop
    quotient+remainder
    random
    ratnum?
    readline
    read-line
    readline-enabled?
    realloc
    realloc*
    ;;Replaced with bindings from (nausicaa language oopp)
    ;;
    ;; receive
    ;; receive-and-return
    record-destructor
    record-destructor-set!
    record-guardian-log
    record-guardian-logger
    record-reset
    record-and-rtd?
    record-type-and-record?
    register-to-avoid-collecting
    remprop
    replace-to-avoid-collecting
    reset-input-port!
    reset-output-port!
    reset-symbol-proc!
    retrieve-to-avoid-collecting
    return
    run-compensations
    s16b-list->bytevector
    s16l-list->bytevector
    s16n-list->bytevector
    s32b-list->bytevector
    s32l-list->bytevector
    s32n-list->bytevector
    s64b-list->bytevector
    s64l-list->bytevector
    s64n-list->bytevector
    s8-list->bytevector
    set-pointer-null!
    set-port-buffer-mode!
    set-port-mode!
    set-rtd-destructor!
    set-rtd-printer!
    set-symbol-value!
    sign
    sinh
    sll
    source-position-byte
    source-position-character
    source-position-column
    source-position-condition?
    source-position-line
    source-position-port-id
    square
    sra
    stale-when
    stats?
    stats-bytes-major
    stats-bytes-minor
    stats-collection-id
    stats-gc-real-secs
    stats-gc-real-usecs
    stats-gc-sys-secs
    stats-gc-sys-usecs
    stats-gc-user-secs
    stats-gc-user-usecs
    stats-real-secs
    stats-real-usecs
    stats-sys-secs
    stats-sys-usecs
    stats-user-secs
    stats-user-usecs
    stderr
    stdin
    stdout
    strcmp
    strdup
    strdup*
    strerror
    string-empty?
    string-base64->bytevector
    string-copy!
    string->cstring
    string->cstring*
    string->filename-func
    string->flonum
    string->guarded-cstring
    string->guarded-cstring*
    string-hex->bytevector
    string-or-symbol->string
    string-or-symbol->symbol
    string->pathname-func
    string-port-buffer-size
    string-reverse-and-concatenate
    strings->argv
    strings->argv*
    strings->guarded-argv
    strings->guarded-argv*
    string->utf16be
    string->utf16le
    string->utf16n
    strlen
    strncmp
    strndup
    strndup*
    struct=?
    struct?
    struct-constructor
    struct-destructor
    struct-field-accessor
    struct-field-mutator
    struct-guardian-log
    struct-guardian-logger
    struct-length
    struct-name
    struct-predicate
    struct-printer
    struct-ref
    struct-reset
    struct-rtd
    struct-set!
    struct-type-descriptor
    struct-type-destructor
    struct-type-field-names
    struct-type-name
    struct-type-symbol
    sub1
    subbytevector-s8
    subbytevector-s8/count
    subbytevector-u8
    subbytevector-u8/count
    subvector
    symbol-bound?
    symbol->keyword
    symbol-value
    syntax-object?
    syntax-object-expression
    syntax-object-marks
    syntax-object-source-objects
    syntax-object-substs
    syntax-transpose
    system-value
    tanh
    time
    time<=?
    time<?
    time=?
    time>=?
    time>?
    time?
    time-addition
    time-and-gather
    time-difference
    time-from-now
    time-gmt-offset
    time-it
    time-nanosecond
    time-second
    top-level-value
    let*-syntax
    let-constants
    let*-constants
    letrec-constants
    letrec*-constants
    trace-define
    trace-define-syntax
    trace-lambda
    trace-let
    trace-letrec-syntax
    trace-let-syntax
    transcoder?
    type-descriptor
    u16b-list->bytevector
    u16l-list->bytevector
    u16n-list->bytevector
    u32b-list->bytevector
    u32l-list->bytevector
    u32n-list->bytevector
    u64b-list->bytevector
    u64l-list->bytevector
    u64n-list->bytevector
    unicode-printable-char?
    uninstall-library
    until
    unwind-protect
    utf-16be-codec
    utf16be->string
    utf-16le-codec
    utf16le->string
    utf-16n-codec
    utf16n->string
    utf-bom-codec
    uuid
    variable-transformer?
    variable-transformer-procedure
    vector-append
    vector-copy
    vector-copy!
    vector-empty?
    vector-exists
    vector-for-all
    vector-resize
    verbose-timer
    vicare-argv0
    vicare-argv0-string
    void
    waiter-prompt-string
    warning
    weak-cons
    weak-pair?
    while
    with
    with-compensations
    with-compensations/on-error
    with-input-from-string
    with-local-storage
    with-output-to-port
    with-output-to-string
    would-block-object
    would-block-object?
    xor

    char-in-ascii-range?
    fixnum-in-character-range?

    define-syntax*

    ;;; syntax utilities
    identifier->string
    string->identifier
    identifier-prefix
    identifier-suffix
    identifier-append
    identifier-format
    duplicate-identifiers?
    delete-duplicate-identifiers
    identifier-memq

    identifier-record-constructor
    identifier-record-predicate
    identifier-record-field-accessor
    identifier-record-field-mutator

    identifier-struct-constructor
    identifier-struct-predicate
    identifier-struct-field-accessor
    identifier-struct-field-mutator

    syntax-car
    syntax-cdr
    syntax->list
    identifiers->list
    all-identifiers?

    syntax->vector
    syntax-unwrap
    syntax=?
    identifier=symbol?
    #;quoted-syntax-object?

    syntax-clauses-unwrap
    syntax-clauses-filter
    syntax-clauses-remove
    syntax-clauses-partition
    syntax-clauses-collapse
    syntax-clauses-verify-at-least-once
    syntax-clauses-verify-at-most-once
    syntax-clauses-verify-exactly-once
    syntax-clauses-verify-mutually-inclusive
    syntax-clauses-verify-mutually-exclusive

    ;; clause specification structs
    make-syntax-clause-spec
    syntax-clause-spec?
    syntax-clause-spec-keyword
    syntax-clause-spec-min-number-of-occurrences
    syntax-clause-spec-max-number-of-occurrences
    syntax-clause-spec-min-number-of-arguments
    syntax-clause-spec-max-number-of-arguments
    syntax-clause-spec-mutually-inclusive
    syntax-clause-spec-mutually-exclusive
    syntax-clauses-single-spec
    syntax-clauses-fold-specs
    syntax-clauses-validate-specs

    ;; environment inquiry
    uname
    utsname?
    utsname-sysname
    utsname-nodename
    utsname-release
    utsname-version
    utsname-machine
    implementation-name
    implementation-version
    cpu-architecture
    machine-name
    os-name
    os-version

    ;; condition types
;;; Redefined by (nausicaa language conditions)
;;;
;;; &procedure-argument-violation
    make-procedure-argument-violation
    procedure-argument-violation?
    procedure-argument-violation

;;; Redefined by (nausicaa language conditions)
;;;
;;; &expression-return-value-violation
    make-expression-return-value-violation
    expression-return-value-violation?
    expression-return-value-violation

    ;; bignums
    bignum-positive?
    bignum-negative?
    bignum-non-negative?
    bignum-non-positive?
    bignum-odd?
    bignum-even?
    least-positive-bignum
    greatest-negative-bignum

    ;; raw octets and strings
    octets->string			string->octets
    octets-encoded-bytevector?		octets-encoded-string?

    ;; ASCII and latin1 encodings
    ascii->string			string->ascii
    latin1->string			string->latin1
    ascii-encoded-bytevector?		latin1-encoded-bytevector?
    ascii-encoded-string?		latin1-encoded-string?

    ;; URI/percent encoding
    string->uri-encoding		uri-encoding->string
    string->percent-encoding		percent-encoding->string
    uri-decode				percent-decode
    uri-encode				percent-encode
    normalise-uri-encoding		normalise-percent-encoding
    uri-encoded-bytevector?		percent-encoded-bytevector?
    uri-encoded-string?			percent-encoded-string?

    ;; misc
    set-cons!
    eval-for-expand
    record-type-field-ref
    record-type-field-set!
    $record-type-field-ref
    $record-type-field-set!
    values->list


;;;; bindings from (nausicaa language oopp)

    set!
    define
    define-values
    begin
    lambda
    let
    let*
    letrec
    letrec*
    let-values
    let*-values
    receive
    receive-and-return
    do
    do*
    case-lambda

    define-label		define-class		define-mixin
    make-from-fields		is-a?
    slot-set!			slot-ref
    tag-unique-identifiers

    define/tags
    lambda/tags			case-lambda/tags
    with-tags
    let/tags			let*/tags
    letrec/tags			letrec*/tags
    let-values/tags		let*-values/tags
    receive/tags
    do/tags			do*/tags
    set!/tags
    with-label-shadowing	with-tagged-arguments-validation
    <-


;;;; bindings from (nausicaa language builtins)

    ;; built-in types
    <top> <boolean> <symbol> <keyword> <pointer>
    <pair> <mutable-pair> <spine> <list> <nonempty-list>
    <char>
    <string> <ascii-string> <latin1-string> <percent-encoded-string> <mutable-string>
    <vector>
    <record-type-descriptor> <record> <condition>
    <hashtable> <hashtable-eq> <hashtable-eqv> <string-hashtable> <string-ci-hashtable> <symbol-hashtable>
    <fixnum> <positive-fixnum> <negative-fixnum>
    <nonzero-fixnum> <nonpositive-fixnum> <nonnegative-fixnum>
    <flonum>
    <exact-integer> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>
    <procedure>

    <transcoder> <port>
    <input-port> <output-port> <input/output-port>
    <binary-port> <binary-input-port> <binary-output-port> <binary-input/output-port>
    <textual-port> <textual-input-port> <textual-output-port> <textual-input/output-port>

    <bytevector> <nonempty-bytevector>
    <bytevector-u8> <bytevector-s8>
    <bytevector-u16l> <bytevector-s16l> <bytevector-u16b> <bytevector-s16b> <bytevector-u16n> <bytevector-s16n>
    <bytevector-u32l> <bytevector-s32l> <bytevector-u32b> <bytevector-s32b> <bytevector-u32n> <bytevector-s32n>
    <bytevector-u64l> <bytevector-s64l> <bytevector-u64b> <bytevector-s64b> <bytevector-u64n> <bytevector-s64n>
    ;;<bytevector-uintl> <bytevector-sintl> <bytevector-uintb> <bytevector-sintb> <bytevector-uintn> <bytevector-sintn>
    <bytevector-singlel> <bytevector-singleb> <bytevector-singlen> <bytevector-doublel> <bytevector-doubleb> <bytevector-doublen>
    <ascii-bytevector> <latin1-bytevector> <percent-encoded-bytevector>

    <hashable-and-properties-clauses>

    <undefined> <unspecified>

    ;; multimethods
    define-generic		define-generic*
    tag-unique-identifiers-of	object->string

    ;; auxiliary syntaxes
    <>				abstract
    getter			setter
    maker			finaliser
    method			method-syntax		methods
    mixins
    predicate
    public-protocol		super-protocol
    satisfies			shadows
    virtual-fields


;;;; bindings from (nausicaa language multimethods)

    define-generic-definer	define-generic*-definer
    define-method		add-method
    call-next-method		next-method?
    reverse-before-methods?	argument-type-inspector
    merge-with-multimethods
    :primary			:around
    :before			:after


;;;; bindings from (nausicaa language conditions)

    define-condition-type
    try					catch
    finally

    ;; redefined from (rnrs conditions (6))
    &condition
    &warning
    &serious
    &error
    &violation
    &assertion
    &irritants
    &who
    &message
    &non-continuable
    &implementation-restriction
    &lexical
    &syntax
    &undefined

    ;; redefined from (rnrs arithmetic flonums (6))
    &no-infinities
    &no-nans

    ;; redefined from (rnrs io ports (6))
    &i/o
    &i/o-read
    &i/o-write
    &i/o-port
    &i/o-encoding
    &i/o-decoding
    &i/o-invalid-position
    &i/o-filename
    &i/o-file-protection
    &i/o-file-is-read-only
    &i/o-file-already-exists
    &i/o-file-does-not-exist

    ;; redefined from (vicare)
    &errno
    &h_errno
    &i/o-eagain
    &out-of-memory-error
    &procedure-argument-violation
    &expression-return-value-violation

    ;; wrong type
    &tagged-binding-violation
    tagged-binding-violation

    ;; mismatch
    &mismatch make-mismatch-condition mismatch-condition?

    ;; wrong num args
    &wrong-num-args make-wrong-num-args-condition wrong-num-args-condition?
    condition-wrong-num-args.procname
    condition-wrong-num-args.expected
    condition-wrong-num-args.given
    raise-wrong-num-args-error

    ;; unimplemented
    &unimplemented make-unimplemented-condition unimplemented-condition?
    raise-unimplemented-error

    ;; convenience labels
    <common-conditions>


;;;; bindings from (nausicaa language increments)

    incr!		decr!
    pre-incr!		post-incr!
    pre-decr!		post-decr!
    $incr!		$decr!
    $pre-incr!		$post-incr!
    $pre-decr!		$post-decr!

;;;; bindings from (nausicaa language infix)
    infix
    % ? :
    && !! ^^ ~~
    ++ --
    & ! ^ ~
    << >>
    fx& fx! fx^ fx~ fx<< fx>>

;;;; bindings from (nausicaa language simple-match)
    match


;;;; bindings from (vicare language-extensions namespaces)
    define-namespace		using

;;;; bindings from (vicare language-extensions sentinels)
    sentinel
    make-sentinel	sentinel?
    undefined		undefined?	defined?
    unspecified		unspecified?	specified?


;;;; mehve bindings

;; arithmetics

    addition-0		addition-1		addition-2
    subtraction-0	subtraction-1		subtraction-2
    multiplication-0	multiplication-1	multiplication-2
    division-0		division-1		division-2

;; transcendental

    log-1 log-2
    atan-1 atan-2

;; numeric predicates

    equal-predicate-0		equal-predicate-1	 equal-predicate-2
    less-than-predicate-0	less-than-predicate-1	 less-than-predicate-2
    greater-than-predicate-0	greater-than-predicate-1 greater-than-predicate-2

    less-than-or-equal-to-predicate-0
    less-than-or-equal-to-predicate-1
    less-than-or-equal-to-predicate-2

    greater-than-or-equal-to-predicate-0
    greater-than-or-equal-to-predicate-1
    greater-than-or-equal-to-predicate-2

;;;; input/output

    display-1		display-2
    write-1		write-2


;;;; done exports

)


  (import (for (except (nausicaa)
		       ;; redefined by numeric predicates
		       = < > <= >=
		       zero? positive? negative? non-negative? non-positive?
		       odd? even?
		       finite? infinite? nan?

		       ;; redefined by arithmetics
		       + - * / abs
		       div div0 mod mod0
		       div-and-mod div0-and-mod0

		       ;; redefined by transcendental
		       expt sqrt exp log
		       sin cos tan asin acos atan

		       ;; redefined by parts
		       numerator denominator rationalize
		       floor ceiling truncate round
		       real-part imag-part magnitude angle
		       make-rectangular make-polar

		       ;; redefined by input/output
		       display write

		       ;; redefined by infix
		       infix)
	    expand run)
    (prefix (only (nausicaa)
		  ;; redefined by numeric predicates
		  = < > <= >=
		  zero? positive? negative? non-negative? non-positive?
		  odd? even?
		  finite? infinite? nan?

		  ;; redefined by arithmetics
		  + - * / abs
		  div div0 mod mod0
		  div-and-mod div0-and-mod0

		  ;; redefined by transcendental
		  expt sqrt exp log
		  sin cos tan asin acos atan

		  ;; redefined by parts
		  numerator denominator rationalize
		  floor ceiling truncate round
		  real-part imag-part magnitude angle
		  make-rectangular make-polar

		  ;; redefined by input/output
		  display write

		  ;; redefined by infix
		  infix)
	    nau.))


;;;; parts

;; rational numbers

(define-generic numerator (x))
(define-generic denominator (x))
(define-generic rationalize (x y))

;; rounding

(define-generic floor (x))
(define-generic ceiling (x))
(define-generic truncate (x))
(define-generic round (x))

;; complex numbers

(define-generic real-part (x))
(define-generic imag-part (x))
(define-generic magnitude (x))
(define-generic angle (x))
(define-generic make-rectangular (R I))
(define-generic make-polar (M A))

(define-method (imag-part (o <real>))
  0)

(define-method (angle (o <integer>))
  0)

;;According  to  R6RS:  all  the  reals except  Inf  and  NaN  are  also
;;rationals.
;;
#;(define-method (angle (o <rational>))
  0)

#;(define-method (angle (o <real>))
  0.0)

(module ()

  (add-method numerator		(<rational>)	nau.numerator)
  (add-method numerator		(<integer>)	values)
  (add-method denominator	(<rational>)	nau.denominator)
  (add-method denominator	(<integer>)	nau.denominator)
  (add-method rationalize	(<real> <real>)	nau.rationalize)

;;;; rounding

  (add-method floor	(<flonum>)		flfloor)
  (add-method floor	(<real>)		nau.floor)
  (add-method ceiling	(<flonum>)		flceiling)
  (add-method ceiling	(<real>)		nau.ceiling)
  (add-method truncate	(<flonum>)		fltruncate)
  (add-method truncate	(<real>)		nau.truncate)
  (add-method round	(<flonum>)		flround)
  (add-method round	(<real>)		nau.round)

;;;; complex numbers

  (add-method real-part	(<real>)		values)
  (add-method real-part	(<complex>)		nau.real-part)
  (add-method imag-part	(<complex>)		nau.imag-part)
  (add-method magnitude	(<real>)		abs)
  (add-method magnitude	(<complex>)		nau.magnitude)
  (add-method angle	(<complex>)		nau.angle)

  (add-method make-rectangular	(<real> <real>)	nau.make-rectangular)
  (add-method make-polar	(<real> <real>)	nau.make-polar)

  #| end of module |# )


;;;; numeric predicates

;; equality comparison

(define-syntax (= stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'equal-predicate)
    ((_)
     #'(equal-predicate-0))
    ((_ ?a)
     #'(equal-predicate-1 ?a))
    ((_ ?a ?b)
     #'(equal-predicate-2 ?a ?b))
    ((_ ?a ?b ?c)
     #'(and (equal-predicate-2 ?a ?b)
	    (equal-predicate-2 ?b ?c)))
    ((_ . ?args)
     #'(equal-predicate . ?args))))

(define equal-predicate
  (case-lambda
   (()
    (equal-predicate-0))
   ((arg)
    (equal-predicate-1 arg))
   (args
    (let loop ((A    (car args))
	       (B    (cadr args))
	       (args (cddr args)))
      (and (equal-predicate-2 A B)
	   (if (null? args)
	       #t
	     (loop B (car args) (cdr args))))))))

(define-generic equal-predicate-0 ())
(define-generic equal-predicate-1 (a))
(define-generic equal-predicate-2 (a b))

(define-method (equal-predicate-0)
  #t)

(define-method (equal-predicate-1 x)
  #t)

;; less than comparison

(define-syntax (< stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'less-than-predicate)
    ((_)
     #'(less-than-predicate-0))
    ((_ ?a)
     #'(less-than-predicate-1 ?a))
    ((_ ?a ?b)
     #'(less-than-predicate-2 ?a ?b))
    ((_ ?a ?b ?c)
     #'(and (less-than-predicate-2 ?a ?b)
	    (less-than-predicate-2 ?b ?c)))
    ((_ . ?args)
     #'(less-than-predicate . ?args))))

(define less-than-predicate
  (case-lambda
   (()
    (less-than-predicate-0))
   ((arg)
    (less-than-predicate-1 arg))
   (args
    (let loop ((A    (car args))
	       (B    (cadr args))
	       (args (cddr args)))
      (and (less-than-predicate-2 A B)
	   (if (null? args)
	       #t
	     (loop B (car args) (cdr args))))))))

(define-generic less-than-predicate-0 ())
(define-generic less-than-predicate-1 (a))
(define-generic less-than-predicate-2 (a b))

(define-method (less-than-predicate-0)
  #f)

(define-method (less-than-predicate-1 x)
  #f)

;; greater than comparison

(define-syntax (> stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'greater-than-predicate)
    ((_)
     #'(greater-than-predicate-0))
    ((_ ?a)
     #'(greater-than-predicate-1 ?a))
    ((_ ?a ?b)
     #'(greater-than-predicate-2 ?a ?b))
    ((_ ?a ?b ?c)
     #'(and (greater-than-predicate-2 ?a ?b)
	    (greater-than-predicate-2 ?b ?c)))
    ((_ . ?args)
     #'(greater-than-predicate . ?args))))

(define greater-than-predicate
  (case-lambda
   (()
    (greater-than-predicate-0))
   ((arg)
    (greater-than-predicate-1 arg))
   ((A B args)
    (let loop ((A    A)
	       (B    B)
	       (args args))
      (and (greater-than-predicate-2 A B)
	   (or (null? args)
	       (loop B (car args) (cdr args))))))))

(define-generic greater-than-predicate-0 ())
(define-generic greater-than-predicate-1 (a))
(define-generic greater-than-predicate-2 (a b))

(define-method (greater-than-predicate-0)
  #f)

(define-method (greater-than-predicate-1 x)
  #f)

;; less than or equal to comparison

(define-syntax <=
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'less-than-or-equal-to-predicate)
      ((_)
       #'(less-than-or-equal-to-predicate-0))
      ((_ ?a)
       #'(less-than-or-equal-to-predicate-1 ?a))
      ((_ ?a ?b)
       #'(less-than-or-equal-to-predicate-2 ?a ?b))
      ((_ ?a ?b ?c)
       #'(and (less-than-or-equal-to-predicate-2 ?a ?b)
	      (less-than-or-equal-to-predicate-2 ?b ?c)))
      ((_ . ?args)
       #'(less-than-or-equal-to-predicate . ?args)))))

(define (less-than-or-equal-to-predicate . args)
  (cond ((null? args)
	 (less-than-or-equal-to-predicate-0))
	((null? (cdr args))
	 (less-than-or-equal-to-predicate-1 (car args)))
	(else
	 (let loop ((A    (car args))
		    (B    (cadr args))
		    (args (cddr args)))
	   (and (less-than-or-equal-to-predicate-2 A B)
		(if (null? args)
		    #t
		  (loop B (car args) (cdr args))))))))

(define-generic less-than-or-equal-to-predicate-0 ())
(define-generic less-than-or-equal-to-predicate-1 (a))
(define-generic less-than-or-equal-to-predicate-2 (a b))

(define-method (less-than-or-equal-to-predicate-0)
  #t)

(define-method (less-than-or-equal-to-predicate-1 x)
  #t)

;;; greater than or equal to comparison

(define-syntax >=
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'greater-than-or-equal-to-predicate)
      ((_)
       #'(greater-than-or-equal-to-predicate-0))
      ((_ ?a)
       #'(greater-than-or-equal-to-predicate-1 ?a))
      ((_ ?a ?b)
       #'(greater-than-or-equal-to-predicate-2 ?a ?b))
      ((_ ?a ?b ?c)
       #'(and (greater-than-or-equal-to-predicate-2 ?a ?b)
	      (greater-than-or-equal-to-predicate-2 ?b ?c)))
      ((_ . ?args)
       #'(greater-than-or-equal-to-predicate . ?args)))))

(define (greater-than-or-equal-to-predicate . args)
  (cond ((null? args)
	 (greater-than-or-equal-to-predicate-0))
	((null? (cdr args))
	 (greater-than-or-equal-to-predicate-1 (car args)))
	(else
	 (let loop ((A    (car args))
		    (B    (cadr args))
		    (args (cddr args)))
	   (and (greater-than-or-equal-to-predicate-2 A B)
		(if (null? args)
		    #t
		  (loop B (car args) (cdr args))))))))

(define-generic greater-than-or-equal-to-predicate-0 ())
(define-generic greater-than-or-equal-to-predicate-1 (a))
(define-generic greater-than-or-equal-to-predicate-2 (a b))

(define-method (greater-than-or-equal-to-predicate-0)
  #t)

(define-method (greater-than-or-equal-to-predicate-1 x)
  #t)

;; other predicates

(define-generic zero? (x))
(define-generic positive? (x))
(define-generic negative? (x))
(define-generic non-negative? (x))
(define-generic non-positive? (x))
(define-generic odd? (x))
(define-generic even? (x))
(define-generic finite? (x))
(define-generic infinite? (x))
(define-generic nan? (x))

(define-method (non-negative? (x <fixnum>))
  (or (fxzero? x) (fxpositive? x)))

(define-method (non-positive? (x <fixnum>))
  (or (fxzero? x) (fxnegative? x)))

(define-method (finite? (x <fixnum>))
  #t)

(define-method (infinite? (x <fixnum>))
  #f)

(define-method (nan? (x <fixnum>))
  #f)

(module ()

  (add-method equal-predicate-2				(<fixnum> <fixnum>)	fx=?)
  (add-method equal-predicate-2				(<flonum> <flonum>)	fl=?)
  (add-method equal-predicate-2				(<complex> <complex>)	nau.=)

  (add-method less-than-predicate-2			(<fixnum> <fixnum>)	fx<?)
  (add-method less-than-predicate-2			(<flonum> <flonum>)	fl<?)
  (add-method less-than-predicate-2			(<real>   <real>)	nau.<)

  (add-method less-than-or-equal-to-predicate-2		(<fixnum> <fixnum>)	fx<=?)
  (add-method less-than-or-equal-to-predicate-2		(<flonum> <flonum>)	fl<=?)
  (add-method less-than-or-equal-to-predicate-2		(<real>   <real>)	nau.<=)

  (add-method greater-than-predicate-2			(<fixnum> <fixnum>)	fx>?)
  (add-method greater-than-predicate-2			(<flonum> <flonum>)	fl>?)
  (add-method greater-than-predicate-2			(<real>   <real>)	nau.>)

  (add-method greater-than-or-equal-to-predicate-2	(<fixnum> <fixnum>)	fx>=?)
  (add-method greater-than-or-equal-to-predicate-2	(<flonum> <flonum>)	fl>=?)
  (add-method greater-than-or-equal-to-predicate-2	(<real>	  <real>)	nau.>=)

  (add-method zero?		(<fixnum>)	fxzero?)
  (add-method positive?		(<fixnum>)	fxpositive?)
  (add-method negative?		(<fixnum>)	fxnegative?)
  (add-method odd?		(<fixnum>)	fxodd?)
  (add-method even?		(<fixnum>)	fxeven?)

  (add-method zero?		(<flonum>)	flzero?)
  (add-method positive?		(<flonum>)	flpositive?)
  (add-method negative?		(<flonum>)	flnegative?)
  (add-method odd?		(<flonum>)	flodd?)
  (add-method even?		(<flonum>)	fleven?)
  (add-method finite?		(<flonum>)	flfinite?)
  (add-method infinite?		(<flonum>)	flinfinite?)
  (add-method nan?		(<flonum>)	flnan?)

  (add-method zero?		(<number>)	nau.zero?)
  (add-method positive?		(<real>)	nau.positive?)
  (add-method negative?		(<real>)	nau.negative?)
  (add-method non-negative?	(<real>)	nau.non-negative?)
  (add-method non-positive?	(<real>)	nau.non-positive?)
  (add-method odd?		(<real>)	nau.odd?)
  (add-method even?		(<real>)	nau.even?)
  (add-method finite?		(<complex>)	nau.finite?)
  (add-method infinite?		(<complex>)	nau.infinite?)
  (add-method nan?		(<complex>)	nau.nan?)

  #| end of module |# )


;;;; arithmetics: addition

(define-syntax +
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'addition)
      ((_)
       #'(addition-0))
      ((_ ?a)
       #'(addition-1 ?a))
      ((_ ?a ?b)
       #'(addition-2 ?a ?b))
      ((_ ?a ?b ?c)
       #'(addition-2 (addition-2 ?a ?b) ?c))
      ((_ . ?args)
       #'(addition . ?args)))))

(define (addition . args)
  (if (null? args)
      (addition-0)
    (let loop ((res  (car args))
	       (args (cdr args)))
      (if (null? args)
	  res
	(loop (addition-2 res (car args)) (cdr args))))))

(define-generic addition-0 ())
(define-generic addition-1 (a))
(define-generic addition-2 (a b))

(define-method (addition-0)
  ;;Weird but compliant with (rnrs base (6)).
  ;;
  0)

(define-method (addition-1 (a <number>))
  a)


;;;; arithmetics: subtraction

(define-syntax -
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'subtraction)
      ((_)
       #'(subtraction-0))
      ((_ ?a)
       #'(subtraction-1 ?a))
      ((_ ?a ?b)
       #'(subtraction-2 ?a ?b))
      ((_ ?a ?b ?c)
       #'(subtraction-2 (subtraction-2 ?a ?b) ?c))
      ((_ . ?args)
       #'(subtraction . ?args)))))

(define (subtraction . args)
  (cond ((null? args)
	 (subtraction-0))
	((null? (cdr args))
	 (subtraction-1 (car args)))
	(else
	 (subtraction-2 (car args) (apply addition (cdr args))))))

(define-generic subtraction-0 ())
(define-generic subtraction-1 (a))
(define-generic subtraction-2 (a b))

(define-method (subtraction-0)
  ;;Weird, not compliant with (rnrs base (6)).
  ;;
  0)


;;;; arithmetics: multiplication

(define-syntax *
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'multiplication)
      ((_)
       #'(multiplication-0))
      ((_ ?a)
       #'(multiplication-1 ?a))
      ((_ ?a ?b)
       #'(multiplication-2 ?a ?b))
      ((_ ?a ?b ?c)
       #'(multiplication-2 (multiplication-2 ?a ?b) ?c))
      ((_ . ?args)
       #'(multiplication . ?args)))))

(define (multiplication . args)
  (if (null? args)
      (multiplication-0)
    (let loop ((res  (car args))
	       (args (cdr args)))
      (if (null? args)
	  res
	(loop (multiplication-2 res (car args)) (cdr args))))))

(define-generic multiplication-0 ())
(define-generic multiplication-1 (a))
(define-generic multiplication-2 (a b))

(define-method (multiplication-0)
  ;;Weird but compliant with (rnrs base (6)).
  ;;
  1)

(define-method (multiplication-1 (a <number>))
  a)


;;;; arithmetics: division

(define-syntax /
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'division)
      ((_)
       #'(division-0))
      ((_ ?a)
       #'(division-1 ?a))
      ((_ ?a ?b)
       #'(division-2 ?a ?b))
      ((_ ?a ?b ?c)
       #'(division-2 ?a (* ?b ?c)))
      ((_ ?a . ?args)
       #'(division-2 ?a (* . ?args))))))

(define (division . args)
  (cond ((null? args)
	 (division-0))
	((null? (cdr args))
	 (division-1 (car args)))
	(else
	 (division-2 (car args) (apply * (cdr args))))))

(define-generic division-0 ())
(define-generic division-1 (a))
(define-generic division-2 (a b))

(define-method (division-0)
  ;;Weird, not compliant with (rnrs base (6)).
  ;;
  +nan.0)


;;;; arithmetics: integer operations

(define-generic div  (a b))
(define-generic mod  (a b))
(define-generic div0 (a b))
(define-generic mod0 (a b))
(define-generic div-and-mod   (a b))
(define-generic div0-and-mod0 (a b))


;;;; arithmetics: absolute

(define-generic abs (x))

(define (%fxabs n)
  (if (fxnegative? n)
      (fx- n)
    n))


;;;; arithmetics: method additions

(module ()

  (add-method addition-2 (<fixnum> <fixnum>) fx+)
  (add-method addition-2 (<flonum> <flonum>) fl+)
  (add-method addition-2 (<number> <number>) nau.+)

  (add-method subtraction-1 (<fixnum>) fx-)
  (add-method subtraction-1 (<flonum>) fl-)
  (add-method subtraction-1 (<number>) nau.-)

  (add-method subtraction-2 (<fixnum> <fixnum>) fx-)
  (add-method subtraction-2 (<flonum> <flonum>) fl-)
  (add-method subtraction-2 (<number> <number>) nau.-)

  (add-method multiplication-2 (<fixnum> <fixnum>) fx*)
  (add-method multiplication-2 (<flonum> <flonum>) fl*)
  (add-method multiplication-2 (<number> <number>) nau.*)

  ;;We want to return exact rationals, so we use the following rather than
  ;;FXDIV.
  (add-method division-1 (<fixnum>) nau./)
  (add-method division-1 (<flonum>) fl/)
  (add-method division-1 (<number>) nau./)

  ;;We want to return exact rationals, so we use the following rather than
  ;;FXDIV.
  (add-method division-2 (<fixnum> <fixnum>)		nau./)
  (add-method division-2 (<flonum> <flonum>)		fl/)
  (add-method division-2 (<number> <number>)		nau./)

  (add-method div  (<fixnum>  <fixnum>)			fxdiv)
  (add-method div  (<integer> <integer>)		nau.div)
  (add-method div  (<real> <real>)			nau.div)
  (add-method div0 (<fixnum>  <fixnum>)			fxdiv0)
  (add-method div0 (<integer> <integer>)		nau.div0)
  (add-method div0  (<real> <real>)			nau.div0)

  (add-method mod  (<fixnum>  <fixnum>)			fxmod)
  (add-method mod  (<integer> <integer>)		nau.mod)
  (add-method mod  (<real> <real>)			nau.mod)
  (add-method mod0 (<fixnum>  <fixnum>)			fxmod0)
  (add-method mod0 (<integer> <integer>)		nau.mod0)
  (add-method mod0 (<real> <real>)			nau.mod0)

  (add-method div-and-mod   (<fixnum>  <fixnum>)	fxdiv-and-mod)
  (add-method div-and-mod   (<integer> <integer>)	nau.div-and-mod)
  (add-method div-and-mod   (<real> <real>)		nau.div-and-mod)
  (add-method div0-and-mod0 (<fixnum>  <fixnum>)	fxdiv0-and-mod0)
  (add-method div0-and-mod0 (<integer> <integer>)	nau.div0-and-mod0)
  (add-method div0-and-mod0 (<real> <real>)		nau.div0-and-mod0)

  (add-method abs	(<fixnum>)	%fxabs)
  (add-method abs	(<flonum>)	flabs)
  (add-method abs	(<real>)	nau.abs)

  #| end of module |# )


;;;; transcendental: exponentials and logarithms

(define-generic expt (x y))
(define-generic sqrt (x))
(define-generic exp (x))

(define-syntax log
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'%log)
      ((_ ?x)
       #'(log-1 ?x))
      ((_ ?x ?base)
       #'(log-2 ?x ?base)))))

(define (%log x . args)
  (cond ((null? args)
	 (log-1 x))
	((null? (cdr args))
	 (log-2 x (car args)))
	(else
	 (assertion-violation 'log "wrong number of arguments" args))))

(define-generic log-1 (x))
(define-generic log-2 (x base))


;;;; transcendental: trigonometric

(define-generic sin (x))
(define-generic cos (x))
(define-generic tan (x))

(define-generic asin (x))
(define-generic acos (x))

(define-syntax atan
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'%atan)
      ((_ ?x)
       #'(atan-1 ?x))
      ((_ ?x ?y)
       #'(atan-2 ?x ?y)))))

(define (%atan x . args)
  (cond ((null? args)
	 (atan-1 x))
	((null? (cdr args))
	 (atan-2 x (car args)))
	(else
	 (assertion-violation 'atan "wrong number of arguments" args))))

(define-generic atan-1 (x))
(define-generic atan-2 (x y))


;;;; transcendental: exponentials and logarithms

(module ()

  (add-method expt (<flonum> <flonum>)		flexpt)
  (add-method expt (<complex> <complex>)	nau.expt)

  (add-method sqrt (<flonum>)			flsqrt)
  (add-method sqrt (<complex>)			nau.sqrt)

  (add-method exp (<flonum>)			flexp)
  (add-method exp (<complex>)			nau.exp)

  (add-method log-1 (<flonum>)			fllog)
  (add-method log-1 (<complex>)			nau.log)
  (add-method log-2 (<flonum> <flonum>)		fllog)
  (add-method log-2 (<complex> <complex>)	nau.log)

  #| end of module |# )


;;;; transcendental: trigonometric and hyperbolic

(module ()

  (add-method sin (<flonum>)			flsin)
  (add-method sin (<real>)			nau.sin)
  (add-method sin (<complex>)			nau.sin)

  (add-method cos (<flonum>)			flcos)
  (add-method cos (<real>)			nau.cos)
  (add-method cos (<complex>)			nau.cos)

  (add-method tan (<flonum>)			fltan)
  (add-method tan (<real>)			nau.tan)
  (add-method tan (<complex>)			nau.tan)

  (add-method asin (<flonum>)			flasin)
  (add-method asin (<real>)			nau.asin)
  (add-method asin (<complex>)			nau.asin)

  (add-method acos (<flonum>)			flacos)
  (add-method acos (<real>)			nau.acos)
  (add-method acos (<complex>)			nau.acos)

  (add-method atan-1 (<flonum>)			flatan)
  (add-method atan-1 (<real>)			nau.atan)
  (add-method atan-1 (<complex>)		nau.atan)

  (add-method atan-2 (<flonum> <flonum>)	flatan)
  (add-method atan-2 (<real> <real>)		nau.atan)

  #| end of module |# )


(define-syntax infix
  (let ()
    (import (rnrs))
    (define make-<lexical-token>	cons)
    (define <lexical-token>?		pair?)
    (define <lexical-token>-category	car)
    (define <lexical-token>-value	cdr)

    ;;; Constant tokens representing the recognised operators.
    ;;
    ;; Arithmetic operations.
    (define $add	(make-<lexical-token> 'ADD #'+))
    (define $sub	(make-<lexical-token> 'SUB #'-))
    (define $mul	(make-<lexical-token> 'MUL #'*))
    (define $/		(make-<lexical-token> 'DIV #'/))
    (define $mod	(make-<lexical-token> 'MOD #'mod))
    (define $mod0	(make-<lexical-token> 'MOD #'mod0))
    (define $div	(make-<lexical-token> 'DIV #'div))
    (define $div0	(make-<lexical-token> 'DIV #'div0))
    (define $expt	(make-<lexical-token> 'EXPT #'expt))
    ;; Comparison operators.
    (define $lt		(make-<lexical-token> 'LT #'<))
    (define $gt		(make-<lexical-token> 'GT #'>))
    (define $le		(make-<lexical-token> 'LE #'<=))
    (define $ge		(make-<lexical-token> 'GE #'>=))
    (define $eq		(make-<lexical-token> 'EQ #'=))
    (define $eq?	(make-<lexical-token> 'EQ #'eq?))
    (define $eqv?	(make-<lexical-token> 'EQ #'eqv?))
    (define $equal?	(make-<lexical-token> 'EQ #'equal?))
    ;; Increment and decrement.
    (define $incr!	(make-<lexical-token> 'INCR	(cons #'pre-incr! #'post-incr!)))
    (define $decr!	(make-<lexical-token> 'DECR	(cons #'pre-decr! #'post-decr!)))
    ;; Logical operators
    (define $and	(make-<lexical-token> 'AND	#'and))
    (define $not	(make-<lexical-token> 'NOT	#'not))
    (define $ior	(make-<lexical-token> 'IOR	#'or))
    (define $xor	(make-<lexical-token> 'XOR	#'xor))
    ;; bitwise operators
    (define $bit-and	(make-<lexical-token> 'BIT-AND	#'bitwise-and))
    (define $bit-ior	(make-<lexical-token> 'BIT-IOR	#'bitwise-ior))
    (define $bit-xor	(make-<lexical-token> 'BIT-XOR	#'bitwise-xor))
    (define $bit-not	(make-<lexical-token> 'BIT-NOT	#'bitwise-not))
    (define $bit-shl	(make-<lexical-token> 'BIT-SHL	#'bitwise-arithmetic-shift-left))
    (define $bit-shr	(make-<lexical-token> 'BIT-SHR	#'bitwise-arithmetic-shift-right))
    ;;Fixnum operators.
    (define $fxadd	(make-<lexical-token> 'ADD	#'fx+))
    (define $fxsub	(make-<lexical-token> 'SUB	#'fx-))
    (define $fxmul	(make-<lexical-token> 'MUL	#'fx*))
    (define $fx/	(make-<lexical-token> 'DIV	#'fx/))
    (define $fxdiv	(make-<lexical-token> 'DIV	#'fxdiv))
    (define $fxdiv0	(make-<lexical-token> 'DIV	#'fxdiv0))
    (define $fxmod	(make-<lexical-token> 'MOD	#'fxmod))
    (define $fxmod0	(make-<lexical-token> 'MOD	#'fxmod0))
    (define $fxlt	(make-<lexical-token> 'LT	#'fx<?))
    (define $fxgt	(make-<lexical-token> 'GT	#'fx>?))
    (define $fxle	(make-<lexical-token> 'LE	#'fx<=?))
    (define $fxge	(make-<lexical-token> 'GE	#'fx>=?))
    (define $fxeq	(make-<lexical-token> 'EQ	#'fx=?))
    (define $fxbit-and	(make-<lexical-token> 'BIT-AND	#'fxand))
    (define $fxbit-ior	(make-<lexical-token> 'BIT-IOR	#'fxior))
    (define $fxbit-xor	(make-<lexical-token> 'BIT-XOR	#'fxxor))
    (define $fxbit-not	(make-<lexical-token> 'BIT-NOT	#'fxnot))
    (define $fxbit-shl	(make-<lexical-token> 'BIT-SHL	#'fxarithmetic-shift-left))
    (define $fxbit-shr	(make-<lexical-token> 'BIT-SHR	#'fxarithmetic-shift-right))
    ;;Flonum operators.
    (define $fladd	(make-<lexical-token> 'ADD	#'fl+))
    (define $flsub	(make-<lexical-token> 'SUB	#'fl-))
    (define $flmul	(make-<lexical-token> 'MUL	#'fl*))
    (define $fl/	(make-<lexical-token> 'DIV	#'fl/))
    (define $flexpt	(make-<lexical-token> 'EXPT	#'flexpt))
    (define $fllt	(make-<lexical-token> 'LT	#'fl<?))
    (define $flgt	(make-<lexical-token> 'GT	#'fl>?))
    (define $flle	(make-<lexical-token> 'LE	#'fl<=?))
    (define $flge	(make-<lexical-token> 'GE	#'fl>=?))
    (define $fleq	(make-<lexical-token> 'EQ	#'fl=?))
    ;;Ternary if-then-else.
    ;;
    ;;Here we  use the  #'if syntax  object as semantic  value: it  is a
    ;;trick to avoid  insertion of a raw value: the  parser will take it
    ;;and use it as the IF in the output form.
    (define $question	(make-<lexical-token> 'QUESTION-ID #'if))
    (define $colon	(make-<lexical-token> 'COLON-ID #':))
    ;;Special constant  tokens.  Notice that  the left and  right parens
    ;;tokens  are wrapped  in  a list  because  below they  are used  as
    ;;arguments to APPEND.
    (define $eoi		(make-<lexical-token> '*eoi* (eof-object)))
    (define $left-paren		(make-<lexical-token> 'LPAREN #\())
    (define $right-paren	(make-<lexical-token> 'RPAREN #\)))

    (define-syntax memv-stx
      (syntax-rules ()
	((_ ?atom ?stx)
	 (free-identifier=? ?atom ?stx))
	((_ ?atom ?stx ...)
	 (or (free-identifier=? ?atom ?stx) ...))))

    (define-syntax case-stx
      (syntax-rules (else)
	((_ ?atom ((?s ...) ?e ...) ... (else ?b ...))
	 (cond ((memv-stx ?atom (syntax ?s) ...) ?e ...) ... (else ?b ...)))))

    (define-syntax drop/stx
      (syntax-rules ()
	((_ ?ell ?k)
	 (let loop ((ell ?ell)
		    (k   ?k))
	   (if (zero? k)
	       ell
	     (loop (cdr ell) (- k 1)))))))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?begin . ?body)
	 (and (identifier? #'?begin)
	      (or (free-identifier=? #'begin #'?begin)
		  (free-identifier=? #'begin/tags #'?begin)))
	 (cons #'begin #'?body))
	(()		'())
	((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
	(?atom		(identifier? #'?atom)	#'?atom)
	(?atom		(syntax->datum #'?atom))))

    (define (stx-list->reversed-tokens sexp reversed-tokens)
      ;;Convert a list of syntax objects to the reversed list of tokens.
      ;;This  is a  recursive function:  it recurses  to  process nested
      ;;lists in the  input SEXP; for this reason  we cannot reverse the
      ;;tokens here, we have to let the caller reverse it.
      ;;
      (syntax-case sexp (syntax quasisyntax quote quasiquote)
	(()
	 reversed-tokens)
	((quote ?expr)
	 (cons (make-<lexical-token> 'NUM sexp) reversed-tokens))
	((quasiquote ?expr)
	 (cons (make-<lexical-token> 'NUM sexp) reversed-tokens))
	((syntax ?expr)
	 (cons (make-<lexical-token> 'NUM sexp) reversed-tokens))
	((quasisyntax ?expr)
	 (cons (make-<lexical-token> 'NUM sexp) reversed-tokens))
	((begin . ?stuff)
	 (cons (make-<lexical-token> 'NUM sexp) reversed-tokens))
	(?id
	 (identifier? #'?id)
	 (case-stx #'?id
	   ((+)		$add)
	   ((-)		$sub)
	   ((*)		$mul)
	   ((/)		$/)
	   ((mod)	$mod)
	   ((mod0)	$mod0)
	   ((expt)	$expt)
	   ((div)	$div)
	   ((div0)	$div0)
	   ((<)		$lt)
	   ((>)		$gt)
	   ((<=)	$le)
	   ((>=)	$ge)
	   ((=)		$eq)
	   ((eq?)	$eq?)
	   ((eqv?)	$eqv?)
	   ((equal?)	$equal?)
	   ((?)		$question)
	   ((:)		$colon)

	   ((incr!)	$incr!)
	   ((decr!)	$decr!)

	   ((and)	$and)
	   ((not)	$not)
	   ((or)	$ior)
	   ((xor)	$xor)

	   ((fx+)	$fxadd)
	   ((fx-)	$fxsub)
	   ((fx*)	$fxmul)
	   ((fxdiv)	$fxdiv)
	   ((fxdiv0)	$fxdiv0)
	   ((fxmod)	$fxmod)
	   ((fxmod0)	$fxmod0)
	   ((fx<?)	$fxlt)
	   ((fx>?)	$fxgt)
	   ((fx<=?)	$fxle)
	   ((fx>=?)	$fxge)
	   ((fx=?)	$fxeq)

	   ((fl+)	$fladd)
	   ((fl-)	$flsub)
	   ((fl*)	$flmul)
	   ((fl/)	$fl/)
	   ((flexpt)	$flexpt)
	   ((fl<?)	$fllt)
	   ((fl>?)	$flgt)
	   ((fl<=?)	$flle)
	   ((fl>=?)	$flge)
	   ((fl=?)	$fleq)

	   ((bitwise-and)			$bit-and)
	   ((bitwise-ior)			$bit-ior)
	   ((bitwise-xor)			$bit-xor)
	   ((bitwise-not)			$bit-not)
	   ((bitwise-arithmetic-shift-left)	$bit-shl)
	   ((bitwise-arithmetic-shift-right)	$bit-shr)

	   ((fxand)				$fxbit-and)
	   ((fxior)				$fxbit-ior)
	   ((fxxor)				$fxbit-xor)
	   ((fxnot)				$fxbit-not)
	   ((fxarithmetic-shift-left)		$fxbit-shl)
	   ((fxarithmetic-shift-right)		$fxbit-shr)

	   (else
	    (make-<lexical-token> 'ID #'?id))))

	((?car . ?cdr)
	 ;;Parentheses  in  reverse order  because  the  TOKENS will  be
	 ;;reversed!!!
	 (stx-list->reversed-tokens
	  #'?cdr
	  (cons $right-paren
		(stx-list->reversed-tokens #'?car (cons $left-paren reversed-tokens)))))

	(_
	 ;;Everything  else is  just  put  there as  "NUM",  that is  as
	 ;;operand.
	 (cons (make-<lexical-token> 'NUM sexp) reversed-tokens))
	))

    (define (lr-driver action-table goto-table reduction-table)
      (define (parser-instance true-lexer error-handler yycustom)
	(let ((stack-values		'(#f))
	      (stack-states		'(0))
	      (reuse-last-token	#f))

	  (define (main lookahead)
	    (let ((category (<lexical-token>-category lookahead)))
	      (if (eq? '*lexer-error* category)
		  (main (attempt-error-recovery lookahead))
		(let ((action (select-action category (current-state))))
		  (cond ((eq? action 'accept) ;success, end of parse
			 (cadr stack-values)) ;return the value to the caller

			((eq? action '*error*) ;syntax error in input
			 (if (eq? category '*eoi*)
			     (error-handler "unexpected end of input" lookahead)
			   (main (attempt-error-recovery lookahead))))

			((<= 0 action) ;shift (= push) token on the stack
			 (stack-push! action (<lexical-token>-value lookahead))
			 (main (if (eq? category '*eoi*)
				   lookahead
				 (begin
				   (reduce-using-default-actions)
				   (lexer)))))

			(else ;reduce using the rule at index "(- ACTION)"
			 (reduce (- action))
			 (main lookahead)))))))

	  (define lexer
	    (let ((last-token #f))
	      (lambda ()
		(if reuse-last-token
		    (set! reuse-last-token #f)
		  (begin
		    (set! last-token (true-lexer))
		    (unless (<lexical-token>? last-token)
		      (error-handler "expected lexical token from lexer" last-token)
		      (true-lexer))))
		last-token)))

	  (define (yypushback)
	    (set! reuse-last-token #t))

	  (define (select-action terminal-symbol state-index)
	    (let* ((action-alist (vector-ref action-table state-index))
		   (pair         (assq terminal-symbol action-alist)))
	      (if pair (cdr pair) (cdar action-alist))))

	  (define (reduce reduction-table-index)
	    (define (%main)
	      (apply (vector-ref reduction-table reduction-table-index)
		     reduce-pop-and-push yypushback yycustom stack-states stack-values))

	    (define (reduce-pop-and-push used-values goto-keyword semantic-clause-result
					 yy-stack-states yy-stack-values)
	      (let* ((yy-stack-states (drop/stx yy-stack-states used-values))
		     (new-state-index (cdr (assq goto-keyword
						 (vector-ref goto-table (car yy-stack-states))))))
		;;This is NOT a call to STACK-PUSH!
		(set! stack-states (cons new-state-index        yy-stack-states))
		(set! stack-values (cons semantic-clause-result yy-stack-values))))

	    (%main))

	  (define (reduce-using-default-actions)
	    (let ((actions-alist (vector-ref action-table (current-state))))
	      (when (= 1 (length actions-alist))
		(let ((default-action (cdar actions-alist)))
		  (when (< default-action 0)
		    (reduce (- default-action))
		    (reduce-using-default-actions))))))

	  (define (attempt-error-recovery lookahead)

	    (define (%main)
	      (error-handler "syntax error, unexpected token" lookahead)
	      (let ((token (synchronise-parser/rewind-stack)))
		;;If recovery succeeds: TOKEN  is set to the next lookahead.
		;;If recovery fails: TOKEN is set to end-of-input.
		(unless (eq? '*eoi* (<lexical-token>-category token))
		  (reduce-using-default-actions))
		token))

	    (define (synchronise-parser/rewind-stack)
	      (if (null? stack-values)
		  (begin ;recovery failed, simulate end-of-input
		    (stack-push! 0 #f) ;restore start stacks state
		    (make-<lexical-token> '*eoi* (eof-object)))
		(let* ((entry (state-entry-with-error-action (current-state))))
		  (if entry
		      (synchronise-lexer/skip-tokens (cdr entry))
		    (begin
		      (stack-pop!)
		      (synchronise-parser/rewind-stack))))))

	    (define-inline (state-entry-with-error-action state-index)
	      (assq 'error (vector-ref action-table state-index)))

	    (define (synchronise-lexer/skip-tokens error-state-index)
	      (let* ((error-actions	   (vector-ref action-table error-state-index))
		     (error-categories (map car (cdr error-actions))))
		(let skip-token ((token lookahead))
		  (let ((category (<lexical-token>-category token)))
		    (cond ((eq? category '*eoi*) ;unexpected end-of-input while trying to recover
			   token)
			  ((memq category error-categories) ;recovery success
			   ;;The following  stack entries will  be processed
			   ;;by  REDUCE-USING-DEFAULT-ACTIONS,  causing  the
			   ;;evaluation  of  the  semantic  action  for  the
			   ;;"error" right-hand  side rule.
			   ;;
			   ;;We want  $1 set  to "error" and  $2 set  to the
			   ;;recovery synchronisation token value.
			   (stack-push! #f 'error)
			   (stack-push! (cdr (assq category error-actions))
					(<lexical-token>-value token))
			   (lexer))
			  (else
			   (skip-token (lexer))))))))

	    (%main))

	  (define-inline (current-state)
	    (car stack-states))

	  (define-inline (stack-push! state value)
	    (set! stack-states (cons state stack-states))
	    (set! stack-values (cons value stack-values)))

	  (define-inline (stack-pop!)
	    (set! stack-states (cdr stack-states))
	    (set! stack-values (cdr stack-values)))

	  (main (lexer))))

      (case-lambda
       ((true-lexer error-handler)
	(parser-instance true-lexer error-handler #f))
       ((true-lexer error-handler yycustom)
	(parser-instance true-lexer error-handler yycustom))))

;;; This function is taken from (nausicaa language infix sexp-parser).
    (define (make-infix-sexp-parser)
      (lr-driver
       '#(((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -32))
	  ((*default* . -29) (LPAREN . 18))
	  ((*default* . *error*) (*eoi* . 41)
           (QUESTION-ID . 40) (AND . 39) (IOR . 38)
           (XOR . 37) (LT . 36) (GT . 35) (LE . 34)
           (GE . 33) (EQ . 32) (ADD . 31) (SUB . 30)
           (MUL . 29) (DIV . 28) (MOD . 27) (EXPT . 26)
           (INCR . 25) (DECR . 24) (BIT-AND . 23)
           (BIT-IOR . 22) (BIT-XOR . 21) (BIT-SHL . 20)
           (BIT-SHR . 19))
	  ((*default* . -26) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -16) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -15) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -14) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -13) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -22) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . *error*) (QUESTION-ID . 40)
           (RPAREN . 42) (AND . 39) (IOR . 38) (XOR . 37)
           (LT . 36) (GT . 35) (LE . 34) (GE . 33) (EQ . 32)
           (ADD . 31) (SUB . 30) (MUL . 29) (DIV . 28)
           (MOD . 27) (EXPT . 26) (INCR . 25) (DECR . 24)
           (BIT-AND . 23) (BIT-IOR . 22) (BIT-XOR . 21)
           (BIT-SHL . 20) (BIT-SHR . 19))
	  ((*default* . -34) (BIT-NOT . 1) (DECR . 2)
           (INCR . 3) (SUB . 4) (ADD . 5) (NOT . 6)
           (LPAREN . 7) (NUM . 8) (ID . 9))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -18)) ((*default* . -17))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -1) (*eoi* . accept))
	  ((*default* . -33))
	  ((*default* . *error*) (RPAREN . 65))
	  ((*default* . -37) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-NOT . 1) (BIT-XOR . 21) (BIT-IOR . 22)
           (BIT-AND . 23) (DECR . 66) (INCR . 67)
           (EXPT . 26) (MOD . 27) (DIV . 28) (MUL . 29)
           (SUB . 68) (ADD . 69) (EQ . 32) (GE . 33)
           (LE . 34) (GT . 35) (LT . 36) (NOT . 6)
           (XOR . 37) (IOR . 38) (AND . 39) (LPAREN . 7)
           (NUM . 8) (QUESTION-ID . 40) (ID . 9))
	  ((*default* . -28)) ((*default* . -27))
	  ((*default* . -25) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -24) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -23) (BIT-SHR . 19) (BIT-SHL . 20))
	  ((*default* . -7) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26))
	  ((*default* . -6) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26))
	  ((*default* . -4) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27))
	  ((*default* . -5) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27))
	  ((*default* . -3) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -2) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -12) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25))
	  ((*default* . -11) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -10) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -9) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -8) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31))
	  ((*default* . -21) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . -20) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . -19) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36))
	  ((*default* . *error*) (QUESTION-ID . 40)
           (COLON-ID . 72) (AND . 39) (IOR . 38) (XOR . 37)
           (LT . 36) (GT . 35) (LE . 34) (GE . 33) (EQ . 32)
           (ADD . 31) (SUB . 30) (MUL . 29) (DIV . 28)
           (MOD . 27) (EXPT . 26) (INCR . 25) (DECR . 24)
           (BIT-AND . 23) (BIT-IOR . 22) (BIT-XOR . 21)
           (BIT-SHL . 20) (BIT-SHR . 19))
	  ((*default* . -30))
	  ((*default* . -18) (BIT-NOT . 1) (DECR . 2)
           (INCR . 3))
	  ((*default* . -17) (BIT-NOT . 1) (DECR . 2)
           (INCR . 3))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -35))
	  ((*default* . -37) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-NOT . 1) (BIT-XOR . 21) (BIT-IOR . 22)
           (BIT-AND . 23) (DECR . 66) (INCR . 67)
           (EXPT . 26) (MOD . 27) (DIV . 28) (MUL . 29)
           (SUB . 68) (ADD . 69) (EQ . 32) (GE . 33)
           (LE . 34) (GT . 35) (LT . 36) (NOT . 6)
           (XOR . 37) (IOR . 38) (AND . 39) (LPAREN . 7)
           (NUM . 8) (QUESTION-ID . 40) (ID . 9))
	  ((*default* . *error*) (ID . 9) (NUM . 8)
           (LPAREN . 7) (NOT . 6) (ADD . 5) (SUB . 4)
           (INCR . 3) (DECR . 2) (BIT-NOT . 1))
	  ((*default* . -3) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29))
	  ((*default* . -2) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29)) ((*default* . -36))
	  ((*default* . -31) (BIT-SHR . 19) (BIT-SHL . 20)
           (BIT-XOR . 21) (BIT-IOR . 22) (BIT-AND . 23)
           (DECR . 24) (INCR . 25) (EXPT . 26) (MOD . 27)
           (DIV . 28) (MUL . 29) (SUB . 30) (ADD . 31)
           (EQ . 32) (GE . 33) (LE . 34) (GT . 35) (LT . 36)
           (XOR . 37) (IOR . 38) (AND . 39)
           (QUESTION-ID . 40)))
       (vector '((1 . 10)) '((1 . 11)) '((1 . 12))
	       '((1 . 13)) '((1 . 14)) '((1 . 15)) '((1 . 16))
	       '((1 . 17)) '() '() '() '() '() '() '() '() '() '()
	       '((2 . 43) (1 . 44)) '((1 . 45)) '((1 . 46))
	       '((1 . 47)) '((1 . 48)) '((1 . 49)) '() '()
	       '((1 . 50)) '((1 . 51)) '((1 . 52)) '((1 . 53))
	       '((1 . 54)) '((1 . 55)) '((1 . 56)) '((1 . 57))
	       '((1 . 58)) '((1 . 59)) '((1 . 60)) '((1 . 61))
	       '((1 . 62)) '((1 . 63)) '((1 . 64)) '() '() '()
	       '((3 . 70) (1 . 71)) '() '() '() '() '() '() '() '()
	       '() '() '() '() '() '() '() '() '() '() '() '() '()
	       '((1 . 12)) '((1 . 13)) '((1 . 73)) '((1 . 74)) '()
	       '((3 . 75) (1 . 71)) '((1 . 76)) '() '() '() '())
       (vector '()
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 $1)
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 $2 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (car $1) $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (car $1) $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (cdr $2) $1)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list (cdr $2) $1)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 1 (list $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $4 $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 4 1 (cons $1 $3)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 5 1 (list $2 $1 $3 $5)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $3 $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 3 1 $2 yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states . yy-stack-values)
		 (yy-reduce-pop-and-push 0 2 '() yy-stack-states
					 yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 2 (cons $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states $2 $1 . yy-stack-values)
		 (yy-reduce-pop-and-push 2 3 (cons $1 $2)
					 yy-stack-states yy-stack-values))
	       (lambda
		   (yy-reduce-pop-and-push yypushback yycustom
					   yy-stack-states . yy-stack-values)
		 (yy-reduce-pop-and-push 0 3 '() yy-stack-states
					 yy-stack-values)))))

    (lambda (stx)
      (syntax-case stx ()
	((?infix ?operand ...)
	 (let ((stx-lst (syntax->list #'(?operand ...))))
	   (cond ((null? stx-lst)
		  #'(values))
		 ((not (pair? stx-lst))
		  stx-lst)
		 (else
		  (let ((tokens (reverse (stx-list->reversed-tokens stx-lst '()))))
		    ((make-infix-sexp-parser) ;parser function
		     (lambda ()		      ;lexer function
		       (if (null? tokens)
			   $eoi
			 (let ((t (car tokens)))
			   (set! tokens (cdr tokens))
			   t)))
		     (lambda (message token) ;error handler
		       (syntax-violation 'infix
			 (string-append "processing infix expression: " message)
			 (syntax->datum #'(?infix ?operand ...)) token))
		     #f))))))))))


;;;; input/output

(define-syntax display
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'%display)
      ((_ ?obj)
       #'(display-1 ?obj))
      ((_ ?obj ?port)
       #'(display-2 ?obj ?port)))))

(define-syntax write
  (lambda (stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'%write)
      ((_ ?obj)
       #'(write-1 ?obj))
      ((_ ?obj ?port)
       #'(write-2 ?obj ?port)))))

(define %display
  (case-lambda
   ((obj)
    (display-1 obj))
   ((obj port)
    (display-2 obj port))))

(define %write
  (case-lambda
   ((obj)
    (write-1 obj))
   ((obj port)
    (write-2 obj port))))

(define-generic display-1 (obj))
(define-generic display-2 (obj port))
(define-generic write-1	(obj))
(define-generic write-2	(obj port))

(module ()

  (add-method display-1	(<top>)		nau.display)
  (add-method display-2	(<top> <port>)	nau.display)
  (add-method write-1	(<top>)		nau.write)
  (add-method write-2	(<top> <port>)	nau.write)

  #| end of module |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'case-stx 'scheme-indent-function 1)
;; End:
