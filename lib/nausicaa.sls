;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/OOPP
;;;Contents: augmented Scheme language around (rnrs)
;;;Date: Wed May 23, 2012
;;;
;;;Abstract
;;;
;;;	This  is a  compound  library re-exporting  bindings from  other
;;;	libraries.   It   defines  the  (nausicaa   oopp)  language:  an
;;;	augmented (rnrs) language.
;;;
;;;Copyright (c) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa)
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
    &procedure-argument-violation
    make-procedure-argument-violation
    procedure-argument-violation?
    procedure-argument-violation

    ;; bignums
    bignum-positive?
    bignum-negative?
    bignum-non-negative?
    bignum-non-positive?
    bignum-odd?
    bignum-even?
    least-positive-bignum
    greatest-negative-bignum

    ;; ASCII and latin1 encodings
    ascii->string			string->ascii
    latin1->string			string->latin1
    ascii-encoded-bytevector?		latin1-encoded-bytevector?

    ;; URI/percent encoding
    string->uri-encoding		uri-encoding->string
    string->percent-encoding		percent-encoding->string
    uri-decode				percent-decode
    uri-encode				percent-encode
    normalise-uri-encoding		normalise-percent-encoding
    uri-encoded-bytevector?		percent-encoded-bytevector?

    ;; misc
    set-cons!
    eval-for-expand
    record-type-field-ref
    record-type-field-set!
    $record-type-field-ref
    $record-type-field-set!
    values->list


;;;; bindings from (nausicaa language oopp)

    (rename
     (set!/tags				set!)
     (define/tags			define)
     (define-values/tags		define-values)
     (begin/tags			begin)
     (lambda/tags			lambda)
     (let/tags				let)
     (let*/tags				let*)
     (letrec/tags			letrec)
     (letrec*/tags			letrec*)
     (let-values/tags			let-values)
     (let*-values/tags			let*-values)
     (receive/tags			receive)
     (receive-and-return/tags		receive-and-return)
     (do/tags				do)
     (do*/tags				do*)
     (case-lambda/tags			case-lambda))

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
    <pair> <mutable-pair> <spine> <list>
    <char> <string> <mutable-string> <vector>
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
    <ascii-bytevector> <percent-encoded-bytevector>

    <hashable-and-properties-clauses>

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
    make-sentinel		sentinel?
    undefined			undefined?
    unspecified			unspecified?


;;;; done exports

)


  (import (for (except (vicare)
		       define-condition-type

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
		       &out-of-memory-error)
	    expand run)
    (for (except (nausicaa language oopp)
		 &tagged-binding-violation
		 make-tagged-binding-violation
		 tagged-binding-violation?)
      expand run)
    (for (nausicaa language multimethods)		expand run)
    (for (nausicaa language builtins)			expand run)
    (for (nausicaa language conditions)			expand run)
    (for (nausicaa language increments)			expand run)
    (for (nausicaa language simple-match)		expand run)
    (for (nausicaa language infix)			expand run)
    (for (vicare language-extensions namespaces)	expand run)
    (for (vicare language-extensions sentinels)		expand run)
    ))

;;; end of file
