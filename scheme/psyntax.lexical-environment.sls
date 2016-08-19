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


(library (psyntax.lexical-environment)
  (export

    ;; core definitions
    make-empty-lexenv
    make-lexenv-entry
    lexenv-entry.label
    lexenv-entry.binding-descriptor		lexenv-entry.binding-descriptor-set!
    push-entry-on-lexenv			push-entries-on-lexenv

    make-syntactic-binding-descriptor
    syntactic-binding-descriptor.type		syntactic-binding-descriptor.type-set!
    syntactic-binding-descriptor.value		syntactic-binding-descriptor.value-set!

    ;; module interfaces
    PSYNTAX-SYNTAX-MATCH
    PSYNTAX-SYNTAX-UTILITIES
    PSYNTAX-TYPE-SYNTAX-OBJECTS
    PSYNTAX-TYPE-SIGNATURES
    PSYNTAX-LAMBDA-SIGNATURES
    PSYNTAX-SYNTACTIC-BINDINGS
    PSYNTAX-ADD-MARK

    ;;configuration
    generate-descriptive-gensyms?
    generate-descriptive-marks?

    top-level-context
    self-evaluating?

    ;; lexical environment
    <lexical-environment>-rtd			<lexical-environment>-rcd
    environment?

    ;; non-interaction environment objects
    <non-interaction-lexical-environment>-rtd	<non-interaction-lexical-environment>-rcd
    make-non-interaction-lexical-environment	non-interaction-lexical-environment?
    env-names
    env-labels
    env-itc

    ;; interaction environment objects
    <interaction-lexical-environment>-rtd	<interaction-lexical-environment>-rcd
    make-interaction-lexical-environment	interaction-lexical-environment?
    interaction-env-rib
    interaction-env-lexenv			set-interaction-env-lexenv!

    ;; operations
    environment-symbols
    environment-labels
    environment-libraries
    environment-binding

    ;; syntax utilities
    generate-temporaries
    syntax-null?			syntax-pair?
    syntax-list?			list-of-identifiers?
    syntax-car				syntax-cdr
    syntax->list			identifiers->list
    all-identifiers?			delete-duplicate-identifiers
    syntax-vector?			syntax-vector->list
    syntax->vector
    syntax-unwrap			syntax=?
    syntax-replace-id

    parse-logic-predicate-syntax

    ;; label gensyms, lexical variable gensyms, storage location gensyms
    generate-lexical-gensym
    generate-storage-location-gensym
    generate-label-gensym

    ;;
    <type-signature>
    <type-signature>-rtd				<type-signature>-rcd
    make-type-signature					type-signature?
    type-signature.syntax-object			type-signature.object-type-specs
    type-signature=?
    type-signature.fully-unspecified?
    type-signature.only-<untyped>-and-<list>?		type-signature.empty?
    type-signature.matching-super-and-sub?		type-signature.compatible-super-and-sub?
    type-signature.single-type?				type-signature.single-top-tag?
    type-signature.single-type-or-fully-untyped?	type-signature.no-return?
    type-signature.match-formals-against-operands
    type-signature.min-count				type-signature.max-count
    type-signature.min-and-max-counts
    type-signature.common-ancestor			type-signature.union
    type-signature.type-descriptor-core-expr

    ;; object types specifications
    <object-type-spec>
    <object-type-spec>-rtd				<object-type-spec>-rcd
    object-type-spec?
    object-type-spec.name				object-type-spec.type-annotation
    object-type-spec.parent-ots				object-type-spec.uids-list
    object-type-spec.constructor-stx			object-type-spec.destructor-stx
    object-type-spec.type-predicate-stx			object-type-spec.equality-predicate
    object-type-spec.comparison-procedure		object-type-spec.hash-function
    object-type-spec.methods-table-public		object-type-spec.methods-table-protected
    object-type-spec.methods-table-private
    object-type-spec.applicable-method-stx		object-type-spec.applicable-private-method-stx
    object-type-spec.applicable-hash-function
    object-type-spec.single-value-validator-lambda-stx	object-type-spec.list-validator-lambda-stx
    object-type-spec.implemented-interfaces
    object-type-spec.type-descriptor-core-expr
    expression-expander-for-core-expressions

    object-type-spec.matching-super-and-sub?		object-type-spec.compatible-super-and-sub?
    object-type-spec=?
    object-type-spec.common-ancestor			object-type-spec.ancestor-ots*
    object-type-spec.procedure?
    object-type-spec.list-sub-type?			object-type-spec.vector-sub-type?

    <core-type-spec>
    <core-type-spec>-rtd				<core-type-spec>-rcd
    make-core-type-spec					core-type-spec?
    core-type-spec.type-descriptor-id			core-type-spec.parent-and-child?

    <closure-type-spec>
    <closure-type-spec>-rtd				<closure-type-spec>-rcd
    make-closure-type-spec				closure-type-spec?
    closure-type-spec.signature				closure-type-spec.set-new-retvals-when-untyped!
    closure-type-spec.thunk?				closure-type-spec.join
    make-closure-type-spec/from-typed-formals

    <struct-type-spec>
    <struct-type-spec>-rtd				<struct-type-spec>-rcd
    make-struct-type-spec				struct-type-spec?
    struct-type-spec.std

    <record-type-spec>
    <record-type-spec>-rtd				<record-type-spec>-rcd
    make-record-type-spec				record-type-spec?
    record-type-spec.rtd-id				record-type-spec.rcd-id
    record-type-spec.super-protocol-id			record-type-spec.virtual-method-signatures
    simple-condition-object-type-spec?

    <compound-condition-type-spec>
    <compound-condition-type-spec>-rtd			<compound-condition-type-spec>-rcd
    make-compound-condition-type-spec			compound-condition-type-spec?
    compound-condition-type-spec.component-ots*

    <union-type-spec>
    <union-type-spec>-rtd				<union-type-spec>-rcd
    make-union-type-spec				union-of-type-specs
    union-type-spec?					union-type-spec.item-ots*

    <intersection-type-spec>
    <intersection-type-spec>-rtd			<intersection-type-spec>-rcd
    intersection-of-type-specs
    intersection-type-spec?				intersection-type-spec.item-ots*

    <complement-type-spec>
    <complement-type-spec>-rtd				<complement-type-spec>-rcd
    make-complement-type-spec				complement-type-spec?
    complement-type-spec.item-ots

    <ancestor-of-type-spec>
    <ancestor-of-type-spec>-rtd				<ancestor-of-type-spec>-rcd
    make-ancestor-of-type-spec				ancestor-of-type-spec?
    ancestor-of-type-spec.item-ots			ancestor-of-type-spec.ancestor-ots*
;;;
    <pair-type-spec>
    <pair-type-spec>-rtd				<pair-type-spec>-rcd
    make-pair-type-spec					pair-type-spec?
    pair-type-spec.car-ots				pair-type-spec.cdr-ots

    <pair-of-type-spec>
    <pair-of-type-spec>-rtd				<pair-of-type-spec>-rcd
    make-pair-of-type-spec				pair-of-type-spec?
    pair-of-type-spec.item-ots
;;;
    <list-type-spec>
    <list-type-spec>-rtd				<list-type-spec>-rcd
    make-list-type-spec					list-type-spec?
    list-type-spec.item-ots*				list-type-spec.length
    list-type-spec.list-of-single-item?

    <list-of-type-spec>
    <list-of-type-spec>-rtd				<list-of-type-spec>-rcd
    make-list-of-type-spec				list-of-type-spec?
    list-of-type-spec.item-ots
;;;
    <vector-type-spec>
    <vector-type-spec>-rtd				<vector-type-spec>-rcd
    make-vector-type-spec				vector-type-spec?
    vector-type-spec.item-ots*				vector-type-spec.length

    <vector-of-type-spec>
    <vector-of-type-spec>-rtd				<vector-of-type-spec>-rcd
    make-vector-of-type-spec				vector-of-type-spec?
    vector-of-type-spec.item-ots
;;;
    <hashtable-type-spec>
    <hashtable-type-spec>-rtd				<hashtable-type-spec>-rcd
    make-hashtable-type-spec				hashtable-type-spec?
    hashtable-type-spec.key-ots				hashtable-type-spec.val-ots

    <alist-type-spec>
    <alist-type-spec>-rtd				<alist-type-spec>-rcd
    make-alist-type-spec				alist-type-spec?
    alist-type-spec.key-ots				alist-type-spec.val-ots

    <enumeration-type-spec>
    <enumeration-type-spec>-rtd				<enumeration-type-spec>-rcd
    make-enumeration-type-spec				enumeration-type-spec?
    enumeration-type-spec.symbol*			enumeration-type-spec.member?

    <label-type-spec>-rtd				<label-type-spec>-rcd
    <label-type-spec>
    make-label-type-spec				label-type-spec?

    <interface-type-spec>-rtd				<interface-type-spec>-rcd
    <interface-type-spec>
    make-interface-type-spec				interface-type-spec?
    interface-type-spec.type-descriptor-id
    interface-type-spec.method-prototypes-table
    assert-implemented-interface-type-and-implementer-interface-type
    build-table-for-interface-types-and-implementer-object-type

    <reference-type-spec>-rtd				<reference-type-spec>-rcd
    <reference-type-spec>
    make-reference-type-spec				reference-type-spec?
    reference-type-spec.object-type-spec		reference-type-spec.object-type-spec-set!
    reference-type-spec.dereference
;;;
    ;; typed variable specification: base type
    <typed-variable-spec>
    typed-variable-spec?
    typed-variable-spec.ots				typed-variable-spec.ots-set!
    typed-variable-spec.private-access?			typed-variable-spec.private-access?-set!

    ;; typed lexical variable specification
    <lexical-typed-variable-spec>
    make-lexical-typed-variable-spec			lexical-typed-variable-spec?
    lexical-typed-variable-spec.lex
    lexical-typed-variable-spec.referenced?		lexical-typed-variable-spec.referenced?-set!
    lexical-typed-variable-spec.assigned?		lexical-typed-variable-spec.assigned?-set!

    <lexical-closure-variable-spec>
    make-lexical-closure-variable-spec			lexical-closure-variable-spec?
    lexical-closure-variable-spec.replacements

    ;; typed global variable specification
    <global-typed-variable-spec>
    make-global-typed-variable-spec			global-typed-variable-spec?
    global-typed-variable-spec.variable-loc

    <global-closure-variable-spec>
    make-global-closure-variable-spec			global-closure-variable-spec?
    global-closure-variable-spec.replacements

    ;; typed core primitive specification
    <core-prim-type-spec>
    make-core-prim-type-spec				core-prim-type-spec?
    core-prim-type-spec.name				core-prim-type-spec.safety
    core-prim-type-spec.replacements
;;;
    make-overloaded-function-spec			overloaded-function-spec?
    overloaded-function-spec.name-id			overloaded-function-spec.ofd-id
    overloaded-function-spec.late-binding-function-id
    overloaded-function-spec.signature*			overloaded-function-spec.id*
    overloaded-function-spec.closure-ots
    overloaded-function-spec.add-specialised-implementation!
    overloaded-function-spec.register-specialisation!
    overloaded-function-spec.expanded-expr
;;;
    ;; lexical environment utilities
    label->syntactic-binding-descriptor
    label->syntactic-binding-descriptor/no-indirection

    system-label	system-label-gensym	SYSTEM-LABEL-GENSYM

    ;; marks of lexical contours
    src-marked?
    rib-src-marked-source-names

    ;; rib objects
    make-rib					rib?
    rib-name*					set-rib-name*!
    rib-mark**					set-rib-mark**!
    rib-label*					set-rib-label*!
    rib-sealed/freq				set-rib-sealed/freq!

    ;; rib operations
    false-or-rib?				list-of-ribs?
    make-rib/empty
    make-rib/from-identifiers-and-labels	make-rib/top-from-source-names-and-labels
    seal-rib!					unseal-rib!
    extend-rib!					export-subst->rib

    ;; syntax objects basics
    <stx>
    <stx>-rtd					<stx>-rcd
    stx?
    stx-expr					stx-mark*
    stx-rib*					stx-annotated-expr*
    <syntactic-identifier>-rtd			<syntactic-identifier>-rcd
    make-syntactic-identifier			syntactic-identifier?
    stx-push-annotated-expr
    syntax-object?
    datum->syntax				~datum->syntax
    syntax->datum
    push-lexical-contour
    syntax-annotation			syntax-object-strip-annotations
    make-top-level-syntactic-identifier-from-source-name-and-label
    make-syntactic-identifier-for-temporary-variable
    make-syntactic-identifier-for-quoted-symbol
    make-top-level-syntax-object/quoted-quoting
    wrap-source-expression

    ;; syntax objects: mapping identifiers to labels and similar
    id->label					id->label/local
    id->object-type-spec			id->record-type-spec
    id->struct-type-spec			type-annotation->object-type-spec
    make-type-specification			syntax-object.type-annotation?
    expression-expander-for-type-annotations
    case-identifier-syntactic-binding-descriptor
    case-identifier-syntactic-binding-descriptor/no-indirection
    __descr__

    ;; identifiers from the built-in environment
    system-id-gensym			system-id
    bless
    trace-bless				debug-bless
    scheme-stx
    core-prim-id			void-id
    underscore-id			underscore-id?
    ellipsis-id				ellipsis-id?
    place-holder-id			place-holder-id?
    brace-id				brace-id?
    method-id				method-id?
    virtual-method-id			virtual-method-id?
    seal-method-id			seal-method-id?
    fields-id				fields-id?
    this-id				this-id?
    public-id
    protected-id
    private-id
    procedure-pred-id
    list-of-id				vector-of-id
    hashtable-id			alist-id
    enumeration-id			void-object?-id?
    define/checked-id			case-define/checked-id
    define/typed-id			case-define/typed-id
    define/std-id			case-define/std-id
    begin-id
    core-prim-spec
    protection-level-id?

    ;; public interface: identifiers handling
    identifier?				false-or-identifier-bound?
    false-or-identifier?
    bound-identifier=?			~bound-identifier=?
    free-identifier=?			~free-identifier=?
    identifier-bound?			~identifier-bound?
    identifier->symbol
    identifier->string			string->identifier
    syntax-parameter-value		retrieve-expand-time-value

    ;; core object-type identifiers
    <untyped>-type-id			<untyped>-type-id?
    <void>-type-id			<void>-type-id?
    <top>-type-id			<top>-type-id?
    <bottom>-type-id			<bottom>-type-id?
    <procedure>-type-id			<procedure>-type-id?
    <pair>-type-id			<pair>-type-id?
    <list>-type-id			<list>-type-id?
    <nelist>-type-id			<nelist>-type-id?
    <null>-type-id			<null>-type-id?
    <vector>-type-id			<vector>-type-id?
    <nevector>-type-id			<nevector>-type-id?
    <empty-vector>-type-id		<empty-vector>-type-id?
    <string>-type-id			<string>-type-id?
    <nestring>-type-id			<nestring>-type-id?
    <empty-string>-type-id		<empty-string>-type-id?
    <stx>-type-id			<stx>-type-id?
    <syntactic-identifier>-type-id	<syntactic-identifier>-type-id?
    <boolean>-type-id
    <true>-type-id			<false>-type-id
    <struct>-type-id
    <record>-type-id			<record>-type-id?
    <condition>-type-id			<condition>-type-id?
    <compound-condition>-type-id	<compound-condition>-type-id?
    &condition-type-id			&condition-type-id?

    ;; core object-type specifications
    <void>-ots				<void>-ots?
    <untyped>-ots			<untyped>-ots?
    <top>-ots				<top>-ots?
    <bottom>-ots			<bottom>-ots?
    <procedure>-ots			<procedure>-ots?
    <pair>-ots				<pair>-ots?
    <null>-ots				<null>-ots?
    <list>-ots				<list>-ots?
    <nelist>-ots			<nelist>-ots?
    <vector>-ots			<vector>-ots?
    <nevector>-ots			<nevector>-ots?
    <empty-vector>-ots			<empty-vector>-ots?
    <string>-ots			<string>-ots?
    <nestring>-ots			<nestring>-ots?
    <empty-string>-ots			<empty-string>-ots?
    <hashtable>-ots			<hashtable>-ots?
    <stx>-ots				<stx>-ots?
    <syntactic-identifier>-ots		<syntactic-identifier>-ots?
    <boolean>-ots			<boolean>-ots?
    <true>-ots				<true>-ots?
    <false>-ots				<false>-ots?
    <symbol>-ots			<symbol>-ots?
    <struct>-ots			<struct>-ots?
    <record>-ots			<record>-ots?
    <condition>-ots			<condition>-ots?
    <compound-condition>-ots		<compound-condition>-ots?
    &condition-ots			&condition-ots?

    ;; utilities for identifiers
    valid-bound-ids?			distinct-bound-ids?
    duplicate-bound-formals?
    bound-id-member?			free-id-member?
    identifier-append

    ;; condition object types
    &application-argument-type-name
    &application-argument-type-name-rtd
    &application-argument-type-name-rcd
    make-application-argument-type-name-condition
    application-argument-type-name-condition?
    condition-application-argument-type-name

    &application-argument-index
    &application-argument-index-rtd
    &application-argument-index-rcd
    make-application-argument-index-condition
    application-argument-index-condition?
    condition-application-argument-index

    &type-method-name
    &type-method-name-rtd
    &type-method-name-rcd
    make-type-method-name-condition
    condition-type-method-name?
    condition-type-method-name

    &syntactic-identifier
    &syntactic-identifier-rtd
    &syntactic-identifier-rcd
    make-syntactic-identifier-condition
    syntactic-identifier-condition?
    condition-syntactic-identifier
;;;
    &syntactic-identifier-resolution-rtd
    &syntactic-identifier-resolution-rcd
    &syntactic-identifier-resolution
    make-syntactic-identifier-resolution-violation
    syntactic-identifier-resolution-violation?

    &syntactic-identifier-unbound-rtd
    &syntactic-identifier-unbound-rcd
    &syntactic-identifier-unbound
    make-syntactic-identifier-unbound-condition
    syntactic-identifier-unbound-condition?

    &syntactic-identifier-not-type-identifier
    &syntactic-identifier-not-type-identifier-rtd
    &syntactic-identifier-not-type-identifier-rcd
    make-syntactic-identifier-not-type-identifier-condition
    syntactic-identifier-not-type-identifier-condition?

    &syntactic-identifier-out-of-context
    &syntactic-identifier-out-of-context-rtd
    &syntactic-identifier-out-of-context-rcd
    make-syntactic-identifier-out-of-context-condition
    syntactic-identifier-out-of-context-condition?
;;;
    &object-type-spec
    &object-type-spec-rtd
    &object-type-spec-rcd
    make-object-type-spec-condition
    object-type-spec-condition?
    condition-object-type-spec

    &syntactic-binding-descriptor
    &syntactic-binding-descriptor-rtd
    &syntactic-binding-descriptor-rcd
    make-syntactic-binding-descriptor-condition
    syntactic-binding-descriptor-condition?
    condition-syntactic-binding-descriptor

    &syntax-definition-expanded-rhs
    &syntax-definition-expanded-rhs-rtd
    &syntax-definition-expanded-rhs-rcd
    make-syntax-definition-expanded-rhs-condition
    syntax-definition-expanded-rhs-condition?
    condition-syntax-definition-expanded-rhs

    &syntax-definition-expression-return-value
    &syntax-definition-expression-return-value-rtd
    &syntax-definition-expression-return-value-rcd
    make-syntax-definition-expression-return-value-condition
    syntax-definition-expression-return-value-condition?
    condition-syntax-definition-expression-return-value

    &type-signature
    &type-signature-rtd
    &type-signature-rcd
    make-type-signature-condition
    type-signature-condition?
    condition-type-signature

    &application-operator-expression
    &application-operator-expression-rtd
    &application-operator-expression-rcd
    make-application-operator-expression-condition
    application-operator-expression-condition?
    condition-application-operator-expression

    &application-operands-expressions
    &application-operands-expressions-rtd
    &application-operands-expressions-rcd
    make-application-operands-expressions-condition
    application-operands-expressions-condition?
    condition-application-operands-expressions

    &application-operator-signature
    &application-operator-signature-rtd
    &application-operator-signature-rcd
    make-application-operator-signature-condition
    application-operator-signature-condition?
    condition-application-operator-signature

    &application-operand-signature
    &application-operand-signature-rtd
    &application-operand-signature-rcd
    make-application-operand-signature-condition
    application-operand-signature-condition?
    condition-application-operand-signature
;;;
    &wrong-number-of-arguments-error-rtd
    &wrong-number-of-arguments-error-rcd
    &wrong-number-of-arguments-error
    make-wrong-number-of-arguments-error-condition
    wrong-number-of-arguments-error-condition?

    &maximum-arguments-count-rtd
    &maximum-arguments-count-rcd
    &maximum-arguments-count
    make-maximum-arguments-count-condition
    maximum-arguments-count-condition?
    condition-maximum-arguments-count

    &minimum-arguments-count-rtd
    &minimum-arguments-count-rcd
    &minimum-arguments-count
    make-minimum-arguments-count-condition
    minimum-arguments-count-condition?
    condition-minimum-arguments-count

    &given-operands-count-rtd
    &given-operands-count-rcd
    &given-operands-count
    make-given-operands-count-condition
    given-operands-count-condition?
    condition-given-operands-count

    &procedure-arguments-signatures-rtd
    &procedure-arguments-signatures-rcd
    &procedure-arguments-signatures
    make-procedure-arguments-signatures-condition
    procedure-arguments-signatures-condition?
    condition-procedure-arguments-signatures

    &application-operands-signature-rtd
    &application-operands-signature-rcd
    &application-operands-signature
    make-application-operands-signature-condition
    application-operands-signature-condition?
    condition-application-operands-signature
;;;
    &expected-type-signature
    &expected-type-signature-rtd
    &expected-type-signature-rcd
    make-expected-type-signature-condition
    expected-type-signature-condition?
    condition-expected-type-signature

    &returned-type-signature
    &returned-type-signature-rtd
    &returned-type-signature-rcd
    make-returned-type-signature-condition
    returned-type-signature-condition?
    condition-returned-type-signature

    &macro-expansion-trace
    &macro-expansion-trace-rtd
    &macro-expansion-trace-rcd
    make-macro-expansion-trace macro-expansion-trace?
    macro-expansion-trace-form

    &expand-time-type-signature-violation
    &expand-time-type-signature-violation-rtd
    &expand-time-type-signature-violation-rcd
    make-expand-time-type-signature-violation
    expand-time-type-signature-violation?

    &expand-time-type-signature-warning
    &expand-time-type-signature-warning-rtd
    &expand-time-type-signature-warning-rcd
    make-expand-time-type-signature-warning
    expand-time-type-signature-warning?

    &expand-time-type-signature-warning-void-operand
    &expand-time-type-signature-warning-void-operand-rtd
    &expand-time-type-signature-warning-void-operand-rcd
    make-expand-time-type-signature-warning-void-operand
    expand-time-type-signature-warning-void-operand?

    &expand-time-type-signature-warning-not-returning-rtd
    &expand-time-type-signature-warning-not-returning-rcd
    &expand-time-type-signature-warning-not-returning
    make-expand-time-type-signature-warning-not-returning
    expand-time-type-signature-warning-not-returning?

    &warning-unused-lexical-variable
    &warning-unused-lexical-variable-rtd
    &warning-unused-lexical-variable-rcd
    make-warning-unused-lexical-variable
    warning-unused-lexical-variable?

    &interface-implementation-violation-rtd
    &interface-implementation-violation-rcd
    &interface-implementation-violation
    make-interface-implementation-violation
    interface-implementation-violation?
    interface-implementation-violation.object-type-name
    interface-implementation-violation.interface-type-name

    &interface-implementation-method-violation-rtd
    &interface-implementation-method-violation-rcd
    &interface-implementation-method-violation
    make-interface-implementation-method-violation
    interface-implementation-method-violation?
    interface-implementation-violation.interface-method-name
    interface-implementation-violation.interface-type-method-signature

    &interface-implementation-missing-method-violation-rtd
    &interface-implementation-missing-method-violation-rcd
    &interface-implementation-missing-method-violation
    make-interface-implementation-missing-method-violation
    interface-implementation-missing-method-violation?

    &interface-implementation-mismatching-method-violation-rtd
    &interface-implementation-mismatching-method-violation-rcd
    &interface-implementation-mismatching-method-violation
    make-interface-implementation-mismatching-method-violation
    interface-implementation-mismatching-method-violation?
    interface-implementation-mismatching-method-violation.object-method-signature

    &dangling-reference-type-spec-rtd
    &dangling-reference-type-spec-rcd
    &dangling-reference-type-spec
    make-dangling-reference-type-spec
    dangling-reference-type-spec?
    dangling-reference-type-spec.name

    assertion-error
    syntax-violation/internal-error
    assertion-violation/internal-error
    syntax-violation
    error-unbound-identifier
    raise-compound-condition-object
    raise-compound-condition-object/continuable

    ;; helpers
    expression-position
    current-run-lexenv

    ;; bindings for (vicare expander)
    (rename (id->label syntactic-identifier->label))
    current-inferior-lexenv)
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer)
    (vicare system $symbols)
    (rnrs mutable-pairs)
    (prefix (rnrs syntax-case) sys::)
    (psyntax.setup)
    (psyntax.compat)
    (psyntax.builders)
    (only (psyntax.special-transformers)
	  expand-time-value?
	  expand-time-value-object)
    (only (psyntax.library-collectors)
	  inv-collector)
    (only (psyntax.library-manager)
	  library?
	  library-name
	  visit-library
	  label->imported-syntactic-binding-descriptor))


;;;; helpers

(include "psyntax.helpers.scm" #t)

(define-syntax let-syntax-rules
  (syntax-rules ()
    ((_ (((?lhs0 . ?args0) ?rhs0) ((?lhs . ?args) ?rhs) ...) ?body0 ?body ...)
     (let-syntax ((?lhs0 (syntax-rules () ((_ . ?args0) ?rhs0)))
		  (?lhs  (syntax-rules () ((_ . ?args)  ?rhs)))
		  ...)
       ?body0 ?body ...))
    ))


;;;; configuration

(define generate-descriptive-gensyms?
  (make-parameter #f))

(define generate-descriptive-marks?
  (make-parameter #f))


;;;; external modules

(include "psyntax.lexical-environment.condition-objects.scm"	#t)

(include "psyntax.lexical-environment.syntax-match.scm"		#t)
(import PSYNTAX-SYNTAX-MATCH)

(include "psyntax.lexical-environment.syntax-utilities.scm"	#t)
(import PSYNTAX-SYNTAX-UTILITIES)

(include "psyntax.lexical-environment.object-type-specs.scm"	#t)
(include "psyntax.lexical-environment.type-signatures.scm"	#t)
(import PSYNTAX-TYPE-SIGNATURES)
(include "psyntax.lexical-environment.lambda-signatures.scm"	#t)
(import PSYNTAX-LAMBDA-SIGNATURES)
(include "psyntax.lexical-environment.type-syntax-objects.scm"	#t)

(include "psyntax.lexical-environment.syntactic-bindings.scm"	#t)
(import PSYNTAX-SYNTACTIC-BINDINGS)
(include "psyntax.lexical-environment.typed-variable-specs.scm"	#t)

(include "psyntax.lexical-environment.overloaded-function-specs.sls"	#t)

;;; --------------------------------------------------------------------

(define* (make-closure-type-spec/from-typed-formals input-form.stx)
  (import PSYNTAX-TYPE-SYNTAX-OBJECTS)
  (receive (standard-formals.stx lambda-signature)
      (syntax-object.parse-typed-clambda-clause-formals input-form.stx)
    (make-closure-type-spec (make-case-lambda-signature (list lambda-signature)))))


;;;; syntactic bindings core definitions

(define-syntax-rule (make-empty-lexenv)
  '())

(define-syntax-rule (make-lexenv-entry ?label ?descr)
  (cons ?label ?descr))

;;Given the entry from a lexical environment: return the gensym acting as label.
;;
(define lexenv-entry.label car)

;;Given the entry from a lexical environment: return the binding value.
;;
(define lexenv-entry.binding-descriptor cdr)
(define lexenv-entry.binding-descriptor-set! set-cdr!)

(define (push-entry-on-lexenv label descr lexenv)
  (cons (make-lexenv-entry label descr) lexenv))

(define (push-entries-on-lexenv label* descriptors* lexenv)
  (fold-left (lambda (lexenv lab des)
	       (push-entry-on-lexenv lab des lexenv))
    lexenv label* descriptors*))

;;; --------------------------------------------------------------------

(define-syntax-rule (make-syntactic-binding-descriptor ?bind-type ?bind-val)
  ;;Build and return a new syntactic binding descriptor.
  ;;
  (cons (quote ?bind-type) ?bind-val))

(define-syntax-rule (syntactic-binding-descriptor.type ?binding-descriptor)
  ;;Given a syntactic binding descriptor, return its type: a symbol.
  ;;
  (car ?binding-descriptor))

(define-syntax-rule (syntactic-binding-descriptor.value ?binding-descriptor)
  ;;Given a syntactic binding descriptor, return its value: a pair.
  ;;
  (cdr ?binding-descriptor))

(define-syntax-rule (syntactic-binding-descriptor.type-set! ?binding-descriptor ?new-type)
  ;;Given a syntactic binding descriptor, return its type: a symbol.
  ;;
  (set-car! ?binding-descriptor ?new-type))

(define-syntax-rule (syntactic-binding-descriptor.value-set! ?binding-descriptor ?new-value)
  ;;Given a syntactic binding descriptor, return its value: a pair.
  ;;
  (set-cdr! ?binding-descriptor ?new-value))


;;;; top-level environments
;;
;;The result of parsing a set of import specs and loading the corresponding libraries
;;with the R6RS function  ENVIRONMENT is a record of type  ENV; ENV records represent
;;an *immutable* top level environment.
;;
;;Whenever  a REPL  is created  (Vicare can  launch multiple  REPLs), an  interaction
;;environment  is  created  to  serve  as top  level  environment.   The  interaction
;;environment is initialised with the  core Vicare library "(vicare)"; an interaction
;;environment  is *mutable*:  new  bindings can  be  added to  it.   For this  reason
;;interaction environments are represented by  records of type INTERACTION-ENV, whose
;;internal format allows adding new bindings.
;;
;;Let's step  back: how does  the REPL  work?  Every time  we type an  expression and
;;press  "Return":  the  expression  is  expanded  in  the  context  of  the  current
;;interaction environment, compiled to machine code, executed.  Every REPL expression
;;is like  a full R6RS program,  with the exception that  the interaction environment
;;"remembers" the bindings we define.
;;
;;Notice that it is possible to use  an interaction environment as second argument to
;;EVAL, allowing for persistence of bindings among evaluations:
;;
;;   (eval '(begin
;;            (define a 1)
;;            (define b 2))
;;         (interaction-environment))
;;   (eval '(list a b)
;;         (interaction-environment))
;;   => (1 2)
;;

;;Set to false or a record of  type INTERACTION-ENV.  This parameter is meant to hold
;;the  top-level   context  when  running   the  REPL   or  invoking  EVAL   with  an
;;INTERACTION-ENV as initial lexical environment.
;;
(define top-level-context
  (make-parameter #f))


;;;; top level environment objects: type definitions

(define-core-record-type (<lexical-environment> unused-make-lexical-environment environment?)
  (define-type-descriptors)
  (nongenerative vicare:expander:<lexical-environment>))

;;; --------------------------------------------------------------------

;;An ENV record encapsulates a substitution and a set of libraries.
;;
(define-core-record-type (<non-interaction-lexical-environment> make-non-interaction-lexical-environment non-interaction-lexical-environment?)
  (nongenerative vicare:expander:<non-interaction-lexical-environment>)
  (define-type-descriptors)
  (parent <lexical-environment>)
  (fields (immutable names	env-names)
		;A vector of symbols representing the public names of bindings from a
		;set of  import specifications as  defined by R6RS.  These  names are
		;from  the  subst  of  the  libraries,  already  processed  with  the
		;directives  in  the import  sets  (prefix,  deprefix, only,  except,
		;rename).
	  (immutable labels	env-labels)
		;A vector of  gensyms representing the labels of bindings  from a set
		;of import specifications as defined  by R6RS.  These labels are from
		;the subst of the libraries.
	  (immutable itc	env-itc)
		;A  collector  function  (see  MAKE-COLLECTOR)  holding  the  LIBRARY
		;records  representing the  libraries selected  by the  source IMPORT
		;specifications.  These libraries have already been interned.
	  #| end of FIELDS |# )
  (custom-printer
    (lambda (S port sub-printer)
      (display "#<non-interaction-lexical-environment>" port))))

;;; --------------------------------------------------------------------

(define-core-record-type (<interaction-lexical-environment> make-interaction-lexical-environment interaction-lexical-environment?)
  (nongenerative vicare:expander:<interaction-lexical-environment>)
  (parent <lexical-environment>)
  (define-type-descriptors)
  (fields (immutable rib	interaction-env-rib)
		;The  top  RIB   structure  for  the  evaluation  of   code  in  this
		;environment.  It maps bound identifiers to labels.
	  (mutable lexenv	interaction-env-lexenv	set-interaction-env-lexenv!)
		;The LEXENV  for both run  time and expand  time.  It maps  labels to
		;syntactic binding descriptors.
	  #| end of FIELDS |# )
  (custom-printer
    (lambda (S port sub-printer)
      (display "#<interaction-lexical-environment>" port))))


;;;; top level environment objects: operations

(define* (environment-symbols {x environment?})
  ;;Return a list of symbols representing the names of the bindings from
  ;;the given environment.
  ;;
  (if (non-interaction-lexical-environment? x)
      (vector->list (env-names x))
    (begin
      #;(assert (interaction-lexical-environment? x))
      (map values (rib-name* (interaction-env-rib x))))))

(define* (environment-labels {x non-interaction-lexical-environment?})
  ;;Return a list of  symbols representing the labels of the  bindings from the given
  ;;environment.
  ;;
  (vector->list (env-labels x)))

(define* (environment-libraries {x non-interaction-lexical-environment?})
  ;;Return  the  list of  LIBRARY  records  representing  the libraries  forming  the
  ;;environment.
  ;;
  ((env-itc x)))

(define* (environment-binding {sym symbol?} {env non-interaction-lexical-environment?})
  ;;Search  the symbol  SYM in  the non-interaction  environment ENV;  if SYM  is the
  ;;public name  of a binding  in ENV  return 2 values:  the label associated  to the
  ;;binding, the list of  values representing the binding.  If SYM  is not present in
  ;;ENV return false and false.
  ;;
  (let ((P (vector-exists (lambda (name label)
			    (import (vicare system $symbols))
			    (and (eq? sym name)
				 (cons label ($symbol-value label))))
	     (env-names  env)
	     (env-labels env))))
    (if P
	(values (car P) (cdr P))
      (values #f #f))))


;;;; label gensym, lexical variable gensyms, storage location gensyms

(define-syntax-rule (%fastest-gensym)
  ($make-symbol #f))

;;Every syntactic binding  has a label associated  to it as unique  identifier in the
;;whole running process; this function generates such labels as gensyms.
;;
;;Labels must have  read/write EQ?  invariance to support  separate compilation (when
;;we write  the expanded sexp to  a file and then  read it back, the  labels must not
;;change and still be globally unique).
;;
(define* (generate-label-gensym seed)
  (if (generate-descriptive-gensyms?)
      (cond ((identifier? seed)
	     ($make-symbol (string-append "lab." ($symbol->string ($identifier->symbol seed)))))
	    ((symbol? seed)
	     ($make-symbol (string-append "lab." ($symbol->string seed))))
	    ((string? seed)
	     ($make-symbol (string-append "lab." seed)))
	    (else
	     (procedure-argument-violation __who__ "expected identifier, symbol or string as argument" seed)))
    (%fastest-gensym)))

;;Generate a unique  symbol to represent the  name of a lexical variable  in the core
;;language forms.  Such symbols have the purpose of being unique in the core language
;;expressions representing a full library or full program.
;;
(define* (generate-lexical-gensym seed)
  (if (generate-descriptive-gensyms?)
      (cond ((identifier? seed)
	     ($make-symbol (string-append "lex." ($symbol->string ($identifier->symbol seed)))))
	    ((symbol? seed)
	     ($make-symbol (string-append "lex." ($symbol->string seed))))
	    (else
	     (procedure-argument-violation __who__ "expected identifier, symbol or string as argument" seed)))
    (%fastest-gensym)))

(define* (generate-storage-location-gensym seed)
  ;;Build  and return  a gensym  to be  used as  storage location  for a
  ;;global lexical variable.  The "value" slot of such gensym is used to
  ;;hold the value of the variable.
  ;;
  (if (generate-descriptive-gensyms?)
      (cond ((identifier? seed)
	     ($make-symbol (string-append "loc." ($symbol->string ($identifier->symbol seed)))))
	    ((symbol? seed)
	     ($make-symbol (string-append "loc." ($symbol->string seed))))
	    ((string? seed)
	     ($make-symbol (string-append "loc." seed)))
	    (else
	     (procedure-argument-violation __who__ "expected identifier, symbol or string as argument" seed)))
    (%fastest-gensym)))


;;;; lexical environment: mapping labels to syntactic binding descriptors

(module (label->syntactic-binding-descriptor
	 label->syntactic-binding-descriptor/no-indirection)
  (import PSYNTAX-SYNTACTIC-BINDINGS)

  (define* (label->syntactic-binding-descriptor/no-indirection label lexenv)
    ;;Look up the  symbol LABEL in the  LEXENV as well as in  the global environment.
    ;;If an  entry with key LABEL  is found: return the  associated syntactic binding
    ;;descriptor;  if  no  matching  entry  is  found,  return  one  of  the  special
    ;;descriptors:
    ;;
    ;;   (displaced-lexical . ())
    ;;   (displaced-lexical . #f)
    ;;
    ;;If the  binding descriptor represents a  fluid syntax or a  synonym syntax: *do
    ;;not* follow through and return the binding descriptor of the syntax definition.
    ;;
    ;;Since all  labels are unique,  it doesn't  matter which environment  we consult
    ;;first; we lookup the global environment first because it's faster.
    ;;
    (cond ((not (symbol? label))
	   ;;If LABEL is  the result of a  previous call to ID->LABEL  for an unbound
	   ;;identifier: LABEL  is false.  This  check makes  it possible to  use the
	   ;;concise expression:
	   ;;
	   ;;   (define ?binding
	   ;;     (label->syntactic-binding-descriptor (id->label ?id) ?lexenv))
	   ;;
	   ;;provided that later we check for the type of ?BINDING.
	   SYNTACTIC-BINDING-DESCRIPTOR/INVALID-LABEL)

	  ;;If  a  label is  associated  to  a  syntactic  binding from  a  library's
	  ;;GLOBAL-ENV (including  those established by  the boot image): it  has the
	  ;;associated descriptor in  its "value" field; otherwise such  field is set
	  ;;to #f.
	  ;;
	  ;;So, if we have a label, we can check if it references an imported binding
	  ;;simply    by    checking    its    "value"   field;    this    is    what
	  ;;LABEL->IMPORTED-SYNTACTIC-BINDING-DESCRIPTOR does.
	  ;;
	  ((label->imported-syntactic-binding-descriptor label)
	   => (lambda (descr)
		;;The  first   time  we  access  a   syntactic  binding's  descriptor
		;;representing a core definition: we mutate  it to a format usable by
		;;the code.
		(cond ((syntactic-binding-descriptor/hard-coded-typed-core-prim? descr)
		       (hard-coded-typed-core-prim-binding-descriptor->type-core-prim-binding-descriptor! descr))

		      ((syntactic-binding-descriptor/hard-coded-core-scheme-type-name? descr)
		       (hard-coded-core-scheme-type-name-symbolic-binding-descriptor->core-scheme-type-name-binding-descriptor! descr))

		      ((syntactic-binding-descriptor/hard-coded-core-condition-object-type-name? descr)
		       (hard-coded-core-condition-object-type-name-binding-descriptor->core-record-type-name-binding-descriptor! descr))

		      ((syntactic-binding-descriptor/hard-coded-core-record-type-name? descr)
		       (hard-coded-core-record-type-name-binding-descriptor->core-record-type-name-binding-descriptor! descr))

		      ((syntactic-binding-descriptor/hard-coded-type-annotation? descr)
		       (hard-coded-core-type-annotation-symbolic-binding-descriptor->core-type-annotation-binding-descriptor! descr)))
		(when (eq? 'core-prim (car descr))
		  (fprintf (current-error-port)
			   "vicare: warning: using untyped core primitive ~s\n" descr))
		descr))

	  ;;Search the given LEXENV.
	  ;;
	  ((assq label lexenv)
	   => lexenv-entry.binding-descriptor)

	  ;;Unbound label.
	  ;;
	  (else
	   SYNTACTIC-BINDING-DESCRIPTOR/UNBOUND-LABEL)))

  (define* (label->syntactic-binding-descriptor label lexenv)
    ;;Look up the  symbol LABEL in the  LEXENV as well as in  the global environment.
    ;;If an  entry with key LABEL  is found: return the  associated syntactic binding
    ;;descriptor;  if  no  matching  entry  is  found,  return  one  of  the  special
    ;;descriptors:
    ;;
    ;;   (displaced-lexical . ())
    ;;   (displaced-lexical . #f)
    ;;
    ;;If the binding  descriptor represents a fluid syntax or  synonym syntax: follow
    ;;through and return the innermost re-definition of the binding.
    ;;
    (if #f	;for debugging purposes
	(receive-and-return (descr)
	    (%label->descriptor label lexenv '())
	  (debug-print __who__ descr))
      (%label->descriptor label lexenv '())))

  (define (%label->descriptor label lexenv accum-labels)
    (let ((descr (label->syntactic-binding-descriptor/no-indirection label lexenv)))
      (cond ((eq? 'displaced-lexical (car descr))
	     descr)
	    ((syntactic-binding-descriptor/fluid-syntax? descr)
	     (%follow-through-fluid-descriptor descr lexenv accum-labels))
	    ((syntactic-binding-descriptor/synonym-syntax? descr)
	     (%follow-through-synonym-descriptor descr lexenv accum-labels))
	    (else
	     descr))))

  (define (%follow-through-fluid-descriptor descr lexenv accum-labels)
    ;;Fluid syntax bindings (created by DEFINE-FLUID-SYNTAX) require different logic.
    ;;The lexical environment contains the main fluid syntax definition entry and one
    ;;or more subordinate fluid syntax re-definition entries.
    ;;
    ;;LABEL is associated to the main binding definition entry; its syntactic binding
    ;;descriptor  contains the  fluid  label,  which is  associated  to  one or  more
    ;;subordinate  re-definitions.  We  extract the  fluid label  from BINDING,  then
    ;;search for the innermost fluid binding associated to it.
    ;;
    ;;Such search  for the fluid  re-definition binding  must begin from  LEXENV, and
    ;;then  in  the  global  environment.   This  is because  we  can  nest  at  will
    ;;FLUID-LET-SYNTAX forms that  redefine the binding by  pushing new re-definition
    ;;entries on  the LEXENV.  To  reach for the innermost  we must query  the LEXENV
    ;;first.
    ;;
    ;;If there is no binding descriptor for FLUID-LABEL: the return value will be:
    ;;
    ;;   (displaced-lexical . #f)
    ;;
    (let* ((fluid-label (syntactic-binding-descriptor/fluid-syntax.fluid-label descr))
	   (fluid-descr (cond ((assq fluid-label lexenv)
			       => lexenv-entry.binding-descriptor)
			      (else
			       (label->syntactic-binding-descriptor/no-indirection fluid-label '())))))
      (if (syntactic-binding-descriptor/synonym-syntax? fluid-descr)
	  (%follow-through-synonym-descriptor descr lexenv accum-labels)
	fluid-descr)))

  (define (%follow-through-synonym-descriptor descr lexenv accum-labels)
    (let ((synonym-label (syntactic-binding-descriptor/synonym-syntax.synonym-label descr)))
      (if (memq synonym-label accum-labels)
	  (syntax-violation #f "circular reference detected while resolving synonym transformers" #f)
	(%label->descriptor synonym-label lexenv (cons synonym-label accum-labels)))))

  (define-constant SYNTACTIC-BINDING-DESCRIPTOR/UNBOUND-LABEL
    '(displaced-lexical . #f))

  (define-constant SYNTACTIC-BINDING-DESCRIPTOR/INVALID-LABEL
    '(displaced-lexical . ()))

  #| end of module |# )


;;;; marks of lexical contours

(module (generate-new-mark)

  (define counter 0)

  (define (generate-new-mark)
    ;;Generate a new unique mark.  We want a new string for every function call.
    ;;
    (if (generate-descriptive-marks?)
	(begin
	  (set! counter (fxadd1 counter))
	  (string-append "mark." (number->string counter)))
      (string)))

  #| end of module |# )

;;By default we use #f as the anti-mark.
;;
(define-constant THE-ANTI-MARK #f)

(define-syntax-rule (lexical-contour-mark? ?obj)
  (string? ?obj))

(define-syntax-rule (src-mark? ?obj)
  (eq? ?obj 'src))

(define-syntax-rule (anti-mark? ?obj)
  (not ?obj))

(define (mark? obj)
  (or (src-mark? obj)
      (lexical-contour-mark? obj)
      (anti-mark? obj)))

(define-list-of-type-predicate list-of-marks? mark?)

;;The body of a library, when it is first processed, gets this set of marks:
;;
;;* This for the "mark*" field of "stx" objects:
(define-constant SRC-MARK*	'(src))
;;
;;* This for the "mark**" field of "rib" objects:
(define-constant SRC-MARK**	'((src)))
;;
;;consequently, every  syntax object  that has a  "src" symbol in  its marks  set was
;;present in the program source.
;;
(define-syntax-rule (src-marked? mark*)
  (memq 'src mark*))


;;;; rib type definition

(define-core-record-type rib
  (nongenerative vicare:expander:rib)
  (fields
   (mutable name*	rib-name*	set-rib-name*!)
		;List  of symbols  representing  the original  binding  names in  the
		;source code.
		;
		;When the RIB is sealed: the list is converted to a vector.

   (mutable mark**	rib-mark**	set-rib-mark**!)
		;List of  sublists of marks;  there is a  sublist of marks  for every
		;item in NAME*.
		;
		;When the RIB is sealed: the list is converted to a vector.

   (mutable label*	rib-label*	set-rib-label*!)
		;List of  label gensyms uniquely identifying  the syntactic bindings;
		;there is a label for each item in NAME*.
		;
		;When the RIB is sealed: the list is converted to a vector.

   (mutable sealed/freq	rib-sealed/freq	set-rib-sealed/freq!)
		;False  or  vector  of  exact  integers.  When  false:  this  RIB  is
		;extensible, that is new bindings can be added to it.  When a vector:
		;this RIB is  sealed; see the documentation in Texinfo  format for an
		;explanation of the frequency vector.
   #| end of FIELDS |# )
  (custom-printer
    (lambda (S port subwriter) ;record printer function
      (define-syntax-rule (%display ?thing)
	(display ?thing port))
      (define-syntax-rule (%write ?thing)
	(write ?thing port))
      (define-syntax-rule (%pretty-print ?thing)
	(pretty-print* ?thing port 0 #f))
      (%display "#<rib")
      (%display " name*=")		(%display (rib-name*  S))
      ;; (%display " mark**=")		(subwriter (rib-mark** S))
      ;; (%display " label*=")		(subwriter (rib-label* S))
      ;; (%display " sealed/freq=")	(subwriter (rib-sealed/freq S))
      (%display ">"))
    ))

;;; --------------------------------------------------------------------

(define (false-or-rib? obj)
  (or (not obj)
      (rib? obj)))

(define-list-of-type-predicate list-of-ribs? rib?)

(define (rib-or-shift? obj)
  (or (rib? obj)
      (eq? obj 'shift)))

(define-list-of-type-predicate list-of-ribs-and-shifts? rib-or-shift?)

;;; --------------------------------------------------------------------
;;; rib constructors

(define-syntax-rule (make-rib/empty)
  ;;Build and return a new, empty RIB object.
  ;;
  ;;Empty ribs  are used to  represent freshly created  lexical contours in  which no
  ;;initial  bindings  are  defined.   For example,  internal  bodies  might  contain
  ;;internal definitions:
  ;;
  ;;   (internal-body
  ;;     (define a 1)
  ;;     (define b 2)
  ;;     (display a))
  ;;
  ;;but we  know about  them only when  we begin their  expansion; when  creating the
  ;;lexical contour for them we must create an empty rib.
  ;;
  ;;If  STX* is  a  list of  syntax  objects representing  expressions  that must  be
  ;;expanded in a new lexical contour:
  ;;
  ;;   (define stx*  #'((define a 1)
  ;;                    (define b 2)
  ;;                    (display a)))
  ;;
  ;;we do:
  ;;
  ;;   (define rib (make-rib/empty))
  ;;   (define stx (push-lexical-contour rib stx*))
  ;;
  ;;and then hand the resulting syntax object STX to the appropriate "chi-*" function
  ;;to perform the expansion.  Later we can add bindings to the rib with:
  ;;
  ;;   (extend-rib! rib id label shadow/redefine-bindings?)
  ;;
  (make-rib '() '() '() #f))

(define (make-rib/from-identifiers-and-labels id* label*)
  ;;Build  and return  a  new RIB  record  initialised with  bindings  having ID*  as
  ;;original identifiers and LABEL* as associated label gensyms.
  ;;
  ;;The  argument ID*  must  be  a list  of  syntactic  identifiers representing  the
  ;;original binding  names.  The argument LABEL*  must be the list  of label gensyms
  ;;associated to the identifiers.
  ;;
  ;;For  example, when  creating a  rib to  represent the  lexical context  of a  LET
  ;;syntax:
  ;;
  ;;   (let ((?lhs* ?rhs*) ...) ?body* ...)
  ;;
  ;;we do:
  ;;
  ;;   (define lhs.id*    ?lhs*)
  ;;   (define body.stx   ?body*)
  ;;   (define lhs.label* (map generate-label-gensym lhs.id*))
  ;;   (define rib        (make-rib/from-identifiers-and-labels lhs.id* lhs.label*))
  ;;   (define body.stx^  (push-lexical-contour rib body.stx))
  ;;
  ;;and then  hand the resulting syntax  object BODY.STX^ to the  appropriate "chi-*"
  ;;function to perform the expansion.
  ;;
  (let ((name*        (map identifier->symbol id*))
	(mark**       (map stx-mark* id*))
	(sealed/freq  #f))
    (make-rib name* mark** label* sealed/freq)))

(define (make-rib/top-from-source-names-and-labels source-name* lab*)
  ;;Build and return a  new "rib" object to be used at the  top-level of a library or
  ;;program body.
  ;;
  ;;The argument SOURCE-NAME*  must be a list of symbols  representing names, without
  ;;duplicates,  of syntactic  bindings  as  they appear  in  the  source code.   The
  ;;argument LAB* must  be the list of label gensyms,  without duplicates, associated
  ;;to the SOURCE-NAME* bindings.
  ;;
  (let ((sealed/freq #f))
    (make-rib source-name* (map (lambda (dummy) SRC-MARK*) lab*) lab* sealed/freq)))

(define (make-rib/top-from-source-name-and-label source-name lab)
  ;;Build and return a  new "rib" object to be used at the  top-level of a library or
  ;;program body.
  ;;
  ;;The argument  SOURCE-NAME must be a  symbol representing the name  of a syntactic
  ;;binding as  it appears in the  source code.  The  argument LAB must be  the label
  ;;gensym associated to the SOURCE-NAME binding.
  ;;
  (let ((sealed/freq #f))
    (make-rib (list source-name) SRC-MARK** (list lab) sealed/freq)))

;;; --------------------------------------------------------------------

(define (export-subst->rib export-subst)
  ;;Build  and  return   a  new  RIB  structure  initialised  with   the  entries  of
  ;;EXPORT-SUBST, which is meant to come from a library object.
  ;;
  ;;An  EXPORT-SUBST  selects the  exported  bindings  among the  syntactic  bindings
  ;;defined at  the top-level of a  LIBRARY form; it is  an alist whose keys  are the
  ;;external symbol names  of the bindings and whose values  are the associated label
  ;;gensyms.
  ;;
  ;;Being defined at  the top-level: every name  is associated to the  "src" mark, so
  ;;its list of marks is SRC-MARK*.
  ;;
  (let loop ((nam.lab*  export-subst)
	     (name*     '())
	     (mark**    '())
	     (label*    '()))
    (if (pair? nam.lab*)
	(let ((nam.lab ($car nam.lab*)))
	  (loop ($cdr nam.lab*)
		(cons ($car nam.lab) name*)
		(cons SRC-MARK*      mark**)
		(cons ($cdr nam.lab) label*)))
      (let ((sealed/freq #f))
	(make-rib name* mark** label* sealed/freq)))))

;;; --------------------------------------------------------------------
;;; rib sealing

(define* (seal-rib! {rib rib?})
  (let ((name* (rib-name* rib)))
    (unless (null? name*) ;only seal if RIB is not empty
      (let ((name* (list->vector name*)))
	(set-rib-name*!       rib name*)
	(set-rib-mark**!      rib (list->vector (rib-mark** rib)))
	(set-rib-label*!      rib (list->vector (rib-label* rib)))
	(set-rib-sealed/freq! rib (make-vector (vector-length name*) 0))))))

(define* (unseal-rib! {rib rib?})
  (when (rib-sealed/freq rib)
    (set-rib-sealed/freq! rib #f)
    (set-rib-name*!       rib (vector->list (rib-name*  rib)))
    (set-rib-mark**!      rib (vector->list (rib-mark** rib)))
    (set-rib-label*!      rib (vector->list (rib-label* rib)))))

;;; --------------------------------------------------------------------

(module (extend-rib!)

  (define* (extend-rib! {rib rib?} {id identifier?} label shadow/redefine-bindings?)
    ;;Add  to  the  RIB  a  syntactic binding's  association  between  the  syntactic
    ;;identifier ID and the LABEL gensym.
    ;;
    ;;This  function  is used  only  while  expanding  a  body, either:  a  library's
    ;;top-level body; a  program's top-level body; an internal body  (uses of LET and
    ;;similar syntaxes,  uses of INTERNAL-BODY);  a top-level expression  expanded in
    ;;the context of an interaction environment.
    ;;
    ;;This function is used to add to a rib object the syntactic bindings established
    ;;by  the  syntaxes:  DEFINE, DEFINE-SYNTAX,  DEFINE-FLUID-SYNTAX,  DEFINE-ALIAS,
    ;;MODULE, IMPORT.
    ;;
    ;;This function  is called  to establish new  syntactic bindings  (either lexical
    ;;variables or macros), but  it is also called when an  IMPORT syntax is expanded
    ;;in an  internal body.   In the first  case, we  need to raise  an error  when a
    ;;syntactic binding is established twice:
    ;;
    ;;   (define a 1)
    ;;   (define a 2)
    ;;
    ;;in the second case, we need to allow:
    ;;
    ;;   (import (rnrs))
    ;;   (import (rnrs))
    ;;
    ;;because importing twice the same binding is fine.
    ;;
    ;;This function  is the only  place where the  argument SHADOW/REDEFINE-BINDINGS?
    ;;makes some difference:
    ;;
    ;;* When SHADOW/REDEFINE-BINDINGS?  is false: if a syntactic binding capturing ID
    ;;already exists  in RIB,  the call  to this function  is interpreted  as illegal
    ;;attempt  to:  redefine  a  previously   defined  syntactic  binding;  shadow  a
    ;;previously imported syntactic  binding.  It is responsibility  of this function
    ;;to raise an exception.
    ;;
    ;;* When SHADOW/REDEFINE-BINDINGS?  is true:  if a syntactic binding capturing ID
    ;;already exists  in RIB,  the new  syntactic binding is  allowed to  redefine or
    ;;shadow the existing  syntactic binding, by replacing the  original label gensym
    ;;with LABEL.
    ;;
    ;;See the  documentation of CHI-BODY*  for the  full description of  the argument
    ;;SHADOW/REDEFINE-BINDINGS?.
    ;;
    (when (rib-sealed/freq rib)
      (assertion-violation/internal-error __who__
	"attempt to extend sealed RIB" rib))
    (let ((id.source-name  ($identifier->symbol id))
	  (id.mark*        (stx-mark*  id))
	  (rib.name*       (rib-name*  rib))
	  (rib.mark**      (rib-mark** rib))
	  (rib.label*      (rib-label* rib)))
      (cond ((and (memq id.source-name rib.name*)
		  (%find-syntactic-binding-with-same-name-and-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*))
	     => (lambda (tail-of-label*)
		  ;;If  we  are here:  we  have  found  in  RIB a  syntactic  binding
		  ;;capturing ID (same source-name, same marks).
		  (cond ((let ((label-in-rib (car tail-of-label*)))
			   (and (label-binding label)
				(eq? label label-in-rib)))
			 ;;This happens when importing twice the same binding, as in:
			 ;;
			 ;;   (import (rnrs))
			 ;;   (import (rnrs))
			 ;;
			 (void))
			(shadow/redefine-bindings?
			 ;;We replace  the old  label with  the new  one, in-so-doing
			 ;;redefining or shadowing the already existing binding.
			 (set-car! tail-of-label* label))
			(else
			 ;;Signal an error if the  identifier was already in the rib.
			 ;;This is the case of:
			 ;;
			 ;;   (define a 1)
			 ;;   (define a 2)
			 ;;
			 (syntax-violation __who__ "multiple definitions of identifier" id)))))
	    (else
	     ;;No capturing binding  exists for ID in RIB: let's  establish a new one
	     ;;by pushing the appropriate tuple on the rib.
	     (set-rib-name*!  rib (cons id.source-name  rib.name*))
	     (set-rib-mark**! rib (cons id.mark*        rib.mark**))
	     (set-rib-label*! rib (cons label           rib.label*))))))

  (define (%find-syntactic-binding-with-same-name-and-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*)
    ;;Here we  know that the  list of source-names RIB.NAME*  has one element  EQ? to
    ;;ID.SOURCE-NAME; we  do not know,  yet, if such  source-name is associated  to a
    ;;list of marks equal to ID.MARK* or not.   In other words: we do not know if the
    ;;rib holds a syntactic binding capturing ID.
    ;;
    ;;Iterate the tuples in RIB.NAME*, RIB.MARK**  and RIB.LABEL* looking for a tuple
    ;;having  name  equal  to  ID.SOURCE-NAME  and marks  equal  to  ID.MARK*.   When
    ;;successful: return the  tail sublist of RIB.LABEL* having  the associated label
    ;;as car.  If no capturing binding is found: return false.
    ;;
    (define-syntax-rule (%recurse rib.name* rib.mark** rib.label*)
      (%find-syntactic-binding-with-same-name-and-same-marks id.source-name id.mark* rib.name* rib.mark** rib.label*))
    (and (pair? rib.name*)
	 (if (and (same-name?  id.source-name ($car rib.name*))
		  (same-marks? id.mark*       ($car rib.mark**)))
	     ;;Capturing binding found!
	     rib.label*
	   (%recurse ($cdr rib.name*) ($cdr rib.mark**) ($cdr rib.label*)))))

  (define-syntax-rule (same-name? x y)
    (eq? x y))

  #| end of module: EXTEND-RIB! |# )

;;; --------------------------------------------------------------------

(define* (rib-src-marked-source-names {rib rib?})
  ;;Scan the  RIB and return a  list of symbols representing  syntactic binding names
  ;;and having only the "src" mark.
  ;;
  (define (%src-marks? obj)
    ;;Return true if OBJ is equal to SRC-MARK*.
    ;;
    (and (pair? obj)
	 (eq? 'src ($car obj))
	 (null? ($cdr obj))))
  (let ((name*  (rib-name*  rib))
	(mark** (rib-mark** rib)))
    (if (rib-sealed/freq rib)
	;;This rib is sealed: here NAME* and MARK** are vectors.
	(let recur ((i    0)
		    (imax ($vector-length name*)))
	  (let-syntax-rules
	      (((%recursion)       (recur ($fxadd1 i) imax))
	       ((%more?)           ($fx< i imax))
	       ((%next-mark* arg)  ($vector-ref arg i))
	       ((%next-name arg)   ($vector-ref arg i)))
	    (if (%more?)
		(if (%src-marks? (%next-mark* mark**))
		    (cons (%next-name name*) (%recursion))
		  (%recursion))
	      '())))
      ;;This rib is not sealed: here NAME* and MARK** are lists.
      (let recur ((name*  name*)
		  (mark** mark**))
	(let-syntax-rules
	    (((%recursion)       (recur ($cdr name*) ($cdr mark**)))
	     ((%more?)           (pair? name*))
	     ((%next-mark* arg)  ($car arg))
	     ((%next-name arg)   ($car arg)))
	  (if (%more?)
	      (if (%src-marks? (%next-mark* mark**))
		  (cons (%next-name name*) (%recursion))
		(%recursion))
	    '()))))))


;;;; helpers to handle syntax object's wraps

(define (%merge-annotated-expr* ls1 ls2)
  ;;Append LS1  and LS2 in  a way that is  appropriate for merging  "<stx>" annotated
  ;;expressions; return the result.
  ;;
  ;;   (%merge-annotated-expr* '(a b c) '(d  e f))   => (a b c d e f)
  ;;   (%merge-annotated-expr* '(a b c) '(#f e f))   => (a b c e f)
  ;;   (%merge-annotated-expr* '(#f)    '(d  e f))   => (#f d e f)
  ;;   (%merge-annotated-expr* '()      '(d  e f))   => (#f d e f)
  ;;   (%merge-annotated-expr* '()      '(#f e f))   => (#f d e f)
  ;;
  ;;Comments:
  ;;
  ;;* The case:
  ;;
  ;;     (%merge-annotated-expr* '(#f)    '(d  e f))   => (#f d e f)
  ;;
  ;;  happens in  ADD-MARK when an anti-mark is  pushed on the input form  of a macro
  ;;  transformer.
  ;;
  ;;* The case:
  ;;
  ;;     (%merge-annotated-expr* '(a b c) '(#f e f))   => (a b c e f)
  ;;
  ;;  happens in ADD-MARK while pushing down the marks.
  ;;
  ;;* The cases:
  ;;
  ;;     (%merge-annotated-expr* '()      '(d  e f))   => (#f d e f)
  ;;     (%merge-annotated-expr* '()      '(#f e f))   => (#f d e f)
  ;;
  ;;  happen  when a  syntax object is  wrapped into another  syntax object  for some
  ;;  purpose.  For example when using PUSH-LEXICAL-CONTOUR, which is defined as:
  ;;
  ;;     (define (push-lexical-contour rib expr.stx)
  ;;       (let ((mark* '())
  ;;             (ae*   '()))
  ;;         (mkstx expr.stx mark* (list rib) ae*)))
  ;;
  ;;  In these cases we really need to *keep* the #f in LS2.
  ;;
  (append ls1 (if (and (pair? ls1)
		       (pair? ls2)
		       (anti-mark? (car ls2)))
		  ;;Here we know the first item in LS2 is #f.
		  (cdr ls2)
		ls2)))


;;;; handling marks in wrapped syntax objects
;;
;;So, what's  an anti-mark and why  is it there?  The  theory goes like this,  when a
;;macro call is encountered:
;;
;;1. The anti-mark is pushed on the MARK* of the syntax object representing the input
;;form.
;;
;;2. The macro transformer function is applied to the anti-marked input form.
;;
;;3. A fresh  mark is pushed on the  MARK* list of the syntax object  returned by the
;;transformer.
;;
;;In the MARK*: when a mark collides  with an anti-mark, they cancel one another when
;;joining the wraps:
;;
;;   (new-mark anti-mark mark1 mark0) => (mark1 mark0)
;;   (new-mark markA markB)           => (new-mark markA markB)
;;
;;therefore:
;;
;;* Any  part of the  input form that  is copied  to the output  form has a  new mark
;;followed immediately by an anti-mark, resulting in the same syntax object (no extra
;;marks) after joining the marks.
;;
;;* Any part of the output form that was  not present in the input form (e.g. that is
;;inserted  in the  output form  by the  macro transformer)  has *no*  anti-mark and,
;;therefore, the new mark is present in the output.
;;
;;So even if they  have the same name: syntactic identifiers from  the input form and
;;syntactic identifiers inserted by the macro transformer are not FREE-IDENTIFIER=?.
;;
;;Every  time  a  mark  is  pushed  to  the MARK*  of  a  wrapped  syntax  object:  a
;;corresponding  "shift" symbol  is pushed  to the  RIB* of  the same  wrapped syntax
;;object.  Every time  a mark is cancelled by an  anti-mark, the corresponding shifts
;;are also cancelled.

;;The procedure  JOIN-WRAPS is  used to  compute the  new MARK*  and RIB*  that would
;;result when the M1* and S1* are added  to a wrapped syntax object's MARK* and RIB*.
;;The only tricky  part is that e may  have an anti-mark that should  cancel with the
;;last mark in m1*.  So, if:
;;
;;  m1* = (mx* ... mx)
;;  m2* = (#f my* ...)
;;
;;then the resulting marks should be:
;;
;;  (mx* ... my* ...)
;;
;;since MX will cancel with the anti-mark.  The ribs will have to also cancel since:
;;
;;    s1* = (sx* ... sx)
;;    s2* = (sy sy* ...)
;;
;;then the resulting ribs should be:
;;
;;    (sx* ... sy* ...)
;;
;;Notice that both SX and SY would be shift marks.
;;
;;All this work  of pushing marks, anti-marks  and joining marks is  performed by the
;;functions ADD-MARK and DO-MACRO-CALL.
;;

(define (same-marks? x y)
  ;;Two lists of marks  are considered the same if they have the  same length and the
  ;;corresponding marks on each are EQ?.
  ;;
  (or (eq? x y)
      (and (pair? x) (pair? y)
	   (eq? (car x) (car y))
	   (same-marks? (cdr x) (cdr y)))))

(module (join-wraps)

  (define* (join-wraps {stx1.mark* list-of-marks?} {stx1.rib* list-of-ribs-and-shifts?} stx1.ae {stx2 stx?})
    ;;Join the given wraps with the ones in STX2 and return the resulting wraps; with
    ;;"wraps"  we  mean the  marks  and  ribs, with  the  addition  of the  annotated
    ;;expressions for debugging purposes.  The scenario is this:
    ;;
    ;;* A  syntax object STX1  (wrapped or  partially unwrapped) contains  the syntax
    ;;object STX2 (an instance of stx) as subexpression.
    ;;
    ;;* Whenever STX1 is fully unwrapped  (for example by SYNTAX-MATCH) its marks and
    ;;ribs must be propagated to all its identifier subexpressions.
    ;;
    ;;* In practice:  the marks of STX1 must  be prepended to the marks  of STX2, the
    ;;ribs of STX1 must be prepended to the ribs of STX2.
    ;;
    ;;this  is  what  this  function  does.   But:  we  must  also  handle  anti-mark
    ;;annihilation and the associated removal of shifts from the ribs.
    ;;
    (let ((stx2.mark* (stx-mark*		stx2))
	  (stx2.rib*  (stx-rib*		stx2))
	  (stx2.ae*   (stx-annotated-expr*	stx2)))
      ;;If the first item in stx2.mark* is an anti-mark...
      (if (and (not (null? stx1.mark*))
	       (not (null? stx2.mark*))
	       (anti-mark? (car stx2.mark*)))
	  ;;...cancel mark, anti-mark, and corresponding shifts.
	  (values (%append-cancel-facing stx1.mark* stx2.mark*)
		  (%append-cancel-facing stx1.rib*  stx2.rib*)
		  (%merge-annotated-expr* stx1.ae stx2.ae*))
	;;..else no cancellation takes place.
	(values (append stx1.mark* stx2.mark*)
		(append stx1.rib*  stx2.rib*)
		(%merge-annotated-expr* stx1.ae stx2.ae*)))))

  (define (%append-cancel-facing ls1 ls2)
    ;;Expect LS1 to be a non-empty proper list and LS2 to be a non-empty proper list.
    ;;Discard  the last  item in  LS1;  discard the  first  item in  LS2; append  the
    ;;resulting lists:
    ;;
    ;;   (%append-cancel-facing '(1 2 3) '(4 5 6))	=> (1 2 5 6)
    ;;   (%append-cancel-facing '(1)     '(2 3 4))	=> (3 4)
    ;;   (%append-cancel-facing '(1)     '(2))		=> ()
    ;;
    (let recur ((A1 (car ls1))
		(D1 (cdr ls1)))
      (if (pair? D1)
	  (cons A1 (recur (car D1) (cdr D1)))
	;;Here D1 is null; A1 is discarded; the car of LS2 is discarded.
	(cdr ls2))))

  #| end of module: JOIN-WRAPS |# )


;;;; adding marks and the anti-mark

(module PSYNTAX-ADD-MARK
  (add-new-mark
   add-anti-mark)

  (define (add-anti-mark input-form.stx)
    ;;Push an anti-mark on the input form of a macro use.
    ;;
    (add-mark THE-ANTI-MARK #f input-form.stx #f))

  (define (add-new-mark rib output-form.stx input-form.stx)
    ;;Push a new mark on the output form of a macro use.
    ;;
    (add-mark (generate-new-mark) rib output-form.stx input-form.stx))

  (define* (add-mark mark {rib false-or-rib?} expr.stx annotated-expr.stx)
    ;;Build and return  a new syntax object wrapping EXPR.STX  and having MARK pushed
    ;;on its list of marks.  This function used only in 2 places:
    ;;
    ;;* It is applied  to the input form of a macro transformer,  with MARK being the
    ;;  anti-mark.
    ;;
    ;;* It is  applied to the output form  of a macro transformer, with  MARK being a
    ;;  proper mark.
    ;;
    ;;MARK is either the anti-mark or a new mark.
    ;;
    ;;RIB can be #f (when MARK is the  anti-mark) or an instance of rib (when MARK is
    ;;a new mark).
    ;;
    ;;EXPR.STX is  either the input  form of a macro  transformer call or  the output
    ;;form of a macro transformer call; it must be a syntax object, either wrapped or
    ;;unwrapped.
    ;;
    ;;ANNOTATED-EXPR.STX is either #f (when MARK  is the anti-mark) or the input form
    ;;of a macro call (when MARK is a new mark).  This argument is used to keep track
    ;;of the transformation a form undergoes when processed as macro use.
    ;;
    ;;The return  value can be either  a "<stx>" instance or  a (partially) unwrapped
    ;;syntax object.
    ;;
    (%push-down-marks expr.stx mark rib expr.stx '() (list annotated-expr.stx)))

  (define (%push-down-marks top-expr.stx mark rib expr.stx accum-rib* accum-ae*)
    ;;Recursive function.   Partially unwraps EXPR.STX  pushing down the  marks, ribs
    ;;and annotated  expressions to nested  "<stx>" instances having  non-empty MARK*
    ;;list; return the resulting syntax object.
    ;;
    ;;The argument TOP-EXPR.STX is the original  syntax object handed to ADD-MARK; it
    ;;is used only when raising an  exception with compound condition object having a
    ;;component of type "&syntax".
    ;;
    ;;The argument MARK  is either the anti-mark  or a proper new mark  which we must
    ;;push on top of all the MARK* lists.
    ;;
    ;;The  argument EXPR.STX  is a  wrapped,  unwrapped or  partially wrapped  syntax
    ;;object  representing  the  expression  we  are  visiting.   Upon  entering  the
    ;;recursion of this function: it is equal  to TOP-EXPR.STX.  It is seen as a tree
    ;;of wrapped syntax objects, pairs, vectors and datums.
    ;;
    ;;ACCUM-RIB* is the list of RIB* (ribs and "shift" symbols), from outer to inner,
    ;;collected so far  in the recursion while visiting "<stx>"  instances with empty
    ;;MARK*.
    ;;
    ;;ACCUM-AE* is the list of syntax objects representing the annotated expressions,
    ;;from outer to  inner, collected so far in the  recursion while visiting "<stx>"
    ;;instances with empty MARK*.  Upon entering the recursion:
    ;;
    ;;* If MARK is the anti-mark: AE* is a list holding a single #f.
    ;;
    ;;* If MARK is a  proper new mark: AE* is a list holding  the macro input form as
    ;;single item.
    ;;
    ;;This function visits the children of EXPR.STX  if EXPR.STX is: a pair, a vector
    ;;or an  "<stx>" with empty  MARK*.  Whenever an  instance of "<stx>"  with empty
    ;;MARK* is found: its RIB* and AE* are appended to ACCUM-RIB* and ACCUM-AE*.
    ;;
    ;;The recursion stops when EXPR.STX is:
    ;;
    ;;* An  instance of "<stx>"  with non-empty MARK*  having the anti-mark  as first
    ;;mark; this means EXPR.STX is the input form of a macro transformer call.
    ;;
    ;;*  An instance  of "<stx>"  having  a proper  mark  as first  mark; this  means
    ;;EXPR.STX is a syntax object created by  a macro transformer and inserted in its
    ;;output form.
    ;;
    ;;* A  non-compound datum (boolean,  number, string, ..., struct,  record, null).
    ;;This function raises an an exception if EXPR.STX is a raw symbol.
    ;;
    ;;When a "<stx>" with non-empty MARK* is found: perform the action; see below for
    ;;details.
    ;;
    (define-syntax-rule (%recurse ?expr ?accum-rib* ?accum-ae*)
      (%push-down-marks top-expr.stx mark rib ?expr ?accum-rib* ?accum-ae*))
    (cond ((stx? expr.stx)
	   (let ((expr.expr	(stx-expr		expr.stx))
		 (expr.mark*	(stx-mark*		expr.stx))
		 (expr.rib*	(stx-rib*		expr.stx))
		 (expr.ae*	(stx-annotated-expr*	expr.stx)))
	     (define result.ae*
	       (%merge-annotated-expr* accum-ae* expr.ae*))
	     (if (pair? expr.mark*)
		 ;;EXPR.STX had non-empty MARK*.
		 (if (anti-mark? (car expr.mark*))
		     ;;EXPR.STX has the anti-mark as  first mark; this means EXPR.STX
		     ;;is the input form of a non-core macro transformer call.
		     ;;
		     ;;Drop both MARK and the  anti-mark (they annihilate each other)
		     ;;from the resulting MARK*.   Join the collected ACCUM-RIB* with
		     ;;the  EXPR.RIB*;  the  first  item in  the  resulting  RIB*  is
		     ;;associated to  the anti-mark  (it is a  "shift" symbol)  so we
		     ;;drop it.
		     ;;
		     ;; (assert (or (and (not (null? accum-rib*))
		     ;;                  (eq? 'shift (car accum-rib*)))
		     ;;             (and (not (null? expr.rib*))
		     ;;                  (eq? 'shift (car expr.rib*)))))
		     (let ((result.mark* (cdr expr.mark*))
			   (result.rib*  (cdr (append accum-rib* expr.rib*))))
		       (make-stx-or-syntactic-identifier expr.expr result.mark* result.rib* result.ae*))
		   ;;EXPR.STX has a proper mark as first mark; this means EXPR.STX is
		   ;;a syntax object  created by a macro transformer  and inserted in
		   ;;its output form.
		   ;;
		   ;;Push MARK on the resulting MARK*.  Join the collected ACCUM-RIB*
		   ;;with  the  EXPR.RIB*; push  a  "shift"  on the  resulting  RIB*,
		   ;;associated to MARK in MARK*.
		   (let* ((result.mark* (cons mark expr.mark*))
			  (result.rib*  (cons 'shift (append accum-rib* expr.rib*)))
			  (result.rib*  (if rib
					    (cons rib result.rib*)
					  result.rib*)))
		     (make-stx-or-syntactic-identifier expr.expr result.mark* result.rib* result.ae*)))
	       ;;EXPR.STX has empty  MARK*: accumulate its RIB* and  AE* then recurse
	       ;;into its expression.
	       (%recurse expr.expr (append accum-rib* expr.rib*) result.ae*))))

	  ((pair? expr.stx)
	   ;;Visit the  items in the pair.   If the visited items  equal the original
	   ;;items: keep EXPR.STX as result.
	   (let ((A (%recurse (car expr.stx) accum-rib* accum-ae*))
		 (D (%recurse (cdr expr.stx) accum-rib* accum-ae*)))
	     (if (eq? A D)
		 expr.stx
	       (cons A D))))

	  ((vector? expr.stx)
	   ;;Visit  all the  items in  the vector.   If the  visited items  equal the
	   ;;original items: keep EXPR.STX as result.
	   (let* ((ls1 (vector->list expr.stx))
		  (ls2 (map (lambda (item)
			      (%recurse item accum-rib* accum-ae*))
			 ls1)))
	     (if (for-all eq? ls1 ls2)
		 expr.stx
	       (list->vector ls2))))


	  ((symbol? expr.stx)
	   ;;A raw symbol is invalid.
	   (syntax-violation 'add-mark "raw symbol encountered in output of macro" top-expr.stx expr.stx))

	  (else
	   ;;If we are here EXPR.STX is a non-compound datum (booleans, numbers, strings,
	   ;;..., structs, records).
	   #;(assert (non-compound-sexp? expr.stx))
	   expr.stx)))

  #| end of module: PSYNTAX-ADD-MARK |# )


;;;; wrapped syntax object type definition

(define-core-record-type <stx>
  (nongenerative vicare:expander:<stx>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (fields (immutable expr		stx-expr)
		;A symbolic expression, possibly  annotated, whose subexpressions can
		;also be instances of stx.
	  (immutable mark*		stx-mark*)
		;Null or a proper list of marks, including the symbol "src".
	  (immutable rib*		stx-rib*)
		;Null or  a proper list of  rib instances or "shift"  symbols.  Every
		;rib represents  a nested lexical  contour; a "shift"  represents the
		;return from a macro transformer application.
		;
		;NOTE The items in the fields  MARK* and RIB* are not associated: the
		;two lists  can grow independently  of each other.   But, considering
		;the whole  structure of nested  "<stx>" instances: the items  in all
		;the MARK*  fields are associated to  the items in all  the RIB*, see
		;the functions JOIN-WRAPS and ADD-MARK for details.
	  (immutable annotated-expr*	stx-annotated-expr*)
		;List of annotated expressions: null or a proper list whose items are
		;#f or input  forms of macro transformer calls.  It  is used to trace
		;the transformations a  form undergoes when it is  processed as macro
		;use.
		;
		;The #f items  are inserted when this instance is  processed as input
		;form of a macro call, but is later discarded.
		;
		;The list in this field is  *not* associated to either MARK* or RIB*:
		;it grows independently.
	  #| end of FIELDS |# )
  ;;While it is fine for the super-type  constructor (this one) to accept a symbol as
  ;;expression, it is not fine for the  public constructor MAKE-STX to do so.  If the
  ;;expression is a symbol we have to use MAKE-SYNTACTIC-IDENTIFIER.
  (protocol
    (lambda (make-record)
      (lambda* ({expr not-symbol?} mark* rib* annotated-expr*)
	(make-record expr mark* rib* annotated-expr*))))
  (super-protocol
    (lambda (make-record)
      (lambda* (expr mark* rib* annotated-expr*)
	(make-record expr mark* rib* annotated-expr*))))
  (custom-printer
    (lambda (S port sub-printer)
      (define-syntax-rule (%display ?thing)
	(display ?thing port))
      (define-syntax-rule (%write ?thing)
	(write ?thing port))
      (define raw-expr
	(syntax->datum S))
      (%display "#[syntax")
      (%display " expr=")	(%write raw-expr)
      ;;Seeing the mark is almost always useless.
      #;(%display " mark*=")	#;(%display (stx-mark* S))
      (let ((expr (stx-expr S)))
	(when (reader-annotation? expr)
	  (let ((pos (reader-annotation-textual-position expr)))
	    (when (source-position-condition? pos)
	      (%display " line=")	(%display (source-position-line    pos))
	      (%display " column=")	(%display (source-position-column  pos))
	      (%display " source=")	(%write   (source-position-port-id pos))))))
      (%display "]"))))


;;;; syntactic identifier type definition

(define-core-record-type <syntactic-identifier>
  (nongenerative vicare:expander:<syntactic-identifier>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <stx>)
  (protocol
    (lambda (make-stx)
      (define* (make-syntactic-identifier {sym symbol-or-annotated-symbol?} mark* rib* annotated-expr*)
	((make-stx sym mark* rib* annotated-expr*)))
      make-syntactic-identifier))
  (custom-printer
    (lambda (S port subwriter)
      (define-syntax-rule (%display ?thing)
	(display ?thing port))
      (define-syntax-rule (%write ?thing)
	(write ?thing port))
      (define raw-expr
	(syntax->datum S))
      (assert (symbol? raw-expr))
      (%display (cond ((id->label S)
		       => (lambda (label)
			    (let ((descr (label->syntactic-binding-descriptor label (current-inferior-lexenv))))
			      (case (syntactic-binding-descriptor.type descr)
				((core-object-type-name local-object-type-name global-object-type-name)
				 "#[type ")
				(else "#[id ")))))
		      (else "#[id ")))
      (%write raw-expr)
      (let ((expr (stx-expr S)))
	(when (reader-annotation? expr)
	  (let ((pos (reader-annotation-textual-position expr)))
	    (when (source-position-condition? pos)
	      (%display " line=")	(%display (source-position-line    pos))
	      (%display " column=")	(%display (source-position-column  pos))
	      (%display " source=")	(%write   (source-position-port-id pos))))))
      (%display "]"))))


;;;; unwrapped syntax object operations

(define (syntax-object? obj)
  ;;Return #t if  OBJ is a wrapped  or unwrapped syntax object;  otherwise return #f.
  ;;This  is not  a full  validation, because  a component  stx may  contain a  raw
  ;;symbol.
  ;;
  (cond ((stx? obj)
	 obj)
	((pair? obj)
	 (cons (syntax-object? ($car obj)) (syntax-object? ($cdr obj))))
	((symbol? obj)
	 #f)
	((vector? obj)
	 (vector-map syntax-object? obj))
	(else
	 (non-compound-sexp? obj))))

;;; --------------------------------------------------------------------

(define* (datum->syntax {id identifier?} datum)
  (~datum->syntax id datum))

(define (~datum->syntax id datum)
  ;;Since all the identifier->label bindings  are encapsulated within the identifier,
  ;;converting  a datum  to  a syntax  object (non-hygienically)  is  done simply  by
  ;;creating an "<stx>" that has the same marks and ribs as the identifier.
  ;;
  ;;We include also  the annotated expression from ID because,  when showing an error
  ;;trace, it helps to understand from where the returned object comes.
  ;;
  (make-stx-or-syntactic-identifier datum (stx-mark* id) (stx-rib* id) (stx-annotated-expr* id)))

(define (syntax->datum S)
  (syntax-object-strip-annotations S '()))

(define (make-top-level-syntactic-identifier-from-source-name-and-label sym lab)
  (wrap-source-expression sym (make-rib/top-from-source-name-and-label sym lab)))

(case-define* make-syntactic-identifier-for-temporary-variable
  ;;Build and return a  new src marked syntactic identifier to  be used for temporary
  ;;variables.  It is  responsibility of the caller to select  a name as SOURCE-NAME:
  ;;usually we should use GENSYM for this.  The returned identifier can be an item in
  ;;the list generated by GENERATE-TEMPORARIES.
  ;;
  (()
   (make-syntactic-identifier (gensym "tmp") SRC-MARK* '() '()))
  ((source-name)
   (make-syntactic-identifier (gensym source-name) SRC-MARK* '() '())))

(define* (make-syntactic-identifier-for-quoted-symbol {source-name symbol?})
  ;;Build and  return a  new src marked  syntactic identifier to  be used  for quoted
  ;;symbols.
  ;;
  (make-syntactic-identifier source-name SRC-MARK* '() '()))

(define* (make-top-level-syntax-object/quoted-quoting sym)
  ;;Return a src-marked syntax object representing one among:
  ;;
  ;;   (quote quasiquote)
  ;;   (quote unquote)
  ;;   (quote unquote-splicing)
  ;;
  (case sym
    ((quasiquote unquote unquote-splicing)
     (let ((Q (core-prim-id 'quote)))
       (list Q (make-syntactic-identifier sym SRC-MARK* (stx-rib* Q) '()))))
    (else
     (syntax-violation __who__
       "invalid quoting syntax name, expected one among: quasiquote, unquote, unquote-splicing"
       sym))))

(define* (wrap-source-expression expr {rib rib?})
  ;;Wrap EXPR in a  syntax object giving it the source mark and  the RIB.  Return the
  ;;stx object.
  ;;
  ;;EXPR   must  be   a  symbolic   expression,   possibly  annotated   as  read   by
  ;;GET-ANNOTATED-DATUM.
  ;;
  (make-stx-or-syntactic-identifier expr SRC-MARK* (list rib) '()))

;;; --------------------------------------------------------------------

(define (make-stx-or-syntactic-identifier sexp mark* rib* annotated-expr*)
  (if (symbol-or-annotated-symbol? sexp)
      (make-syntactic-identifier sexp mark* rib* annotated-expr*)
    (make-stx sexp mark* rib* annotated-expr*)))

(define* (mkstx expr-stx mark* {rib* list-of-ribs-and-shifts?} annotated-expr*)
  ;;This is the proper constructor for wrapped syntax objects.
  ;;
  ;;EXPR-STX can be a raw sexp, an  instance of STX or a (partially) unwrapped syntax
  ;;object.
  ;;
  ;;MARK* must be null or a proper list of marks, including the symbol "src".
  ;;
  ;;RIB* must be null or a proper list of rib instances and "shift" symbols.
  ;;
  ;;ANNOTATED-EXPR* must  be null or a  proper list of annotated  expressions: syntax
  ;;objects being input forms for macro transformer calls.
  ;;
  ;;When EXPR-STX is a raw sexp or  an unwrapped syntax object: just build and return
  ;;a new syntax object with the lexical context described by the given arguments.
  ;;
  ;;When EXPR-STX is a  stx instance: join the wraps from  EXPR-STX with given wraps,
  ;;making sure that marks and anti-marks and corresponding shifts cancel properly.
  ;;
  (if (stx? expr-stx)
      (if (src-marked? mark*)
	  ;;Here we just replace the wraps in EXPR-STX with the given ones.
	  (make-stx-or-syntactic-identifier (stx-expr expr-stx) mark* rib* annotated-expr*)
	;;Here we know EXPR-STX is an instance of "<stx>" and a new set of wraps need
	;;to be pushed on it.  So we join the wraps first.
	(receive (mark* rib* annotated-expr*)
	    (join-wraps mark* rib* annotated-expr* expr-stx)
	  (make-stx-or-syntactic-identifier (stx-expr expr-stx) mark* rib* annotated-expr*)))
    ;;Here we just wrap a source expression with the given ones.
    (make-stx-or-syntactic-identifier expr-stx mark* rib* annotated-expr*)))

(define* (push-lexical-contour {rib rib?} expr.stx)
  ;;Add  a rib  to a  syntax object  or expression  and return  the resulting  syntax
  ;;object.  The role of this function is this: during the expansion process we visit
  ;;the  nested subexpressions  in  a  syntax object  repesenting  source code;  this
  ;;procedure introduces  a lexical contour in  the context of EXPR.STX,  for example
  ;;when we enter a LET syntax.
  ;;
  ;;RIB must be an instance of RIB.
  ;;
  ;;EXPR.STX can be a raw sexp, an instance of STX or a wrapped syntax object.
  ;;
  ;;This function prepares a computation that will be lazily performed later; the RIB
  ;;will  be pushed  down on  the stack  of  ribs in  every identifier  in the  fully
  ;;unwrapped version of the returned syntax object.
  ;;
  (let ((mark*	'())
	(ae*	'()))
    (mkstx expr.stx mark* (list rib) ae*)))

(define* (stx-push-annotated-expr stx annotated-expr)
  ;;Build  and return  a new  syntax object  with the  same wraps  of STX  and having
  ;;ANNOTATED-EXPR pushed on  top of the annotated expressions list.   This is useful
  ;;when expanding the  input form of a macro:  the input form must be  pushed on the
  ;;stack of annotated expressions.
  ;;
  (if (stx? stx)
      (make-stx-or-syntactic-identifier (stx-expr stx) (stx-mark* stx) (stx-rib* stx)
					(%merge-annotated-expr* (list annotated-expr)
								(stx-annotated-expr* stx)))
    (let ((mark* '())
	  (rib*  '()))
      (mkstx stx mark* rib* (list annotated-expr)))))

(define (syntax-annotation x)
  (if (stx? x)
      (stx-expr x)
    x))

;;; --------------------------------------------------------------------

(module (syntax-object-strip-annotations)

  (define (syntax-object-strip-annotations expr mark*)
    ;;Remove  the wrap  of a  syntax object.   EXPR  and MARK*  are meant  to be  the
    ;;expression and associated marks of a  wrapped or unwrapped syntax object.  This
    ;;function is also the implementation of SYNTAX->DATUM.
    ;;
    ;;NOTE This function assumes that: if  MARK* contains the symbol "src", then EXPR
    ;;is a raw  symbolic expression or an annotated symbolic  expression; that is: it
    ;;is not a syntax object.
    ;;
    (if (src-marked? mark*)
	(if (or (reader-annotation? expr)
		(and (pair? expr)
		     (reader-annotation? ($car expr)))
		(and (vector? expr)
		     (not ($vector-empty? expr))
		     (reader-annotation? ($vector-ref expr 0))))
	    ;;TODO Ask Kent why this is a sufficient test.  (Abdulaziz Ghuloum)
	    (%strip-annotations expr)
	  expr)
      (let f ((x expr))
	(cond ((stx? x)
	       (syntax-object-strip-annotations (stx-expr x) (stx-mark* x)))
	      ((reader-annotation? x)
	       (reader-annotation-stripped x))
	      ((pair? x)
	       (let ((a (f ($car x)))
		     (d (f ($cdr x))))
		 (if (and (eq? a ($car x))
			  (eq? d ($cdr x)))
		     x
		   (cons a d))))
	      ((vector? x)
	       (let* ((old (vector->list x))
		      (new (map f old)))
		 (if (for-all eq? old new)
		     x
		   (list->vector new))))
	      (else x)))))

  (define (%strip-annotations x)
    (cond ((pair? x)
	   (cons (%strip-annotations ($car x))
		 (%strip-annotations ($cdr x))))
	  ((vector? x)
	   (vector-map %strip-annotations x))
	  ((reader-annotation? x)
	   (reader-annotation-stripped x))
	  (else x)))

  #| end of module: SYNTAX-OBJECT-STRIP-ANNOTATIONS |# )


;;;; syntax objects: mapping identifiers to labels

(module (id->label id->label/local)

  (define* (id->label/local {id identifier?})
    ;;This is like ID->LABEL, but it searches only the first rib in ID.
    ;;
    (let ((id.source-name	(identifier->symbol id))
	  (rib			(car (stx-rib* id)))
	  (mark*		(stx-mark* id))
	  (fail-kont		(lambda () #f)))
      (if (rib-sealed/freq rib)
	  (%search-in-rib/sealed rib id.source-name mark* fail-kont)
	(%search-in-rib/non-sealed rib id.source-name mark* fail-kont))))

  (define* (id->label {id identifier?})
    ;;Given  the syntactic  identifier ID  search its  ribs for  a syntactic  binding
    ;;having the  same source-name  and marks.  If  successful: return  the syntactic
    ;;binding's label gensym; otherwise return false.
    ;;
    (define id.source-name (identifier->symbol id))
    #;(debug-print __who__ id.source-name)
    (let search ((rib*  (stx-rib* id))
		 (mark* (stx-mark* id)))
      (and (pair? rib*)
	   (if (eq? (car rib*) 'shift)
	       ;;This is the only  place in the expander where a  symbol "shift" in a
	       ;;RIB* makes some  difference; a "shift" is pushed on  the RIB* when a
	       ;;mark is pushed on the MARK*.
	       ;;
	       ;;When we  find a "shift" in  RIB*: we skip the  corresponding mark in
	       ;;MARK*.
	       (search (cdr rib*) (cdr mark*))
	     (let ((rib (car rib*)))
	       (define (search-in-next-rib)
		 (search (cdr rib*) mark*))
	       (if (rib-sealed/freq rib)
		   (%search-in-rib/sealed rib id.source-name mark* search-in-next-rib)
		 (%search-in-rib/non-sealed rib id.source-name mark* search-in-next-rib)))))))

  (define-syntax-rule (same-name? x y)
    (eq? x y))

  (define (%search-in-rib/non-sealed rib id.source-name id.mark* search-in-next-rib)
    (let loop ((rib.source-name* (rib-name*  rib))
	       (rib.mark**       (rib-mark** rib))
	       (rib.label*       (rib-label* rib)))
      (let-syntax-rules
	  (((more-tuples?)		(pair? rib.source-name*))
	   ((loop-to-next-rib-tuple)	(loop (cdr rib.source-name*) (cdr rib.mark**) (cdr rib.label*)))
	   ((next-rib-source-name)	(car rib.source-name*))
	   ((next-rib-mark*)		(car rib.mark**))
	   ((next-rib-label)		(car rib.label*)))
	(if (more-tuples?)
	    (if (and (same-name?  id.source-name (next-rib-source-name))
		     (same-marks? id.mark*       (next-rib-mark*)))
		(next-rib-label)
	      (loop-to-next-rib-tuple))
	  (search-in-next-rib)))))

  (define (%search-in-rib/sealed rib id.source-name id.mark* search-in-next-rib)
    (define rib.source-name* (rib-name*  rib))
    (define rib.mark**       (rib-mark** rib))
    (define rib.label*       (rib-label* rib))
    (let loop ((i    0)
	       (imax ($vector-length rib.source-name*)))
      (let-syntax-rules
	  (((more-tuples?)		(fx<? i imax))
	   ((loop-to-next-rib-tuple)	(loop (fxadd1 i) imax))
	   ((next-rib-source-name)	($vector-ref rib.source-name* i))
	   ((next-rib-mark*)		($vector-ref rib.mark**       i))
	   ((next-rib-label)		(receive-and-return (label)
					    ($vector-ref rib.label* i)
					  (%increment-rib-frequency! rib i))))
	(if (more-tuples?)
	    (if (and (same-name?  id.source-name (next-rib-source-name))
		     (same-marks? id.mark*       (next-rib-mark*)))
		(next-rib-label)
	      (loop-to-next-rib-tuple))
	  (search-in-next-rib)))))

  (define (%increment-rib-frequency! rib src.idx)
    ;;The argument RIB is a sealed rib  object.  The argument SRC.IDX is the index of
    ;;the tuple in RIB that was last accessed.
    ;;
    ;;If the scenario before the access is:
    ;;
    ;;   name*       = #(b a)
    ;;   sealed/freq = #(0 0)
    ;;
    ;;and we access A, we want the scenario after the access to be:
    ;;
    ;;   name*       = #(a b)
    ;;   sealed/freq = #(1 0)
    ;;
    ;;so the tuple of A is moved to the left and its access frequency is incremented.
    ;;
    ;;If the scenario before the access is:
    ;;
    ;;   name*       = #(f e d c b a)
    ;;   sealed/freq = #(1 1 1 0 0 0)
    ;;
    ;;and we  access A:  first we want  the tuple of  A to  be moved in  the leftmost
    ;;position in the group of tuples with FREQ equal to 0:
    ;;
    ;;   name*       = #(f e d a b c)
    ;;   sealed/freq = #(1 1 1 0 0 0)
    ;;
    ;;then we increment its access frequency; the scenario after the access must be:
    ;;
    ;;   name*       = #(f e d a b c)
    ;;   sealed/freq = #(1 1 1 1 0 0)
    ;;
    ;;notice that:  while we  need to  swap the  tuple values  in the  vectors NAME*,
    ;;MARK** and LABEL*, we  just need to increment the freq  in the destination slot
    ;;of A: there is no swapping needed in the freq vector.
    ;;
    (unless (fxzero? src.idx)
      (let* ((rib.freq* (rib-sealed/freq rib))
	     (freq      ($vector-ref rib.freq* src.idx))
	     ;;Search for the leftmost slot, starting from SRC.IDX, that has the same
	     ;;freq of the slot at SRC.IDX.
	     (dst.idx   (let loop ((i src.idx))
			  (if (fxzero? i)
			      0
			    (let ((j (fxsub1 i)))
			      (if (fx= freq ($vector-ref rib.freq* j))
				  ;;The  freq of  the slot  previous to  I is  equal:
				  ;;loop.
				  (loop j)
				;;The  freq of  the slot  previous to  I is  greater:
				;;accept I as swap position.
				i))))))
	;;Rather than swapping the slots DST.IDX and SRC.IDX in the frequency vector:
	;;we just increment the dst slot.
	($vector-set! rib.freq* dst.idx (fxadd1 freq))
	(unless (fx= dst.idx src.idx)
	  (let ((rib.name*  (rib-name*  rib))
		(rib.mark** (rib-mark** rib))
		(rib.label* (rib-label* rib)))
	    (let-syntax ((%vector-swap (syntax-rules ()
					 ((_ ?vec ?idx1 ?idx2)
					  (let ((V ($vector-ref ?vec ?idx1)))
					    ($vector-set! ?vec ?idx1 ($vector-ref ?vec ?idx2))
					    ($vector-set! ?vec ?idx2 V))))))
	      (%vector-swap rib.name*  src.idx dst.idx)
	      (%vector-swap rib.mark** src.idx dst.idx)
	      (%vector-swap rib.label* src.idx dst.idx)))))))

  #| end of module: ID->LABEL |# )


;;;; syntax objects: mapping identifiers to values

(case-define* id->object-type-spec
  ((id)
   (id->object-type-spec id (current-inferior-lexenv)))
  (({id identifier?} lexenv)
   ;;ID is meant to be a syntactic identifier representing an object-type name, whose
   ;;syntactic  binding's descriptor  contains an  instance of  "<object-type-spec>";
   ;;retrieve its label, then its descriptor from LEXENV, finally return the instance
   ;;of "<object-type-spec>".
   ;;
   ;;If  ID is  bound to  an  imported syntactic  binding: the  exporting library  is
   ;;visited.
   ;;
   ;;If an error occurs while resolving ID, raise and exception of one of the types:
   ;;
   ;;   &syntactic-identifier-unbound
   ;;   &syntactic-identifier-out-of-context
   ;;   &syntactic-identifier-not-type-identifier
   ;;
   ;;all of which are sub-types of "&syntactic-identifier-resolution".
   ;;
   (import PSYNTAX-SYNTACTIC-BINDINGS)
   (case-identifier-syntactic-binding-descriptor (__who__ id lexenv)
     ((local-object-type-name)
      (syntactic-binding-descriptor/local-object-type.object-type-spec  __descr__))
     ((global-object-type-name)
      (syntactic-binding-descriptor/global-object-type.object-type-spec __descr__))
     ((core-object-type-name)
      (syntactic-binding-descriptor/core-object-type.object-type-spec   __descr__))
     (else
      (raise
       (condition (make-syntactic-identifier-not-type-identifier-condition)
		  (make-who-condition __who__)
		  (make-message-condition "identifier not bound to an object-type specification")
		  (make-syntactic-identifier-condition id)
		  (make-syntactic-binding-descriptor-condition __descr__)))))))

(case-define* id->record-type-spec
  ((id)
   (id->record-type-spec id (current-inferior-lexenv)))
  (({id identifier?} lexenv)
   ;;ID is meant to be a  syntactic identifier representing a record-type name, whose
   ;;syntactic  binding's descriptor  contains an  instance of  "<record-type-spec>";
   ;;retrieve its label, then its binding  descriptor from LEXENV, finally return the
   ;;instance of "<record-type-spec>".
   ;;
   ;;If  ID is  bound to  an  imported syntactic  binding: the  exporting library  is
   ;;visited.
   ;;
   ;;If an error occurs while resolving ID, raise and exception of one of the types:
   ;;
   ;;   &syntactic-identifier-unbound
   ;;   &syntactic-identifier-out-of-context
   ;;   &syntactic-identifier-not-type-identifier
   ;;
   ;;all of which are sub-types of "&syntactic-identifier-resolution".
   ;;
   (import PSYNTAX-SYNTACTIC-BINDINGS)
   (receive-and-return (ots)
       (id->object-type-spec id lexenv)
     (unless (record-type-spec? ots)
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "type identifier not bound to a record-type specification")
		   (make-syntactic-identifier-condition id)
		   (make-object-type-spec-condition ots)))))))

(case-define* id->struct-type-spec
  ((id)
   (id->struct-type-spec id (current-inferior-lexenv)))
  ((id lexenv)
   ;;ID is meant to be a  syntactic identifier representing a struct-type name, whose
   ;;syntactic  binding's descriptor  contains an  instance of  "<struct-type-spec>";
   ;;retrieve its label, then its binding  descriptor from LEXENV, finally return the
   ;;instance of "<struct-type-name>".
   ;;
   ;;If  ID is  bound to  an  imported syntactic  binding: the  exporting library  is
   ;;visited.
   ;;
   ;;If an error occurs while resolving ID, raise and exception of one of the types:
   ;;
   ;;   &syntactic-identifier-unbound
   ;;   &syntactic-identifier-out-of-context
   ;;   &syntactic-identifier-not-type-identifier
   ;;
   ;;all of which are sub-types of "&syntactic-identifier-resolution".
   ;;
   (import PSYNTAX-SYNTACTIC-BINDINGS)
   (receive-and-return (ots)
       (id->object-type-spec id lexenv)
     (unless (struct-type-spec? ots)
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "type identifier not bound to a struct-type specification")
		   (make-syntactic-identifier-condition id)
		   (make-object-type-spec-condition ots)))))))


;;;; identifier to syntactic binding's descriptor

(define-fluid-syntax __descr__
  (lambda (stx)
    (sys::syntax-violation '__descr__ "unset fluid syntax" stx)))

(module (case-identifier-syntactic-binding-descriptor case-identifier-syntactic-binding-descriptor/no-indirection)

  (define-syntax case-identifier-syntactic-binding-descriptor
    (syntax-rules ()
      ((_ (?who ?id ?lexenv)
	  ((?type0 ?type ...) . ?body)
	  ...
	  (else . ?else-body))
       (%case-identifier-syntactic-binding-descriptor
	label->syntactic-binding-descriptor
	(?who ?id ?lexenv)
	((?type0 ?type ...) . ?body)
	...
	(else . ?else-body)))
      ))

  (define-syntax case-identifier-syntactic-binding-descriptor/no-indirection
    (syntax-rules ()
      ((_ (?who ?id ?lexenv)
	  ((?type0 ?type ...) . ?body)
	  ...
	  (else . ?else-body))
       (%case-identifier-syntactic-binding-descriptor
	label->syntactic-binding-descriptor/no-indirection
	(?who ?id ?lexenv)
	((?type0 ?type ...) . ?body)
	...
	(else . ?else-body)))
      ))

  (define-syntax %case-identifier-syntactic-binding-descriptor
    (lambda (stx)
      (define (%id-or-false X)
	(or (sys::identifier? X)
	    (not (sys::syntax->datum X))))
      (sys::syntax-case stx (else)
	((_ ?label->descr (?who ?id ?lexenv)
	    ((?type0 ?type ...) . ?body)
	    ...
	    (else . ?else-body))
	 (and (sys::identifier? (sys::syntax ?lexenv))
	      (sys::identifier? (sys::syntax ?id)))
	 (sys::syntax
	  (cond ((id->label ?id)
		 => (lambda (label)
		      (let ((descr (?label->descr label ?lexenv)))
			(fluid-let-syntax ((__descr__ (identifier-syntax descr)))
			  (case (syntactic-binding-descriptor.type descr)
			    ((?type0 ?type ...) . ?body)
			    ...
			    ((displaced-lexical)
			     (error-identifier-out-of-context ?who ?id))
			    (else . ?else-body))))))
		(else
		 (error-unbound-identifier ?who ?id)))))
	)))

  #| end of module |# )


;;;; system label gensym

(define-constant SYSTEM-LABEL-GENSYM
  ;;Notice  that  this  gensym is  generated  a-new  every  time  the boot  image  is
  ;;initialised.  We  must avoid the source  optimizer to precompute and  hard-code a
  ;;value.
  ;;
  ;;This syntactic binding is  not part of the public API: it is  not exported by the
  ;;library  "(vicare)".  However,  it  is  used by  the  init  library generated  by
  ;;"makefile.sps" when building a new boot image.
  (expand-time-gensym "system-label-gensym"))

(define (system-label-gensym)
  SYSTEM-LABEL-GENSYM)

(define* (system-label {x symbol?})
  ;;If X is  the symbol name of  a primitive procedure: return  its syntactic binding
  ;;label gensym, otherwise return false.
  ;;
  (getprop x SYSTEM-LABEL-GENSYM))


;;;; system id gensym

(module (system-id-gensym system-id)

  (define-constant SYSTEM-ID-GENSYM
    ;;Notice  that this  gensym  is generated  a-new  every time  the  boot image  is
    ;;initialised.  We must avoid the source  optimizer to precompute and hard-code a
    ;;value.
    (expand-time-gensym "system-id-gensym"))

  (define (system-id-gensym)
    SYSTEM-ID-GENSYM)

  (case-define* system-id
    (({x symbol?})
     ;;If X is  the symbol name of  a core primitive's syntactic  binding: return its
     ;;syntactic binding identifier, otherwise return false.
     ;;
     (getprop x SYSTEM-ID-GENSYM))
    (({x symbol?} property)
     (putprop x SYSTEM-ID-GENSYM property)))

  #| end of module |# )


;;;; system ots gensym

(module (system-ots-gensym system-ots)

  (define-constant SYSTEM-OTS-GENSYM
    ;;Notice  that this  gensym  is generated  a-new  every time  the  boot image  is
    ;;initialised.  We must avoid the source  optimizer to precompute and hard-code a
    ;;value.
    (expand-time-gensym "system-ots-gensym"))

  (define (system-ots-gensym)
    SYSTEM-OTS-GENSYM)

  (case-define* system-ots
    (({x symbol?})
     ;;If X is  the symbol name of  a core primitive's syntactic  binding: return its
     ;;"<object-types-spec>" instance, otherwise return false.
     ;;
     (getprop x SYSTEM-OTS-GENSYM))
    (({x symbol?} property)
     (putprop x SYSTEM-OTS-GENSYM property)))

  #| end of module |# )


;;;; identifiers from the built-in environment

(define (bless input-form.stx)
  ;;Given a raw sexp,  a single syntax object, a wrapped  syntax object, an unwrapped
  ;;syntax  object or  a partly  unwrapped syntax  object X:  return a  syntax object
  ;;representing the input, possibly X itself.
  ;;
  ;;When  INPUT-FORM.STX is  a sexp  or a  (partially) unwrapped  syntax object:  raw
  ;;symbols in INPUT-FORM.STX are converted to:
  ;;
  ;;* Bound identifiers  that will be captured  by a core primitive  binding from the
  ;;  top-level image.
  ;;
  ;;* Free identifiers that will not be captured by any binding.  These can be safely
  ;;  used for local bindings.
  ;;
  (cond ((stx? input-form.stx)
	 input-form.stx)
	((pair? input-form.stx)
	 (cons (bless ($car input-form.stx))
	       (bless ($cdr input-form.stx))))
	((symbol? input-form.stx)
	 (scheme-stx input-form.stx))
	((vector? input-form.stx)
	 (vector-map bless input-form.stx))
	(else
	 ;;If we  are here INPUT-FORM.STX  is a non-compound datum  (boolean, number,
	 ;;string, ..., struct, record, null).
	 #;(assert (non-compound-sexp? input-form.stx))
	 input-form.stx)))

(define (trace-bless input-form.stx)
  (receive-and-return (output-form.stx)
      (bless input-form.stx)
    (debug-print 'bless-input  (syntax->datum input-form.stx)
		 'bless-output (syntax->datum output-form.stx))))

(define (debug-bless input-form.stx)
  (receive-and-return (output-form.stx)
      (bless input-form.stx)
    (debug-print 'bless-output (syntax->datum output-form.stx))))

;;; --------------------------------------------------------------------

(define* (scheme-stx {sym symbol?})
  ;;Take a symbol and if it's the public  name of a syntactic binding exported by the
  ;;library "(psyntax system  $all)": create a fresh identifier that  maps the symbol
  ;;to its label in that library.  Symbols not in that library become fresh.
  ;;
  (or (system-id sym)
      (cond ((system-label sym)
	     ;;SYM is the  name of a core  primitive, so we build  a bound identifier
	     ;;with a proper "rib" and  the binding's label.  Such bound identifier
	     ;;will be captured by the entry in the top-level environment.
	     => (lambda (label)
		  (receive-and-return (id)
		      (make-top-level-syntactic-identifier-from-source-name-and-label sym label)
		    (system-id sym id))))
	    (else
	     ;;SYM is  not the  name of  a core primitive,  so we  just build  a free
	     ;;identifier.   Such free  identifier  will work  just  fine in  binding
	     ;;position.
	     (make-syntactic-identifier sym SRC-MARK* '() '())))))

;;; --------------------------------------------------------------------

(define* (core-prim-id {sym symbol?})
  ;;Take a symbol  and if it's in the library:
  ;;
  ;;   (psyntax system $all)
  ;;
  ;;create a fresh identifier that maps only the symbol to its label in that library.
  ;;This function is similar to SCHEME-STX,  but it does not create fresh identifiers
  ;;for non-core-primitive symbols.
  ;;
  (or (system-id sym)
      (cond ((system-label sym)
	     ;;SYM is the  name of a core  primitive, so we build  a bound identifier
	     ;;with a  proper "rib" and  the binding's label.  Such  bound identifier
	     ;;will be captured by the entry in the top-level environment.
	     => (lambda (label)
		  (receive-and-return (id)
		      (make-top-level-syntactic-identifier-from-source-name-and-label sym label)
		    (system-id sym id))))
	    (else
	     (assertion-violation __who__ "invalid core primitive symbol name" sym)))))

(case-define* core-prim-spec
  (({sym symbol?})
   (core-prim-spec sym (current-inferior-lexenv)))
  (({sym symbol?} lexenv)
   (or (system-ots sym)
       (receive-and-return (ots)
	   (type-annotation->object-type-spec (core-prim-id sym) lexenv)
	 (system-ots sym ots)))))

(define* (make-syntactic-identifier-for-fake-core-primitive {source-name symbol?})
  (make-syntactic-identifier source-name '() '() '()))

(let-syntax
    ((define-core-prim-id-retriever (syntax-rules ()
				      ((_ ?who ?core-prim)
				       (define ?who
					 (let ((memoized-id #f))
					   (lambda ()
					     (or memoized-id
						 (receive-and-return (id)
						     (core-prim-id '?core-prim)
						   (set! memoized-id id))))))))))
  (define-core-prim-id-retriever underscore-id		_)
  (define-core-prim-id-retriever ellipsis-id		...)
  (define-core-prim-id-retriever place-holder-id	<>)
  (define-core-prim-id-retriever void-id		void)
  (define-core-prim-id-retriever procedure-pred-id	procedure?)
  (define-core-prim-id-retriever fields-id		fields)
  (define-core-prim-id-retriever public-id		public)
  (define-core-prim-id-retriever protected-id		protected)
  (define-core-prim-id-retriever private-id		private)
  (define-core-prim-id-retriever method-id		method)
  (define-core-prim-id-retriever virtual-method-id	virtual-method)
  (define-core-prim-id-retriever seal-method-id		seal-method)
  (define-core-prim-id-retriever this-id		this)
  (define-core-prim-id-retriever brace-id		brace)
  (define-core-prim-id-retriever list-of-id		list-of)
  (define-core-prim-id-retriever vector-of-id		vector-of)
  (define-core-prim-id-retriever hashtable-id		hashtable)
  (define-core-prim-id-retriever alist-id		alist)
  (define-core-prim-id-retriever enumeration-id		enumeration)
  (define-core-prim-id-retriever void-object?-id	void-object?)
  ;;
  (define-core-prim-id-retriever define/checked-id	define/checked)
  (define-core-prim-id-retriever define/typed-id	define/typed)
  (define-core-prim-id-retriever define/std-id		define/std)
  (define-core-prim-id-retriever case-define/checked-id	case-define/checked)
  (define-core-prim-id-retriever case-define/typed-id	case-define/typed)
  (define-core-prim-id-retriever case-define/std-id	case-define/std)
  (define-core-prim-id-retriever begin-id		begin)
  #| end of let-syntax |# )

(define (underscore-id? id)
  (and (identifier? id)
       (~free-identifier=? id (underscore-id))))

(define (ellipsis-id? id)
  (and (identifier? id)
       (~free-identifier=? id (ellipsis-id))))

(define (place-holder-id? id)
  (and (identifier? id)
       (~free-identifier=? id (place-holder-id))))

(define (method-id? id)
  (and (identifier? id)
       (~free-identifier=? id (method-id))))

(define (fields-id? id)
  (and (identifier? id)
       (~free-identifier=? id (fields-id))))

(define (virtual-method-id? id)
  (and (identifier? id)
       (~free-identifier=? id (virtual-method-id))))

(define (seal-method-id? id)
  (and (identifier? id)
       (~free-identifier=? id (seal-method-id))))

(define (this-id? id)
  (and (identifier? id)
       (~free-identifier=? id (this-id))))

(define (brace-id? id)
  (and (identifier? id)
       (~free-identifier=? id (brace-id))))

(define (void-object?-id? id)
  (and (identifier? id)
       (~free-identifier=? id (void-object?-id))))

(define (protection-level-id? id)
  (syntax-match id (public protected private)
    (public	#t)
    (protected	#t)
    (private	#t)
    (_		#f)))


;;;; public interface: identifiers handling

(define (identifier? x)
  ;;Return true if X is an  identifier: a syntax object whose expression
  ;;is a symbol.
  ;;
  (and (stx? x)
       (symbol-or-annotated-symbol? (stx-expr x))))

(define (false-or-identifier? x)
  (or (not x)
      (identifier? x)))

;;; --------------------------------------------------------------------

(define* (bound-identifier=? {x identifier?} {y identifier?})
  (~bound-identifier=? x y))

(define (~bound-identifier=? id1 id2)
  ;;Two identifiers  are ~BOUND-IDENTIFIER=? if they  have the same name  and the
  ;;same set of marks.
  ;;
  (and (eq? ($identifier->symbol id1) ($identifier->symbol id2))
       (same-marks? (stx-mark* id1) (stx-mark* id2))))

(define* (free-identifier=? {x identifier?} {y identifier?})
  (~free-identifier=? x y))

(define (~free-identifier=? id1 id2)
  ;;Two identifiers are ~FREE-IDENTIFIER=? if either both are bound to the same label
  ;;or if both are unbound and they have the same name.
  ;;
  (let ((t1 (id->label id1))
	(t2 (id->label id2)))
    (if (or t1 t2)
	(eq? t1 t2)
      (eq? ($identifier->symbol id1)
	   ($identifier->symbol id2)))))

;;; --------------------------------------------------------------------

(define* (identifier-bound? {id identifier?})
  (~identifier-bound? id))

(define (~identifier-bound? id)
  (and (id->label id) #t))

(define (false-or-identifier-bound? id)
  (or (not id)
      (and (identifier? id)
	   (~identifier-bound? id))))

;;; --------------------------------------------------------------------

(define* (identifier->symbol {x identifier?})
  ;;Given an identifier return its symbol expression.
  ;;
  ($identifier->symbol x))

(define ($identifier->symbol x)
  (let ((expr (stx-expr x)))
    (if (reader-annotation? expr)
	(reader-annotation-stripped expr)
      expr)))

(define* (identifier->string {id identifier?})
  (symbol->string (syntax->datum id)))

(define* (string->identifier {ctx identifier?} {str string?})
  (datum->syntax ctx (string->symbol str)))


;;;; current inferior lexenv
;;
;;When calling a macro transformer procedure: we set the paramerer CURRENT-RUN-LEXENV
;;to a function  returning the current inferior LEXENV.  This  allows us to implement
;;syntax parameters and compile-time value retrieving.
;;

(define current-run-lexenv
  ;;This parameter holds a function which is  meant to return the value of LEXENV.RUN
  ;;while a macro is being expanded.
  ;;
  ;;The default value  will return null, which represents an  empty LEXENV; when such
  ;;value is used with LABEL->SYNTACTIC-BINDING-DESCRIPTOR: the mapping label/binding
  ;;is performed only in the top-level environment.
  ;;
  ;;Another possibility we could think of is to use as default value the function:
  ;;
  ;;   (lambda ()
  ;;     (syntax-violation 'current-run-lexenv
  ;; 	   "called outside the extent of a macro expansion"
  ;; 	   '(current-run-lexenv)))
  ;;
  ;;However there are  cases where we actually  want the returned LEXENV  to be null,
  ;;for example:  when evaluating  the visit code  of a library  just loaded  in FASL
  ;;form; such  visit code might need,  for example, to access  the syntactic binding
  ;;property lists, and it would do it outside any macro expansion.
  ;;
  (make-parameter
      (lambda () '())
    (lambda* ({obj procedure?})
      obj)))

(define (current-inferior-lexenv)
  ((current-run-lexenv)))


;;;; identifiers: syntax parameters

(define* (syntax-parameter-value {id identifier?})
  (import PSYNTAX-SYNTACTIC-BINDINGS)
  (define lexenv
    (current-inferior-lexenv))
  (case-identifier-syntactic-binding-descriptor (__who__ id lexenv)
    ((local-etv)
     (syntactic-binding-descriptor/local-expand-time-value.object __descr__))

    ((global-etv)
     (syntactic-binding-descriptor/global-expand-time-value.object __descr__))

    (else
     (procedure-argument-violation __who__
       "expected identifier bound to compile-time value" id))))


;;;; expand-time values

(define* (retrieve-expand-time-value {id identifier?})
  ;;This is the compile-time values  retriever function.  Given an identifier: search
  ;;an  entry in  the lexical  environment; when  found return  its value,  otherwise
  ;;return false.
  ;;
  (import PSYNTAX-SYNTACTIC-BINDINGS)
  (cond ((id->label id)
	 => (lambda (label)
	      (let ((descr (label->syntactic-binding-descriptor label (current-inferior-lexenv))))
		(case (syntactic-binding-descriptor.type descr)
		  ((displaced-lexical)
		   (assertion-violation __who__
		     "identifier out of context (identifier's label not in LEXENV)" id))
		  ;;The given identifier is bound to a local compile-time value.  The
		  ;;actual object is stored in the descriptor itself.
		  ((local-etv)
		   (syntactic-binding-descriptor/local-expand-time-value.object descr))
		  ;;The given  identifier is bound  to a compile-time  value imported
		  ;;from a library  or the top-level environment.   The actual object
		  ;;is stored in the "value" field of a loc gensym.
		  ((global-etv)
		   (syntactic-binding-descriptor/global-expand-time-value.object descr))
		  (else
		   ;; (assertion-violation __who__
		   ;;   "identifier not bound to an object-type specification"
		   ;;   id descr)
		   #f)))))
	(else
	 (error-unbound-identifier __who__ id))))


;;;; utilities for identifiers

(define (valid-bound-ids? id*)
  ;;Given a list return #t if it  is made of identifers none of which is
  ;;~BOUND-IDENTIFIER=? to another; else return #f.
  ;;
  ;;This function is called to validate  both list of LAMBDA formals and
  ;;list of LET binding identifiers.  The only guarantee about the input
  ;;is that it is a list.
  ;;
  (and (for-all identifier? id*)
       (distinct-bound-ids? id*)))

(define (distinct-bound-ids? id*)
  ;;Given a list of identifiers: return #t if none of the identifiers is
  ;;~BOUND-IDENTIFIER=? to another; else return #f.
  ;;
  (or (null? id*)
      (and (not (bound-id-member? ($car id*) ($cdr id*)))
	   (distinct-bound-ids? ($cdr id*)))))

(define (duplicate-bound-formals? standard-formals-stx)
  ;;Given  a  syntax  object  representing a  list  of  UNtagged  LAMBDA
  ;;formals:  return #f  if none  of the  identifiers is  ~BOUND-IDENTIFIER=?  to
  ;;another; else return a duplicate identifier.
  ;;
  (let recur ((fmls          standard-formals-stx)
	      (collected-id* '()))
    (syntax-case fmls ()
      ;;STANDARD-FORMALS-STX is a proper list and it ends here.  Good.
      (() #f)
      ;;STANDARD-FORMALS-STX is an IMproper list and it ends here with a
      ;;rest argument.  Check it.
      (?rest
       (identifier? #'?rest)
       (if (bound-id-member? #'?rest collected-id*)
	   #'?rest
	 #f))
      ((?id . ?rest)
       (identifier? #'?id)
       (if (bound-id-member? #'?id collected-id*)
	   #'?id
	 (recur #'?rest (cons #'?id collected-id*))))
      (_
       (syntax-violation #f "invalid formals" standard-formals-stx)))))

(define (free-id-member? id id*)
  ;;Given  an identifier  ID  and a  list  of identifiers  ID*: return  #t  if ID  is
  ;;~BOUND-IDENTIFIER=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (~free-identifier=? id (car id*))
	   (free-id-member?    id (cdr id*)))))

(define (bound-id-member? id id*)
  ;;Given  an identifier  ID  and a  list  of identifiers  ID*: return  #t  if ID  is
  ;;~BOUND-IDENTIFIER=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (~bound-identifier=? id (car id*))
	   (bound-id-member?    id (cdr id*)))))

(define* (identifier-append {ctxt identifier?} . str*)
  ;;Given  the identifier  CTXT  and a  list of  strings  or symbols  or
  ;;identifiers STR*: concatenate all the items in STR*, with the result
  ;;build and return a new identifier in the same context of CTXT.
  ;;
  (~datum->syntax ctxt
		  (string->symbol
		   (apply string-append
			  (map (lambda (x)
				 (cond ((symbol? x)
					(symbol->string x))
				       ((string? x)
					x)
				       ((identifier? x)
					(symbol->string (syntax->datum x)))
				       (else
					(assertion-violation __who__
					  "expected string, symbol or identifier as item argument" x))))
			    str*)))))


;;;; basic object-type identifiers

(let-syntax-rules
    (((define-type-id-retriever ?who ?tag)
      (define ?who
	(let ((memoized-id #f))
	  (lambda ()
	    (or memoized-id
		(receive-and-return (id)
		    (core-prim-id '?tag)
		  (set! memoized-id id))))))))
  (define-type-id-retriever <void>-type-id			<void>)
  (define-type-id-retriever <untyped>-type-id			<untyped>)
  (define-type-id-retriever <bottom>-type-id			<bottom>)
  (define-type-id-retriever <top>-type-id			<top>)
  (define-type-id-retriever <procedure>-type-id			<procedure>)
  (define-type-id-retriever <boolean>-type-id			<boolean>)
  (define-type-id-retriever <true>-type-id			<true>)
  (define-type-id-retriever <false>-type-id			<false>)
  (define-type-id-retriever <symbol>-type-id			<symbol>)
  (define-type-id-retriever <struct>-type-id			<struct>)
  (define-type-id-retriever <record>-type-id			<record>)
  (define-type-id-retriever <vector>-type-id			<vector>)
  (define-type-id-retriever <nevector>-type-id			<nevector>)
  (define-type-id-retriever <empty-vector>-type-id		<empty-vector>)
  (define-type-id-retriever <string>-type-id			<string>)
  (define-type-id-retriever <nestring>-type-id			<nestring>)
  (define-type-id-retriever <empty-string>-type-id		<empty-string>)
  (define-type-id-retriever <pair>-type-id			<pair>)
  (define-type-id-retriever <list>-type-id			<list>)
  (define-type-id-retriever <nelist>-type-id			<nelist>)
  (define-type-id-retriever <null>-type-id			<null>)
  (define-type-id-retriever <stx>-type-id			<stx>)
  (define-type-id-retriever <syntactic-identifier>-type-id	<syntactic-identifier>)
  (define-type-id-retriever <condition>-type-id			<condition>)
  (define-type-id-retriever <compound-condition>-type-id	<compound-condition>)
  (define-type-id-retriever &condition-type-id			&condition)
  #| end of let-syntax |# )

(let-syntax-rules
    (((define-type-spec-retriever ?who ?tag)
      (define ?who
	(let ((memoized-ots #f))
	  (lambda ()
	    (or memoized-ots
		(receive-and-return (ots)
		    (id->object-type-spec (core-prim-id '?tag) (make-empty-lexenv))
		  (set! memoized-ots ots))))))))
  (define-type-spec-retriever <void>-ots			<void>)
  (define-type-spec-retriever <untyped>-ots			<untyped>)
  (define-type-spec-retriever <bottom>-ots			<bottom>)
  (define-type-spec-retriever <top>-ots				<top>)
  (define-type-spec-retriever <procedure>-ots			<procedure>)
  (define-type-spec-retriever <boolean>-ots			<boolean>)
  (define-type-spec-retriever <true>-ots			<true>)
  (define-type-spec-retriever <false>-ots			<false>)
  (define-type-spec-retriever <symbol>-ots			<symbol>)
  (define-type-spec-retriever <struct>-ots			<struct>)
  (define-type-spec-retriever <record>-ots			<record>)
  (define-type-spec-retriever <vector>-ots			<vector>)
  (define-type-spec-retriever <nevector>-ots			<nevector>)
  (define-type-spec-retriever <empty-vector>-ots		<empty-vector>)
  (define-type-spec-retriever <string>-ots			<string>)
  (define-type-spec-retriever <nestring>-ots			<nestring>)
  (define-type-spec-retriever <empty-string>-ots		<empty-string>)
  (define-type-spec-retriever <hashtable>-ots			<hashtable>)
  (define-type-spec-retriever <null>-ots			<null>)
  (define-type-spec-retriever <list>-ots			<list>)
  (define-type-spec-retriever <nelist>-ots			<nelist>)
  (define-type-spec-retriever <pair>-ots			<pair>)
  (define-type-spec-retriever <stx>-ots				<stx>)
  (define-type-spec-retriever <syntactic-identifier>-ots	<syntactic-identifier>)
  (define-type-spec-retriever <condition>-ots			<condition>)
  (define-type-spec-retriever <compound-condition>-ots		<compound-condition>)
  (define-type-spec-retriever &condition-ots			&condition)
  #| end of let-syntax |# )

;;; --------------------------------------------------------------------

(let-syntax-rules
    (((define-type-id-predicate ?who ?tag-retriever)
      (define (?who obj)
	(and (identifier? obj)
	     (~free-identifier=? obj (?tag-retriever))))))
  (define-type-id-predicate <void>-type-id?			<void>-type-id)
  (define-type-id-predicate <untyped>-type-id?			<untyped>-type-id)
  (define-type-id-predicate <bottom>-type-id?			<bottom>-type-id)
  (define-type-id-predicate <top>-type-id?			<top>-type-id)
  (define-type-id-predicate <boolean>-type-id?			<boolean>-type-id)
  (define-type-id-predicate <true>-type-id?			<true>-type-id)
  (define-type-id-predicate <false>-type-id?			<false>-type-id)
  (define-type-id-predicate <symbol>-type-id?			<symbol>-type-id)
  (define-type-id-predicate <procedure>-type-id?		<procedure>-type-id)
  (define-type-id-predicate <vector>-type-id?			<vector>-type-id)
  (define-type-id-predicate <nevector>-type-id?			<nevector>-type-id)
  (define-type-id-predicate <empty-vector>-type-id?		<empty-vector>-type-id)
  (define-type-id-predicate <string>-type-id?			<string>-type-id)
  (define-type-id-predicate <nestring>-type-id?			<nestring>-type-id)
  (define-type-id-predicate <empty-string>-type-id?		<empty-string>-type-id)
  (define-type-id-predicate <pair>-type-id?			<pair>-type-id)
  (define-type-id-predicate <list>-type-id?			<list>-type-id)
  (define-type-id-predicate <nelist>-type-id?			<nelist>-type-id)
  (define-type-id-predicate <null>-type-id?			<null>-type-id)
  (define-type-id-predicate <record>-type-id?			<record>-type-id)
  (define-type-id-predicate <stx>-type-id?			<stx>-type-id)
  (define-type-id-predicate <syntactic-identifier>-type-id?	<syntactic-identifier>-type-id)
  (define-type-id-predicate <condition>-type-id?		<condition>-type-id)
  (define-type-id-predicate <compound-condition>-type-id?	<compound-condition>-type-id)
  (define-type-id-predicate &condition-type-id?			&condition-type-id)
  #| end of LET-SYNTAX |# )

;;We  want to  define fast  predicates like  "<list>-ots?"  which  can be  applied to
;;instances of "<object-type-spec>" and determine  if they are instances representing
;;built-in object-types.
;;
;;We generate only  one OTS for every built-in  type in a single run  of the "vicare"
;;process, but multiple runs  may store an OTS in a FASL file;  so at run-time we end
;;up with multiple  OTSs representing the same  built-in type.  This is  why in these
;;predicates we use FREE-IDENTIFIER=? to compare the object-type specifications.
;;
(let-syntax-rules
    (((define-type-spec-predicate ?who ?ots-retriever)
      (define ?who
	(let ((memoised-ots  #f)
	      (memoised-name #f))
	  (lambda (obj)
	    (let ((src-ots (or memoised-ots
			       (receive-and-return (src-ots)
				   (?ots-retriever)
				 (set! memoised-ots src-ots)))))
	      (or (eq? obj src-ots)
		  (and (core-type-spec? obj)
		       ;;We know that  if the type of OBJ  is "<core-type-spec>", its
		       ;;NAME field holds an identifier.
		       (let ((src-name (or memoised-name
					   (receive-and-return (src-name)
					       (object-type-spec.name src-ots)
					     (set! memoised-name src-name)))))
			 (~free-identifier=? (object-type-spec.name obj) src-name))))))))))
  (define-type-spec-predicate <void>-ots?			<void>-ots)
  (define-type-spec-predicate <untyped>-ots?			<untyped>-ots)
  (define-type-spec-predicate <bottom>-ots?			<bottom>-ots)
  (define-type-spec-predicate <top>-ots?			<top>-ots)
  (define-type-spec-predicate <boolean>-ots?			<boolean>-ots)
  (define-type-spec-predicate <true>-ots?			<true>-ots)
  (define-type-spec-predicate <false>-ots?			<false>-ots)
  (define-type-spec-predicate <symbol>-ots?			<symbol>-ots)
  (define-type-spec-predicate <struct>-ots?			<struct>-ots)
  (define-type-spec-predicate <record>-ots?			<record>-ots)
  (define-type-spec-predicate <procedure>-ots?			<procedure>-ots)
  (define-type-spec-predicate <vector>-ots?			<vector>-ots)
  (define-type-spec-predicate <nevector>-ots?			<nevector>-ots)
  (define-type-spec-predicate <empty-vector>-ots?		<empty-vector>-ots)
  (define-type-spec-predicate <string>-ots?			<string>-ots)
  (define-type-spec-predicate <nestring>-ots?			<nestring>-ots)
  (define-type-spec-predicate <empty-string>-ots?		<empty-string>-ots)
  (define-type-spec-predicate <hashtable>-ots?			<hashtable>-ots)
  (define-type-spec-predicate <null>-ots?			<null>-ots)
  (define-type-spec-predicate <list>-ots?			<list>-ots)
  (define-type-spec-predicate <nelist>-ots?			<nelist>-ots)
  (define-type-spec-predicate <pair>-ots?			<pair>-ots)
  (define-type-spec-predicate <stx>-ots?			<stx>-ots)
  (define-type-spec-predicate <syntactic-identifier>-ots?	<syntactic-identifier>-ots)
  (define-type-spec-predicate <condition>-ots?			<condition>-ots)
  (define-type-spec-predicate <compound-condition>-ots?		<compound-condition>-ots)
  (define-type-spec-predicate &condition-ots?			&condition-ots)
  #| end of LET-SYNTAX |# )


;;;; errors helpers

(define (expression-position x)
  (if (stx? x)
      (let ((x (stx-expr x)))
	(if (reader-annotation? x)
	    (reader-annotation-textual-position x)
	  (condition)))
    (condition)))


;;;; done

;; #!vicare
;; (import (only (vicare) foreign-call))
;; (foreign-call "ikrt_print_emergency" #ve(ascii "psyntax.lexical-environment.sls here"))

#| end of library |# )

;;; end of file
