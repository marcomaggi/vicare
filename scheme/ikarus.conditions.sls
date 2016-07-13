;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus conditions)
  (export
    define-core-condition-type

    condition? compound-condition? condition-and-rtd?
    simple-condition?
    list-of-conditions?
    list-of-simple-conditions?
    simple-conditions condition-predicate
    condition condition-accessor print-condition

    raise-non-continuable-standard-condition

    make-message-condition message-condition?
    condition-message make-warning warning?
    make-serious-condition serious-condition? make-error
    error? make-violation violation? make-assertion-violation
    assertion-violation? make-irritants-condition
    irritants-condition? condition-irritants
    make-who-condition who-condition? condition-who
    make-non-continuable-violation non-continuable-violation?
    make-implementation-restriction-violation
    implementation-restriction-violation?
    make-lexical-violation lexical-violation?
    make-syntax-violation syntax-violation?
    syntax-violation-form syntax-violation-subform
    make-undefined-violation undefined-violation?
    make-i/o-error i/o-error? make-i/o-read-error
    i/o-read-error? make-i/o-write-error i/o-write-error?
    make-i/o-invalid-position-error
    i/o-invalid-position-error? i/o-error-position
    make-i/o-filename-error i/o-filename-error?
    i/o-error-filename make-i/o-file-protection-error
    i/o-file-protection-error? make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error? make-i/o-port-error
    i/o-port-error? i/o-error-port make-i/o-decoding-error
    i/o-decoding-error? make-i/o-encoding-error
    i/o-encoding-error? i/o-encoding-error-char
    no-infinities-violation? make-no-infinities-violation
    no-nans-violation? make-no-nans-violation
    interrupted-condition? make-interrupted-condition
    make-source-position-condition source-position-condition?
    source-position-port-id
    source-position-byte source-position-character
    source-position-line source-position-column

    &condition			&message		&warning
    &serious			&error			&violation
    &assertion			&irritants		&who
    &non-continuable		&implementation-restriction
    &lexical			&syntax			&undefined
    &i/o			&i/o-read		&i/o-write
    &i/o-invalid-position	&i/o-filename		&i/o-file-protection
    &i/o-file-is-read-only	&i/o-file-already-exists
    &i/o-file-does-not-exist	&i/o-port		&i/o-decoding
    &i/o-encoding		&no-infinities
    &no-nans			&interrupted		&source-position

    &condition-rtd &condition-rcd &message-rtd &message-rcd
    &warning-rtd &warning-rcd &serious-rtd &serious-rcd
    &error-rtd &error-rcd &violation-rtd &violation-rcd
    &assertion-rtd &assertion-rcd &irritants-rtd
    &irritants-rcd &who-rtd &who-rcd &non-continuable-rtd
    &non-continuable-rcd &implementation-restriction-rtd
    &implementation-restriction-rcd &lexical-rtd &lexical-rcd
    &syntax-rtd &syntax-rcd &undefined-rtd &undefined-rcd
    &i/o-rtd &i/o-rcd &i/o-read-rtd &i/o-read-rcd
    &i/o-write-rtd &i/o-write-rcd &i/o-invalid-position-rtd
    &i/o-invalid-position-rcd &i/o-filename-rtd
    &i/o-filename-rcd &i/o-file-protection-rtd
    &i/o-file-protection-rcd &i/o-file-is-read-only-rtd
    &i/o-file-is-read-only-rcd &i/o-file-already-exists-rtd
    &i/o-file-already-exists-rcd &i/o-file-does-not-exist-rtd
    &i/o-file-does-not-exist-rcd &i/o-port-rtd &i/o-port-rcd
    &i/o-decoding-rtd &i/o-decoding-rcd &i/o-encoding-rtd
    &i/o-encoding-rcd &no-infinities-rtd &no-infinities-rcd
    &no-nans-rtd &no-nans-rcd
    &interrupted-rtd &interrupted-rcd
    &source-position-rtd &source-position-rcd

    ;;&i/o-eagain
    make-i/o-eagain i/o-eagain-error?
    &i/o-eagain-rtd &i/o-eagain-rcd

    ;;&errno
    &errno-rtd &errno-rcd
    make-errno-condition errno-condition? condition-errno
    ;;&h_errno
    &h_errno-rtd &h_errno-rcd
    make-h_errno-condition h_errno-condition? condition-h_errno

    ;; &i/o-wrong-fasl-header-error
    &i/o-wrong-fasl-header-error-rtd &i/o-wrong-fasl-header-error-rcd
    make-i/o-wrong-fasl-header-error
    i/o-wrong-fasl-header-error?

    ;;&out-of-memory-error
    &out-of-memory-error-rtd &out-of-memory-error-rcd
    make-out-of-memory-error out-of-memory-error?

    ;;&failed-expression
    &failed-expression-rtd
    &failed-expression-rcd
    make-failed-expression-condition
    failed-expression-condition?
    condition-failed-expression

    ;;&one-based-return-value-index
    &one-based-return-value-index-rtd
    &one-based-return-value-index-rcd
    make-one-based-return-value-index-condition
    one-based-return-value-index-condition?
    condition-one-based-return-value-index

    ;;&procedure-precondition-violation
    &procedure-precondition-violation-rtd
    &procedure-precondition-violation-rcd
    make-procedure-precondition-violation
    procedure-precondition-violation?

    ;;&procedure-postcondition-violation
    &procedure-postcondition-violation-rtd
    &procedure-postcondition-violation-rcd
    make-procedure-postcondition-violation
    procedure-postcondition-violation?

    ;;&procedure-argument-violation
    &procedure-argument-violation-rtd
    &procedure-argument-violation-rcd
    make-procedure-argument-violation
    procedure-argument-violation?
    procedure-argument-violation

    ;;&procedure-signature-argument-violation
    &procedure-signature-argument-violation-rtd
    &procedure-signature-argument-violation-rcd
    make-procedure-signature-argument-violation
    procedure-signature-argument-violation?
    procedure-signature-argument-violation.one-based-argument-index
    procedure-signature-argument-violation.failed-expression
    procedure-signature-argument-violation.offending-value
    procedure-signature-argument-violation

    ;;&procedure-signature-return-value-violation
    &procedure-signature-return-value-violation-rtd
    &procedure-signature-return-value-violation-rcd
    make-procedure-signature-return-value-violation
    procedure-signature-return-value-violation?
    procedure-signature-return-value-violation.one-based-return-value-index
    procedure-signature-return-value-violation.failed-expression
    procedure-signature-return-value-violation.offending-value
    procedure-signature-return-value-violation

    ;;&procedure-arguments-consistency-violation
    &procedure-arguments-consistency-violation-rtd
    &procedure-arguments-consistency-violation-rcd
    make-procedure-arguments-consistency-violation
    procedure-arguments-consistency-violation?
    procedure-arguments-consistency-violation
    procedure-arguments-consistency-violation/failed-expression

    ;;&expression-return-value-violation
    &expression-return-value-violation-rtd
    &expression-return-value-violation-rcd
    make-expression-return-value-violation
    expression-return-value-violation?
    expression-return-value-violation

    ;;&non-reinstatable
    &non-reinstatable-rtd
    &non-reinstatable-rcd
    make-non-reinstatable-violation
    non-reinstatable-violation?
    non-reinstatable-violation

    ;; late binding errors
    &late-binding-error-rtd
    &late-binding-error-rcd
    make-late-binding-error
    late-binding-error?

    &method-late-binding-error-rtd
    &method-late-binding-error-rcd
    make-method-late-binding-error
    method-late-binding-error?

    &overloaded-function-late-binding-error-rtd
    &overloaded-function-late-binding-error-rcd
    make-overloaded-function-late-binding-error
    overloaded-function-late-binding-error?
    overloaded-function-late-binding-error.overloaded-function-descriptor

    &interface-method-late-binding-error-rtd
    &interface-method-late-binding-error-rcd
    make-interface-method-late-binding-error
    interface-method-late-binding-error?
    interface-method-late-binding-error.interface-uid
    interface-method-late-binding-error.method-name
    interface-method-late-binding-error.subject
    interface-method-late-binding-error.type-descriptor

    ;; string encoding and decoding
    ;;&string-encoding
    &string-encoding-rtd
    &string-encoding-rcd
    make-string-encoding-error
    string-encoding-error?

    ;;&string-decoding
    &string-decoding-rtd
    &string-decoding-rcd
    make-string-decoding-error
    string-decoding-error?

;;;

    ;;&utf8-string-encoding
    &utf8-string-encoding-rtd
    &utf8-string-encoding-rcd
    make-utf8-string-encoding-error
    utf8-string-encoding-error?

    ;;&utf16-string-encoding
    &utf16-string-encoding-rtd
    &utf16-string-encoding-rcd
    make-utf16-string-encoding-error
    utf16-string-encoding-error?

    ;;&utf32-string-encoding
    &utf32-string-encoding-rtd
    &utf32-string-encoding-rcd
    make-utf32-string-encoding-error
    utf32-string-encoding-error?

    ;;&utf8-string-decoding
    &utf8-string-decoding-rtd
    &utf8-string-decoding-rcd
    make-utf8-string-decoding-error
    utf8-string-decoding-error?

    ;;&utf16-string-decoding
    &utf16-string-decoding-rtd
    &utf16-string-decoding-rcd
    make-utf16-string-decoding-error
    utf16-string-decoding-error?

    ;;&utf32-string-decoding
    &utf32-string-decoding-rtd
    &utf32-string-decoding-rcd
    make-utf32-string-decoding-error
    utf32-string-decoding-error?

;;;

    ;;&utf8-string-decoding-invalid-octet
    &utf8-string-decoding-invalid-octet-rtd
    &utf8-string-decoding-invalid-octet-rcd
    make-utf8-string-decoding-invalid-octet
    utf8-string-decoding-invalid-octet?
    utf8-string-decoding-invalid-octet.bytevector
    utf8-string-decoding-invalid-octet.index
    utf8-string-decoding-invalid-octet.octets

    ;;&utf8-string-decoding-invalid-2-tuple
    &utf8-string-decoding-invalid-2-tuple-rtd
    &utf8-string-decoding-invalid-2-tuple-rcd
    make-utf8-string-decoding-invalid-2-tuple
    utf8-string-decoding-invalid-2-tuple?
    utf8-string-decoding-invalid-2-tuple.bytevector
    utf8-string-decoding-invalid-2-tuple.index
    utf8-string-decoding-invalid-2-tuple.octets

    ;;&utf8-string-decoding-invalid-3-tuple
    &utf8-string-decoding-invalid-3-tuple-rtd
    &utf8-string-decoding-invalid-3-tuple-rcd
    make-utf8-string-decoding-invalid-3-tuple
    utf8-string-decoding-invalid-3-tuple?
    utf8-string-decoding-invalid-3-tuple.bytevector
    utf8-string-decoding-invalid-3-tuple.index
    utf8-string-decoding-invalid-3-tuple.octets

    ;;&utf8-string-decoding-invalid-4-tuple
    &utf8-string-decoding-invalid-4-tuple-rtd
    &utf8-string-decoding-invalid-4-tuple-rcd
    make-utf8-string-decoding-invalid-4-tuple
    utf8-string-decoding-invalid-4-tuple?
    utf8-string-decoding-invalid-4-tuple.bytevector
    utf8-string-decoding-invalid-4-tuple.index
    utf8-string-decoding-invalid-4-tuple.octets

    ;;&utf8-string-decoding-incomplete-2-tuple
    &utf8-string-decoding-incomplete-2-tuple-rtd
    &utf8-string-decoding-incomplete-2-tuple-rcd
    make-utf8-string-decoding-incomplete-2-tuple
    utf8-string-decoding-incomplete-2-tuple?
    utf8-string-decoding-incomplete-2-tuple.bytevector
    utf8-string-decoding-incomplete-2-tuple.index
    utf8-string-decoding-incomplete-2-tuple.octets

    ;;&utf8-string-decoding-incomplete-3-tuple
    &utf8-string-decoding-incomplete-3-tuple-rtd
    &utf8-string-decoding-incomplete-3-tuple-rcd
    make-utf8-string-decoding-incomplete-3-tuple
    utf8-string-decoding-incomplete-3-tuple?
    utf8-string-decoding-incomplete-3-tuple.bytevector
    utf8-string-decoding-incomplete-3-tuple.index
    utf8-string-decoding-incomplete-3-tuple.octets

    ;;&utf8-string-decoding-incomplete-4-tuple
    &utf8-string-decoding-incomplete-4-tuple-rtd
    &utf8-string-decoding-incomplete-4-tuple-rcd
    make-utf8-string-decoding-incomplete-4-tuple
    utf8-string-decoding-incomplete-4-tuple?
    utf8-string-decoding-incomplete-4-tuple.bytevector
    utf8-string-decoding-incomplete-4-tuple.index
    utf8-string-decoding-incomplete-4-tuple.octets

;;;

    ;;&utf16-string-decoding-invalid-first-word
    &utf16-string-decoding-invalid-first-word-rtd
    &utf16-string-decoding-invalid-first-word-rcd
    make-utf16-string-decoding-invalid-first-word
    utf16-string-decoding-invalid-first-word?
    utf16-string-decoding-invalid-first-word.bytevector
    utf16-string-decoding-invalid-first-word.index
    utf16-string-decoding-invalid-first-word.word

    ;;&utf16-string-decoding-invalid-second-word
    &utf16-string-decoding-invalid-second-word-rtd
    &utf16-string-decoding-invalid-second-word-rcd
    make-utf16-string-decoding-invalid-second-word
    utf16-string-decoding-invalid-second-word?
    utf16-string-decoding-invalid-second-word.bytevector
    utf16-string-decoding-invalid-second-word.index
    utf16-string-decoding-invalid-second-word.first-word
    utf16-string-decoding-invalid-second-word.second-word

    ;;&utf16-string-decoding-missing-second-word
    &utf16-string-decoding-missing-second-word-rtd
    &utf16-string-decoding-missing-second-word-rcd
    make-utf16-string-decoding-missing-second-word
    utf16-string-decoding-missing-second-word?
    utf16-string-decoding-missing-second-word.bytevector
    utf16-string-decoding-missing-second-word.index
    utf16-string-decoding-missing-second-word.word

    ;;&utf16-string-decoding-standalone-octet
    &utf16-string-decoding-standalone-octet-rtd
    &utf16-string-decoding-standalone-octet-rcd
    make-utf16-string-decoding-standalone-octet
    utf16-string-decoding-standalone-octet?
    utf16-string-decoding-standalone-octet.bytevector
    utf16-string-decoding-standalone-octet.index
    utf16-string-decoding-standalone-octet.octet

;;;

    ;;&utf32-string-decoding-invalid-word
    &utf32-string-decoding-invalid-word-rtd
    &utf32-string-decoding-invalid-word-rcd
    make-utf32-string-decoding-invalid-word
    utf32-string-decoding-invalid-word?
    utf32-string-decoding-invalid-word.bytevector
    utf32-string-decoding-invalid-word.index
    utf32-string-decoding-invalid-word.word

    ;;&utf32-string-decoding-orphan-octets
    &utf32-string-decoding-orphan-octets-rtd
    &utf32-string-decoding-orphan-octets-rcd
    make-utf32-string-decoding-orphan-octets
    utf32-string-decoding-orphan-octets?
    utf32-string-decoding-orphan-octets.bytevector
    utf32-string-decoding-orphan-octets.index
    utf32-string-decoding-orphan-octets.octets

    ;; macros
    preconditions

    ;; for internal use only
    $condition-predicate			$condition-accessor
    make-simple-condition			%raise-out-of-memory)
  (import (except (vicare)
		  ;;We use an internal macro  definition to define condition types in
		  ;;this library.
		  define-condition-type

		  condition? compound-condition? condition-and-rtd?
		  simple-condition?
		  list-of-conditions?
		  list-of-simple-conditions?
		  simple-conditions
		  condition condition-predicate condition-accessor
		  print-condition

		  raise-non-continuable-standard-condition

		  &condition &message &warning &serious &error &violation
		  &assertion &irritants &who &non-continuable
		  &implementation-restriction &lexical &syntax &undefined
		  &i/o &i/o-read &i/o-write &i/o-invalid-position
		  &i/o-filename &i/o-file-protection &i/o-file-is-read-only
		  &i/o-file-already-exists &i/o-file-does-not-exist
		  &i/o-port &i/o-decoding &i/o-encoding &no-infinities
		  &no-nans

		  make-message-condition message-condition?
		  condition-message make-warning warning?
		  make-serious-condition serious-condition? make-error
		  error? make-violation violation? make-assertion-violation
		  assertion-violation? make-irritants-condition
		  irritants-condition? condition-irritants
		  make-who-condition who-condition? condition-who
		  make-non-continuable-violation non-continuable-violation?
		  make-implementation-restriction-violation
		  implementation-restriction-violation?
		  make-lexical-violation lexical-violation?
		  make-syntax-violation syntax-violation?
		  syntax-violation-form syntax-violation-subform
		  make-undefined-violation undefined-violation?
		  make-i/o-error i/o-error? make-i/o-read-error
		  i/o-read-error? make-i/o-write-error i/o-write-error?
		  make-i/o-invalid-position-error
		  i/o-invalid-position-error? i/o-error-position
		  make-i/o-filename-error i/o-filename-error?
		  i/o-error-filename make-i/o-file-protection-error
		  i/o-file-protection-error? make-i/o-file-is-read-only-error
		  i/o-file-is-read-only-error?
		  make-i/o-file-already-exists-error
		  i/o-file-already-exists-error?
		  make-i/o-file-does-not-exist-error
		  i/o-file-does-not-exist-error? make-i/o-port-error
		  i/o-port-error? i/o-error-port make-i/o-decoding-error
		  i/o-decoding-error? make-i/o-encoding-error
		  i/o-encoding-error? i/o-encoding-error-char
		  no-infinities-violation? make-no-infinities-violation
		  no-nans-violation? make-no-nans-violation

		  &i/o-eagain make-i/o-eagain i/o-eagain-error?
		  &i/o-eagain-rtd &i/o-eagain-rcd

		  &errno &errno-rtd &errno-rcd
		  make-errno-condition errno-condition?
		  condition-errno

		  &h_errno &h_errno-rtd &h_errno-rcd
		  make-h_errno-condition h_errno-condition?
		  condition-h_errno

		  &i/o-wrong-fasl-header-error-rtd &i/o-wrong-fasl-header-error-rcd
		  &i/o-wrong-fasl-header-error
		  make-i/o-wrong-fasl-header-error
		  i/o-wrong-fasl-header-error?

		  &out-of-memory-error-rtd &out-of-memory-error-rcd
		  &out-of-memory-error
		  make-out-of-memory-error out-of-memory-error?

		  &interrupted &interrupted-rtd &interrupted-rcd
		  interrupted-condition? make-interrupted-condition

		  &source-position &source-position-rtd &source-position-rcd
		  make-source-position-condition source-position-condition?
		  source-position-port-id
		  source-position-byte source-position-character
		  source-position-line source-position-column

		  &failed-expression
		  &failed-expression-rtd
		  &failed-expression-rcd
		  make-failed-expression-condition
		  failed-expression-condition?
		  condition-failed-expression

		  &one-based-return-value-index
		  &one-based-return-value-index-rtd
		  &one-based-return-value-index-rcd
		  make-one-based-return-value-index-condition
		  one-based-return-value-index-condition?
		  condition-one-based-return-value-index

		  &procedure-precondition-violation
		  &procedure-precondition-violation-rtd
		  &procedure-precondition-violation-rcd
		  make-procedure-precondition-violation
		  procedure-precondition-violation?

		  &procedure-postcondition-violation
		  &procedure-postcondition-violation-rtd
		  &procedure-postcondition-violation-rcd
		  make-procedure-postcondition-violation
		  procedure-postcondition-violation?

		  &procedure-argument-violation
		  &procedure-argument-violation-rtd
		  &procedure-argument-violation-rcd
		  make-procedure-argument-violation
		  procedure-argument-violation?
		  procedure-argument-violation

		  &procedure-signature-argument-violation
		  &procedure-signature-argument-violation-rtd
		  &procedure-signature-argument-violation-rcd
		  make-procedure-signature-argument-violation
		  procedure-signature-argument-violation?
		  procedure-signature-argument-violation.one-based-argument-index
		  procedure-signature-argument-violation.failed-expression
		  procedure-signature-argument-violation.offending-value
		  procedure-signature-argument-violation

		  &procedure-signature-return-value-violation
		  &procedure-signature-return-value-violation-rtd
		  &procedure-signature-return-value-violation-rcd
		  make-procedure-signature-return-value-violation
		  procedure-signature-return-value-violation?
		  procedure-signature-return-value-violation.one-based-return-value-index
		  procedure-signature-return-value-violation.failed-expression
		  procedure-signature-return-value-violation.offending-value
		  procedure-signature-return-value-violation

		  &procedure-arguments-consistency-violation
		  &procedure-arguments-consistency-violation-rtd
		  &procedure-arguments-consistency-violation-rcd
		  make-procedure-arguments-consistency-violation
		  procedure-arguments-consistency-violation?
		  procedure-arguments-consistency-violation
		  procedure-arguments-consistency-violation/failed-expression

		  &expression-return-value-violation
		  &expression-return-value-violation-rtd
		  &expression-return-value-violation-rcd
		  make-expression-return-value-violation
		  expression-return-value-violation?
		  expression-return-value-violation

		  &non-reinstatable
		  &non-reinstatable-rtd
		  &non-reinstatable-rcd
		  make-non-reinstatable-violation
		  non-reinstatable-violation?
		  non-reinstatable-violation

		  &late-binding-error-rtd
		  &late-binding-error-rcd
		  &late-binding-error
		  make-late-binding-error
		  late-binding-error?

		  &method-late-binding-error-rtd
		  &method-late-binding-error-rcd
		  &method-late-binding-error
		  make-method-late-binding-error
		  method-late-binding-error?

		  &overloaded-function-late-binding-error-rtd
		  &overloaded-function-late-binding-error-rcd
		  &overloaded-function-late-binding-error
		  make-overloaded-function-late-binding-error
		  overloaded-function-late-binding-error?
		  overloaded-function-late-binding-error.overloaded-function-descriptor

		  &interface-method-late-binding-error-rtd
		  &interface-method-late-binding-error-rcd
		  &interface-method-late-binding-error
		  make-interface-method-late-binding-error
		  interface-method-late-binding-error?
		  interface-method-late-binding-error.interface-uid
		  interface-method-late-binding-error.method-name
		  interface-method-late-binding-error.subject
		  interface-method-late-binding-error.type-descriptor

		  ;; string encoding and decoding
		  &string-encoding
		  &string-encoding-rtd
		  &string-encoding-rcd
		  make-string-encoding-error
		  string-encoding-error?

		  &string-decoding
		  &string-decoding-rtd
		  &string-decoding-rcd
		  make-string-decoding-error
		  string-decoding-error?

		  &utf8-string-encoding
		  &utf8-string-encoding-rtd
		  &utf8-string-encoding-rcd
		  make-utf8-string-encoding-error
		  utf8-string-encoding-error?

		  &utf16-string-encoding
		  &utf16-string-encoding-rtd
		  &utf16-string-encoding-rcd
		  make-utf16-string-encoding-error
		  utf16-string-encoding-error?

		  &utf32-string-encoding
		  &utf32-string-encoding-rtd
		  &utf32-string-encoding-rcd
		  make-utf32-string-encoding-error
		  utf32-string-encoding-error?

		  &utf8-string-decoding
		  &utf8-string-decoding-rtd
		  &utf8-string-decoding-rcd
		  make-utf8-string-decoding-error
		  utf8-string-decoding-error?

		  &utf16-string-decoding
		  &utf16-string-decoding-rtd
		  &utf16-string-decoding-rcd
		  make-utf16-string-decoding-error
		  utf16-string-decoding-error?

		  &utf32-string-decoding
		  &utf32-string-decoding-rtd
		  &utf32-string-decoding-rcd
		  make-utf32-string-decoding-error
		  utf32-string-decoding-error?

		  &utf8-string-decoding-invalid-octet
		  &utf8-string-decoding-invalid-octet-rtd
		  &utf8-string-decoding-invalid-octet-rcd
		  make-utf8-string-decoding-invalid-octet
		  utf8-string-decoding-invalid-octet?
		  utf8-string-decoding-invalid-octet.bytevector
		  utf8-string-decoding-invalid-octet.index
		  utf8-string-decoding-invalid-octet.octets

		  &utf8-string-decoding-invalid-2-tuple
		  &utf8-string-decoding-invalid-2-tuple-rtd
		  &utf8-string-decoding-invalid-2-tuple-rcd
		  make-utf8-string-decoding-invalid-2-tuple
		  utf8-string-decoding-invalid-2-tuple?
		  utf8-string-decoding-invalid-2-tuple.bytevector
		  utf8-string-decoding-invalid-2-tuple.index
		  utf8-string-decoding-invalid-2-tuple.octets

		  &utf8-string-decoding-invalid-3-tuple
		  &utf8-string-decoding-invalid-3-tuple-rtd
		  &utf8-string-decoding-invalid-3-tuple-rcd
		  make-utf8-string-decoding-invalid-3-tuple
		  utf8-string-decoding-invalid-3-tuple?
		  utf8-string-decoding-invalid-3-tuple.bytevector
		  utf8-string-decoding-invalid-3-tuple.index
		  utf8-string-decoding-invalid-3-tuple.octets

		  &utf8-string-decoding-invalid-4-tuple
		  &utf8-string-decoding-invalid-4-tuple-rtd
		  &utf8-string-decoding-invalid-4-tuple-rcd
		  make-utf8-string-decoding-invalid-4-tuple
		  utf8-string-decoding-invalid-4-tuple?
		  utf8-string-decoding-invalid-4-tuple.bytevector
		  utf8-string-decoding-invalid-4-tuple.index
		  utf8-string-decoding-invalid-4-tuple.octets

		  &utf8-string-decoding-incomplete-2-tuple
		  &utf8-string-decoding-incomplete-2-tuple-rtd
		  &utf8-string-decoding-incomplete-2-tuple-rcd
		  make-utf8-string-decoding-incomplete-2-tuple
		  utf8-string-decoding-incomplete-2-tuple?
		  utf8-string-decoding-incomplete-2-tuple.bytevector
		  utf8-string-decoding-incomplete-2-tuple.index
		  utf8-string-decoding-incomplete-2-tuple.octets

		  &utf8-string-decoding-incomplete-3-tuple
		  &utf8-string-decoding-incomplete-3-tuple-rtd
		  &utf8-string-decoding-incomplete-3-tuple-rcd
		  make-utf8-string-decoding-incomplete-3-tuple
		  utf8-string-decoding-incomplete-3-tuple?
		  utf8-string-decoding-incomplete-3-tuple.bytevector
		  utf8-string-decoding-incomplete-3-tuple.index
		  utf8-string-decoding-incomplete-3-tuple.octets

		  &utf8-string-decoding-incomplete-4-tuple
		  &utf8-string-decoding-incomplete-4-tuple-rtd
		  &utf8-string-decoding-incomplete-4-tuple-rcd
		  make-utf8-string-decoding-incomplete-4-tuple
		  utf8-string-decoding-incomplete-4-tuple?
		  utf8-string-decoding-incomplete-4-tuple.bytevector
		  utf8-string-decoding-incomplete-4-tuple.index
		  utf8-string-decoding-incomplete-4-tuple.octets


		  &utf16-string-decoding-invalid-first-word
		  &utf16-string-decoding-invalid-first-word-rtd
		  &utf16-string-decoding-invalid-first-word-rcd
		  make-utf16-string-decoding-invalid-first-word
		  utf16-string-decoding-invalid-first-word?
		  utf16-string-decoding-invalid-first-word.bytevector
		  utf16-string-decoding-invalid-first-word.index
		  utf16-string-decoding-invalid-first-word.word

		  &utf16-string-decoding-invalid-second-word
		  &utf16-string-decoding-invalid-second-word-rtd
		  &utf16-string-decoding-invalid-second-word-rcd
		  make-utf16-string-decoding-invalid-second-word
		  utf16-string-decoding-invalid-second-word?
		  utf16-string-decoding-invalid-second-word.bytevector
		  utf16-string-decoding-invalid-second-word.index
		  utf16-string-decoding-invalid-second-word.first-word
		  utf16-string-decoding-invalid-second-word.second-word

		  &utf16-string-decoding-missing-second-word
		  &utf16-string-decoding-missing-second-word-rtd
		  &utf16-string-decoding-missing-second-word-rcd
		  make-utf16-string-decoding-missing-second-word
		  utf16-string-decoding-missing-second-word?
		  utf16-string-decoding-missing-second-word.bytevector
		  utf16-string-decoding-missing-second-word.index
		  utf16-string-decoding-missing-second-word.word

		  &utf16-string-decoding-standalone-octet
		  &utf16-string-decoding-standalone-octet-rtd
		  &utf16-string-decoding-standalone-octet-rcd
		  make-utf16-string-decoding-standalone-octet
		  utf16-string-decoding-standalone-octet?
		  utf16-string-decoding-standalone-octet.bytevector
		  utf16-string-decoding-standalone-octet.index
		  utf16-string-decoding-standalone-octet.octet

		  &utf32-string-decoding-invalid-word
		  &utf32-string-decoding-invalid-word-rtd
		  &utf32-string-decoding-invalid-word-rcd
		  make-utf32-string-decoding-invalid-word
		  utf32-string-decoding-invalid-word?
		  utf32-string-decoding-invalid-word.bytevector
		  utf32-string-decoding-invalid-word.index
		  utf32-string-decoding-invalid-word.word

		  &utf32-string-decoding-orphan-octets
		  &utf32-string-decoding-orphan-octets-rtd
		  &utf32-string-decoding-orphan-octets-rcd
		  make-utf32-string-decoding-orphan-octets
		  utf32-string-decoding-orphan-octets?
		  utf32-string-decoding-orphan-octets.bytevector
		  utf32-string-decoding-orphan-octets.index
		  utf32-string-decoding-orphan-octets.octets
		  )
    (only (ikarus records procedural)
	  $make-record-type-descriptor
	  $make-record-type-descriptor-ex
	  $make-record-constructor-descriptor
	  $record-and-rtd?
	  $record-constructor
	  $rtd-subtype?
	  $record-accessor/index)
    (vicare system $structs)
    (prefix (only (ikarus records procedural)
		  record-type-method-retriever-set!)
	    records::)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate))


(define-syntax define-condition-type-syntax
  ;;This  syntax is  used by  DEFINE-CORE-CONDITION-TYPE.  It  defines the  syntactic
  ;;binding for the  type name of condition-object  types, so that it  is possible to
  ;;use the name to access the RTD and CTD of the record-type.  Example:
  ;;
  ;;   (define-core-condition-type &demo
  ;;       &condition
  ;;     make-demo-condition condition-decmo?)
  ;;
  ;;   (&demo rtd)	==> &demo-rtd
  ;;   (&demo rcd)	==> &demo-ctd
  ;;
  (syntax-rules ()
    ((_ ?type-name ?rtd ?rcd)
     (define-syntax ?type-name
       (lambda (stx)
	 (syntax-case stx ()
	   ((_ ?command)
	    (identifier? #'?command)
	    (case (syntax->datum #'?command)
	      ((rtd)	#'?rtd)
	      ((rcd)	#'?rcd)
	      (else
	       (syntax-violation (quote ?type-name)
		 "invalid command for core condition-object type name" stx #'?command))))
	   (_
	    (syntax-violation (quote ?type-name) "invalid use of core condition-object type name" stx #f))))))
    ))


(define-syntax (define-core-condition-type stx)
  ;;This macro is used in this library to define condition object types.
  ;;
  ;;NOTE Remember that this  macro is *not* the one exported by  the boot image.  The
  ;;transformer of  the keyword  binding DEFINE-CONDITION-TYPE  exported by  the boot
  ;;image is integrated in the expander.
  ;;
  (define (main input-form.stx)
    (syntax-case input-form.stx ()
      ((?kwd ?name ?parent-name ?constructor ?predicate (?field ?accessor) ...)
       (and (identifier? #'?name)
	    (identifier? #'?parent-name)
	    (identifier? #'?constructor)
	    (identifier? #'?predicate)
	    (andmap identifier? #'(?field ...))
	    (andmap identifier? #'(?accessor ...)))
       (with-syntax
	   ((UID		(mkname "vicare:core-type:" #'?name ""))
	    (GENERATIVE?	#f)
	    (RTD		(mkname "" #'?name "-rtd"))
	    (RCD		(mkname "" #'?name "-rcd"))
	    ((ACCESSOR-IDX ...)	(iota 0 #'(?accessor ...)))
	    (SEALED?		#f)
	    (OPAQUE?		#f)
	    (METHOD-RETRIEVER	(if (null? (syntax->datum #'(?field ...)))
				    #f
				  #'(lambda (name)
				      (case name
					((?field)	?accessor)
					...
					(else #f))))))
	 ;;We use the records procedural layer  and the unsafe functions to make it
	 ;;easier to rotate the boot images.
	 #'(begin ;;module (RTD RCD ?constructor ?predicate ?accessor ...)
	     (define RTD
	       ($make-record-type-descriptor-ex (quote ?name) (?parent-name rtd)
						(quote UID) GENERATIVE? SEALED? OPAQUE?
						'#((immutable ?field) ...) '#((#f . ?field) ...)
						#f ;destructor
						#f ;printer
						#f ;equality-predicate
						#f ;comparison-procedure
						#f ;hash-function
						METHOD-RETRIEVER
						#f ;implemented-interfaces
						))
	     (define RCD
	       ($make-record-constructor-descriptor RTD (?parent-name rcd) #f))
	     (define ?constructor
	       ($record-constructor RCD))
	     (define ?predicate
	       ($condition-predicate RTD))
	     (define ?accessor
	       ($condition-accessor RTD ($record-accessor/index RTD ACCESSOR-IDX (quote ?accessor)) (quote ?accessor)))
	     ...
	     ;;We define this syntactic binding with  the only purpose of using it to
	     ;;access the  syntactic bindings RTD and  RCD in the lexical  context of
	     ;;the definition.
	     (define-condition-type-syntax ?name RTD RCD)
	     )))
      ))

  (define (mkname prefix name suffix)
    (datum->syntax name (string->symbol (string-append prefix (symbol->string (syntax->datum name)) suffix))))

  (define (iota idx stx)
    (syntax-case stx ()
      (()	'())
      ((?x . ?x*)
       (cons idx (iota (fxadd1 idx) #'?x*)))))

  ;; (receive-and-return (out)
  ;;     (main stx)
  ;;   (debug-print 'ikarus.conditions (syntax->datum out)))
  (main stx))


;;;; arguments validation

(define (simple-condition-rtd-subtype? obj)
  (and (record-type-descriptor? obj)
       ($rtd-subtype? obj &condition-rtd)))

(define-list-of-type-predicate list-of-conditions? condition?)
(define-list-of-type-predicate list-of-simple-conditions? simple-condition?)

(define-syntax-rule ($record-of-type ?obj ?rtd)
  (and ($struct? ?obj)
       ($record-and-rtd? ?obj ?rtd)))

(define (who-condition-value? obj)
  (or (not     obj)
      (symbol? obj)
      (string? obj)))


;;;; data types and some predicates

;;NOTE We could  use the records syntactic  layer as shown below, but  instead we use
;;the procedural  layer to allow  boot image  initialisation (without crashes  due to
;;not-yet-initialised core primitives).
;;
;; (begin
;;   (define-record-type (&condition make-simple-condition simple-condition?)
;;     (nongenerative))
;;   (define &condition-rtd
;;     (record-type-descriptor &condition))
;;   (define &condition-rcd
;;     (record-constructor-descriptor &condition))
;;   (define-record-type compound-condition
;;     (nongenerative)
;;     (fields (immutable components))
;;     (sealed #t)
;;     (opaque #f))
;;   #| end of BEGIN |# )

(begin
  (define &condition-rtd
    ($make-record-type-descriptor '&condition #f 'vicare:conditions:&condition #f #f '#() '#()))
  (define &condition-rcd
    ($make-record-constructor-descriptor &condition-rtd #f #f))
  (define make-simple-condition
    ($record-constructor &condition-rcd))
  (define (simple-condition? X)
    ($record-of-type X &condition-rtd))
  ;;We define this syntactic binding with the  only purpose of using it to access the
  ;;syntactic bindings  &CONDITION-RTD and &CONDITION-RCD  in the lexical  context of
  ;;the definition.
  (define-condition-type-syntax &condition &condition-rtd &condition-rcd)
  #| end of BEGIN |# )

(begin
  (define compound-condition-rtd
    ($make-record-type-descriptor 'compound-condition #f 'vicare:conditions:compound-condition #t #f
				  '#((immutable . components)) '#((#f . components))))
  (define compound-condition-rcd
    ($make-record-constructor-descriptor compound-condition-rtd #f #f))
  (define make-compound-condition
    ($record-constructor compound-condition-rcd))
  (define (compound-condition? X)
    ($record-of-type X compound-condition-rtd))
  ;; (define compound-condition-components
  ;;   (record-accessor compound-condition-rtd 0))
  (define ($compound-condition-components cnd)
    ($struct-ref cnd 0))
  ;;We define this syntactic binding with the  only purpose of using it to access the
  ;;syntactic  bindings &COMPOUND-CONDITION-RTD  and  &COMPOUND-CONDITION-RCD in  the
  ;;lexical context of the definition.
  (define-condition-type-syntax &compound-condition &compound-condition-rtd &compound-condition-rcd)
  #| end of BEGIN |# )

;;; --------------------------------------------------------------------

(define (condition? x)
  ;;Defined by R6RS.  Return  #t if X is a (simple  or compound) condition, otherwise
  ;;return #f.
  ;;
  (or (simple-condition? x)
      (compound-condition? x)))

(define* (condition-and-rtd? obj {rtd simple-condition-rtd-subtype?})
  (cond ((compound-condition? obj)
	 (let loop ((ls ($compound-condition-components obj)))
	   (and (pair? ls)
		(or ($record-of-type (car ls) rtd)
		    (loop (cdr ls))))))
	((simple-condition? obj)
	 ($record-of-type obj rtd))
	(else #f)))


(case-define* condition
  ;;Defined by R6RS.  Return a condition  object with the components of the condition
  ;;arguments  as its  components,  in the  same order.   The  returned condition  is
  ;;compound  if  the  total number  of  components  is  zero  or greater  than  one.
  ;;Otherwise, it may be compound or simple.
  ;;
  (()
   (make-compound-condition '()))
  (({x condition?})
   x)
  (x*
   (let ((ls (let recur ((x* x*))
	       (if (pair? x*)
		   (cond ((simple-condition? (car x*))
			  (cons (car x*) (recur (cdr x*))))
			 ((compound-condition? (car x*))
			  (append (simple-conditions (car x*)) (recur (cdr x*))))
			 (else
			  (procedure-argument-violation 'condition
			    "expected condition object as argument" (car x*))))
		 '()))))
     (cond ((null? ls)
	    (make-compound-condition '()))
	   ((null? (cdr ls))
	    (car ls))
	   (else
	    (make-compound-condition ls))))))

(define* (simple-conditions x)
  ;;Defined by R6RS.  Return a list of the components of X, in the same order as they
  ;;appeared  in the  construction of  X.  The  returned list  is immutable.   If the
  ;;returned list is modified, the effect on X is unspecified.
  ;;
  ;;NOTE  Because   CONDITION  decomposes  its  arguments   into  simple  conditions,
  ;;SIMPLE-CONDITIONS always returns a ``flattened'' list of simple conditions.
  ;;
  (cond ((compound-condition? x)
	 ($compound-condition-components x))
	((simple-condition? x)
	 (list x))
	(else
	 (procedure-argument-violation __who__
	   "expected condition object as argument"
	   x))))


(define* (condition-predicate {rtd simple-condition-rtd-subtype?})
  ;;Defined  by  R6RS.   RTD  must  be  a record-type  descriptor  of  a  subtype  of
  ;;"&condition".  The  CONDITION-PREDICATE procedure returns a  procedure that takes
  ;;one argument.  This  procedure returns #t if  its argument is a  condition of the
  ;;condition type represented  by RTD, i.e., if  it is either a  simple condition of
  ;;that record type (or  one of its subtypes) or a compound  conditition with such a
  ;;simple condition as one of its components, and #f otherwise.
  ;;
  ($condition-predicate rtd))

(define ($condition-predicate rtd)
  (lambda (X)
    (or (and (compound-condition? X)
	     (let loop ((ls ($compound-condition-components X)))
	       (and (pair? ls)
		    (or ($record-of-type (car ls) rtd)
			(loop (cdr ls))))))
	($record-of-type X rtd))))

(case-define* condition-accessor
  ;;Defined  by  R6RS.   RTD  must  be  a record-type  descriptor  of  a  subtype  of
  ;;"&condition".  PROC  should accept one argument,  a record of the  record type of
  ;;RTD.
  ;;
  ;;The  CONDITION-ACCESSOR  procedure returns  a  procedure  that accepts  a  single
  ;;argument,  which must  be  a condition  of  the type  represented  by RTD.   This
  ;;procedure extracts the  first component of the condition of  the type represented
  ;;by RTD, and returns the result of applying PROC to that component.
  ;;
  (({rtd simple-condition-rtd-subtype?} {proc procedure?})
   ($condition-accessor rtd proc 'anonymous-condition-accessor))
  (({rtd simple-condition-rtd-subtype?} {proc procedure?} {accessor-who (or not symbol?)})
   ($condition-accessor rtd proc accessor-who)))

(define ($condition-accessor rtd proc accessor-who)
  (lambda (X)
    (define (%error)
      (procedure-arguments-consistency-violation accessor-who "not a condition of correct type" X rtd))
    (cond ((compound-condition? X)
	   (let loop ((ls ($compound-condition-components X)))
	     (cond ((pair? ls)
		    (if ($record-of-type (car ls) rtd)
			(proc (car ls))
		      (loop (cdr ls))))
		   (else
		    (%error)))))
	  (($record-of-type X rtd)
	   (proc X))
	  (else
	   (%error)))))


;;;; raising exceptions

(case-define* raise-non-continuable-standard-condition
  ;;NOTE Remember that the order in  which we concatenate simple condition objects in
  ;;compound condition  objects is  important: it  is the order  in which  the simple
  ;;objects  will  be shown  to  the  user, when  an  exception  is raised  and  goes
  ;;uncatched.
  ((who {message string?} {irritants list?})
   (let ((C (condition (make-message-condition message)
		       (make-irritants-condition irritants))))
     (raise (if who
		(if (or (symbol? who)
			(string? who))
		    (condition (make-who-condition who) C)
		  (procedure-signature-argument-violation __who__
		    "invalid value for &who" 1 '(or (symbol? who) (string? who)) who))
	      C))))
  ((who {message string?} {irritants list?} {cnd condition?})
   (let ((C (condition (make-message-condition message)
		       cnd
		       (make-irritants-condition irritants))))
     (raise (if who
		(if (or (symbol? who)
			(string? who))
		    (condition (make-who-condition who) C)
		  (procedure-signature-argument-violation __who__
		    "invalid value for &who" 1 '(or (symbol? who) (string? who)) who))
	      C)))))


;;;; R6RS condition types

(define-core-condition-type &message &condition
  make-message-condition message-condition?
  (message condition-message))

(define-core-condition-type &warning &condition
  make-warning warning?)

(define-core-condition-type &serious &condition
  make-serious-condition serious-condition?)

(define-core-condition-type &error &serious
  make-error error?)

(define-core-condition-type &violation &serious
  make-violation violation?)

(define-core-condition-type &assertion &violation
  make-assertion-violation assertion-violation?)

(define-core-condition-type &irritants &condition
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

(define-core-condition-type &who &condition
  %make-who-condition who-condition?
  (who condition-who))

(define* (make-who-condition {who who-condition-value?})
  (%make-who-condition who))

(define-core-condition-type &non-continuable &violation
  make-non-continuable-violation non-continuable-violation?)

(define-core-condition-type &implementation-restriction &violation
  make-implementation-restriction-violation
  implementation-restriction-violation?)

(define-core-condition-type &lexical &violation
  make-lexical-violation lexical-violation?)

(define-core-condition-type &syntax &violation
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

(define-core-condition-type &undefined &violation
  make-undefined-violation undefined-violation?)

(define-core-condition-type &i/o &error
  make-i/o-error i/o-error?)

(define-core-condition-type &i/o-read &i/o
  make-i/o-read-error i/o-read-error?)

(define-core-condition-type &i/o-write &i/o
  make-i/o-write-error i/o-write-error?)

(define-core-condition-type &i/o-invalid-position &i/o
  make-i/o-invalid-position-error i/o-invalid-position-error?
  (position i/o-error-position))

(define-core-condition-type &i/o-filename &i/o
  make-i/o-filename-error i/o-filename-error?
  (filename i/o-error-filename))

(define-core-condition-type &i/o-file-protection &i/o-filename
  make-i/o-file-protection-error i/o-file-protection-error?)

(define-core-condition-type &i/o-file-is-read-only &i/o-file-protection
  make-i/o-file-is-read-only-error i/o-file-is-read-only-error?)

(define-core-condition-type &i/o-file-already-exists &i/o-filename
  make-i/o-file-already-exists-error i/o-file-already-exists-error?)

(define-core-condition-type &i/o-file-does-not-exist &i/o-filename
  make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?)

(define-core-condition-type &i/o-port &i/o
  make-i/o-port-error i/o-port-error?
  (port i/o-error-port))

(define-core-condition-type &i/o-decoding &i/o-port
  make-i/o-decoding-error i/o-decoding-error?)

(define-core-condition-type &i/o-encoding &i/o-port
  make-i/o-encoding-error i/o-encoding-error?
  (char i/o-encoding-error-char))

(define-core-condition-type &no-infinities &implementation-restriction
  make-no-infinities-violation no-infinities-violation?)

(define-core-condition-type &no-nans &implementation-restriction
  make-no-nans-violation no-nans-violation?)

;;; --------------------------------------------------------------------
;;; Ikarus specific condition types

(define-core-condition-type &interrupted &serious
  make-interrupted-condition interrupted-condition?)

(define-core-condition-type &source-position &condition
  make-source-position-condition source-position-condition?
  (port-id	source-position-port-id)
  (byte		source-position-byte)
  (character	source-position-character)
  (line		source-position-line)
  (column	source-position-column))


;;; Vicare specific condition types

(define-core-condition-type &i/o-eagain
    &i/o
  make-i/o-eagain i/o-eagain-error?)

(define-core-condition-type &errno
    &condition
  make-errno-condition errno-condition?
  (code		condition-errno))

(define-core-condition-type &h_errno
    &condition
  make-h_errno-condition h_errno-condition?
  (code		condition-h_errno))

(define-core-condition-type &i/o-wrong-fasl-header-error
    &i/o
  make-i/o-wrong-fasl-header-error
  i/o-wrong-fasl-header-error?)

;;; --------------------------------------------------------------------

(define-core-condition-type &out-of-memory-error
    &error
  make-out-of-memory-error
  out-of-memory-error?)

(define (%raise-out-of-memory who)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition "failed raw memory allocation")
	      (make-out-of-memory-error))))

;;; --------------------------------------------------------------------

(define-core-condition-type &failed-expression
    &condition
  make-failed-expression-condition
  failed-expression-condition?
  (failed-expression	condition-failed-expression))

;;; --------------------------------------------------------------------

(define-core-condition-type &one-based-return-value-index
    &condition
  make-one-based-return-value-index-condition
  one-based-return-value-index-condition?
  (index	condition-one-based-return-value-index))

;;; --------------------------------------------------------------------

(define-core-condition-type &procedure-precondition-violation
    &assertion
  make-procedure-precondition-violation
  procedure-precondition-violation?)

;;; --------------------------------------------------------------------

(define-core-condition-type &procedure-postcondition-violation
    &assertion
  make-procedure-postcondition-violation
  procedure-postcondition-violation?)

;;; --------------------------------------------------------------------

(define-core-condition-type &procedure-argument-violation
    &procedure-precondition-violation
  make-procedure-argument-violation procedure-argument-violation?)

(define (procedure-argument-violation who message . irritants)
  (raise-non-continuable-standard-condition who
    message irritants (make-procedure-argument-violation)))

;;; --------------------------------------------------------------------

(define-core-condition-type &procedure-signature-argument-violation
    &procedure-argument-violation
  make-procedure-signature-argument-violation procedure-signature-argument-violation?
  ;;One-base index of the offending operand.
  (one-based-argument-index	procedure-signature-argument-violation.one-based-argument-index)
  ;;Symbolic expression representing the predicate used to validate the operand.
  (failed-expression		procedure-signature-argument-violation.failed-expression)
  ;;The actual operand.
  (offending-value		procedure-signature-argument-violation.offending-value))

(define (procedure-signature-argument-violation who message operand-index failed-expression offending-value)
  (raise-non-continuable-standard-condition who
    message (list offending-value)
    (make-procedure-signature-argument-violation operand-index failed-expression offending-value)))

;;; --------------------------------------------------------------------

(define-core-condition-type &procedure-signature-return-value-violation
    &procedure-postcondition-violation
  make-procedure-signature-return-value-violation procedure-signature-return-value-violation?
  ;;One-base index of the  offending return value in the tuple  of values returned by
  ;;the expression.
  (one-based-return-value-index	procedure-signature-return-value-violation.one-based-return-value-index)
  ;;Symbolic expression representing the predicate used to validate the return value.
  (failed-expression		procedure-signature-return-value-violation.failed-expression)
  ;;The actual value returned by the expression.
  (offending-value		procedure-signature-return-value-violation.offending-value))

(define (procedure-signature-return-value-violation who message retval-index failed-expression offending-value)
  (raise-non-continuable-standard-condition who
    message (list offending-value)
    (make-procedure-signature-return-value-violation retval-index failed-expression offending-value)))

;;; --------------------------------------------------------------------

(define-core-condition-type &procedure-arguments-consistency-violation
    &procedure-precondition-violation
  make-procedure-arguments-consistency-violation
  procedure-arguments-consistency-violation?)

(define (procedure-arguments-consistency-violation who message . irritants)
  (raise-non-continuable-standard-condition who
    message irritants
    (make-procedure-arguments-consistency-violation)))

(define (procedure-arguments-consistency-violation/failed-expression who message failed-expression . irritants)
  (raise-non-continuable-standard-condition who
    message irritants
    (condition (make-procedure-arguments-consistency-violation)
	       (make-failed-expression-condition failed-expression))))

;;; --------------------------------------------------------------------

(define-core-condition-type &expression-return-value-violation
    &assertion
  make-expression-return-value-violation expression-return-value-violation?)

(define* (expression-return-value-violation {who who-condition-value?} message idx . irritants)
  (raise
   ;;We want "&expression-return-value-violation" to be the first component.
   (condition (make-expression-return-value-violation)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants)
	      (make-one-based-return-value-index-condition idx))))

;;; --------------------------------------------------------------------

(define-core-condition-type &non-reinstatable
    &violation
  make-non-reinstatable-violation
  non-reinstatable-violation?)

(define (non-reinstatable-violation who message . irritants)
  (raise-non-continuable-standard-condition who
    message irritants (make-non-reinstatable-violation)))

;;; --------------------------------------------------------------------

(define-core-condition-type &late-binding-error
    &error
  make-late-binding-error late-binding-error?)

(define-core-condition-type &method-late-binding-error
    &late-binding-error
  make-method-late-binding-error method-late-binding-error?)

(define-core-condition-type &overloaded-function-late-binding-error
    &late-binding-error
  make-overloaded-function-late-binding-error overloaded-function-late-binding-error?
  (overloaded-function-descriptor	overloaded-function-late-binding-error.overloaded-function-descriptor))

(define-core-condition-type &interface-method-late-binding-error
    &method-late-binding-error
  make-interface-method-late-binding-error
  interface-method-late-binding-error?
  (interface-uid	interface-method-late-binding-error.interface-uid)
  (method-name		interface-method-late-binding-error.method-name)
  (subject		interface-method-late-binding-error.subject)
  (descriptor		interface-method-late-binding-error.type-descriptor))


;;; Vicare specific condition types: string encoding and decoding

(define-core-condition-type &string-encoding		&error	make-string-encoding-error	      string-encoding-error?)
(define-core-condition-type &string-decoding		&error	make-string-decoding-error	      string-decoding-error?)

(define-core-condition-type &utf8-string-encoding	&error	make-utf8-string-encoding-error	      utf8-string-encoding-error?)
(define-core-condition-type &utf16-string-encoding	&error	make-utf16-string-encoding-error      utf16-string-encoding-error?)
(define-core-condition-type &utf32-string-encoding	&error	make-utf32-string-encoding-error      utf32-string-encoding-error?)

(define-core-condition-type &utf8-string-decoding	&error	make-utf8-string-decoding-error	      utf8-string-decoding-error?)
(define-core-condition-type &utf16-string-decoding	&error	make-utf16-string-decoding-error      utf16-string-decoding-error?)
(define-core-condition-type &utf32-string-decoding	&error	make-utf32-string-decoding-error      utf32-string-decoding-error?)

;;; --------------------------------------------------------------------
;;; UTF-8 encoding errors, used by string->utf16

;;; --------------------------------------------------------------------
;;; UTF-8 decoding errors, used by utf16->string

(define-core-condition-type &utf8-string-decoding-invalid-octet
    &utf8-string-decoding
  %make-utf8-string-decoding-invalid-octet
  utf8-string-decoding-invalid-octet?
  (bytevector	utf8-string-decoding-invalid-octet.bytevector)
  (index	utf8-string-decoding-invalid-octet.index)
  (octets	utf8-string-decoding-invalid-octet.octets))

(define* (make-utf8-string-decoding-invalid-octet {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-invalid-octet bytevector index octets))

;;; invalid sequences of octets

(define-core-condition-type &utf8-string-decoding-invalid-2-tuple
    &utf8-string-decoding
  %make-utf8-string-decoding-invalid-2-tuple
  utf8-string-decoding-invalid-2-tuple?
  (bytevector	utf8-string-decoding-invalid-2-tuple.bytevector)
  (index	utf8-string-decoding-invalid-2-tuple.index)
  (octets	utf8-string-decoding-invalid-2-tuple.octets))

(define* (make-utf8-string-decoding-invalid-2-tuple {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-invalid-2-tuple bytevector index octets))

(define-core-condition-type &utf8-string-decoding-invalid-3-tuple
    &utf8-string-decoding
  %make-utf8-string-decoding-invalid-3-tuple
  utf8-string-decoding-invalid-3-tuple?
  (bytevector	utf8-string-decoding-invalid-3-tuple.bytevector)
  (index	utf8-string-decoding-invalid-3-tuple.index)
  (octets	utf8-string-decoding-invalid-3-tuple.octets))

(define* (make-utf8-string-decoding-invalid-3-tuple {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-invalid-3-tuple bytevector index octets))

(define-core-condition-type &utf8-string-decoding-invalid-4-tuple
    &utf8-string-decoding
  %make-utf8-string-decoding-invalid-4-tuple
  utf8-string-decoding-invalid-4-tuple?
  (bytevector	utf8-string-decoding-invalid-4-tuple.bytevector)
  (index	utf8-string-decoding-invalid-4-tuple.index)
  (octets	utf8-string-decoding-invalid-4-tuple.octets))

(define* (make-utf8-string-decoding-invalid-4-tuple {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-invalid-4-tuple bytevector index octets))

;;; incomplete sequences of octets

(define-core-condition-type &utf8-string-decoding-incomplete-2-tuple
    &utf8-string-decoding
  %make-utf8-string-decoding-incomplete-2-tuple
  utf8-string-decoding-incomplete-2-tuple?
  (bytevector	utf8-string-decoding-incomplete-2-tuple.bytevector)
  (index	utf8-string-decoding-incomplete-2-tuple.index)
  (octets	utf8-string-decoding-incomplete-2-tuple.octets))

(define* (make-utf8-string-decoding-incomplete-2-tuple {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-incomplete-2-tuple bytevector index octets))

(define-core-condition-type &utf8-string-decoding-incomplete-3-tuple
    &utf8-string-decoding
  %make-utf8-string-decoding-incomplete-3-tuple
  utf8-string-decoding-incomplete-3-tuple?
  (bytevector	utf8-string-decoding-incomplete-3-tuple.bytevector)
  (index	utf8-string-decoding-incomplete-3-tuple.index)
  (octets	utf8-string-decoding-incomplete-3-tuple.octets))

(define* (make-utf8-string-decoding-incomplete-3-tuple {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-incomplete-3-tuple bytevector index octets))

(define-core-condition-type &utf8-string-decoding-incomplete-4-tuple
    &utf8-string-decoding
  %make-utf8-string-decoding-incomplete-4-tuple
  utf8-string-decoding-incomplete-4-tuple?
  (bytevector	utf8-string-decoding-incomplete-4-tuple.bytevector)
  (index	utf8-string-decoding-incomplete-4-tuple.index)
  (octets	utf8-string-decoding-incomplete-4-tuple.octets))

(define* (make-utf8-string-decoding-incomplete-4-tuple {bytevector bytevector?} {index non-negative-fixnum?} {octets list?})
  (%make-utf8-string-decoding-incomplete-4-tuple bytevector index octets))

;;; --------------------------------------------------------------------
;;; UTF-16 encoding errors, used by string->utf16

;;; --------------------------------------------------------------------
;;; UTF-16 decoding errors, used by utf16->string

;;At INDEX of BYTEVECTOR there should be either a standalone 16-bit word or the first
;;16-bit word of a surrogate pair; instead, there is an invalid WORD.
;;
(define-core-condition-type &utf16-string-decoding-invalid-first-word
    &utf16-string-decoding
  %make-utf16-string-decoding-invalid-first-word
  utf16-string-decoding-invalid-first-word?
  (bytevector	utf16-string-decoding-invalid-first-word.bytevector)
  (index	utf16-string-decoding-invalid-first-word.index)
  (word		utf16-string-decoding-invalid-first-word.word))

(define* (make-utf16-string-decoding-invalid-first-word {bytevector bytevector?} {index non-negative-fixnum?} {word fixnum?})
  (%make-utf16-string-decoding-invalid-first-word bytevector index word))

;;At INDEX of BYTEVECTOR there should be  the second 16-bit word of a surrogate pair;
;;instead, there is an invalid WORD.
;;
(define-core-condition-type &utf16-string-decoding-invalid-second-word
    &utf16-string-decoding
  %make-utf16-string-decoding-invalid-second-word
  utf16-string-decoding-invalid-second-word?
  (bytevector	utf16-string-decoding-invalid-second-word.bytevector)
  (index	utf16-string-decoding-invalid-second-word.index)
  (first-word	utf16-string-decoding-invalid-second-word.first-word)
  (second-word	utf16-string-decoding-invalid-second-word.second-word))

(define* (make-utf16-string-decoding-invalid-second-word {bytevector bytevector?} {index non-negative-fixnum?}
							 {first-word fixnum?} {second-word fixnum?})
  (%make-utf16-string-decoding-invalid-second-word bytevector index first-word second-word))

;;At INDEX of BYTEVECTOR there is the first  16-bit WORD of a surrogate pair, but the
;;second word is missing because the first word is at the end of the bytevector.
;;
(define-core-condition-type &utf16-string-decoding-missing-second-word
    &utf16-string-decoding
  %make-utf16-string-decoding-missing-second-word
  utf16-string-decoding-missing-second-word?
  (bytevector	utf16-string-decoding-missing-second-word.bytevector)
  (index	utf16-string-decoding-missing-second-word.index)
  (word		utf16-string-decoding-missing-second-word.word))

(define* (make-utf16-string-decoding-missing-second-word {bytevector bytevector?} {index non-negative-fixnum?} {word fixnum?})
  (%make-utf16-string-decoding-missing-second-word bytevector index word))

;;At the end of  BYTEVECTOR, at INDEX, there is a standalone OCTET  which is not part
;;of a 16-bit word.
;;
(define-core-condition-type &utf16-string-decoding-standalone-octet
    &utf16-string-decoding
  %make-utf16-string-decoding-standalone-octet
  utf16-string-decoding-standalone-octet?
  (bytevector	utf16-string-decoding-standalone-octet.bytevector)
  (index	utf16-string-decoding-standalone-octet.index)
  (octet	utf16-string-decoding-standalone-octet.octet))

(define* (make-utf16-string-decoding-standalone-octet {bytevector bytevector?} {index non-negative-fixnum?} {octet fixnum?})
  (%make-utf16-string-decoding-standalone-octet bytevector index octet))

;;; --------------------------------------------------------------------
;;; UTF-32 encoding errors, used by string->utf32

;;; --------------------------------------------------------------------
;;; UTF-32 decoding errors, used by utf32->string

(define-core-condition-type &utf32-string-decoding-invalid-word
    &utf32-string-decoding
  %make-utf32-string-decoding-invalid-word
  utf32-string-decoding-invalid-word?
  (bytevector	utf32-string-decoding-invalid-word.bytevector)
  (index	utf32-string-decoding-invalid-word.index)
  (word		utf32-string-decoding-invalid-word.word))

(define* (make-utf32-string-decoding-invalid-word {bv bytevector?} {bv.idx non-negative-fixnum?} {word exact-integer?})
  (%make-utf32-string-decoding-invalid-word bv bv.idx word))

(define-core-condition-type &utf32-string-decoding-orphan-octets
    &utf32-string-decoding
  %make-utf32-string-decoding-orphan-octets
  utf32-string-decoding-orphan-octets?
  (bytevector	utf32-string-decoding-orphan-octets.bytevector)
  (index	utf32-string-decoding-orphan-octets.index)
  (octets	utf32-string-decoding-orphan-octets.octets))

(define* (make-utf32-string-decoding-orphan-octets {bv bytevector?} {bv.idx non-negative-fixnum?} {octet* list?})
  (%make-utf32-string-decoding-orphan-octets bv bv.idx octet*))


;;;; printing condition objects

(case-define* print-condition
  ;;Defined by Ikarus.  Print a human readable serialisation of a condition object to
  ;;the given port.
  ;;
  ((x)
   (print-condition x (console-error-port)))
  ((x {port textual-output-port?})
   (if (condition? x)
       (let ((ls (simple-conditions x)))
	 (if (pair? ls)
	     (begin
	       (display " Condition components:\n" port)
	       (let loop ((ls ls) (i 1))
		 (when (pair? ls)
		   (display "   " port)
		   (display i port)
		   (display ". " port)
		   (%print-simple-condition (car ls) port)
		   (loop (cdr ls) (fxadd1 i)))))
	   (display "Condition object with no further information\n" port)))
     (begin
       (display " Non-condition object: " port)
       (write x port)
       (newline port)))))

(define (%print-simple-condition x port)
  (let* ((rtd	(record-rtd x))
	 ;;Association list  having RTDs as keys and  vectors of symbols
	 ;;representing  the  field  names  as values.   Represents  the
	 ;;hierarchy of RTDs from child to parent.
	 ;;
	 (rf	(let loop ((rtd   rtd)
			   (accum '()))
		  (if rtd
		      (loop (record-type-parent rtd)
			    (cons (cons rtd (record-type-field-names rtd))
				  accum))
		    (remp (lambda (a)
			    (zero? (vector-length (cdr a))))
		      accum))))
	 (rf-len (fold-left (lambda (sum pair)
			      (+ sum (vector-length (cdr pair))))
		   0
		   rf)
		 #;(apply + (map vector-length (map cdr rf)))))
    (display (record-type-name rtd) port)
    (case rf-len
      ((0)	;Most condition objects have no fields...
       (newline port))
      ((1)	;... or only one field.
       (display ": " port)
       (pretty-print ((record-accessor (caar rf) 0) x) port)
       #;(write ((record-accessor (caar rf) 0) x) port)
       #;(newline port))
      (else
       (display ":\n" port)
       (for-each
	   (lambda (a)
	     (let loop ((i   0)
			(rtd (car a))
			(v   (cdr a)))
	       (unless (= i (vector-length v))
		 (display "       " port)
		 (display (vector-ref v i) port)
		 (display ": " port)
		 ;;Sometimes WRITE is better than PRETTY-PRINT, but what
		 ;;can I do?  (Marco Maggi; Oct 31, 2012)
		 (pretty-print ((record-accessor rtd i) x)
			       port)
		 ;; (begin
		 ;;   (write ((record-accessor rtd i) x) port)
		 ;;   (newline port))
		 (loop (fxadd1 i) rtd v))))
	 rf)))))


;;;; syntaxes

(define-syntax (preconditions stx)
  (module (vicare-built-with-arguments-validation-enabled)
    (module (arguments-validation)
      (include "ikarus.config.scm" #t))
    (define (vicare-built-with-arguments-validation-enabled)
      arguments-validation)
    #| end of module |# )
  (syntax-case stx ()
    ;;Single precondition.
    ;;
    ((_ (?predicate ?arg ...))
     (identifier? #'?who)
     (if (vicare-built-with-arguments-validation-enabled)
	 #'(unless (?predicate ?arg ...)
	     (procedure-arguments-consistency-violation/failed-expression __who__
	       "failed precondition" '(?predicate ?arg ...) ?arg ...))
       #'(void)))

    ;;Multiple preconditions.
    ;;
    ((_ (?predicate ?arg ...) ...)
     (identifier? #'?who)
     (if (vicare-built-with-arguments-validation-enabled)
	 #'(begin
	     (preconditions (?predicate ?arg ...))
	     ...)
       #'(void)))
    ))


;;;; done

;; (define end-of-file-dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.conditions end")))

#| end of library |# )

;;; end of file
