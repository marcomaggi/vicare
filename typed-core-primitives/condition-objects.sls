;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for condition objects core primitives
;;Date: Tue Dec 24, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives condition-objects)
  (export typed-core-primitives.condition-objects)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.condition-objects)


;;;; condition objects, safe procedures, generic condition object procedures

(section

(declare-type-predicate condition?		<condition>)
(declare-type-predicate compound-condition?	<compound-condition>)

(declare-core-primitive condition-and-rtd?
    (safe)
  (signatures
   ((<top> <record-type-descriptor>)	=> (<boolean>))))

(declare-core-primitive condition
    (safe)
  (signatures
   ((list-of <condition>)		=> (<compound-condition>)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-simple-condition
    (safe)
  (signatures
   (()					=> (&condition))))

(declare-core-primitive simple-conditions
    (safe)
  (signatures
   ((<condition>)		=> ((list-of &condition))))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive condition-predicate
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> (<procedure>)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive condition-accessor
    (safe)
  (signatures
   ((<record-type-descriptor> <procedure>)		=> (<procedure>))
   ((<record-type-descriptor> <procedure> <symbol>)	=> (<procedure>)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive print-condition
    (safe)
  (signatures
   ((<condition>)				=> (<void>))
   ((<condition> <textual-output-port>)		=> (<void>)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

/section)


;;;; built-in condition objects record-type descriptors

(section

(declare-core-rtd &application-argument-index-rtd)
(declare-core-rtd &application-argument-type-name-rtd)
(declare-core-rtd &application-operand-signature-rtd)
(declare-core-rtd &application-operands-expressions-rtd)
(declare-core-rtd &application-operator-expression-rtd)
(declare-core-rtd &application-operator-signature-rtd)
(declare-core-rtd &assertion-rtd)
(declare-core-rtd &compile-time-arity-error-rtd)
(declare-core-rtd &compile-time-core-type-error-rtd)
(declare-core-rtd &compile-time-error-rtd)
(declare-core-rtd &compile-time-operand-core-type-error-rtd)
(declare-core-rtd &compile-time-retval-core-type-error-rtd)
(declare-core-rtd &compiler-internal-error-rtd)
(declare-core-rtd &condition-rtd)
(declare-core-rtd &errno-rtd)
(declare-core-rtd &error-rtd)
(declare-core-rtd &expand-time-type-signature-violation-rtd)
(declare-core-rtd &expand-time-type-signature-warning-rtd)
(declare-core-rtd &expected-type-signature-rtd)
(declare-core-rtd &expression-return-value-violation-rtd)
(declare-core-rtd &failed-expression-rtd)
(declare-core-rtd &h_errno-rtd)
(declare-core-rtd &i/o-decoding-rtd)
(declare-core-rtd &i/o-eagain-rtd)
(declare-core-rtd &i/o-encoding-rtd)
(declare-core-rtd &i/o-file-already-exists-rtd)
(declare-core-rtd &i/o-file-does-not-exist-rtd)
(declare-core-rtd &i/o-file-is-read-only-rtd)
(declare-core-rtd &i/o-file-protection-rtd)
(declare-core-rtd &i/o-filename-rtd)
(declare-core-rtd &i/o-invalid-position-rtd)
(declare-core-rtd &i/o-port-rtd)
(declare-core-rtd &i/o-read-rtd)
(declare-core-rtd &i/o-rtd)
(declare-core-rtd &i/o-write-rtd)
(declare-core-rtd &implementation-restriction-rtd)
(declare-core-rtd &interrupted-rtd)
(declare-core-rtd &irritants-rtd)
(declare-core-rtd &lexical-rtd)
(declare-core-rtd &message-rtd)
(declare-core-rtd &no-infinities-rtd)
(declare-core-rtd &no-nans-rtd)
(declare-core-rtd &non-continuable-rtd)
(declare-core-rtd &non-reinstatable-rtd)
(declare-core-rtd &object-type-spec-rtd)
(declare-core-rtd &one-based-return-value-index-rtd)
(declare-core-rtd &out-of-memory-error-rtd)
(declare-core-rtd &procedure-argument-violation-rtd)
(declare-core-rtd &procedure-arguments-consistency-violation-rtd)
(declare-core-rtd &procedure-postcondition-violation-rtd)
(declare-core-rtd &procedure-precondition-violation-rtd)
(declare-core-rtd &procedure-signature-argument-violation-rtd)
(declare-core-rtd &procedure-signature-return-value-violation-rtd)
(declare-core-rtd &returned-type-signature-rtd)
(declare-core-rtd &serious-rtd)
(declare-core-rtd &source-position-rtd)
(declare-core-rtd &string-decoding-rtd)
(declare-core-rtd &string-encoding-rtd)
(declare-core-rtd &syntactic-binding-descriptor-rtd)
(declare-core-rtd &syntactic-identifier-not-type-identifier-rtd)
(declare-core-rtd &syntactic-identifier-out-of-context-rtd)
(declare-core-rtd &syntactic-identifier-resolution-rtd)
(declare-core-rtd &syntactic-identifier-rtd)
(declare-core-rtd &syntactic-identifier-unbound-rtd)
(declare-core-rtd &syntax-definition-expanded-rhs-rtd)
(declare-core-rtd &syntax-definition-expression-return-value-rtd)
(declare-core-rtd &syntax-rtd)
(declare-core-rtd &type-signature-rtd)
(declare-core-rtd &undefined-rtd)
(declare-core-rtd &utf16-string-decoding-invalid-first-word-rtd)
(declare-core-rtd &utf16-string-decoding-invalid-second-word-rtd)
(declare-core-rtd &utf16-string-decoding-missing-second-word-rtd)
(declare-core-rtd &utf16-string-decoding-rtd)
(declare-core-rtd &utf16-string-decoding-standalone-octet-rtd)
(declare-core-rtd &utf16-string-encoding-rtd)
(declare-core-rtd &utf32-string-decoding-invalid-word-rtd)
(declare-core-rtd &utf32-string-decoding-orphan-octets-rtd)
(declare-core-rtd &utf32-string-decoding-rtd)
(declare-core-rtd &utf32-string-encoding-rtd)
(declare-core-rtd &utf8-string-decoding-incomplete-2-tuple-rtd)
(declare-core-rtd &utf8-string-decoding-incomplete-3-tuple-rtd)
(declare-core-rtd &utf8-string-decoding-incomplete-4-tuple-rtd)
(declare-core-rtd &utf8-string-decoding-invalid-2-tuple-rtd)
(declare-core-rtd &utf8-string-decoding-invalid-3-tuple-rtd)
(declare-core-rtd &utf8-string-decoding-invalid-4-tuple-rtd)
(declare-core-rtd &utf8-string-decoding-invalid-octet-rtd)
(declare-core-rtd &utf8-string-decoding-rtd)
(declare-core-rtd &utf8-string-encoding-rtd)
(declare-core-rtd &violation-rtd)
(declare-core-rtd &warning-rtd)
(declare-core-rtd &who-rtd)

/section)


;;;; built-in condition objects record-constructor descriptors

(section

(declare-core-rcd &application-argument-index-rcd)
(declare-core-rcd &application-argument-type-name-rcd)
(declare-core-rcd &application-operand-signature-rcd)
(declare-core-rcd &application-operands-expressions-rcd)
(declare-core-rcd &application-operator-expression-rcd)
(declare-core-rcd &application-operator-signature-rcd)
(declare-core-rcd &assertion-rcd)
(declare-core-rcd &compile-time-arity-error-rcd)
(declare-core-rcd &compile-time-core-type-error-rcd)
(declare-core-rcd &compile-time-error-rcd)
(declare-core-rcd &compile-time-operand-core-type-error-rcd)
(declare-core-rcd &compile-time-retval-core-type-error-rcd)
(declare-core-rcd &compiler-internal-error-rcd)
(declare-core-rcd &condition-rcd)
(declare-core-rcd &errno-rcd)
(declare-core-rcd &error-rcd)
(declare-core-rcd &expand-time-type-signature-violation-rcd)
(declare-core-rcd &expand-time-type-signature-warning-rcd)
(declare-core-rcd &expected-type-signature-rcd)
(declare-core-rcd &expression-return-value-violation-rcd)
(declare-core-rcd &failed-expression-rcd)
(declare-core-rcd &h_errno-rcd)
(declare-core-rcd &i/o-decoding-rcd)
(declare-core-rcd &i/o-eagain-rcd)
(declare-core-rcd &i/o-encoding-rcd)
(declare-core-rcd &i/o-file-already-exists-rcd)
(declare-core-rcd &i/o-file-does-not-exist-rcd)
(declare-core-rcd &i/o-file-is-read-only-rcd)
(declare-core-rcd &i/o-file-protection-rcd)
(declare-core-rcd &i/o-filename-rcd)
(declare-core-rcd &i/o-invalid-position-rcd)
(declare-core-rcd &i/o-port-rcd)
(declare-core-rcd &i/o-read-rcd)
(declare-core-rcd &i/o-rcd)
(declare-core-rcd &i/o-write-rcd)
(declare-core-rcd &implementation-restriction-rcd)
(declare-core-rcd &interrupted-rcd)
(declare-core-rcd &irritants-rcd)
(declare-core-rcd &lexical-rcd)
(declare-core-rcd &message-rcd)
(declare-core-rcd &no-infinities-rcd)
(declare-core-rcd &no-nans-rcd)
(declare-core-rcd &non-continuable-rcd)
(declare-core-rcd &non-reinstatable-rcd)
(declare-core-rcd &object-type-spec-rcd)
(declare-core-rcd &one-based-return-value-index-rcd)
(declare-core-rcd &out-of-memory-error-rcd)
(declare-core-rcd &procedure-argument-violation-rcd)
(declare-core-rcd &procedure-arguments-consistency-violation-rcd)
(declare-core-rcd &procedure-postcondition-violation-rcd)
(declare-core-rcd &procedure-precondition-violation-rcd)
(declare-core-rcd &procedure-signature-argument-violation-rcd)
(declare-core-rcd &procedure-signature-return-value-violation-rcd)
(declare-core-rcd &returned-type-signature-rcd)
(declare-core-rcd &serious-rcd)
(declare-core-rcd &source-position-rcd)
(declare-core-rcd &string-decoding-rcd)
(declare-core-rcd &string-encoding-rcd)
(declare-core-rcd &syntactic-binding-descriptor-rcd)
(declare-core-rcd &syntactic-identifier-not-type-identifier-rcd)
(declare-core-rcd &syntactic-identifier-out-of-context-rcd)
(declare-core-rcd &syntactic-identifier-resolution-rcd)
(declare-core-rcd &syntactic-identifier-rcd)
(declare-core-rcd &syntactic-identifier-unbound-rcd)
(declare-core-rcd &syntax-definition-expanded-rhs-rcd)
(declare-core-rcd &syntax-definition-expression-return-value-rcd)
(declare-core-rcd &syntax-rcd)
(declare-core-rcd &type-signature-rcd)
(declare-core-rcd &undefined-rcd)
(declare-core-rcd &utf16-string-decoding-invalid-first-word-rcd)
(declare-core-rcd &utf16-string-decoding-invalid-second-word-rcd)
(declare-core-rcd &utf16-string-decoding-missing-second-word-rcd)
(declare-core-rcd &utf16-string-decoding-rcd)
(declare-core-rcd &utf16-string-decoding-standalone-octet-rcd)
(declare-core-rcd &utf16-string-encoding-rcd)
(declare-core-rcd &utf32-string-decoding-invalid-word-rcd)
(declare-core-rcd &utf32-string-decoding-orphan-octets-rcd)
(declare-core-rcd &utf32-string-decoding-rcd)
(declare-core-rcd &utf32-string-encoding-rcd)
(declare-core-rcd &utf8-string-decoding-incomplete-2-tuple-rcd)
(declare-core-rcd &utf8-string-decoding-incomplete-3-tuple-rcd)
(declare-core-rcd &utf8-string-decoding-incomplete-4-tuple-rcd)
(declare-core-rcd &utf8-string-decoding-invalid-2-tuple-rcd)
(declare-core-rcd &utf8-string-decoding-invalid-3-tuple-rcd)
(declare-core-rcd &utf8-string-decoding-invalid-4-tuple-rcd)
(declare-core-rcd &utf8-string-decoding-invalid-octet-rcd)
(declare-core-rcd &utf8-string-decoding-rcd)
(declare-core-rcd &utf8-string-encoding-rcd)
(declare-core-rcd &violation-rcd)
(declare-core-rcd &warning-rcd)
(declare-core-rcd &who-rcd)

/section)


;;;; built-in condition objects makers

(section

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    (()			=> (?type)))
		   (attributes
		    (()			effect-free result-true))))
		)))
  (declare make-assertion-violation			&assertion)
  (declare make-error					&error)
  (declare make-expression-return-value-violation	&expression-return-value-violation)
  (declare make-i/o-eagain				&i/o-eagain)
  (declare make-i/o-error				&i/o)
  (declare make-i/o-read-error				&i/o-read)
  (declare make-i/o-write-error				&i/o-write)
  (declare make-implementation-restriction-violation	&implementation-restriction)
  (declare make-interrupted-condition			&interrupted)
  (declare make-lexical-violation			&lexical)
  (declare make-no-infinities-violation			&no-infinities)
  (declare make-no-nans-violation			&no-nans)
  (declare make-non-continuable-violation		&non-continuable)
  (declare make-procedure-argument-violation		&procedure-argument-violation)
  (declare make-serious-condition			&serious)
  (declare make-undefined-violation			&undefined)
  (declare make-violation				&violation)
  (declare make-warning					&warning)
  (declare make-non-reinstatable-violation		&non-reinstatable)
  #| end of LET-SYNTAX |# )

(declare-core-primitive make-who-condition
    (safe)
  (signatures
   ((<&who-value>)		=> (&who)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-message-condition
    (safe)
  (signatures
   ((<string>)		=> (&message)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-irritants-condition
    (safe)
  (signatures
   ((list-of <top>)	=> (&irritants)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-syntax-violation
    (safe)
  (signatures
   ((_ _)		=> (&syntax)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive make-errno-condition
    (safe)
  (signatures
   ((<fixnum>)		=> (&errno)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-h_errno-condition
    (safe)
  (signatures
   ((<fixnum>)		=> (&h_errno)))
  (attributes
   ((_)			effect-free result-true)))


(declare-core-primitive make-i/o-port-error
    (safe)
  (signatures
   ((<port>)		=> (&i/o-port)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-invalid-position-error
    (safe)
  (signatures
   ((<top>)		=> (&i/o-invalid-position)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-filename-error
    (safe)
  (signatures
   ((<string>)		=> (&i/o-filename)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-protection-error
    (safe)
  (signatures
   ((<string>)		=> (&i/o-file-protection)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-decoding-error
    (safe)
  (signatures
   ((<port>)		=> (&i/o-decoding)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-encoding-error
    (safe)
  (signatures
   ((<port> <char>)	=> (&i/o-encoding)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-already-exists-error
    (safe)
  (signatures
   ((<string>)		=> (&i/o-file-already-exists)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-does-not-exist-error
    (safe)
  (signatures
   ((<string>)		=> (&i/o-file-does-not-exist)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-is-read-only-error
    (safe)
  (signatures
   ((<string>)		=> (&i/o-file-is-read-only)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-source-position-condition
    (safe)
  (signatures
   ((<string> <exact-integer> <exact-integer> <exact-integer> <exact-integer>)
    => (&source-position)))
  (attributes
   ((_ _ _ _ _)			effect-free result-true)))

(declare-core-primitive make-procedure-signature-argument-violation
    (safe)
  (signatures
   ((<positive-fixnum> <top> <top>)		=> (&procedure-signature-argument-violation))))

(declare-core-primitive make-procedure-signature-return-value-violation
    (safe)
  (signatures
   ((<positive-fixnum> <top> <top>)		=> (&procedure-signature-return-value-violation))))

(declare-core-primitive make-procedure-arguments-consistency-violation
    (safe)
  (signatures
   (()						=> (&procedure-arguments-consistency-violation))))

(declare-core-primitive make-failed-expression-condition
    (safe)
  (signatures
   ((<top>)					=> (&failed-expression))))

;;;

(declare-core-primitive make-one-based-return-value-index-condition
    (safe)
  (signatures
   ((<non-negative-fixnum>)			=> (&one-based-return-value-index))))

(declare-core-primitive make-procedure-postcondition-violation
    (safe)
  (signatures
   (()						=> (&procedure-postcondition-violation))))

(declare-core-primitive make-procedure-precondition-violation
    (safe)
  (signatures
   (()						=> (&procedure-precondition-violation))))

(declare-core-primitive make-string-decoding-error
    (safe)
  (signatures
   (()						=> (&string-decoding))))

(declare-core-primitive make-string-encoding-error
    (safe)
  (signatures
   (()						=> (&string-encoding))))

(declare-core-primitive make-utf16-string-decoding-error
    (safe)
  (signatures
   (()						=> (&utf16-string-decoding))))

(declare-core-primitive make-utf16-string-decoding-invalid-first-word
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <fixnum>)	=> (&utf16-string-decoding-invalid-first-word))))

(declare-core-primitive make-utf16-string-decoding-invalid-second-word
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <fixnum> <fixnum>)	=> (&utf16-string-decoding-invalid-second-word))))

(declare-core-primitive make-utf16-string-decoding-missing-second-word
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <fixnum>)	=> (&utf16-string-decoding-missing-second-word))))

(declare-core-primitive make-utf16-string-decoding-standalone-octet
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (&utf16-string-decoding-standalone-octet))))

(declare-core-primitive make-utf16-string-encoding-error
    (safe)
  (signatures
   (()						=> (&utf16-string-encoding))))

(declare-core-primitive make-utf32-string-decoding-error
    (safe)
  (signatures
   (()						=> (&utf32-string-decoding))))

(declare-core-primitive make-utf32-string-decoding-invalid-word
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <exact-integer>)	=> (&utf32-string-decoding-invalid-word))))

(declare-core-primitive make-utf32-string-decoding-orphan-octets
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <list>)			=> (&utf32-string-decoding-orphan-octets))))

(declare-core-primitive make-utf32-string-encoding-error
    (safe)
  (signatures
   (()							=> (&utf32-string-encoding))))

(declare-core-primitive make-utf8-string-decoding-error
    (safe)
  (signatures
   (()						=> (&utf8-string-decoding))))

(declare-core-primitive make-utf8-string-decoding-incomplete-2-tuple
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-incomplete-2-tuple))))

(declare-core-primitive make-utf8-string-decoding-incomplete-3-tuple
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-incomplete-3-tuple))))

(declare-core-primitive make-utf8-string-decoding-incomplete-4-tuple
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-incomplete-4-tuple))))

(declare-core-primitive make-utf8-string-decoding-invalid-2-tuple
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-invalid-2-tuple))))

(declare-core-primitive make-utf8-string-decoding-invalid-3-tuple
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-invalid-3-tuple))))

(declare-core-primitive make-utf8-string-decoding-invalid-4-tuple
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-invalid-4-tuple))))

(declare-core-primitive make-utf8-string-decoding-invalid-octet
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> (list-of <non-negative-fixnum>))	=> (&utf8-string-decoding-invalid-octet))))

(declare-core-primitive make-utf8-string-encoding-error
    (safe)
  (signatures
   (()						=> (&utf8-string-encoding))))

/section)


;;;; built-in condition objects predicates

(section

(declare-condition-type-predicate assertion-violation?			&assertion)
(declare-condition-type-predicate errno-condition?			&errno)
(declare-condition-type-predicate error?				&error)
(declare-condition-type-predicate expression-return-value-violation?	&expression-return-value-violation)
(declare-condition-type-predicate h_errno-condition?			&h_errno)
(declare-condition-type-predicate i/o-decoding-error?			&i/o-decoding)
(declare-condition-type-predicate i/o-eagain-error?			&i/o-eagain)
(declare-condition-type-predicate i/o-encoding-error?			&i/o-encoding)
(declare-condition-type-predicate i/o-error?				&i/o)
(declare-condition-type-predicate i/o-file-already-exists-error?	&i/o-file-already-exists)
(declare-condition-type-predicate i/o-file-does-not-exist-error?	&i/o-file-does-not-exist)
(declare-condition-type-predicate i/o-file-is-read-only-error?		&i/o-file-is-read-only)
(declare-condition-type-predicate i/o-file-protection-error?		&i/o-file-protection)
(declare-condition-type-predicate i/o-filename-error?			&i/o-filename)
(declare-condition-type-predicate i/o-invalid-position-error?		&i/o-invalid-position)
(declare-condition-type-predicate i/o-port-error?			&i/o-port)
(declare-condition-type-predicate i/o-read-error?			&i/o-read)
(declare-condition-type-predicate i/o-write-error?			&i/o-write)
(declare-condition-type-predicate implementation-restriction-violation?	&implementation-restriction)
(declare-condition-type-predicate interrupted-condition?		&interrupted)
(declare-condition-type-predicate irritants-condition?			&irritants)
(declare-condition-type-predicate lexical-violation?			&lexical)
(declare-condition-type-predicate message-condition?			&message)
(declare-condition-type-predicate no-infinities-violation?		&no-infinities)
(declare-condition-type-predicate no-nans-violation?			&no-nans)
(declare-condition-type-predicate non-continuable-violation?		&non-continuable)
(declare-condition-type-predicate procedure-argument-violation?		&procedure-argument-violation)
(declare-condition-type-predicate serious-condition?			&serious)
(declare-condition-type-predicate source-position-condition?		&source-position)
(declare-condition-type-predicate syntax-violation?			&syntax)
(declare-condition-type-predicate undefined-violation?			&undefined)
(declare-condition-type-predicate violation?				&violation)
(declare-condition-type-predicate warning?				&warning)
(declare-condition-type-predicate who-condition?			&who)
(declare-condition-type-predicate non-reinstatable-violation?		&non-reinstatable)
(declare-condition-type-predicate failed-expression-condition?		&failed-expression)

(declare-condition-type-predicate procedure-signature-argument-violation?	&procedure-signature-argument-violation)
(declare-condition-type-predicate procedure-signature-return-value-violation?	&procedure-signature-return-value-violation)
(declare-condition-type-predicate procedure-arguments-consistency-violation?	&procedure-arguments-consistency-violation)

(declare-condition-type-predicate one-based-return-value-index-condition?	&one-based-return-value-index)
(declare-condition-type-predicate procedure-postcondition-violation?		&procedure-postcondition-violation)
(declare-condition-type-predicate procedure-precondition-violation?		&procedure-precondition-violation)
(declare-condition-type-predicate string-decoding-error?			&string-decoding)
(declare-condition-type-predicate string-encoding-error?			&string-encoding)
(declare-condition-type-predicate utf16-string-decoding-error?			&utf16-string-decoding)
(declare-condition-type-predicate utf16-string-decoding-invalid-first-word?	&utf16-string-decoding-invalid-first-word)
(declare-condition-type-predicate utf16-string-decoding-invalid-second-word?	&utf16-string-decoding-invalid-second-word)
(declare-condition-type-predicate utf16-string-decoding-missing-second-word?	&utf16-string-decoding-missing-second-word)
(declare-condition-type-predicate utf16-string-decoding-standalone-octet?	&utf16-string-decoding-standalone-octet)
(declare-condition-type-predicate utf16-string-encoding-error?			&utf16-string-encoding)
(declare-condition-type-predicate utf32-string-decoding-error?			&utf32-string-decoding)
(declare-condition-type-predicate utf32-string-decoding-invalid-word?		&utf32-string-decoding-invalid-word)
(declare-condition-type-predicate utf32-string-decoding-orphan-octets?		&utf32-string-decoding-orphan-octets)
(declare-condition-type-predicate utf32-string-encoding-error?			&utf32-string-encoding)
(declare-condition-type-predicate utf8-string-decoding-error?			&utf8-string-decoding)
(declare-condition-type-predicate utf8-string-decoding-incomplete-2-tuple?	&utf8-string-decoding-incomplete-2-tuple)
(declare-condition-type-predicate utf8-string-decoding-incomplete-3-tuple?	&utf8-string-decoding-incomplete-3-tuple)
(declare-condition-type-predicate utf8-string-decoding-incomplete-4-tuple?	&utf8-string-decoding-incomplete-4-tuple)
(declare-condition-type-predicate utf8-string-decoding-invalid-2-tuple?		&utf8-string-decoding-invalid-2-tuple)
(declare-condition-type-predicate utf8-string-decoding-invalid-3-tuple?		&utf8-string-decoding-invalid-3-tuple)
(declare-condition-type-predicate utf8-string-decoding-invalid-4-tuple?		&utf8-string-decoding-invalid-4-tuple)
(declare-condition-type-predicate utf8-string-decoding-invalid-octet?		&utf8-string-decoding-invalid-octet)
(declare-condition-type-predicate utf8-string-encoding-error?			&utf8-string-encoding)

/section)


;;;; built-in condition objects accessors

(section

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who ?type)
		 (declare ?who ?type <top>))
		((_ ?who ?type ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((?type)	=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare condition-who		&who				<&who-value>)
  (declare condition-message		&message			<string>)
  (declare condition-irritants		&irritants			(list-of <top>))

  (declare condition-errno		&errno				<fixnum>)
  (declare condition-h_errno		&h_errno			<fixnum>)
  (declare i/o-encoding-error-char	&i/o-encoding)
  (declare i/o-error-filename		&i/o-filename)
  (declare i/o-error-port		&i/o-port			<port>)
  (declare i/o-error-position		&i/o-invalid-position)
  (declare syntax-violation-form	&syntax)
  (declare syntax-violation-subform	&syntax)

  (declare source-position-port-id	&source-position		<string>)
  (declare source-position-byte		&source-position		<exact-integer>)
  (declare source-position-character	&source-position		<exact-integer>)
  (declare source-position-line		&source-position		<exact-integer>)
  (declare source-position-column	&source-position		<exact-integer>)

  (declare condition-one-based-return-value-index	&one-based-return-value-index	<positive-fixnum>)

  (declare procedure-signature-argument-violation.one-based-argument-index
	   &procedure-signature-argument-violation	      <positive-fixnum>)
  (declare procedure-signature-argument-violation.failed-expression
	   &procedure-signature-argument-violation	      <top>)
  (declare procedure-signature-argument-violation.offending-value
	   &procedure-signature-argument-violation	      <top>)

  (declare procedure-signature-return-value-violation.one-based-return-value-index
	   &procedure-signature-return-value-violation	<positive-fixnum>)
  (declare procedure-signature-return-value-violation.failed-expression
	   &procedure-signature-return-value-violation	<top>)
  (declare procedure-signature-return-value-violation.offending-value
	   &procedure-signature-return-value-violation	<top>)

  (declare condition-failed-expression				&failed-expression	<top>)

  (declare utf8-string-decoding-invalid-octet.bytevector	&utf8-string-decoding-invalid-octet   <bytevector>)
  (declare utf8-string-decoding-invalid-octet.index		&utf8-string-decoding-invalid-octet   <fixnum>)
  (declare utf8-string-decoding-invalid-octet.octets		&utf8-string-decoding-invalid-octet   (list-of <fixnum>))

  (declare utf8-string-decoding-invalid-2-tuple.bytevector	&utf8-string-decoding-invalid-2-tuple <bytevector>)
  (declare utf8-string-decoding-invalid-2-tuple.index		&utf8-string-decoding-invalid-2-tuple <fixnum>)
  (declare utf8-string-decoding-invalid-2-tuple.octets		&utf8-string-decoding-invalid-2-tuple (list-of <fixnum>))

  (declare utf8-string-decoding-invalid-3-tuple.bytevector	&utf8-string-decoding-invalid-3-tuple <bytevector>)
  (declare utf8-string-decoding-invalid-3-tuple.index		&utf8-string-decoding-invalid-3-tuple <fixnum>)
  (declare utf8-string-decoding-invalid-3-tuple.octets		&utf8-string-decoding-invalid-3-tuple (list-of <fixnum>))

  (declare utf8-string-decoding-invalid-4-tuple.bytevector	&utf8-string-decoding-invalid-4-tuple <bytevector>)
  (declare utf8-string-decoding-invalid-4-tuple.index		&utf8-string-decoding-invalid-4-tuple <fixnum>)
  (declare utf8-string-decoding-invalid-4-tuple.octets		&utf8-string-decoding-invalid-4-tuple (list-of <fixnum>))

  (declare utf8-string-decoding-incomplete-2-tuple.bytevector	&utf8-string-decoding-incomplete-2-tuple      <bytevector>)
  (declare utf8-string-decoding-incomplete-2-tuple.index	&utf8-string-decoding-incomplete-2-tuple      <fixnum>)
  (declare utf8-string-decoding-incomplete-2-tuple.octets	&utf8-string-decoding-incomplete-2-tuple      (list-of <fixnum>))

  (declare utf8-string-decoding-incomplete-3-tuple.bytevector	&utf8-string-decoding-incomplete-3-tuple      <bytevector>)
  (declare utf8-string-decoding-incomplete-3-tuple.index	&utf8-string-decoding-incomplete-3-tuple      <fixnum>)
  (declare utf8-string-decoding-incomplete-3-tuple.octets	&utf8-string-decoding-incomplete-3-tuple      (list-of <fixnum>))

  (declare utf8-string-decoding-incomplete-4-tuple.bytevector	&utf8-string-decoding-incomplete-4-tuple      <bytevector>)
  (declare utf8-string-decoding-incomplete-4-tuple.index	&utf8-string-decoding-incomplete-4-tuple      <fixnum>)
  (declare utf8-string-decoding-incomplete-4-tuple.octets	&utf8-string-decoding-incomplete-4-tuple      (list-of <fixnum>))

  (declare utf16-string-decoding-invalid-first-word.bytevector	&utf16-string-decoding-invalid-first-word     <bytevector>)
  (declare utf16-string-decoding-invalid-first-word.index	&utf16-string-decoding-invalid-first-word     <fixnum>)
  (declare utf16-string-decoding-invalid-first-word.word	&utf16-string-decoding-invalid-first-word     <fixnum>)

  (declare utf16-string-decoding-invalid-second-word.bytevector	&utf16-string-decoding-invalid-second-word    <bytevector>)
  (declare utf16-string-decoding-invalid-second-word.index	&utf16-string-decoding-invalid-second-word    <fixnum>)
  (declare utf16-string-decoding-invalid-second-word.first-word	&utf16-string-decoding-invalid-second-word    <fixnum>)
  (declare utf16-string-decoding-invalid-second-word.second-word &utf16-string-decoding-invalid-second-word   <fixnum>)

  (declare utf16-string-decoding-missing-second-word.bytevector	&utf16-string-decoding-missing-second-word    <bytevector>)
  (declare utf16-string-decoding-missing-second-word.index	&utf16-string-decoding-missing-second-word    <fixnum>)
  (declare utf16-string-decoding-missing-second-word.word	&utf16-string-decoding-missing-second-word    <fixnum>)

  (declare utf16-string-decoding-standalone-octet.bytevector	&utf16-string-decoding-standalone-octet	      <bytevector>)
  (declare utf16-string-decoding-standalone-octet.index		&utf16-string-decoding-standalone-octet	      <fixnum>)
  (declare utf16-string-decoding-standalone-octet.octet		&utf16-string-decoding-standalone-octet	      <fixnum>)

  (declare utf32-string-decoding-invalid-word.bytevector	&utf32-string-decoding-invalid-word	      <bytevector>)
  (declare utf32-string-decoding-invalid-word.index		&utf32-string-decoding-invalid-word	      <fixnum>)
  (declare utf32-string-decoding-invalid-word.word		&utf32-string-decoding-invalid-word	      <exact-integer>)

  (declare utf32-string-decoding-orphan-octets.bytevector	&utf32-string-decoding-orphan-octets	      <bytevector>)
  (declare utf32-string-decoding-orphan-octets.index		&utf32-string-decoding-orphan-octets	      <fixnum>)
  (declare utf32-string-decoding-orphan-octets.octets		&utf32-string-decoding-orphan-octets	      (list-of <fixnum>))

  #| end of LET-SYNTAX |# )

/section)


;;;; condition objects, safe procedures, condition object raisers

(section

;;These are not effect-free because raising an exception is a "side effect".

(declare-core-primitive error
    (safe)
  (signatures
   ((<&who-value> <string> . <list>)		=> <no-return>)))

(declare-core-primitive assertion-violation
    (safe)
  (signatures
   ((<&who-value> <string> . <list>)		=> <no-return>)))

(declare-core-primitive syntax-violation
    (safe)
  (signatures
   ((<&who-value> <string> <top>)		=> <no-return>)
   ((<&who-value> <string> <top> <top>)	=> <no-return>)))

(declare-core-primitive warning
    (safe)
  (signatures
   ;;We do not know the number of returned values.
   ((<&who-value> <string> . <list>)		=> <no-return>)))

;;This is deprecated.
(declare-core-primitive die
    (safe)
  (signatures
   ((<&who-value> <string> . <list>)		=> <no-return>)))

(declare-core-primitive procedure-argument-violation
    (safe)
  (signatures
   ((<&who-value> <string> . <list>)		=> <no-return>)))

(declare-core-primitive expression-return-value-violation
    (safe)
  (signatures
   ((<&who-value> <string> <positive-fixnum> . <list>)	=> <no-return>)))

(declare-core-primitive non-reinstatable-violation
    (safe)
  (signatures
   ((<&who-value> <string> . <list>)			=> <no-return>)))

(declare-core-primitive procedure-signature-argument-violation
    (safe)
  (signatures
   ((<&who-value> <string> <positive-fixnum> <top> <top>)	=> (<no-return>))))

(declare-core-primitive assertion-error
    (safe)
  (signatures
   ((<top> <syntax-object>
	   <non-negative-exact-integer> <non-negative-exact-integer>
	   <non-negative-exact-integer> <non-negative-exact-integer>)
    => (<no-return>))))

(declare-core-primitive procedure-arguments-consistency-violation
    (safe)
  (signatures
   ((<&who-value> <string> . <list>)		=> (<no-return>))))

(declare-core-primitive procedure-arguments-consistency-violation/failed-expression
    (safe)
  (signatures
   ((<&who-value> <string> <top> . <list>)	=> (<no-return>))))

(declare-core-primitive procedure-signature-return-value-violation
    (safe)
  (signatures
   ((<&who-value> <string> <non-negative-fixnum> <top> <top>)	=> (<no-return>))))

/section)


;;;; condition objects, unsafe procedures

(section

(declare-core-primitive $condition-predicate
    (unsafe)
  (signatures
   ((<record-type-descriptor>)				=> (<procedure>))))

(declare-core-primitive $condition-accessor
    (unsafe)
  (signatures
   ((<record-type-descriptor> <procedure> <symbol>)	=> (<procedure>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
