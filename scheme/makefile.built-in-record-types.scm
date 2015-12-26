;;
;;Part of: Vicare Scheme
;;Contents: table of built-in record types and condition object types
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; core syntactic binding descriptors: built-in condition object types

(define-auxiliary-syntaxes methods)

(define-syntax (define-built-in-record-type stx)
  (define (%false-or-id? obj)
    (or (identifier? obj)
	(not (syntax->datum obj))))
  (syntax-case stx (methods)
    ((?kwd ?type-name ?parent-name ?constructor ?predicate)
     (and (identifier? #'?type-name)
	  (%false-or-id? #'?parent-name)
	  (%false-or-id? #'?constructor)
	  (identifier? #'?predicate))
     #'(?kwd ?type-name ?parent-name ?constructor ?predicate (methods)))

    ((_    ?type-name ?parent-name ?constructor ?predicate (methods (?field-name ?accessor-name) ...))
     (and (identifier? #'?type-name)
	  (%false-or-id? #'?parent-name)
	  (%false-or-id? #'?constructor)
	  (identifier? #'?predicate))
     (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
       (define (mkid . str*)
	 (datum->syntax #'?type-name (string->symbol (apply string-append str*))))
       (with-syntax
	   ((TYPE-RTD (mkid type-name.str "-rtd"))
	    (TYPE-RCD (mkid type-name.str "-rcd")))
	 #'(quote (?type-name
		   ($core-record-type-name
		    . (?type-name TYPE-RTD TYPE-RCD ?parent-name ?constructor ?predicate ((?field-name . ?accessor-name) ...))))))))
    ))

;;; --------------------------------------------------------------------

(define-constant VICARE-CORE-BUILT-IN-RECORD-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
  (list

   (define-built-in-record-type <library>
       #f
     make-library library?
     (methods
      (uid			library-uid)
      (name			library-name)
      (imp-lib*			library-imp-lib*)
      (vis-lib*			library-vis-lib*)
      (inv-lib*			library-inv-lib*)
      (export-subst		library-export-subst)
      (global-env		library-global-env)
      (typed-locs		library-typed-locs)
      (visit-state		library-visit-state)
      (invoke-state		library-invoke-state)
      (visit-code		library-visit-code)
      (invoke-code		library-invoke-code)
      (guard-code		library-guard-code)
      (guard-lib*		library-guard-lib*)
      (visible?			library-visible?)
      (source-file-name		library-source-file-name)
      (option*			library-option*)
      (foreign-library*		library-foreign-library*)))

;;; --------------------------------------------------------------------

   (define-built-in-record-type <annotation>
       #f
     get-annotated-datum annotation?
     (methods
      (expression		annotation-expression)
      (stripped			annotation-stripped)
      (source			annotation-source)
      (textual-position		annotation-textual-position)))

;;; --------------------------------------------------------------------

   (define-built-in-record-type <lexical-environment>
       #f
     #f environment?)

   (define-built-in-record-type <interaction-lexical-environment>
       <lexical-environment>
     new-interaction-environment interaction-lexical-environment?)

   (define-built-in-record-type <non-interaction-lexical-environment>
       <lexical-environment>
     environment non-interaction-lexical-environment?)

;;; --------------------------------------------------------------------

   (define-built-in-record-type <list-type-spec>
       #f
     make-list-type-spec list-type-spec?
     (methods
      (type-id			list-type-spec.type-id)))

   (define-built-in-record-type <vector-type-spec>
       #f
     make-vector-type-spec vector-type-spec?
     (methods
      (type-id			vector-type-spec.type-id)))

;;; --------------------------------------------------------------------

   (define-built-in-record-type <type-signature>
       #f
     make-type-signature type-signature?
     (methods
      (tags			type-signature-tags)))

   ))


;;;; core syntactic binding descriptors: built-in condition object types

(define-syntax (define-built-in-condition-type stx)
  (syntax-case stx (methods)
    ((?kwd ?type-name ?parent-name ?constructor ?predicate)
     (and (identifier? #'?type-name)
	  (or (identifier? #'?parent-name)
	      (not (syntax->datum #'?parent-name)))
	  (identifier? #'?constructor)
	  (identifier? #'?predicate))
     #'(?kwd ?type-name ?parent-name ?constructor ?predicate (methods)))
    ((_    ?type-name ?parent-name ?constructor ?predicate (methods (?field-name ?accessor-name) ...))
     (and (identifier? #'?type-name)
	  (or (identifier? #'?parent-name)
	      (not (syntax->datum #'?parent-name)))
	  (identifier? #'?constructor)
	  (identifier? #'?predicate))
     (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
       (define (mkid . str*)
	 (datum->syntax #'?type-name (string->symbol (apply string-append str*))))
       (with-syntax
	   ((TYPE-RTD (mkid type-name.str "-rtd"))
	    (TYPE-RCD (mkid type-name.str "-rcd")))
	 #'(quote (?type-name
		   ($core-condition-object-type-name
		    . (?type-name TYPE-RTD TYPE-RCD ?parent-name ?constructor ?predicate ((?field-name . ?accessor-name) ...))))))))
    ))

;;; --------------------------------------------------------------------

(define-constant VICARE-CORE-BUILT-IN-CONDITION-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
  (list
   (define-built-in-condition-type &condition
       <condition>
     make-simple-condition condition?)

   (define-built-in-condition-type &message
       &condition
     make-message-condition message-condition?
     (methods
      (message		condition-message)))

   (define-built-in-condition-type &warning
       &condition
     make-warning warning?)

   (define-built-in-condition-type &serious
       &condition
     make-serious-condition serious-condition?)

   (define-built-in-condition-type &error
       &serious
     make-error error?)

   (define-built-in-condition-type &violation
       &serious
     make-violation violation?)

   (define-built-in-condition-type &assertion
       &violation
     make-assertion-violation assertion-violation?)

   (define-built-in-condition-type &irritants
       &condition
     make-irritants-condition irritants-condition?
     (methods
      (irritants	condition-irritants)))

   (define-built-in-condition-type &who
       &condition
     make-who-condition who-condition?
     (methods
      (who		condition-who)))

   (define-built-in-condition-type &non-continuable
       &violation
     make-non-continuable-violation non-continuable-violation?)

   (define-built-in-condition-type &implementation-restriction
       &violation
     make-implementation-restriction-violation implementation-restriction-violation?)

   (define-built-in-condition-type &lexical
       &violation
     make-lexical-violation lexical-violation?)

   (define-built-in-condition-type &syntax
       &violation
     make-syntax-violation syntax-violation?
     (methods
      (form		syntax-violation-form)
      (subform		syntax-violation-subform)))

   (define-built-in-condition-type &undefined
       &violation
     make-undefined-violation undefined-violation?)

   (define-built-in-condition-type &i/o
       &error
     make-i/o-error i/o-error?)

   (define-built-in-condition-type &i/o-read
       &i/o make-i/o-read-error i/o-read-error?)

   (define-built-in-condition-type &i/o-write
       &i/o
     make-i/o-write-error i/o-write-error?)

   (define-built-in-condition-type &i/o-invalid-position
       &i/o
     make-i/o-invalid-position-error i/o-invalid-position-error?
     (methods
      (position		i/o-error-position)))


   (define-built-in-condition-type &i/o-filename
       &i/o
     make-i/o-filename-error i/o-filename-error?
     (methods
      (filename		i/o-error-filename)))

   (define-built-in-condition-type &i/o-file-protection
       &i/o-filename
     make-i/o-file-protection-error i/o-file-protection-error?)

   (define-built-in-condition-type &i/o-file-is-read-only
       &i/o-file-protection
     make-i/o-file-is-read-only-error i/o-file-is-read-only-error?)

   (define-built-in-condition-type &i/o-file-already-exists
       &i/o-filename
     make-i/o-file-already-exists-error i/o-file-already-exists-error?)

   (define-built-in-condition-type &i/o-file-does-not-exist
       &i/o-filename
     make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?)

   (define-built-in-condition-type &i/o-port
       &i/o
     make-i/o-port-error i/o-port-error?
     (methods
      (port		i/o-error-port)))

   (define-built-in-condition-type &i/o-decoding
       &i/o-port
     make-i/o-decoding-error i/o-decoding-error?)

   (define-built-in-condition-type &i/o-encoding
       &i/o-port
     make-i/o-encoding-error i/o-encoding-error?
     (methods
      (char		i/o-encoding-error-char)))

   (define-built-in-condition-type &no-infinities
       &implementation-restriction
     make-no-infinities-violation no-infinities-violation?)

   (define-built-in-condition-type &no-nans
       &implementation-restriction
     make-no-nans-violation no-nans-violation?)

;;; --------------------------------------------------------------------
;;; Ikarus specific

   (define-built-in-condition-type &interrupted
       &serious
     make-interrupted-condition interrupted-condition?)

   (define-built-in-condition-type &source-position
       &condition
     make-source-position-condition source-position-condition?
     (methods
      (port-id		source-position-port-id)
      (byte		source-position-byte)
      (character	source-position-character)
      (line		source-position-line)
      (column		source-position-column)))

;;; --------------------------------------------------------------------
;;; Vicare specific

   (define-built-in-condition-type &i/o-eagain
       &i/o
     make-i/o-eagain i/o-eagain-error?)

   (define-built-in-condition-type &errno
       &condition
     make-errno-condition errno-condition?
     (methods
      (code		condition-errno)))

   (define-built-in-condition-type &h_errno
       &condition
     make-h_errno-condition h_errno-condition?
     (methods
      (code		condition-h_errno)))

   (define-built-in-condition-type &out-of-memory-error
       &error
     make-out-of-memory-error out-of-memory-error?)

   (define-built-in-condition-type &failed-expression-condition
       &condition
     make-failed-expression-condition
     failed-expression-condition?
     (methods
      (failed-expression	condition-failed-expression)))

   (define-built-in-condition-type &procedure-precondition-violation
       &assertion
     make-procedure-precondition-violation procedure-precondition-violation?)

   (define-built-in-condition-type &procedure-postcondition-violation
       &assertion
     make-procedure-postcondition-violation procedure-postcondition-violation?)

   (define-built-in-condition-type &procedure-argument-violation
       &procedure-precondition-violation
     make-procedure-argument-violation procedure-argument-violation?)

   (define-built-in-condition-type &procedure-signature-argument-violation
       &procedure-argument-violation
     make-procedure-signature-argument-violation procedure-signature-argument-violation?
     (methods
      (one-based-argument-index	procedure-signature-argument-violation.one-based-argument-index)
      (failed-expression	procedure-signature-argument-violation.failed-expression)
      (offending-value		procedure-signature-argument-violation.offending-value)))

   (define-built-in-condition-type &procedure-signature-return-value-violation
       &procedure-postcondition-violation
     make-procedure-signature-return-value-violation procedure-signature-return-value-violation?
     (methods
      (one-based-return-value-index	procedure-signature-return-value-violation.one-based-return-value-index)
      (failed-expression		procedure-signature-return-value-violation.failed-expression)
      (offending-value			procedure-signature-return-value-violation.offending-value)))

   (define-built-in-condition-type &procedure-arguments-consistency-violation
       &procedure-precondition-violation
     make-procedure-arguments-consistency-violation procedure-arguments-consistency-violation?)

   (define-built-in-condition-type &expression-return-value-violation
       &assertion
     make-expression-return-value-violation expression-return-value-violation?)

   (define-built-in-condition-type &non-reinstatable
       &violation
     make-non-reinstatable-violation non-reinstatable-violation?)
;;;
   (define-built-in-condition-type &string-encoding
       &error
     make-string-encoding-error string-encoding-error?)

   (define-built-in-condition-type &string-decoding
       &error
     make-string-decoding-error string-decoding-error?)

   (define-built-in-condition-type &utf8-string-encoding
       &error
     make-utf8-string-encoding-error utf8-string-encoding-error?)

   (define-built-in-condition-type &utf16-string-encoding
       &error
     make-utf16-string-encoding-error utf16-string-encoding-error?)

   (define-built-in-condition-type &utf32-string-encoding
       &error
     make-utf32-string-encoding-error      utf32-string-encoding-error?)

   (define-built-in-condition-type &utf8-string-decoding
       &error
     make-utf8-string-decoding-error	      utf8-string-decoding-error?)

   (define-built-in-condition-type &utf16-string-decoding
       &error
     make-utf16-string-decoding-error      utf16-string-decoding-error?)

   (define-built-in-condition-type &utf32-string-decoding
       &error
     make-utf32-string-decoding-error      utf32-string-decoding-error?)

   (define-built-in-condition-type &utf8-string-decoding-invalid-octet
       &utf8-string-decoding
     make-utf8-string-decoding-invalid-octet utf8-string-decoding-invalid-octet?
     (methods
      (bytevector	utf8-string-decoding-invalid-octet.bytevector)
      (index		utf8-string-decoding-invalid-octet.index)
      (octets		utf8-string-decoding-invalid-octet.octets)))

   (define-built-in-condition-type &utf8-string-decoding-invalid-2-tuple
       &utf8-string-decoding
     make-utf8-string-decoding-invalid-2-tuple utf8-string-decoding-invalid-2-tuple?
     (methods
      (bytevector	utf8-string-decoding-invalid-2-tuple.bytevector)
      (index		utf8-string-decoding-invalid-2-tuple.index)
      (octets		utf8-string-decoding-invalid-2-tuple.octets)))

   (define-built-in-condition-type &utf8-string-decoding-invalid-3-tuple
       &utf8-string-decoding
     make-utf8-string-decoding-invalid-3-tuple utf8-string-decoding-invalid-3-tuple?
     (methods
      (bytevector	utf8-string-decoding-invalid-3-tuple.bytevector)
      (index		utf8-string-decoding-invalid-3-tuple.index)
      (octets		utf8-string-decoding-invalid-3-tuple.octets)))

   (define-built-in-condition-type &utf8-string-decoding-invalid-4-tuple
       &utf8-string-decoding
     make-utf8-string-decoding-invalid-4-tuple utf8-string-decoding-invalid-4-tuple?
     (methods
      (bytevector	utf8-string-decoding-invalid-4-tuple.bytevector)
      (index		utf8-string-decoding-invalid-4-tuple.index)
      (octets		utf8-string-decoding-invalid-4-tuple.octets)))

   (define-built-in-condition-type &utf8-string-decoding-incomplete-2-tuple
       &utf8-string-decoding
     make-utf8-string-decoding-incomplete-2-tuple utf8-string-decoding-incomplete-2-tuple?
     (methods
      (bytevector	utf8-string-decoding-incomplete-2-tuple.bytevector)
      (index		utf8-string-decoding-incomplete-2-tuple.index)
      (octets		utf8-string-decoding-incomplete-2-tuple.octets)))

   (define-built-in-condition-type &utf8-string-decoding-incomplete-3-tuple
       &utf8-string-decoding
     make-utf8-string-decoding-incomplete-3-tuple utf8-string-decoding-incomplete-3-tuple?
     (methods
      (bytevector	utf8-string-decoding-incomplete-3-tuple.bytevector)
      (index		utf8-string-decoding-incomplete-3-tuple.index)
      (octets		utf8-string-decoding-incomplete-3-tuple.octets)))

   (define-built-in-condition-type &utf8-string-decoding-incomplete-4-tuple
       &utf8-string-decoding
     make-utf8-string-decoding-incomplete-4-tuple	utf8-string-decoding-incomplete-4-tuple?
     (methods
      (bytevector	utf8-string-decoding-incomplete-4-tuple.bytevector)
      (index		utf8-string-decoding-incomplete-4-tuple.index)
      (octets		utf8-string-decoding-incomplete-4-tuple.octets)))

   (define-built-in-condition-type &utf16-string-decoding-invalid-first-word
       &utf16-string-decoding
     make-utf16-string-decoding-invalid-first-word utf16-string-decoding-invalid-first-word?
     (methods
      (bytevector	utf16-string-decoding-invalid-first-word.bytevector)
      (index		utf16-string-decoding-invalid-first-word.index)
      (word		utf16-string-decoding-invalid-first-word.word)))

   (define-built-in-condition-type &utf16-string-decoding-invalid-second-word
       &utf16-string-decoding
     make-utf16-string-decoding-invalid-second-word utf16-string-decoding-invalid-second-word?
     (methods
      (bytevector	utf16-string-decoding-invalid-second-word.bytevector)
      (index		utf16-string-decoding-invalid-second-word.index)
      (first-word	utf16-string-decoding-invalid-second-word.first-word)
      (second-word	utf16-string-decoding-invalid-second-word.second-word)))

   (define-built-in-condition-type &utf16-string-decoding-missing-second-word
       &utf16-string-decoding
     make-utf16-string-decoding-missing-second-word utf16-string-decoding-missing-second-word?
     (methods
      (bytevector	utf16-string-decoding-missing-second-word.bytevector)
      (index		utf16-string-decoding-missing-second-word.index)
      (word		utf16-string-decoding-missing-second-word.word)))

   (define-built-in-condition-type &utf16-string-decoding-standalone-octet
       &utf16-string-decoding
     make-utf16-string-decoding-standalone-octet utf16-string-decoding-standalone-octet?
     (methods
      (bytevector	utf16-string-decoding-standalone-octet.bytevector)
      (index		utf16-string-decoding-standalone-octet.index)
      (octet		utf16-string-decoding-standalone-octet.octet)))

   (define-built-in-condition-type &utf32-string-decoding-invalid-word
       &utf32-string-decoding
     make-utf32-string-decoding-invalid-word utf32-string-decoding-invalid-word?
     (methods
      (bytevector	utf32-string-decoding-invalid-word.bytevector)
      (index		utf32-string-decoding-invalid-word.index)
      (word		utf32-string-decoding-invalid-word.word)))

   (define-built-in-condition-type &utf32-string-decoding-orphan-octets
       &utf32-string-decoding
     make-utf32-string-decoding-orphan-octets utf32-string-decoding-orphan-octets?
     (methods
      (bytevector	utf32-string-decoding-orphan-octets.bytevector)
      (index		utf32-string-decoding-orphan-octets.index)
      (octets		utf32-string-decoding-orphan-octets.octets)))
;;;
   (define-built-in-condition-type &expand-time-type-signature-violation
       &violation
     make-expand-time-type-signature-violation expand-time-type-signature-violation?)

   (define-built-in-condition-type &expand-time-retvals-signature-violation
       &expand-time-type-signature-violation
     make-expand-time-retvals-signature-violation expand-time-retvals-signature-violation?
     (methods
      (expected-signature	expand-time-retvals-signature-violation-expected-signature)
      (returned-signature	expand-time-retvals-signature-violation-returned-signature)))
;;;
   (define-built-in-condition-type &type-syntactic-identifier
       &condition
     make-type-syntactic-identifier-condition type-syntactic-identifier-condition?
     (methods
      (type-identifier		condition-type-syntactic-identifier)))

   (define-built-in-condition-type &argument-type-syntactic-identifier
       &condition
     make-argument-type-syntactic-identifier-condition argument-type-syntactic-identifier-condition?
     (methods
      (argument-type-identifier	condition-operand-type-syntactic-identifier)))

   (define-built-in-condition-type &operand-type-syntactic-identifier
       &condition
     make-operand-type-syntactic-identifier-condition operand-type-syntactic-identifier-condition?
     (methods
      (operand-type-identifier	condition-operand-type-syntactic-identifier)))

   (define-built-in-condition-type &argument-index
       &condition
     make-argument-index-condition argument-index-condition?
     (methods
      (argument-index		condition-argument-index)))

   ))


;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; eval: (put 'define-built-in-condition-type	'scheme-indent-function 2)
;; eval: (put 'define-built-in-record-type 	'scheme-indent-function 2)
;; End:
