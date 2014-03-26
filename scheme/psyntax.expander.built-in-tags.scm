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


(define (initialise-type-spec-for-built-in-object-types)
  ;;The tag <top> is special because it is the only one having #f in the
  ;;parent spec field.
  (let ((tag-id  (S <top>))
	(pred-id (S always-true)))
    (set-identifier-object-type-spec! tag-id
      (%make-object-type-spec tag-id #f pred-id #f #f #f #f)))
  (%initialise-some-non-compound-object-types)
  (%initialise-some-compound-object-types)
  (%initialise-numeric-object-types)
  (%initialise-input/output-port-object-types)
  (%initialise-predefined-condition-object-types)
  (void))


(define (%initialise-some-non-compound-object-types)
  (%basic '<procedure>		'<top>		'procedure?)
  (%basic '<boolean>		'<top>		'boolean?)
  (%basic '<char>		'<top>		'char?)
  (%basic '<symbol>		'<top>		'symbol?)
  (%basic '<keyword>		'<top>		'keyword?)
  (%basic '<pointer>		'<top>		'pointer?)
  (%basic '<transcoder>		'<top>		'transcoder?)
  (void))


(define (%initialise-some-compound-object-types)
  (set-identifier-object-type-spec! (S <string>)
    (make-object-type-spec (S <string>) (S <top>) (S string?)))

  (set-identifier-object-type-spec! (S <vector>)
    (make-object-type-spec (S <vector>) (S <top>) (S vector?)))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field-id safe? input-form-stx)
      (case (syntax->datum field-id)
	((car) (values (S car) (S <top>)))
	((cdr) (values (S cdr) (S <top>)))
	(else
	 (syntax-violation '<pair> "unknown field name" input-form-stx field-id))))

    (define (%mutator-maker field-id safe? input-form-stx)
      (case (syntax->datum field-id)
	((car) (values (S set-car!) (S <top>)))
	((cdr) (values (S set-cdr!) (S <top>)))
	(else
	 (syntax-violation '<pair> "unknown field name" input-form-stx field-id))))

    (define (%setter-maker keys-stx input-form-stx)
      (syntax-case keys-stx ()
	(([?field-id])
	 (identifier? #'?field-id)
	 (%mutator-maker #'?field-id #t input-form-stx))
	(else
	 (syntax-violation '<pair>
	   "invalid setter keys syntax" input-form-stx keys-stx))))

    (define (%dispatcher method-id input-form-stx)
      (values #f #f))

    (define type-spec
      (make-object-type-spec (S <pair>) (S <top>) (S pair?)
			     %accessor-maker %mutator-maker %setter-maker %dispatcher))

    (set-identifier-object-type-spec! (S <pair>) type-spec))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <list>)
    (make-object-type-spec (S <list>) (S <pair>) (S list?)))

  (set-identifier-object-type-spec! (S <bytevector>)
    (make-object-type-spec (S <bytevector>) (S <top>) (S bytevector?)))

  (set-identifier-object-type-spec! (S <hashtable>)
    (make-object-type-spec (S <hashtable>) (S <top>) (S hashtable?)))

  (set-identifier-object-type-spec! (S <struct>)
    (make-object-type-spec (S <struct>) (S <top>) (S struct?)))

  (set-identifier-object-type-spec! (S <struct-type-descriptor>)
    (make-object-type-spec (S <struct-type-descriptor>) (S <struct>) (S struct-type-descriptor?)))

  (set-identifier-object-type-spec! (S <record>)
    (make-object-type-spec (S <record>) (S <struct>) (S record?)))

  (set-identifier-object-type-spec! (S <record-type-descriptor>)
    (make-object-type-spec (S <record-type-descriptor>) (S <struct>) (S record-type-descriptor?)))

  (void))


(define (%initialise-numeric-object-types)
  (set-identifier-object-type-spec! (S <number>)
    (make-object-type-spec (S <number>) (S <top>) (S number?)))

  (set-identifier-object-type-spec! (S <complex>)
    (make-object-type-spec (S <complex>) (S <number>) (S complex?)))

  (set-identifier-object-type-spec! (S <real-valued>)
    (make-object-type-spec (S <real-valued>) (S <complex>) (S real-valued?)))

  (set-identifier-object-type-spec! (S <real>)
    (make-object-type-spec (S <real>) (S <real-valued>) (S real?)))

  (set-identifier-object-type-spec! (S <rational-valued>)
    (make-object-type-spec (S <rational-valued>) (S <real>) (S rational-valued?)))

  (set-identifier-object-type-spec! (S <rational>)
    (make-object-type-spec (S <rational>) (S <rational-valued>) (S rational?)))

  (set-identifier-object-type-spec! (S <integer-valued>)
    (make-object-type-spec (S <integer-valued>) (S <rational-valued>) (S integer-valued?)))

  (set-identifier-object-type-spec! (S <integer>)
    (make-object-type-spec (S <integer>) (S <rational-valued>) (S integer?)))

  (set-identifier-object-type-spec! (S <exact-integer>)
    (make-object-type-spec (S <exact-integer>) (S <integer>) (S exact-integer?)))

  (set-identifier-object-type-spec! (S <fixnum>)
    (make-object-type-spec (S <fixnum>) (S <exact-integer>) (S fixnum?)))

  (set-identifier-object-type-spec! (S <bignum>)
    (make-object-type-spec (S <bignum>) (S <exact-integer>) (S bignum?)))

  (set-identifier-object-type-spec! (S <flonum>)
    (make-object-type-spec (S <flonum>) (S <real>) (S flonum?)))

  (set-identifier-object-type-spec! (S <ratnum>)
    (make-object-type-spec (S <ratnum>) (S <rational>) (S ratnum?)))

  (set-identifier-object-type-spec! (S <compnum>)
    (make-object-type-spec (S <compnum>) (S <complex>) (S compnum?)))

  (set-identifier-object-type-spec! (S <cflonum>)
    (make-object-type-spec (S <cflonum>) (S <complex>) (S cflonum?)))

  (void))


(define (%initialise-input/output-port-object-types)
  (set-identifier-object-type-spec! (S <port>)
    (make-object-type-spec (S <port>) (S <top>) (S port?)))

  (set-identifier-object-type-spec! (S <input-port>)
    (make-object-type-spec (S <input-port>) (S <port>) (S input-port?)))

  (set-identifier-object-type-spec! (S <output-port>)
    (make-object-type-spec (S <output-port>) (S <port>) (S output-port?)))

  (set-identifier-object-type-spec! (S <input/output-port>)
    (make-object-type-spec (S <input/output-port>) (S <port>) (S input/output-port?)))

  (set-identifier-object-type-spec! (S <textual-port>)
    (make-object-type-spec (S <textual-port>) (S <port>) (S textual-port?)))

  (set-identifier-object-type-spec! (S <binary-port>)
    (make-object-type-spec (S <binary-port>) (S <port>) (S binary-port?)))

  (set-identifier-object-type-spec! (S <textual-input-port>)
    (make-object-type-spec (S <textual-input-port>) (S <input-port>) (S textual-input-port?)))

  (set-identifier-object-type-spec! (S <textual-output-port>)
    (make-object-type-spec (S <textual-output-port>) (S <output-port>) (S textual-output-port?)))

  (set-identifier-object-type-spec! (S <textual-input/output-port>)
    (make-object-type-spec (S <textual-input/output-port>) (S <input/output-port>) (S textual-input/output-port?)))

  (set-identifier-object-type-spec! (S <binary-input-port>)
    (make-object-type-spec (S <binary-input-port>) (S <input-port>) (S binary-input-port?)))

  (set-identifier-object-type-spec! (S <binary-output-port>)
    (make-object-type-spec (S <binary-output-port>) (S <output-port>) (S binary-output-port?)))

  (set-identifier-object-type-spec! (S <binary-input/output-port>)
    (make-object-type-spec (S <binary-input/output-port>) (S <input/output-port>) (S binary-input/output-port?)))

  (void))


;;;; predefined condition object types

(define (%initialise-predefined-condition-object-types)
  (%basic '&condition			'<record>			'condition?)
  (%basic '&message			'&condition			'message-condition?)
  (%basic '&warning			'&condition			'warning?)
  (%basic '&serious			'&condition			'serious-condition?)
  (%basic '&error			'&serious			'error?)
  (%basic '&violation			'&serious			'violation?)
  (%basic '&assertion			'&violation			'assertion-violation?)
  (%basic '&irritants			'&condition			'irritants-condition?)
  (%basic '&who				'&condition			'who-condition?)
  (%basic '&non-continuable		'&violation			'non-continuable-violation?)
  (%basic '&implementation-restriction	'&violation			'implementation-restriction-violation?)
  (%basic '&lexical			'&violation			'lexical-violation?)
  (%basic '&syntax			'&violation			'syntax-violation?)
  (%basic '&undefined			'&violation			'undefined-violation?)
  (%basic '&i/o				'&error				'i/o-error?)
  (%basic '&i/o-read			'&i/o				'i/o-read-error?)
  (%basic '&i/o-write			'&i/o				'i/o-write-error?)
  (%basic '&i/o-invalid-position	'&i/o				'i/o-invalid-position-error?)
  (%basic '&i/o-filename		'&i/o				'i/o-filename-error?)
  (%basic '&i/o-file-protection		'&i/o-filename			'i/o-file-protection-error?)
  (%basic '&i/o-file-is-read-only	'&i/o-file-protection		'i/o-file-is-read-only-error?)
  (%basic '&i/o-file-already-exists 	'&i/o-filename			'i/o-file-already-exists-error?)
  (%basic '&i/o-file-does-not-exist	'&i/o-filename			'i/o-file-does-not-exist-error?)
  (%basic '&i/o-port			'&i/o				'i/o-port-error?)
  (%basic '&i/o-decoding		'&i/o-port			'i/o-decoding-error?)
  (%basic '&i/o-encoding		'&i/o-port			'i/o-encoding-error?)
  (%basic '&no-infinities		'&implementation-restriction	'no-infinities-violation?)
  (%basic '&no-nans			'&implementation-restriction	'no-nans-violation?)
  (%basic '&interrupted			'&serious			'interrupted-condition?)
  (%basic '&source-position		'&condition			'source-position-condition?)

  (%basic '&i/o-eagain			'&i/o				'i/o-eagain-error?)
  (%basic '&errno			'&condition			'errno-condition?)
  (%basic '&h_errno			'&condition			'h_errno-condition?)
  (%basic '&out-of-memory-error		'&error				'out-of-memory-error?)

  (%basic '&procedure-argument-violation	'&assertion		'procedure-argument-violation?)
  (%basic '&expression-return-value-violation	'&assertion		'expression-return-value-violation?)

  (void))


;;;; helpers and utilities

(define-syntax-rule (S ?sym)
  (scheme-stx (quote ?sym)))

(define (%basic name-sym parent-sym pred-sym)
  ;;Initialise a built-in tag with a basic "object-type-spec".
  ;;
  (let ((tag-id    (scheme-stx name-sym))
	(parent-id (scheme-stx parent-sym))
	(pred-id   (scheme-stx pred-sym)))
    (set-identifier-object-type-spec! tag-id
      (make-object-type-spec tag-id parent-id pred-id))))

(define (top-id)
  <top>)

(define (retvals-signature-of-datum datum)
  (cond ((boolean? datum)	(list (S <boolean>)))
	((char?    datum)	(list (S <char>)))
	((symbol?  datum)	(list (S <symbol>)))
	((keyword? datum)	(list (S <keyword>)))

	((fixnum?  datum)	(list (S <fixnum>)))
	((flonum?  datum)	(list (S <flonum>)))
	((ratnum?  datum)	(list (S <ratnum>)))
	((bignum?  datum)	(list (S <bignum>)))
	((compnum? datum)	(list (S <compnum>)))
	((cflonum? datum)	(list (S <cflonum>)))
	((real?    datum)	(list (S <real>)))
	((complex? datum)	(list (S <complex>)))
	((number?  datum)	(list (S <number>)))

	((string?  datum)	(list (S <string>)))
	((vector?  datum)	(list (S <vector>)))
	((list?    datum)	(list (S <list>)))
	((pair?    datum)	(list (S <pair>)))
	((bytevector? datum)	(list (S <bytevector>)))

	(else			#f)))

;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; End:
