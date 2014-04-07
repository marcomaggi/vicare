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

(define-syntax-rule (S ?sym)
  (scheme-stx (quote ?sym)))

(define-syntax (M stx)
  (sys.syntax-case stx ()
    ((_ ?method)
     (sys.with-syntax
	 ((ARGS (datum->syntax (sys.syntax ?method) 'arg*.stx)))
       (sys.syntax (bless
		    (quasiquote
		     (lambda (O)
		       (?method O . (unquote ARGS))))))))
    ))

(define (%basic name-sym parent-sym pred-sym)
  ;;Initialise a built-in tag with a basic "object-type-spec".
  ;;
  (let ((uid       (string->symbol (string-append "vicare:expander:tags:" (symbol->string name-sym))))
	(tag-id    (scheme-stx name-sym))
	(parent-id (scheme-stx parent-sym))
	(pred-id   (scheme-stx pred-sym)))
    (set-identifier-object-type-spec! tag-id
      (make-object-type-spec uid tag-id parent-id pred-id))))


(define (initialise-type-spec-for-built-in-object-types)
  ;;The tag  "<top>" is special because  it is the only  one having #f in  the parent
  ;;spec field.  "<top>" is the default tag for single-value untagged bindings.
  (let ((tag-id   (top-tag-id))
	(pred-id  (S always-true)))
    (set-identifier-object-type-spec! tag-id
      (%make-object-type-spec '(vicare:expander:tags:<top>) tag-id pred-id
			      #f #f  #f #f  #f #f #f)))

  (%basic '<void>		'<top>		'void?)
  (%basic '<procedure>		'<top>		'procedure?)
  (%basic '<boolean>		'<top>		'boolean?)

;;; --------------------------------------------------------------------

  (%initialise-<symbol>)
  (%initialise-<keyword>)
  (%initialise-<pointer>)
  (%initialise-<char>)
  (%initialise-<transcoder>)

  (%initialise-<pair>)
  (%initialise-<string>)

  (%initialise-some-compound-object-types)
  (%initialise-numeric-object-types)
  (%initialise-input/output-port-object-types)
  (%initialise-predefined-condition-object-types)
  (void))


;;;; non-compound built-in tags: <symbol>

(define (%initialise-<symbol>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((string)		(S symbol->string))
      ((hash)		(S symbol-hash))
      ((bound?)		(S symbol-bound?))
      ((value)		(S symbol-value))
      (else
       (syntax-violation '<symbol> "unknown field name" input-form.stx field.sym))))

  (define (%mutator-maker field.sym input-form.stx)
    (case field.sym
      ((value)		(S set-symbol-value!))
      (else
       (syntax-violation '<symbol> "unknown field name" input-form.stx field.sym))))

  (define (%caster-maker source-tag input-form.stx)
    (if source-tag
	(cond ((free-id=? source-tag (S <string>))		(S string->symbol))
	      (else
	       (syntax-violation '<symbol> "invalid cast source object type" input-form.stx source-tag)))
      (S any->symbol)))

  (define (%dispatcher method.sym arg*.stx input-form.stx)
    (case method.sym
      ((putprop)	(M putprop))
      ((getprop)	(M getprop))
      ((remprop)	(M remprop))
      ((property-list)	(M property-list))
      (else #f)))

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<symbol>
			   (S <symbol>) (top-tag-id) (S symbol?)
			   %accessor-maker %mutator-maker
			   #f #f
			   %caster-maker %dispatcher))

  (set-identifier-object-type-spec! (S <symbol>) type-spec))


;;;; non-compound built-in tags: <keyword>

(define (%initialise-<keyword>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((symbol)			(S keyword->symbol))
      ((string)			(S keyword->string))
      ((hash)			(S keyword-hash))
      (else
       (syntax-violation '<keyword> "unknown field name" input-form.stx field.sym))))

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<keyword>
			   (S <keyword>) (top-tag-id) (S keyword?)
			   %accessor-maker #f  #f #f  #f #f))

  (set-identifier-object-type-spec! (S <keyword>) type-spec))


;;;; non-compound built-in tags: <pointer>

(define (%initialise-<pointer>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((null?)			(S pointer-null?))
      ((integer)		(S pointer->integer))
      ((hash)			(S pointer-hash))
      (else
       (syntax-violation '<pointer> "unknown field name" input-form.stx field.sym))))

  (define (%mutator-maker field.sym input-form.stx)
    (case field.sym
      ((set-null!)	(S set-pointer-null!))
      (else
       (syntax-violation '<pointer> "unknown field name" input-form.stx field.sym))))

  (define (%dispatcher method.sym arg*.stx input-form.stx)
    (case method.sym
      ((=)		(M pointer=?))
      ((!=)		(M pointer!=?))
      ((<)		(M pointer<?))
      ((>)		(M pointer>?))
      ((<=)		(M pointer<=?))
      ((>=)		(M pointer>=?))
      ((add)		(M pointer-add))
      ((diff)		(M pointer-diff))
      ((clone)		(M pointer-clone))
      (else		#f)))

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<symbol>
			   (S <pointer>) (top-tag-id) (S pointer?)
			   %accessor-maker %mutator-maker  #f #f  #f #f))

  (set-identifier-object-type-spec! (S <pointer>) type-spec))


;;;; non-compound built-in tags: <char>

(define (%initialise-<char>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((upcase)			(S char-upcase))
      ((downcase)		(S char-downcase))
      ((titlecase)		(S char-titlecase))
      ((foldcase)		(S char-foldcase))

      ((alphabetic?)		(S char-alphabetic?))
      ((numeric?)		(S char-numeric?))
      ((whitespace?)		(S char-whitespace?))
      ((upper-case?)		(S char-upper-case?))
      ((lower-case?)		(S char-lower-case?))
      ((title-case?)		(S char-title-case?))

      ((general-category)	(S char-general-category))

      (else
       (syntax-violation '<char> "unknown field name" input-form.stx field.sym))))

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<char>
			   (S <char>) (top-tag-id) (S char?)
			   %accessor-maker #f  #f #f  #f #f))

  (set-identifier-object-type-spec! (S <char>) type-spec))


;;;; non-compound built-in tags: <transcoder>

(define (%initialise-<transcoder>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((codec)			(S transcoder-codec))
      ((eol-style)		(S transcoder-eol-style))
      ((error-handling-mode)	(S transcoder-error-handling-mode))
      (else
       (syntax-violation '<transcoder> "unknown field name" input-form.stx field.sym))))

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<transcoder>
			   (S <transcoder>) (top-tag-id) (S transcoder?)
			   %accessor-maker #f  #f #f  #f #f))

  (set-identifier-object-type-spec! (S <transcoder>) type-spec))


;;;; compound built-in tags: <pair>

(define (%initialise-<pair>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((car)		(S car))
      ((cdr)		(S cdr))
      (else
       (syntax-violation '<pair> "unknown field name" input-form.stx field.sym))))

  (define (%mutator-maker field.sym input-form.stx)
    (case field.sym
      ((car)		(S set-car!))
      ((cdr)		(S set-cdr!) )
      (else
       (syntax-violation '<pair> "unknown field name" input-form.stx field.sym))))

  (define (%getter-maker keys-stx input-form.stx)
    (syntax-case keys-stx ()
      (([?field.sym])
       (identifier? #'?field.sym)
       (%accessor-maker #'?field.sym input-form.stx))
      (else
       (syntax-violation '<pair>
	 "invalid setter keys syntax" input-form.stx keys-stx))))

  (define (%setter-maker keys-stx input-form.stx)
    (syntax-case keys-stx ()
      (([?field.sym])
       (identifier? #'?field.sym)
       (%mutator-maker #'?field.sym input-form.stx))
      (else
       (syntax-violation '<pair>
	 "invalid setter keys syntax" input-form.stx keys-stx))))

  (define (%dispatcher method.sym arg*.stx input-form.stx)
    #f)

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<pair>
			   (S <pair>) (top-tag-id) (S pair?)
			   %accessor-maker %mutator-maker
			   %getter-maker %setter-maker
			   #f %dispatcher))

  (set-identifier-object-type-spec! (S <pair>) type-spec))


;;;; compound built-in tags: <string>

(define (%initialise-<string>)
  (define (%accessor-maker field.sym input-form.stx)
    (case field.sym
      ((length)			(S string-length))
      ((upcase)			(S string-upcase))
      ((downcase)		(S string-downcase))
      ((titlecase)		(S string-titlecase))
      ((foldcase)		(S string-foldcase))

      ((ascii)			(S string->ascii))
      ((latin1)			(S string->latin1))
      ((utf8)			(S string->utf8))
      ((utf16)			(S string->utf16))
      ((utf16le)		(S string->utf16le))
      ((utf16be)		(S string->utf16be))
      ((utf16n)			(S string->utf16n))
      ((utf32)			(S string->utf32))
      ((percent-encoding)	(S string->uri-encoding))

      (else
       (syntax-violation '<string> "unknown field name" input-form.stx field.sym))))

  (define (%mutator-maker field.sym input-form.stx)
    (case field.sym

      (else
       (syntax-violation '<string> "unknown field name" input-form.stx field.sym))))

  (define (%getter-maker keys-stx input-form.stx)
    (syntax-match keys-stx ()
      (([?char-index])
       (bless
	`(lambda ({_ <char>} {str <string>})
	   (string-ref str ,?char-index))))
      (else
       (syntax-violation '<string>
	 "invalid getter keys syntax" input-form.stx keys-stx))))

  (define (%setter-maker keys-stx input-form.stx)
    (syntax-match keys-stx ()
      (([?char-index])
       (bless
	`(lambda (str new-value)
	   (string-set! str ,?char-index new-value))))
      (else
       (syntax-violation '<string>
	 "invalid setter keys syntax" input-form.stx keys-stx))))

  (define (%dispatcher method.sym arg*.stx input-form.stx)
    (case method.sym
      ((hash)			(M string-hash))
      ((substring)		(M substring))
      ((append)			(M string-append))
      ((list)			(M string->list))
      ((for-each)		(M string-for-each))
      ((copy)			(M string-copy))
      ((=)			(M string=?))
      (else			#f)))

  (define (%caster-maker source-tag input-form.stx)
    (if source-tag
	(cond ((free-id=? source-tag (S <symbol>))
	       (S symbol->string))
	      ((or (free-id=? source-tag (S <fixnum>))
		   (free-id=? source-tag (S <flonum>))
		   (free-id=? source-tag (S <ratnum>))
		   (free-id=? source-tag (S <bignum>))
		   (free-id=? source-tag (S <compnum>))
		   (free-id=? source-tag (S <cflonum>))
		   (free-id=? source-tag (S <exact-integer>))
		   (free-id=? source-tag (S <integer-valued>))
		   (free-id=? source-tag (S <integer>))
		   (free-id=? source-tag (S <rational-valued>))
		   (free-id=? source-tag (S <rational>))
		   (free-id=? source-tag (S <real-valued>))
		   (free-id=? source-tag (S <real>))
		   (free-id=? source-tag (S <complex>))
		   (free-id=? source-tag (S <number>)))
	       (S number->string))
	      (else
	       (syntax-violation '<string> "invalid cast source object type" input-form.stx source-tag)))
      (S any->string)))

  (define type-spec
    (make-object-type-spec 'vicare:expander:tags:<string>
			   (S <string>) (top-tag-id) (S string?)
			   %accessor-maker %mutator-maker
			   %getter-maker %setter-maker
			   %caster-maker %dispatcher))

  (set-identifier-object-type-spec! (S <string>) type-spec))


(define (%initialise-some-compound-object-types)

  (set-identifier-object-type-spec! (S <vector>)
    (make-object-type-spec 'vicare:expander:tags:<vector>
			   (S <vector>) (top-tag-id) (S vector?)))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <list>)
    (make-object-type-spec 'vicare:expander:tags:<list>
			   (S <list>) (S <pair>) (S list?)))

  (set-identifier-object-type-spec! (S <bytevector>)
    (make-object-type-spec 'vicare:expander:tags:<bytevector>
			   (S <bytevector>) (top-tag-id) (S bytevector?)))

  (set-identifier-object-type-spec! (S <hashtable>)
    (make-object-type-spec 'vicare:expander:tags:<hashtable>
			   (S <hashtable>) (top-tag-id) (S hashtable?)))

  (set-identifier-object-type-spec! (S <struct>)
    (make-object-type-spec 'vicare:expander:tags:<struct>
			   (S <struct>) (top-tag-id) (S struct?)))

  (set-identifier-object-type-spec! (S <struct-type-descriptor>)
    (make-object-type-spec 'vicare:expander:tags:<struct-type-descriptor>
			   (S <struct-type-descriptor>) (S <struct>) (S struct-type-descriptor?)))

  (set-identifier-object-type-spec! (S <record>)
    (make-object-type-spec 'vicare:expander:tags:<record>
			   (S <record>) (S <struct>) (S record?)))

  (set-identifier-object-type-spec! (S <record-type-descriptor>)
    (make-object-type-spec 'vicare:expander:tags:<record-type-descriptor>
			   (S <record-type-descriptor>) (S <struct>) (S record-type-descriptor?)))

  (void))


(define (%initialise-numeric-object-types)
  (set-identifier-object-type-spec! (S <number>)
    (make-object-type-spec 'vicare:expander:tags:<number>
			   (S <number>) (top-tag-id) (S number?)))

  (set-identifier-object-type-spec! (S <complex>)
    (make-object-type-spec 'vicare:expander:tags:<complex>
			   (S <complex>) (S <number>) (S complex?)))

  (set-identifier-object-type-spec! (S <real-valued>)
    (make-object-type-spec 'vicare:expander:tags:<real-valued>
			   (S <real-valued>) (S <complex>) (S real-valued?)))

  (set-identifier-object-type-spec! (S <real>)
    (make-object-type-spec 'vicare:expander:tags:<real>
			   (S <real>) (S <real-valued>) (S real?)))

  (set-identifier-object-type-spec! (S <rational-valued>)
    (make-object-type-spec 'vicare:expander:tags:<rational-valued>
			   (S <rational-valued>) (S <real>) (S rational-valued?)))

  (set-identifier-object-type-spec! (S <rational>)
    (make-object-type-spec 'vicare:expander:tags:<rational>
			   (S <rational>) (S <rational-valued>) (S rational?)))

  (set-identifier-object-type-spec! (S <integer-valued>)
    (make-object-type-spec 'vicare:expander:tags:<integer-valued>
			   (S <integer-valued>) (S <rational-valued>) (S integer-valued?)))

  (set-identifier-object-type-spec! (S <integer>)
    (make-object-type-spec 'vicare:expander:tags:<integer>
			   (S <integer>) (S <rational-valued>) (S integer?)))

  (set-identifier-object-type-spec! (S <exact-integer>)
    (make-object-type-spec 'vicare:expander:tags:<exact-integer>
			   (S <exact-integer>) (S <integer>) (S exact-integer?)))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((even?)		(S fxeven?))
	((odd?)			(S fxodd?))
	((negative?)		(S fxnegative?))
	((positive?)		(S fxpositive?)	)
	((non-negative?)	(S fxnonnegative?))
	((non-positive?)	(S fxnonpositive?))
	((zero?)		(S fxzero?))
	((sign)			(S fxsign))
	(else
	 (syntax-violation '<fixnum> "unknown field name" input-form.stx field.sym))))

    (define (%dispatcher method.sym arg*.stx input-form.stx)
      #f)

    (define type-spec
      (make-object-type-spec 'vicare:expander:tags:<fixnum>
			     (S <fixnum>) (S <exact-integer>) (S fixnum?)
			     %accessor-maker #f  #f #f
			     #f %dispatcher))

    (set-identifier-object-type-spec! (S <fixnum>) type-spec))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <bignum>)
    (make-object-type-spec 'vicare:expander:tags:<bignum>
			   (S <bignum>) (S <exact-integer>) (S bignum?)))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((integer?)			(S flinteger?))
	((finite?)			(S flfinite?))
	((infinite?)			(S flinfinite?))
	((nan?)				(S flnan?))
	((negative?)			(S flnegative?))
	((positive?)			(S flpositive?))
	((nonnegative?)			(S flnonnegative?))
	((nonpositive?)			(S flnonpositive?))
	((zero?)			(S flzero?))
	((zero?/positive)		(S flzero?/positive))
	((zero?/negative)		(S flzero?/negative))
	((even?)			(S fleven?))
	((odd?)				(S flodd?))
	(else
	 (syntax-violation '<flonum> "unknown field name" input-form.stx field.sym))))

    (define (%dispatcher method.sym arg*.stx input-form.stx)
      (case method.sym
	((string)				(M flonum->string))
	;; methods: arithmetic functions
	((abs)					(M flabs))
	((*)					(M fl*))
	((+)					(M fl+))
	((-)					(M fl-))
	((/)					(M fl/))
	((div)					(M fldiv))
	((mod)					(M flmod))
	((div-and-mod)				(M fldiv-and-mod))
	((div0)					(M fldiv0))
	((mod0)					(M flmod0))
	((div0-and-mod0)			(M fldiv0-and-mod0))
	;; methods: power functions
	((expt)					(M flexpt))
	((square)				(M flsquare))
	((cube)					(M flcube))
	((sqrt)					(M flsqrt))
	((cbrt)					(M flcbrt))
	;; methods: comparison functions
	((=)					(M fl=?))
	((<)					(M fl<?))
	((>)					(M fl>?))
	((<=)					(M fl<=?))
	((>=)					(M fl>=?))
	;; methods: trigonometric functions
	((sin)					(M flsin))
	((cos)					(M flcos))
	((tan)					(M fltan))
	((acos)					(M flacos))
	((asin)					(M flasin))
	((atan)					(M flatan))
	;; methods: hyperbolic functions
	((sinh)					(M flsinh))
	((cosh)					(M flcosh))
	((tanh)					(M fltanh))
	((acosh)				(M flacosh))
	((asinh)				(M flasinh))
	((atanh)				(M flatanh))
	;; methods: rounding functions
	((ceiling)				(M flceiling))
	((floor)				(M flfloor))
	((round)				(M flround))
	((truncate)				(M fltruncate))
	;; methods: rationals operations
	((numerator)				(M flnumerator))
	((denominator)				(M fldenominator))
	;; methods: exponentiation and logarithms
	((exp)					(M flexp))
	((log)					(M fllog))
	((log1p)				(M fllog1p))
	((expm1)				(M flexpm1))
	((hypot)				(M flhypot))
	;; methods: min and max
	((max)					(M flmax))
	((min)					(M flmin))
	(else #f)))

    (define type-spec
      (make-object-type-spec 'vicare:expander:tags:<flonum>
			     (S <flonum>) (S <real>) (S flonum?)
			     %accessor-maker #f  #f #f
			     #f %dispatcher))

    (set-identifier-object-type-spec! (S <flonum>) type-spec))

;;; --------------------------------------------------------------------

  (let ()
    (define (%accessor-maker field.sym input-form.stx)
      (case field.sym
	((numerator)		(S numerator))
	((denominator)		(S denominator))
	(else
	 (syntax-violation '<ratnum> "unknown field name" input-form.stx field.sym))))

    (define (%dispatcher method.sym arg*.stx input-form.stx)
      #f)

    (define type-spec
      (make-object-type-spec 'vicare:expander:tags:<ratnum>
			     (S <ratnum>) (S <rational>) (S ratnum?)
			     %accessor-maker #f  #f #f
			     #f %dispatcher))

    (set-identifier-object-type-spec! (S <ratnum>) type-spec))

;;; --------------------------------------------------------------------

  (set-identifier-object-type-spec! (S <compnum>)
    (make-object-type-spec 'vicare:expander:tags:<compnum>
			   (S <compnum>) (S <complex>) (S compnum?)))

  (set-identifier-object-type-spec! (S <cflonum>)
    (make-object-type-spec 'vicare:expander:tags:<cflonum>
			   (S <cflonum>) (S <complex>) (S cflonum?)))

  (void))


(define (%initialise-input/output-port-object-types)
  (set-identifier-object-type-spec! (S <port>)
    (make-object-type-spec 'vicare:expander:tags:<port>
			   (S <port>) (top-tag-id) (S port?)))

  (set-identifier-object-type-spec! (S <input-port>)
    (make-object-type-spec 'vicare:expander:tags:<input-port>
			   (S <input-port>) (S <port>) (S input-port?)))

  (set-identifier-object-type-spec! (S <output-port>)
    (make-object-type-spec 'vicare:expander:tags:<output-port>
			   (S <output-port>) (S <port>) (S output-port?)))

  (set-identifier-object-type-spec! (S <input/output-port>)
    (make-object-type-spec 'vicare:expander:tags:<input/output-port>
			   (S <input/output-port>) (S <port>) (S input/output-port?)))

  (set-identifier-object-type-spec! (S <textual-port>)
    (make-object-type-spec 'vicare:expander:tags:<textual-port>
			   (S <textual-port>) (S <port>) (S textual-port?)))

  (set-identifier-object-type-spec! (S <binary-port>)
    (make-object-type-spec 'vicare:expander:tags:<binary-port>
			   (S <binary-port>) (S <port>) (S binary-port?)))

  (set-identifier-object-type-spec! (S <textual-input-port>)
    (make-object-type-spec 'vicare:expander:tags:<textual-input-port>
			   (S <textual-input-port>) (S <input-port>) (S textual-input-port?)))

  (set-identifier-object-type-spec! (S <textual-output-port>)
    (make-object-type-spec 'vicare:expander:tags:<textual-output-port>
			   (S <textual-output-port>) (S <output-port>) (S textual-output-port?)))

  (set-identifier-object-type-spec! (S <textual-input/output-port>)
    (make-object-type-spec 'vicare:expander:tags:<textual-input/output-port>
			   (S <textual-input/output-port>) (S <input/output-port>) (S textual-input/output-port?)))

  (set-identifier-object-type-spec! (S <binary-input-port>)
    (make-object-type-spec 'vicare:expander:tags:<binary-input-port>
			   (S <binary-input-port>) (S <input-port>) (S binary-input-port?)))

  (set-identifier-object-type-spec! (S <binary-output-port>)
    (make-object-type-spec 'vicare:expander:tags:<binary-output-port>
			   (S <binary-output-port>) (S <output-port>) (S binary-output-port?)))

  (set-identifier-object-type-spec! (S <binary-input/output-port>)
    (make-object-type-spec 'vicare:expander:tags:<binary-input/output-port>
			   (S <binary-input/output-port>) (S <input/output-port>) (S binary-input/output-port?)))

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

(let-syntax
    ((define-tag-retriever (syntax-rules ()
			     ((_ ?who ?tag)
			      (define ?who
				(let ((memoized-id #f))
				  (lambda ()
				    (or memoized-id
					(receive-and-return (id)
					    (scheme-stx '?tag)
					  (set! memoized-id id))))))))))
  (define-tag-retriever top-tag-id		<top>)
  (define-tag-retriever void-tag-id		<void>)
  (define-tag-retriever procedure-tag-id	<procedure>)
  (define-tag-retriever list-tag-id		<list>)
  (define-tag-retriever boolean-tag-id		<boolean>)
  #| end of let-syntax |# )

;;; --------------------------------------------------------------------

(define ($procedure-tag-id? id)
  (free-id=? id (procedure-tag-id)))

(define ($list-tag-id? id)
  (free-id=? id (list-tag-id)))

(define ($top-tag-id? id)
  (free-id=? id (top-tag-id)))

;;; --------------------------------------------------------------------

(define (procedure-tag-id? id)
  (and (identifier? id)
       ($procedure-tag-id? id)))

(define (list-tag-id? id)
  (and (identifier? id)
       ($list-tag-id? id)))

(define (top-tag-id? id)
  (and (identifier? id)
       ($top-tag-id? id)))

;;; --------------------------------------------------------------------

(define (retvals-signature-of-datum datum)
  (make-retvals-signature
   (list (cond ((boolean? datum)	(S <boolean>))
	       ((char?    datum)	(S <char>))
	       ((symbol?  datum)	(S <symbol>))
	       ((keyword? datum)	(S <keyword>))

	       ((fixnum?  datum)	(S <fixnum>))
	       ((flonum?  datum)	(S <flonum>))
	       ((ratnum?  datum)	(S <ratnum>))
	       ((bignum?  datum)	(S <bignum>))
	       ((compnum? datum)	(S <compnum>))
	       ((cflonum? datum)	(S <cflonum>))
	       ((real?    datum)	(S <real>))
	       ((complex? datum)	(S <complex>))
	       ((number?  datum)	(S <number>))

	       ((string?  datum)	(S <string>))
	       ((vector?  datum)	(S <vector>))
	       ((list?    datum)	(S <list>))
	       ((pair?    datum)	(S <pair>))
	       ((bytevector? datum)	(S <bytevector>))

	       ((eq? datum (void))	(S <void>))
	       (else			(top-tag-id))))))

;;; end of file
;; Local Variables:
;; mode: vicare
;; fill-column: 85
;; eval: (put 'set-identifier-object-type-spec! 'scheme-indent-function 1)
;; eval: (put 'sys.syntax-case 'scheme-indent-function 2)
;; eval: (put 'sys.with-syntax 'scheme-indent-function 1)
;; End:
