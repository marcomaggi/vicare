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
(module SCHEME-OBJECTS-ONTOLOGY
  (core-type-tag-description
		;Given an  instance of record  type CORE-TYPE-TAG: convert it  into a
		;descriptive and human readable list of symbols representing the type
		;bits that are set in X.

   core-type-tag=?
		;Given two  instances of  record type  CORE-TYPE-TAG: return  true if
		;they have exactly the same bits, otherwise return false.

   core-type-tag-and
		;Given two instances of record type CORE-TYPE-TAG: combine their bits
		;with  a  bitwise   AND  operation  and  return  a   record  of  type
		;CORE-TYPE-TAG holding the result.

   core-type-tag-or
		;Given two instances of record type CORE-TYPE-TAG: combine their bits
		;with  a   bitwise  OR  operation   and  return  a  record   of  type
		;CORE-TYPE-TAG holding the result.

   determine-constant-core-type
		;Given  a  Scheme  object:  return a  record  of  type  CORE-TYPE-TAG
		;representing its core type.

   ;;Records  of type  CORE-TYPE-TAG representing  predefined type  descriptions.  By
   ;;themselves they  represent valid type tags;  they are also used  as arguments to
   ;;CORE-TYPE-TAG-AND and CORE-TYPE-TAG-OR to compose more informative type tags.
   ;;
   T:object		T:other-object		T:immediate	T:nonimmediate
   T:non-false		T:false			T:true		T:void
   T:boolean		T:char			T:symbol	T:string
   T:null		T:pair			T:vector	T:bytevector
   T:procedure		T:port			T:struct	T:record
   T:transcoder

   T:number		T:exact			T:inexact
   T:fixnum		T:bignum		T:ratnum
   T:flonum		T:compnum		T:cflonum
   T:positive		T:zero			T:negative
   T:exact-integer	T:real			T:complex

   ;;Type validators; applied to a record of type CORE-TYPE-TAG return the symbol:
   ;;
   ;;yes -
   ;;   If the type of the record is a subset of the predefined type.  Examples:
   ;;
   ;;      (T:number? T:fixnum)		=> yes
   ;;      (T:number? T:exact)		=> yes
   ;;
   ;;no -
   ;;   If the type of the record it not a subset of the predefined type.  Examples:
   ;;
   ;;      (T:number? T:string)		=> no
   ;;      (T:number? T:exact)		=> no
   ;;
   ;;maybe -
   ;;   If some the bits in the record are equal to the bits in the predefined type.
   ;;
   ;;      (T:exact? T:number)		=> maybe
   ;;
   ;;The names are self explanatory with the following exceptions:
   ;;
   ;;T:object?
   ;;   All the Scheme objects satisfy this validator.
   ;;
   ;;T:other-object?
   ;;   Tests for objects that are not:
   ;;
   ;;	   T:symbol	T:bytevector	T:void		T:char
   ;;      T:null	T:pair		T:vector	T:string
   ;;      T:procedure	T:false		T:true
   ;;      T:fixnum	T:bignum	T:ratnum
   ;;      T:flonum	T:compnum	T:cflonum
   ;;
   T:object?		T:other-object?		T:immediate?	T:nonimmediate?
   T:non-false?		T:false?		T:true?		T:void?
   T:boolean?		T:char?			T:symbol?	T:string?
   T:null?		T:pair?			T:vector?	T:bytevector?
   T:procedure?		T:port?			T:struct?	T:record?
   T:transcoder

   T:number?		T:exact?		T:inexact?
   T:fixnum?		T:bignum?		T:ratnum?
   T:flonum?		T:compnum?		T:cflonum?
   T:positive?		T:zero?			T:negative?
   T:exact-integer?	T:real?			T:complex?

   #| end of module's export list|# )


(define-syntax (define-ontology x)
  ;;Define  the operators:  T:description,  T?, T=?,  T:and, T:or  to  be applied  to
  ;;records of type T.
  ;;
  (define (main x)
    (syntax-case x ()
      ((_ T make-T T? T:=? T:and T:or T:description
	  (?name0 ?cls0)
	  (?name  ?cls)
	  ...)
       (with-syntax
	   ((((NAME PREDNAME VAL) ...)
	     (%generate-base-cases #'T #'?name0 #'((?name0 ?cls0) (?name ?cls) ...)))
	    ($T-bits (datum->syntax #'T (string->symbol
					 (string-append "$"
							(symbol->string (syntax->datum #'T))
							"-bits")))))
	 #'(begin
	     ;;NOTE This  is a record  rather than a struct  because I like  the fact
	     ;;that the field is immutable.  (Marco Maggi; Fri Sep 12, 2014)
	     (define-record-type (T make-T T?)
	       (sealed #t)
	       (fields (immutable bits))
	       (protocol
		(lambda (make-instance)
		  (lambda* ({bits exact-integer?})
		    (make-instance bits)))))

	     (define* (T:=? {x T?} {y T?})
	       (= ($T-bits x) ($T-bits y)))

	     (define* (T:and {x0 T?} {x1 T?})
	       (make-T (bitwise-and ($T-bits x0) ($T-bits x1))))

	     (define* (T:or {x0 T?} {x1 T?})
	       (make-T (bitwise-ior ($T-bits x0) ($T-bits x1))))

	     (define-constant NAME (make-T VAL))
	     ...

	     (define* (PREDNAME {x T?})
	       (%test-bits ($T-bits x) VAL))
	     ...

	     (define (T:description x)
	       ;;Convert X, a  record of type T, into a  list of symbols representing
	       ;;the type bits that are set in X.
	       ;;
	       (let* ((ls '())
		      (ls (case (PREDNAME x)
			    ((yes) (cons 'NAME ls))
			    (else  ls)))
		      ...)
		 ls))

	     )))))

;;; --------------------------------------------------------------------

  (define (%generate-base-cases T main ls)
    (define (%value-name x)
      ;;Return an identifier with name "T:x" in the lexical context of
      ;;T.
      (datum->syntax T (string->symbol (string-append "T:" (symbol->string x)))))

    (define (%predicate-name x)
      ;;Return an  identifier with name "T:x?" in  the lexical context
      ;;of T.
      (datum->syntax T (string->symbol (string-append "T:" (symbol->string x) "?"))))

    (define (%property-names ls)
      ;;Given a list of S-expressions each having one of the formats:
      ;;
      ;;   (INAME (inclusive IPROP ...))
      ;;   (ENAME (exclusive EPROP ...))
      ;;
      ;;return the list: (IPROP ...)
      ;;
      (if (pair? ls)
	  (let ((first (car ls))
		(rest  (%property-names (cdr ls))))
	    (let ((name (car  first))
		  (info (cadr first)))
	      (case (car info)
		((exclusive)
		 rest)
		((inclusive)
		 (append (cdr info) rest))
		(else
		 (assert #f)))))
	'()))

    (let* ((main-datum (syntax->datum main))
	   (ls-datum   (syntax->datum ls))
	   (pnames     (%property-names ls-datum))
	   (alist      (%make-ontology main-datum ls-datum))
	   ;;Filter out from ALIST all then "inclusive" entries.
	   (alist1     (remp (lambda (entry)
			       (memq (car entry) pnames))
			 alist)))
      (map (lambda (entry)
	     (list (%value-name     (car entry))
		   (%predicate-name (car entry))
		   (cdr entry)))
	alist1)))

;;; --------------------------------------------------------------------

  (define (%make-ontology main-id ls)
    ;;LS is a list of S-expressions each having one of the formats:
    ;;
    ;;   (INAME (inclusive IPROP ...))
    ;;   (ENAME (exclusive EPROP ...))
    ;;
    (define (set-cons x ls)
      (cond
       ((memq x ls) ls)
       (else (cons x ls))))
    (define (union ls1 ls2)
      (cond
       ((null? ls1) ls2)
       (else (union (cdr ls1) (set-cons (car ls1) ls2)))))
    (define (difference ls1 ls2)
      (cond
       ((null? ls1) '())
       ((memq (car ls1) ls2) (difference (cdr ls1) ls2))
       (else (cons (car ls1) (difference (cdr ls1) ls2)))))
    (define (collect-names ls)
      (syntax-case ls ()
	(() '())
	(((name (of name* ...)) . rest)
	 (union (cons #'name #'(name* ...)) (collect-names #'rest)))))
    (define (expand x all)
      (define (lookup x ls)
	(cond
	 ((null? ls) (values 'tag '()))
	 (else
	  (let ((a (car ls)))
	    (cond
	     ((eq? x (car a))
	      (values (cadr a) (cdr ls)))
	     (else
	      (let-values (((xp ls) (lookup x (cdr ls))))
		(values xp (cons a ls)))))))))
      (let f ((x x) (ls ls))
	(let-values (((xp ls) (lookup x ls)))
	  (cond
	   ((pair? xp)
	    (cons (car xp) (map (lambda (x) (f x ls)) (cdr xp))))
	   ((eq? xp 'tag) x)
	   (else (error 'expand-lookup "invalid" xp))))))
    (define (rename alist x)
      (cond
       ((symbol? x) (cdr (assq x alist)))
       (else (cons (car x) (map (lambda (x) (rename alist x)) (cdr x))))))
    (define (enumerate ls)
      (let f ((i 1) (ls ls))
	(cond
	 ((null? ls) '())
	 (else (cons i (f (* i 2) (cdr ls)))))))
    (define (unique-elements x)
      (define (exclude m ls)
	(cond
	 ((null? ls) '())
	 ((zero? (bitwise-and m (car ls)))
	  (cons (car ls) (exclude m (cdr ls))))
	 (else (exclude m (cdr ls)))))
      (define (exclusive* m* x**)
	(cond
	 ((null? (cdr m*)) (values (car m*) (car x**)))
	 (else
	  (let-values (((m1 x1*) (values (car m*) (car x**)))
		       ((m2 x2*) (exclusive* (cdr m*) (cdr x**))))
	    (let ((x1* (exclude m2 x1*))
		  (x2* (exclude m1 x2*)))
	      (values (bitwise-ior m1 m2) (append x1* x2*)))))))
      (define (inclusive* m* x**)
	(cond
	 ((null? (cdr m*)) (values (car m*) (car x**)))
	 (else
	  (let-values (((m1 x1*) (values (car m*) (car x**)))
		       ((m2 x2*) (inclusive* (cdr m*) (cdr x**))))
	    (values (bitwise-ior m1 m2)
		    (remp not
		      (apply append
			     (map (lambda (x)
				    (map (lambda (y)
					   (if (= (bitwise-and m1 m2 x)
						  (bitwise-and m1 m2 y))
					       (bitwise-ior x y)
					     #f))
				      x2*))
			       x1*))))))))
      (define (f* ls)
	(cond
	 ((null? ls) (values '() '()))
	 (else
	  (let-values (((m x*) (f (car ls)))
		       ((m* x**) (f* (cdr ls))))
	    (values (cons m m*) (cons x* x**))))))
      (define (f x)
	(cond
	 ((integer? x) (values x (list x)))
	 (else
	  (let ((tag (car x)) (ls (cdr x)))
	    (let-values (((m* x**) (f* ls)))
	      (case tag
		((exclusive) (exclusive* m* x**))
		((inclusive) (inclusive* m* x**))
		(else (error 'f "invalid"))))))))
      (let-values (((m ls) (f x)))
	ls))
    (define (expand-names alist)
      (lambda (n)
	(let f ((alist alist))
	  (cond
	   ((null? alist) '())
	   ((zero? (bitwise-and n (cdar alist)))
	    (f (cdr alist)))
	   (else
	    (cons (caar alist) (f (cdr alist))))))))
    (define (extend-alist* ls alist)
      (define (extend-alist x alist)
	(define (lookup x)
	  (cond
	   ((assq x alist) => cdr)
	   (else (error 'lookup "cannot find" x alist))))
	(let ((name (car x)) (info (cadr x)))
	  (let ((tag (car info)) (x* (map lookup (cdr info))))
	    (case tag
	      ((exclusive)
	       (cons (cons name (apply bitwise-ior x*)) alist))
	      ((inclusive)
	       (assert (= (apply bitwise-ior x*) (apply bitwise-and x*)))
	       (cons (cons name (apply bitwise-ior x*)) alist))
	      (else (assert #f))))))
      (cond
       ((null? ls) alist)
       (else
	(extend-alist (car ls)
		      (extend-alist* (cdr ls) alist)))))
    (let* ((names (difference (collect-names ls) (map car ls)))
	   (names-alist (map cons names (enumerate names))))
      (let* ((expanded (expand main-id ls))
	     (renamed (rename names-alist expanded)))
	(let* ((unique* (list-sort < (unique-elements renamed)))
	       (canonicals (map (expand-names names-alist) unique*)))
	  (let* ((canonical-alist (map cons canonicals (enumerate canonicals)))
		 (seed-alist
		  (map
		      (lambda (x)
			(let ((ls (filter (lambda (y) (memq x (car y))) canonical-alist)))
			  (cons x (apply bitwise-ior (map cdr ls)))))
		    names)))
	    (extend-alist* ls seed-alist))))))

  (receive-and-return (output-form)
      (main x)
    ;;Uncomment this to get a dump of the macro expansion.
    #;(debug-print (syntax->datum output-form))
    (void)))


(define (%test-bits bits predefined-type-bits)
  (cond ((zero? (bitwise-and bits predefined-type-bits))
	 ;;None of the  PREDEFINED-TYPE-BITS are set in BITS; some  other bits may be
	 ;;set in BITS.  Bits examples:
	 ;;
	 ;;   BITS   := #b110000
	 ;;   PREDEF := #b001111
	 ;;
	 ;;BITS and PREDEF is disjunct.  Validator examples:
	 ;;
	 ;;   (T:string? T:fixnum)			=> no
	 ;;   (T:string? (T:or T:fixnum T:pair))	=> no
	 ;;
	 'no)
	((= predefined-type-bits (bitwise-ior bits predefined-type-bits))
	 ;;All  the  BITS   are  also  set  in  PREDEFINED-TYPE-BITS;   some  of  the
	 ;;PREDEFINED-TYPE-BITS are not set in BITS.  Bits examples:
	 ;;
	 ;;   BITS   := #b001111
	 ;;   PREDEF := #b001111
	 ;;
	 ;;   BITS   := #b000011
	 ;;   PREDEF := #b001111
	 ;;
	 ;;BITS is equal to, or a subset of, PREDEF.  Validator examples:
	 ;;
	 ;;   (T:number? T:number)			=> yes
	 ;;   (T:number? T:fixnum)			=> yes
	 ;;   (T:exact?  T:fixnum)			=> yes
	 ;;
	 'yes)
	(else
	 ;;Some of the PREDEFINED-TYPE-BITS are set in BITS; some of the BITS are not
	 ;;set in PREDEFINED-TYPE-BITS.  Bits examples:
	 ;;
	 ;;   BITS   := #b110011
	 ;;   PREDEF := #b001111
	 ;;
	 ;;Validator examples:
	 ;;
	 ;;   (T:string? (T:or T:fixnum T:string))	=> maybe
	 ;;
	 'maybe)))

(define-syntax core-type-tag-and*
  (syntax-rules ()
    ((_ ?tag)
     ?tag)
    ((_ ?tag0 ?tag1 ?tag ...)
     (core-type-tag-and ?tag0 (core-type-tag-and* ?tag1 ?tag ...)))
    ))

(define-syntax core-type-tag-or*
  (syntax-rules ()
    ((_ ?tag)
     ?tag)
    ((_ ?tag0 ?tag1 ?tag ...)
     (core-type-tag-or ?tag0 (core-type-tag-or* ?tag1 ?tag ...)))
    ))

(define-syntax (define-underspecified-core-type stx)
  (syntax-case stx ()
    ((_ ?type-name ?instance)
     (identifier? #'?type-name)
     (with-syntax
	 ((PRED (identifier-suffix #'?type-name "?")))
       #'(begin
	   (define-constant ?type-name
	     ?instance)
	   (define* (PRED {x core-type-tag?})
	     (%test-bits ($core-type-tag-bits x)
			 ($core-type-tag-bits ?type-name))))))
    ))


;;;; ontology definition

;;See below for the expansion of this syntax.
;;
;;NOTE The last element of the list  represents a "other-?object" value, which is not
;;one of the  preceeding values; this "everything else" item  *must* be present, even
;;when it  is not used  as type tag.   So the following  are present even  if unused:
;;other-number,   other-exact,    other-exact-integer,   other-real,   other-complex,
;;other-struct.
;;
(define-ontology core-type-tag
  make-core-type-tag core-type-tag? core-type-tag=?
  core-type-tag-and core-type-tag-or
  core-type-tag-description

  (object		(inclusive obj-tag obj-immediacy obj-truth))
  (obj-immediacy	(exclusive nonimmediate immediate))
  (immediate		(exclusive fixnum boolean null char transcoder void))
  (obj-truth		(exclusive false non-false))
  (obj-tag		(exclusive procedure string vector pair null
				   boolean char transcoder number void bytevector
				   symbol port struct other-object))
  (boolean		(exclusive true false))

  (number		(inclusive number-tag number-sign number-exactness))
  (number-sign		(exclusive negative zero positive))
  (number-tag		(exclusive fixnum bignum ratnum flonum cflonum compnum other-number))

  (number-exactness	(exclusive exact inexact))
  (exact		(exclusive fixnum bignum ratnum other-exact))
  (inexact		(exclusive flonum cflonum other-inexact))

  (exact-integer	(exclusive fixnum bignum other-exact-integer))
  (real			(exclusive fixnum bignum ratnum flonum other-real))
  (complex		(exclusive cflonum compnum other-complex))

  (struct		(exclusive record other-struct))

  #| end of ontology definition |# )


(module (determine-constant-core-type)

  (define (determine-constant-core-type x)
    (cond ((number?     x)   (%determine-numeric-constant-type x))
	  ((boolean?    x)   (if x T:true T:false))
	  ((null?       x)   T:null)
	  ((char?       x)   T:char)
	  ((string?     x)   T:string)
	  ((symbol?     x)   T:symbol)
	  ((vector?     x)   T:vector)
	  ((pair?       x)   T:pair)
	  ((bytevector? x)   T:bytevector)

	  ;;NOTE  These are  here for  completeness, but  commented out  because such
	  ;;objects can never be hard-coded constants.
	  ;;
	  ;;((port?      x)  T:port)
	  ;;((struct?    x)  T:struct)
	  ;;((record?    x)  T:record)

	  ((eq? x (void))    T:void)
	  (else              T:object)))

  (define (%determine-numeric-constant-type x)
    (cond ((fixnum? x)
	   (%sign x T:fixnum))
	  ((bignum? x)
	   (%sign x T:bignum))
	  ((ratnum? x)
	   (%sign x T:ratnum))
	  ((flonum? x)
	   (%sign x T:flonum))
	  ((cflonum? x)
	   (if (zero? x)
	       (core-type-tag-and T:cflonum T:zero)
	     T:cflonum))
	  ((compnum? x)
	   ;;FIXME After  the next boot image  rotation the test must  be the simple:
	   ;;"(zero? x)".  (Marco Maggi; Tue Nov 11, 2014)
	   (if (and (zero? (real-part x))
		    (zero? (imag-part x)))
	       (core-type-tag-and T:compnum T:zero)
	     T:compnum))
	  (else
	   ;;This should not happen.
	   T:number)))

  (define (%sign x t)
    (core-type-tag-and t (cond ((< x 0) T:negative)
			       ((> x 0) T:positive)
			       ((= x 0) T:zero)
			       (else    t))))

  #| end of module: DETERMINE-CONSTANT-CORE-TYPE |# )


;;;; done

#| end of module: SCHEME-OBJECTS-ONTOLOGY |# )


;;;;inline tests
;;
;;These tests (when included) are executed every time the init code in the boot image
;;is executed.
;;

;;Comment this EOF to include the tests.
;;#!eof

(module ()
  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (%do-check expr result expected)
    (unless (equal? result expected)
      (compiler-internal-error 'check-objects-ontology __who__
	"failed/got/expected" expr result expected)))

  (define-syntax check
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (%do-check '?expr ?expr '?expected))))

  (check (T:object? T:object)			=> yes)
  (check (T:object? T:other-object)		=> yes)
  (check (T:object? T:true)			=> yes)
  (check (T:object? (core-type-tag-and T:true T:false))	=> no)

  (check (T:true? T:object)			=> maybe)
  (check (T:true? T:true)			=> yes)
  (check (T:true? T:false)			=> no)
  (check (T:true? T:null)			=> no)

  (check (T:non-false? T:true)			=> yes)
  (check (T:non-false? T:null)			=> yes)
  (check (T:non-false? T:false)			=> no)
  (check (T:non-false? T:boolean)		=> maybe)
  (check (T:non-false? T:object)		=> maybe)

;;; --------------------------------------------------------------------

  (check (T:boolean? T:true)			=> yes)
  (check (T:boolean? T:false)			=> yes)
  (check (T:boolean? (core-type-tag-or  T:true T:false)) => yes)
  (check (T:boolean? (core-type-tag-and T:true T:false)) => no)

;;; --------------------------------------------------------------------

  (check (T:number? T:fixnum)			=> yes)
  (check (T:number? T:bignum)			=> yes)
  (check (T:number? T:ratnum)			=> yes)
  (check (T:number? T:flonum)			=> yes)
  (check (T:number? T:cflonum)			=> yes)
  (check (T:number? T:compnum)			=> yes)
  (check (T:number? T:number)			=> yes)
  (check (T:number? T:exact)			=> yes)
  (check (T:number? T:inexact)			=> yes)
  (check (T:number? T:exact-integer)		=> yes)
  (check (T:number? T:real)			=> yes)
  (check (T:number? T:complex)			=> yes)
  (check (T:number? T:string)			=> no)

  (check (T:exact? T:exact)			=> yes)
  (check (T:exact? T:inexact)			=> no)
  (check (T:exact? T:fixnum)			=> yes)
  (check (T:exact? T:bignum)			=> yes)
  (check (T:exact? T:ratnum)			=> yes)
  (check (T:exact? T:flonum)			=> no)
  (check (T:exact? T:cflonum)			=> no)
  (check (T:exact? T:compnum)			=> maybe)
  (check (T:exact? T:number)			=> maybe)
  (check (T:exact? T:exact-integer)		=> yes)
  (check (T:exact? T:real)			=> maybe)
  (check (T:exact? T:complex)			=> maybe)
  (check (T:exact? T:string)			=> no)

  (check (T:inexact? T:exact)			=> no)
  (check (T:inexact? T:inexact)			=> yes)
  (check (T:inexact? T:fixnum)			=> no)
  (check (T:inexact? T:bignum)			=> no)
  (check (T:inexact? T:ratnum)			=> no)
  (check (T:inexact? T:flonum)			=> yes)
  (check (T:inexact? T:cflonum)			=> yes)
  (check (T:inexact? T:compnum)			=> maybe)
  (check (T:inexact? T:number)			=> maybe)
  (check (T:inexact? T:exact-integer)		=> no)
  (check (T:inexact? T:real)			=> maybe)
  (check (T:inexact? T:complex)			=> maybe)
  (check (T:inexact? T:string)			=> no)

  (check (T:exact-integer? T:exact)		=> maybe)
  (check (T:exact-integer? T:inexact)		=> no)
  (check (T:exact-integer? T:fixnum)		=> yes)
  (check (T:exact-integer? T:bignum)		=> yes)
  (check (T:exact-integer? T:ratnum)		=> no)
  (check (T:exact-integer? T:flonum)		=> no)
  (check (T:exact-integer? T:cflonum)		=> no)
  (check (T:exact-integer? T:compnum)		=> no)
  (check (T:exact-integer? T:number)		=> maybe)
  (check (T:exact-integer? T:exact-integer)	=> yes)
  (check (T:exact-integer? T:real)		=> maybe)
  (check (T:exact-integer? T:complex)		=> no)
  (check (T:exact-integer? T:string)		=> no)

  (check (T:real? T:exact)			=> maybe)
  (check (T:real? T:inexact)			=> maybe)
  (check (T:real? T:fixnum)			=> yes)
  (check (T:real? T:bignum)			=> yes)
  (check (T:real? T:ratnum)			=> yes)
  (check (T:real? T:flonum)			=> yes)
  (check (T:real? T:cflonum)			=> no)
  (check (T:real? T:compnum)			=> no)
  (check (T:real? T:number)			=> maybe)
  (check (T:real? T:exact-integer)		=> yes)
  (check (T:real? T:real)			=> yes)
  (check (T:real? T:complex)			=> no)
  (check (T:real? T:string)			=> no)

  (check (T:complex? T:exact)			=> maybe)
  (check (T:complex? T:inexact)			=> maybe)
  (check (T:complex? T:fixnum)			=> no)
  (check (T:complex? T:bignum)			=> no)
  (check (T:complex? T:ratnum)			=> no)
  (check (T:complex? T:flonum)			=> no)
  (check (T:complex? T:cflonum)			=> yes)
  (check (T:complex? T:compnum)			=> yes)
  (check (T:complex? T:number)			=> maybe)
  (check (T:complex? T:exact-integer)		=> no)
  (check (T:complex? T:real)			=> no)
  (check (T:complex? T:complex)			=> yes)
  (check (T:complex? T:string)			=> no)

;;; --------------------------------------------------------------------

  (check (T:immediate? T:immediate)		=> yes)
  (check (T:immediate? T:nonimmediate)		=> no)

  (check (T:immediate? T:boolean)		=> yes)
  (check (T:immediate? T:null)			=> yes)
  (check (T:immediate? T:char)			=> yes)
  (check (T:immediate? T:transcoder)		=> yes)
  (check (T:immediate? T:void)			=> yes)
  (check (T:immediate? T:pair)			=> no)
  (check (T:immediate? T:procedure)		=> no)
  (check (T:immediate? T:string)		=> no)
  (check (T:immediate? T:symbol)		=> no)
  (check (T:immediate? T:vector)		=> no)
  (check (T:immediate? T:bytevector)		=> no)
  (check (T:immediate? T:port)			=> no)
  (check (T:immediate? T:struct)		=> no)
  (check (T:immediate? T:record)		=> no)
  (check (T:immediate? T:other-object)		=> no)

  (check (T:immediate? T:fixnum)		=> yes)
  (check (T:immediate? T:bignum)		=> no)
  (check (T:immediate? T:ratnum)		=> no)
  (check (T:immediate? T:flonum)		=> no)
  (check (T:immediate? T:cflonum)		=> no)
  (check (T:immediate? T:compnum)		=> no)

  (check (T:immediate? T:negative)		=> maybe)
  (check (T:immediate? T:zero)			=> maybe)
  (check (T:immediate? T:positive)		=> maybe)

;;; --------------------------------------------------------------------

  (check (T:nonimmediate? T:immediate)		=> no)
  (check (T:nonimmediate? T:nonimmediate)	=> yes)

  #;(check (T:nonimmediate? T:boolean)		=> yes)
  ;; (check (T:nonimmediate? T:null)		=> yes)
  ;; (check (T:nonimmediate? T:char)		=> yes)
  ;; (check (T:nonimmediate? T:transcoder)		=> yes)
  ;; (check (T:nonimmediate? T:void)		=> yes)
  ;; (check (T:nonimmediate? T:pair)		=> no)
  ;; (check (T:nonimmediate? T:procedure)		=> no)
  ;; (check (T:nonimmediate? T:string)		=> no)
  ;; (check (T:nonimmediate? T:symbol)		=> no)
  ;; (check (T:nonimmediate? T:vector)		=> no)
  ;; (check (T:nonimmediate? T:bytevector)		=> no)
  ;; (check (T:nonimmediate? T:port)		=> no)
  ;; (check (T:nonimmediate? T:struct)		=> no)
  ;; (check (T:nonimmediate? T:record)		=> no)
  ;; (check (T:nonimmediate? T:other-object)	=> no)

  ;; (check (T:nonimmediate? T:fixnum)		=> yes)
  ;; (check (T:nonimmediate? T:bignum)		=> no)
  ;; (check (T:nonimmediate? T:ratnum)		=> no)
  ;; (check (T:nonimmediate? T:flonum)		=> no)
  ;; (check (T:nonimmediate? T:cflonum)		=> no)
  ;; (check (T:nonimmediate? T:compnum)		=> no)

  ;; (check (T:nonimmediate? T:negative)		=> maybe)
  ;; (check (T:nonimmediate? T:zero)		=> maybe)
  ;; (check (T:nonimmediate? T:positive)		=> maybe)

;;; --------------------------------------------------------------------

  (check (T:struct? T:struct)			=> yes)
  (check (T:struct? T:record)			=> yes)
  (check (T:struct? T:string)			=> no)

  (check (T:record? T:struct)			=> maybe)
  (check (T:record? T:record)			=> yes)
  (check (T:record? T:string)			=> no)

;;; --------------------------------------------------------------------

  (check (T:port? T:port)			=> yes)
  (check (T:port? T:string)			=> no)

;;; --------------------------------------------------------------------
;;; multitype tests

  (check (T:fixnum? (core-type-tag-or T:fixnum T:flonum))	=> maybe)
  (check (T:flonum? (core-type-tag-or T:fixnum T:flonum))	=> maybe)
  (check (T:string? (core-type-tag-or T:fixnum T:flonum))	=> no)

  (check (T:fixnum? (core-type-tag-or T:fixnum T:string))	=> maybe)
  (check (T:string? (core-type-tag-or T:fixnum T:string))	=> maybe)
  (check (T:pair?   (core-type-tag-or T:fixnum T:string))	=> no)

  #| end of module |# )

#!eof


;;;The expansion of  DEFINE-ONTOLOGY above is the following  (minus some adjustements
;;;for readability) (last updated Tue Nov 11, 2014; I do *not* update it every time I
;;;change the ontology!!!).

(begin
  (define-record-type
    (core-type-tag make-core-type-tag core-type-tag?)
    (sealed #t)
    (fields (immutable bits))
    (protocol
     (lambda (make-instance)
       (lambda* ((brace bits fixnum?))
	 (make-instance bits)))))

  (define* (core-type-tag=? (brace x core-type-tag?)
			    (brace y core-type-tag?))
    (= ($core-type-tag-bits x)
       ($core-type-tag-bits y)))

  (define* (core-type-tag-and (brace x0 core-type-tag?)
			      (brace x1 core-type-tag?))
    (make-core-type-tag (bitwise-and ($core-type-tag-bits x0)
				     ($core-type-tag-bits x1))))

  (define* (core-type-tag-or (brace x0 core-type-tag?)
			     (brace x1 core-type-tag?))
    (make-core-type-tag (bitwise-ior ($core-type-tag-bits x0)
				     ($core-type-tag-bits x1))))

;;;Define the exact integers representing the type informations.
;;;
;;;By keeping the values in the 24-bit range  we make sure that they fit into fixnums
;;;as defined by  R6RS; by keeping the values  in the 30-bit range we  make sure that
;;;they fit into fixnums as defined by Vicare on a 32-bit platform (and automatically
;;;on a  64-bit platform).  If we  go over 30 bits:  on 32-bit platforms we  will use
;;;bignums, on  64-bit platforms  we still  use fixnums  up to  61 bits;  bignums are
;;;slower.
;;;
;;;                                                                                                        9876543210
;;;                                                                                            9876543210
;;;                                                                                  9876543210
;;;                                                                        9876543210
  (define-constant T:object		(make-core-type-tag 549755813887)) ;;;111111111111111111111111111111111111111
  (define-constant T:other-object	(make-core-type-tag 1))            ;;;000000000000000000000000000000000000001

  (define-constant T:symbol		(make-core-type-tag 2))            ;;;000000000000000000000000000000000000010
  (define-constant T:bytevector		(make-core-type-tag 4))            ;;;000000000000000000000000000000000000100
  (define-constant T:void		(make-core-type-tag 8))            ;;;000000000000000000000000000000000001000
  (define-constant T:char		(make-core-type-tag 16))           ;;;000000000000000000000000000000000010000
  (define-constant T:null		(make-core-type-tag 32))           ;;;000000000000000000000000000000000100000
  (define-constant T:pair		(make-core-type-tag 64))           ;;;000000000000000000000000000000001000000
  (define-constant T:vector		(make-core-type-tag 128))          ;;;000000000000000000000000000000010000000
  (define-constant T:string		(make-core-type-tag 256))          ;;;000000000000000000000000000000100000000
  (define-constant T:procedure		(make-core-type-tag 512))          ;;;000000000000000000000000000001000000000
  (define-constant T:false		(make-core-type-tag 1024))         ;;;000000000000000000000000000010000000000
  (define-constant T:true		(make-core-type-tag 2048))         ;;;000000000000000000000000000100000000000

  (define-constant T:boolean		(make-core-type-tag 3072))         ;;;000000000000000000000000000110000000000
  (define-constant T:non-false		(make-core-type-tag 549755812863)) ;;;111111111111111111111111111101111111111

  (define-constant T:immediate		(make-core-type-tag 939527224))	   ;;;000000000111000000000000000110000111000
  (define-constant T:nonimmediate	(make-core-type-tag 548816286663)) ;;;111111111000111111111111111001111000111

  (define-constant T:number		(make-core-type-tag 549755809792)) ;;;111111111111111111111111111000000000000
  (define-constant T:other-number	(make-core-type-tag 258048))       ;;;000000000000000000000111111000000000000

  (define-constant T:fixnum		(make-core-type-tag 939524096))    ;;;000000000111000000000000000000000000000
  (define-constant T:bignum		(make-core-type-tag 117440512))    ;;;000000000000111000000000000000000000000
  (define-constant T:ratnum		(make-core-type-tag 14680064))     ;;;000000000000000111000000000000000000000
  (define-constant T:flonum		(make-core-type-tag 1835008))      ;;;000000000000000000111000000000000000000
  (define-constant T:cflonum		(make-core-type-tag 481036337152)) ;;;111000000000000000000000000000000000000
  (define-constant T:compnum		(make-core-type-tag 67645734912))  ;;;000111111000000000000000000000000000000

  (define-constant T:exact		(make-core-type-tag 8587866112))   ;;;000000111111111111000000111000000000000
  (define-constant T:inexact		(make-core-type-tag 541167943680)) ;;;111111000000000000111111000000000000000
  (define-constant T:exact-integer	(make-core-type-tag 1056964608))   ;;;000000000111111000000000000000000000000

  (define-constant T:real		(make-core-type-tag 1073479680))   ;;;000000000111111111111000000000000000000
  (define-constant T:complex		(make-core-type-tag 548682072064)) ;;;111111111000000000000000000000000000000

  (define-constant T:positive		(make-core-type-tag 78536544256))  ;;;001001001001001001001001001000000000000
  (define-constant T:zero		(make-core-type-tag 157073088512)) ;;;010010010010010010010010010000000000000
  (define-constant T:negative		(make-core-type-tag 314146177024)) ;;;100100100100100100100100100000000000000

  (define-constant T:other-exact	(make-core-type-tag 7516221440))   ;;;000000111000000000000000111000000000000
  (define-constant T:other-inexact	(make-core-type-tag 60129771520))
  (define-constant T:other-exact-integer (make-core-type-tag 0))
  (define-constant T:other-real		(make-core-type-tag 0))
  (define-constant T:other-complex	(make-core-type-tag 0))

;;; --------------------------------------------------------------------

  (define* (T:object? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 549755813887))

  (define* (T:immediate? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 939527224))

  (define* (T:boolean? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 3072))

  (define* (T:number? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 549755809792))

  (define* (T:exact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 8587866112))

  (define* (T:inexact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 541167943680))

  (define* (T:exact-integer? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1056964608))

  (define* (T:real? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1073479680))

  (define* (T:complex? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 548682072064))

  (define* (T:nonimmediate? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 548816286663))

  (define* (T:non-false? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 549755812863))

  (define* (T:other-object? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1))

  (define* (T:symbol? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 2))

  (define* (T:bytevector? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 4))

  (define* (T:void? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 8))

  (define* (T:char? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 16))

  (define* (T:null? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 32))

  (define* (T:pair? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 64))

  (define* (T:vector? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 128))

  (define* (T:string? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 256))

  (define* (T:procedure? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 512))

  (define* (T:false? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1024))

  (define* (T:true? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 2048))

  (define* (T:positive? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 78536544256))

  (define* (T:zero? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 157073088512))

  (define* (T:negative? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 314146177024))

  (define* (T:other-number? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 258048))

  (define* (T:other-exact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 7516221440))

  (define* (T:other-inexact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 60129771520))

  (define* (T:other-exact-integer? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 0))

  (define* (T:other-real? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 0))

  (define* (T:flonum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1835008))

  (define* (T:ratnum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 14680064))

  (define* (T:bignum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 117440512))

  (define* (T:fixnum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 939524096))

  (define* (T:other-complex? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 0))

  (define* (T:compnum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 67645734912))

  (define* (T:cflonum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 481036337152))

  (define (core-type-tag-description x)
    (let* ((ls '())
	   (ls
	    (case (T:object? x)
	      ((yes) (cons 'T:object ls))
	      (else ls)))
	   (ls
	    (case (T:immediate? x)
	      ((yes) (cons 'T:immediate ls))
	      (else ls)))
	   (ls
	    (case (T:boolean? x)
	      ((yes) (cons 'T:boolean ls))
	      (else ls)))
	   (ls
	    (case (T:number? x)
	      ((yes) (cons 'T:number ls))
	      (else ls)))
	   (ls
	    (case (T:exact? x)
	      ((yes) (cons 'T:exact ls))
	      (else ls)))
	   (ls
	    (case (T:inexact? x)
	      ((yes) (cons 'T:inexact ls))
	      (else ls)))
	   (ls
	    (case (T:exact-integer? x)
	      ((yes) (cons 'T:exact-integer ls))
	      (else ls)))
	   (ls
	    (case (T:real? x)
	      ((yes) (cons 'T:real ls))
	      (else ls)))
	   (ls
	    (case (T:complex? x)
	      ((yes) (cons 'T:complex ls))
	      (else ls)))
	   (ls
	    (case (T:nonimmediate? x)
	      ((yes) (cons 'T:nonimmediate ls))
	      (else ls)))
	   (ls
	    (case (T:non-false? x)
	      ((yes) (cons 'T:non-false ls))
	      (else ls)))
	   (ls
	    (case (T:other-object? x)
	      ((yes) (cons 'T:other-object ls))
	      (else ls)))
	   (ls
	    (case (T:symbol? x)
	      ((yes) (cons 'T:symbol ls))
	      (else ls)))
	   (ls
	    (case (T:bytevector? x)
	      ((yes) (cons 'T:bytevector ls))
	      (else ls)))
	   (ls
	    (case (T:void? x)
	      ((yes) (cons 'T:void ls))
	      (else ls)))
	   (ls
	    (case (T:char? x)
	      ((yes) (cons 'T:char ls))
	      (else ls)))
	   (ls
	    (case (T:null? x)
	      ((yes) (cons 'T:null ls))
	      (else ls)))
	   (ls
	    (case (T:pair? x)
	      ((yes) (cons 'T:pair ls))
	      (else ls)))
	   (ls
	    (case (T:vector? x)
	      ((yes) (cons 'T:vector ls))
	      (else ls)))
	   (ls
	    (case (T:string? x)
	      ((yes) (cons 'T:string ls))
	      (else ls)))
	   (ls
	    (case (T:procedure? x)
	      ((yes) (cons 'T:procedure ls))
	      (else ls)))
	   (ls
	    (case (T:false? x)
	      ((yes) (cons 'T:false ls))
	      (else ls)))
	   (ls
	    (case (T:true? x)
	      ((yes) (cons 'T:true ls))
	      (else ls)))
	   (ls
	    (case (T:positive? x)
	      ((yes) (cons 'T:positive ls))
	      (else ls)))
	   (ls
	    (case (T:zero? x)
	      ((yes) (cons 'T:zero ls))
	      (else ls)))
	   (ls
	    (case (T:negative? x)
	      ((yes) (cons 'T:negative ls))
	      (else ls)))
	   (ls
	    (case (T:other-number? x)
	      ((yes) (cons 'T:other-number ls))
	      (else ls)))
	   (ls
	    (case (T:other-exact? x)
	      ((yes) (cons 'T:other-exact ls))
	      (else ls)))
	   (ls
	    (case (T:other-inexact? x)
	      ((yes) (cons 'T:other-inexact ls))
	      (else ls)))
	   (ls
	    (case (T:other-exact-integer? x)
	      ((yes) (cons 'T:other-exact-integer ls))
	      (else ls)))
	   (ls
	    (case (T:other-real? x)
	      ((yes) (cons 'T:other-real ls))
	      (else ls)))
	   (ls
	    (case (T:flonum? x)
	      ((yes) (cons 'T:flonum ls))
	      (else ls)))
	   (ls
	    (case (T:ratnum? x)
	      ((yes) (cons 'T:ratnum ls))
	      (else ls)))
	   (ls
	    (case (T:bignum? x)
	      ((yes) (cons 'T:bignum ls))
	      (else ls)))
	   (ls
	    (case (T:fixnum? x)
	      ((yes) (cons 'T:fixnum ls))
	      (else ls)))
	   (ls
	    (case (T:other-complex? x)
	      ((yes) (cons 'T:other-complex ls))
	      (else ls)))
	   (ls
	    (case (T:compnum? x)
	      ((yes) (cons 'T:compnum ls))
	      (else ls)))
	   (ls
	    (case (T:cflonum? x)
	      ((yes) (cons 'T:cflonum ls))
	      (else ls))))
      ls))

  #| end of BEGIN |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'define-underspecified-core-type	'scheme-indent-function 1)
;; End:
