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

   ;;Records  of type  CORE-TYPE-TAG representing  predefined type  descriptions.  By
   ;;themselves they  represent valid type tags;  they are also used  as arguments to
   ;;CORE-TYPE-TAG-AND and CORE-TYPE-TAG-OR to compose more informative type tags.
   ;;
   T:object		T:immediate		T:boolean
   T:number		T:exact			T:inexact
   T:nonimmediate	T:non-false		T:other-object
   T:symbol		T:bytevector		T:void
   T:char		T:null			T:pair
   T:vector		T:string		T:procedure
   T:false		T:true			T:other-exact
   T:fixnum		T:other-inexact		T:flonum
   T:positive		T:zero			T:negative
   T:other-number

   ;;Types not fully specified.
   T:ratnum		T:bignum		T:compnum
   T:cflonum

   T:maybe-port		T:maybe-struct		T:maybe-record

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
   ;;T:other-number?
   ;;   Tests  for numbers that  are neither  fixnums nor flonums:  ratnums, bignums,
   ;;   compnums, cflonums.
   ;;
   ;;T:other-exact?
   ;;   Tests for numbers that are exact, but not fixnums: ratnums, bignums.
   ;;
   ;;T:other-inexact?
   ;;   Tests for numbers that are inexact, but not flonums: compnums, cflonums.
   ;;
   ;;T:other-object?
   ;;   Tests for objects that are not:
   ;;
   ;;	   T:symbol	T:bytevector	T:void		T:char
   ;;      T:null	T:pair		T:vector	T:string
   ;;      T:procedure	T:false		T:true		T:other-exact
   ;;      T:fixnum	T:other-inexact	T:flonum
   ;;
   T:object?		T:immediate?		T:nonimmediate?
   T:boolean?		T:non-false?
   T:number?		T:exact?		T:inexact?
   T:other-object?	T:symbol?		T:bytevector?
   T:void?		T:char?			T:null?
   T:pair?		T:vector?		T:string?
   T:procedure?		T:false?		T:true?
   T:positive?		T:zero?			T:negative?
   T:other-number?	T:other-exact?		T:fixnum?
   T:other-inexact?	T:flonum?

   ;;These are not fully specified.
   T:ratnum?		T:bignum?		T:compnum?
   T:cflonum?

   T:maybe-port?	T:maybe-struct?		T:maybe-record?)


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
		  (lambda* ({bits fixnum?})
		    (make-instance bits)))))

	     (define* (T:=? {x T?} {y T?})
	       (fx=? ($T-bits x) ($T-bits y)))

	     (define* (T:and {x0 T?} {x1 T?})
	       (make-T (fxlogand ($T-bits x0) ($T-bits x1))))

	     (define* (T:or {x0 T?} {x1 T?})
	       (make-T (fxlogor ($T-bits x0) ($T-bits x1))))

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
  (cond ((fxzero? (fxlogand bits predefined-type-bits))
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
	((fx=? predefined-type-bits (fxlogor bits predefined-type-bits))
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

(define-syntax (define-underspecified-core-type stx)
  (syntax-case stx ()
    ((_ ?type-name ?tag0 ?tag ...)
     (all-identifiers? #'(_ ?type-name ?tag0 ?tag ...))
     (with-syntax
	 ((PRED (identifier-suffix #'?type-name "?")))
       #'(begin
	   (define-constant ?type-name
	     (core-type-tag-and* ?tag0 ?tag ...))
	   (define* (PRED {x core-type-tag?})
	     (%test-bits ($core-type-tag-bits x)
			 ($core-type-tag-bits ?type-name))))))
    ))


;;;; ontology definition

;;See below for the expansion of this syntax.
(define-ontology core-type-tag
  make-core-type-tag core-type-tag? core-type-tag=?
  core-type-tag-and core-type-tag-or
  core-type-tag-description
  (object		(inclusive obj-tag obj-immediacy obj-truth))
  (obj-immediacy	(exclusive nonimmediate immediate))
  (immediate		(exclusive fixnum boolean null char void))
  (obj-truth		(exclusive false non-false))
  (obj-tag		(exclusive procedure string vector pair null
				   boolean char number void bytevector
				   symbol other-object))
  (boolean		(exclusive true false))
  (number		(inclusive number-tag number-size number-exactness))
  (number-size		(exclusive negative zero positive))
  (number-tag		(exclusive fixnum flonum other-number))
  (number-exactness	(exclusive exact inexact))
  (exact		(exclusive fixnum other-exact))
  (inexact		(exclusive flonum other-inexact)))

;;; --------------------------------------------------------------------

(define-underspecified-core-type T:bignum
  T:other-number T:nonimmediate T:non-false T:exact)

(define-underspecified-core-type T:ratnum
  T:other-number T:nonimmediate T:non-false T:exact)

(define-underspecified-core-type T:compnum
  T:other-number T:nonimmediate T:non-false)

(define-underspecified-core-type T:cflonum
  T:other-number T:nonimmediate T:non-false T:inexact)

;;; --------------------------------------------------------------------

(define-underspecified-core-type T:maybe-port
  T:other-object T:nonimmediate T:non-false)

(define-underspecified-core-type T:maybe-struct
  T:other-object T:nonimmediate T:non-false)

(define-underspecified-core-type T:maybe-record
  T:other-object T:nonimmediate T:non-false)

#| end of module: SCHEME-OBJECTS-ONTOLOGY |# )


;;;inline tests

;;Uncomment this form to test.
#|
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

  (check (T:boolean? T:true)			=> yes)
  (check (T:boolean? T:false)			=> yes)
  (check
      (T:boolean? (core-type-tag-or T:true T:false))
    => yes)
  (check
      (T:boolean? (core-type-tag-and T:true T:false))
    => no)

  (check (T:number? T:fixnum)			=> yes)
  (check (T:number? T:flonum)			=> yes)
  (check (T:number? T:exact)			=> yes)
  (check (T:number? T:inexact)			=> yes)
  (check (T:number? T:other-inexact)		=> yes)
  (check (T:number? T:string)			=> no)

  (check (T:exact? T:exact)			=> yes)
  (check (T:exact? T:inexact)			=> no)
  (check (T:exact? T:fixnum)			=> yes)
  (check (T:exact? T:flonum)			=> no)
  (check (T:exact? T:number)			=> maybe)
  (check (T:exact? T:other-exact)		=> yes)
  (check (T:exact? T:other-inexact)		=> no)
  (check (T:exact? T:other-number)		=> maybe)
  (check (T:exact? T:string)			=> no)

  (check (T:inexact? T:exact)			=> no)
  (check (T:inexact? T:inexact)			=> yes)
  (check (T:inexact? T:fixnum)			=> no)
  (check (T:inexact? T:flonum)			=> yes)
  (check (T:inexact? T:other-exact)		=> no)
  (check (T:inexact? T:other-inexact)		=> yes)
  (check (T:inexact? T:other-number)		=> maybe) ;T:other-number = not fixnum, not flonum
  (check (T:inexact? T:number)			=> maybe)
  (check (T:inexact? T:string)			=> no)

  ;;Tests for exact number, but not fixnum.
  (check (T:other-exact? T:other-exact)		=> yes)
  (check (T:other-exact? T:other-inexact)	=> no)
  (check (T:other-exact? T:exact)		=> maybe)
  (check (T:other-exact? T:inexact)		=> no)
  (check (T:other-exact? T:fixnum)		=> no)
  (check (T:other-exact? T:flonum)		=> no)
  (check (T:other-exact? T:number)		=> maybe)
  (check (T:other-exact? T:string)		=> no)

  ;;Tests for inexact number, but not flonum.
  (check (T:other-inexact? T:other-inexact)	=> yes)
  (check (T:other-inexact? T:other-exact)	=> no)
  (check (T:other-inexact? T:exact)		=> no)
  (check (T:other-inexact? T:inexact)		=> maybe)
  (check (T:other-inexact? T:fixnum)		=> no)
  (check (T:other-inexact? T:flonum)		=> no)
  (check (T:other-inexact? T:number)		=> maybe)
  (check (T:other-inexact? T:string)		=> no)

  ;;Tests  for bignum  and  ratnum numbers;  but neither  fixnums,  nor flonums,  nor
  ;;compnums, nor cflonums.
  (check (T:other-number? T:other-number)	=> yes)
  (check (T:other-number? T:number)		=> maybe)
  (check (T:other-number? T:fixnum)		=> no)
  (check (T:other-number? T:flonum)		=> no)
  (check (T:other-number? T:exact)		=> maybe)
  (check (T:other-number? T:inexact)		=> maybe)
  (check (T:other-number? T:other-exact)	=> yes) ;T:other-exact is exact but not fixnum
  (check (T:other-number? T:other-inexact)	=> yes) ;T:other-inexact is inexact but not flonum
  (check (T:other-number? T:string)		=> no)

  ;;Multitype tests.
  (check (T:fixnum? (core-type-tag-or T:fixnum T:flonum))	=> maybe)
  (check (T:flonum? (core-type-tag-or T:fixnum T:flonum))	=> maybe)
  (check (T:string? (core-type-tag-or T:fixnum T:flonum))	=> no)

  (check (T:fixnum? (core-type-tag-or T:fixnum T:string))	=> maybe)
  (check (T:string? (core-type-tag-or T:fixnum T:string))	=> maybe)
  (check (T:pair?   (core-type-tag-or T:fixnum T:string))	=> no)

  #| end of module |# )

|#

#!eof


;;;The expansion  of DEFINE-ONTOLOGY above  is the following (minus  some adjustement
;;;for readability) (last updated Sat Sep 13, 2014):

(begin
  (define-record-type (core-type-tag make-T core-type-tag?)
    (sealed #t)
    (fields (immutable bits))
    (protocol
     (lambda (make-instance)
       (lambda* ((brace bits fixnum?))
	 (make-instance bits)))))

  (define* (core-type-tag=? (brace x core-type-tag?) (brace y core-type-tag?))
    (fx=? ($core-type-tag-bits x) ($core-type-tag-bits y)))

  (define* (core-type-tag-and (brace x0 core-type-tag?) (brace x1 core-type-tag?))
    (make-T (fxlogand ($core-type-tag-bits x0)
		      ($core-type-tag-bits x1))))

  (define* (core-type-tag-or (brace x0 core-type-tag?) (brace x1 core-type-tag?))
    (make-T (fxlogor ($core-type-tag-bits x0)
		     ($core-type-tag-bits x1))))

  (define (%test-bits bits predefined-type-bits)
    (cond ((fxzero? (fxlogand bits predefined-type-bits))
	   'no)
	  ((fx=? predefined-type-bits
		 (fxlogor bits predefined-type-bits))
	   'yes)
	  (else 'maybe)))

;;;Define  the exact  integers representing  the type  informations.  By  keeping the
;;;values in the 24-bit range we make sure that they fit into fixnums.
;;;
;;;                                                                            9876543210
;;;                                                                  9876543210
;;;                                                              3210
  (define-constant T:object		(make-T 16777215))	;111111111111111111111111
  (define-constant T:immediate		(make-T 232504))	;      111000110000111000
  (define-constant T:boolean		(make-T 3072))		;            110000000000
  (define-constant T:number		(make-T 16773120))	;111111111111000000000000
  (define-constant T:exact		(make-T 258048))	;      111111000000000000
  (define-constant T:inexact		(make-T 16515072))	;111111000000000000000000
  (define-constant T:nonimmediate	(make-T 16544711))	;111111000111001111000111
  (define-constant T:non-false		(make-T 16776191))	;111111111111101111111111
  (define-constant T:other-object	(make-T 1))		;                       1
  (define-constant T:symbol		(make-T 2))		;                      10
  (define-constant T:bytevector		(make-T 4))		;                     100
  (define-constant T:void		(make-T 8))		;                    1000
  (define-constant T:char		(make-T 16))		;                   10000
  (define-constant T:null		(make-T 32))		;                  100000
  (define-constant T:pair		(make-T 64))		;                 1000000
  (define-constant T:vector		(make-T 128))		;                10000000
  (define-constant T:string		(make-T 256))		;               100000000
  (define-constant T:procedure		(make-T 512))		;              1000000000
  (define-constant T:false		(make-T 1024))		;             10000000000
  (define-constant T:true		(make-T 2048))		;            100000000000
  (define-constant T:other-exact	(make-T 28672))		;         111000000000000
  (define-constant T:fixnum		(make-T 229376))	;      111000000000000000
  (define-constant T:other-inexact	(make-T 1835008))	;   111000000000000000000
  (define-constant T:flonum		(make-T 14680064))	;111000000000000000000000
  (define-constant T:positive		(make-T 2396160))	;  1001001001000000000000
  (define-constant T:zero		(make-T 4792320))	; 10010010010000000000000
  (define-constant T:negative		(make-T 9584640))	;100100100100000000000000
  (define-constant T:other-number	(make-T 1863680))	;   111000111000000000000

  (define* (T:object? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 16777215))

  (define* (T:immediate? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 232504))

  (define* (T:boolean? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 3072))

  (define* (T:number? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 16773120))

  (define* (T:exact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 258048))

  (define* (T:inexact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 16515072))

  (define* (T:nonimmediate? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 16544711))

  (define* (T:non-false? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 16776191))

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
    (%test-bits ($core-type-tag-bits x) 2396160))

  (define* (T:zero? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 4792320))

  (define* (T:negative? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 9584640))

  (define* (T:other-number? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1863680))

  (define* (T:other-exact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 28672))

  (define* (T:fixnum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 229376))

  (define* (T:other-inexact? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 1835008))

  (define* (T:flonum? (brace x core-type-tag?))
    (%test-bits ($core-type-tag-bits x) 14680064))

  (define (core-type-tag-description x)
    (let* ((ls '())
	   (ls (case (T:object? x)
		 ((yes) (cons 'T:object ls))
		 (else ls)))
	   (ls (case (T:immediate? x)
		 ((yes) (cons 'T:immediate ls))
		 (else ls)))
	   (ls (case (T:boolean? x)
		 ((yes) (cons 'T:boolean ls))
		 (else ls)))
	   (ls (case (T:number? x)
		 ((yes) (cons 'T:number ls))
		 (else ls)))
	   (ls (case (T:exact? x)
		 ((yes) (cons 'T:exact ls))
		 (else ls)))
	   (ls (case (T:inexact? x)
		 ((yes) (cons 'T:inexact ls))
		 (else ls)))
	   (ls (case (T:nonimmediate? x)
		 ((yes) (cons 'T:nonimmediate ls))
		 (else ls)))
	   (ls (case (T:non-false? x)
		 ((yes) (cons 'T:non-false ls))
		 (else ls)))
	   (ls (case (T:other-object? x)
		 ((yes) (cons 'T:other-object ls))
		 (else ls)))
	   (ls (case (T:symbol? x)
		 ((yes) (cons 'T:symbol ls))
		 (else ls)))
	   (ls (case (T:bytevector? x)
		 ((yes) (cons 'T:bytevector ls))
		 (else ls)))
	   (ls (case (T:void? x)
		 ((yes) (cons 'T:void ls))
		 (else ls)))
	   (ls (case (T:char? x)
		 ((yes) (cons 'T:char ls))
		 (else ls)))
	   (ls (case (T:null? x)
		 ((yes) (cons 'T:null ls))
		 (else ls)))
	   (ls (case (T:pair? x)
		 ((yes) (cons 'T:pair ls))
		 (else ls)))
	   (ls (case (T:vector? x)
		 ((yes) (cons 'T:vector ls))
		 (else ls)))
	   (ls (case (T:string? x)
		 ((yes) (cons 'T:string ls))
		 (else ls)))
	   (ls (case (T:procedure? x)
		 ((yes) (cons 'T:procedure ls))
		 (else ls)))
	   (ls (case (T:false? x)
		 ((yes) (cons 'T:false ls))
		 (else ls)))
	   (ls (case (T:true? x)
		 ((yes) (cons 'T:true ls))
		 (else ls)))
	   (ls (case (T:positive? x)
		 ((yes) (cons 'T:positive ls))
		 (else ls)))
	   (ls (case (T:zero? x)
		 ((yes) (cons 'T:zero ls))
		 (else ls)))
	   (ls (case (T:negative? x)
		 ((yes) (cons 'T:negative ls))
		 (else ls)))
	   (ls (case (T:other-number? x)
		 ((yes) (cons 'T:other-number ls))
		 (else ls)))
	   (ls (case (T:other-exact? x)
		 ((yes) (cons 'T:other-exact ls))
		 (else ls)))
	   (ls (case (T:fixnum? x)
		 ((yes) (cons 'T:fixnum ls))
		 (else ls)))
	   (ls (case (T:other-inexact? x)
		 ((yes) (cons 'T:other-inexact ls))
		 (else ls)))
	   (ls (case (T:flonum? x)
		 ((yes) (cons 'T:flonum ls))
		 (else ls))))
      ls))

  #| end of BEGIN |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'define-underspecified-core-type	'scheme-indent-function 1)
;; End:
