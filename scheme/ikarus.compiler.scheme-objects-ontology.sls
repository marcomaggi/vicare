;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.scheme-objects-ontology)
  (export SCHEME-OBJECTS-ONTOLOGY)
  (import (vicare))


(module SCHEME-OBJECTS-ONTOLOGY
  (core-type-tag?
		;Return  true  if the  single  argument  is a  CORE-TYPE-TAG  record,
		;otherwise return false.

   core-type-tag-description
		;Given an  instance of record  type CORE-TYPE-TAG: convert it  into a
		;descriptive and human readable list of symbols representing the type
		;bits that are set in X.

   core-type-tag-bits
		;Given an instance  of CORE-TYPE-TAG: return the  naked bits defining
		;the type.

   core-type-tag=?
		;Given two  instances of  record type  CORE-TYPE-TAG: return  true if
		;they have exactly the same bits, otherwise return false.

   core-type-tag-and
		;Given two instances of record type CORE-TYPE-TAG: combine their bits
		;with  a  bitwise   AND  operation  and  return  a   record  of  type
		;CORE-TYPE-TAG holding the result.

   core-type-tag-ior
		;Given two instances of record type CORE-TYPE-TAG: combine their bits
		;with  a   bitwise  OR  operation   and  return  a  record   of  type
		;CORE-TYPE-TAG holding the result.

   make-core-type-tag-predicate
		;Given an instance  of record type CORE-TYPE-TAG:  return a predicate
		;function which can be used to test other instances.  Example:
		;
		;   (define p (make-core-type-tag-predicate T:exact-integer))
		;   (p T:fixnum)	=> yes
		;   (p T:number)	=> maybe
		;   (p T:string)	=> no
		;

   core-type-tag-is-a?
		;Given 2  instances of record  type CORE-TYPE-TAG: perform  core type
		;tag inclusion test.  Examples:
		;
		;   ;; "T:fixnum" is a "T:number" ?
		;   (core-type-tag-is-a? T:fixnum T:number)	=> yes
		;
		;   ;; "T:number" is a "T:fixnum" ?
		;   (core-type-tag-is-a? T:number T:fixnum)	=> maybe
		;
		;   ;; "T:string" is a "T:number" ?
		;   (core-type-tag-is-a? T:string T:number)	=> no
		;

   determine-constant-core-type
		;Given  a  Scheme  object:  return a  record  of  type  CORE-TYPE-TAG
		;representing its core type.

   name->core-type-tag
		;Given a  symbol representing  the name  of a  core type:  return the
		;associated CORE-TYPE-TAG value.  Otherwise raise an exception.

   ;;Records  of type  CORE-TYPE-TAG representing  predefined type  descriptions.  By
   ;;themselves they  represent valid type tags;  they are also used  as arguments to
   ;;CORE-TYPE-TAG-AND and CORE-TYPE-TAG-IOR to compose more informative type tags.
   ;;
   T:object		T:other-object		T:immediate	T:nonimmediate
   T:non-false		T:false			T:true		T:void
   T:boolean		T:char			T:symbol	T:string
   T:vector		T:bytevector		T:keyword
   T:procedure		T:transcoder		T:pointer	T:hashtable
   T:eof		T:would-block		T:code		T:pathname

   T:struct-type-descriptor
   T:record-type-descriptor	T:record-constructor-descriptor
   T:struct		T:other-record		T:record	T:other-struct
   T:enum-set		T:condition		T:library	T:lexical-environment
   T:stats		T:time			T:promise	T:utsname
   T:syntax-object	T:identifier

   T:memory-block	T:pointer/memory-block		T:pointer/bytevector

   T:null		T:standalone-pair	T:non-empty-proper-list
   T:pair		T:proper-list		T:improper-list

   T:port		T:textual-port		T:binary-port
   T:input-port		T:output-port		T:input/output-port
   T:textual-input-port	T:textual-output-port	T:textual-input/output-port
   T:binary-input-port	T:binary-output-port	T:binary-input/output-port

   T:number		T:exact			T:inexact
   T:fixnum		T:bignum		T:ratnum
   T:flonum		T:flonum-integer	T:flonum-fractional
   T:flonum-finite	T:flonum-infinite	T:flonum-nan
   T:cflonum
   T:exact-integer	T:integer
   T:real		T:exact-real
   T:compnum		T:exact-compnum
   T:non-real		T:complex

   T:zero	T:positive	T:negative	T:non-positive	T:non-negative

   T:positive-fixnum	T:negative-fixnum	T:non-positive-fixnum	T:non-negative-fixnum
   T:positive-bignum	T:negative-bignum
   T:positive-flonum	T:negative-flonum	T:non-positive-flonum	T:non-negative-flonum
   T:positive-exact-integer	T:non-positive-exact-integer
   T:negative-exact-integer	T:non-negative-exact-integer

   T:octet		T:byte			T:octet/byte
   T:sint8		T:uint8
   T:sint16		T:uint16
   T:sint32		T:uint32
   T:sint64		T:uint64
   T:file-descriptor

   T:pointer/false	T:string/false		T:number/false		T:fixnum/false

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
   ;;   Tests for objects that do not have a tag defined here.
   ;;
   T:object?		T:other-object?		T:immediate?	T:nonimmediate?
   T:non-false?		T:false?		T:true?		T:void?
   T:boolean?		T:char?			T:symbol?	T:string?
   T:vector?		T:bytevector?		T:keyword?
   T:procedure?		T:transcoder?		T:pointer?	T:hashtable?
   T:eof?		T:would-block?		T:code?		T:pathname?

   T:struct-type-descriptor?
   T:record-type-descriptor?	T:record-constructor-descriptor?
   T:struct?		T:other-record?
   T:record?		T:other-struct?
   T:enum-set?		T:condition?		T:library?	T:lexical-environment?
   T:stats?		T:time?			T:promise?	T:utsname?
   T:syntax-object?	T:identifier?

   T:memory-block?	T:pointer/memory-block?		T:pointer/bytevector?

   T:null?		T:standalone-pair?	T:non-empty-proper-list?
   T:pair?		T:proper-list?		T:improper-list?

   T:port?		T:textual-port?		T:binary-port?
   T:input-port?	T:output-port?		T:input/output-port?
   T:textual-input-port? T:textual-output-port?	T:textual-input/output-port?
   T:binary-input-port?	T:binary-output-port?	T:binary-input/output-port?

   T:number?		T:exact?		T:inexact?
   T:fixnum?		T:bignum?		T:ratnum?
   T:flonum?		T:flonum-integer?	T:flonum-fractional?
   T:flonum-finite?	T:flonum-infinite?	T:flonum-nan?
   T:cflonum?
   T:exact-integer?	T:integer?
   T:real?		T:exact-real?
   T:compnum?		T:exact-compnum?
   T:non-real?		T:complex?

   T:zero?	T:positive?	T:negative?	T:non-positive?	T:non-negative?

   T:positive-fixnum?	T:negative-fixnum?	T:non-positive-fixnum?	T:non-negative-fixnum?
   T:positive-bignum?	T:negative-bignum?
   T:positive-flonum?	T:negative-flonum?	T:non-positive-flonum?	T:non-negative-flonum?
   T:positive-exact-integer?	T:non-positive-exact-integer?
   T:negative-exact-integer?	T:non-negative-exact-integer?

   T:octet?		T:byte?			T:octet/byte?
   T:sint8?		T:uint8?
   T:sint16?		T:uint16?
   T:sint32?		T:uint32?
   T:sint64?		T:uint64?
   T:file-descriptor?

   T:pointer/false?	T:string/false?		T:number/false?		T:fixnum/false?

   #| end of module's export list|# )


;;;; helpers

(define-syntax __module_who__
  (identifier-syntax 'SCHEME-OBJECTS-ONTOLOGY))


;;;; core type tag type definition
;;
;;It would be really  neat to use proper type definitions to  represent the core type
;;tags, for example:
;;
;;   (define-struct core-type-tag
;;     (bits))
;;
;;   (define* (%core-type-tag-and {x0 core-type-tag?} {x1 core-type-tag?})
;;     (make-core-type-tag (bitwise-and (core-type-tag-bits x0) (core-type-tag-bits x1))))
;;
;;   (define* (%core-type-tag-ior {x0 core-type-tag?} {x1 core-type-tag?})
;;     (make-core-type-tag (bitwise-ior (core-type-tag-bits x0) (core-type-tag-bits x1))))
;;
;;   (define-syntax core-type-tag-and
;;     (syntax-rules ()
;;       ((_ ?tag)
;;        ?tag)
;;       ((_ ?tag0 ?tag1 ?tag ...)
;;        (%core-type-tag-and ?tag0 (core-type-tag-and ?tag1 ?tag ...)))))
;;
;;   (define-syntax core-type-tag-ior
;;     (syntax-rules ()
;;       ((_ ?tag)
;;        ?tag)
;;       ((_ ?tag0 ?tag1 ?tag ...)
;;        (%core-type-tag-ior ?tag0 (core-type-tag-ior ?tag1 ?tag ...)))))
;;
;;But initialisation  speed, usage speed  and memory consumption are  also important,
;;given that there  are a number of core  types used to characterise a  big number of
;;core primitives.  So we opt for naked exact integers, which are faster.
;;
;;We attempt  to write code that  maximises the number of  expressions precomputed by
;;the source optimiser; this means defining  a number of syntaxes, rather than normal
;;DEFINE bindings, so  that the most constant values are  inlined even across library
;;boundaries.
;;
;;(Marco Maggi; Thu Dec 4, 2014)
;;

(define-syntax-rule (make-core-type-tag ?bits)
  ?bits)

(define-syntax-rule (core-type-tag? ?obj)
  (exact-integer? ?obj))

(define-syntax-rule (core-type-tag-bits ?obj)
  ?obj)

(define-syntax-rule (core-type-tag-and ?tag0 ?tag ...)
  (bitwise-and ?tag0 ?tag ...))

(define-syntax-rule (core-type-tag-ior ?tag0 ?tag ...)
  (bitwise-ior ?tag0 ?tag ...))

;;; --------------------------------------------------------------------

(define-syntax (define-underspecified-core-type stx)
  (syntax-case stx ()
    ((_ ?type-name ?instance-expr)
     (identifier? #'?type-name)
     (with-syntax
	 ((PRED (identifier-suffix #'?type-name "?")))
       #'(begin
	   (define-syntax ?type-name
	     (identifier-syntax ?instance-expr))

	   (define-syntax-rule (PRED ??x)
	     (%test-bits (core-type-tag-bits ??x)
			 (core-type-tag-bits ?type-name)))

	   (module ()
	     (set-symbol-value! (quote ?type-name) ?type-name))

	   #| end of BEGIN |# )))
    ))

;;; --------------------------------------------------------------------

(define-syntax-rule (name->core-type-tag ?name)
  (symbol-value ?name))

;;; --------------------------------------------------------------------
;;; flat comparison

(define* (core-type-tag=? {x core-type-tag?} {y core-type-tag?})
  (= (core-type-tag-bits x) (core-type-tag-bits y)))

;;; --------------------------------------------------------------------

(define* (make-core-type-tag-predicate {y core-type-tag?})
  ;;Given an instance of CORE-TYPE-TAG: return a predicate function which can be used
  ;;to test other instances.  Example:
  ;;
  ;;   (define p (make-core-type-tag-predicate T:exact-integer))
  ;;   (p T:fixnum)	=> yes
  ;;   (p T:number)	=> maybe
  ;;   (p T:string)	=> no
  ;;
  (let ((y.bits (core-type-tag-bits y)))
    (lambda* ({x core-type-tag?})
      (%test-bits (core-type-tag-bits x) y.bits))))

(define* (core-type-tag-is-a? {x core-type-tag?} {y core-type-tag?})
  ;;Perform core type tag inclusion test.  Examples:
  ;;
  ;;   (core-type-tag-is-a? T:fixnum T:number)	=> yes		;;T:fixnum is a T:number ?
  ;;   (core-type-tag-is-a? T:number T:fixnum)	=> maybe	;;T:number is a T:fixnum ?
  ;;   (core-type-tag-is-a? T:string T:number)	=> no		;;T:string is a T:number ?
  ;;
  (%test-bits (core-type-tag-bits x) (core-type-tag-bits y)))

(define (%test-bits bits predefined-type-bits)
  ;;This function  is for  internal use  and it  is the  heart of  the core  type tag
  ;;system.
  ;;
  ;;BITS must be a fixnum or bignum representing  the core type tag of a return value
  ;;from an expression in the source  code.  PREDEFINED-TYPE-BITS must be a fixnum or
  ;;bignum representing  the core  type specification  of a  predefined CORE-TYPE-TAG
  ;;struct, like "T:fixnum" or "T:positive-exact-integer".
  ;;
  ;;The bits are matched against each other and the return value is a symbol:
  ;;
  ;;yes -
  ;;   If BITS is of type PREDEFINED-TYPE-BITS.  Example:
  ;;
  ;;      (%test-bits (core-type-tag-bits T:fixnum)
  ;;                  (core-type-tag-bits T:exact-integer))
  ;;      => yes
  ;;
  ;;no -
  ;;   If BITS is not of type PREDEFINED-TYPE-BITS.  Example:
  ;;
  ;;      (%test-bits (core-type-tag-bits T:fixnum)
  ;;                  (core-type-tag-bits T:string))
  ;;      => no
  ;;
  ;;maybe -
  ;;   If BITS is compatible with PREDEFINED-TYPE-BITS, but it may also be of another
  ;;   type.  Example:
  ;;
  ;;      (%test-bits (core-type-tag-bits T:number)
  ;;                  (core-type-tag-bits T:fixnum))
  ;;      => maybe
  ;;
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


(define-syntax (define-ontology x)
  (define (main x)
    (syntax-case x ()
      ((_ T:description
	  (?name0 ?cls0)
	  (?name  ?cls)
	  ...)
       (with-syntax
	   ((((NAME PREDNAME VAL) ...)
	     (%generate-base-cases #'T:description #'?name0 #'((?name0 ?cls0) (?name ?cls) ...))))
	 #'(begin
	     (define-syntax NAME
	       (identifier-syntax (make-core-type-tag VAL)))
	     ...

	     (define-syntax-rule (PREDNAME ?x)
	       (%test-bits (core-type-tag-bits ?x) VAL))
	     ...

	     (define (T:description x)
	       ;;Convert X,  a record of type  CORE-TYPE-TAG, into a list  of symbols
	       ;;representing the type bits that are set in X.
	       ;;
	       (let* ((ls '())
		      (ls (case (PREDNAME x)
			    ((yes) (cons 'NAME ls))
			    (else  ls)))
		      ...)
		 ls))

	     (module ()
	       (set-symbol-value! (quote NAME) NAME)
	       ...)

	     )))))

;;; --------------------------------------------------------------------

  (define (%generate-base-cases ctx main ls)
    (define (%value-name x)
      ;;Return an identifier with name "T:x" in the lexical context of CTX.
      (datum->syntax ctx (string->symbol (string-append "T:" (symbol->string x)))))

    (define (%predicate-name x)
      ;;Return an identifier with name "T:x?" in the lexical context of ctx.
      (datum->syntax ctx (string->symbol (string-append "T:" (symbol->string x) "?"))))

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
	   ;;Filter out from ALIST all the "inclusive" entries.
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
    ;;
    ;; (parametrise ((pretty-width			120)
    ;; 		  (printer-integer-radix	2))
    ;;   (debug-print (syntax->datum output-form))
    ;;   (with-output-to-file "/tmp/marco/p"
    ;; 	(lambda ()
    ;; 	  (pretty-print (syntax->datum output-form) (current-output-port))
    ;; 	  (flush-output-port (current-output-port))))
    ;;   (void))
    (void)))


;;;; ontology definition

;;See below for the expansion of this syntax.
;;
;;NOTE The last element of the list  represents a "dummy-?object" value, which is not
;;one of the  preceeding values; this "everything else" item  *must* be present, even
;;when it  is not used  as type tag.   So the following  are present even  if unused:
;;dummy-number, dummy-exact-integer, dummy-exact-real, dummy-non-real, dummy-port.
;;
(define-ontology
  core-type-tag-description

  (object		(inclusive obj-tag obj-immediacy obj-truth))
  (obj-immediacy	(exclusive immediate nonimmediate))
  (immediate		(exclusive fixnum boolean null char transcoder eof void))
  (obj-truth		(exclusive false non-false))
  (obj-tag		(exclusive boolean char transcoder void number pointer
				   null standalone-pair non-empty-proper-list
				   symbol keyword string procedure vector bytevector
				   hashtable struct eof would-block
				   port code other-object))

  (boolean		(exclusive true false))

  (proper-list		(exclusive null non-empty-proper-list dummy-list))
  (pair			(exclusive standalone-pair non-empty-proper-list dummy-pair))

  (number		(inclusive number-tag number-sign number-exactness))
  (number-sign		(exclusive negative zero positive))
  (number-tag		(exclusive fixnum bignum ratnum
				   flonum-integer flonum-fractional flonum-infinite flonum-nan
				   cflonum compnum dummy-number))

  ;; T:flonum-integer		- flonums representing integers (example: 12.0)
  ;; T:flonum-fractional	- all flonums being non-integer, non-infinite, non-nan
  ;; T:flonum-finite		- all flonums being non-infinite, non-nan
  ;; T:flonum-infinite		- only flonums being +inf.0, -inf.0
  ;; T:flonum-nan		- only flonums being +nan.0, -nan.0
  (flonum-finite	(exclusive flonum-integer flonum-fractional))
  (flonum		(exclusive flonum-integer flonum-fractional flonum-infinite flonum-nan))

  ;;"T:compnum" can be "T:other-exact" and also "T:other-inexact".
  (number-exactness	(exclusive exact inexact))
  (exact		(exclusive fixnum bignum ratnum other-exact))
  (inexact		(exclusive flonum-integer flonum-fractional flonum-infinite flonum-nan
				   cflonum other-inexact))

  (exact-integer	(exclusive fixnum bignum dummy-exact-integer))

  (real			(exclusive fixnum bignum ratnum
				   flonum-integer flonum-fractional flonum-infinite flonum-nan))
  (non-real		(exclusive cflonum compnum dummy-non-real))
  (exact-real		(exclusive fixnum bignum ratnum dummy-exact-real))

  ;; T:struct			- Vicare struct
  ;; T:struct-type-descriptor	- Vicare struct type descriptor
  ;; T:record			- R6RS record instance
  ;; T:record-type-descriptor	- R6RS record type descriptor
  ;; T:record-constructor-descriptor - R6RS record constructor descriptor
  ;; T:enum-set			- struct instance representing an enum-set
  ;; T:time			- struct instance representing time instant
  ;; T:promise			- struct instance representing a promise object
  ;; T:utsname			- struct instance
  ;; T:syntax-object		- struct instance representing a syntax object
  ;; T:other-struct		- struct instance of some type
  (struct		(exclusive struct-type-descriptor record-type-descriptor
				   record-constructor-descriptor record enum-set
				   library lexical-environment time promise
				   utsname syntax-object other-struct))

  ;; T:condition                - R6RS condition object, either simple or compound
  ;; T:stats			- timing data
  (record		(exclusive condition stats other-record))

  (syntax-object	(exclusive identifier other-syntax-object))

  ;;NOTE I  am unable to  define the port attributes  so that input/output  ports are
  ;;correctly recognised as input  port or output port when needed.   But most of the
  ;;predicates work fine, see the test suite.  (Marco Maggi; Wed Nov 12, 2014)
  (port			(exclusive textual-input-port textual-output-port textual-input/output-port
  				   binary-input-port binary-output-port binary-input/output-port
  				   dummy-port))
  (input-port		(exclusive textual-input-port        binary-input-port        dummy-input-port))
  (output-port		(exclusive textual-output-port       binary-output-port       dummy-output-port))
  (input/output-port	(exclusive textual-input/output-port binary-input/output-port dummy-input/output-port))
  (textual-port		(exclusive textual-input-port textual-output-port textual-input/output-port dummy-textual-port))
  (binary-port		(exclusive  binary-input-port  binary-output-port  binary-input/output-port  dummy-binary-port))

  #| end of ontology definition |# )


;;;; additional core type tags

(define-underspecified-core-type T:improper-list
  T:object)

(define-underspecified-core-type T:memory-block
  T:other-struct)

(define-underspecified-core-type T:pointer/memory-block
  (core-type-tag-ior T:pointer T:memory-block))

(define-underspecified-core-type T:pointer/bytevector
  (core-type-tag-ior T:pointer T:bytevector))

(define-underspecified-core-type T:pathname
  (core-type-tag-ior T:string T:bytevector))

;;; --------------------------------------------------------------------

(define-underspecified-core-type T:non-positive
  (core-type-tag-ior T:zero T:negative))

(define-underspecified-core-type T:non-negative
  (core-type-tag-ior T:zero T:positive))

(define-underspecified-core-type T:complex
  T:number)

;;; --------------------------------------------------------------------
;;; more fixnum types

(define-underspecified-core-type T:positive-fixnum
  (core-type-tag-and T:fixnum T:positive))

(define-underspecified-core-type T:negative-fixnum
  (core-type-tag-and T:fixnum T:negative))

(define-underspecified-core-type T:non-positive-fixnum
  (core-type-tag-and T:fixnum T:non-positive))

(define-underspecified-core-type T:non-negative-fixnum
  (core-type-tag-and T:fixnum T:non-negative))

(define-underspecified-core-type T:octet
  (core-type-tag-and T:fixnum T:non-negative))

(define-underspecified-core-type T:byte
  T:fixnum)

(define-underspecified-core-type T:octet/byte
  (core-type-tag-ior T:octet T:byte))

(define-underspecified-core-type T:file-descriptor
  T:non-negative-fixnum)

;;; --------------------------------------------------------------------
;;; more bignum types

(define-underspecified-core-type T:positive-bignum
  (core-type-tag-and T:bignum T:positive))

(define-underspecified-core-type T:negative-bignum
  (core-type-tag-and T:bignum T:negative))

;;; --------------------------------------------------------------------
;;; more flonum types

(define-underspecified-core-type T:positive-flonum
  (core-type-tag-and T:flonum T:positive))

(define-underspecified-core-type T:negative-flonum
  (core-type-tag-and T:flonum T:negative))

(define-underspecified-core-type T:non-positive-flonum
  (core-type-tag-and T:flonum T:non-positive))

(define-underspecified-core-type T:non-negative-flonum
  (core-type-tag-and T:flonum T:non-negative))

;;; --------------------------------------------------------------------
;;; more numeric types

(define-underspecified-core-type T:exact-compnum
  (core-type-tag-and T:compnum T:exact))

(define-underspecified-core-type T:integer
  (core-type-tag-ior T:fixnum T:bignum T:flonum-integer))

(define-underspecified-core-type T:positive-exact-integer
  (core-type-tag-and T:exact-integer T:positive))

(define-underspecified-core-type T:negative-exact-integer
  (core-type-tag-and T:exact-integer T:negative))

(define-underspecified-core-type T:non-positive-exact-integer
  (core-type-tag-and T:exact-integer T:non-positive))

(define-underspecified-core-type T:non-negative-exact-integer
  (core-type-tag-and T:exact-integer T:non-negative))

;;;

(define-underspecified-core-type T:sint8	T:byte)
(define-underspecified-core-type T:uint8	T:octet)

(define-underspecified-core-type T:sint16	T:fixnum)
(define-underspecified-core-type T:uint16	T:non-negative-fixnum)

(define-underspecified-core-type T:sint32	T:exact-integer)
(define-underspecified-core-type T:uint32	T:non-negative-exact-integer)

(define-underspecified-core-type T:sint64	T:exact-integer)
(define-underspecified-core-type T:uint64	T:non-negative-exact-integer)

;;; --------------------------------------------------------------------

(define-underspecified-core-type T:pointer/false
  (core-type-tag-ior T:pointer T:false))

(define-underspecified-core-type T:string/false
  (core-type-tag-ior T:string T:false))

(define-underspecified-core-type T:fixnum/false
  (core-type-tag-ior T:fixnum T:false))

(define-underspecified-core-type T:number/false
  (core-type-tag-ior T:number T:false))


(module (determine-constant-core-type)

  (define (determine-constant-core-type x)
    (cond ((number?     x)   (%determine-numeric-constant-type x))
	  ((boolean?    x)   (if x T:true T:false))
	  ((char?       x)   T:char)
	  ((string?     x)   T:string)
	  ((symbol?     x)   T:symbol)
	  ((vector?     x)   T:vector)
	  ((bytevector? x)   T:bytevector)

	  ((null?       x)   T:null)
	  ((list?       x)   T:non-empty-proper-list)
	  ((pair?       x)   T:standalone-pair)

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
	   (cond ((flinteger? x)
		  (%sign x T:flonum-integer))
		 ((flfinite? x)
		  (%sign x T:flonum-fractional))
		 ((flinfinite? x)
		  (%sign x T:flonum-infinite))
		 ((flnan? x)
		  (%sign x T:flonum-nan))
		 (else
		  (%sign x T:flonum))))
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


;;;The expansion of  DEFINE-ONTOLOGY above is the following  (minus some adjustements
;;;for readability) (last updated Tue Nov 11, 2014; I do *not* update it every time I
;;;change the ontology!!!).

#|

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
    (= (core-type-tag-bits x)
       (core-type-tag-bits y)))

  (define* (core-type-tag-and (brace x0 core-type-tag?)
			      (brace x1 core-type-tag?))
    (make-core-type-tag (bitwise-and (core-type-tag-bits x0)
				     (core-type-tag-bits x1))))

  (define* (core-type-tag-ior (brace x0 core-type-tag?)
			     (brace x1 core-type-tag?))
    (make-core-type-tag (bitwise-ior (core-type-tag-bits x0)
				     (core-type-tag-bits x1))))

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
  (define-constant T:non-real		(make-core-type-tag 548682072064)) ;;;111111111000000000000000000000000000000

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
    (%test-bits (core-type-tag-bits x) 549755813887))

  (define* (T:immediate? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 939527224))

  (define* (T:boolean? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 3072))

  (define* (T:number? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 549755809792))

  (define* (T:exact? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 8587866112))

  (define* (T:inexact? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 541167943680))

  (define* (T:exact-integer? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 1056964608))

  (define* (T:real? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 1073479680))

  (define* (T:non-real? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 548682072064))

  (define* (T:nonimmediate? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 548816286663))

  (define* (T:non-false? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 549755812863))

  (define* (T:other-object? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 1))

  (define* (T:symbol? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 2))

  (define* (T:bytevector? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 4))

  (define* (T:void? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 8))

  (define* (T:char? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 16))

  (define* (T:null? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 32))

  (define* (T:pair? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 64))

  (define* (T:vector? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 128))

  (define* (T:string? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 256))

  (define* (T:procedure? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 512))

  (define* (T:false? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 1024))

  (define* (T:true? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 2048))

  (define* (T:positive? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 78536544256))

  (define* (T:zero? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 157073088512))

  (define* (T:negative? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 314146177024))

  (define* (T:other-number? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 258048))

  (define* (T:other-exact? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 7516221440))

  (define* (T:other-inexact? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 60129771520))

  (define* (T:other-exact-integer? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 0))

  (define* (T:other-real? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 0))

  (define* (T:flonum? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 1835008))

  (define* (T:ratnum? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 14680064))

  (define* (T:bignum? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 117440512))

  (define* (T:fixnum? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 939524096))

  (define* (T:other-complex? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 0))

  (define* (T:compnum? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 67645734912))

  (define* (T:cflonum? (brace x core-type-tag?))
    (%test-bits (core-type-tag-bits x) 481036337152))

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
	    (case (T:non-real? x)
	      ((yes) (cons 'T:non-real ls))
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

|#


;;;; done

)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; mode: vicare
;; eval: (put 'define-underspecified-core-type	'scheme-indent-function 1)
;; End:
