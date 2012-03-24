;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;long with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; introduction
;;
;;This  library defines  enumeration  type objects  and enumeration  set
;;objects: the  enumeration type  holds the set  of Scheme symbols  in a
;;vector, mapping each  to an index; the enumeration  sets references an
;;enumeration type.
;;
;;		 ------		 ------
;;		| type |<-------| set1 |
;;		 ------		 ------
;;                 ^
;;		   |		 ------
;;		    ------------| set2 |
;;				 ------
;;
;;An enumeration set holds a vector  of bits (an exact integer), one for
;;each symbol in the set: if a  bit is set to 1 the corresponding symbol
;;is contained.  The offset of a bit in the set's bitvector is the index
;;of the symbol in the type's vector.
;;
;;This  way: the union  between sets  is the  bitwise inclusive  OR, the
;;intersection between sets is the  bitwise AND, the complement of a set
;;is the bitwise NOT.
;;
;;Notice that a Vicare's fixnum is  a machine word and a Vicare's bignum
;;is an array of machine words  with a single machine word of meta data;
;;storing the bitvector in an exact integer is as efficient as possible.
;;


(library (ikarus enumerations)
  (export
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
    enum-set?
    make-file-options)
  (import (except (ikarus)
;;;		  define-enumeration
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
		  enum-set?
		  make-file-options)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; record types

(define-struct enum-type
  (symbol-to-index
		;Hashtable mapping symbols to indexes.
   index-to-symbol
		;Vector mapping indexes to symbols.
   universe-bitvector
		;Exact integer, interpreted as  vector of bits, having a
		;number of bits equal to  the number of symbols, all set
		;to 1.
   universe
		;Enum set representing the  universe of this type.  Enum
		;set records are immutable, so it is all right to have a
		;prebuilt universe.
		;
		;This  field is mutable  because of  the chicken-and-egg
		;problem while building the type's record.
   ))

(define-struct enum-set
  (type
		;Instance  of ENUM-TYPE  representing the  type  of this
		;enumeration set.
   bitvector
		;Exact integer having  one bit set to 1  for each symbol
		;in   this  enumeration   set,  with   respect   to  the
		;UNIVERSE-BITVECTOR field in the ENUM-TYPE value.
   ))


;;;; arguments validation

(define-constant EXPECTED_ENUM_SET
  "expected enumeration set as argument")

(define-argument-validation (enum-set who obj)
  (enum-set? obj)
  (assertion-violation who EXPECTED_ENUM_SET obj))

(define-argument-validation (symbol who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as argument" obj))

(define-argument-validation (list-of-symbols who obj)
  (and (list? obj) (for-all symbol? obj))
  (assertion-violation who "expected list of symbols as argument" obj))


;;;; helpers

(define-inline (make-symbol-to-index-map list-of-symbols)
  (let ((symbol-to-index (make-eq-hashtable)))
    (let next-symbol ((index		0)
		      (list-of-symbols	list-of-symbols))
      (if (null? list-of-symbols)
	  symbol-to-index
	(begin
	  (symbol-to-index-set! symbol-to-index (car list-of-symbols) index)
	  (next-symbol (+ 1 index) (cdr list-of-symbols)))))))
(define symbol-to-index-set!		hashtable-set!)
(define-inline (symbol-to-index-ref table key)
  (hashtable-ref table key #f))
(define symbol-to-index-size		hashtable-size)

(define-inline (make-index-to-symbol-map list-of-symbols)
  (list->vector list-of-symbols))
(define-inline (index-to-symbol-set! vector index value)
  (unsafe.vector-set! vector index value))
(define-inline (index-to-symbol-ref vector index)
  (unsafe.vector-ref vector index))
(define-inline (index-to-symbol-size vector)
  (unsafe.vector-length vector))

;;Applied to  an exact integer  representing a bivector: return  true if
;;the  rightmost  bit  is set  to  zero.   This  means that  the  symbol
;;associated to the rightmost bit is not in the set.
;;
(define-inline (%rightmost-bit-set-to-zero? bitvector)
  (even? bitvector))

(define-inline (>> bitvector)
  (bitwise-arithmetic-shift-right bitvector 1))


;;;; bitmasks

(define-inline (%full-bitmask number-of-bits)
  ;;Return an exact integer having  at least NUMBER-OF-BITS all set to 1
  ;;and the other bits set to zero.
  ;;
  (- (bitwise-arithmetic-shift-left 1 number-of-bits) 1)
  #;(- (expt 2 number-of-bits) 1))

(define-inline (%single-bitmask bit-offset)
  ;;Return an  exact integer having the  bit at BIT-OFFSET set  to 1 and
  ;;all the other bits set to zero.
  ;;
  (bitwise-arithmetic-shift-left 1 bit-offset)
  #;(expt 2 bit-offset))


(define (make-enumeration ell)
  ;;Create a new enumeration type  whose universe consists of the Scheme
  ;;symbols in ELL (in canonical  order of their first appearance in the
  ;;ELL) and return  that universe as an enumeration  set whose universe
  ;;is  itself   and  whose  enumeration  type  is   the  newly  created
  ;;enumeration type.
  ;;
  (define who 'make-enumeration)
  (with-arguments-validation (who)
      ((list-of-symbols	ell))
    (let* ((symbol-to-index	(make-symbol-to-index-map ell))
	   (index-to-symbol	(make-index-to-symbol-map ell))
	   (universe-bitvector	(%full-bitmask (index-to-symbol-size index-to-symbol)))
	   (type		(make-enum-type symbol-to-index index-to-symbol universe-bitvector #f))
	   (universe		(make-enum-set type universe-bitvector)))
      (set-enum-type-universe! type universe)
      universe)))


;;;; inspection and conversion

(define (enum-set-universe set)
  ;;Return  the set of  all symbols  that comprise  the universe  of its
  ;;argument, as an enumeration set.
  ;;
  (define who 'enum-set-universe)
  (with-arguments-validation (who)
      ((enum-set	set))
    (enum-type-universe (enum-set-type set))))

(define (enum-set-indexer set)
  ;;Return  a  unary procedure  that,  given a  symbol  that  is in  the
  ;;universe of  SET, returns its zero-based index  within the canonical
  ;;ordering of the  symbols in the universe; given a  symbol not in the
  ;;universe, the unary procedure returns false.
  ;;
  (define who 'enum-set-indexer)
  (with-arguments-validation (who)
      ((enum-set	set))
    (let ((table (enum-type-symbol-to-index (enum-set-type set))))
      (lambda (symbol)
	(define who 'anonymous-enumeration-indexer)
	(with-arguments-validation (who)
	    ((symbol	symbol))
	  (symbol-to-index-ref table symbol))))))

(define (enum-set-constructor set)
  ;;Return a unary  procedure that, given a list  of symbols that belong
  ;;to  the universe  of SET,  returns a  subset of  that  universe that
  ;;contains exactly  the symbols in the  list.  The values  in the list
  ;;must all belong to the universe.
  ;;
  (define who 'enum-set-constructor)
  (with-arguments-validation (who)
      ((enum-set	set))
    (let* ((set.type	(enum-set-type set))
	   (set.stoi	(enum-type-symbol-to-index set.type)))
      (lambda (ell)
	(define who 'anonymous-enumeration-set-constructor)
	(define-inline (make-bitmask-for-symbol sym)
	  (%single-bitmask (or (symbol-to-index-ref set.stoi sym)
				       (assertion-violation who
					 "given symbol is not in universe of enumeration set"
					 sym set.type))))
	(with-arguments-validation (who)
	    ((list-of-symbols	ell))
	  (let loop ((ell		ell)
		     (newset.bitvector	0))
	    (if (null? ell)
		(make-enum-set set.type newset.bitvector)
	      (loop (cdr ell) (bitwise-ior newset.bitvector (make-bitmask-for-symbol (car ell)))))))))))

(define (enum-set->list set)
  ;;Return a  list of the  symbols that belong  to its argument,  in the
  ;;canonical order of the universe of SET.
  ;;
  (define who 'enum-set->list)
  (with-arguments-validation (who)
      ((enum-set	set))
    (let ((set.index-to-symbol (enum-type-index-to-symbol (enum-set-type set))))
      (let loop ((bitvector	(enum-set-bitvector set))
		 (index		0)
		 (ell		'()))
	(if (zero? bitvector)
	    (reverse ell)
	  (loop (>> bitvector) (+ 1 index)
		(if (%rightmost-bit-set-to-zero? bitvector)
		    ell
		  (cons (index-to-symbol-ref set.index-to-symbol index) ell))))))))


;;;; membership and equality

(define (enum-set-member? sym set)
  ;;Return true if the SYM is a member of SET.
  ;;
  (define who 'enum-set-member?)
  (with-arguments-validation (who)
      ((symbol		sym)
       (enum-set	set))
    (let ((index (symbol-to-index-ref (enum-type-symbol-to-index (enum-set-type set)) sym)))
      (and index (bitwise-bit-set? (enum-set-bitvector set) index)))))

(define (enum-set-subset? set1 set2)
  ;;Return true if  the universe of SET1 is a subset  of the universe of
  ;;SET2 and  every element of  SET1 is a  member of SET2;  return false
  ;;otherwise.
  ;;
  (define who 'enum-set-subset?)
  (with-arguments-validation (who)
      ((enum-set	set1)
       (enum-set	set2))
    ;;If the type is  the same: the universe of SET1 is  a subset of the
    ;;universe of SET2 (the two being equal).
    (if (eq? (enum-set-type set1) (enum-set-type set2))
	(let ((set1.bitvector (enum-set-bitvector set1))
	      (set2.bitvector (enum-set-bitvector set2)))
	  ;;Tests that all the elements of SET1 are in SET2.
	  (= set1.bitvector (bitwise-and set1.bitvector set2.bitvector)))
      ;;The type is  different: check, element by element,  that all the
      ;;elements of SET1 are in SET2; if yes: check, element by element,
      ;;that the universe of SET1 is a subset of the universe of SET2.
      (and (%enum-set-for-all (lambda (sym-from-set1)
				(enum-set-member? sym-from-set1 set2))
			      set1)
	   (let ((set2.universe (enum-set-universe set2)))
	     (%enum-set-for-all (lambda (sym-from-set1)
				  (enum-set-member? sym-from-set1 set2.universe))
				(enum-set-universe set1)))))))

(define (enum-set=? set1 set2)
  ;;Return  true  if  SET1 is  a  subset  of  SET2  and vice  versa,  as
  ;;determined by the ENUM-SET-SUBSET?  procedure; this implies that the
  ;;universes of the two sets are equal as sets of symbols, but does not
  ;;imply that they  are equal as enumeration types  (that is: the order
  ;;of the symbols is different).  Otherwise return false.
  ;;
  (define who 'enum-set=?)
  (with-arguments-validation (who)
      ((enum-set	set1)
       (enum-set	set2))
    (if (eq? (enum-set-type set1) (enum-set-type set2))
	(= (enum-set-bitvector set1) (enum-set-bitvector set2))
      (and (%enum-set-for-all (lambda (sym) (enum-set-member? sym set2)) set1)
	   (%enum-set-for-all (lambda (sym) (enum-set-member? sym set1)) set2)
	   (let ((set1.universe (enum-set-universe set1))
		 (set2.universe (enum-set-universe set2)))
	     (and (%enum-set-for-all (lambda (s) (enum-set-member? s set2.universe)) set1.universe)
		  (%enum-set-for-all (lambda (s) (enum-set-member? s set1.universe)) set2.universe)))))))


;;;; set operations

(define (%enum-set-op set1 set2 who combine)
  ;;Perform  the COMBINE  operation between  SET1 and  SET2;  return the
  ;;resulting enumeration set.
  ;;
  (define-argument-validation (enum-type who set2 set1 type1)
    (eq? type1 (enum-set-type set2))
    (assertion-violation who
      "expected enumeration sets with the same enumeration type" set1 set2))
  (with-arguments-validation (who)
      ((enum-set	set1)
       (enum-set	set2))
    (let ((set1.type (enum-set-type set1)))
      (with-arguments-validation (who)
	  ((enum-type	set2 set1 set1.type))
	(make-enum-set set1.type (combine (enum-set-bitvector set1) (enum-set-bitvector set2)))))))

(define (enum-set-union set1 set2)
  (%enum-set-op set1 set2 'enum-set-union bitwise-ior))

(define (enum-set-intersection set1 set2)
  (%enum-set-op set1 set2 'enum-set-intersection bitwise-and))

(define (enum-set-difference set1 set2)
  (%enum-set-op set1 set2 'enum-set-difference
		(lambda (bitvector1 bitvector2)
		  (bitwise-and bitvector1 (bitwise-not bitvector2)))))

;;; --------------------------------------------------------------------

(define (enum-set-complement set)
  (define who 'enum-set-complement)
  (with-arguments-validation (who)
      ((enum-set	set))
    (let ((set.type (enum-set-type set)))
      (make-enum-set set.type
		     (bitwise-and (enum-type-universe-bitvector set.type)
				  (bitwise-not (enum-set-bitvector set)))))))

(define (enum-set-projection set1 set2)
  ;;Project SET1  into the  universe of SET2,  dropping any  elements of
  ;;SET1 that do not belong to the universe of SET2.
  ;;
  ;;If  SET1 is  a  subset of  the  universe of  SET2,  no elements  are
  ;;dropped, and the injection is returned.
  ;;
  (define who 'enum-set-projection)
  (with-arguments-validation (who)
      ((enum-set	set1)
       (enum-set	set2))
    (let* ((set2.type      (enum-set-type set2))
	   (universe2.stoi (enum-type-symbol-to-index set2.type))
	   (universe1.itos (enum-type-index-to-symbol (enum-set-type set1))))
      ;;For every symbol in SET1: check if it is also in the universe of
      ;;SET2; if it is: add it to the result.
      (let loop ((in-bits  (enum-set-bitvector set1))
		 (index1	 0)
		 (out-bits 0))
	(define-inline (next-bit-from-set1)
	  (loop (>> in-bits) (+ 1 index1) out-bits))
	(if (zero? in-bits)
	    (make-enum-set set2.type out-bits)
	  (if (%rightmost-bit-set-to-zero? in-bits)
	      (next-bit-from-set1)
	    (let ((index2 (symbol-to-index-ref universe2.stoi
					       (index-to-symbol-ref universe1.itos index1))))
	      (if index2
		  (loop (>> in-bits) (+ 1 index1)
			(bitwise-ior out-bits (%single-bitmask index2)))
		(next-bit-from-set1)))))))))


;;;; utilities

(define (%enum-set-for-all proc set)
  ;;Return true  if PROC applied to  each of the symbols  in SET returns
  ;;true; return false otherwise.
  ;;
  (let ((set.index-to-symbol (enum-type-index-to-symbol (enum-set-type set))))
    (let loop ((set.bitvector	(enum-set-bitvector set))
	       (index		0))
      (define-inline (next)
	(loop (>> set.bitvector) (+ 1 index)))
      (or (zero? set.bitvector)
	  (if (%rightmost-bit-set-to-zero? set.bitvector)
	      (next)
	    (and (proc (unsafe.vector-ref set.index-to-symbol index))
		 (next)))))))

(define make-file-options
  ;;This constructor builds empty enum sets.
  (enum-set-constructor (make-enumeration '(no-create no-fail no-truncate))))


;;;; done

)

;;; end of file

#;(library (ikarus enumerations)
  (export make-enumeration enum-set-universe enum-set-indexer
    enum-set-constructor enum-set->list enum-set-member?
    enum-set-subset? enum-set=? enum-set-union enum-set-difference
    enum-set-intersection enum-set-complement enum-set-projection
    make-file-options enum-set?)
  (import
(except (ikarus)
      make-enumeration enum-set-universe enum-set-indexer
      enum-set-constructor enum-set->list enum-set-member?
      enum-set-subset? enum-set=? enum-set-union enum-set-difference
      enum-set-intersection enum-set-complement
      enum-set-projection
      make-file-options enum-set?))

  (define-record-type enum-type
    (fields id mask symbol->index-hashtable index->symbol-vector)
    (sealed #t)
    (opaque #t)
    (nongenerative))

  (define-record-type enum-set
    (fields type bits)
    (sealed #t)
    (nongenerative))

  (define (make-enumeration ls)
    (unless (and (list? ls) (for-all symbol? ls))
      (die 'make-enumeration "not a list of symbols" ls))
    (let ([h (make-eq-hashtable)] [v (list->vector ls)])
      (let f ([i 0] [n (vector-length v)])
        (cond
          [(= i n)
           (let ([mask (sub1 (sll 1 n))])
             (let ([t (make-enum-type (gensym) mask h v)])
               (make-enum-set t mask)))]
          [else
           (hashtable-set! h (vector-ref v i) i)
           (f (+ i 1) n)]))))

  (define (enum-set-universe x)
    (unless (enum-set? x)
      (die 'enum-set-universe "not an enum set" x))
    (let ([t (enum-set-type x)])
      (make-enum-set (enum-set-type x) (enum-type-mask t))))

  (define (enum-set-indexer x)
    (unless (enum-set? x)
      (die 'enum-set-indexer "not an enum set" x))
    (let ([h (enum-type-symbol->index-hashtable (enum-set-type x))])
      (lambda (s)
        (unless (symbol? s)
          (die 'enum-set-indexer "not a symbol" s))
        (hashtable-ref h s #f))))

  (define (enum-set-constructor x)
    (unless (enum-set? x)
      (die 'enum-set-constructor "not an enum set" x))
    (let ([t (enum-set-type x)])
      (let ([h (enum-type-symbol->index-hashtable t)])
        (lambda (ls)
          (unless (list? ls) (die 'enum-set-constructor "not a list" ls))
          (let f ([ls ls] [n 0])
            (cond
              [(null? ls) (make-enum-set t n)]
              [else
               (f (cdr ls)
                  (bitwise-ior n
                    (sll 1
                      (or (hashtable-ref h (car ls) #f)
                          (die 'enum-set-constructor
                               "not in universe"
                               (car ls) t)))))]))))))

  (define (enum-set->list x)
    (unless (enum-set? x)
      (die 'enum-set->list "not an enum set" x))
    (let ([v (enum-type-index->symbol-vector (enum-set-type x))])
      (let ([n (vector-length v)])
        (let f ([bits (enum-set-bits x)] [i 0])
          (if (eqv? bits 0)
              '()
              (if (even? bits)
                  (f (sra bits 1) (+ i 1))
                  (cons (vector-ref v i)
                    (f (sra bits 1) (+ i 1)))))))))


  (define (enum-set-andmap proc x)
    (let ([v (enum-type-index->symbol-vector (enum-set-type x))])
      (let ([n (vector-length v)])
        (let f ([bits (enum-set-bits x)] [i 0])
          (if (= bits 0)
              #t
              (if (even? bits)
                  (f (sra bits 1) (+ i 1))
                  (and (proc (vector-ref v i))
                       (f (sra bits 1) (+ i 1)))))))))

  (define (enum-set-member? s x)
    (unless (enum-set? x)
      (die 'enum-set-member? "not an enum set" x))
    (let ([h (enum-type-symbol->index-hashtable (enum-set-type x))])
      (let ([idx (hashtable-ref h s #f)])
        (cond
          [idx (bitwise-bit-set? (enum-set-bits x) idx)]
          [(symbol? s) #f]
          [else (die 'enum-set-member? "not a symbol" s)]))))

  (define (enum-set-subset? x1 x2)
    (unless (enum-set? x1)
      (die 'enum-set-subset? "not an enum set" x1))
    (unless (enum-set? x2)
      (die 'enum-set-subset? "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (if (or (eq? t1 t2) (eq? (enum-type-id t1) (enum-type-id t2)))
          (let ([b1 (enum-set-bits x1)] [b2 (enum-set-bits x2)])
            (= (bitwise-and b1 b2) b1))
          (and (enum-set-andmap (lambda (s) (enum-set-member? s x2)) x1)
               (let ([u2 (enum-set-universe x2)])
                 (enum-set-andmap (lambda (s) (enum-set-member? s u2))
                   (enum-set-universe x1)))))))


  (define (enum-set=? x1 x2)
    (unless (enum-set? x1)
      (die 'enum-set=? "not an enum set" x1))
    (unless (enum-set? x2)
      (die 'enum-set=? "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (if (or (eq? t1 t2) (eq? (enum-type-id t1) (enum-type-id t2)))
          (= (enum-set-bits x1) (enum-set-bits x2))
          (and (enum-set-andmap (lambda (s) (enum-set-member? s x2)) x1)
               (enum-set-andmap (lambda (s) (enum-set-member? s x1)) x2)
               (let ([u1 (enum-set-universe x1)] [u2 (enum-set-universe x2)])
                 (and
                   (enum-set-andmap (lambda (s) (enum-set-member? s u2)) u1)
                   (enum-set-andmap (lambda (s) (enum-set-member? s u1)) u2)))))))

  (define (enum-set-op x1 x2 who combine)
    (unless (enum-set? x1)
      (die who "not an enum set" x1))
    (unless (enum-set? x2)
      (die who "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (if (or (eq? t1 t2) (eq? (enum-type-id t1) (enum-type-id t2)))
          (make-enum-set t1 (combine (enum-set-bits x1) (enum-set-bits x2)))
          (die who "enum sets have different enumeration types" x1 x2))))

  (define (enum-set-union x1 x2)
    (enum-set-op x1 x2 'enum-set-union bitwise-ior))

  (define (enum-set-intersection x1 x2)
    (enum-set-op x1 x2 'enum-set-intersection bitwise-and))

  (define (enum-set-difference x1 x2)
    (enum-set-op x1 x2 'enum-set-difference
      (lambda (n1 n2) (bitwise-and n1 (bitwise-not n2)))))

  (define (enum-set-complement x)
    (define who 'enum-set-complement)
    (unless (enum-set? x)
      (die who "not an enum set" x))
    (let ([t (enum-set-type x)])
      (make-enum-set t
        (bitwise-and
          (enum-type-mask t)
          (bitwise-not (enum-set-bits x))))))

  (define (enum-set-projection x1 x2)
    (define who 'enum-set-projection)
    (unless (enum-set? x1) (die who "not an enum set" x1))
    (unless (enum-set? x2) (die who "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (let ([h (enum-type-symbol->index-hashtable t2)]
            [v (enum-type-index->symbol-vector t1)])
        (let f ([in-bits (enum-set-bits x1)] [i 0] [out-bits 0])
          (if (= in-bits 0)
              (make-enum-set t2 out-bits)
              (if (even? in-bits)
                  (f (sra in-bits 1) (+ i 1) out-bits)
                  (let ([idx (hashtable-ref h (vector-ref v i) #f)])
                    (if idx
                        (f (sra in-bits 1) (+ i 1)
                           (bitwise-ior out-bits (sll 1 idx)))
                        (f (sra in-bits 1) (+ i 1)
                           out-bits)))))))))

  (define make-file-options
    ;;This constructor builds empty enum sets.
    (enum-set-constructor
      (make-enumeration
        '(no-create no-fail no-truncate))))

)

