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
;;This  library defines  enumeration type  objects and  enumeration set  objects: the
;;enumeration type holds  the set of Scheme  symbols in a vector, mapping  each to an
;;index; the enumeration sets references an enumeration type.
;;
;;		 ------		 ------
;;		| type |<-------| set1 |
;;		 ------		 ------
;;                 ^
;;		   |		 ------
;;		    ------------| set2 |
;;				 ------
;;
;;An enumeration set holds  a vector of bits (an exact integer),  one for each symbol
;;in the set: if a bit is set to 1 the corresponding symbol is contained.  The offset
;;of a bit in the set's bitvector is the index of the symbol in the type's vector.
;;
;;This way:  the union  between sets  is the bitwise  inclusive OR,  the intersection
;;between sets is the bitwise AND, the complement of a set is the bitwise NOT.
;;
;;Notice that a Vicare's  fixnum is a machine word and a Vicare's  bignum is an array
;;of machine words with a single machine  word of meta data; storing the bitvector in
;;an exact integer is as efficient as possible.
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
    make-file-options
    make-expander-options
    make-compiler-options)
  (import (except (vicare)
		  ;;FIXME  This except  is  to  be removed  at  the  next boot  image
		  ;;rotation.  (Marco Maggi; Wed Apr 1, 2015)
		  list-of-symbols?

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
		  make-file-options
		  make-expander-options
		  make-compiler-options)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Wed Apr 1,
    ;;2015)
    (only (ikarus.symbols)
	  list-of-symbols?)
    (vicare language-extensions syntaxes)
    (vicare unsafe operations)
    (vicare arguments validation))


;;;; record types

(define-struct enum-type
  (symbol-to-index
		;Hashtable mapping symbols to indexes.
   index-to-symbol
		;Vector mapping indexes to symbols.
   universe-bitvector
		;Exact integer,  interpreted as  vector of bits,  having a  number of
		;bits equal to the number of symbols, all set to 1.
   universe
		;Enum set representing  the universe of this type.   Enum set records
		;are immutable, so it is all right to have a prebuilt universe.
		;
		;This field is  mutable because of the  chicken-and-egg problem while
		;building the type's record.
   ))

(define-struct enum-set
  (type
		;Instance of ENUM-TYPE representing the type of this enumeration set.
   bitvector
		;Exact  integer having  one bit  set  to 1  for each  symbol in  this
		;enumeration set, with respect to the UNIVERSE-BITVECTOR field in the
		;ENUM-TYPE value.
   ))


;;;; helpers

(define (make-symbol-to-index-map list-of-symbols)
  (let ((symbol-to-index (make-eq-hashtable)))
    (let next-symbol ((index		0)
		      (list-of-symbols	list-of-symbols))
      (if (null? list-of-symbols)
	  symbol-to-index
	(begin
	  (symbol-to-index-set! symbol-to-index (car list-of-symbols) index)
	  (next-symbol (+ 1 index) (cdr list-of-symbols)))))))
(define symbol-to-index-set!		hashtable-set!)
(define-syntax-rule (symbol-to-index-ref table key)
  (hashtable-ref table key #f))
(define symbol-to-index-size		hashtable-size)

(define-syntax-rule (make-index-to-symbol-map list-of-symbols)
  (list->vector list-of-symbols))
(define-syntax-rule (index-to-symbol-set! vector index value)
  ($vector-set! vector index value))
(define-syntax-rule (index-to-symbol-ref vector index)
  ($vector-ref vector index))
(define-syntax-rule (index-to-symbol-size vector)
  ($vector-length vector))

;;Applied to an  exact integer representing a bivector: return  true if the rightmost
;;bit is set to zero.  This means that  the symbol associated to the rightmost bit is
;;not in the set.
;;
(define-syntax-rule (%rightmost-bit-set-to-zero? bitvector)
  (even? bitvector))

(define-syntax-rule (>> bitvector)
  (bitwise-arithmetic-shift-right bitvector 1))


;;;; bitmasks

(define-syntax-rule (%full-bitmask number-of-bits)
  ;;Return an exact integer having at least NUMBER-OF-BITS all set to 1 and the other
  ;;bits set to zero.
  ;;
  (- (bitwise-arithmetic-shift-left 1 number-of-bits) 1)
  #;(- (expt 2 number-of-bits) 1))

(define-syntax-rule (%single-bitmask bit-offset)
  ;;Return an exact integer  having the bit at BIT-OFFSET set to 1  and all the other
  ;;bits set to zero.
  ;;
  (bitwise-arithmetic-shift-left 1 bit-offset)
  #;(expt 2 bit-offset))


(define* (make-enumeration {ell list-of-symbols?})
  ;;Create a  new enumeration type whose  universe consists of the  Scheme symbols in
  ;;ELL (in  canonical order of  their first appearance in  the ELL) and  return that
  ;;universe as  an enumeration set  whose universe  is itself and  whose enumeration
  ;;type is the newly created enumeration type.
  ;;
  (let* ((symbol-to-index	(make-symbol-to-index-map ell))
	 (index-to-symbol	(make-index-to-symbol-map ell))
	 (universe-bitvector	(%full-bitmask (index-to-symbol-size index-to-symbol)))
	 (type			(make-enum-type symbol-to-index index-to-symbol universe-bitvector #f)))
    (receive-and-return (universe)
	(make-enum-set type universe-bitvector)
      (set-enum-type-universe! type universe))))


;;;; inspection and conversion

(define* (enum-set-universe {set enum-set?})
  ;;Return the set of  all symbols that comprise the universe of  its argument, as an
  ;;enumeration set.
  ;;
  (enum-type-universe (enum-set-type set)))

(define* (enum-set-indexer {set enum-set?})
  ;;Return a  unary procedure that, given  a symbol that  is in the universe  of SET,
  ;;returns its zero-based index within the  canonical ordering of the symbols in the
  ;;universe; given a symbol not in the universe, the unary procedure returns false.
  ;;
  (let ((table (enum-type-symbol-to-index (enum-set-type set))))
    (lambda (symbol)
      (unless (symbol? symbol)
	(procedure-argument-violation 'anonymous-enumeration-indexer
	  "expected symbol as argument" symbol))
      (symbol-to-index-ref table symbol))))

(define* (enum-set-constructor {set enum-set?})
  ;;Return  a unary  procedure that,  given  a list  of  symbols that  belong to  the
  ;;universe of  SET, returns  a subset  of that universe  that contains  exactly the
  ;;symbols in the list.  The values in the list must all belong to the universe.
  ;;
  (let* ((set.type	(enum-set-type set))
	 (set.stoi	(enum-type-symbol-to-index set.type)))
    (fluid-let-syntax
	((__who__ (identifier-syntax 'anonymous-enumeration-set-constructor)))
      (define (make-bitmask-for-symbol sym)
	(%single-bitmask (or (symbol-to-index-ref set.stoi sym)
			     (assertion-violation __who__
			       "given symbol is not in universe of enumeration set" sym set.type))))
      (lambda (ell)
	(unless (list-of-symbols? ell)
	  (procedure-argument-violation __who__ "expected list of symbols as argument" ell))
	(let loop ((ell			ell)
		   (newset.bitvector	0))
	  (if (null? ell)
	      (make-enum-set set.type newset.bitvector)
	    (loop (cdr ell) (bitwise-ior newset.bitvector (make-bitmask-for-symbol (car ell))))))))))

(define* (enum-set->list {set enum-set?})
  ;;Return a list of the symbols that  belong to its argument, in the canonical order
  ;;of the universe of SET.
  ;;
  (let ((set.index-to-symbol (enum-type-index-to-symbol (enum-set-type set))))
    (let loop ((bitvector	(enum-set-bitvector set))
	       (index		0)
	       (ell		'()))
      (if (zero? bitvector)
	  (reverse ell)
	(loop (>> bitvector) (+ 1 index)
	      (if (%rightmost-bit-set-to-zero? bitvector)
		  ell
		(cons (index-to-symbol-ref set.index-to-symbol index) ell)))))))


;;;; membership and equality

(define* (enum-set-member? {sym symbol?} {set enum-set?})
  ;;Return true if the SYM is a member of SET.
  ;;
  (let ((index (symbol-to-index-ref (enum-type-symbol-to-index (enum-set-type set)) sym)))
    (and index (bitwise-bit-set? (enum-set-bitvector set) index))))

(define* (enum-set-subset? {set1 enum-set?} {set2 enum-set?})
  ;;Return true if the universe of SET1 is a subset of the universe of SET2 and every
  ;;element of SET1 is a member of SET2; return false otherwise.
  ;;
  ;;If the type is the same: the universe of SET1 is a subset of the universe of SET2
  ;;(the two being equal).
  (if (eq? (enum-set-type set1) (enum-set-type set2))
      (let ((set1.bitvector (enum-set-bitvector set1))
	    (set2.bitvector (enum-set-bitvector set2)))
	;;Tests that all the elements of SET1 are in SET2.
	(= set1.bitvector (bitwise-and set1.bitvector set2.bitvector)))
    ;;The type is different: check, element by element, that all the elements of SET1
    ;;are in SET2; if yes: check, element by  element, that the universe of SET1 is a
    ;;subset of the universe of SET2.
    (and (%enum-set-for-all (lambda (sym-from-set1)
			      (enum-set-member? sym-from-set1 set2))
			    set1)
	 (let ((set2.universe (enum-set-universe set2)))
	   (%enum-set-for-all (lambda (sym-from-set1)
				(enum-set-member? sym-from-set1 set2.universe))
			      (enum-set-universe set1))))))

(define* (enum-set=? {set1 enum-set?} {set2 enum-set?})
  ;;Return true  if SET1 is  a subset of  SET2 and vice  versa, as determined  by the
  ;;ENUM-SET-SUBSET?  procedure; this implies that the  universes of the two sets are
  ;;equal as sets of  symbols, but does not imply that they  are equal as enumeration
  ;;types (that is: the order of the symbols is different).  Otherwise return false.
  ;;
  (if (eq? (enum-set-type set1) (enum-set-type set2))
      (= (enum-set-bitvector set1) (enum-set-bitvector set2))
    (and (%enum-set-for-all (lambda (sym) (enum-set-member? sym set2)) set1)
	 (%enum-set-for-all (lambda (sym) (enum-set-member? sym set1)) set2)
	 (let ((set1.universe (enum-set-universe set1))
	       (set2.universe (enum-set-universe set2)))
	   (and (%enum-set-for-all (lambda (s) (enum-set-member? s set2.universe)) set1.universe)
		(%enum-set-for-all (lambda (s) (enum-set-member? s set1.universe)) set2.universe))))))


;;;; set operations

(define (%enum-set-op set1 set2 who combine)
  ;;Perform  the  COMBINE operation  between  SET1  and  SET2; return  the  resulting
  ;;enumeration set.
  ;;
  (let ((set1.type (enum-set-type set1)))
    (unless (eq? set1.type (enum-set-type set2))
      (procedure-argument-violation who "expected enumeration sets with the same enumeration type" set1 set2))
    (make-enum-set set1.type (combine (enum-set-bitvector set1) (enum-set-bitvector set2)))))

(define* (enum-set-union {set1 enum-set?} {set2 enum-set?})
  (%enum-set-op set1 set2 __who__ bitwise-ior))

(define* (enum-set-intersection {set1 enum-set?} {set2 enum-set?})
  (%enum-set-op set1 set2 __who__ bitwise-and))

(define* (enum-set-difference {set1 enum-set?} {set2 enum-set?})
  (%enum-set-op set1 set2 __who__
		(lambda (bitvector1 bitvector2)
		  (bitwise-and bitvector1 (bitwise-not bitvector2)))))

;;; --------------------------------------------------------------------

(define* (enum-set-complement {set enum-set?})
  (let ((set.type (enum-set-type set)))
    (make-enum-set set.type
		   (bitwise-and (enum-type-universe-bitvector set.type)
				(bitwise-not (enum-set-bitvector set))))))

(define* (enum-set-projection {set1 enum-set?} {set2 enum-set?})
  ;;Project SET1 into the universe of SET2, dropping any elements of SET1 that do not
  ;;belong to the universe of SET2.
  ;;
  ;;If SET1  is a subset of  the universe of SET2,  no elements are dropped,  and the
  ;;injection is returned.
  ;;
  (let* ((set2.type      (enum-set-type set2))
	 (universe2.stoi (enum-type-symbol-to-index set2.type))
	 (universe1.itos (enum-type-index-to-symbol (enum-set-type set1))))
    ;;For every symbol  in SET1: check if it  is also in the universe of  SET2; if it
    ;;is: add it to the result.
    (let loop ((in-bits  (enum-set-bitvector set1))
	       (index1	 0)
	       (out-bits 0))
      (define-syntax-rule (next-bit-from-set1)
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
	      (next-bit-from-set1))))))))


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
	    (and (proc ($vector-ref set.index-to-symbol index))
		 (next)))))))

(define make-file-options
  ;;This constructor builds empty enum sets.
  (enum-set-constructor (make-enumeration '(no-create no-fail no-truncate executable))))

(define make-expander-options
  ;;This constructor builds empty enum sets.
  (enum-set-constructor (make-enumeration '(strict-r6rs tagged-language))))

(define make-compiler-options
  ;;This constructor builds empty enum sets.
  (enum-set-constructor (make-enumeration '(strict-r6rs))))


;;;; done

#| end of library |# )

;;; end of file
