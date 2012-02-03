;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: weak hash tables
;;;Date: Fri Feb  3, 2012
;;;
;;;Abstract
;;;
;;;	The  code  in   this  library  is  derived  from   the  code  in
;;;	"ikarus.symbol-table.sls".
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare weak-hashtables)
  (export
    make-weak-hashtable		weak-hashtable?
    weak-hashtable-set!		weak-hashtable-ref
    weak-hashtable-size		weak-hashtable-delete!)
  (import (vicare)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (vicare syntactic-extensions))


;;;; arguments validation

(define-argument-validation (weak-table who obj)
  (weak-table? obj)
  (assertion-violation who "expected weak hashtable as argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

;; (define-argument-validation (dimension who obj)
;;   (and (fixnum? obj) (fx<= 0 obj))
;;   (assertion-violation who
;;     "expected non-negative fixnum as initial weak table size" obj))


;;;; weak table data structure
;;
;;A weak hashtable is a vector  holding nulls or alists composed of weak
;;pairs:
;;
;;   |-----|-----|-----|-----|-----| vector of buckets
;;            |
;;            v
;;         |-----|-----| weak pair
;;            |      |
;;            v      -------> |-----|-----| weak pair
;;   |-----|-----|weak pair      |     |
;;     key  value                v      -> null
;;                          |-----|-----| weak pair
;;                            key  value
;;
;;When  the number  of collected  objects equals  the number  of buckets
;;(whatever the distribution), the table is enlarged doubling the number
;;of buckets.  The  table is never restricted by  reducing the number of
;;buckets.
;;
;;Constructor: make-weak-table SIZE MASK VECTOR GUARDIAN
;;
;;Predicate: weak-table? OBJ
;;
;;Field name: size
;;Accessor: weak-table-size TABLE
;;Mutator: set-weak-table-size! TABLE
;;  The number of  entries in the table.  The  maximum number of entries
;;  is the greatest fixnum.
;;
;;Field name: mask
;;Accessor: weak-table-mask TABLE
;;Mutator: set-weak-table-mask! TABLE
;;  A bitmask used to convert an object's hash number into an index into
;;  the vector in the VEC field as follows:
;;
;;    (define index ($fxlogand mask (hash-function object)))
;;
;;Field name: buckets
;;Accessor: weak-table-buckets TABLE
;;Mutator: set-weak-table-buckets! TABLE
;;  The vector of buckets of the hash table.  Each element in the vector
;;  is a list of weak pairs holding the entries as key/value pairs.  The
;;  maximum number of buckets is half the greatest fixnum.
;;
;;Field name: guardian
;;Accessor: weak-table-guardian TABLE
;;Mutator: set-weak-table-guardian! TABLE
;;  A  guardian in  which  interned keys  are  registered.  Whenever  an
;;  interned  key  is  garbage  collected,  the  guardian  extracts  the
;;  associated pair from the table.
;;
;;Field name: hash-function
;;Accessor: weak-table-hash-function
;;Mutator: set-weak-table-hash-function!
;;  The function used to compute the hash value of a given key.  It must
;;  accept one argument and return one argument.
;;
;;Field name: equiv-function
;;Accessor: weak-table-equiv-function
;;Mutator: set-weak-table-equiv-function!
;;  The function used to compare two keys.  It must accept two arguments
;;  and return a single value, true if the keys are equal.
;;
(define-struct weak-table
  (size mask buckets guardian hash-function equiv-function))


;;;; low-level operations

(define MAX-NUMBER-OF-BUCKETS
  (fxdiv (greatest-fixnum) 2))

(define (%intern! table bucket-index key value)
  (define who 'intern!)
  (let ((number-of-entries (weak-table-size table)))
    (if (unsafe.fx= number-of-entries (greatest-fixnum))
	(assertion-violation who
	  "reached maximum number of entries in weak table"
	  table bucket-index key value)
      (let ((buckets (weak-table-buckets table)))
	(unsafe.vector-set! buckets bucket-index
			    (weak-cons (weak-cons key value)
				       (unsafe.vector-ref buckets bucket-index)))
	((weak-table-guardian table) key)
	(let ((N (unsafe.fxadd1 number-of-entries)))
	  (set-weak-table-size! table N)
	  (when (unsafe.fx= N (weak-table-mask table))
	    (%extend-table! table)))))))

(define (%unintern! table key bucket-index)
  (set-weak-table-size! table (unsafe.fxsub1 (weak-table-size table)))
  (let* ((buckets (weak-table-buckets table))
	 (ls      (unsafe.vector-ref buckets bucket-index)))
    (if (eq? (unsafe.car ls) key)
	(unsafe.vector-set! buckets bucket-index (cdr ls))
      (let loop ((prev ls)
		 (ls   (unsafe.cdr ls)))
	(if (eq? (unsafe.car ls) key)
	    (unsafe.set-cdr! prev (unsafe.cdr ls))
	  (loop ls (unsafe.cdr ls)))))))

(define (%extend-table! table)
  ;;Double the size of the vector in TABLE, which must be an instance of
  ;;WEAK-TABLE structure.
  ;;
  (let* ((vec1	(weak-table-buckets table))
	 (len1	(unsafe.vector-length vec1))
	 (hash	(weak-table-hash-function table)))
    ;;Do not allow the vector length to exceed the maximum fixnum.
    (when (unsafe.fx< len1 MAX-NUMBER-OF-BUCKETS)
      ;;If  we start  with an  even and  power of  2 vector  length, the
      ;;length is always even and power of 2...
      (let* ((len2	(unsafe.fx+ len1 len1))
	     ;;... and the mask is always composed by all the significant
	     ;;bits set to 1.
	     (mask	(unsafe.fxsub1 len2))
	     (vec2	(make-vector len2 '())))
	(define (%insert p)
	  (unless (null? p)
	    (let ((a    (unsafe.car p))
		  (rest (unsafe.cdr p)))
	      ;;Recycle this pair by setting its cdr to the value in the
	      ;;vector.
	      (let ((idx (unsafe.fxand (hash (unsafe.car a)) mask)))
		(unsafe.set-cdr! p (unsafe.vector-ref vec2 idx))
		(unsafe.vector-set! vec2 idx p))
	      (%insert rest))))
	;;Insert in the new vector all the entries in the old vector.
	(vector-for-each %insert vec1)
	;;Update the TABLE structure.
	(set-weak-table-buckets! table vec2)
	(set-weak-table-mask!    table mask)))))


;;;; high-level operations

(define (make-weak-hashtable hash-function equiv-function)
  (define who 'make-weak-hashtable)
  (with-arguments-validation (who)
      ((procedure	hash-function)
       (procedure	equiv-function))
    (let* ((guardian	(make-guardian))
	   (buckets	(make-vector 4096 '()))
	   (table	(make-weak-table 0 4095 buckets guardian hash-function equiv-function)))
      (define (cleanup)
	(do ((obj (guardian) (guardian)))
	    ((not obj))
	  (%unintern! obj table)))
      (post-gc-hooks (cons cleanup (post-gc-hooks)))
      table)))

(define weak-hashtable? weak-table?)

(define-inline (%compute-bucket-index table key)
  (let ((table table))
    (unsafe.fxand ((weak-table-hash-function table) key) (weak-table-mask table))))

(define (weak-hashtable-set! table key value)
  (define who 'weak-hashtable-set!)
  (with-arguments-validation (who)
      ((weak-table	table))
    (%intern! table (%compute-bucket-index table key) key value)))

(define (weak-hashtable-ref table key default)
  (define who 'weak-hashtable-ref)
  (with-arguments-validation (who)
      ((weak-table	table))
    (let* ((equiv?	(weak-table-equiv-function table))
	   (buckets	(weak-table-buckets table))
	   (ls		(unsafe.vector-ref buckets (%compute-bucket-index table key))))
(pretty-print (list 'chain ls))
      (let loop ((ls ls))
	(if (null? ls)
	    default
	  (let ((intern-key (caar ls)))
;(pretty-print (list 'testing intern-key key))
	    (if (or (eq? key intern-key)
		    (equiv? key intern-key))
		(unsafe.cdr (unsafe.car ls))
	      (loop (unsafe.cdr ls)))))))))

(define (weak-hashtable-delete! table key)
  (define who 'weak-hashtable-ref)
  (with-arguments-validation (who)
      ((weak-table	table))
    (%unintern! table key (%compute-bucket-index table key))))

(define weak-hashtable-size weak-table-size)


;;;; done

)

;;; end of file
