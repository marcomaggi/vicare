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
    weak-hashtable-size		weak-hashtable-delete!
    weak-hashtable-contains?	weak-hashtable-clear!
    weak-hashtable-keys		weak-hashtable-entries
    weak-hashtable-update!)
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

(define-argument-validation (dimension who obj)
  (and (fixnum? obj) (fx<= 0 obj))
  (assertion-violation who
    "expected non-negative fixnum as initial weak table size" obj))


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
;;  A bitmask used to convert a keys' hash number into an index into the
;;  vector of buckets as follows:
;;
;;    (define index ($fxlogand mask (hash-function object)))
;;
;;  The mask is always the number of buckets minus 1.
;;
;;Field name: buckets
;;Accessor: weak-table-buckets TABLE
;;Mutator: set-weak-table-buckets! TABLE
;;
;;  The vector of buckets of the hash table.  Each element in the vector
;;  is a list of weak pairs holding the entries as key/value weak pairs.
;;  The  maximum number  of buckets  is  half the  greatest fixnum;  the
;;  number of buckets must be an exact power of 2.
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

(define-inline (%compute-bucket-index table key)
  (unsafe.fxand ((weak-table-hash-function table) key) (weak-table-mask table)))

(define (%intern! table bucket-index key value)
  ;;If KEY is not already interned: insert a new entry holding KEY/VALUE
  ;;in TABLE at BUCKET-INDEX.  If KEY is already interned: overwrite the
  ;;old value with VALUE.
  ;;
  (define who 'intern!)
  (let ((number-of-entries (weak-table-size table)))
    (if (unsafe.fx= number-of-entries (greatest-fixnum))
	(assertion-violation who
	  "reached maximum number of entries in weak table"
	  table key value)
      (begin
	(let* ((buckets (weak-table-buckets table))
	       (entries (unsafe.vector-ref buckets bucket-index)))
	  (if (null? entries)
	      (unsafe.vector-set! buckets bucket-index
				  (weak-cons (weak-cons key value) '()))
	    ;;If  the key is  already interned:  overwrite the  old value;
	    ;;else append a new weak pair to the chain of entries.
	    (let loop ((equiv?	(weak-table-equiv-function table))
		       (prev    entries)
		       (entries (unsafe.cdr entries)))
	      (if (null? entries) ;key not found
		  (unsafe.set-cdr! prev (weak-cons (weak-cons key value) '()))
		(let ((intern-key (unsafe.car (unsafe.car entries))))
		  (if (or (eq?    key intern-key)
			  (equiv? key intern-key))
		      (unsafe.set-cdr! (unsafe.car entries) value)
		    (loop equiv? entries (unsafe.cdr entries))))))))
	((weak-table-guardian table) key)
	(let ((N (unsafe.fxadd1 number-of-entries)))
	  (set-weak-table-size! table N)
	  (when (unsafe.fx= N (weak-table-mask table))
	    (%extend-table! table)))))))

(define (%unintern! table key bucket-index)
  ;;Remove  the entry  associated to  KEY from  TABLE in  the  bucket at
  ;;BUCKET-INDEX.  If KEY is not found: just do nothing.
  ;;
  (let* ((buckets (weak-table-buckets        table))
	 (equiv?  (weak-table-equiv-function table)))
    (let ((entries (unsafe.vector-ref buckets bucket-index)))
      (if (null? entries) ;empty bucket
	  (values)	  ;key not found
	;;Check separately the first entry  because if we have to remove
	;;it we must also update the value in the bucket.
	(let ((intern-key (unsafe.car (unsafe.car entries))))
	  (if (or (eq?    key intern-key)
		  (equiv? key intern-key))
	      (begin
		(set-weak-table-size! table (unsafe.fxsub1 (weak-table-size table)))
		(unsafe.vector-set! buckets bucket-index (unsafe.cdr entries)))
	    ;;Now check the tail of the chain.
	    (let loop ((prev    entries)
		       (entries (unsafe.cdr entries)))
	      (if (null? entries)
		  (values) ;key not found
		(let ((intern-key (unsafe.car (unsafe.car entries))))
		  (if (or (eq?    key intern-key)
			  (equiv? key intern-key))
		      (begin
			(set-weak-table-size! table (unsafe.fxsub1 (weak-table-size table)))
			(unsafe.set-cdr! prev (unsafe.cdr entries)))
		    (loop entries (unsafe.cdr entries))))))))))))

(define (%extend-table! table)
  ;;Unless the number  of buckets is already at  its maximum: double the
  ;;size of the vector in TABLE, which must be an instance of WEAK-TABLE
  ;;structure.
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
	    (let ((entry (unsafe.car p))
		  (rest  (unsafe.cdr p)))
	      ;;Recycle this pair by setting its cdr to the value in the
	      ;;vector.
	      (let ((idx (unsafe.fxand (hash (unsafe.car entry)) mask)))
		(unsafe.set-cdr! p (unsafe.vector-ref vec2 idx))
		(unsafe.vector-set! vec2 idx p))
	      (%insert rest))))
	;;Insert in the new vector all the entries in the old vector.
	(vector-for-each %insert vec1)
	;;Update the TABLE structure.
	(set-weak-table-buckets! table vec2)
	(set-weak-table-mask!    table mask)))))


;;;; high-level operations

(define make-weak-hashtable
  (case-lambda
   ((hash-function equiv-function)
    (make-weak-hashtable hash-function equiv-function 16))
   ((hash-function equiv-function init-dimension)
    (define who 'make-weak-hashtable)
    (with-arguments-validation (who)
	((procedure	hash-function)
	 (procedure	equiv-function)
	 (dimension	init-dimension))
      (let* ((guardian	(make-guardian))
	     ;;Smallest power of 2 greater than INIT-DIMENSION.
	     (dim	(fxlength init-dimension))
	     (mask	(fxsub1 dim))
	     (buckets	(make-vector dim '()))
	     (table	(make-weak-table 0 mask buckets guardian hash-function equiv-function)))
	(define (cleanup)
	  (do ((key (guardian) (guardian)))
	      ((not key))
	    (%unintern! table key (%compute-bucket-index table key))))
	(post-gc-hooks (cons cleanup (post-gc-hooks)))
	table)))))

(define weak-hashtable? weak-table?)

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
	   (buckets	(weak-table-buckets        table))
	   (entries	(unsafe.vector-ref buckets (%compute-bucket-index table key))))
      (let loop ((entries entries))
	(if (null? entries)
	    default
	  (let ((intern-key (unsafe.car (unsafe.car entries))))
	    (if (or (eq?    key intern-key)
		    (equiv? key intern-key))
		(unsafe.cdr (unsafe.car entries))
	      (loop (unsafe.cdr entries)))))))))

(define (weak-hashtable-delete! table key)
  (define who 'weak-hashtable-ref)
  (with-arguments-validation (who)
      ((weak-table	table))
    (%unintern! table key (%compute-bucket-index table key))))

(define (weak-hashtable-contains? table key)
  (define who 'weak-hashtable-contains?)
  (with-arguments-validation (who)
      ((weak-table	table))
    (let* ((equiv?	(weak-table-equiv-function table))
	   (buckets	(weak-table-buckets        table))
	   (entries	(unsafe.vector-ref buckets (%compute-bucket-index table key))))
      (let loop ((entries entries))
	(if (null? entries)
	    #f
	  (let ((intern-key (unsafe.car (unsafe.car entries))))
	    (if (or (eq?    key intern-key)
		    (equiv? key intern-key))
		#t
	      (loop (unsafe.cdr entries)))))))))

(define (weak-hashtable-clear! table)
  (let* ((dim		(fxlength 16))
	 (mask		(fxsub1 dim))
	 (buckets	(make-vector dim '())))
    (set-weak-table-size!     table 0)
    (set-weak-table-buckets!  table buckets)
    (set-weak-table-mask!     table mask)
    (set-weak-table-guardian! table (make-guardian))))

(define (weak-hashtable-keys table)
  (let* ((keys		(make-vector (weak-table-size table)))
	 (buckets	(weak-table-buckets table))
	 (dim		(unsafe.vector-length buckets))
	 (count		0))
    (do ((i 0 (unsafe.fxadd1 i)))
	((unsafe.fx= i dim)
	 keys)
      (let loop ((entries (unsafe.vector-ref buckets i)))
	(unless (null? entries)
	  (unsafe.vector-set! keys count (unsafe.car (unsafe.car entries)))
	  (set! count (unsafe.fxadd1 count))
	  (loop (unsafe.cdr entries)))))))

(define (weak-hashtable-entries table)
  (let* ((size		(weak-table-size table))
	 (keys		(make-vector size))
	 (vals		(make-vector size))
	 (buckets	(weak-table-buckets table))
	 (dim		(unsafe.vector-length buckets))
	 (count		0))
    (do ((i 0 (unsafe.fxadd1 i)))
	((unsafe.fx= i dim)
	 (values keys vals))
      (let loop ((entries (unsafe.vector-ref buckets i)))
	(unless (null? entries)
	  (let ((entry (unsafe.car entries)))
	    (unsafe.vector-set! keys count (unsafe.car entry))
	    (unsafe.vector-set! vals count (unsafe.cdr entry))
	    (set! count (unsafe.fxadd1 count)))
	  (loop (unsafe.cdr entries)))))))

(define weak-hashtable-size weak-table-size)

(define (weak-hashtable-update! table key proc default)
  (define who 'weak-hashtable-update!)
  (with-arguments-validation (who)
      ((weak-table	table)
       (procedure	proc))
    (let* ((equiv?	(weak-table-equiv-function table))
	   (buckets	(weak-table-buckets        table))
	   (idx		(%compute-bucket-index table key))
	   (the-entries	(unsafe.vector-ref buckets idx)))
      (let loop ((entries the-entries))
	(if (null? entries)
	    ;;add a new entry
	    (unsafe.vector-set! buckets idx (weak-cons (weak-cons key (proc default))
						       the-entries))
	  (let* ((entry      (unsafe.car entries))
		 (intern-key (unsafe.car entry)))
	    (if (or (eq?    key intern-key)
		    (equiv? key intern-key))
		(set-cdr! entry (proc (unsafe.cdr entry)))
	      (loop (unsafe.cdr entries)))))))))


;;;; done

)

;;; end of file
