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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers weak-hashtables)
  (export
    make-weak-hashtable		weak-hashtable?
    weak-hashtable-set!		weak-hashtable-ref
    weak-hashtable-size		weak-hashtable-delete!
    weak-hashtable-contains?	weak-hashtable-clear!
    weak-hashtable-keys		weak-hashtable-entries
    weak-hashtable-update!)
  (import (vicare)
    (vicare unsafe operations)
    (vicare language-extensions syntaxes)
    (vicare arguments validation))


;;;; arguments validation

(define-argument-validation (weak-hashtable who obj)
  (weak-table? obj)
  (assertion-violation who "expected weak hashtable as argument" obj))

(define-argument-validation (dimension who obj)
  (and (fixnum? obj) (fx<= 0 obj))
  (assertion-violation who
    "expected non-negative fixnum as initial weak table size" obj))


;;;; weak table data structure
;;
;;A weak hashtable is a vector  holding nulls or alists; alists have the
;;spine composed of strong pairs, while the entries are weak pairs:
;;
;;   |-----|-----|-----|-----|-----| vector of buckets
;;            |
;;            v
;;         |-----|-----| pair
;;            |      |
;;            v      -------> |-----|-----| pair
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
;;Constructor: make-weak-table SIZE INIT-DIM MASK VECTOR HASH-FUNCTION EQUIV-FUNCTION
;;
;;Predicate: weak-table? OBJ
;;
;;Field name: size
;;Accessor: weak-table-size TABLE
;;Mutator: set-weak-table-size! TABLE
;;  The number of  entries in the table.  The  maximum number of entries
;;  is the greatest fixnum.
;;
;;Field name: init-dim
;;Accessor: weak-table-init-dim TABLE
;;Mutator: set-weak-table-init-dim! TABLE
;;  The initial number of buckets.  It is used when clearing a table.
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
;;  The vector of buckets of the hash table.  Each element in the vector
;;  is a list of weak pairs holding the entries as key/value weak pairs.
;;  The maximum number of buckets  is the greatest fixnum; the number of
;;  buckets must be an exact power of 2.
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
  (size init-dim mask buckets hash-function equiv-function))

(define (%struct-weak-table-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%newline)
    (newline port))
  (%display "#[weak-table")
  (%display " size=")		(%display (weak-table-size S))
  (%display " init-dim=")	(%display (weak-table-init-dim S))
  (%display " mask=")		(%display (number->string (weak-table-mask S) 2))
  (let* ((buckets (weak-table-buckets S))
	 (nbucks  ($vector-length buckets)))
    (%display " num-of-buckets=")	(%display nbucks)
    (do ((i 0 ($fxadd1 i)))
	(($fx= i nbucks))
      (%newline)
      (%display " bucket[")
      (%display i)
      (%display "]=")
      (%display ($vector-ref buckets i))))
  (%display "]"))


;;;; low-level operations

(define MAX-NUMBER-OF-BUCKETS
  (fxdiv (greatest-fixnum) 2))

(define-inline (%compute-bucket-index table key)
  ($fxand ((weak-table-hash-function table) key) (weak-table-mask table)))

(define (%clean-bucket-from-bwp-entries table bucket-index)
  ;;Scan the selected bucket and remove all the weak pairs having BWP in
  ;;key position.  Update the size of the table accordingly.
  ;;
  (define-inline (decr! var)
    (set! var ($fxsub1 var)))
  (let* ((size    (weak-table-size table))
	 (buckets (weak-table-buckets table))
	 (entries ($vector-ref buckets bucket-index))
	 ;;Remove the leading entries with BWP key.
	 (entries (let loop ((entries entries))
		    (cond ((null? entries)
			   entries)
			  ((bwp-object? ($car ($car entries)))
			   (decr! size)
			   (loop ($cdr entries)))
			  (else
			   entries)))))
    (unless (null? entries)
      ;;Remove entries in the tail with  BWP key; we know that the first
      ;;HEAD has valid key.
      (let loop ((head entries)
		 (tail ($cdr entries)))
	(unless (null? tail)
	  (if (bwp-object? ($car ($car tail)))
	      (let ((tail ($cdr tail)))
		(decr! size)
		($set-cdr! head tail)
		(loop head tail))
	    (loop tail ($cdr tail))))))
    (set-weak-table-size! table size)
    ($vector-set! buckets bucket-index entries)))

(define (%intern! table bucket-index key value)
  ;;If KEY is not already interned: insert a new entry holding KEY/VALUE
  ;;in TABLE at BUCKET-INDEX.  If KEY is already interned: overwrite the
  ;;old value with VALUE.
  ;;
  (define who '%intern!)
  (when ($fx= (greatest-fixnum) ($weak-table-size table))
    (assertion-violation who "reached maximum number of entries in weak table" table key value))
  (let* ((buckets ($weak-table-buckets table))
	 (entries ($vector-ref buckets bucket-index)))
    (if (null? entries)
	(begin
	  ($vector-set! buckets bucket-index
			(cons (weak-cons key value) '()))
	  ($set-weak-table-size! table ($fxadd1 ($weak-table-size table))))
      ;;If the key is already  interned: overwrite the old value; else
      ;;append a new weak pair to the chain of entries.
      (let loop ((equiv?  ($weak-table-equiv-function table))
		 (head    entries)
		 (tail    ($cdr entries)))
	(let ((intern-key ($caar head)))
	  ;;Here it does not matter if INTERN-KEY is BWP.
	  (cond ((or (eq?    key intern-key)
		     (equiv? key intern-key))
		 ;;The  key is  already  interned:  overwrite the  old
		 ;;value and return.
		 ($set-cdr! ($car head) value))
		((null? tail)
		 ;;The key is not interned: insert a new entry, update
		 ;;the number of entries, enlarge the table if needed,
		 ;;then return.
		 ($set-cdr! head (cons (weak-cons key value) '()))
		 (let ((N ($fxadd1 ($weak-table-size table))))
		   ($set-weak-table-size! table N)
		   (when ($fx= N ($weak-table-mask table))
		     (%extend-table! table))))
		(else
		 ;;Try with the next entry.
		 (loop equiv? tail ($cdr tail)))))))))

(define (%unintern! table key bucket-index)
  ;;Remove  the entry  associated to  KEY from  TABLE in  the  bucket at
  ;;BUCKET-INDEX.  If KEY is not found: just do nothing.
  ;;
  (let* ((buckets (weak-table-buckets        table))
	 (equiv?  (weak-table-equiv-function table)))
    (let ((entries ($vector-ref buckets bucket-index)))
      (if (null? entries) ;empty bucket
	  (values)	  ;key not found
	;;Check separately the first entry  because if we have to remove
	;;it we must also update the value in the bucket.
	(let ((intern-key ($car ($car entries))))
	  ;;Here it does not matter if INTERN-KEY is BWP.
	  (if (or (eq?    key intern-key)
		  (equiv? key intern-key))
	      (begin
		(set-weak-table-size! table ($fxsub1 (weak-table-size table)))
		($vector-set! buckets bucket-index ($cdr entries)))
	    ;;Now check the tail of the chain.
	    (let loop ((prev    entries)
		       (entries ($cdr entries)))
	      (if (null? entries)
		  (values) ;key not found
		(let ((intern-key ($car ($car entries))))
		  ;;Here it does not matter if INTERN-KEY is BWP.
		  (if (or (eq?    key intern-key)
			  (equiv? key intern-key))
		      (begin
			(set-weak-table-size! table ($fxsub1 (weak-table-size table)))
			($set-cdr! prev ($cdr entries)))
		    (loop entries ($cdr entries))))))))))))

(define (%extend-table! table)
  ;;Unless the number  of buckets is already at  its maximum: double the
  ;;size of the vector in TABLE, which must be an instance of WEAK-TABLE
  ;;structure.
  ;;
  (let* ((vec1	(weak-table-buckets table))
	 (len1	($vector-length vec1))
	 (hash	(weak-table-hash-function table)))
    ;;Do not allow the vector length to exceed the maximum fixnum.
    (when ($fx< len1 MAX-NUMBER-OF-BUCKETS)
      ;;If  we start  with an  even and  power of  2 vector  length, the
      ;;length is always even and power of 2...
      (let* ((len2	($fx+ len1 len1))
	     ;;... and the mask is always composed by all the significant
	     ;;bits set to 1.
	     (mask	($fxsub1 len2))
	     (vec2	(make-vector len2 '())))
	(define (%insert p)
	  (unless (null? p)
	    (let ((entry ($car p))
		  (rest  ($cdr p)))
	      ;;Recycle this pair by setting its cdr to the value in the
	      ;;vector.
	      (let ((idx ($fxand (hash ($car entry)) mask)))
		($set-cdr! p ($vector-ref vec2 idx))
		($vector-set! vec2 idx p))
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
      ;;The actual initial number of  buckets is the smallest power of 2
      ;;greater than INIT-DIMENSION:
      ;;
      ;;  DIM = 2^(fxlength init-dimension)
      ;;
      (let* ((dim	(fxarithmetic-shift-left 1 (fxlength init-dimension)))
	     (mask	($fxsub1 dim))
	     (buckets	(make-vector dim '())))
	(make-weak-table 0 dim mask buckets hash-function equiv-function))))))

(define weak-hashtable? weak-table?)

(define (weak-hashtable-set! table key value)
  (define who 'weak-hashtable-set!)
  (with-arguments-validation (who)
      ((weak-hashtable	table))
    (let ((bucket-index (%compute-bucket-index table key)))
      (%clean-bucket-from-bwp-entries table bucket-index)
      (%intern! table bucket-index key value))))

(define (weak-hashtable-ref table key default)
  (define who 'weak-hashtable-ref)
  (with-arguments-validation (who)
      ((weak-hashtable	table))
    (let* ((equiv?		(weak-table-equiv-function table))
	   (buckets		(weak-table-buckets        table))
	   (bucket-index	(%compute-bucket-index table key)))
      (%clean-bucket-from-bwp-entries table bucket-index)
      (let loop ((entries ($vector-ref buckets bucket-index)))
	(if (null? entries)
	    default
	  (let ((intern-key ($car ($car entries))))
	    ;;Here it does not matter if INTERN-KEY is BWP.
	    (if (or (eq?    key intern-key)
		    (equiv? key intern-key))
		($cdr ($car entries))
	      (loop ($cdr entries)))))))))

(define (weak-hashtable-delete! table key)
  (define who 'weak-hashtable-ref)
  (with-arguments-validation (who)
      ((weak-hashtable	table))
    (let ((bucket-index (%compute-bucket-index table key)))
      (%clean-bucket-from-bwp-entries table bucket-index)
      (%unintern! table key bucket-index))))

(define (weak-hashtable-contains? table key)
  (define who 'weak-hashtable-contains?)
  (with-arguments-validation (who)
      ((weak-hashtable	table))
    (let* ((equiv?		(weak-table-equiv-function table))
	   (buckets		(weak-table-buckets        table))
	   (bucket-index	(%compute-bucket-index table key)))
      (%clean-bucket-from-bwp-entries table bucket-index)
      (let loop ((entries ($vector-ref buckets bucket-index)))
	(if (null? entries)
	    #f
	  (let ((intern-key ($car ($car entries))))
	    ;;Here it does not matter if INTERN-KEY is BWP.
	    (if (or (eq?    key intern-key)
		    (equiv? key intern-key))
		#t
	      (loop ($cdr entries)))))))))

(define (weak-hashtable-clear! table)
  (let* ((dim		(weak-table-init-dim table))
	 (mask		($fxsub1 dim))
	 (buckets	(make-vector dim '())))
    (set-weak-table-size!     table 0)
    (set-weak-table-buckets!  table buckets)
    (set-weak-table-mask!     table mask)))

(define (weak-hashtable-keys table)
  (let* ((keys		(make-vector (weak-table-size table)))
	 (buckets	(weak-table-buckets table))
	 (dim		($vector-length buckets))
	 (count		0))
    (do ((i 0 ($fxadd1 i)))
	(($fx= i dim)
	 keys)
      (%clean-bucket-from-bwp-entries table i)
      (let loop ((entries ($vector-ref buckets i)))
	(unless (null? entries)
	  ($vector-set! keys count ($car ($car entries)))
	  (set! count ($fxadd1 count))
	  (loop ($cdr entries)))))))

(define (weak-hashtable-entries table)
  (let* ((size		(weak-table-size table))
	 (keys		(make-vector size))
	 (vals		(make-vector size))
	 (buckets	(weak-table-buckets table))
	 (dim		($vector-length buckets))
	 (count		0))
    (do ((i 0 ($fxadd1 i)))
	(($fx= i dim)
	 (values keys vals))
      (%clean-bucket-from-bwp-entries table i)
      (let loop ((entries ($vector-ref buckets i)))
	(unless (null? entries)
	  (let ((entry ($car entries)))
	    ($vector-set! keys count ($car entry))
	    ($vector-set! vals count ($cdr entry))
	    (set! count ($fxadd1 count)))
	  (loop ($cdr entries)))))))

(define weak-hashtable-size weak-table-size)

(define (weak-hashtable-update! table key proc default)
  (define who 'weak-hashtable-update!)
  (with-arguments-validation (who)
      ((weak-hashtable	table)
       (procedure	proc))
    (let* ((equiv?		(weak-table-equiv-function table))
	   (buckets		(weak-table-buckets        table))
	   (bucket-index	(%compute-bucket-index table key)))
      (%clean-bucket-from-bwp-entries table bucket-index)
      (let ((the-entries ($vector-ref buckets bucket-index)))
	(let loop ((entries the-entries))
	  (if (null? entries)
	      ;;add a new entry
	      ($vector-set! buckets bucket-index (cons (weak-cons key (proc default))
							     the-entries))
	    (let* ((entry      ($car entries))
		   (intern-key ($car entry)))
	      ;;Here it does not matter if INTERN-KEY is BWP.
	      (if (or (eq?    key intern-key)
		      (equiv? key intern-key))
		  (set-cdr! entry (proc ($cdr entry)))
		(loop ($cdr entries))))))))))


;;;; done

(set-rtd-printer! (type-descriptor weak-table) %struct-weak-table-printer)

)

;;; end of file
