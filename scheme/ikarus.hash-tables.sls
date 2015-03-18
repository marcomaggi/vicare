;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Mofified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;See the paper:
;;
;;  Ghuloum, Dybvig.  "Generation-Friendly Eq Hash Tables".  Proceedings
;;  of the 2007 Workshop on Scheme and Functional Programming.
;;


#!vicare
(library (ikarus hash-tables)
  (export
    make-eq-hashtable		make-eqv-hashtable	make-hashtable
    hashtable?			hashtable-mutable?	mutable-hashtable?
    hashtable-ref		hashtable-set!
    hashtable-size
    hashtable-delete!		hashtable-clear!
    hashtable-contains?
    hashtable-update!
    hashtable-keys		hashtable-entries
    hashtable-copy
    hashtable-equivalence-function
    hashtable-hash-function

    ;; hash functions
    string-hash			string-ci-hash
    symbol-hash			bytevector-hash
    equal-hash
    fixnum-hash			exact-integer-hash
    flonum-hash			number-hash
    char-hash			char-ci-hash
    boolean-hash		void-hash
    eof-object-hash		would-block-hash
    struct-hash			record-hash
    object-hash

    ;; unsafe operations
    $string-hash		$string-ci-hash
    $symbol-hash		$bytevector-hash)
  (import (except (vicare)
		  make-eq-hashtable		make-eqv-hashtable
		  make-hashtable
		  hashtable?			hashtable-mutable?	mutable-hashtable?
		  hashtable-ref			hashtable-set!
		  hashtable-size
		  hashtable-delete!		hashtable-clear!
		  hashtable-contains?
		  hashtable-update!
		  hashtable-keys		hashtable-entries
		  hashtable-copy
		  hashtable-equivalence-function
		  hashtable-hash-function
		  string-hash			string-ci-hash
		  symbol-hash			bytevector-hash
		  equal-hash
		  fixnum-hash			exact-integer-hash
		  flonum-hash			number-hash
		  char-hash			char-ci-hash
		  boolean-hash			void-hash
		  eof-object-hash		would-block-hash
		  struct-hash			record-hash
		  object-hash)
    (vicare system $bignums)
    (vicare system $chars)
    (vicare system $compnums)
    (vicare system $flonums)
    (vicare system $fx)
    (vicare system $numerics)
    (vicare system $pairs)
    (vicare system $ratnums)
    (vicare system $tcbuckets)
    (vicare system $vectors))


;;;; helpers

(define (%initial-capacity? obj)
  (and (or (fixnum? obj)
	   (bignum? obj))
       (>= obj 0)))

(define (%boolean-or-non-negative-fixnum? obj)
  (or (boolean? obj)
      (and (fixnum? obj)
	   (fxnonnegative? obj))))

(define (%not-void? obj)
  (not (eq? obj (void))))


;;;; data structure

(define-struct hasht
  (buckets-vector
		;Scheme vector representing the tables' buckets vector.
   size
		;Non-negative fixnum representing the number of entries in the table.
		;Limiting this value to a fixnum is a constraint deriving from:
		;
		;* The R6RS, which includes in  the API functions that return vectors
		;  holding all the keys and values in the table.
		;
		;* The implementation  of Vicare, which defines vectors  as having at
		;  most (greatest-fixnum) elements.
   tc
		;If this is a EQ? or EQV? table: the boolean false.
		;
		;If  this table  has  a user-selected  equivalence  function: a  pair
		;initialised as follows:
		;
		;   |---|---|
		;     |   |
		;      ---+---> (#f . #f)
		;
   mutable?
		;Boolean.   True if  values can  be added  to and  removed from  this
		;hashtable; otherwise false.
   hashf
		;If this is a EQ? or EQV? table: the boolean false.
		;
		;the hash
		;function to be  used to compute keys's hash values.   It is the same
		;value of the field HASHF0 only if  it is one among the built-in hash
		;functions, otherwise it  is a wrapper for HASHF0  that validates the
		;return value as exact integer.
   equivf
		;The equivalence function given to the constructor of this struct.
   hashf0
		;If this is a EQ? or EQV? table: the boolean false.
		;
		;If this  table has  a user-selected  equivalence function:  the hash
		;function given to the constructor of this struct.
   ))


;;;; directly from Dybvig's paper
;;
;;This code hash been modified in Vicare.
;;

(define (make-empty-tc)
  (let ((x (cons #f #f)))
    (cons x x)))

;;; --------------------------------------------------------------------

(define (make-new-buckets-vector n)
  (init-buckets-vector (make-vector n) 0 n))

(define (init-buckets-vector v i n)
  ;;Set to I the slot of the vector V at index I; increment I by 1; recurse while I <
  ;;N.
  ;;
  (if ($fx= i n)
      v
    (begin
      ($vector-set! v i i)
      (init-buckets-vector v (fxadd1 i) n))))

(define-syntax-rule (hash-value->buckets-vector-index ?hv ?buckets-vector)
  ;;Convert the fixnum ?HV representing the hash value  of a key into an index in the
  ;;buckets vector.
  ;;
  ($fxlogand ?hv ($fxsub1 ($vector-length ?buckets-vector))))

;;; --------------------------------------------------------------------
;;; chains of tcbuckets

(define (get-bucket-index B)
  ;;Recursive  function.  Given  a tcbucket  B from  a chain:  return a  non-negative
  ;;fixnum representing the index of the bucket holding the chain.
  ;;
  (let ((next ($tcbucket-next B)))
    (if (fixnum? next)
	next
      (get-bucket-index next))))

(define (remove-tcbucket-from-chain! leading-bucket B B.next)
  ;;LEADING-BUCKET is the first tcbucket in a chain of tcbuckets.  B is a tcbucket in
  ;;the chain.  B.NEXT is the successor of B  in the chain: either a tcbucket if B is
  ;;not the last,  or a fixnum if B  is the last (the fixnum represents  the index of
  ;;the chain in the buckets vector).  Layout example:
  ;;
  ;;   buckets vector
  ;;   --
  ;;   |
  ;;   --
  ;;   | --> | leading-bucket | --> |other| --> | B | --> | B.next | --> ...
  ;;   --
  ;;   |
  ;;   --
  ;;
  ;;Recursive function.  Extract  B from the chain replacing it  with B.NEXT.  Return
  ;;unspecified values.
  ;;
  (let ((leading-bucket.next ($tcbucket-next leading-bucket)))
    (cond ((eq? leading-bucket.next B)
	   ($set-tcbucket-next! leading-bucket B.next)
	   (void))
	  (else
	   (remove-tcbucket-from-chain! leading-bucket.next B B.next)))))

;;; --------------------------------------------------------------------
;;; searching in EQ? hash tables

(define (direct-lookup key buck)
  ;;Recursive function.  Used only with EQ? hash tables.  ASSQ-like lookup of the key
  ;;KEY in the chain of tcbuckets starting  with BUCK.  If a matching entry is found:
  ;;return its tcbucket; otherwise return false.
  ;;
  (cond ((fixnum? buck)
	 ;;End of chain.
	 #f)
	((eq? key ($tcbucket-key buck))
	 ;;Match found.
	 buck)
	(else
	 ;;Try next.
	 (direct-lookup key ($tcbucket-next buck)))))

(module (rehash-lookup)

  (define (rehash-lookup H tc key)
    ;;Used only with EQ? hash tables.  H is the  hash table; TC is the tc field of H;
    ;;KEY is the key.   If a matching entry is found:  return its tcbucket; otherwise
    ;;return false.
    ;;
    (cond ((tc-pop tc)
	   => (lambda (buck)
		(if (not ($tcbucket-next buck))
		    (rehash-lookup H tc key)
		  (begin
		    (re-add! H buck)
		    (if (eq? key ($tcbucket-key buck))
			buck
		      (rehash-lookup H tc key))))))
	  (else #f)))

  (define (tc-pop tc)
    ;;This function might  mutate the pair TC,  which is a TC field  from a hashtable
    ;;struct.  If  TC is  empty: return  false.  Otherwise pop  and return  its first
    ;;entry.
    ;;
    (let ((A ($car tc)))
      (if (eq? A ($cdr tc))
	  ;;The pair TC has EQ? car and cdr: it is empty.
	  #f
	;;Before:
	;;           -------
	;;   TC --> | A | o-|--> ...
	;;           -------
	;;            |     -------      -------
	;;             --> | M | o-|--> | P | o-|--> ...
	;;                  -------      -------
	;;after:
	;;           -------
	;;   TC --> | A | D |--> ...
	;;           -------
	;;            |     -------
	;;             --> | P | Q | --> ...
	;;                  -------
	;;and return M.
	(receive-and-return (v)
	    ($car A)
	  ($set-car! tc ($cdr A))
	  ;;Clean the pair A so that it does not reference the TC anymore.
	  ($set-car! A #f)
	  ($set-cdr! A #f)))))

  (define (re-add! H buck)
    ;;BUCK is the tcbucket representing the entry to be rehashed.
    ;;
    (define buckets-vector
      (hasht-buckets-vector H))
    ;;First remove BUCK from its old place.
    (let* ((next        ($tcbucket-next buck))
	   ;;The index in the  buckes vector of the chain of  tcbuckets to which BUCK
	   ;;belongs.
	   (bucket-idx  (if (fixnum? next)
			    next
			  (get-bucket-index next)))
	   ;;The first tcbucket in the chain of tcbuckets to which BUCK belongs.
	   (fst         ($vector-ref buckets-vector bucket-idx)))
      (if (eq? fst buck)
	  ;;BUCK is the first of its chain.
	  ($vector-set! buckets-vector bucket-idx next)
	;;BUCK is not  the first of its  chain.  Remove BUCK from  the chain starting
	;;with FST.
	(remove-tcbucket-from-chain! fst buck next)))
    ;;Then reset the tcbucket-tconc.
    ($set-tcbucket-tconc! buck (hasht-tc H))
    ;;Finally re-hash the key and prepend BUCK to its new chain.
    (let ((idx (hash-value->buckets-vector-index (pointer-value ($tcbucket-key buck))
						 buckets-vector)))
      ($set-tcbucket-next! buck ($vector-ref buckets-vector idx))
      ($vector-set! buckets-vector idx buck)
      (void)))

  #| end of module: REHASH-LOOKUP |# )

;;; --------------------------------------------------------------------
;;; general searching in tables

(module (get-bucket)

  (define (get-bucket H key)
    ;;Return the tcbucket associated to KEY in the table H.  Return false if there is
    ;;no association.
    ;;
    (cond ((hasht-hashf H)
	   ;;The hashtable has user-supplied hash and equivalence functions.
	   => (lambda (hashf)
		(get-hashed H key (hashf key))))
	  ((and (eq? eqv? (hasht-equivf H))
		(number? key))
	   ;;The hashtable  has EQV? as  equivalence function, it has  NUMBER-HASH as
	   ;;hash function.
	   (get-hashed H key (number-hash key)))
	  (else
	   ;;The hashtable has EQ?  as  equivalence function, it has POINTER-VALUE as
	   ;;hash function.
	   (let ((vec (hasht-buckets-vector H)))
	     (or (direct-lookup key ($vector-ref vec (hash-value->buckets-vector-index (pointer-value key) vec)))
		 (rehash-lookup H (hasht-tc H) key))))))

  (define (get-hashed H key ih)
    ;;Used  for EQV?   hashtables and  for hashtables  having user-supplied  hash and
    ;;equivalence functions.
    ;;
    (let next-tcbucket ((equiv? (hasht-equivf H))
			(B      (let ((vec (hasht-buckets-vector H)))
				  ($vector-ref vec (hash-value->buckets-vector-index ih vec)))))
      (cond ((fixnum? B)
	     #f)
	    ((equiv? key ($tcbucket-key B))
	     B)
	    (else
	     (next-tcbucket equiv? ($tcbucket-next B))))))

  #| end of module: GET-BUCKET |# )

(define (get-hash table key default)
  ;;This is the implementation of HASHTABLE-REF as defined by R6RS.
  ;;
  (cond ((get-bucket table key)
	 => (lambda (B)
	      ($tcbucket-val B)))
	(else default)))

(define (in-hash? table key)
  ;;This is the implementation of HASHTABLE-CONTAINS? as defined by R6RS.
  ;;
  (and (get-bucket table key) #t))

;;; --------------------------------------------------------------------

(module (put-hash!)

  (define (put-hash! H x v)
    (define (put-hashed H x v ih)
      (let ((equiv? (hasht-equivf H))
	    (vec (hasht-buckets-vector H)))
	(let ((idx (hash-value->buckets-vector-index ih vec)))
	  (let f ((b ($vector-ref vec idx)))
	    (cond ((fixnum? b)
		   ($vector-set! vec idx (vector x v ($vector-ref vec idx)))
		   (let ((ct (hasht-size H)))
		     (set-hasht-size! H (fxadd1 ct))
		     (when ($fx> ct ($vector-length vec))
		       (enlarge-table H))))
		  ((equiv? x ($tcbucket-key b))
		   ($set-tcbucket-val! b v))
		  (else
		   (f ($tcbucket-next b))))))))
    (cond ((hasht-hashf H)
	   => (lambda (hashf)
		(put-hashed H x v (hashf x))))
	  ((and (eq? eqv? (hasht-equivf H))
		(number? x))
	   (put-hashed H x v (number-hash x)))
	  (else
	   (let ((pv  (pointer-value x))
		 (vec (hasht-buckets-vector H)))
	     (let ((ih pv))
	       (let ((idx (hash-value->buckets-vector-index ih vec)))
		 (let ((b ($vector-ref vec idx)))
		   (cond ((or (direct-lookup x b)
			      (rehash-lookup H (hasht-tc H) x))
			  => (lambda (b)
			       ($set-tcbucket-val! b v)
			       (void)))
			 (else
			  ;;Prepend a new TCBUCKET in  the buckets vector slot at index
			  ;;IDX.
			  (let ((bucket ($make-tcbucket (hasht-tc H)	       ;tconc
							x		       ;key
							v		       ;val
							($vector-ref vec idx)))) ;next
			    (if ($fx= (pointer-value x) pv)
				($vector-set! vec idx bucket)
			      (let* ((ih  (pointer-value x))
				     (idx (hash-value->buckets-vector-index ih vec)))
				($set-tcbucket-next! bucket ($vector-ref vec idx))
				($vector-set! vec idx bucket))))
			  (let ((ct (hasht-size H)))
			    (set-hasht-size! H (fxadd1 ct))
			    (when ($fx> ct ($vector-length vec))
			      (enlarge-table H))))))))))))

  (define (enlarge-table h)
    (define (enlarge-hashtable h hashf)
      (define (insert-b b vec mask)
	(let* ((x    ($tcbucket-key b))
	       (ih   (hashf x))
	       (idx  ($fxlogand ih mask))
	       (next ($tcbucket-next b)))
	  ($set-tcbucket-next! b ($vector-ref vec idx))
	  ($vector-set! vec idx b)
	  (unless (fixnum? next)
	    (insert-b next vec mask))))
      (define (move-all vec1 i n vec2 mask)
	(unless ($fx= i n)
	  (let ((b ($vector-ref vec1 i)))
	    (unless (fixnum? b)
	      (insert-b b vec2 mask))
	    (move-all vec1 (fxadd1 i) n vec2 mask))))
      (let* ((vec1 (hasht-buckets-vector h))
	     (n1   ($vector-length vec1))
	     (n2   ($fxsll n1 1))
	     (vec2 (make-new-buckets-vector n2)))
	(move-all vec1 0 n1 vec2 ($fxsub1 n2))
	(set-hasht-buckets-vector! h vec2)))
    (cond ((hasht-hashf h)
	   => (lambda (hashf)
		(enlarge-hashtable h hashf)))
	  ((eq? eq? (hasht-equivf h))
	   (enlarge-hashtable h (lambda (x)
				  (pointer-value x))))
	  (else
	   (enlarge-hashtable h (lambda (x)
				  (if (number? x)
				      (number-hash x)
				    (pointer-value x)))))))

  #| end of module: PUT-HASH! |# )

;;; --------------------------------------------------------------------

(define (del-hash h x)
  (define (unlink! h b)
    (let ((vec (hasht-buckets-vector h))
	  (next ($tcbucket-next b)))
      ;; first remove it from its old place
      (let ((idx (if (fixnum? next)
		     next
		   (get-bucket-index next))))
	(let ((fst ($vector-ref vec idx)))
	  (cond ((eq? fst b)
		 ($vector-set! vec idx next))
		(else
		 (remove-tcbucket-from-chain! fst b next)))))
      ;; set next to be #f, denoting, not in table
      ($set-tcbucket-next! b #f)))
  (cond ((get-bucket h x)
	 => (lambda (b)
	      (receive-and-return (key val)
		  ;;Returning these values is a Vicare extension.
		  (values ($tcbucket-key b)
			  ($tcbucket-val b))
		(unlink! h b)
		;; don't forget the count.
		(set-hasht-size! h (fxsub1 (hasht-size h))))))))

;;; --------------------------------------------------------------------

(define (update-hash! h x proc default)
  (cond ((get-bucket h x)
	 => (lambda (b)
	      ($set-tcbucket-val! b (proc ($tcbucket-val b)))))
	(else
	 (put-hash! h x (proc default)))))

(define (clear-hash! h)
  (let ((v (hasht-buckets-vector h)))
    (init-buckets-vector v 0 (vector-length v)))
  (unless (hasht-hashf h)
    (set-hasht-tc! h (make-empty-tc)))
  (set-hasht-size! h 0))

;;; --------------------------------------------------------------------

(define (get-keys h)
  (let ((v (hasht-buckets-vector h))
	(n (hasht-size h)))
    (let ((kv (make-vector n)))
      (let f ((i  ($fxsub1 n))
	      (j  ($fxsub1 (vector-length v)))
	      (kv kv)
	      (v  v))
	(cond (($fx= i -1)
	       kv)
	      (else
	       (let ((b ($vector-ref v j)))
		 (if (fixnum? b)
		     (f i ($fxsub1 j) kv v)
		   (f (let f ((i i) (b b) (kv kv))
			($vector-set! kv i ($tcbucket-key b))
			(let ((b ($tcbucket-next b))
			      (i ($fxsub1 i)))
			  (cond
			   ((fixnum? b) i)
			   (else (f i b kv)))))
		      ($fxsub1 j) kv v)))))))))

(define (get-entries table)
  ;;This is the implementation of HASHTABLE-ENTRIES as defined by R6RS.
  ;;
  (let* ((buck-vec (hasht-buckets-vector   table))
	 (N        (hasht-size  table))
	 (keys-vec (make-vector N))
	 (vals-vec (make-vector N)))
    (let next-bucket ((i        ($fxsub1 N))
		      (buck-idx ($fxsub1 ($vector-length buck-vec)))
		      (keys-vec keys-vec)
		      (vals-vec vals-vec)
		      (buck-vec buck-vec))
      (if ($fx= i -1)
	  (values keys-vec vals-vec)
	(let ((b ($vector-ref buck-vec buck-idx)))
	  (if (fixnum? b)
	      (next-bucket i ($fxsub1 buck-idx) keys-vec vals-vec buck-vec)
	    (next-bucket (let inner-loop ((i i) (b b) (keys-vec keys-vec) (vals-vec vals-vec))
			   ($vector-set! keys-vec i ($tcbucket-key b))
			   ($vector-set! vals-vec i ($tcbucket-val b))
			   (let ((b ($tcbucket-next b))
				 (i ($fxsub1 i)))
			     (if (fixnum? b)
				 i
			       (inner-loop i b keys-vec vals-vec))))
			 ($fxsub1 buck-idx)
			 keys-vec vals-vec buck-vec)))))))

;;; --------------------------------------------------------------------

(define (hasht-copy h mutable?)
  (define (dup-hasht h mutable? n)
    (let* ((hashf (hasht-hashf h))
	   (tc    (and (not hashf)
		       (make-empty-tc))))
      (make-hasht (make-new-buckets-vector n) ;buckets-vector
		  0			      ;size
		  tc			      ;tc
		  mutable?		      ;mutable?
		  hashf			      ;validated hash function
		  (hasht-equivf h)	      ;equivalence function
		  (hasht-hashf0 h))))	      ;original hash function
  (let ((v (hasht-buckets-vector h))
	(n (hasht-size h)))
    (let ((r (dup-hasht h mutable? (vector-length v))))
      (let f ((i ($fxsub1 n))
	      (j ($fxsub1 (vector-length v)))
	      (r r)
	      (v v))
	(cond (($fx= i -1)
	       r)
	      (else
	       (let ((b ($vector-ref v j)))
		 (if (fixnum? b)
		     (f i ($fxsub1 j) r v)
		   (f (let f ((i i) (b b) (r r))
			(put-hash! r ($tcbucket-key b) ($tcbucket-val b))
			(let ((b ($tcbucket-next b))
			      (i ($fxsub1 i)))
			  (cond ((fixnum? b)
				 i)
				(else
				 (f i b r)))))
		      ($fxsub1 j) r v)))))))))


;;;; public interface: constructors and predicate

(define hashtable? hasht?)

(case-define* make-eq-hashtable
  (()
   (make-hasht (make-new-buckets-vector 32) ;buckets-vector
	       0			    ;size
	       (make-empty-tc)		    ;tc
	       #t			    ;mutable?
	       #f			    ;hashf
	       eq?			    ;equivf
	       #f))			    ;hashf0
  (({cap %initial-capacity?})
   (make-eq-hashtable)))

(case-define* make-eqv-hashtable
  (()
   (make-hasht (make-new-buckets-vector 32) ;buckets-vector
	       0			    ;size
	       (make-empty-tc)		    ;tc
	       #t			    ;mutable?
	       #f			    ;hashf
	       eqv?			    ;equivf
	       #f))			    ;hashf0
  (({cap %initial-capacity?})
   (make-eqv-hashtable)))

(module (make-hashtable)

  (case-define* make-hashtable
    (({hashf procedure?} {equivf procedure?})
     (make-hasht (make-new-buckets-vector 32)  ;buckets-vector
		 0			       ;count
		 #f			       ;tc
		 #t			       ;mutable?
		 (%make-hashfun-wrapper hashf) ;hashf
		 equivf			       ;equivf
		 hashf))		       ;hashf0
    (({hashf procedure?} {equivf procedure?} {cap %initial-capacity?})
     (make-hashtable hashf equivf)))

  (define (%make-hashfun-wrapper f)
    (if (or (eq? f symbol-hash)
	    (eq? f string-hash)
	    (eq? f string-ci-hash)
	    (eq? f fixnum-hash)
	    (eq? f exact-integer-hash)
	    (eq? f flonum-hash)
	    (eq? f number-hash)
	    (eq? f char-hash)
	    (eq? f char-ci-hash)
	    (eq? f bytevector-hash)
	    (eq? f boolean-hash)
	    (eq? f struct-hash)
	    (eq? f record-hash)
	    (eq? f object-hash)
	    (eq? f void-hash)
	    (eq? f eof-object-hash)
	    (eq? f would-block-hash)
	    (eq? f equal-hash))
	f
      (lambda (k)
	(fluid-let-syntax
	    ((__who__ (identifier-syntax 'hashfunc-wrapper)))
	  (receive-and-return (i)
	      (f k)
	    (unless (exact-integer? i)
	      (procedure-argument-violation __who__
		"invalid hash value from custom hash function"
		i)))))))

  #| end of module: MAKE-HASHTABLE |# )

;;; --------------------------------------------------------------------

(case-define* hashtable-copy
  (({table hashtable?})
   (if (hasht-mutable? table)
       (hasht-copy table #f)
     table))
  (({table hashtable?} mutable?)
   (if (or mutable?
	   (hasht-mutable? table))
       (hasht-copy table (and mutable? #t))
     table)))


;;;; public interface: accessors and mutators

(case-define* hashtable-ref
  (({table hashtable?} key)
   (get-hash table key (void)))
  (({table hashtable?} key default)
   (get-hash table key default)))

(define* (hashtable-set! {table mutable-hashtable?} key {val %not-void?})
  (put-hash! table key val))

;;; --------------------------------------------------------------------

(define* (hashtable-contains? {table hashtable?} key)
  (in-hash? table key))

;;; --------------------------------------------------------------------

(define* (hashtable-update! {table mutable-hashtable?} key {proc procedure?} {default %not-void?})
  (update-hash! table key proc default))

(define* (hashtable-delete! {table mutable-hashtable?} key)
  ;;Remove any association for KEY within TABLE;  if there is no association for KEY:
  ;;do nothing.
  ;;
  ;;As Vicare extension:
  ;;
  ;;* If an  association is  found: return  two values,  the key  and value.
  ;;
  ;;* If no association is found: return two values, void and void.
  ;;
  ;;FIXME: should shrink table if number of keys drops below:
  ;;
  ;;(sqrt (vector-length (hasht-buckets-vector h)))
  ;;
  ;;(Abdulaziz Ghuloum)
  ;;
  (del-hash table key))

(define* (hashtable-clear! {table mutable-hashtable?})
  (clear-hash! table))


;;;; public interface: inspection

;; (define* (hashtable-size {table hashtable?})
;;   (hasht-size table))

(define hashtable-size hasht-size)

(define* (hashtable-entries {table hashtable?})
  (get-entries table))

(define* (hashtable-keys {table hashtable?})
  (get-keys table))

(define* (hashtable-mutable? {table hashtable?})
  (hasht-mutable? table))

(define (mutable-hashtable? obj)
  (and (hashtable?     obj)
       (hasht-mutable? obj)))

;;; --------------------------------------------------------------------

(define* (hashtable-equivalence-function {table hashtable?})
  (hasht-equivf table))

(define* (hashtable-hash-function {table hashtable?})
  (hasht-hashf0 table))


;;;; hash functions

(case-define* string-hash
  ;;Defined by Vicare.
  ;;
  (({bv string?})
   ($string-hash bv #f))
  (({bv string?} {max-len %boolean-or-non-negative-fixnum?})
   ($string-hash bv max-len)))

(case-define $string-hash
  ((s)
   (foreign-call "ikrt_string_hash" s #f))
  ((s max-len)
   (foreign-call "ikrt_string_hash" s max-len)))

;;; --------------------------------------------------------------------

(case-define* string-ci-hash
  ;;Defined by Vicare.
  ;;
  (({bv string?})
   ($string-ci-hash bv #f))
  (({bv string?} {max-len %boolean-or-non-negative-fixnum?})
   ($string-ci-hash bv max-len)))

(case-define $string-ci-hash
  ((s)
   (foreign-call "ikrt_string_hash" (string-foldcase s) #f))
  ((s max-len)
   (foreign-call "ikrt_string_hash" (string-foldcase s) max-len)))

;;; --------------------------------------------------------------------

(define* (symbol-hash {sym symbol?})
  ($symbol-hash sym))

(define ($symbol-hash sym)
  (foreign-call "ikrt_string_hash" (symbol->string sym)))

;;; --------------------------------------------------------------------

(case-define* bytevector-hash
  ;;Defined by Vicare.
  ;;
  (({bv bytevector?})
   ($bytevector-hash bv #f))
  (({bv bytevector?} {max-len %boolean-or-non-negative-fixnum?})
   ($bytevector-hash bv max-len)))

(case-define $bytevector-hash
  ((bv)
   (foreign-call "ikrt_bytevector_hash" bv #f))
  ((bv max-len)
   (foreign-call "ikrt_bytevector_hash" bv max-len)))

;;; --------------------------------------------------------------------

(define* (fixnum-hash {fx fixnum?})
  ($fixnum-hash fx))

(define ($fixnum-hash fx)
  ($abs-fixnum fx))

;;; --------------------------------------------------------------------

(define* (exact-integer-hash {N exact-integer?})
  (if (fixnum? N)
      ($fixnum-hash N)
    ($bignum-hash N)))

(define ($bignum-hash N)
  (foreign-call "ikrt_bignum_hash" N))

;;; --------------------------------------------------------------------

(define* (flonum-hash {fl flonum?})
  ($flonum-hash fl))

(define* ($flonum-hash fl)
  (foreign-call "ikrt_flonum_hash" fl))

;;; --------------------------------------------------------------------

(define* (number-hash Z)
  (cond ((fixnum? Z)
	 ($fixnum-hash Z))
	((flonum? Z)
	 ($flonum-hash Z))
	((bignum? Z)
	 ($bignum-hash Z))
	((ratnum? Z)
	 (fxxor (number-hash ($ratnum-n Z))
		(number-hash ($ratnum-d Z))))
	((cflonum? Z)
	 (fxxor ($flonum-hash ($cflonum-real Z))
		($flonum-hash ($cflonum-imag Z))))
	((compnum? Z)
	 (fxxor (number-hash ($compnum-real Z))
		(number-hash ($compnum-imag Z))))
	(else
	 (procedure-argument-violation __who__ "expected number object" Z))))

;;; --------------------------------------------------------------------

(define* (char-hash {ch char?})
  ($char-hash ch))

(define ($char-hash ch)
  ($fixnum-hash ($char->fixnum ch)))

;;; --------------------------------------------------------------------

(define* (char-ci-hash {ch char?})
  ($char-ci-hash ch))

(define ($char-ci-hash ch)
  ($fixnum-hash ($char->fixnum (char-foldcase ch))))

;;; --------------------------------------------------------------------

(define* (struct-hash {stru struct?})
  ($struct-hash stru))

(define ($struct-hash stru)
  (if (zero? (struct-length stru))
      (symbol-hash (struct-type-symbol (struct-rtd stru)))
    (object-hash (struct-ref stru 0))))

;;; --------------------------------------------------------------------

(define* (record-hash {rec record?})
  ($record-hash rec))

(define ($record-hash rec)
  (let ((rtd (record-rtd rec)))
    (if (zero? (vector-length (record-type-field-names rtd)))
	(let ((uid (record-type-uid rtd)))
	  (if uid
	      (symbol-hash uid)
	    0))
      (object-hash (struct-ref rec 0)))))

;;; --------------------------------------------------------------------

(define (void-hash obj)
  0)

(define (eof-object-hash obj)
  1)

(define (would-block-hash obj)
  2)

;;; --------------------------------------------------------------------

(define* (boolean-hash {obj boolean?})
  ($boolean-hash obj))

(define ($boolean-hash obj)
  (if obj 1 0))

;;; --------------------------------------------------------------------

(define (object-hash obj)
  (cond ((string? obj)
	 ($string-hash obj #f))
	((symbol? obj)
	 ($symbol-hash obj))
	((bytevector? obj)
	 ($bytevector-hash obj #f))
	((fixnum? obj)
	 ($fixnum-hash obj))
	((flonum? obj)
	 ($flonum-hash obj))
	((bignum? obj)
	 ($bignum-hash obj))
	((number? obj)
	 (number-hash obj))
	((char? obj)
	 ($char-hash obj))
	((boolean? obj)
	 ($boolean-hash obj))
	((record? obj)
	 ($record-hash obj))
	((struct? obj)
	 ($struct-hash obj))
	((eq? obj (void))
	 (void-hash obj))
	((eof-object? obj)
	 (eof-object-hash obj))
	((would-block-object? obj)
	 (would-block-hash obj))
	(else
	 (equal-hash obj))))

;;; --------------------------------------------------------------------

(define (equal-hash obj)
  (string-hash (call-with-string-output-port
		   (lambda (port)
		     (write obj port)))))


;;;; done

(set-rtd-printer! (type-descriptor hasht)	(lambda (x p wr)
						  (display "#<hashtable>" p)))

#| end of library |# )

;;; end of file
