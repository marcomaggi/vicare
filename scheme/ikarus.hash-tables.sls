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


#!r6rs
(library (ikarus hash-tables)
  (export
    make-eq-hashtable		make-eqv-hashtable
    make-hashtable
    hashtable?			hashtable-mutable?
    hashtable-ref		hashtable-set!
    hashtable-size
    hashtable-delete!		hashtable-clear!
    hashtable-contains?
    hashtable-update!
    hashtable-keys		hashtable-entries
    hashtable-copy
    hashtable-equivalence-function
    hashtable-hash-function
    string-hash			string-ci-hash
    symbol-hash			equal-hash)
  (import (except (ikarus)
		  make-eq-hashtable	make-eqv-hashtable
		  make-hashtable
		  hashtable?		hashtable-mutable?
		  hashtable-ref		hashtable-set!
		  hashtable-size
		  hashtable-delete!	hashtable-clear!
		  hashtable-contains?
		  hashtable-update!
		  hashtable-keys	hashtable-entries
		  hashtable-copy
		  hashtable-equivalence-function
		  hashtable-hash-function
		  string-hash		string-ci-hash
		  symbol-hash		equal-hash)
    (ikarus system $pairs)
    (ikarus system $vectors)
    (ikarus system $tcbuckets)
    (ikarus system $fx))


;;;; data structure

(define-struct hasht
  (vec
   count
   tc
   mutable?
   hashf
   equivf
   hashf0
   ))



;;Directly from Dybvig's paper.
(define tc-pop
  (lambda (tc)
    (let ((x ($car tc)))
      (if (eq? x ($cdr tc))
	  #f
	(let ((v ($car x)))
	  ($set-car! tc ($cdr x))
	  ($set-car! x #f)
	  ($set-cdr! x #f)
	  v)))))

  ;;; assq-like lookup
(define direct-lookup
  (lambda (x b)
    (if (fixnum? b)
	#f
      (if (eq? x ($tcbucket-key b))
	  b
	(direct-lookup x ($tcbucket-next b))))))

(define rehash-lookup
  (lambda (h tc x)
    (cond
     ((tc-pop tc) =>
      (lambda (b)
	(if (eq? ($tcbucket-next b) #f)
	    (rehash-lookup h tc x)
	  (begin
	    (re-add! h b)
	    (if (eq? x ($tcbucket-key b))
		b
	      (rehash-lookup h tc x))))))
     (else #f))))

(define get-bucket-index
  (lambda (b)
    (let ((next ($tcbucket-next b)))
      (if (fixnum? next)
	  next
	(get-bucket-index next)))))

(define replace!
  (lambda (lb x y)
    (let ((n ($tcbucket-next lb)))
      (cond
       ((eq? n x)
	($set-tcbucket-next! lb y)
	(void))
       (else
	(replace! n x y))))))

(define re-add!
  (lambda (h b)
    (let ((vec (hasht-vec h))
	  (next ($tcbucket-next b)))
        ;;; first remove it from its old place
      (let ((idx
	     (if (fixnum? next)
		 next
	       (get-bucket-index next))))
	(let ((fst ($vector-ref vec idx)))
	  (cond
	   ((eq? fst b)
	    ($vector-set! vec idx next))
	   (else
	    (replace! fst b next)))))
        ;;; reset the tcbucket-tconc FIRST
      ($set-tcbucket-tconc! b (hasht-tc h))
        ;;; then add it to the new place
      (let ((k ($tcbucket-key b)))
	(let ((ih (pointer-value k)))
	  (let ((idx ($fxlogand ih ($fx- ($vector-length vec) 1))))
	    (let ((n ($vector-ref vec idx)))
	      ($set-tcbucket-next! b n)
	      ($vector-set! vec idx b)
	      (void))))))))

(define (get-bucket h x)
  (define (get-hashed h x ih)
    (let ((equiv? (hasht-equivf h))
	  (vec (hasht-vec h)))
      (let ((idx (bitwise-and ih ($fx- ($vector-length vec) 1))))
	(let f ((b ($vector-ref vec idx)))
	  (cond
	   ((fixnum? b) #f)
	   ((equiv? x ($tcbucket-key b)) b)
	   (else (f ($tcbucket-next b))))))))
  (cond
   ((hasht-hashf h) =>
    (lambda (hashf)
      (get-hashed h x (hashf x))))
   ((and (eq? eqv? (hasht-equivf h)) (number? x))
    (get-hashed h x (number-hash x)))
   (else
    (let ((pv (pointer-value x))
	  (vec (hasht-vec h)))
      (let ((ih pv))
	(let ((idx ($fxlogand ih ($fx- ($vector-length vec) 1))))
	  (let ((b ($vector-ref vec idx)))
	    (or (direct-lookup x b)
		(rehash-lookup h (hasht-tc h) x)))))))))

(define (get-hash h x v)
  (cond
   ((get-bucket h x) =>
    (lambda (b) ($tcbucket-val b)))
   (else v)))

(define (in-hash? h x)
  (and (get-bucket h x) #t))

(define (del-hash h x)
  (define unlink!
    (lambda (h b)
      (let ((vec (hasht-vec h))
	    (next ($tcbucket-next b)))
          ;;; first remove it from its old place
	(let ((idx
	       (if (fixnum? next)
		   next
		 (get-bucket-index next))))
	  (let ((fst ($vector-ref vec idx)))
	    (cond
	     ((eq? fst b)
	      ($vector-set! vec idx next))
	     (else
	      (replace! fst b next)))))
          ;;; set next to be #f, denoting, not in table
	($set-tcbucket-next! b #f))))
  (cond
   ((get-bucket h x) =>
    (lambda (b)
      (unlink! h b)
         ;;; don't forget the count.
      (set-hasht-count! h (- (hasht-count h) 1))))))

(define put-hash!
  (lambda (h x v)
    (define (put-hashed h x v ih)
      (let ((equiv? (hasht-equivf h))
	    (vec (hasht-vec h)))
	(let ((idx (bitwise-and ih ($fx- ($vector-length vec) 1))))
	  (let f ((b ($vector-ref vec idx)))
	    (cond
	     ((fixnum? b)
	      ($vector-set! vec idx
			    (vector x v ($vector-ref vec idx)))
	      (let ((ct (hasht-count h)))
		(set-hasht-count! h ($fxadd1 ct))
		(when ($fx> ct ($vector-length vec))
		  (enlarge-table h))))
	     ((equiv? x ($tcbucket-key b))
	      ($set-tcbucket-val! b v))
	     (else (f ($tcbucket-next b))))))))
    (cond
     ((hasht-hashf h) =>
      (lambda (hashf)
	(put-hashed h x v (hashf x))))
     ((and (eq? eqv? (hasht-equivf h)) (number? x))
      (put-hashed h x v (number-hash x)))
     (else
      (let ((pv (pointer-value x))
	    (vec (hasht-vec h)))
	(let ((ih pv))
	  (let ((idx ($fxlogand ih ($fx- ($vector-length vec) 1))))
	    (let ((b ($vector-ref vec idx)))
	      (cond
	       ((or (direct-lookup x b) (rehash-lookup h (hasht-tc h) x))
		=>
		(lambda (b)
		  ($set-tcbucket-val! b v)
		  (void)))
	       (else
		(let ((bucket
		       ($make-tcbucket (hasht-tc h) x v
				       ($vector-ref vec idx))))
		  (if ($fx= (pointer-value x) pv)
		      ($vector-set! vec idx bucket)
		    (let* ((ih (pointer-value x))
			   (idx
			    ($fxlogand ih ($fx- ($vector-length vec) 1))))
		      ($set-tcbucket-next! bucket ($vector-ref vec idx))
		      ($vector-set! vec idx bucket))))
		(let ((ct (hasht-count h)))
		  (set-hasht-count! h ($fxadd1 ct))
		  (when ($fx> ct ($vector-length vec))
		    (enlarge-table h)))))))))))))

(define (update-hash! h x proc default)
  (cond
   ((get-bucket h x) =>
    (lambda (b) ($set-tcbucket-val! b (proc ($tcbucket-val b)))))
   (else (put-hash! h x (proc default)))))

(define enlarge-table
  (lambda (h)
    (define (enlarge-hashtable h hashf)
      (define insert-b
	(lambda (b vec mask)
	  (let* ((x ($tcbucket-key b))
		 (ih (hashf x))
		 (idx (bitwise-and ih mask))
		 (next ($tcbucket-next b)))
	    ($set-tcbucket-next! b ($vector-ref vec idx))
	    ($vector-set! vec idx b)
	    (unless (fixnum? next)
	      (insert-b next vec mask)))))
      (define move-all
	(lambda (vec1 i n vec2 mask)
	  (unless ($fx= i n)
	    (let ((b ($vector-ref vec1 i)))
	      (unless (fixnum? b)
		(insert-b b vec2 mask))
	      (move-all vec1 ($fxadd1 i) n vec2 mask)))))
      (let* ((vec1 (hasht-vec h))
	     (n1 ($vector-length vec1))
	     (n2 ($fxsll n1 1))
	     (vec2 (make-base-vec n2)))
	(move-all vec1 0 n1 vec2 ($fx- n2 1))
	(set-hasht-vec! h vec2)))
    (cond
     ((hasht-hashf h) =>
      (lambda (hashf)
	(enlarge-hashtable h hashf)))
     ((eq? eq? (hasht-equivf h))
      (enlarge-hashtable h
			 (lambda (x) (pointer-value x))))
     (else
      (enlarge-hashtable h
			 (lambda (x)
			   (if (number? x)
			       (number-hash x)
			     (pointer-value x))))))))

(define init-vec
  (lambda (v i n)
    (if ($fx= i n)
	v
      (begin
	($vector-set! v i i)
	(init-vec v ($fxadd1 i) n)))))

(define make-base-vec
  (lambda (n)
    (init-vec (make-vector n) 0 n)))

(define (clear-hash! h)
  (let ((v (hasht-vec h)))
    (init-vec v 0 (vector-length v)))
  (unless (hasht-hashf h)
    (set-hasht-tc! h
		   (let ((x (cons #f #f)))
		     (cons x x))))
  (set-hasht-count! h 0))

(define (get-keys h)
  (let ((v (hasht-vec h)) (n (hasht-count h)))
    (let ((kv (make-vector n)))
      (let f ((i ($fxsub1 n)) (j ($fxsub1 (vector-length v))) (kv kv) (v v))
	(cond
	 (($fx= i -1) kv)
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

(define (get-entries h)
  (let ((v (hasht-vec h)) (n (hasht-count h)))
    (let ((kv (make-vector n)) (vv (make-vector n)))
      (let f ((i ($fxsub1 n)) (j ($fxsub1 (vector-length v))) (kv kv) (vv vv) (v v))
	(cond
	 (($fx= i -1) (values kv vv))
	 (else
	  (let ((b ($vector-ref v j)))
	    (if (fixnum? b)
		(f i ($fxsub1 j) kv vv v)
	      (f (let f ((i i) (b b) (kv kv) (vv vv))
		   ($vector-set! kv i ($tcbucket-key b))
		   ($vector-set! vv i ($tcbucket-val b))
		   (let ((b ($tcbucket-next b))
			 (i ($fxsub1 i)))
		     (cond
		      ((fixnum? b) i)
		      (else (f i b kv vv)))))
		 ($fxsub1 j) kv vv v)))))))))

(define (hasht-copy h mutable?)
  (define (dup-hasht h mutable? n)
    (let* ((hashf (hasht-hashf h))
	   (tc (and (not hashf) (let ((x (cons #f #f))) (cons x x)))))
      (make-hasht (make-base-vec n) 0 tc mutable?
		  hashf (hasht-equivf h) (hasht-hashf0 h))))
  (let ((v (hasht-vec h)) (n (hasht-count h)))
    (let ((r (dup-hasht h mutable? (vector-length v))))
      (let f ((i ($fxsub1 n)) (j ($fxsub1 (vector-length v))) (r r) (v v))
	(cond
	 (($fx= i -1) r)
	 (else
	  (let ((b ($vector-ref v j)))
	    (if (fixnum? b)
		(f i ($fxsub1 j) r v)
	      (f (let f ((i i) (b b) (r r))
		   (put-hash! r ($tcbucket-key b) ($tcbucket-val b))
		   (let ((b ($tcbucket-next b)) (i ($fxsub1 i)))
		     (cond
		      ((fixnum? b) i)
		      (else (f i b r)))))
		 ($fxsub1 j) r v)))))))))


;;;; public interface

(define (hashtable? x) (hasht? x))

(define make-eq-hashtable
  (case-lambda
   (()
    (let ((x (cons #f #f)))
      (let ((tc (cons x x)))
	(make-hasht (make-base-vec 32) 0 tc #t #f eq? #f))))
   ((k)
    (if (and (or (fixnum? k) (bignum? k)) (>= k 0))
	(make-eq-hashtable)
      (die 'make-eq-hashtable "invalid initial capacity" k)))))

(define make-eqv-hashtable
  (case-lambda
   (()
    (let ((x (cons #f #f)))
      (let ((tc (cons x x)))
	(make-hasht (make-base-vec 32) 0 tc #t #f eqv? #f))))
   ((k)
    (if (and (or (fixnum? k) (bignum? k)) (>= k 0))
	(make-eqv-hashtable)
      (die 'make-eqv-hashtable "invalid initial capacity" k)))))

(define make-hashtable
  (case-lambda
   ((hashf equivf) (make-hashtable hashf equivf 0))
   ((hashf equivf k)
    (define who 'make-hashtable)
    (define (wrap f)
      (cond
       ((or (eq? f symbol-hash)
	    (eq? f string-hash)
	    (eq? f string-ci-hash)
	    (eq? f equal-hash))
	f)
       (else
	(lambda (k)
	  (let ((i (f k)))
	    (if (or (fixnum? i) (bignum? i))
		i
	      (die #f "invalid return value from hash function" i)))))))
    (unless (procedure? hashf)
      (die who "hash function is not a procedure" hashf))
    (unless (procedure? equivf)
      (die who "equivalence function is not a procedure" equivf))
    (if (and (or (fixnum? k) (bignum? k)) (>= k 0))
	(make-hasht (make-base-vec 32) 0 #f #t (wrap hashf) equivf hashf)
      (die who "invalid initial capacity" k)))))

(define hashtable-ref
  (lambda (h x v)
    (if (hasht? h)
	(get-hash h x v)
      (die 'hashtable-ref "not a hash table" h))))

(define hashtable-contains?
  (lambda (h x)
    (if (hasht? h)
	(in-hash? h x)
      (die 'hashtable-contains? "not a hash table" h))))

(define hashtable-set!
  (lambda (h x v)
    (if (hasht? h)
	(if (hasht-mutable? h)
	    (put-hash! h x v)
	  (die 'hashtable-set! "hashtable is immutable" h))
      (die 'hashtable-set! "not a hash table" h))))

(define hashtable-update!
  (lambda (h x proc default)
    (if (hasht? h)
	(if (hasht-mutable? h)
	    (if (procedure? proc)
		(update-hash! h x proc default)
	      (die 'hashtable-update! "not a procedure" proc))
	  (die 'hashtable-update! "hashtable is immutable" h))
      (die 'hashtable-update! "not a hash table" h))))

(define hashtable-size
  (lambda (h)
    (if (hasht? h)
	(hasht-count h)
      (die 'hashtable-size "not a hash table" h))))

(define hashtable-delete!
  (lambda (h x)
      ;;; FIXME: should shrink table if number of keys drops below
      ;;; (sqrt (vector-length (hasht-vec h)))
    (if (hasht? h)
	(if (hasht-mutable? h)
	    (del-hash h x)
	  (die 'hashtable-delete! "hash table is immutable" h))
      (die 'hashtable-delete! "not a hash table" h))))

(define (hashtable-entries h)
  (if (hasht? h)
      (get-entries h)
    (die 'hashtable-entries "not a hash table" h)))

(define (hashtable-keys h)
  (if (hasht? h)
      (get-keys h)
    (die 'hashtable-keys "not a hash table" h)))

(define (hashtable-mutable? h)
  (if (hasht? h)
      (hasht-mutable? h)
    (die 'hashtable-mutable? "not a hash table" h)))

(define (hashtable-clear! h)
  (if (hasht? h)
      (if (hasht-mutable? h)
	  (clear-hash! h)
	(die 'hashtable-clear! "hash table is immutable" h))
    (die 'hashtable-clear! "not a hash table" h)))

(define hashtable-copy
  (case-lambda
   ((h)
    (if (hasht? h)
	(if (hasht-mutable? h)
	    (hasht-copy h #f)
	  h)
      (die 'hashtable-copy "not a hash table" h)))
   ((h mutable?)
    (if (hasht? h)
	(if (or mutable? (hasht-mutable? h))
	    (hasht-copy h (and mutable? #t))
	  h)
      (die 'hashtable-copy "not a hash table" h)))))

(define (hashtable-equivalence-function h)
  (if (hasht? h)
      (hasht-equivf h)
    (die 'hashtable-equivalence-function "not a hash table" h)))

(define (hashtable-hash-function h)
  (if (hasht? h)
      (hasht-hashf0 h)
    (die 'hashtable-hash-function "not a hash table" h)))


;;;; hash functions

(define (string-hash s)
  (if (string? s)
      (foreign-call "ikrt_string_hash" s)
    (die 'string-hash "not a string" s)))

(define (string-ci-hash s)
  (if (string? s)
      (foreign-call "ikrt_string_hash"
		    (string-foldcase s))
    (die 'string-ci-hash "not a string" s)))

(define (symbol-hash s)
  (if (symbol? s)
      (foreign-call "ikrt_string_hash" (symbol->string s))
    (die 'symbol-hash "not a symbol" s)))

(define (equal-hash s)
  (string-hash (call-with-string-output-port
		   (lambda (port)
		     (write s port)))))

(define (number-hash x)
  (cond ((fixnum? x)
	 x)
	((flonum? x)
	 (foreign-call "ikrt_flonum_hash" x))
	((bignum? x)
	 (foreign-call "ikrt_bignum_hash" x))
	((ratnum? x)
	 (fxxor (number-hash (numerator x))
		(number-hash (denominator x))))
	(else
	 (fxxor (number-hash (real-part x))
		(number-hash (imag-part x))))))


;;;; done

(set-rtd-printer! (type-descriptor hasht)	(lambda (x p wr)
						  (display "#<hashtable>" p)))

)

;;; end of file
