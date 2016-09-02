;;;
;;;Part of: Vicare Scheme
;;;Contents: bitvectors on top of bytevectors
;;;Date: Fri Aug  5, 2011
;;;
;;;Abstract
;;;
;;;	We  implement  a  bitvector as  a  vector  of  fixnums.   We want  a  mutable
;;;	bit-vector here.
;;;
;;;     Notice  that: while  it could  appear convenient  to use  a single  fixnum or
;;;     bignum as  storage for  bits, such  objects are immutable  and would  lead to
;;;     significant memory allocation at every operation.
;;;
;;;Copyright (C) 2011, 2013, 2014, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare containers bitvectors)
  (options typed-language)
  (export
    <bitvector>
    list->bitvector
    vector->bitvector)
  (import (vicare)
    (only (vicare platform words)
	  case-word-size)
    (vicare language-extensions friend-functions)
    (vicare language-extensions mixins))


;;;; common clauses mixin

(define-mixin-type <common-compound-object>

  (private
    (fields
      (mutable the-uid)
		;False or a gensym representing the UID of this bitvector.
      ))

  (method ({uid <gensym>})
    (let ((obj (.the-uid this)))
      (case-type obj
	((<false>)
	 (receive-and-return (uid)
	     (gensym)
	   (.the-uid this uid)))
	((<gensym>)
	 obj))))

  (method ({putprop <void>} {key <symbol>} value)
    (putprop (.uid this) key value))

  (method ({getprop <top>} {key <symbol>})
    (getprop (.uid this) key))

  (method ({remprop <void>} {key <symbol>})
    (remprop (.uid this) key))

  (method ({property-list (alist <symbol> <top>)})
    (property-list (.uid this)))

  #| end of DEFINE-MIXIN |# )


;;;; word size

(define-constant NUMBER-OF-BITS-IN-FIXNUM
  ;;R6RS states that fixnums  are at least 24 bits long.  The  value selected here is
  ;;Vicare-specific.
  ;;
  (case-word-size
   ((32)	30)
   ((64)	61)))


;;;; helpers

(define-alias <nnfx>
  <non-negative-fixnum>)

(define-alias <nnei>
  <non-negative-exact-integer>)

(define-type <booleans>
  (list-of <boolean>))

(define ({%bit-ref <boolean>} {fx <fixnum>} {bit-offset <nnfx>})
  ;;Return true if the bit at BIT-OFFSET in FX is set to 1, else return false.
  ;;
  (fxbit-set? fx bit-offset))

(define ({%bit-set! <fixnum>} {fx <fixnum>} {bit-offset <nnfx>} {bit-boolean-value <boolean>})
  ;;Set a single bit in a fx and return the resulting fx.
  ;;
  (let ((mask (fxarithmetic-shift-left 1 bit-offset)))
    (if bit-boolean-value
	(fxior fx mask)
      (fxand fx (fxnot mask)))))

(define ({%bit-mask <fixnum>} {number-of-bits <nnfx>})
  ;;Return a fixnum representing a  bitmask with the NUMBER-OF-BITS least significant
  ;;bits set to 1 and all the other bits set to 0.
  ;;
  (+ -1 (expt 2 number-of-bits)))

(define ({bit-offset? <boolean>} {offset <top>} {number-of-bits <nnei>})
  ;;Return  true  if   OFFSET  is  a  valid  bit  offset   for  a  bytevector  having
  ;;NUMBER-OF-BITS.
  ;;
  (and (or (fixnum? offset)
	   (bignum? offset))
       (>= offset 0)
       (<  offset number-of-bits)))

(define ({offset->indexes <nnfx> <nnfx>} {offset <nnei>})
  ;;Given a bit offset into a bitvector, return two values: the index of the selected
  ;;word, the index of the bit in the word.
  ;;
  (if (zero? offset)
      (values 0 0)
    (div-and-mod offset (fixnum-width))))


(define-record-type <bitvector>
  (nongenerative vicare:containers:bitvectors:<bitvector>)

  (private
    (fields
      (immutable {pool <vector>})
		;The vector of fixnums used as storage for bit values.
      #| end of FIELDS |# ))
  (fields
    (immutable {length <integer>})
		;The number of bits in the bitvector.
    (immutable {number-of-full-words <nnfx>})
		;= index of last word
    (immutable {number-of-bits-in-last-word <nnfx>})
		;The number of bits,  in the last fixnum of the  pool, which are part
		;of the bitvector.
    #| end of FIELDS |# )

  (mixins <common-compound-object>)

  (protocol
    (lambda (make-record)
      (define ({make-<bitvector> <bitvector>} {number-of-bits <nnei>})
	(receive (D M)
	    (div-and-mod number-of-bits NUMBER-OF-BITS-IN-FIXNUM)
	  (unless (and (fixnum? D)
		       (fixnum? M))
	    (procedure-argument-violation __who__ "requested number of bits too big" number-of-bits))
	  (let* ((number-of-words		(if (fxzero? M) D (add1 D)))
		 (number-of-full-words		(if (fxzero? M) number-of-words D))
		 (number-of-bits-in-last-word	M))
	    (unless (fixnum? number-of-words)
	      (procedure-argument-violation __who__ "requested number of bits too big" number-of-bits))
	    (make-record (make-vector number-of-words 0)
			 number-of-bits number-of-full-words number-of-bits-in-last-word
			 #f #;uid ))))

      make-<bitvector>))

  (constructor-signature
    (lambda (<nnei>) => (<bitvector>)))


;;;; common object-type features

(equality-predicate
  (lambda ({_ (equality-predicate <bitvector>)})
    <bitvector>-equality-predicate))

(hash-function
  (lambda ({_ (hash-function <bitvector>)})
    (lambda ({_ <non-negative-fixnum>} {this <bitvector>})
      (symbol-hash (.uid this)))))

(method ({clone <bitvector>})
  (receive-and-return ({R <bitvector>})
      (new <bitvector> (.length this))
    (vector-copy! (.pool this) 0
		  (pool R) 0
		  (pool-length this))))


;;;; bitwise methods

(method (index-of-last-word)
  (.number-of-full-words this))

(method ({bit-set! <void>} {offset <nnei>} boolean-value)
  (assert-bit-offset __who__ this offset)
  (receive ({word-index <nnfx>} {bit-index <nnfx>})
      (offset->indexes offset)
    (let ((word (vector-ref (.pool this) word-index)))
      (vector-set! (.pool this) word-index (%bit-set! word bit-index boolean-value)))))

(method ({bit-ref <boolean>} {offset <nnei>})
  (assert-bit-offset __who__ this offset)
  (receive ({word-index <nnfx>} {bit-index <nnfx>})
      (offset->indexes offset)
    (%bit-ref (pool-ref this word-index) bit-index)))

(method ({set-all! <void>})
  (vector-fill! (.pool this) (greatest-fixnum)))

(method ({clear-all! <void>})
  (vector-fill! (.pool this) 0))

(method ({toggle! <void>} {offset <nnei>})
  (assert-bit-offset __who__ this offset)
  (receive ({word-index <nnfx>} {bit-index <nnfx>})
      (offset->indexes offset)
    (let ((word (vector-ref (.pool this) word-index)))
      (vector-set! (.pool this) word-index (%bit-set! word bit-index (not (%bit-ref word bit-index)))))))


;;;; bitwise negation

(method ({not <bitvector>})
  (let ((imax	(pool-length this))
	(R	(new <bitvector> (.length this))))
    (do ((i 0 (add1 i)))
	((= i imax)
	 R)
      (pool-set! R i (fxnot (pool-ref this i))))))

(method ({not! <bitvector>})
  (let ((imax (pool-length this)))
    (do ((i 0 (add1 i)))
	((= i imax)
	 this)
      (pool-set! this i (fxnot (pool-ref this i))))))


;;;; bitwise AND

(method ({and <bitvector>} {that <bitvector>})
  (assert-bitvectors-of-equal-length __who__ this that)
  (let* ((imax		(pool-length this))
	 (R		(new <bitvector> (.length this)))
	 (this.pool	(.pool this))
	 (that.pool	(pool that))
	 (R.pool	(pool R)))
    (do ((i 0 (add1 i)))
	((= i imax)
	 R)
      (vector-set! R.pool i (fxand (vector-ref this.pool i)
				   (vector-ref that.pool i))))))

(method (and! {that <bitvector>})
  (assert-bitvectors-of-equal-length __who__ this that)
  (let ((imax (pool-length this)))
    (do ((i 0 (add1 i)))
	((= i imax)
	 this)
      (pool-set! this i (fxand (pool-ref this i)
			       (pool-ref that i))))))


;;;; bitwise inclusive OR

(method ({ior <bitvector>} {that <bitvector>})
  (assert-bitvectors-of-equal-length __who__ this that)
  (let* ((imax			(pool-length this))
	 ({R <bitvector>}	(new <bitvector> (.length this))))
    (do ((i 0 (add1 i)))
	((= i imax)
	 R)
      (pool-set! R i (fxior (pool-ref this i)
			    (pool-ref that i))))))

(method (ior! {that <bitvector>})
  (assert-bitvectors-of-equal-length __who__ this that)
  (let ((imax (pool-length this)))
    (do ((i 0 (add1 i)))
	((= i imax)
	 this)
      (pool-set! this i (fxior (pool-ref this i)
			       (pool-ref that i))))))


;;;; bitwise exclusive OR

(method ({xor <bitvector>} {that <bitvector>})
  (assert-bitvectors-of-equal-length __who__ this that)
  (let* ((imax			(pool-length this))
	 ({R <bitvector>}	(new <bitvector> (.length this))))
    (do ((i 0 (add1 i)))
	((= i imax)
	 R)
      (pool-set! R i (fxxor (pool-ref this i)
			    (pool-ref that i))))))

(method (xor! {that <bitvector>})
  (assert-bitvectors-of-equal-length __who__ this that)
  (let ((imax (pool-length this)))
    (do ((i 0 (add1 i)))
	((= i imax)
	 this)
      (pool-set! this i (fxxor (pool-ref this i)
			       (pool-ref that i))))))


;;;; counting and searching bits

(method ({bit-count <nnei>})
  (let (({count <nnei>} 0))
    (do (({i <nnei>} 0 (add1 i)))
	((= i (.index-of-last-word this))
	 (+ count (fxbit-count (%most-significant-word this))))
      (++ count (fxbit-count (pool-ref this i))))))

(method ({first-bit-set <exact-integer>})
  ;;Return an exact integer representing the offset  of the first bit set to true; if
  ;;all the bits are set to false: return -1.
  ;;
  (let next-word (({i		<nnei>} 0)
		  ({offset	<nnei>} 0)
		  ({word	<nnfx>} (pool-ref this 0)))
    (let ((c (fxfirst-bit-set word)))
      (if (fx=? c -1)
	  (if (= i (.index-of-last-word this))
	      (let ((c (fxfirst-bit-set (%most-significant-word this))))
		(if (fx=? c -1)
		    -1
		  (+ c offset)))
	    (let ((i1 (add1 i)))
	      (next-word i1 (+ offset NUMBER-OF-BITS-IN-FIXNUM) (pool-ref this i1))))
	(+ c offset)))))


;;;; conversion

(method ({list <booleans>})
  (let words-loop (({bits		<booleans>}	'())
		   ({full-word-index	<nnfx>}		0))
    (if (= full-word-index (.number-of-full-words this))
	;;We have reached the last word in the pool vector.
	(let (({word <fixnum>} (pool-ref this full-word-index)))
	  (let bits-loop-tail-word (({bits	<booleans>}	bits)
				    ({bit-index	<nnfx>}		0))
	    (if (= bit-index (.number-of-bits-in-last-word this))
		(reverse bits)
	      (bits-loop-tail-word (cons (%bit-ref word bit-index) bits)
				   (fxadd1 bit-index)))))
      ;;We are in the middle of the pool vector.
      (let (({word <fixnum>} (pool-ref this full-word-index)))
	(words-loop (let bits-loop-full-word (({bits      <booleans>}	bits)
					      ({bit-index <nnfx>}	0))
		      (if (= bit-index NUMBER-OF-BITS-IN-FIXNUM)
			  bits
			(bits-loop-full-word (cons (%bit-ref word bit-index) bits)
					     (fxadd1 bit-index))))
		    (fxadd1 full-word-index))))))

(method ({vector (vector-of <boolean>)})
  (let (({V (vector-of <boolean>)} (make-vector (.length this) #f)))
    (do (({full-word-index <nnfx>} 0 (fxadd1 full-word-index))
	 ({vector-index    <nnfx>} 0 vector-index)) ;we increment it below
	((fx=? full-word-index (.number-of-full-words this))
	 ;;Here we do the last word.
	 (let (({word <fixnum>} (pool-ref this full-word-index)))
	   (do (({bit-index <nnfx>} 0 (fxadd1 bit-index)))
	       ((fx=? bit-index (.number-of-bits-in-last-word this))
		V)
	     (vector-set! V vector-index (%bit-ref word bit-index))
	     (++ vector-index))))
      (let ((word (pool-ref this full-word-index)))
	(do (({bit-index <nnfx>} 0 (fxadd1 bit-index)))
	    ((fx=? bit-index NUMBER-OF-BITS-IN-FIXNUM))
	  (vector-set! V vector-index (%bit-ref word bit-index))
	  (++ vector-index))))))

#| end of DEFINE-RECORD-TYPE |# )


;;;; bitvector functions

(define/friend ({<bitvector>-equality-predicate <boolean>} {A <bitvector>} {B <bitvector>})
  (or (eq? A B)
      (and (= (.length A) (.length B))
	   (unsafe-cast-signature (<boolean>)
	     (vector-for-all fx=? (.pool A) (pool B))))))

(define ({list->bitvector <bitvector>} {ell (list-of <boolean>)})
  (let* ((len (length ell))
	 (biv (new <bitvector> len)))
    (if (zero? len)
	biv
      (let (({ell (nelist-of <boolean>)} ell))
	(do ((i 0 (add1 i))
	     (ell ell (cdr ell)))
	    ((= i len)
	     biv)
	  (.bit-set! biv i (car ell)))))))

(define ({vector->bitvector <bitvector>} {V (vector-of <boolean>)})
  (let* ((len (.length V))
	 (biv (new <bitvector> len)))
    (if (zero? len)
	biv
      (do ((i 0 (add1 i)))
	  ((= i (.length V))
	   biv)
	(.bit-set! biv i (vector-ref V i))))))


;;;; pool functions

(define/friend ({pool (vector-of <fixnum>)} {this <bitvector>})
  (.pool this))

(define/friend ({pool-length <nnfx>} {this <bitvector>})
  (vector-length (.pool this)))

(define/friend ({pool-ref <fixnum>} {this <bitvector>} {i <nnfx>})
  (unsafe-cast-signature (<fixnum>) (vector-ref (.pool this) i)))

(define/friend ({pool-set! <void>} {this <bitvector>} {i <nnfx>} {word <fixnum>})
  (vector-set! (.pool this) i word))


;;;; helpers

(define (assert-bit-offset who {bitvector <bitvector>} obj)
  (unless (bit-offset? obj (.length bitvector))
    (procedure-argument-violation who
      "expected valid bit offset for <bitvector> as argument" bitvector obj)))

(define ({%most-significant-word <fixnum>} {O <bitvector>})
  ;;Return a fixnum holding the bits of  the most significant word in the bytevector;
  ;;all the other bits are set to 0.
  ;;
  (fxand (%bit-mask (.number-of-bits-in-last-word O))
	 (vector-ref (pool O) (.index-of-last-word O))))

(define (assert-bitvectors-of-equal-length who {A <bitvector>} {B <bitvector>})
  (unless (= (.length A) (.length B))
    (procedure-argument-violation who
      "expected <bitvector> arguments with equal length" A B)))


;;;; done

#| end of library |# )

;;; end of file
