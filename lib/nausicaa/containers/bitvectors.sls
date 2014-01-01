;;;
;;;Part of: Vicare Scheme
;;;Contents: bitvectors on top of bytevectors
;;;Date: Fri Aug  5, 2011
;;;
;;;Abstract
;;;
;;;	We  interpret a  bytevector as  a  vector of  fixnums stored  in
;;;	words; a word is 16 bits stored in big endianness.
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (nausicaa containers bitvectors)
  (export
    <bitvector>
    <bitvector>.vicare-arguments-validation
    list->bitvector
    vector->bitvector)
  (import (nausicaa)
    (only (vicare platform words)
	  case-word-size)
    (vicare arguments validation)
    (vicare unsafe operations)
    (vicare system $numerics))


;;;; word size

(define-constant NUMBER-OF-BITS-IN-FIXNUM
  ;;R6RS  states that  fixnums are  at least  24 bits  long.  The  value
  ;;selected here is Vicare-specific.
  ;;
  (case-word-size
   ((32)	30)
   ((64)	61)))


;;;; helpers

(define-inline (%bit-ref fx bit-offset)
  ;;Return true if the bit at BIT-OFFSET  in FX is set to 1, else return
  ;;false.
  ;;
  (fxbit-set? fx bit-offset))

(define-inline (%bit-set! fx bit-offset bit-boolean-value)
  ;;Set a single bit in a fx and return the resulting fx.
  ;;
  (let ((mask (fxarithmetic-shift-left 1 bit-offset)))
    (if bit-boolean-value
	(fxior fx mask)
      (fxand fx (fxnot mask)))))

(define-inline (%bit-mask number-of-bits)
  ;;Return a fixnum representing a bitmask with the NUMBER-OF-BITS least
  ;;significant bits set to 1 and all the other bits set to 0.
  ;;
  (+ -1 (expt 2 number-of-bits)))

(define (bit-offset? offset number-of-bits)
  ;;Return true if OFFSET is a  valid bit offset for a bytevector having
  ;;NUMBER-OF-BITS.
  ;;
  (and (or (fixnum? offset)
	   (bignum? offset))
       (>= offset 0)
       (<  offset number-of-bits)))

(define-argument-validation (bit-offset who bitvector obj)
  (bit-offset? obj (<bitvector>-length bitvector))
  (procedure-argument-violation who
    "expected valid bit offset for <bitvector> as argument" bitvector obj))

(define-inline (offset->indexes offset)
  ;;Given a bit offset into a bitvector, return two values: the index of
  ;;the selected word, the index of the bit in the word.
  ;;
  (if (zero? offset)
      (values 0 0)
    (div-and-mod offset (fixnum-width))))

(define (%most-significant-word (O <bitvector>))
  ;;Return a fixnum holding the bits of the most significant word in the
  ;;bytevector; all the other bits are set to 0.
  ;;
  (fxand (%bit-mask (O number-of-bits-in-last-word))
	 ($vector-ref (O pool) (O index-of-last-word))))

(define (%normalise-last-word! (O <bitvector>))
  ;;The last  fixnum in the  pool may  be partially used;  this function
  ;;normalises it so that all the unused bits are set to zero.
  ;;
  ($vector-set! (O pool) (O index-of-last-word) (%most-significant-word O)))


;;;; vector and fixnums storage

#;(define-namespace bits-storage::
  (export
    number-of-bits-in-word
    word-mask
    word-ref
    word-set!
    offset->indexes
    most-significant-word
    mask-fixnum-to-word
    storage=?)

  (define-syntax-rule (number-of-bits-in-word)
    (fixnum-width))

  (define-syntax-rule (word-mask)
    (greatest-fixnum))

  (define word-ref	vector-ref)
  (define word-set!	vector-set!)

  (define-inline (most-significant-word o)
    ;;Return a fixnum holding the bits of the most significant word in the
    ;;bytevector; all the other bits are set to 0.
    ;;
    (let (((o <bits>) o))
      (fxand (%bit-mask  (o number-of-bits-in-last-word))
	     (vector-ref (o pool) (o index-of-last-word)))))

  (define-inline (mask-fixnum-to-word fx)
    ;;Set to  0 all the bits  in the fixnum FX  which are not part  of a
    ;;word.
    ;;
    (fxand fx (word-mask)))

  (define storage=? equal?)

  #| end of namespace |# )


(define-mixin <common-uid-clauses>

  (fields (mutable the-uid))

  (method (uid (O <class>))
    (or (O the-uid)
	(receive-and-return (uid)
	    (gensym)
	  (set! (O the-uid) uid))))

  (method (hash (O <class>))
    (symbol-hash (O uid)))

  (method (putprop (O <class>) (key <symbol>) value)
    (putprop (O uid) key value))

  (method (getprop (O <class>) (key <symbol>))
    (getprop (O uid) key))

  (method (remprop (O <class>) (key <symbol>))
    (remprop (O uid) key))

  (method (property-list (O <class>))
    (property-list (O uid)))

  #| end of mixin |# )


(define-class <bitvector>
  ;;We  want a  mutable bit-vector  here.  Notice  that: while  it could
  ;;appear convenient  to use a single  fixnum or bignum as  storage for
  ;;bits,  such objects  are  immutable and  would  lead to  significant
  ;;memory allocation at every operation.
  ;;
  (nongenerative nausicaa:containers:bitvectors:<bitvector>)
  (mixins (<common-uid-clauses>
	   (<class>	<bitvector>)))
  (fields
   (immutable (pool <vector>))
		;The vector of fixnums used as storage for bit values.
   (immutable (length <integer>))
		;The number of bits in the bitvector.
   (immutable (number-of-full-words <integer>))
		;= index of last word
   (immutable (number-of-bits-in-last-word <integer>)
		;The number  of bits,  in the last  fixnum of  the pool,
		;which are part of the bitvector.
	      ))

  (virtual-fields
   (immutable index-of-last-word <bitvector>-number-of-full-words)
   (immutable $pool-length (lambda ((B <bitvector>))
			     ($vector-length (B pool)))))

  (protocol
   (lambda (make-top)
     (lambda (number-of-bits)
       (define who '<bitvector>)
       (receive (D M)
	   (div-and-mod number-of-bits NUMBER-OF-BITS-IN-FIXNUM)
	 (unless (and (fixnum? D)
		      (fixnum? M))
	   (procedure-argument-violation who "requested number of bits too big" number-of-bits))
	 (let* ((number-of-words		(if ($fxzero? M) D (+ 1 D)))
		(number-of-full-words		(if ($fxzero? M) number-of-words D))
		(number-of-bits-in-last-word	M))
	   (unless (fixnum? number-of-words)
	     (procedure-argument-violation who "requested number of bits too big" number-of-bits))
	   ((make-top) (make-vector number-of-words 0)
	    number-of-bits number-of-full-words number-of-bits-in-last-word
	    #f #;uid ))))))

;;; --------------------------------------------------------------------

  (method (clone (O <bitvector>))
    (receive-and-return ((R <bitvector>))
	(<bitvector> ((O length)))
      ($vector-copy! (O pool) 0
		     (R pool) 0
		     (O $pool-length))))

;;; --------------------------------------------------------------------

  (setter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?O ((?offset)) ?boolean)
	       #`(#,tag #:oopp-syntax (?O bit-set! ?offset ?boolean)))
	      )))

  (getter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?O ((?offset)))
	       #`(#,tag #:oopp-syntax (?O bit-ref ?offset)))
	      )))

  (method (bit-set! (O <bitvector>) offset boolean-value)
    (define who '<bitvector>-bit-set!)
    (with-arguments-validation (who)
	((bit-offset	O offset))
      (receive (word-index bit-index)
	  (offset->indexes offset)
	(let ((word ($vector-ref (O pool) word-index)))
	  ($vector-set! (O pool) word-index (%bit-set! word bit-index boolean-value))))))

  (method (bit-ref (O <bitvector>) offset)
    (define who '<bitvector>-bit-ref)
    (with-arguments-validation (who)
	((bit-offset	O offset))
      (receive (word-index bit-index)
	  (offset->indexes offset)
	(%bit-ref ($vector-ref (O pool) word-index) bit-index))))

  (method (set-all! (O <bitvector>))
    ($vector-fill! (O pool) 0 (O $pool-length) (greatest-fixnum)))

  (method (clear-all! (O <bitvector>))
    ($vector-fill! (O pool) 0 (O $pool-length) 0))

  (method (toggle! (O <bitvector>) offset)
    (define who '<bitvector>-toggle!)
    (with-arguments-validation (who)
	((bit-offset	O offset))
      (receive (word-index bit-index)
	  (offset->indexes offset)
	(let ((word ($vector-ref (O pool) word-index)))
	  ($vector-set! (O pool) word-index (%bit-set! word bit-index (not (%bit-ref word bit-index))))))))

;;; --------------------------------------------------------------------

  (method (= (A <bitvector>) (B <bitvector>))
    ;;Normalise the last (possibly partially used) word...
    (%normalise-last-word! A)
    (%normalise-last-word! B)
    ;;... then compare the vectors.
    (vector-for-all fx=? (A pool) (B pool)))

;;; --------------------------------------------------------------------

  (method (not (O <bitvector>))
    (receive-and-return ((R <bitvector>))
	(<bitvector> ((O length)))
      (do ((i 0 (+ 1 i)))
	  ((= i (O $pool-length)))
	($vector-set! (R pool) i (fxnot ($vector-ref (O pool) i))))))

  (method (not! (O <bitvector>))
    (do ((i 0 (+ 1 i)))
	((= i (O $pool-length))
	 O)
      ($vector-set! (O pool) i (fxnot ($vector-ref (O pool) i)))))

;;; --------------------------------------------------------------------

  (method (and (A <bitvector>) (B <bitvector>))
    (define who '<bitvector>-and)
    (with-arguments-validation (who)
	((bitvectors-of-equal-length	A B))
      (receive-and-return ((R <bitvector>))
	  (<bitvector> ((A length)))
	(do ((i 0 (+ 1 i)))
	    ((= i (A $pool-length)))
	  ($vector-set! (R pool) i (fxand ($vector-ref (A pool) i)
					  ($vector-ref (B pool) i)))))))

  (method (and! (A <bitvector>) (B <bitvector>))
    (define who '<bitvector>-and!)
    (with-arguments-validation (who)
	((bitvectors-of-equal-length	A B))
      (do ((i 0 (+ 1 i)))
	  ((= i (A $pool-length))
	   A)
	($vector-set! (A pool) i (fxand ($vector-ref (A pool) i)
					($vector-ref (B pool) i))))))

;;; --------------------------------------------------------------------

  (method (ior (A <bitvector>) (B <bitvector>))
    (define who '<bitvector>-ior)
    (with-arguments-validation (who)
	((bitvectors-of-equal-length	A B))
      (receive-and-return ((R <bitvector>))
	  (<bitvector> ((A length)))
	(do ((i 0 (+ 1 i)))
	    ((= i (A $pool-length)))
	  ($vector-set! (R pool) i (fxior ($vector-ref (A pool) i)
					  ($vector-ref (B pool) i)))))))

  (method (ior! (A <bitvector>) (B <bitvector>))
    (define who '<bitvector>-ior!)
    (with-arguments-validation (who)
	((bitvectors-of-equal-length	A B))
      (do ((i 0 (+ 1 i)))
	  ((= i (A $pool-length))
	   A)
	($vector-set! (A pool) i (fxior ($vector-ref (A pool) i)
					($vector-ref (B pool) i))))))

;;; --------------------------------------------------------------------

  (method (xor (A <bitvector>) (B <bitvector>))
    (define who '<bitvector>-xor)
    (with-arguments-validation (who)
	((bitvectors-of-equal-length	A B))
    (receive-and-return ((R <bitvector>))
	(<bitvector> ((A length)))
      (do ((i 0 (+ 1 i)))
	  ((= i (A $pool-length)))
	($vector-set! (R pool) i (fxxor ($vector-ref (A pool) i)
					($vector-ref (B pool) i)))))))

  (method (xor! (A <bitvector>) (B <bitvector>))
    (define who '<bitvector>-xor!)
    (with-arguments-validation (who)
	((bitvectors-of-equal-length	A B))
      (do ((i 0 (+ 1 i)))
	  ((= i (A $pool-length))
	   A)
	($vector-set! (A pool) i (fxxor ($vector-ref (A pool) i)
					($vector-ref (B pool) i))))))

;;; --------------------------------------------------------------------

  (method (bit-count (O <bitvector>))
    (let ((count 0))
      (do ((i 0 (+ 1 i)))
	  ((= i (O index-of-last-word))
	   (+ count (fxbit-count (%most-significant-word O))))
	(incr! count (fxbit-count ($vector-ref (O pool) i))))))

  (method (first-bit-set (O <bitvector>))
    (let next-word ((i 0) (offset 0) (word ($vector-ref (O pool) 0)))
      (let ((c (fxfirst-bit-set word)))
	(if (fx=? c -1)
	    (if (= i (O index-of-last-word))
		(let ((c (fxfirst-bit-set (%most-significant-word O))))
		  (if (fx=? c -1)
		      -1
		    (+ c offset)))
	      (let ((i1 (+ 1 i)))
		(next-word i1 (+ offset NUMBER-OF-BITS-IN-FIXNUM) ($vector-ref (O pool) i1))))
	  (+ c offset)))))

;;; --------------------------------------------------------------------

  (method (list (O <bitvector>))
    (let words-loop ((bits '()) (full-word-index 0))
      (if (= full-word-index (O number-of-full-words))
	  (let ((word ($vector-ref (O pool) full-word-index)))
	    (let bits-loop-tail-word ((bits bits)
				      (bit-index 0))
	      (if (= bit-index (O number-of-bits-in-last-word))
		  (reverse bits)
		(bits-loop-tail-word (cons (%bit-ref word bit-index) bits)
				     (+ 1 bit-index)))))
	(let ((word ($vector-ref (O pool) full-word-index)))
	  (words-loop
	   (let bits-loop-full-word ((bits bits)
				     (bit-index 0))
	     (if (= bit-index NUMBER-OF-BITS-IN-FIXNUM)
		 bits
	       (bits-loop-full-word (cons (%bit-ref word bit-index) bits)
				    (+ 1 bit-index))))
	   (+ 1 full-word-index))))))

  (method (vector (O <bitvector>))
    (let ((V (make-vector (O length))))
      (do ((full-word-index 0 (+ 1 full-word-index))
	   (vector-index    0 vector-index)) ;we increment it below
	  ((= full-word-index (O number-of-full-words))
	   (let ((word ($vector-ref (O pool) full-word-index)))
	     (do ((bit-index 0 (+ 1 bit-index)))
		 ((= bit-index (O number-of-bits-in-last-word))
		  V)
	       (vector-set! V vector-index (%bit-ref word bit-index))
	       (incr! vector-index))))
	(let ((word ($vector-ref (O pool) full-word-index)))
	  (do ((bit-index 0 (+ 1 bit-index)))
	      ((= bit-index NUMBER-OF-BITS-IN-FIXNUM)
	       )
	    (vector-set! V vector-index (%bit-ref word bit-index))
	    (incr! vector-index)
	    )))))

  #| end of class definition |# )

(define-argument-validation (<bitvector> who obj)
  ((<bitvector>) obj)
  (procedure-argument-violation who "expected <bitvector> as argument" obj))

(define-argument-validation (bitvectors-of-equal-length who A B)
  (= (<bitvector>-length A)
     (<bitvector>-length B))
  (procedure-argument-violation who
    "expected <bitvector> arguments with equal length" A B))


;;;; bitvector functions

(define (list->bitvector ell)
  (define who 'list-bitvector)
  (with-arguments-validation (who)
      ((list		ell))
    (let* ((len (length ell))
	   ((bv <bitvector>) (<bitvector> [len])))
      (do ((i 0 (add1 i))
	   (ell ell ($cdr ell)))
	  ((= i len)
	   bv)
	(set! bv[i] ($car ell))))))

(define (vector->bitvector (V <vector>))
  (receive-and-return ((bv <bitvector>))
      (<bitvector> ((V length)))
    (do ((i 0 (+ 1 i)))
	((= i (V length)))
      (set! bv[i] (V[i])))))


;;;; done

)

;;; end of file
