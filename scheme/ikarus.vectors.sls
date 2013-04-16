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
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (ikarus vectors)
  (export
    make-vector		vector
    subvector		vector-length
    vector-ref		vector-set!
    vector->list	list->vector
    vector-map		vector-for-each
    vector-for-all	vector-exists
    vector-fill!	vector-append
    vector-copy		vector-copy!)
  (import (except (ikarus)
		  make-vector		vector
		  subvector		vector-length
		  vector-ref		vector-set!
		  vector->list		list->vector
		  vector-map		vector-for-each
		  vector-for-all	vector-exists
		  vector-fill!		vector-append
		  vector-copy		vector-copy!)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (vector who obj)
  (vector? obj)
  (assertion-violation who "expected vector as argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (length who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as vector length argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected fixnum as vector index argument" obj))

(define-argument-validation (index-for who idx vec)
  (unsafe.fx< idx (unsafe.vector-length vec))
  (assertion-violation who "index argument is out of range for vector" idx vec))

;;; --------------------------------------------------------------------

(define-argument-validation (start-index-and-length who idx len)
  ;;To be used after INDEX validation.
  ;;
  (unsafe.fx<= idx len)
  (assertion-violation who "start index argument out of range for vector" idx len))

(define-argument-validation (end-index-and-length who idx len)
  ;;To be used after INDEX validation.
  ;;
  (unsafe.fx<= idx len)
  (assertion-violation who "end index argument out of range for vector" idx len))

(define-argument-validation (start-and-end-indices who start end)
  ;;To be used after INDEX validation.
  ;;
  (unsafe.fx<= start end)
  (assertion-violation who "start and end index arguments are in decreasing order" start end))

;;; --------------------------------------------------------------------

(define-argument-validation (count who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as items count argument" obj))

(define-argument-validation (start-index-and-count-and-length who start count len)
  (unsafe.fx<= (unsafe.fx+ start count) len)
  (assertion-violation who
    (vector-append "count argument out of range for vector of length " (number->string len)
		   " and start index " (number->string start))
    count))

;;; --------------------------------------------------------------------

(define-argument-validation (vector-of-length who vec len)
  (unsafe.fx= len (unsafe.vector-length vec))
  (assertion-violation who "expected vector arguments with the same length" len vec))

(define-argument-validation (list-of-vectors-of-length who who1 ell len)
  ;;WHO is used  twice in the arguments list because we  need it also in
  ;;the predicate expression.
  ;;
  (let next-vector ((ell ell)
		    (len len))
    (or (null? ell)
	(let ((a (unsafe.car ell)))
	  (with-arguments-validation (who1)
	      ((vector a))
	    (and (unsafe.fx= (unsafe.vector-length a) len)
		 (next-vector (unsafe.cdr ell) len))))))
  (assertion-violation who "expected vector arguments with the same length" len ell))


;;;; constants

(define EXPECTED_PROPER_LIST_AS_ARGUMENT
  "expected proper list as argument")


;;;; helpers

(define (%unsafe.vector-copy! src.vec src.start
			      dst.vec dst.start
			      src.end)
  (unsafe.vector-copy! src.vec src.start
		       dst.vec dst.start
		       src.end))


(define (vector-length vec)
  ;;Defined by R6RS.   Return the number of elements in  VEC as an exact
  ;;integer object.
  ;;
  (define who 'vector-length)
  (with-arguments-validation (who)
      ((vector vec))
    (unsafe.vector-length vec)))

(define make-vector
  ;;Defined by R6RS.   Return a newly allocated vector  of LEN elements.
  ;;If a second  argument is given, then each  element is initialized to
  ;;FILL.    Otherwise  the   initial  contents   of  each   element  is
  ;;unspecified.
  ;;
  (case-lambda
   ((len)
    (make-vector len (void)))
   ((len fill)
    (define who 'make-vector)
    (with-arguments-validation (who)
	((length len))
      (let loop ((vec	(unsafe.make-clean-vector len))
		 (i	0)
		 (len	len)
		 (fill	fill))
	(if (unsafe.fx= i len)
	    vec
	  (begin
	    (unsafe.vector-set! vec i fill)
	    (loop vec (unsafe.fxadd1 i) len fill))))))))

(define vector
  ;;Defined  by R6RS.  Return  a newly  allocated vector  whose elements
  ;;contain the given arguments.  Analogous to LIST.
  ;;
  (case-lambda
   (()
    '#())
   ((one)
    (let ((vec (unsafe.make-clean-vector 1)))
      (unsafe.vector-set! vec 0 one)
      vec))
   ((one two)
    (let ((vec (unsafe.make-clean-vector 2)))
      (unsafe.vector-set! vec 0 one)
      (unsafe.vector-set! vec 1 two)
      vec))
   ((one two three)
    (let ((vec (unsafe.make-clean-vector 3)))
      (unsafe.vector-set! vec 0 one)
      (unsafe.vector-set! vec 1 two)
      (unsafe.vector-set! vec 2 three)
      vec))
   ((one two three four)
    (let ((vec (unsafe.make-clean-vector 4)))
      (unsafe.vector-set! vec 0 one)
      (unsafe.vector-set! vec 1 two)
      (unsafe.vector-set! vec 2 three)
      (unsafe.vector-set! vec 3 four)
      vec))
   ((one . ls)
    (define (length ls n)
      (if (null? ls)
	  n
	(length (unsafe.cdr ls) (unsafe.fxadd1 n))))
    (define (loop v ls i n)
      (if (unsafe.fx= i n)
	  v
	(begin
	  (unsafe.vector-set! v i (unsafe.car ls))
	  (loop v (unsafe.cdr ls) (unsafe.fxadd1 i) n))))
    (define who 'make-vector)
    (let* ((ls (cons one ls))
	   (n  (length ls 0)))
      (with-arguments-validation (who)
	  ((length n))
	(loop (unsafe.make-clean-vector n) ls 0 n))))))

(define (vector-fill! vec fill)
  ;;Defined by  R6RS.  Store  FILL in every  element of VEC  and returns
  ;;unspecified.
  ;;
  (define who 'vector-fill!)
  (with-arguments-validation (who)
      ((vector vec))
    (let f ((vec  vec)
	    (i    0)
	    (len  (unsafe.vector-length vec))
	    (fill fill))
      (unless (unsafe.fx= i len)
	(unsafe.vector-set! vec i fill)
	(f vec (unsafe.fxadd1 i) len fill)))))


(define (vector-ref vec idx)
  ;;Defined by  R6RS.  IDX  must be  a valid index  of VEC.   Return the
  ;;contents of element IDX of VEC.
  ;;
  (define who 'vector-ref)
  (with-arguments-validation (who)
      ((vector		vec)
       (index		idx)
       (index-for	idx vec))
    (unsafe.vector-ref vec idx)))

(define (vector-set! vec idx new-item)
  ;;Defined by R6RS.  IDX must be  a valid index of VEC.  Store NEW-ITEM
  ;;in element IDX of VEC, and return unspecified values.
  ;;
  ;;Passing an immutable vector to VECTOR-SET! should cause an exception
  ;;with condition type "&assertion" to be raised.
  ;;
  (define who 'vector-set!)
  (with-arguments-validation (who)
      ((vector		vec)
       (index		idx)
       (index-for	idx vec))
    (unsafe.vector-set! vec idx new-item)))


(define (vector->list vec)
  ;;Defined  by R6RS.   Return a  newly  allocated list  of the  objects
  ;;contained in the elements of VEC.
  ;;
  (define who 'vector->list)
  (define (f vec idx ls)
    (if (unsafe.fx< idx 0)
	ls
      (f vec (unsafe.fxsub1 idx) (cons (unsafe.vector-ref vec idx) ls))))
  (with-arguments-validation (who)
      ((vector vec))
    (let ((len (unsafe.vector-length vec)))
      (if (unsafe.fxzero? len)
	  '()
	(f vec (unsafe.fxsub1 len) '())))))

(define (list->vector ls)
  ;;Defined by R6RS.   Return a newly created vector  initialized to the
  ;;elements of the list LS.
  ;;
  (define who 'list->vector)
  (define (race h t ls n)
    (cond ((pair? h)
	   (let ((h (unsafe.cdr h)))
	     (cond ((pair? h)
		    (if (not (eq? h t))
			(race (unsafe.cdr h) (unsafe.cdr t) ls (unsafe.fx+ n 2))
		      (assertion-violation who
			"circular list is invalid as argument" ls)))
		   ((null? h)
		    (unsafe.fx+ n 1))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))

  (define (fill v i ls)
    (if (null? ls)
	v
      (let ((c (unsafe.car ls)))
	(unsafe.vector-set! v i c)
	(fill v (unsafe.fxadd1 i) (cdr ls)))))

  (let ((n (race ls ls ls 0)))
    (with-arguments-validation (who)
	((length n))
      (fill (unsafe.make-clean-vector n) 0 ls))))


(module (vector-map)
  ;;Defined  by R6RS.   The  vector  arguments must  all  have the  same
  ;;length.  The procedure should accept  as many arguments as there are
  ;;vector arguments and return a single value.
  ;;
  ;;The VECTOR-MAP  procedure applies P element-wise to  the elements of
  ;;the vectors  and returns a  vector of the  results, in order.   P is
  ;;always called in the  same dynamic environment as VECTOR-MAP itself.
  ;;The order  in which P is applied  to the elements of  the vectors is
  ;;unspecified.  If multiple returns  occur from VECTOR-MAP, the return
  ;;values returned by earlier returns are not mutated.
  ;;
  ;;Analogous to MAP.
  ;;
  ;;IMPLEMENTATION  RESPONSIBILITIES The  implementation must  check the
  ;;restrictions  on  P  to  the  extent performed  by  applying  it  as
  ;;described.  An implementation may  check whether P is an appropriate
  ;;argument before applying it.
  ;;
  (define who 'vector-map)

  (define (ls->vec ls n)
    (let f ((v  (unsafe.make-clean-vector n))
	    (n  n)
	    (ls ls))
      (if (null? ls)
	  v
	(let ((n (unsafe.fxsub1 n)))
	  (unsafe.vector-set! v n (unsafe.car ls))
	  (f v n (unsafe.cdr ls))))))

  (define vector-map
    ;;Notice that R6RS states:
    ;;
    ;;"If multiple  returns occur  from VECTOR-MAP, the  return values
    ;;returned by earlier returns are not mutated."
    ;;
    ;;so  if  we  jump  back   into  the  mapping  procedure  using  a
    ;;continuation,  VECTOR-MAP  must   return  a  new  vector.   This
    ;;behaviour can be demonstrated with the following program:
    ;;
    ;;#!r6rs
    ;;(import (rnrs))
    ;;(let ((only-once #t)
    ;;      (v0 (vector 1 2 3 4 5 6))
    ;;      (cl '())
    ;;      (old-v1 #f))
    ;;  (let ((v1 (vector-map (lambda (elt)
    ;;                          (call/cc
    ;;                              (lambda (c)
    ;;                                (set! cl (cons c cl))
    ;;                                (* elt elt))))
    ;;                        v0)))
    ;;    (when only-once
    ;;      (set! only-once #f)
    ;;      (set! old-v1 v1)
    ;;      ((car (reverse cl)) 'x))
    ;;
    ;;    (write v0)(newline)
    ;;    (write old-v1)(newline)
    ;;    (write v1)(newline)))
    ;;
    ;;which must print:
    ;;
    ;;#(1 2 3 4 5 6)
    ;;#(1 4 9 16 25 36)
    ;;#(x 4 9 16 25 36)
    ;;
    ;;rather than:
    ;;
    ;;#(1 2 3 4 5 6)
    ;;#(x 4 9 16 25 36)  ;; wrong!!!
    ;;#(x 4 9 16 25 36)
    ;;
    (case-lambda
     ((p v)
      (with-arguments-validation (who)
	  ((procedure	p)
	   (vector	v))
	(let f ((p  p)
		(v  v)
		(i  0)
		(n  (vector-length v))
		(ac '()))
	  (if (unsafe.fx= i n)
	      (ls->vec ac n)
	    (f p v (unsafe.fxadd1 i) n (cons (p (vector-ref v i)) ac))))))

     ((p v0 v1)
      (with-arguments-validation (who)
	  ((procedure	p)
	   (vector	v0)
	   (vector	v1))
	(let ((n (vector-length v0)))
	  (unless (unsafe.fx= n (unsafe.vector-length v1))
	    (assertion-violation who "length mismatch" v0 v1))
	  (let f ((p  p)
		  (v0 v0)
		  (v1 v1)
		  (i  0)
		  (n  n)
		  (ac '()))
	    (if (unsafe.fx= i n)
		(ls->vec ac n)
	      (f p v0 v1 (unsafe.fxadd1 i) n
		 (cons (p (unsafe.vector-ref v0 i) (unsafe.vector-ref v1 i)) ac)))))))

     ((p v0 v1 . v*)
      (with-arguments-validation (who)
	  ((procedure	p)
	   (vector	v0)
	   (vector	v1))
	(let ((n (vector-length v0)))
	  (with-arguments-validation (who)
	      ((vector-of-length          v1 n)
	       (list-of-vectors-of-length who v* n))
	    (let f ((p p) (v0 v0) (v1 v1) (v* v*) (i 0) (n n) (ac '()))
	      (if (unsafe.fx= i n)
		  (ls->vec ac n)
		(f p v0 v1 v* (unsafe.fxadd1 i) n
		   (cons (apply p (unsafe.vector-ref v0 i) (unsafe.vector-ref v1 i)
				(let f ((i i) (v* v*))
				  (if (null? v*)
				      '()
				    (cons (unsafe.vector-ref (unsafe.car v*) i)
					  (f i (unsafe.cdr v*))))))
			 ac))))))))
     ))

  #| end of module|# )


(define vector-for-each
  ;;Defined  by R6RS.   The  vector  arguments must  all  have the  same
  ;;length.  The  procedure P should  accept as many arguments  as there
  ;;are vectors.   The VECTOR-FOR-EACH procedure  applies P element-wise
  ;;to the elements  of the vectors for its side  effects, in order from
  ;;the first  elements to  the last.   P is always  called in  the same
  ;;dynamic environment as VECTOR-FOR-EACH itself.  The return values of
  ;;VECTOR-FOR-EACH are unspecified.
  ;;
  ;;Analogous to FOR-EACH.
  ;;
  ;;IMPLEMENTATION  RESPONSIBILITIES The  implementation must  check the
  ;;restrictions  on  P  to  the  extent performed  by  applying  it  as
  ;;described.  An implementation may  check whether P is an appropriate
  ;;argument before applying it.
  ;;
  (case-lambda
   ((p v)
    (define who 'vector-for-each)
    (with-arguments-validation (who)
	((procedure p)
	 (vector    v))
      (let f ((p p) (v v) (i 0) (n (unsafe.vector-length v)))
	(if (unsafe.fx= i n)
	    (void)
	  (begin
	    (p (unsafe.vector-ref v i))
	    (f p v (unsafe.fxadd1 i) n))))))

   ((p v0 v1)
    (define who 'vector-for-each)
    (with-arguments-validation (who)
	((procedure p)
	 (vector    v0)
	 (vector    v1))
      (let ((n (unsafe.vector-length v0)))
	(with-arguments-validation (who)
	    ((vector-of-length v1 n))
	  (let f ((p p) (v0 v0) (v1 v1) (i 0) (n n))
	    (if (unsafe.fx= i n)
		(void)
	      (begin
		(p (unsafe.vector-ref v0 i)
		   (unsafe.vector-ref v1 i))
		(f p v0 v1 (unsafe.fxadd1 i) n))))))))

   ((p v0 v1 . v*)
    (define who 'vector-for-each)
    (with-arguments-validation (who)
	((procedure p)
	 (vector    v0)
	 (vector    v1))
      (let ((n (unsafe.vector-length v0)))
	(with-arguments-validation (who)
	    ((vector-of-length          v1 n)
	     (list-of-vectors-of-length who v* n))
	  (let f ((p p) (v0 v0) (v1 v1) (v* v*) (i 0) (n n))
	    (if (unsafe.fx= i n)
		(void)
	      (begin
		(apply p (unsafe.vector-ref v0 i) (unsafe.vector-ref v1 i)
		       (let f ((i i) (v* v*))
			 (if (null? v*)
			     '()
			   (cons (unsafe.vector-ref (unsafe.car v*) i)
				 (f i (unsafe.cdr v*))))))
		(f p v0 v1 v* (unsafe.fxadd1 i) n))))))))
   ))


(define-syntax define-vector-iterator
  (syntax-rules ()
    ((_ ?name ?combine)
     (define (?name proc vec . vectors)
       (define who '?name)
       (define (iterator-1 proc vec)
	 (unless (vector? vec)
	   (assertion-violation who "not a vector" vec))
	 (let ((len (vector-length vec)))
	   (if (zero? len)
	       (?combine) ;not PROC!!!
	     (let ((len-1 (- len 1)))
	       (let loop ((i 0))
		 (if (= i len-1)
		     (proc (vector-ref vec i)) ;tail call deciding the return value
		   (?combine (proc (vector-ref vec i))
			     (loop (+ 1 i)))))))))
       (define (iterator-n proc vectors)
	 ;;To be called with 2 or more vector arguments.
	 ;;
	 (let loop ((vectors vectors))
	   (unless (null? vectors)
	     (if (vector? (car vectors))
		 (loop (cdr vectors))
	       (assertion-violation who "not a vector" (car vectors)))))
	 (let ((len (vector-length (car vectors))))
	   (unless (for-all (lambda (vec)
			      (= len (vector-length vec)))
		     (cdr vectors))
	     (assertion-violation who "length mismatch" vectors))
	   (let ((len-1 (- len 1)))
	     (let loop ((i 0))
	       (if (= i len-1)
		   (apply proc (map (lambda (vec)
				      (vector-ref vec i))
				 vectors)) ;tail call deciding the return value
		 (?combine (apply proc (map (lambda (vec)
					      (vector-ref vec i))
					 vectors))
			   (loop (+ 1 i))))))))
       (if (null? vectors)
	   (iterator-1 proc vec)
	 (iterator-n proc (cons vec vectors)))))))

(define-vector-iterator vector-for-all and)
(define-vector-iterator vector-exists  or)


(define vector-append
  ;;Defined by Vicare.  Return a newly allocated vector whose items form
  ;;the concatenation of the given vectors.
  ;;
  (case-lambda
   (() '#())

   ((vec)
    (define who 'vector-append)
    (with-arguments-validation (who)
	((vector vec))
      vec))

   ((vec1 vec2)
    (define who 'vector-append)
    (with-arguments-validation (who)
	((vector vec1)
	 (vector vec2))
      (let* ((len1	(unsafe.vector-length vec1))
	     (len2	(unsafe.vector-length vec2))
	     (dst.len	(+ len1 len2)))
	(with-arguments-validation (who)
	    ((length dst.len))
	  (let ((dst.vec (unsafe.make-clean-vector dst.len)))
	    (%unsafe.vector-copy! vec1 0 dst.vec 0    len1)
	    (%unsafe.vector-copy! vec2 0 dst.vec len1 len2)
	    dst.vec)))))

   ((vec1 vec2 vec3)
    (define who 'vector-append)
    (with-arguments-validation (who)
	((vector vec1)
	 (vector vec2)
	 (vector vec3))
      (let* ((len1	(unsafe.vector-length vec1))
	     (len2	(unsafe.vector-length vec2))
	     (len3	(unsafe.vector-length vec3))
	     (dst.len	(+ len1 len2 len3)))
	(with-arguments-validation (who)
	    ((length  dst.len))
	  (let ((dst.vec (unsafe.make-clean-vector dst.len)))
	    (%unsafe.vector-copy! vec1 0 dst.vec 0    len1)
	    (%unsafe.vector-copy! vec2 0 dst.vec len1 len2)
	    (%unsafe.vector-copy! vec3 0 dst.vec (unsafe.fx+ len1 len2) len3)
	    dst.vec)))))

   ((vec1 vec2 vec3 vec4)
    (define who 'vector-append)
    (with-arguments-validation (who)
	((vector  vec1)
	 (vector  vec2)
	 (vector  vec3)
	 (vector  vec4))
      (let* ((len1	(unsafe.vector-length vec1))
	     (len2	(unsafe.vector-length vec2))
	     (len3	(unsafe.vector-length vec3))
	     (len4	(unsafe.vector-length vec4))
	     (dst.len	(+ len1 len2 len3 len4)))
	(with-arguments-validation (who)
	    ((length  dst.len))
	  (let ((dst.vec (unsafe.make-clean-vector dst.len)))
	    (%unsafe.vector-copy! vec1 0 dst.vec 0    len1)
	    (%unsafe.vector-copy! vec2 0 dst.vec len1 len2)
	    (let ((dst.start (unsafe.fx+ len1 len2)))
	      (%unsafe.vector-copy! vec3 0 dst.vec dst.start len3)
	      (let ((dst.start (unsafe.fx+ dst.start len3)))
		(%unsafe.vector-copy! vec4 0 dst.vec dst.start len4)))
	    dst.vec)))))

   ((vec1 . vecs)
    (define who 'vector-append)
    (define (%length-and-validation vecs len)
      (if (null? vecs)
	  len
	(let ((vec (unsafe.car vecs)))
	  (with-arguments-validation (who)
	      ((vector vec))
	    (%length-and-validation (unsafe.cdr vecs) (+ len (unsafe.vector-length vec)))))))

    (define (%fill-vectors dst.vec vecs dst.start)
      (if (null? vecs)
	  dst.vec
	(let* ((src.vec (unsafe.car vecs))
	       (src.len (unsafe.vector-length src.vec)))
	  (begin
	    (unsafe.vector-copy! src.vec 0 dst.vec dst.start src.len)
	    (%fill-vectors dst.vec (unsafe.cdr vecs) (unsafe.fx+ dst.start src.len))))))

    (let* ((vecs    (cons vec1 vecs))
           (dst.len (%length-and-validation vecs 0)))
      (with-arguments-validation (who)
	  ((length dst.len))
	(%fill-vectors (unsafe.make-clean-vector dst.len) vecs 0))))))


(define (subvector vec start end)
  ;;Defined by Vicare.  VEC must be  a vector, and START and END must be
  ;;exact integer objects satisfying:
  ;;
  ;; 0 <= START <= END <= (vector-length VEC)
  ;;
  ;;Return  a  newly allocated  vector  formed  from  the items  of  VEC
  ;;beginning  with index START  (inclusive) and  ending with  index END
  ;;(exclusive).
  ;;
  (define who 'subvector)
  (with-arguments-validation (who)
      ((vector	vec)
       (index	start)
       (index	end))
    (let ((len (unsafe.vector-length vec)))
      (with-arguments-validation (who)
	  ((start-index-and-length	start len)
	   (end-index-and-length	end   len)
	   (start-and-end-indices	start end))
	(unsafe.subvector vec start end)))))

(define (vector-copy vec)
  ;;Defined by Vicare.  Return a newly allocated copy of the given VEC.
  ;;
  (define who 'vector-copy)
  (with-arguments-validation (who)
      ((vector vec))
    (let ((end (unsafe.vector-length vec)))
      (unsafe.subvector vec 0 end))))

(define (vector-copy! src.vec src.start dst.vec dst.start count)
  ;;Defined  by  Vicare.  Copy  COUNT  items  from  SRC.VEC starting  at
  ;;SRC.START  (inclusive)  to DST.VEC  starting  at DST.START.   Return
  ;;unspecified values.
  ;;
  (define who 'vector-copy!)
  (with-arguments-validation (who)
      ((vector		src.vec)
       (vector		dst.vec)
       (index		src.start)
       (index		dst.start)
       (count		count))
    (let ((src.len (unsafe.vector-length src.vec))
	  (dst.len (unsafe.vector-length dst.vec)))
      (with-arguments-validation (who)
	  ((start-index-and-length		src.start src.len)
	   (start-index-and-length		dst.start dst.len)
	   (start-index-and-count-and-length	src.start count src.len)
	   (start-index-and-count-and-length	dst.start count dst.len))
	(cond ((unsafe.fxzero? count)
	       (void))
	      ((eq? src.vec dst.vec)
	       (cond ((unsafe.fx< dst.start src.start)
		      (unsafe.vector-self-copy-forwards!  src.vec src.start dst.start count))
		     ((unsafe.fx> dst.start src.start)
		      (unsafe.vector-self-copy-backwards! src.vec src.start dst.start count))
		     (else (void))))
	      (else
	       (let ((src.end (unsafe.fx+ src.start count)))
		 (unsafe.vector-copy! src.vec src.start dst.vec dst.start src.end))))))))


;;;; done

)


(library (ikarus system vectors)
  (export $vector-ref $vector-length)
  (import (ikarus))
  (define $vector-ref vector-ref)
  (define $vector-length vector-length))

;;; end of file
