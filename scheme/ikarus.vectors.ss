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
  (export make-vector vector vector-length vector-ref vector-set!
          vector->list list->vector vector-map vector-for-each
          vector-fill!)
  (import (except (ikarus) make-vector vector
		  vector-length vector-ref vector-set!
		  vector->list list->vector vector-map vector-for-each
		  vector-fill!)
    (ikarus system $vectors)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (vector who obj)
  (vector? obj)
  (assertion-violation who "expected vector as argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (length who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as vector length argument" obj))

(define-argument-validation (index who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as vector index argument" obj))

(define-argument-validation (index-for who idx vec)
  (and (unsafe.fx>= idx 0)
       (unsafe.fx<  idx (unsafe.vector-length vec)))
  (assertion-violation who "index argument is out of range for vector" idx vec))

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


(define (vector-length x)
  (with-arguments-validation (vector-length)
      ((vector x))
    (unsafe.vector-length x)))

(define make-vector
  (case-lambda
   ((n)
    (make-vector n (void)))
   ((n fill)
    (with-arguments-validation (make-vector)
	((length n))
      (let loop ((v	(unsafe.make-vector n))
		 (i	0)
		 (n	n)
		 (fill	fill))
	(if (unsafe.fx= i n)
	    v
	  (begin
	   (unsafe.vector-set! v i fill)
	   (loop v (unsafe.fxadd1 i) n fill))))))))

(define vector
  (case-lambda
   (()
    '#())
   ((one)
    (let ((vec (unsafe.make-vector 1)))
      (unsafe.vector-set! vec 0 one)
      vec))
   ((one two)
    (let ((vec (unsafe.make-vector 2)))
      (unsafe.vector-set! vec 0 one)
      (unsafe.vector-set! vec 1 two)
      vec))
   ((one two three)
    (let ((vec (unsafe.make-vector 3)))
      (unsafe.vector-set! vec 0 one)
      (unsafe.vector-set! vec 1 two)
      (unsafe.vector-set! vec 2 three)
      vec))
   ((one two three four)
    (let ((vec (unsafe.make-vector 4)))
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
    (let* ((ls (cons one ls))
	   (n  (length ls 0)))
      (with-arguments-validation (make-vector)
	  ((length n))
	(loop (unsafe.make-vector n) ls 0 n))))))

(define (vector-fill! v fill)
  (with-arguments-validation (vector-fill!)
      ((vector v))
    (let f ((v    v)
	    (i    0)
	    (n    (unsafe.vector-length v))
	    (fill fill))
      (unless (unsafe.fx= i n)
	(unsafe.vector-set! v i fill)
	(f v (unsafe.fxadd1 i) n fill)))))


(define (vector-ref v i)
  (with-arguments-validation (vector-ref)
      ((vector		v)
       (index		i)
       (index-for	i v))
    (unsafe.vector-ref v i)))

(define (vector-set! v i c)
  (with-arguments-validation (vector-set!)
      ((vector		v)
       (index		i)
       (index-for	i v))
    (unsafe.vector-set! v i c)))


(define (vector->list v)
  (define (f v i ls)
    (if (unsafe.fx< i 0)
	ls
      (f v (unsafe.fxsub1 i) (cons (unsafe.vector-ref v i) ls))))
  (with-arguments-validation (vector->list)
      ((vector v))
    (let ((n (unsafe.vector-length v)))
      (if (unsafe.fxzero? n)
	  '()
	(f v (unsafe.fxsub1 n) '())))))

(define (list->vector ls)
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
      (fill (unsafe.make-vector n) 0 ls))))


(module (vector-map)
  (define who 'vector-map)

  (define (ls->vec ls n)
    (let f ((v  (unsafe.make-vector n))
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


;;;; done

)


(library (ikarus system vectors)
  (export $vector-ref $vector-length)
  (import (ikarus))
  (define $vector-ref vector-ref)
  (define $vector-length vector-length))

;;; end of file
