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

(library (ikarus lists)
  (export list? list cons* make-list append length list-ref reverse
          last-pair memq memp memv member find assq assp assv assoc
          remq remv remove remp filter map for-each andmap ormap list-tail
          partition for-all exists fold-left fold-right)
  (import (except (ikarus)
		  list? list cons* make-list append reverse
		  last-pair length list-ref memq memp memv member find
		  assq assp assv assoc remq remv remove remp filter
		  map for-each andmap ormap list-tail partition
		  for-all exists fold-left fold-right)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (pair who obj)
  (pair? obj)
  (assertion-violation who "argument does not have required pair structure" obj))

(define-argument-validation (list who obj)
  (list? obj)
  (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT obj))

(define-argument-validation (length who obj)
  (and (fixnum? obj) (unsafe.fx>= obj 0))
  (assertion-violation who "expected non-negative fixnum list length argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx>= obj 0))
  (assertion-violation who "expected non-negative fixnum list index argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))


;;;; constants

(define EXPECTED_PROPER_LIST_AS_ARGUMENT
  "expected proper list as argument")

(define CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT
  "circular list is invalid as argument")

(define IMPROPER_LIST_IS_INVALID_AS_ARGUMENT
  "improper list is invalid as argument")

(define INDEX_IS_OUT_OF_RANGE
  "index is out of range")

(define MALFORMED_ALIST_AS_ARGUMENT
  "malformed alist as argument")

(define LIST_WAS_ALTERED_WHILE_PROCESSING
  "list was altered while processing")

(define LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS
  "length mismatch among list arguments")


;;Commented out because  it appears to be useless:  $MEMQ is a primitive
;;operation (Marco Maggi; Oct 28, 2011).
;;
;; (define ($memq x ls)
;;   (and (pair? ls)
;;        (if (eq? x (unsafe.car ls))
;; 	   ls
;; 	 ($memq x (unsafe.cdr ls)))))

(define list (lambda x x))

(define (cons* fst . rest)
  (let loop ((fst fst) (rest rest))
    (if (null? rest)
	fst
      (cons fst (loop (unsafe.car rest) (unsafe.cdr rest))))))

(define (list? x)
  (define (%race h t)
    (if (pair? h)
	(let ((h (unsafe.cdr h)))
	  (if (pair? h)
	      (and (not (eq? h t))
		   (%race (unsafe.cdr h) (unsafe.cdr t)))
	    (null? h)))
      (null? h)))
  (%race x x))

(define make-list
  (case-lambda
   ((n)
    (define who 'make-list)
    (with-arguments-validation (who)
	((length n))
      (%unsafe.make-list n (void) '())))
   ((n fill)
    (define who 'make-list)
    (with-arguments-validation (who)
	((length n))
      (%unsafe.make-list n fill '())))))

(define (%unsafe.make-list n fill ls)
  (if (unsafe.fxzero? n)
      ls
    (%unsafe.make-list (unsafe.fxsub1 n) fill (cons fill ls))))

(define (length ls)
  (define who 'length)
  (define (%race h t ls n)
    (cond ((pair? h)
	   (let ((h (unsafe.cdr h)))
	     (if (pair? h)
		 (if (not (eq? h t))
		     (%race (unsafe.cdr h) (unsafe.cdr t) ls (unsafe.fx+ n 2))
		   (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls))
	       (if (null? h)
		   (unsafe.fx+ n 1)
		 (assertion-violation who IMPROPER_LIST_IS_INVALID_AS_ARGUMENT ls)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race ls ls ls 0))


(define (list-ref the-list the-index)
  (define who 'list-ref)
  (define (%error-index-out-of-range)
    (assertion-violation who INDEX_IS_OUT_OF_RANGE the-index the-list))
  (define-argument-validation (in-range who obj)
    (pair? obj)
    (%error-index-out-of-range))
  (define (%unsafe.list-ref ls i)
    (cond ((unsafe.fxzero? i)
	   (with-arguments-validation (who)
	       ((in-range ls))
	     (unsafe.car ls)))
	  ((pair? ls)
	   (%unsafe.list-ref (unsafe.cdr ls) (unsafe.fxsub1 i)))
	  ((null? ls)
	   (%error-index-out-of-range))
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT the-list))))
  (with-arguments-validation (who)
      ((index the-index))
    (%unsafe.list-ref the-list the-index)))


(define (list-tail list index)
  (define who 'list-tail)
  (define (%unsafe.list-tail ls i)
    (cond ((unsafe.fxzero? i)
	   ls)
	  ((pair? ls)
	   (%unsafe.list-tail (unsafe.cdr ls) (unsafe.fxsub1 i)))
	  ((null? ls)
	   (assertion-violation who INDEX_IS_OUT_OF_RANGE index list))
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT list))))
  (with-arguments-validation (who)
      ((index index))
    (%unsafe.list-tail list index)))


(define append
  (case-lambda
   (()		'())
   ((ls)	ls)
   ((ls . ls*)
    (define who 'append)
    (define (reverse h t ls ac)
      (cond ((pair? h)
	     (let ((h (unsafe.cdr h)) (a1 (unsafe.car h)))
	       (cond ((pair? h)
		      (if (not (eq? h t))
			  (let ((a2 (unsafe.car h)))
			    (reverse (unsafe.cdr h) (unsafe.cdr t) ls (cons a2 (cons a1 ac))))
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
		     ((null? h)
		      (cons a1 ac))
		     (else
		      (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))
	    ((null? h)
	     ac)
	    (else
	     (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))

    (define (rev! ls ac)
      (if (null? ls)
	  ac
	(let ((ls^ (unsafe.cdr ls)))
	  (unsafe.set-cdr! ls ac)
	  (rev! ls^ ls))))

    (define (append1 ls ls*)
      (if (null? ls*)
	  ls
	(rev! (reverse ls ls ls '())
	      (append1 (unsafe.car ls*) (unsafe.cdr ls*)))))

    (append1 ls ls*))))


(define (reverse x)
  (define who 'reverse)
  (define (%race h t ls ac)
    (cond ((pair? h)
	   (let ((h  (unsafe.cdr h))
		 (ac (cons (unsafe.car h) ac)))
	     (cond ((pair? h)
		    (if (not (eq? h t))
			(%race (unsafe.cdr h) (unsafe.cdr t) ls (cons (unsafe.car h) ac))
		      (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
		   ((null? h)
		    ac)
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))
	  ((null? h)
	   ac)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race x x x '()))


(define (last-pair x)
  (define who 'last-pair)
  (define (%race h t ls last)
    (if (pair? h)
	(let ((h (unsafe.cdr h)) (last h))
	  (if (pair? h)
	      (if (not (eq? h t))
		  (%race (unsafe.cdr h) (unsafe.cdr t) ls h)
		(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls))
	    last))
      last))
  (with-arguments-validation (who)
      ((pair x))
    (let ((d (unsafe.cdr x)))
      (%race d d x x))))


(define (memq x ls)
  (define who 'memq)
  (define (%race h t ls x)
    (cond ((pair? h)
	   (if (eq? (unsafe.car h) x)
	       h
	     (let ((h (unsafe.cdr h)))
	       (cond ((pair? h)
		      (cond ((eq? (unsafe.car h) x)
			     h)
			    ((not (eq? h t))
			     (%race (unsafe.cdr h) (unsafe.cdr t) ls x))
			    (else
			     (assertion-violation who
			       CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls))))
		     ((null? h)
		      #f)
		     (else
		      (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race ls ls ls x))


(define (memv x ls)
  (define who 'memv)
  (define (%race h t ls x)
    (cond ((pair? h)
	   (if (eqv? (unsafe.car h) x)
	       h
	     (let ((h (unsafe.cdr h)))
	       (cond ((pair? h)
		      (cond ((eqv? (unsafe.car h) x)
			     h)
			    ((not (eq? h t))
			     (%race (unsafe.cdr h) (unsafe.cdr t) ls x))
			    (else
			     (assertion-violation who
			       CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls))))
		     ((null? h)
		      #f)
		     (else
		      (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race ls ls ls x))


(define (member x ls)
  (define who 'member)
  (define (%race h t ls x)
    (cond ((pair? h)
	   (if (equal? (unsafe.car h) x)
	       h
	     (let ((h (unsafe.cdr h)))
	       (cond ((pair? h)
		      (cond ((equal? (unsafe.car h) x)
			     h)
			    ((not (eq? h t))
			     (%race (unsafe.cdr h) (unsafe.cdr t) ls x))
			    (else
			     (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls))))
		     ((null? h)
		      #f)
		     (else
		      (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race ls ls ls x))


(define (memp p ls)
  (define who 'memp)
  (define (%race h t ls p)
    (cond ((pair? h)
	   (if (p (unsafe.car h))
	       h
	     (let ((h (unsafe.cdr h)))
	       (cond ((pair? h)
		      (cond ((p (unsafe.car h))
			     h)
			    ((not (eq? h t))
			     (%race (unsafe.cdr h) (unsafe.cdr t) ls p))
			    (else
			     (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls))))
		     ((null? h)
		      #f)
		     (else
		      (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (with-arguments-validation (who)
      ((procedure p))
    (%race ls ls ls p)))


(define (find p ls)
  (define who 'find)
  (define (%race h t ls p)
    (cond ((pair? h)
	   (let ((a (unsafe.car h)))
	     (if (p a)
		 a
	       (let ((h (unsafe.cdr h)))
		 (cond ((pair? h)
			(let ((a (unsafe.car h)))
			  (cond ((p a)
				 a)
				((not (eq? h t))
				 (%race (unsafe.cdr h) (unsafe.cdr t) ls p))
				(else
				 (assertion-violation who
				   CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))))
		       ((null? h)
			#f)
		       (else
			(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (with-arguments-validation (who)
      ((procedure p))
    (%race ls ls ls p)))


(define (assq x ls)
  (define who 'assq)
  (define (%race x h t ls)
    (cond ((pair? h)
	   (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
	     (if (pair? a)
		 (cond ((eq? (unsafe.car a) x)
			a)
		       ((pair? h)
			(if (not (eq? h t))
			    (let ((a (unsafe.car h)))
			      (if (pair? a)
				  (if (eq? (unsafe.car a) x)
				      a
				    (%race x (unsafe.cdr h) (unsafe.cdr t) ls))
				(assertion-violation who
				  MALFORMED_ALIST_AS_ARGUMENT ls)))
			  (assertion-violation who
			    CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
		       ((null? h)
			#f)
		       (else
			(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))
	       (assertion-violation who MALFORMED_ALIST_AS_ARGUMENT ls))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race x ls ls ls))


(define (assp p ls)
  (define who 'assp)
  (define (%race p h t ls)
    (cond ((pair? h)
	   (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
	     (if (pair? a)
		 (cond ((p (unsafe.car a))
			a)
		       ((pair? h)
			(if (not (eq? h t))
			    (let ((a (unsafe.car h)))
			      (if (pair? a)
				  (if (p (unsafe.car a))
				      a
				    (%race p (unsafe.cdr h) (unsafe.cdr t) ls))
				(assertion-violation who
				  MALFORMED_ALIST_AS_ARGUMENT ls)))
			  (assertion-violation who
			    CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
		       ((null? h)
			#f)
		       (else
			(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))
	       (assertion-violation who MALFORMED_ALIST_AS_ARGUMENT ls))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (with-arguments-validation (who)
      ((procedure p))
    (%race p ls ls ls)))


(define (assv x ls)
  (define who 'assv)
  (define (%race x h t ls)
    (cond ((pair? h)
	   (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
	     (if (pair? a)
		 (cond ((eqv? (unsafe.car a) x)
			a)
		       ((pair? h)
			(if (not (eq? h t))
			    (let ((a (unsafe.car h)))
			      (if (pair? a)
				  (if (eqv? (unsafe.car a) x)
				      a
				    (%race x (unsafe.cdr h) (unsafe.cdr t) ls))
				(assertion-violation who MALFORMED_ALIST_AS_ARGUMENT ls)))
			  (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
		       ((null? h)
			#f)
		       (else
			(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))
	       (assertion-violation who MALFORMED_ALIST_AS_ARGUMENT ls))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race x ls ls ls))


(define (assoc x ls)
  (define who 'assoc)
  (define (%race x h t ls)
    (cond ((pair? h)
	   (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
	     (if (pair? a)
		 (cond ((equal? (unsafe.car a) x)
			a)
		       ((pair? h)
			(if (not (eq? h t))
			    (let ((a (unsafe.car h)))
			      (if (pair? a)
				  (if (equal? (unsafe.car a) x)
				      a
				    (%race x (unsafe.cdr h) (unsafe.cdr t) ls))
				(assertion-violation who MALFORMED_ALIST_AS_ARGUMENT ls)))
			  (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
		       ((null? h)
			#f)
		       (else
			(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))
	       (assertion-violation who MALFORMED_ALIST_AS_ARGUMENT ls))))
	  ((null? h)
	   #f)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (%race x ls ls ls))


(define-syntax define-remover
  (syntax-rules ()
    ((_ ?name ?cmp ?check)
     (define (?name x ls)
       (define who '?name)
       (define (%race h t ls x)
	 (cond ((pair? h)
		(if (?cmp (unsafe.car h) x)
		    (let ((h (unsafe.cdr h)))
		      (cond ((pair? h)
			     (if (not (eq? h t))
				 (if (?cmp (unsafe.car h) x)
				     (%race (unsafe.cdr h) (unsafe.cdr t) ls x)
				   (cons (unsafe.car h) (%race (unsafe.cdr h) (unsafe.cdr t) ls x)))
			       (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
			    ((null? h)
			     '())
			    (else
			     (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
		  (let ((a0 (unsafe.car h)) (h (unsafe.cdr h)))
		    (cond ((pair? h)
			   (if (not (eq? h t))
			       (if (?cmp (unsafe.car h) x)
				   (cons a0 (%race (unsafe.cdr h) (unsafe.cdr t) ls x))
				 (cons* a0 (unsafe.car h) (%race (unsafe.cdr h) (unsafe.cdr t) ls x)))
			     (assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)))
			  ((null? h)
			   (list a0))
			  (else
			   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))))
	       ((null? h)
		'())
	       (else
		(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
	 (with-arguments-validation (who)
	     ((?check x))
	   (%race ls ls ls x))))))

(define-remover remq eq? void)

(define-remover remv eqv? void)

(define-remover remove equal? void)

(define-remover remp (lambda (elt p) (p elt)) procedure)

(define-remover filter (lambda (elt p) (not (p elt))) procedure)


(module (map)
  (define who 'map)

  (define (len h t n)
    (cond ((pair? h)
	   (let ((h (unsafe.cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT)
		      (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2))))
		   ((null? h)
		    (unsafe.fxadd1 n))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT))))

  (define (map1 f a d n)
    (cond ((pair? d)
	   (if (unsafe.fxzero? n)
	       (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
	     (cons (f a) (map1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
	  ((null? d)
	   (if (unsafe.fxzero? n)
	       (cons (f a) '())
	     (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)))
	  (else
	   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define (map2 f a1 a2 d1 d2 n)
    (cond
     ((pair? d1)
      (cond ((pair? d2)
	     (if (unsafe.fxzero? n)
		 (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
	       (cons (f a1 a2)
		     (map2 f
			   (unsafe.car d1) (unsafe.car d2)
			   (unsafe.cdr d1) (unsafe.cdr d2)
			   (unsafe.fxsub1 n)))))
	    ((null? d2)
	     (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
	    (else
	     (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT))))
     ((null? d1)
      (cond ((null? d2)
	     (if (unsafe.fxzero? n)
		 (cons (f a1 a2) '())
	       (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)))
	    (else
	     (assertion-violation who
	       (if (list? d2)
		   LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS
		 EXPECTED_PROPER_LIST_AS_ARGUMENT)))))
     (else
      (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define (cars ls*)
    (if (null? ls*)
	'()
      (let ((a (car ls*)))
	(if (pair? a)
	    (cons (car a) (cars (cdr ls*)))
	  (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))))

  (define (cdrs ls*)
    (if (null? ls*)
	'()
      (let ((a (car ls*)))
	(if (pair? a)
	    (cons (cdr a) (cdrs (cdr ls*)))
	  (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))))

  (define (err-mutated all-lists)
    (apply assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING all-lists))

  (define (err-mismatch all-lists)
    (apply assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS all-lists))

  (define (err-invalid all-lists)
    (apply assertion-violation who "invalid arguments" all-lists))

  (define (mapm f ls ls* n all-lists)
    (cond ((null? ls)
	   (if (andmap null? ls*)
	       (if (fxzero? n)
		   '()
		 (err-mutated all-lists))
	     (err-mismatch all-lists)))
	  ((fxzero? n)
	   (err-mutated all-lists))
	  (else
	   (cons (apply f (car ls) (cars ls*))
		 (mapm f (cdr ls) (cdrs ls*) (fxsub1 n) all-lists)))))

  (define map
    (case-lambda
     ((f ls)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (let ((d (unsafe.cdr ls)))
		 (map1 f (unsafe.car ls) d (len d d 0))))
	      ((null? ls)
	       '())
	      (else
	       (err-invalid (list ls))))))

     ((f ls ls2)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (if (pair? ls2)
		   (let ((d (unsafe.cdr ls)))
		     (map2 f (unsafe.car ls) (unsafe.car ls2) d (unsafe.cdr ls2) (len d d 0)))
		 (err-invalid (list ls ls2))))
	      ((and (null? ls) (null? ls2))
	       '())
	      (else
	       (err-invalid (list ls ls2))))))

     ((f ls . ls*)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (let ((n (len ls ls 0)))
		 (mapm f ls ls* n (cons ls ls*))))
	      ((and (null? ls) (andmap null? ls*))
	       '())
	      (else
	       (err-invalid (cons ls ls*))))))))

  #| end of module |# )


(module (for-each)
  (define who 'for-each)

  (define (len h t n)
    (cond ((pair? h)
	   (let ((h (unsafe.cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT)
		      (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2))))
		   ((null? h)
		    (unsafe.fxadd1 n))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT))))

  (define (for-each1 f a d n)
    (cond ((pair? d)
	   (if (unsafe.fxzero? n)
	       (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
	     (begin
	       (f a)
	       (for-each1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
	  ((null? d)
	   (if (unsafe.fxzero? n)
	       (f a)
	     (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)))
	  (else
	   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define (for-each2 f a1 a2 d1 d2 n)
    (cond ((pair? d1)
	   (if (pair? d2)
	       (if (unsafe.fxzero? n)
		   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
		 (begin
		   (f a1 a2)
		   (for-each2 f
			      (unsafe.car d1) (unsafe.car d2)
			      (unsafe.cdr d1) (unsafe.cdr d2)
			      (unsafe.fxsub1 n))))
	     (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	  ((null? d1)
	   (if (null? d2)
	       (if (unsafe.fxzero? n)
		   (f a1 a2)
		 (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))
	     (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	  (else
	   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define for-each
    (case-lambda
     ((f ls)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (let ((d (unsafe.cdr ls)))
		 (for-each1 f (unsafe.car ls) d (len d d 0))))
	      ((null? ls)
	       (void))
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))

     ((f ls ls2)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (if (pair? ls2)
		   (let ((d (unsafe.cdr ls)))
		     (for-each2 f (unsafe.car ls) (unsafe.car ls2) d (unsafe.cdr ls2) (len d d 0)))
		 (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	      ((null? ls)
	       (if (null? ls2)
		   (void)
		 (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))

     ((f ls . ls*)
      (with-arguments-validation (who)
	  ((procedure	f)
	   (list	ls))
	(let ((n (length ls)))
	  (for-each (lambda (x)
		      (unless (and (list? x) (= (length x) n))
			(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT x)))
	    ls*)
	  (let loop ((n (length ls)) (ls ls) (ls* ls*))
	    (if (unsafe.fx= n 0)
		(unless (and (null? ls) (andmap null? ls*))
		  (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING f))
	      (begin
	       (unless (and (pair? ls) (andmap pair? ls*))
		 (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING f))
	       (apply f (car ls) (map car ls*))
	       (loop (fx- n 1) (cdr ls) (map cdr ls*))))))))
     ))

  #| end of module |#)


(module (andmap)
  ;;ANDMAP should be the same as R6RS's FOR-ALL (Marco Maggi; Oct 28, 2011).
  ;;
  (define who 'andmap)

  (define (len h t n)
    (cond ((pair? h)
	   (let ((h (unsafe.cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT)
		      (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2))))
		   ((null? h)
		    (unsafe.fxadd1 n))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT))))

  (define (andmap1 f a d n)
    (cond ((pair? d)
	   (if (unsafe.fxzero? n)
	       (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
	     (and (f a)
		  (andmap1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
	  ((null? d)
	   (if (unsafe.fxzero? n)
	       (f a)
	     (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)))
	  (else
	   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define (andmap2 f a1 a2 d1 d2 n)
    (cond ((pair? d1)
	   (if (pair? d2)
	       (if (unsafe.fxzero? n)
		   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
		 (and (f a1 a2)
		      (andmap2 f
			       (unsafe.car d1) (unsafe.car d2)
			       (unsafe.cdr d1) (unsafe.cdr d2)
			       (unsafe.fxsub1 n))))
	     (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	  ((null? d1)
	   (if (null? d2)
	       (if (unsafe.fxzero? n)
		   (f a1 a2)
		 (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))
	     (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	  (else
	   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define andmap
    (case-lambda
     ((f ls)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (let ((d (unsafe.cdr ls)))
		 (andmap1 f (unsafe.car ls) d (len d d 0))))
	      ((null? ls)
	       #t)
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))

     ((f ls ls2)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (if (pair? ls2)
		   (let ((d (unsafe.cdr ls)))
		     (andmap2 f
			      (unsafe.car ls) (unsafe.car ls2) d (unsafe.cdr ls2) (len d d 0)))
		 (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	      ((null? ls)
	       (if (null? ls2)
		   #t
		 (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS)))
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))

     ((f ls . ls*)
      (with-arguments-validation (who)
	  ((procedure f))
	(assertion-violation who "vararg not yet supported")))
     ))

  #| end of module |# )


(module (ormap)
  ;;ANDMAP should be the same as R6RS's EXISTS (Marco Maggi; Oct 28, 2011).
  ;;
  (define who 'ormap)

  (define (len h t n)
    (cond ((pair? h)
	   (let ((h (unsafe.cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT)
		      (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2))))
		   ((null? h)
		    (unsafe.fxadd1 n))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT))))

  (define (ormap1 f a d n)
    (cond ((pair? d)
	   (if (unsafe.fxzero? n)
	       (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)
	     (or (f a)
		 (ormap1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
	  ((null? d)
	   (if (unsafe.fxzero? n)
	       (f a)
	     (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING)))
	  (else
	   (assertion-violation who LIST_WAS_ALTERED_WHILE_PROCESSING))))

  (define ormap
    (case-lambda
     ((f ls)
      (with-arguments-validation (who)
	  ((procedure f))
	(cond ((pair? ls)
	       (let ((d (unsafe.cdr ls)))
		 (ormap1 f (unsafe.car ls) d (len d d 0))))
	      ((null? ls)
	       #f)
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT)))))
     (_
      (assertion-violation who "vararg not supported yet"))
     ))

  #| end of module |#)


(define (partition p ls)
  (define who 'partition)
  (define (%race h t ls p)
    (cond ((pair? h)
	   (let ((a0 (unsafe.car h))
		 (h  (unsafe.cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)
		      (let ((a1 (unsafe.car h)))
			(let-values (((a* b*) (%race (unsafe.cdr h) (unsafe.cdr t) ls p)))
			  (cond ((p a0)
				 (if (p a1)
				     (values (cons* a0 a1 a*) b*)
				   (values (cons a0 a*) (cons a1 b*))))
				((p a1)
				 (values (cons a1 a*) (cons a0 b*)))
				(else
				 (values a* (cons* a0 a1 b*))))))))
		   ((null? h)
		    (if (p a0)
			(values (list a0) '())
		      (values '() (list a0))))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))
	  ((null? h)
	   (values '() '()))
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))
  (with-arguments-validation (who)
      ((procedure p))
    (%race ls ls ls p)))


(define-syntax define-iterator
  (syntax-rules ()
    ((_ ?name ?combine)
     (module (?name)
       (define who '?name)
       (define (null*? ls)
	 (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))

       (define (err* ls*)
	 (cond ((null? ls*)
		(assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
	       ((list? (car ls*))
		(err* (cdr ls*)))
	       (else
		(assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT (car ls*)))))

       (define (cars+cdrs ls ls*)
	 (if (null? ls)
	     (values '() '())
	   (let ((a (car ls)))
	     (cond ((pair? a)
		    (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
		      (values (cons (car a) cars) (cons (cdr a) cdrs))))
		   ((list? (car ls*))
		    (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT (car ls*)))))))

       (define (loop1 f a h t ls)
	 (cond ((pair? h)
		(let ((b (car h)) (h (cdr h)))
		  (?combine (f a)
			    (cond ((pair? h)
				   (if (eq? h t)
				       (assertion-violation who "circular" ls)
				     (let ((c (car h)) (h (cdr h)))
				       (?combine (f b) (loop1 f c h (cdr t) ls)))))
				  ((null? h)
				   (f b))
				  (else
				   (?combine (f b)
					     (assertion-violation who
					       EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))))
	       ((null? h)
		(f a))
	       (else
		(?combine (f a) (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))

       (define (loopn f a a* h h* t ls ls*)
	 (cond ((pair? h)
		(let-values (((b* h*) (cars+cdrs h* ls*)))
		  (let ((b (car h)) (h (cdr h)))
		    (?combine (apply f a a*)
			      (if (pair? h)
				  (if (eq? h t)
				      (assertion-violation who "circular" ls)
				    (let-values (((c* h*) (cars+cdrs h* ls*)))
				      (let ((c (car h)) (h (cdr h)))
					(?combine (apply f b b*)
						  (loopn f c c* h h* (cdr t) ls ls*)))))
				(if (and (null? h) (null*? h*))
				    (apply f b b*)
				  (?combine (apply f b b*) (err* (cons ls ls*)))))))))
	       ((and (null? h) (null*? h*))
		(apply f a a*))
	       (else
		(?combine (apply f a a*) (err* (cons ls ls*))))))

       (define ?name
	 (case-lambda
	  ((f ls)
	   (with-arguments-validation (who)
	       ((procedure f))
	     (cond ((pair? ls)
		    (loop1 f (car ls) (cdr ls) (cdr ls) ls))
		   ((null? ls)
		    (?combine))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))

	  ((f ls . ls*)
	   (with-arguments-validation (who)
	       ((procedure f))
	     (cond ((pair? ls)
		    (let-values (((cars cdrs) (cars+cdrs ls* ls*)))
		      (loopn f (car ls) cars (cdr ls) cdrs (cdr ls) ls ls*)))
		   ((and (null? ls) (null*? ls*))
		    (?combine))
		   (else
		    (err* ls*)))))))

       #| end of module |#)
     )))

(define-iterator for-all and)
(define-iterator exists  or)


(module (fold-left)
  (define who 'fold-left)

  (define (null*? ls)
    (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))

  (define (err* ls*)
    (cond ((null? ls*)
	   (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
	  ((list? (car ls*))
	   (err* (cdr ls*)))
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT (car ls*)))))

  (define (cars+cdrs ls ls*)
    (if (null? ls)
	(values '() '())
      (let ((a (car ls)))
	(cond ((pair? a)
	       (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
		 (values (cons (car a) cars) (cons (cdr a) cdrs))))
	      ((list? (car ls*))
	       (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT (car ls*)))))))

  (define (loop1 f nil h t ls)
    (cond ((pair? h)
	   (let ((a (car h)) (h (cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)
		      (let ((b (car h)) (h (cdr h)) (t (cdr t)))
			(loop1 f (f (f nil a) b) h t ls))))
		   ((null? h)
		    (f nil a))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))
	  ((null? h)
	   nil)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))

  (define (loopn f nil h h* t ls ls*)
    (cond ((pair? h)
	   (let-values (((a* h*) (cars+cdrs h* ls*)))
	     (let ((a (car h)) (h (cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (assertion-violation who
			    CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)
			(let-values (((b* h*) (cars+cdrs h* ls*)))
			  (let ((b (car h)) (h (cdr h)) (t (cdr t)))
			    (loopn f
				   (apply f (apply f nil a a*) b b*)
				   h h* t ls ls*)))))
		     ((and (null?  h)
			   (null*? h*))
		      (apply f nil a a*))
		     (else
		      (err* (cons ls ls*)))))))
	  ((and (null? h) (null*? h*))
	   nil)
	  (else
	   (err* (cons ls ls*)))))

  (define fold-left
    (case-lambda
     ((f nil ls)
      (with-arguments-validation (who)
	  ((procedure f))
	(loop1 f nil ls ls ls)))
     ((f nil ls . ls*)
      (with-arguments-validation (who)
	  ((procedure f))
	(loopn f nil ls ls* ls ls ls*)))))

  #| end of module |# )


(module (fold-right)
  (define who 'fold-right)

  (define (null*? ls)
    (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))

  (define (err* ls*)
    (cond ((null? ls*)
	   (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
	  ((list? (car ls*))
	   (err* (cdr ls*)))
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT (car ls*)))))

  (define (cars+cdrs ls ls*)
    (if (null? ls)
	(values '() '())
      (let ((a (car ls)))
	(cond ((pair? a)
	       (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
		 (values (cons (car a) cars) (cons (cdr a) cdrs))))
	      ((list? (car ls*))
	       (assertion-violation who LENGTH_MISMATCH_AMONG_LIST_ARGUMENTS))
	      (else
	       (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT (car ls*)))))))

  (define (loop1 f nil h t ls)
    (cond ((pair? h)
	   (let ((a (car h)) (h (cdr h)))
	     (cond ((pair? h)
		    (if (eq? h t)
			(assertion-violation who CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)
		      (let ((b (car h)) (h (cdr h)) (t (cdr t)))
			(f a (f b (loop1 f nil h t ls))))))
		   ((null? h)
		    (f a nil))
		   (else
		    (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls)))))
	  ((null? h)
	   nil)
	  (else
	   (assertion-violation who EXPECTED_PROPER_LIST_AS_ARGUMENT ls))))

  (define (loopn f nil h h* t ls ls*)
    (cond ((pair? h)
	   (let-values (((a* h*) (cars+cdrs h* ls*)))
	     (let ((a (car h)) (h (cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (assertion-violation who
			    CIRCULAR_LIST_IS_INVALID_AS_ARGUMENT ls)
			(let-values (((b* h*) (cars+cdrs h* ls*)))
			  (let ((b (car h))
				(h (cdr h))
				(t (cdr t)))
			    (apply f a
				   (append
				    a* (list
					(apply f
					       b (append
						  b* (list (loopn f nil h h* t ls ls*)))))))))))
		     ((and (null?  h)
			   (null*? h*))
		      (apply f a (append a* (list nil))))
		     (else
		      (err* (cons ls ls*)))))))
	  ((and (null? h) (null*? h*))
	   nil)
	  (else
	   (err* (cons ls ls*)))))

  (define fold-right
    (case-lambda
     ((f nil ls)
      (with-arguments-validation (who)
	  ((procedure f))
	(loop1 f nil ls ls ls)))
     ((f nil ls . ls*)
      (with-arguments-validation (who)
	  ((procedure f))
	(loopn f nil ls ls* ls ls ls*)))
     ))

  #| end of module |#)


;;;; done

)

;;; end of file
