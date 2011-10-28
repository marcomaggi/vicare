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
  (export $memq list? list cons* make-list append length list-ref reverse
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

(define-argument-validation (length who obj)
  (and (fixnum? obj) (unsafe.fx>= obj 0))
  (assertion-violation who "expected non-negative fixnum list length argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx>= obj 0))
  (assertion-violation who "expected non-negative fixnum list index argument" obj))


(define ($memq x ls)
  (and (pair? ls)
       (if (eq? x (unsafe.car ls))
	   ls
	 ($memq x (unsafe.cdr ls)))))

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
    (with-arguments-validation (make-list)
	((length n))
      (%unsafe.make-list n (void) '())))
   ((n fill)
    (with-arguments-validation (make-list)
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
		   (assertion-violation who "circular list is invalid as argument" ls))
	       (if (null? h)
		   (unsafe.fx+ n 1)
		 (assertion-violation who "improper list is invalid as argument" ls)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who "not a proper list" ls))))
  (%race ls ls ls 0))


(define (list-ref the-list the-index)
  (define who 'list-ref)
  (define (%error-index-out-of-range)
    (assertion-violation who "index is out of range" the-index the-list))
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
	   (assertion-violation who "expected proper list as argument" the-list))))
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
	   (assertion-violation who "index is out of range" index list))
	  (else
	   (assertion-violation who "expected proper list as argument" list))))
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
			(assertion-violation who "circular list is invalid as argument" ls)))
		     ((null? h)
		      (cons a1 ac))
		     (else
		      (assertion-violation who "expected proper list as argument" ls)))))
	    ((null? h)
	     ac)
	    (else
	     (assertion-violation who "expected proper list as argument" ls))))

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
		      (assertion-violation who "circular list is invalid as argument" ls)))
		   ((null? h)
		    ac)
		   (else
		    (assertion-violation who "expected proper list as argument" ls)))))
	  ((null? h)
	   ac)
	  (else
	   (assertion-violation who "expected proper list as argument" ls))))
  (%race x x x '()))


(define last-pair
  (letrec ((race
	    (lambda (h t ls last)
	      (if (pair? h)
		  (let ((h (unsafe.cdr h)) (last h))
		    (if (pair? h)
			(if (not (eq? h t))
			    (race (unsafe.cdr h) (unsafe.cdr t) ls h)
			  (die 'last-pair "circular list" ls))
		      last))
		last))))
    (lambda (x)
      (if (pair? x)
	  (let ((d (cdr x)))
	    (race d d x x))
	(die 'last-pair "not a pair" x)))))


(define memq
  (letrec ((race
	    (lambda (h t ls x)
	      (if (pair? h)
		  (if (eq? (unsafe.car h) x)
		      h
		    (let ((h (unsafe.cdr h)))
		      (if (pair? h)
			  (if (eq? (unsafe.car h) x)
			      h
			    (if (not (eq? h t))
				(race (unsafe.cdr h) (unsafe.cdr t) ls x)
			      (die 'memq "circular list" ls)))
			(if (null? h)
			    '#f
			  (die 'memq "not a proper list" ls)))))
		(if (null? h)
		    '#f
		  (die 'memq "not a proper list" ls))))))
    (lambda (x ls)
      (race ls ls ls x))))

(define memv
  (letrec ((race
	    (lambda (h t ls x)
	      (if (pair? h)
		  (if (eqv? (unsafe.car h) x)
		      h
		    (let ((h (unsafe.cdr h)))
		      (if (pair? h)
			  (if (eqv? (unsafe.car h) x)
			      h
			    (if (not (eq? h t))
				(race (unsafe.cdr h) (unsafe.cdr t) ls x)
			      (die 'memv "circular list" ls)))
			(if (null? h)
			    '#f
			  (die 'memv "not a proper list" ls)))))
		(if (null? h)
		    '#f
		  (die 'memv "not a proper list" ls))))))
    (lambda (x ls)
      (race ls ls ls x))))

(define member
  (letrec ((race
	    (lambda (h t ls x)
	      (if (pair? h)
		  (if (equal? (unsafe.car h) x)
		      h
		    (let ((h (unsafe.cdr h)))
		      (if (pair? h)
			  (if (equal? (unsafe.car h) x)
			      h
			    (if (not (eq? h t))
				(race (unsafe.cdr h) (unsafe.cdr t) ls x)
			      (die 'member "circular list" ls)))
			(if (null? h)
			    '#f
			  (die 'member "not a proper list" ls)))))
		(if (null? h)
		    '#f
		  (die 'member "not a proper list" ls))))))
    (lambda (x ls)
      (race ls ls ls x))))


(define memp
  (letrec ((race
	    (lambda (h t ls p)
	      (if (pair? h)
		  (if (p (unsafe.car h))
		      h
		    (let ((h (unsafe.cdr h)))
		      (if (pair? h)
			  (if (p (unsafe.car h))
			      h
			    (if (not (eq? h t))
				(race (unsafe.cdr h) (unsafe.cdr t) ls p)
			      (die 'memp "circular list" ls)))
			(if (null? h)
			    '#f
			  (die 'memp "not a proper list" ls)))))
		(if (null? h)
		    '#f
		  (die 'memp "not a proper list" ls))))))
    (lambda (p ls)
      (unless (procedure? p)
	(die 'memp "not a procedure" p))
      (race ls ls ls p))))

(define find
  (letrec ((race
	    (lambda (h t ls p)
	      (if (pair? h)
		  (let ((a (unsafe.car h)))
		    (if (p a)
			a
		      (let ((h (unsafe.cdr h)))
			(if (pair? h)
			    (let ((a (unsafe.car h)))
			      (if (p a)
				  a
				(if (not (eq? h t))
				    (race (unsafe.cdr h) (unsafe.cdr t) ls p)
				  (die 'find "circular list" ls))))
			  (if (null? h)
			      '#f
			    (die 'find "not a proper list" ls))))))
		(if (null? h)
		    '#f
		  (die 'find "not a proper list" ls))))))
    (lambda (p ls)
      (unless (procedure? p)
	(die 'find "not a procedure" p))
      (race ls ls ls p))))

(define assq
  (letrec ((race
	    (lambda (x h t ls)
	      (if (pair? h)
		  (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
		    (if (pair? a)
			(if (eq? (unsafe.car a) x)
			    a
			  (if (pair? h)
			      (if (not (eq? h t))
				  (let ((a (unsafe.car h)))
				    (if (pair? a)
					(if (eq? (unsafe.car a) x)
					    a
					  (race x (unsafe.cdr h) (unsafe.cdr t) ls))
				      (die 'assq "malformed alist"
					   ls)))
				(die 'assq "circular list" ls))
			    (if (null? h)
				#f
			      (die 'assq "not a proper list" ls))))
		      (die 'assq "malformed alist" ls)))
		(if (null? h)
		    #f
		  (die 'assq "not a proper list" ls))))))
    (lambda (x ls)
      (race x ls ls ls))))


(define assp
  (letrec ((race
	    (lambda (p h t ls)
	      (if (pair? h)
		  (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
		    (if (pair? a)
			(if (p (unsafe.car a))
			    a
			  (if (pair? h)
			      (if (not (eq? h t))
				  (let ((a (unsafe.car h)))
				    (if (pair? a)
					(if (p (unsafe.car a))
					    a
					  (race p (unsafe.cdr h) (unsafe.cdr t) ls))
				      (die 'assp "malformed alist"
					   ls)))
				(die 'assp "circular list" ls))
			    (if (null? h)
				#f
			      (die 'assp "not a proper list" ls))))
		      (die 'assp "malformed alist" ls)))
		(if (null? h)
		    #f
		  (die 'assp "not a proper list" ls))))))
    (lambda (p ls)
      (unless (procedure? p)
	(die 'assp "not a procedure" p))
      (race p ls ls ls))))

(define assv
  (letrec ((race
	    (lambda (x h t ls)
	      (if (pair? h)
		  (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
		    (if (pair? a)
			(if (eqv? (unsafe.car a) x)
			    a
			  (if (pair? h)
			      (if (not (eq? h t))
				  (let ((a (unsafe.car h)))
				    (if (pair? a)
					(if (eqv? (unsafe.car a) x)
					    a
					  (race x (unsafe.cdr h) (unsafe.cdr t) ls))
				      (die 'assv "malformed alist"
					   ls)))
				(die 'assv "circular list" ls))
			    (if (null? h)
				#f
			      (die 'assv "not a proper list" ls))))
		      (die 'assv "malformed alist" ls)))
		(if (null? h)
		    #f
		  (die 'assv "not a proper list" ls))))))
    (lambda (x ls)
      (race x ls ls ls))))

(define assoc
  (letrec ((race
	    (lambda (x h t ls)
	      (if (pair? h)
		  (let ((a (unsafe.car h)) (h (unsafe.cdr h)))
		    (if (pair? a)
			(if (equal? (unsafe.car a) x)
			    a
			  (if (pair? h)
			      (if (not (eq? h t))
				  (let ((a (unsafe.car h)))
				    (if (pair? a)
					(if (equal? (unsafe.car a) x)
					    a
					  (race x (unsafe.cdr h) (unsafe.cdr t) ls))
				      (die 'assoc "malformed alist"
					   ls)))
				(die 'assoc "circular list" ls))
			    (if (null? h)
				#f
			      (die 'assoc "not a proper list" ls))))
		      (die 'assoc "malformed alist" ls)))
		(if (null? h)
		    #f
		  (die 'assoc "not a proper list" ls))))))
    (lambda (x ls)
      (race x ls ls ls))))


(module (remq remv remove remp filter)
  (define-syntax define-remover
    (syntax-rules ()
      ((_ name cmp check)
       (define name
	 (letrec ((race
		   (lambda (h t ls x)
		     (if (pair? h)
			 (if (cmp (unsafe.car h) x)
			     (let ((h (unsafe.cdr h)))
			       (if (pair? h)
				   (if (not (eq? h t))
				       (if (cmp (unsafe.car h) x)
					   (race (unsafe.cdr h) (unsafe.cdr t) ls x)
					 (cons (unsafe.car h) (race (unsafe.cdr h) (unsafe.cdr t) ls x)))
				     (die 'name "circular list" ls))
				 (if (null? h)
				     '()
				   (die 'name "not a proper list" ls))))
			   (let ((a0 (unsafe.car h)) (h (unsafe.cdr h)))
			     (if (pair? h)
				 (if (not (eq? h t))
				     (if (cmp (unsafe.car h) x)
					 (cons a0 (race (unsafe.cdr h) (unsafe.cdr t) ls x))
				       (cons* a0 (unsafe.car h) (race (unsafe.cdr h) (unsafe.cdr t) ls x)))
				   (die 'name "circular list" ls))
			       (if (null? h)
				   (list a0)
				 (die 'name "not a proper list" ls)))))
		       (if (null? h)
			   '()
			 (die 'name "not a proper list" ls))))))
	   (lambda (x ls)
	     (check x ls)
	     (race ls ls ls x)))))))
  (define-remover remq eq? (lambda (x ls) #t))
  (define-remover remv eqv? (lambda (x ls) #t))
  (define-remover remove equal? (lambda (x ls) #t))
  (define-remover remp (lambda (elt p) (p elt))
    (lambda (x ls)
      (unless (procedure? x)
	(die 'remp "not a procedure" x))))
  (define-remover filter (lambda (elt p) (not (p elt)))
    (lambda (x ls)
      (unless (procedure? x)
	(die 'filter "not a procedure" x)))))


(module (map)
  (define who 'map)
  (define len
    (lambda (h t n)
      (if (pair? h)
	  (let ((h (unsafe.cdr h)))
	    (if (pair? h)
		(if (eq? h t)
		    (die who "circular list")
		  (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2)))
	      (if (null? h)
		  (unsafe.fxadd1 n)
		(die who "improper list"))))
	(if (null? h)
	    n
	  (die who "improper list")))))
  (define map1
    (lambda (f a d n)
      (cond
       ((pair? d)
	(if (unsafe.fxzero? n)
	    (die who "list was altered!")
	  (cons (f a) (map1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
       ((null? d)
	(if (unsafe.fxzero? n)
	    (cons (f a) '())
	  (die who "list was altered")))
       (else (die who "list was altered")))))
  (define map2
    (lambda (f a1 a2 d1 d2 n)
      (cond
       ((pair? d1)
	(cond
	 ((pair? d2)
	  (if (unsafe.fxzero? n)
	      (die who "list was altered")
	    (cons (f a1 a2)
		  (map2 f
			(unsafe.car d1) (unsafe.car d2)
			(unsafe.cdr d1) (unsafe.cdr d2)
			(unsafe.fxsub1 n)))))
	 ((null? d2) (die who "length mismatch"))
	 (else (die who "not a proper list"))))
       ((null? d1)
	(cond
	 ((null? d2)
	  (if (unsafe.fxzero? n)
	      (cons (f a1 a2) '())
	    (die who "list was altered")))
	 (else
	  (die who
	       (if (list? d2) "length mismatch" "not a proper list")))))
       (else (die who "list was altered")))))
  (define cars
    (lambda (ls*)
      (cond
       ((null? ls*) '())
       (else
	(let ((a (car ls*)))
	  (cond
	   ((pair? a)
	    (cons (car a) (cars (cdr ls*))))
	   (else
	    (die 'map "length mismatch"))))))))
  (define cdrs
    (lambda (ls*)
      (cond
       ((null? ls*) '())
       (else
	(let ((a (car ls*)))
	  (cond
	   ((pair? a)
	    (cons (cdr a) (cdrs (cdr ls*))))
	   (else
	    (die 'map "length mismatch"))))))))
  (define (err-mutated all-lists)
    (apply die 'map "some lists were mutated during operation" all-lists))
  (define (err-mismatch all-lists)
    (apply die 'map "length mismatch" all-lists))
  (define (err-invalid all-lists)
    (apply die 'map "invalid arguments" all-lists))
  (define mapm
    (lambda (f ls ls* n all-lists)
      (cond
       ((null? ls)
	(if (andmap null? ls*)
	    (if (fxzero? n)
		'()
	      (err-mutated all-lists))
	  (err-mismatch all-lists)))
       ((fxzero? n) (err-mutated all-lists))
       (else
	(cons
	 (apply f (car ls) (cars ls*))
	 (mapm f (cdr ls) (cdrs ls*) (fxsub1 n) all-lists))))))
  (define map
    (case-lambda
     ((f ls)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(let ((d (unsafe.cdr ls)))
	  (map1 f (unsafe.car ls) d (len d d 0))))
       ((null? ls) '())
       (else (err-invalid (list ls)))))
     ((f ls ls2)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(if (pair? ls2)
	    (let ((d (unsafe.cdr ls)))
	      (map2 f (unsafe.car ls) (unsafe.car ls2) d (unsafe.cdr ls2) (len d d 0)))
	  (err-invalid (list ls ls2))))
       ((and (null? ls) (null? ls2)) '())
       (else (err-invalid (list ls ls2)))))
     ((f ls . ls*)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(let ((n (len ls ls 0)))
	  (mapm f ls ls* n (cons ls ls*))))
       ((and (null? ls) (andmap null? ls*)) '())
       (else (err-invalid (cons ls ls*))))))))

(module (for-each)
  (define who 'for-each)
  (define len
    (lambda (h t n)
      (if (pair? h)
	  (let ((h (unsafe.cdr h)))
	    (if (pair? h)
		(if (eq? h t)
		    (die who "circular list")
		  (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2)))
	      (if (null? h)
		  (unsafe.fxadd1 n)
		(die who "improper list"))))
	(if (null? h)
	    n
	  (die who "improper list")))))
  (define for-each1
    (lambda (f a d n)
      (cond
       ((pair? d)
	(if (unsafe.fxzero? n)
	    (die who "list was altered!")
	  (begin
	    (f a)
	    (for-each1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
       ((null? d)
	(if (unsafe.fxzero? n)
	    (f a)
	  (die who "list was altered")))
       (else (die who "list was altered")))))
  (define for-each2
    (lambda (f a1 a2 d1 d2 n)
      (cond
       ((pair? d1)
	(cond
	 ((pair? d2)
	  (if (unsafe.fxzero? n)
	      (die who "list was altered")
	    (begin
	      (f a1 a2)
	      (for-each2 f
			 (unsafe.car d1) (unsafe.car d2)
			 (unsafe.cdr d1) (unsafe.cdr d2)
			 (unsafe.fxsub1 n)))))
	 (else (die who "length mismatch"))))
       ((null? d1)
	(cond
	 ((null? d2)
	  (if (unsafe.fxzero? n)
	      (f a1 a2)
	    (die who "list was altered")))
	 (else (die who "length mismatch"))))
       (else (die who "list was altered")))))
  (define for-each
    (case-lambda
     ((f ls)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(let ((d (unsafe.cdr ls)))
	  (for-each1 f (unsafe.car ls) d (len d d 0))))
       ((null? ls) (void))
       (else (die who "improper list"))))
     ((f ls ls2)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(if (pair? ls2)
	    (let ((d (unsafe.cdr ls)))
	      (for-each2 f
			 (unsafe.car ls) (unsafe.car ls2) d (unsafe.cdr ls2) (len d d 0)))
	  (die who "length mismatch")))
       ((null? ls)
	(if (null? ls2)
	    (void)
	  (die who "length mismatch")))
       (else (die who "not a list"))))
     ((f ls . ls*)
      (unless (procedure? f)
	(die 'for-each "not a procedure" f))
      (unless (list? ls)
	(die 'for-each "not a list" ls))
      (let ((n (length ls)))
	(for-each
	    (lambda (x)
	      (unless (and (list? x) (= (length x) n))
		(die 'for-each "not a list" x)))
	  ls*)
	(let loop ((n (length ls)) (ls ls) (ls* ls*))
	  (cond
	   ((unsafe.fx= n 0)
	    (unless (and (null? ls) (andmap null? ls*))
	      (die 'for-each "list modified" f)))
	   (else
	    (unless (and (pair? ls) (andmap pair? ls*))
	      (die 'for-each "list modified" f))
	    (apply f (car ls) (map car ls*))
	    (loop (fx- n 1) (cdr ls) (map cdr ls*))))))))))

(module (andmap)
  (define who 'andmap)
  (define len
    (lambda (h t n)
      (if (pair? h)
	  (let ((h (unsafe.cdr h)))
	    (if (pair? h)
		(if (eq? h t)
		    (die who "circular list")
		  (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2)))
	      (if (null? h)
		  (unsafe.fxadd1 n)
		(die who "improper list"))))
	(if (null? h)
	    n
	  (die who "improper list")))))
  (define andmap1
    (lambda (f a d n)
      (cond
       ((pair? d)
	(if (unsafe.fxzero? n)
	    (die who "list was altered!")
	  (and (f a)
	       (andmap1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
       ((null? d)
	(if (unsafe.fxzero? n)
	    (f a)
	  (die who "list was altered")))
       (else (die who "list was altered")))))
  (define andmap2
    (lambda (f a1 a2 d1 d2 n)
      (cond
       ((pair? d1)
	(cond
	 ((pair? d2)
	  (if (unsafe.fxzero? n)
	      (die who "list was altered")
	    (and
	     (f a1 a2)
	     (andmap2 f
                      (unsafe.car d1) (unsafe.car d2)
                      (unsafe.cdr d1) (unsafe.cdr d2)
                      (unsafe.fxsub1 n)))))
	 (else (die who "length mismatch"))))
       ((null? d1)
	(cond
	 ((null? d2)
	  (if (unsafe.fxzero? n)
	      (f a1 a2)
	    (die who "list was altered")))
	 (else (die who "length mismatch"))))
       (else (die who "list was altered")))))
  (define andmap
    (case-lambda
     ((f ls)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(let ((d (unsafe.cdr ls)))
	  (andmap1 f (unsafe.car ls) d (len d d 0))))
       ((null? ls) #t)
       (else (die who "improper list"))))
     ((f ls ls2)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(if (pair? ls2)
	    (let ((d (unsafe.cdr ls)))
	      (andmap2 f
		       (unsafe.car ls) (unsafe.car ls2) d (unsafe.cdr ls2) (len d d 0)))
	  (die who "length mismatch")))
       ((null? ls)
	(if (null? ls2)
	    #t
	  (die who "length mismatch")))
       (else (die who "not a list"))))
     ((f ls . ls*)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (die who "vararg not yet supported")))))




(module (ormap)
  (define who 'ormap)
  (define len
    (lambda (h t n)
      (if (pair? h)
	  (let ((h (unsafe.cdr h)))
	    (if (pair? h)
		(if (eq? h t)
		    (die who "circular list")
		  (len (unsafe.cdr h) (unsafe.cdr t) (unsafe.fx+ n 2)))
	      (if (null? h)
		  (unsafe.fxadd1 n)
		(die who "improper list"))))
	(if (null? h)
	    n
	  (die who "improper list")))))
  (define ormap1
    (lambda (f a d n)
      (cond
       ((pair? d)
	(if (unsafe.fxzero? n)
	    (die who "list was altered!")
	  (or (f a)
	      (ormap1 f (unsafe.car d) (unsafe.cdr d) (unsafe.fxsub1 n)))))
       ((null? d)
	(if (unsafe.fxzero? n)
	    (f a)
	  (die who "list was altered")))
       (else (die who "list was altered")))))
  (define ormap
    (case-lambda
     ((f ls)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (cond
       ((pair? ls)
	(let ((d (unsafe.cdr ls)))
	  (ormap1 f (unsafe.car ls) d (len d d 0))))
       ((null? ls) #f)
       (else (die who "improper list"))))
     (_ (die who "vararg not supported yet")))))



(define partition
  (letrec ((race
	    (lambda (h t ls p)
	      (if (pair? h)
		  (let ((a0 (unsafe.car h)) (h (unsafe.cdr h)))
		    (if (pair? h)
			(if (eq? h t)
			    (die 'partition "circular list" ls)
			  (let ((a1 (unsafe.car h)))
			    (let-values (((a* b*) (race (unsafe.cdr h) (unsafe.cdr t) ls p)))
			      (if (p a0)
				  (if (p a1)
				      (values (cons* a0 a1 a*) b*)
				    (values (cons a0 a*) (cons a1 b*)))
				(if (p a1)
				    (values (cons a1 a*) (cons a0 b*))
				  (values a* (cons* a0 a1 b*)))))))
		      (if (null? h)
			  (if (p a0)
			      (values (list a0) '())
			    (values '() (list a0)))
			(die 'parititon "not a proper list" ls))))
		(if (null? h)
		    (values '() '())
		  (die 'parition "not a proper list" ls))))))
    (lambda (p ls)
      (unless (procedure? p)
	(die 'partition "not a procedure" p))
      (race ls ls ls p))))



(define-syntax define-iterator
  (syntax-rules ()
    ((_ name combine)
     (module (name)
       (define who 'name)
       (define (null*? ls)
	 (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))
       (define (err* ls*)
	 (if (null? ls*)
	     (die who "length mismatch")
	   (if (list? (car ls*))
	       (err* (cdr ls*))
	     (die who "not a proper list" (car ls*)))))
       (define (cars+cdrs ls ls*)
	 (cond
	  ((null? ls) (values '() '()))
	  (else
	   (let ((a (car ls)))
	     (if (pair? a)
		 (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
		   (values (cons (car a) cars) (cons (cdr a) cdrs)))
	       (if (list? (car ls*))
		   (die who "length mismatch")
		 (die who "not a proper list" (car ls*))))))))
       (define (loop1 f a h t ls)
	 (if (pair? h)
	     (let ((b (car h)) (h (cdr h)))
	       (combine (f a)
			(if (pair? h)
			    (if (eq? h t)
				(die who "circular" ls)
			      (let ((c (car h)) (h (cdr h)))
				(combine (f b) (loop1 f c h (cdr t) ls))))
			  (if (null? h)
			      (f b)
			    (combine (f b) (die who "not a proper list" ls))))))
	   (if (null? h)
	       (f a)
	     (combine (f a) (die who "not a proper list" ls)))))
       (define (loopn f a a* h h* t ls ls*)
	 (if (pair? h)
	     (let-values (((b* h*) (cars+cdrs h* ls*)))
	       (let ((b (car h)) (h (cdr h)))
		 (combine (apply f a a*)
			  (if (pair? h)
			      (if (eq? h t)
				  (die who "circular" ls)
				(let-values (((c* h*) (cars+cdrs h* ls*)))
				  (let ((c (car h)) (h (cdr h)))
				    (combine (apply f b b*)
					     (loopn f c c* h h* (cdr t) ls ls*)))))
			    (if (and (null? h) (null*? h*))
				(apply f b b*)
			      (combine (apply f b b*) (err* (cons ls ls*))))))))
	   (if (and (null? h) (null*? h*))
	       (apply f a a*)
	     (combine (apply f a a*) (err* (cons ls ls*))))))
       (define name
	 (case-lambda
	  ((f ls)
	   (unless (procedure? f)
	     (die who "not a procedure" f))
	   (if (pair? ls)
	       (loop1 f (car ls) (cdr ls) (cdr ls) ls)
	     (if (null? ls)
		 (combine)
	       (die who "not a list" ls))))
	  ((f ls . ls*)
	   (unless (procedure? f)
	     (die who "not a procedure" f))
	   (if (pair? ls)
	       (let-values (((cars cdrs) (cars+cdrs ls* ls*)))
		 (loopn f (car ls) cars (cdr ls) cdrs (cdr ls) ls ls*))
	     (if (and (null? ls) (null*? ls*))
		 (combine)
	       (err* ls*))))))))))

(define-iterator for-all and)
(define-iterator exists  or)

(module (fold-left)
  (define who 'fold-left)
  (define (null*? ls)
    (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))
  (define (err* ls*)
    (if (null? ls*)
	(die who "length mismatch")
      (if (list? (car ls*))
	  (err* (cdr ls*))
	(die who "not a proper list" (car ls*)))))
  (define (cars+cdrs ls ls*)
    (cond
     ((null? ls) (values '() '()))
     (else
      (let ((a (car ls)))
	(if (pair? a)
	    (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
	      (values (cons (car a) cars) (cons (cdr a) cdrs)))
	  (if (list? (car ls*))
	      (die who "length mismatch")
	    (die who "not a proper list" (car ls*))))))))
  (define (loop1 f nil h t ls)
    (if (pair? h)
	(let ((a (car h)) (h (cdr h)))
	  (if (pair? h)
	      (if (eq? h t)
		  (die who "circular" ls)
		(let ((b (car h)) (h (cdr h)) (t (cdr t)))
		  (loop1 f (f (f nil a) b) h t ls)))
	    (if (null? h)
		(f nil a)
	      (die who "not a proper list" ls))))
      (if (null? h)
	  nil
	(die who "not a proper list" ls))))
  (define (loopn f nil h h* t ls ls*)
    (if (pair? h)
	(let-values (((a* h*) (cars+cdrs h* ls*)))
	  (let ((a (car h)) (h (cdr h)))
	    (if (pair? h)
		(if (eq? h t)
		    (die who "circular" ls)
		  (let-values (((b* h*) (cars+cdrs h* ls*)))
		    (let ((b (car h)) (h (cdr h)) (t (cdr t)))
		      (loopn f
			     (apply f (apply f nil a a*) b b*)
			     h h* t ls ls*))))
	      (if (and (null? h) (null*? h*))
		  (apply f nil a a*)
		(err* (cons ls ls*))))))
      (if (and (null? h) (null*? h*))
	  nil
	(err* (cons ls ls*)))))
  (define fold-left
    (case-lambda
     ((f nil ls)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (loop1 f nil ls ls ls))
     ((f nil ls . ls*)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (loopn f nil ls ls* ls ls ls*)))))

(module (fold-right)
  (define who 'fold-right)
  (define (null*? ls)
    (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))
  (define (err* ls*)
    (if (null? ls*)
	(die who "length mismatch")
      (if (list? (car ls*))
	  (err* (cdr ls*))
	(die who "not a proper list" (car ls*)))))
  (define (cars+cdrs ls ls*)
    (cond
     ((null? ls) (values '() '()))
     (else
      (let ((a (car ls)))
	(if (pair? a)
	    (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
	      (values (cons (car a) cars) (cons (cdr a) cdrs)))
	  (if (list? (car ls*))
	      (die who "length mismatch")
	    (die who "not a proper list" (car ls*))))))))
  (define (loop1 f nil h t ls)
    (if (pair? h)
	(let ((a (car h)) (h (cdr h)))
	  (if (pair? h)
	      (if (eq? h t)
		  (die who "circular" ls)
		(let ((b (car h)) (h (cdr h)) (t (cdr t)))
		  (f a (f b (loop1 f nil h t ls)))))
	    (if (null? h)
		(f a nil)
	      (die who "not a proper list" ls))))
      (if (null? h)
	  nil
	(die who "not a proper list" ls))))
  (define (loopn f nil h h* t ls ls*)
    (if (pair? h)
	(let-values (((a* h*) (cars+cdrs h* ls*)))
	  (let ((a (car h)) (h (cdr h)))
	    (if (pair? h)
		(if (eq? h t)
		    (die who "circular" ls)
		  (let-values (((b* h*) (cars+cdrs h* ls*)))
		    (let ((b (car h)) (h (cdr h)) (t (cdr t)))
		      (apply f a
			     (append a*
				     (list
				      (apply f
					     b (append b*
						       (list (loopn f nil h h* t ls ls*))))))))))
	      (if (and (null? h) (null*? h*))
		  (apply f a (append a* (list nil)))
		(err* (cons ls ls*))))))
      (if (and (null? h) (null*? h*))
	  nil
	(err* (cons ls ls*)))))
  (define fold-right
    (case-lambda
     ((f nil ls)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (loop1 f nil ls ls ls))
     ((f nil ls . ls*)
      (unless (procedure? f)
	(die who "not a procedure" f))
      (loopn f nil ls ls* ls ls ls*))
     )))


;;;; done

)

;;; end of file
