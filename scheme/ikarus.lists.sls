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


#!vicare
(library (ikarus lists)
  (export
    make-list-of-predicate
    list? circular-list? list-of-single-item?
    list cons* make-list append length list-ref reverse
    last-pair memq memp memv member find assq assp assv assoc
    remq remv remove remp filter map for-each
    (rename (for-each for-each-in-order)) andmap ormap list-tail
    partition for-all exists fold-left fold-right
    make-queue-procs

    ;; unsafe bindings
    $length)
  (import (except (vicare)
		  make-list-of-predicate
		  list?  circular-list? list-of-single-item?
		  list cons* make-list append reverse
		  last-pair length list-ref memq memp memv member find
		  assq assp assv assoc remq remv remove remp filter
		  map for-each for-each-in-order andmap ormap list-tail partition
		  for-all exists fold-left fold-right
		  make-queue-procs)
    (vicare system $fx)
    (vicare system $pairs))


;;;; arguments validation

(define (list-length? obj)
  (and (fixnum? obj) ($fxnonnegative? obj)))

(define (list-index? obj)
  (and (fixnum? obj) ($fxnonnegative? obj)))

(define-syntax-rule (%error-list-was-altered-while-processing)
  (assertion-violation __who__ "list was altered while processing"))

(define-syntax-rule (%error-circular-list-is-invalid-as-argument ?obj)
  (assertion-violation __who__ "circular list is invalid as argument" ?obj))

(define-syntax-rule (%error-length-mismatch-among-list-arguments)
  (procedure-arguments-consistency-violation __who__ "length mismatch among list arguments"))

(define-syntax-rule (%error-expected-proper-list-as-argument ?obj)
  (assertion-violation __who__ "expected proper list as argument" ?obj))

(define-syntax-rule (%error-improper-list-is-invalid-as-argument ?obj)
  (assertion-violation __who__ "improper list is invalid as argument" ?obj))

(define-syntax-rule (%error-malformed-alist-as-argument ?arg-index ?arg)
  (procedure-argument-violation __who__ "malformed alist as argument" ?arg))


;;;; helpers

(define-syntax-rule (with-who ?name . ?body)
  (fluid-let-syntax
      ((__who__ (identifier-syntax (quote ?name))))
    . ?body))

;;Commented out because  it appears to be useless:  $MEMQ is a primitive
;;operation (Marco Maggi; Oct 28, 2011).
;;
;; (define ($memq x ls)
;;   (and (pair? ls)
;;        (if (eq? x ($car ls))
;; 	   ls
;; 	 ($memq x ($cdr ls)))))

(define list (lambda x x))

(define (cons* fst . rest)
  (let loop ((fst fst) (rest rest))
    (if (null? rest)
	fst
      (cons fst (loop ($car rest) ($cdr rest))))))

(module (list?)

  (define (list? x)
    (%race x x))

  (define (%race h t)
    ;;Tortoise and hare algorithm to detect circular lists.
    (if (pair? h)
	(let ((h ($cdr h)))
	  (if (pair? h)
	      (and (not (eq? h t))
		   (%race ($cdr h) ($cdr t)))
	    (null? h)))
      (null? h)))

  #| end of module |# )

(define (list-of-single-item? ell)
  (and (pair? ell)
       (null? (cdr ell))))

(define (circular-list? obj)
  ;;At every iteration ELL is CDR-ed twice, LAG is CDR-ed once.
  (let loop ((ell obj)
	     (lag obj))
    (and (pair? ell)
	 (let ((ell (cdr ell)))
	   (and (pair? ell)
		(let ((ell (cdr ell))
		      (lag (cdr lag)))
		  (or (eq? ell lag)
		      (loop ell lag))))))))

(define (make-list-of-predicate item-pred)
  (define (%race h t)
    ;;Tortoise and hare algorithm to detect circular lists.
    (if (pair? h)
	(begin
	  (debug-print 'list-of (car h))
	  (and (item-pred ($car h))
	       (let ((h ($cdr h)))
		 (if (pair? h)
		     (begin
		       (debug-print 'list-of (car h))
		       (and (item-pred ($car h))
			    (not (eq? h t))
			    (%race ($cdr h) ($cdr t))))
		   (null? h)))))
      (null? h)))
  (lambda (obj)
    (%race obj obj)))

(case-define* make-list
  (({n list-length?})
   (%$make-list n (void) '()))
  (({n list-length?} fill)
   (%$make-list n fill '())))

(define (%$make-list n fill ls)
  (if ($fxzero? n)
      ls
    (%$make-list ($fxsub1 n) fill (cons fill ls))))

(define* (length ls)
  (define (%race h t ls n)
    (with-who length
      (cond ((pair? h)
	     (let ((h ($cdr h)))
	       (if (pair? h)
		   (if (not (eq? h t))
		       (%race ($cdr h) ($cdr t) ls ($fx+ n 2))
		     (%error-circular-list-is-invalid-as-argument ls))
		 (if (null? h)
		     ($fxadd1 n)
		   (%error-improper-list-is-invalid-as-argument ls)))))
	    ((null? h)
	     n)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls 0))

(define ($length ell)
  ;;Assume ELL is a proper list and compute its length as fast as possible.
  ;;
  (let recur ((len 0)
	      (ell ell))
    (if (pair? ell)
	(recur ($fxadd1 len) ($cdr ell))
      len)))


(define* (list-ref the-list {the-index list-index?})
  (define (%error-index-out-of-range)
    (procedure-arguments-consistency-violation __who__ "index is out of range" the-index the-list))
  (define (%$list-ref ls i)
    (with-who list-ref
      (cond (($fxzero? i)
	     (if (pair? ls)
		 ($car ls)
	       (%error-index-out-of-range)))
	    ((pair? ls)
	     (%$list-ref ($cdr ls) ($fxsub1 i)))
	    ((null? ls)
	     (%error-index-out-of-range))
	    (else
	     (%error-expected-proper-list-as-argument the-list)))))
  (%$list-ref the-list the-index))


(define* (list-tail list {index list-index?})
  (define (%$list-tail ls i)
    (with-who list-tail
      (cond (($fxzero? i)
	     ls)
	    ((pair? ls)
	     (%$list-tail ($cdr ls) ($fxsub1 i)))
	    ((null? ls)
	     (procedure-arguments-consistency-violation __who__ "index is out of range" index list))
	    (else
	     (%error-expected-proper-list-as-argument list)))))
  (%$list-tail list index))


(case-define* append
  (()		'())
  ((ls)		ls)
  ((ls . ls*)
   (define (reverse h t ls ac)
     (with-who append
       (cond ((pair? h)
	      (let ((h ($cdr h)) (a1 ($car h)))
		(cond ((pair? h)
		       (if (not (eq? h t))
			   (let ((a2 ($car h)))
			     (reverse ($cdr h) ($cdr t) ls (cons a2 (cons a1 ac))))
			 (%error-circular-list-is-invalid-as-argument ls)))
		      ((null? h)
		       (cons a1 ac))
		      (else
		       (%error-expected-proper-list-as-argument ls)))))
	     ((null? h)
	      ac)
	     (else
	      (%error-expected-proper-list-as-argument ls)))))

   (define (rev! ls ac)
     (if (null? ls)
	 ac
       (let ((ls^ ($cdr ls)))
	 ($set-cdr! ls ac)
	 (rev! ls^ ls))))

   (define (append1 ls ls*)
     (if (null? ls*)
	 ls
       (rev! (reverse ls ls ls '())
	     (append1 ($car ls*) ($cdr ls*)))))

   (append1 ls ls*))
  #| end of CASE-DEFINE* |# )


(define* (reverse x)
  (define (%race h t ls ac)
    (with-who reverse
      (cond ((pair? h)
	     (let ((h  ($cdr h))
		   (ac (cons ($car h) ac)))
	       (cond ((pair? h)
		      (if (not (eq? h t))
			  (%race ($cdr h) ($cdr t) ls (cons ($car h) ac))
			(%error-circular-list-is-invalid-as-argument ls)))
		     ((null? h)
		      ac)
		     (else
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? h)
	     ac)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race x x x '()))


(define* (last-pair {x pair?})
  (define (%race h t ls last)
    (if (pair? h)
	(let ((h ($cdr h)) (last h))
	  (if (pair? h)
	      (if (not (eq? h t))
		  (%race ($cdr h) ($cdr t) ls h)
		(%error-circular-list-is-invalid-as-argument ls))
	    last))
      last))
  (let ((d ($cdr x)))
    (%race d d x x)))


(define* (memq x ls)
  (define (%race h t ls x)
    (with-who memq
      (cond ((pair? h)
	     (if (eq? ($car h) x)
		 h
	       (let ((h ($cdr h)))
		 (cond ((pair? h)
			(cond ((eq? ($car h) x)
			       h)
			      ((not (eq? h t))
			       (%race ($cdr h) ($cdr t) ls x))
			      (else
			       (%error-circular-list-is-invalid-as-argument ls))))
		       ((null? h)
			#f)
		       (else
			(%error-expected-proper-list-as-argument ls))))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls x))


(define* (memv x ls)
  (define (%race h t ls x)
    (with-who memv
      (cond ((pair? h)
	     (if (eqv? ($car h) x)
		 h
	       (let ((h ($cdr h)))
		 (cond ((pair? h)
			(cond ((eqv? ($car h) x)
			       h)
			      ((not (eq? h t))
			       (%race ($cdr h) ($cdr t) ls x))
			      (else
			       (%error-circular-list-is-invalid-as-argument ls))))
		       ((null? h)
			#f)
		       (else
			(%error-expected-proper-list-as-argument ls))))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls x))


(define* (member x ls)
  (define (%race h t ls x)
    (with-who member
      (cond ((pair? h)
	     (if (equal? ($car h) x)
		 h
	       (let ((h ($cdr h)))
		 (cond ((pair? h)
			(cond ((equal? ($car h) x)
			       h)
			      ((not (eq? h t))
			       (%race ($cdr h) ($cdr t) ls x))
			      (else
			       (%error-circular-list-is-invalid-as-argument ls))))
		       ((null? h)
			#f)
		       (else
			(%error-expected-proper-list-as-argument ls))))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls x))


(define* (memp {p procedure?} ls)
  (define (%race h t ls p)
    (with-who memp
      (cond ((pair? h)
	     (if (p ($car h))
		 h
	       (let ((h ($cdr h)))
		 (cond ((pair? h)
			(cond ((p ($car h))
			       h)
			      ((not (eq? h t))
			       (%race ($cdr h) ($cdr t) ls p))
			      (else
			       (%error-circular-list-is-invalid-as-argument ls))))
		       ((null? h)
			#f)
		       (else
			(%error-expected-proper-list-as-argument ls))))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls p))


(define* (find {p procedure?} ls)
  (define (%race h t ls p)
    (with-who find
      (cond ((pair? h)
	     (let ((a ($car h)))
	       (if (p a)
		   a
		 (let ((h ($cdr h)))
		   (cond ((pair? h)
			  (let ((a ($car h)))
			    (cond ((p a)
				   a)
				  ((not (eq? h t))
				   (%race ($cdr h) ($cdr t) ls p))
				  (else
				   (%error-circular-list-is-invalid-as-argument ls)))))
			 ((null? h)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument ls)))))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls p))


(define* (assq x ls)
  (define (%race x h t ls)
    (with-who assq
      (cond ((pair? h)
	     (let ((a ($car h)) (h ($cdr h)))
	       (if (pair? a)
		   (cond ((eq? ($car a) x)
			  a)
			 ((pair? h)
			  (if (not (eq? h t))
			      (let ((a ($car h)))
				(if (pair? a)
				    (if (eq? ($car a) x)
					a
				      (%race x ($cdr h) ($cdr t) ls))
				  (%error-malformed-alist-as-argument 2 ls)))
			    (%error-circular-list-is-invalid-as-argument ls)))
			 ((null? h)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument ls)))
		 (%error-malformed-alist-as-argument 2 ls))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race x ls ls ls))


(define* (assp {p procedure?} ls)
  (define (%race p h t ls)
    (with-who assp
      (cond ((pair? h)
	     (let ((a ($car h)) (h ($cdr h)))
	       (if (pair? a)
		   (cond ((p ($car a))
			  a)
			 ((pair? h)
			  (if (not (eq? h t))
			      (let ((a ($car h)))
				(if (pair? a)
				    (if (p ($car a))
					a
				      (%race p ($cdr h) ($cdr t) ls))
				  (%error-malformed-alist-as-argument 2 ls)))
			    (%error-circular-list-is-invalid-as-argument ls)))
			 ((null? h)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument ls)))
		 (%error-malformed-alist-as-argument 2 ls))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race p ls ls ls))


(define* (assv x ls)
  (define (%race x h t ls)
    (with-who assv
      (cond ((pair? h)
	     (let ((a ($car h)) (h ($cdr h)))
	       (if (pair? a)
		   (cond ((eqv? ($car a) x)
			  a)
			 ((pair? h)
			  (if (not (eq? h t))
			      (let ((a ($car h)))
				(if (pair? a)
				    (if (eqv? ($car a) x)
					a
				      (%race x ($cdr h) ($cdr t) ls))
				  (%error-malformed-alist-as-argument 2 ls)))
			    (%error-circular-list-is-invalid-as-argument ls)))
			 ((null? h)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument ls)))
		 (%error-malformed-alist-as-argument 2 ls))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race x ls ls ls))


(define* (assoc x ls)
  (define (%race x h t ls)
    (with-who assoc
      (cond ((pair? h)
	     (let ((a ($car h)) (h ($cdr h)))
	       (if (pair? a)
		   (cond ((equal? ($car a) x)
			  a)
			 ((pair? h)
			  (if (not (eq? h t))
			      (let ((a ($car h)))
				(if (pair? a)
				    (if (equal? ($car a) x)
					a
				      (%race x ($cdr h) ($cdr t) ls))
				  (%error-malformed-alist-as-argument 2 ls)))
			    (%error-circular-list-is-invalid-as-argument ls)))
			 ((null? h)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument ls)))
		 (%error-malformed-alist-as-argument 2 ls))))
	    ((null? h)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race x ls ls ls))


(define-syntax define-remover
  (syntax-rules ()
    ((_ ?name ?cmp ?check)
     (define* (?name {x ?check} ls)
       (define (%race h t ls x)
	 (with-who ?name
	   (cond ((pair? h)
		  (if (?cmp ($car h) x)
		      (let ((h ($cdr h)))
			(cond ((pair? h)
			       (if (not (eq? h t))
				   (if (?cmp ($car h) x)
				       (%race ($cdr h) ($cdr t) ls x)
				     (cons ($car h) (%race ($cdr h) ($cdr t) ls x)))
				 (%error-circular-list-is-invalid-as-argument ls)))
			      ((null? h)
			       '())
			      (else
			       (%error-expected-proper-list-as-argument ls))))
		    (let ((a0 ($car h)) (h ($cdr h)))
		      (cond ((pair? h)
			     (if (not (eq? h t))
				 (if (?cmp ($car h) x)
				     (cons a0 (%race ($cdr h) ($cdr t) ls x))
				   (cons* a0 ($car h) (%race ($cdr h) ($cdr t) ls x)))
			       (%error-circular-list-is-invalid-as-argument ls)))
			    ((null? h)
			     (list a0))
			    (else
			     (%error-expected-proper-list-as-argument ls))))))
		 ((null? h)
		  '())
		 (else
		  (%error-expected-proper-list-as-argument ls)))))
       (%race ls ls ls x)))
    ))

(define (%always-true? obj)
  #t)

(define-remover remq	eq?				%always-true?)
(define-remover remv	eqv?				%always-true?)
(define-remover remove	equal?				%always-true?)
(define-remover remp	(lambda (elt p) (p elt))		procedure?)
(define-remover filter	(lambda (elt p) (not (p elt)))	procedure?)


(module (map)

  (case-define* map
    (({f procedure?} ls)
     (cond ((pair? ls)
	    (let ((d ($cdr ls)))
	      (map1 f ($car ls) d (len d d 0))))
	   ((null? ls)
	    '())
	   (else
	    (err-invalid (list ls)))))

    (({f procedure?} ls ls2)
     (cond ((pair? ls)
	    (if (pair? ls2)
		(let ((d ($cdr ls)))
		  (map2 f ($car ls) ($car ls2) d ($cdr ls2) (len d d 0)))
	      (err-invalid (list ls ls2))))
	   ((and (null? ls) (null? ls2))
	    '())
	   (else
	    (err-invalid (list ls ls2)))))

    (({f procedure?} ls . ls*)
     (cond ((pair? ls)
	    (let ((n (len ls ls 0)))
	      (mapm f ls ls* n (cons ls ls*))))
	   ((and (null? ls) (andmap null? ls*))
	    '())
	   (else
	    (err-invalid (cons ls ls*)))))

    #| end of CASE-DEFINE* |# )

  (define (len h t n)
    (with-who map
      (cond ((pair? h)
	     (let ((h ($cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument h)
			(len ($cdr h) ($cdr t) ($fx+ n 2))))
		     ((null? h)
		      ($fxadd1 n))
		     (else
		      (%error-expected-proper-list-as-argument h)))))
	    ((null? h)
	     n)
	    (else
	     (%error-expected-proper-list-as-argument h)))))

  (define (map1 f a d n)
    (with-who map
      (cond ((pair? d)
	     (if ($fxzero? n)
		 (%error-list-was-altered-while-processing)
	       (cons (f a) (map1 f ($car d) ($cdr d) ($fxsub1 n)))))
	    ((null? d)
	     (if ($fxzero? n)
		 (cons (f a) '())
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define (map2 f a1 a2 d1 d2 n)
    (with-who map
      (cond ((pair? d1)
	     (cond ((pair? d2)
		    (if ($fxzero? n)
			(%error-list-was-altered-while-processing)
		      (cons (f a1 a2)
			    (map2 f
				  ($car d1) ($car d2)
				  ($cdr d1) ($cdr d2)
				  ($fxsub1 n)))))
		   ((null? d2)
		    (%error-length-mismatch-among-list-arguments))
		   (else
		    (%error-expected-proper-list-as-argument d2))))
	    ((null? d1)
	     (cond ((null? d2)
		    (if ($fxzero? n)
			(cons (f a1 a2) '())
		      (%error-list-was-altered-while-processing)))
		   (else
		    (if (list? d2)
			(%error-length-mismatch-among-list-arguments)
		      (%error-expected-proper-list-as-argument d2)))))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define (cars ls*)
    (with-who map
      (if (null? ls*)
	  '()
	(let ((a (car ls*)))
	  (if (pair? a)
	      (cons (car a) (cars (cdr ls*)))
	    (%error-length-mismatch-among-list-arguments))))))

  (define (cdrs ls*)
    (with-who map
      (if (null? ls*)
	  '()
	(let ((a (car ls*)))
	  (if (pair? a)
	      (cons (cdr a) (cdrs (cdr ls*)))
	    (%error-length-mismatch-among-list-arguments))))))

  (define (err-mutated all-lists)
    (with-who map
      (%error-list-was-altered-while-processing)))

  (define (err-mismatch all-lists)
    (with-who map
      (%error-length-mismatch-among-list-arguments)))

  (define (err-invalid all-lists)
    (with-who map
      (apply assertion-violation __who__ "invalid arguments" all-lists)))

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

  #| end of module |# )


(module (for-each)

  (case-define* for-each
    (({f procedure?} ls)
     (cond ((pair? ls)
	    (let ((d ($cdr ls)))
	      (for-each1 f ($car ls) d (len d d 0))))
	   ((null? ls)
	    (void))
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    (({f procedure?} ls ls2)
     (cond ((pair? ls)
	    (if (pair? ls2)
		(let ((d ($cdr ls)))
		  (for-each2 f ($car ls) ($car ls2) d ($cdr ls2) (len d d 0)))
	      (%error-length-mismatch-among-list-arguments)))
	   ((null? ls)
	    (if (null? ls2)
		(void)
	      (%error-length-mismatch-among-list-arguments)))
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    (({f procedure?} {ls list?} . ls*)
     (let ((n (length ls)))
       (for-each (lambda (x)
		   (unless (and (list? x) (= (length x) n))
		     (%error-expected-proper-list-as-argument x)))
	 ls*)
       (let loop ((n (length ls)) (ls ls) (ls* ls*))
	 (if ($fxzero? n)
	     (unless (and (null? ls) (andmap null? ls*))
	       (%error-list-was-altered-while-processing))
	   (begin
	     (unless (and (pair? ls) (andmap pair? ls*))
	       (%error-list-was-altered-while-processing))
	     (apply f (car ls) (map car ls*))
	     (loop (fx- n 1) (cdr ls) (map cdr ls*)))))))

    #| end of CASE-DEFINE* |# )

  (define (len h t n)
    (with-who for-each
      (cond ((pair? h)
	     (let ((h ($cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument h)
			(len ($cdr h) ($cdr t) ($fx+ n 2))))
		     ((null? h)
		      ($fxadd1 n))
		     (else
		      (%error-expected-proper-list-as-argument h)))))
	    ((null? h)
	     n)
	    (else
	     (%error-expected-proper-list-as-argument h)))))

  (define (for-each1 f a d n)
    (with-who for-each
      (cond ((pair? d)
	     (if ($fxzero? n)
		 (%error-list-was-altered-while-processing)
	       (begin
		 (f a)
		 (for-each1 f ($car d) ($cdr d) ($fxsub1 n)))))
	    ((null? d)
	     (if ($fxzero? n)
		 (f a)
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define (for-each2 f a1 a2 d1 d2 n)
    (with-who for-each
      (cond ((pair? d1)
	     (if (pair? d2)
		 (if ($fxzero? n)
		     (%error-list-was-altered-while-processing)
		   (begin
		     (f a1 a2)
		     (for-each2 f
				($car d1) ($car d2)
				($cdr d1) ($cdr d2)
				($fxsub1 n))))
	       (%error-length-mismatch-among-list-arguments)))
	    ((null? d1)
	     (if (null? d2)
		 (if ($fxzero? n)
		     (f a1 a2)
		   (%error-list-was-altered-while-processing))
	       (%error-length-mismatch-among-list-arguments)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  #| end of module |#)


(module (andmap)
  ;;ANDMAP should be the same as R6RS's FOR-ALL (Marco Maggi; Oct 28, 2011).
  ;;
  (case-define* andmap
    (({f procedure?} ls)
     (cond ((pair? ls)
	    (let ((d ($cdr ls)))
	      (andmap1 f ($car ls) d (len d d 0))))
	   ((null? ls)
	    #t)
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    (({f procedure?} ls ls2)
     (cond ((pair? ls)
	    (if (pair? ls2)
		(let ((d ($cdr ls)))
		  (andmap2 f
			   ($car ls) ($car ls2) d ($cdr ls2) (len d d 0)))
	      (%error-length-mismatch-among-list-arguments)))
	   ((null? ls)
	    (if (null? ls2)
		#t
	      (%error-length-mismatch-among-list-arguments)))
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    #| end of CASE-DEFINE* |# )

  (define (len h t n)
    (with-who andmap
      (cond ((pair? h)
	     (let ((h ($cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument h)
			(len ($cdr h) ($cdr t) ($fx+ n 2))))
		     ((null? h)
		      ($fxadd1 n))
		     (else
		      (%error-expected-proper-list-as-argument h)))))
	    ((null? h)
	     n)
	    (else
	     (%error-expected-proper-list-as-argument h)))))

  (define (andmap1 f a d n)
    (with-who for-each
      (cond ((pair? d)
	     (if ($fxzero? n)
		 (%error-list-was-altered-while-processing)
	       (and (f a)
		    (andmap1 f ($car d) ($cdr d) ($fxsub1 n)))))
	    ((null? d)
	     (if ($fxzero? n)
		 (f a)
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define (andmap2 f a1 a2 d1 d2 n)
    (with-who for-each
      (cond ((pair? d1)
	     (if (pair? d2)
		 (if ($fxzero? n)
		     (%error-list-was-altered-while-processing)
		   (and (f a1 a2)
			(andmap2 f
				 ($car d1) ($car d2)
				 ($cdr d1) ($cdr d2)
				 ($fxsub1 n))))
	       (%error-length-mismatch-among-list-arguments)))
	    ((null? d1)
	     (if (null? d2)
		 (if ($fxzero? n)
		     (f a1 a2)
		   (%error-list-was-altered-while-processing))
	       (%error-length-mismatch-among-list-arguments)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  #| end of module |# )


(module (ormap)
  ;;ANDMAP should be the same as R6RS's EXISTS (Marco Maggi; Oct 28, 2011).
  ;;
  (define* (ormap {f procedure?} ls)
    (cond ((pair? ls)
	   (let ((d ($cdr ls)))
	     (ormap1 f ($car ls) d (len d d 0))))
	  ((null? ls)
	   #f)
	  (else
	   (%error-expected-proper-list-as-argument ls))))

  (define (len h t n)
    (with-who for-each
      (cond ((pair? h)
	     (let ((h ($cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument h)
			(len ($cdr h) ($cdr t) ($fx+ n 2))))
		     ((null? h)
		      ($fxadd1 n))
		     (else
		      (%error-expected-proper-list-as-argument h)))))
	    ((null? h)
	     n)
	    (else
	     (%error-expected-proper-list-as-argument h)))))

  (define (ormap1 f a d n)
    (with-who for-each
      (cond ((pair? d)
	     (if ($fxzero? n)
		 (%error-list-was-altered-while-processing)
	       (or (f a)
		   (ormap1 f ($car d) ($cdr d) ($fxsub1 n)))))
	    ((null? d)
	     (if ($fxzero? n)
		 (f a)
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  #| end of module |# )


(define* (partition {p procedure?} ls)
  (define (%race h t ls p)
    (with-who partition
      (cond ((pair? h)
	     (let ((a0 ($car h))
		   (h  ($cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument ls)
			(let ((a1 ($car h)))
			  (let-values (((a* b*) (%race ($cdr h) ($cdr t) ls p)))
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
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? h)
	     (values '() '()))
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls p))


(define-syntax define-iterator
  (syntax-rules ()
    ((_ ?name ?combine)
     (module (?name)
       (case-define* ?name
	 (({f procedure?} ls)
	  (cond ((pair? ls)
		 (loop1 f (car ls) (cdr ls) (cdr ls) ls))
		((null? ls)
		 (?combine))
		(else
		 (%error-expected-proper-list-as-argument ls))))

	 (({f procedure?} ls . ls*)
	  (cond ((pair? ls)
		 (let-values (((cars cdrs) (cars+cdrs ls* ls*)))
		   (loopn f (car ls) cars (cdr ls) cdrs (cdr ls) ls ls*)))
		((and (null? ls) (null*? ls*))
		 (?combine))
		(else
		 (err* ls*))))
	 #| end of CASE-DEFINE* |# )

       (define (null*? ls)
	 (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))

       (define (err* ls*)
	 (with-who ?name
	   (for-each (lambda (ls)
		       (unless (list? ls)
			 (%error-expected-proper-list-as-argument ls)))
	     ls*)
	   (%error-length-mismatch-among-list-arguments)))

       (define (cars+cdrs ls ls*)
	 (with-who ?name
	   (if (null? ls)
	       (values '() '())
	     (let ((a (car ls)))
	       (cond ((pair? a)
		      (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
			(values (cons (car a) cars) (cons (cdr a) cdrs))))
		     ((list? (car ls*))
		      (%error-length-mismatch-among-list-arguments))
		     (else
		      (%error-expected-proper-list-as-argument (car ls*))))))))

       (define (loop1 f a h t ls)
	 (with-who ?name
	   (cond ((pair? h)
		  (let ((b (car h)) (h (cdr h)))
		    (?combine (f a)
			      (cond ((pair? h)
				     (if (eq? h t)
					 (%error-circular-list-is-invalid-as-argument h)
				       (let ((c (car h)) (h (cdr h)))
					 (?combine (f b) (loop1 f c h (cdr t) ls)))))
				    ((null? h)
				     (f b))
				    (else
				     (?combine (f b)
					       (%error-expected-proper-list-as-argument ls)))))))
		 ((null? h)
		  (f a))
		 (else
		  (?combine (f a) (%error-expected-proper-list-as-argument ls))))))

       (define (loopn f a a* h h* t ls ls*)
	 (with-who ?name
	   (cond ((pair? h)
		  (let-values (((b* h*) (cars+cdrs h* ls*)))
		    (let ((b (car h)) (h (cdr h)))
		      (?combine (apply f a a*)
				(if (pair? h)
				    (if (eq? h t)
					(%error-circular-list-is-invalid-as-argument h)
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
		  (?combine (apply f a a*) (err* (cons ls ls*)))))))

       #| end of module |# )
     )))

(define-iterator for-all and)
(define-iterator exists  or)


(module (fold-left)
  (case-define* fold-left
    (({f procedure?} nil ls)
     (loop1 f nil ls ls ls))
    (({f procedure?} nil ls . ls*)
     (loopn f nil ls ls* ls ls ls*))
    #| end of CASE-DEFINE* |# )

  (define (null*? ls)
    (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))

  (define (err* ls*)
    (with-who fold-left
      (cond ((null? ls*)
	     (%error-length-mismatch-among-list-arguments))
	    ((list? (car ls*))
	     (err* (cdr ls*)))
	    (else
	     (%error-expected-proper-list-as-argument (car ls*))))))

  (define (cars+cdrs ls ls*)
    (with-who fold-left
      (if (null? ls)
	  (values '() '())
	(let ((a (car ls)))
	  (cond ((pair? a)
		 (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
		   (values (cons (car a) cars) (cons (cdr a) cdrs))))
		((list? (car ls*))
		 (%error-length-mismatch-among-list-arguments))
		(else
		 (%error-expected-proper-list-as-argument (car ls*))))))))

  (define (loop1 f nil h t ls)
    (with-who fold-left
      (cond ((pair? h)
	     (let ((a (car h)) (h (cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument ls)
			(let ((b (car h)) (h (cdr h)) (t (cdr t)))
			  (loop1 f (f (f nil a) b) h t ls))))
		     ((null? h)
		      (f nil a))
		     (else
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? h)
	     nil)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))

  (define (loopn f nil h h* t ls ls*)
    (with-who fold-left
      (cond ((pair? h)
	     (let-values (((a* h*) (cars+cdrs h* ls*)))
	       (let ((a (car h)) (h (cdr h)))
		 (cond ((pair? h)
			(if (eq? h t)
			    (%error-circular-list-is-invalid-as-argument ls)
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
	     (err* (cons ls ls*))))))

  #| end of module |# )


(module (fold-right)
  (case-define* fold-right
    (({f procedure?} nil ls)
     (loop1 f nil ls ls ls))
    (({f procedure?} nil ls . ls*)
     (loopn f nil ls ls* ls ls ls*))
    #| end of CASE-DEFINE* |# )

  (define (null*? ls)
    (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))

  (define (err* ls*)
    (with-who fold-right
      (cond ((null? ls*)
	     (%error-length-mismatch-among-list-arguments))
	    ((list? (car ls*))
	     (err* (cdr ls*)))
	    (else
	     (%error-expected-proper-list-as-argument (car ls*))))))

  (define (cars+cdrs ls ls*)
    (with-who fold-right
      (if (null? ls)
	  (values '() '())
	(let ((a (car ls)))
	  (cond ((pair? a)
		 (let-values (((cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))))
		   (values (cons (car a) cars) (cons (cdr a) cdrs))))
		((list? (car ls*))
		 (%error-length-mismatch-among-list-arguments))
		(else
		 (%error-expected-proper-list-as-argument (car ls*))))))))

  (define (loop1 f nil h t ls)
    (with-who fold-right
      (cond ((pair? h)
	     (let ((a (car h)) (h (cdr h)))
	       (cond ((pair? h)
		      (if (eq? h t)
			  (%error-circular-list-is-invalid-as-argument ls)
			(let ((b (car h)) (h (cdr h)) (t (cdr t)))
			  (f a (f b (loop1 f nil h t ls))))))
		     ((null? h)
		      (f a nil))
		     (else
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? h)
	     nil)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))

  (define (loopn f nil h h* t ls ls*)
    (with-who fold-right
      (cond ((pair? h)
	     (let-values (((a* h*) (cars+cdrs h* ls*)))
	       (let ((a (car h)) (h (cdr h)))
		 (cond ((pair? h)
			(if (eq? h t)
			    (%error-circular-list-is-invalid-as-argument ls)
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
	     (err* (cons ls ls*))))))

  #| end of module |#)


;;;; queue of items

(define make-queue-procs
  (case-lambda
   (()
    (make-queue-procs '()))
   ((init-values)
    ;;The value of this variable is #f or a pair representing a queue of
    ;;items.
    ;;
    ;;The car of the queue-pair is the  first pair of the list of items.
    ;;The cdr of the queue-pair is the last pair of the list of items.
    ;;
    (define queue-pair
      (if (null? init-values)
	  #f
	(cons init-values
	      (let find-last-pair ((L init-values))
		(if (null? ($cdr L))
		    L
		  (find-last-pair ($cdr L)))))))

    (define-syntax queue
      (syntax-rules ()
	((_)
	 queue-pair)
	((_ ?item)
	 (set! queue-pair ?item))))

    (define (empty-queue?)
      (not (queue)))

    (define (enqueue! item)
      (if (queue)
	  (let ((old-last-pair ($cdr (queue)))
		(new-last-pair (list item)))
	    ($set-cdr! old-last-pair new-last-pair)
	    ($set-cdr! (queue) new-last-pair))
	(let ((Q (list item)))
	  (queue (cons Q Q)))))

    (define (dequeue!)
      (if (queue)
	  (let ((head ($car (queue))))
	    (begin0
		($car head)
	      (let ((head ($cdr head)))
		(if (null? head)
		    (queue #f)
		  ($set-car! (queue) head)))))
	(error 'dequeue! "no more items in queue")))

    (values empty-queue? enqueue! dequeue!))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'with-who 'scheme-indent-function		1)
;; End:
