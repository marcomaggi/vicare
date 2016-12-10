;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified in 2010-2016 by Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (options typed-language)
  (export
    make-list-of-predicate
    list? circular-list? list-of-single-item?
    list cons* make-list append append-lists length list-ref reverse
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
		  list cons* make-list append append-lists reverse
		  last-pair length list-ref memq memp memv member find
		  assq assp assv assoc remq remv remove remp filter
		  map for-each for-each-in-order andmap ormap list-tail partition
		  for-all exists fold-left fold-right
		  make-queue-procs)
    (vicare system $pairs))


;;;; arguments validation

(define-type <list-length>
  <non-negative-exact-integer>)

(define-type <list-index>
  <non-negative-exact-integer>)

(define-syntax %error-list-was-altered-while-processing
  (syntax-rules ()
    ((_)
     (assertion-violation __who__ "list was altered while processing"))
    ((_ ?who)
     (assertion-violation ?who    "list was altered while processing"))
    ))

(define-syntax %error-circular-list-is-invalid-as-argument
  (syntax-rules ()
    ((_ ?obj)
     (assertion-violation __who__ "circular list is invalid as argument" ?obj))
    ((_ ?obj ?who)
     (assertion-violation ?who    "circular list is invalid as argument" ?obj))
    ))

(define-syntax-rule (%error-length-mismatch-among-list-arguments)
  (procedure-arguments-consistency-violation __who__ "length mismatch among list arguments"))

(define-syntax %error-expected-proper-list-as-argument
  (syntax-rules ()
    ((_ ?obj)
     (assertion-violation __who__ "expected proper list as argument" ?obj))
    ((_ ?obj ?who)
     (assertion-violation ?who    "expected proper list as argument" ?obj))
    ))

(define-syntax %error-improper-list-is-invalid-as-argument
  (syntax-rules ()
    ((_ ?obj)
     (assertion-violation __who__ "improper list is invalid as argument" ?obj))
    ((_ ?obj ?who)
     (assertion-violation ?who    "improper list is invalid as argument" ?obj))
    ))

(define-syntax %error-malformed-alist-as-argument
  (syntax-rules ()
    ((_ ?arg-index ?arg)
     (procedure-argument-violation __who__ "malformed alist as argument" ?arg))
    ((_ ?arg-index ?arg ?who)
     (procedure-argument-violation ?who    "malformed alist as argument" ?arg))
    ))


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


;;;; constructors

(define list (lambda x x))

(define (cons* fst . rest)
  (let loop ((fst fst) (rest rest))
    (if (null? rest)
	fst
      (cons fst (loop ($car rest) ($cdr rest))))))

(case-define make-list
  (({_ <list>} {n <non-negative-fixnum>})
   ;;FIXME We should use void here.  (Marco Maggi; Thu Dec 8, 2016)
   (make-list n #f #;(void)))
  (({_ <list>} {n <non-negative-fixnum>} fill)
   (unsafe-cast-signature (<list>)
     (let loop ((n	n)
		(fill	fill)
		(ls	'()))
       (if (zero-fixnum? n)
	   ls
	 (loop (sub1 n) fill (cons fill ls)))))))


;;;; predicates

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
	(and (item-pred ($car h))
	     (let ((h ($cdr h)))
	       (if (pair? h)
		   (begin
		     (debug-print 'list-of (car h))
		     (and (item-pred ($car h))
			  (not (eq? h t))
			  (%race ($cdr h) ($cdr t))))
		 (null? h))))
      (null? h)))
  (lambda (obj)
    (%race obj obj)))


;;;; computing list length

(define/typed ({length <list-length>} ls)
  (define/typed ({%race <list-length>} hare tortoise ls {n <list-length>})
    (with-who length
      (cond ((pair? hare)
	     (let ((hare ($cdr hare)))
	       (if (pair? hare)
		   (if (not (eq? hare tortoise))
		       (%race ($cdr hare) ($cdr tortoise) ls (fx+ n 2))
		     (%error-circular-list-is-invalid-as-argument ls))
		 (if (null? hare)
		     (fxadd1 n)
		   (%error-improper-list-is-invalid-as-argument ls)))))
	    ((null? hare)
	     n)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls 0))

(define/typed ({$length <top>} ell)
  ;;Assume ELL is a proper list and compute its length as fast as possible.
  ;;
  (let {recur <top>} ((len 0)
		      (ell ell))
    (if (pair? ell)
	(recur (add1 len) (cdr ell))
      len)))

;;; --------------------------------------------------------------------

(define/typed ({%length <list-length>} hare tortoise {n <list-length>} {who <symbol>})
  ;;Compute the length of HARE.  Raise an exception if HARE is not a proper list.
  ;;
  (cond ((pair? hare)
	 (let ((hare ($cdr hare)))
	   (cond ((pair? hare)
		  (if (eq? hare tortoise)
		      (%error-circular-list-is-invalid-as-argument hare who)
		    (%length ($cdr hare) ($cdr tortoise) (+ n 2) who)))
		 ((null? hare)
		  (add1 n))
		 (else
		  (%error-expected-proper-list-as-argument hare who)))))
	((null? hare)
	 n)
	(else
	 (%error-expected-proper-list-as-argument hare who))))


(define/typed ({list-ref <top>} the-list {the-index <list-index>})
  (define-syntax-rule (%error-index-out-of-range)
    (procedure-arguments-consistency-violation __who__ "index is out of range" the-index the-list))
  (define (%$list-ref ls i)
    (with-who list-ref
      (cond ((zero-fixnum? i)
	     (if (pair? ls)
		 ($car ls)
	       (%error-index-out-of-range)))
	    ((pair? ls)
	     (%$list-ref ($cdr ls) (sub1 i)))
	    ((null? ls)
	     (%error-index-out-of-range))
	    (else
	     (%error-expected-proper-list-as-argument the-list)))))
  (%$list-ref the-list the-index))


(define ({list-tail <top>} input-ls {input-index <list-index>})
  (define/typed ({%tail <top>} ls {i <list-index>})
    (cond ((zero-fixnum? i)
	   ls)
	  ((pair? ls)
	   (%tail (cdr ls) (sub1 i)))
	  ((null? ls)
	   (procedure-arguments-consistency-violation __who__ "index is out of range" input-index input-ls))
	  (else
	   (%error-expected-proper-list-as-argument input-ls))))
  (%tail input-ls input-index))


(case-define/typed append
  (({_ <null>})
   '())

  ;;Remember that the following usage examples are valid:
  ;;
  ;;   (append 1)	=> 1
  ;;   (append '(1) 2)	=> '(1 . 2)
  ;;
  ;;so we do not really know the type of arguments and return value.
  ;;
  (({_ <top>} ls)
   ls)

  ;;NOTE!!!  We  need to remember that  the last item in  LS* is allowed not  to be a
  ;;list!!!  Example:
  ;;
  ;;   (append '(1 2 3) 4) => (1 2 3 . 4)
  ;;
  ;;The following is also valid:
  ;;
  ;;   (append '() '())	=> ()
  ;;
  (({_ <top>} ls . {ls* <list>})
   (define (reverse ls tortoise input-ls accum)
     ;;Reverse  LS and  return the  result.   Validate LS  as proper  list; raise  an
     ;;exception if it is circular or improper.
     ;;
     ;;The argument TORTOISE  is used only to  detect if LS is a  circular list.  The
     ;;argument INPUT-LS is used only as irritant when an exception is raised.
     ;;
     ;;Example of arguments in recursive calls, LS has an even number of items:
     ;;
     ;;   (append  '(1 2 3 4) '(5))
     ;;   (reverse '(1 2 3 4) '(1 2 3 4) '(1 2 3 4)            '())
     ;;   (reverse '(3 4)     '(2 3 4)   '(1 2 3 4)     '(2 1 . ()))
     ;;   (reverse '()        '(3 4)     '(1 2 3 4) '(4 3 2 1 . ())) => (4 3 2 1)
     ;;
     ;;Another  example of  arguments in  recursive calls,  LS has  an odd  number of
     ;;items:
     ;;
     ;;   (append  '(1 2 3) '(4))
     ;;   (reverse '(1 2 3) '(1 2 3) '(1 2 3)          '())
     ;;   (reverse '(3)     '(2 3)   '(1 2 3)   '(2 1 . ())) => (3 2 1)
     ;;
     (with-who append
       (cond ((pair? ls)
	      (let ((D (cdr ls))
		    (A (car ls)))
		(cond ((pair? D)
		       (if (eq? D tortoise)
			   (%error-circular-list-is-invalid-as-argument input-ls)
			 ;;Notice that here we perform 2  steps in LS and one step in
			 ;;TORTOISE.
			 (reverse (cdr D) ($cdr tortoise) input-ls (cons (car D) (cons A accum)))))
		      ((null? D)
		       (cons A accum))
		      (else
		       (%error-expected-proper-list-as-argument input-ls)))))
	     ((null? ls)
	      accum)
	     (else
	      (%error-expected-proper-list-as-argument input-ls)))))

   (define (rev! ls accum)
     ;;ACCUM can be a proper or improper list.
     ;;
     ;;Example of arguments in recursive calls:
     ;;
     ;;   (append '(1 2 3) '(4 5 6))
     ;;   (rev! '(3 2 1) '(4 5 6))
     ;;   (rev! '(2 1) '(3 4 5 6))
     ;;   (rev! '(1) '(2 3 4 5 6))
     ;;   (rev! '() '(1 2 3 4 5 6)) => '(1 2 3 4 5 6)
     ;;
     ;;Another example of arguments in recursive calls:
     ;;
     ;;   (append '(1 2 3) 4)
     ;;   (rev! '(3 2 1) 4)
     ;;   (rev! '(2 1) '(3 . 4))
     ;;   (rev! '(1) '(2 3 . 4))
     ;;   (rev! '() '(1 2 3 . 4)) => '(1 2 3 . 4)
     ;;
     (if (pair? ls)
	 ;;Here we recycle the first pair in LS  and make it become the first pair in
	 ;;ACCUM in the next recursive call.
	 (let ((D (cdr ls)))
	   (set-cdr! ls accum)
	   (rev! D ls))
       accum))

   (define (append1 ls ls*)
     ;;Upon entering  this recursive function:  LS is  a list; LS*  can be a  list of
     ;;lists but also a list ending with a non-list:
     ;;
     ;;   (append '(1 2) '(3) '(4))	--> LS* == ((3) (4))
     ;;   (append '(1 2) '(3) 4)	--> LS* == ((3) 4)
     ;;   (append '(1 2) 4)     	--> LS* == (4)
     ;;
     ;;after a recursion LS is an iterator over the  items in LS*, so it may not be a
     ;;list.  We can say nothing about the type of the return value of this function.
     ;;
     (if (pair? ls*)
	 (rev! (reverse ls ls ls '())
	       (append1 (car ls*) (cdr ls*)))
       ls))

   (append1 ls ls*))
  #| end of CASE-DEFINE* |# )


(case-define/typed append-lists
  ;;This is like APPEND  but it accepts only proper lists as  arguments and it always
  ;;returns a proper list.
  ;;
  (({_ <null>})
   '())

  (({_ <list>} {ls <list>})
   ls)

  (({_ <list>} {ls <list>} . {ls* (list-of <list>)})
   (define/typed ({reverse <list>} ls tortoise input-ls accum)
     ;;Reverse  LS and  return the  result.   Validate LS  as proper  list; raise  an
     ;;exception if it is circular or improper.
     ;;
     ;;The argument TORTOISE  is used only to  detect if LS is a  circular list.  The
     ;;argument INPUT-LS is used only as irritant when an exception is raised.
     ;;
     ;;Example of arguments in recursive calls, LS has an even number of items:
     ;;
     ;;   (append  '(1 2 3 4) '(5))
     ;;   (reverse '(1 2 3 4) '(1 2 3 4) '(1 2 3 4)            '())
     ;;   (reverse '(3 4)     '(2 3 4)   '(1 2 3 4)     '(2 1 . ()))
     ;;   (reverse '()        '(3 4)     '(1 2 3 4) '(4 3 2 1 . ())) => (4 3 2 1)
     ;;
     ;;Another  example of  arguments in  recursive calls,  LS has  an odd  number of
     ;;items:
     ;;
     ;;   (append  '(1 2 3) '(4))
     ;;   (reverse '(1 2 3) '(1 2 3) '(1 2 3)          '())
     ;;   (reverse '(3)     '(2 3)   '(1 2 3)   '(2 1 . ())) => (3 2 1)
     ;;
     (with-who append
       (cond ((pair? ls)
	      (let ((D (cdr ls))
		    (A (car ls)))
		(cond ((pair? D)
		       (if (eq? D tortoise)
			   (%error-circular-list-is-invalid-as-argument input-ls)
			 ;;Notice that here we perform 2  steps in LS and one step in
			 ;;TORTOISE.
			 (reverse (cdr D) ($cdr tortoise) input-ls (cons (car D) (cons A accum)))))
		      ((null? D)
		       (cons A accum))
		      (else
		       (%error-expected-proper-list-as-argument input-ls)))))
	     ((null? ls)
	      accum)
	     (else
	      (%error-expected-proper-list-as-argument input-ls)))))

   (define/typed ({rev! <list>} {ls <list>} {accum <list>})
     ;;ACCUM can be a proper or improper list.
     ;;
     ;;Example of arguments in recursive calls:
     ;;
     ;;   (append '(1 2 3) '(4 5 6))
     ;;   (rev! '(3 2 1) '(4 5 6))
     ;;   (rev! '(2 1) '(3 4 5 6))
     ;;   (rev! '(1) '(2 3 4 5 6))
     ;;   (rev! '() '(1 2 3 4 5 6)) => '(1 2 3 4 5 6)
     ;;
     ;;Another example of arguments in recursive calls:
     ;;
     ;;   (append '(1 2 3) 4)
     ;;   (rev! '(3 2 1) 4)
     ;;   (rev! '(2 1) '(3 . 4))
     ;;   (rev! '(1) '(2 3 . 4))
     ;;   (rev! '() '(1 2 3 . 4)) => '(1 2 3 . 4)
     ;;
     (if (pair? ls)
	 ;;Here we recycle the first pair in LS  and make it become the first pair in
	 ;;ACCUM in the next recursive call.
	 (let ((D (cdr ls)))
	   (set-cdr! ls accum)
	   (rev! D ls))
       accum))

   (define/typed ({append-lists1 <list>} {ls <list>} {ls* (list-of <list>)})
     ;;Upon entering  this recursive function:  LS is  a list; LS*  can be a  list of
     ;;lists but also a list ending with a non-list:
     ;;
     ;;   (append '(1 2) '(3) '(4))	--> LS* == ((3) (4))
     ;;   (append '(1 2) '(3) 4)	--> LS* == ((3) 4)
     ;;   (append '(1 2) 4)     	--> LS* == (4)
     ;;
     ;;after a recursion LS is an iterator over the  items in LS*, so it may not be a
     ;;list.  We can say nothing about the type of the return value of this function.
     ;;
     (if (pair? ls*)
	 (rev! (reverse ls ls ls '())
	       (append-lists1 (car ls*) (cdr ls*)))
       ls))

   (append-lists1 ls ls*))
  #| end of CASE-DEFINE* |# )


(define/typed ({reverse <list>} x)
  (define/typed ({%race <list>} hare tortoise ls ac)
    (with-who reverse
      (cond ((pair? hare)
	     (let ((hare  ($cdr hare))
		   (ac    (cons ($car hare) ac)))
	       (cond ((pair? hare)
		      (if (not (eq? hare tortoise))
			  (%race ($cdr hare) ($cdr tortoise) ls (cons ($car hare) ac))
			(%error-circular-list-is-invalid-as-argument ls)))
		     ((null? hare)
		      ac)
		     (else
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? hare)
	     ac)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race x x x '()))


(define (last-pair {ls <pair>})
  (define (%race hare tortoise input-ls last)
    (if (pair? hare)
	(let ((hare ($cdr hare))
	      (last hare))
	  (if (pair? hare)
	      (if (not (eq? hare tortoise))
		  (%race ($cdr hare) ($cdr tortoise) input-ls hare)
		(%error-circular-list-is-invalid-as-argument input-ls))
	    last))
      last))
  (let ((D ($cdr ls)))
    (%race D D ls ls)))


;;;; searching

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?equality-predicate)
		 (define (?who x ls)
		   (define (%race hare tortoise input-ls x)
		     (with-who ?who
		       (cond ((pair? hare)
			      (if (?equality-predicate ($car hare) x)
				  hare
				(let ((hare ($cdr hare)))
				  (cond ((pair? hare)
					 (cond ((?equality-predicate ($car hare) x)
						hare)
					       ((not (eq? hare tortoise))
						(%race ($cdr hare) ($cdr tortoise) input-ls x))
					       (else
						(%error-circular-list-is-invalid-as-argument input-ls))))
					((null? hare)
					 #f)
					(else
					 (%error-expected-proper-list-as-argument input-ls))))))
			     ((null? hare)
			      #f)
			     (else
			      (%error-expected-proper-list-as-argument input-ls)))))
		   (%race ls ls ls x))

		 )
		)))
  (declare memq		eq?)
  (declare memv		eqv?)
  (declare member	equal?)
  #| end of LET-SYNTAX |# )

(define (memp {pred (lambda (_) => (<top>))} ls)
  (define (%race hare tortoise input-ls pred)
    (with-who memp
      (cond ((pair? hare)
	     (if (pred ($car hare))
		 hare
	       (let ((hare ($cdr hare)))
		 (cond ((pair? hare)
			(cond ((pred ($car hare))
			       hare)
			      ((not (eq? hare tortoise))
			       (%race ($cdr hare) ($cdr tortoise) input-ls pred))
			      (else
			       (%error-circular-list-is-invalid-as-argument input-ls))))
		       ((null? hare)
			#f)
		       (else
			(%error-expected-proper-list-as-argument input-ls))))))
	    ((null? hare)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument input-ls)))))
  (%race ls ls ls pred))


(define (find {p (lambda (_) => (<top>))} ls)
  (define (%race hare tortoise input-ls p)
    (with-who find
      (cond ((pair? hare)
	     (let ((a ($car hare)))
	       (if (p a)
		   a
		 (let ((hare ($cdr hare)))
		   (cond ((pair? hare)
			  (let ((a ($car hare)))
			    (cond ((p a)
				   a)
				  ((not (eq? hare tortoise))
				   (%race ($cdr hare) ($cdr tortoise) input-ls p))
				  (else
				   (%error-circular-list-is-invalid-as-argument input-ls)))))
			 ((null? hare)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument input-ls)))))))
	    ((null? hare)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument input-ls)))))
  (%race ls ls ls p))


;;;; searching in alist

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?equal-pred)
		 (define (?who x ls)
		   (define (%race x hare tortoise input-ls)
		     (with-who ?who
		       (cond ((pair? hare)
			      (let ((a    ($car hare))
				    (hare ($cdr hare)))
				(if (pair? a)
				    (cond ((?equal-pred ($car a) x)
					   a)
					  ((pair? hare)
					   (if (not (eq? hare tortoise))
					       (let ((a ($car hare)))
						 (if (pair? a)
						     (if (?equal-pred ($car a) x)
							 a
						       (%race x ($cdr hare) ($cdr tortoise) input-ls))
						   (%error-malformed-alist-as-argument 2 input-ls)))
					     (%error-circular-list-is-invalid-as-argument input-ls)))
					  ((null? hare)
					   #f)
					  (else
					   (%error-expected-proper-list-as-argument input-ls)))
				  (%error-malformed-alist-as-argument 2 input-ls))))
			     ((null? hare)
			      #f)
			     (else
			      (%error-expected-proper-list-as-argument input-ls)))))
		   (%race x ls ls ls)))
		)))
  (declare assq		eq?)
  (declare assv		eqv?)
  (declare assoc	equal?)
  #| end of LET-SYNTAX |# )

(define (assp {proc (lambda (_) => (<top>))} ls)
  (define (%race proc hare tortoise input-ls)
    (with-who assp
      (cond ((pair? hare)
	     (let ((a    ($car hare))
		   (hare ($cdr hare)))
	       (if (pair? a)
		   (cond ((proc ($car a))
			  a)
			 ((pair? hare)
			  (if (not (eq? hare tortoise))
			      (let ((a ($car hare)))
				(if (pair? a)
				    (if (proc ($car a))
					a
				      (%race proc ($cdr hare) ($cdr tortoise) input-ls))
				  (%error-malformed-alist-as-argument 2 input-ls)))
			    (%error-circular-list-is-invalid-as-argument input-ls)))
			 ((null? hare)
			  #f)
			 (else
			  (%error-expected-proper-list-as-argument input-ls)))
		 (%error-malformed-alist-as-argument 2 input-ls))))
	    ((null? hare)
	     #f)
	    (else
	     (%error-expected-proper-list-as-argument input-ls)))))
  (%race proc ls ls ls))


;;;; removing from a list

(let-syntax
    ((declare (syntax-rules ()
		((_ ?name ?equal-pred ?type-ann)
		 (define (?name {x ?type-ann} ls)
		   (define (%race hare tortoise input-ls x)
		     (with-who ?name
		       (cond ((pair? hare)
			      (if (?equal-pred ($car hare) x)
				  (let ((hare ($cdr hare)))
				    (cond ((pair? hare)
					   (if (not (eq? hare tortoise))
					       (if (?equal-pred ($car hare) x)
						   (%race ($cdr hare) ($cdr tortoise) input-ls x)
						 (cons ($car hare) (%race ($cdr hare) ($cdr tortoise) input-ls x)))
					     (%error-circular-list-is-invalid-as-argument input-ls)))
					  ((null? hare)
					   '())
					  (else
					   (%error-expected-proper-list-as-argument input-ls))))
				(let ((a0 ($car hare)) (hare ($cdr hare)))
				  (cond ((pair? hare)
					 (if (not (eq? hare tortoise))
					     (if (?equal-pred ($car hare) x)
						 (cons a0 (%race ($cdr hare) ($cdr tortoise) input-ls x))
					       (cons* a0 ($car hare) (%race ($cdr hare) ($cdr tortoise) input-ls x)))
					   (%error-circular-list-is-invalid-as-argument input-ls)))
					((null? hare)
					 (list a0))
					(else
					 (%error-expected-proper-list-as-argument input-ls))))))
			     ((null? hare)
			      '())
			     (else
			      (%error-expected-proper-list-as-argument input-ls)))))
		   (%race ls ls ls x)))
		)))
  (declare remq		eq?				<top>)
  (declare remv		eqv?				<top>)
  (declare remove	equal?				<top>)
  (declare remp		(lambda (elt p) (p elt))		(lambda (_) => (<top>)))
  (declare filter	(lambda (elt p) (not (p elt)))	(lambda (_) => (<top>)))
  #| end of LET-SYNTAX |# )


(module (map)

  (define ({err-mutated . <bottom>} all-lists)
    (with-who map
      (%error-list-was-altered-while-processing)))

  (define ({err-mismatch . <bottom>} all-lists)
    (with-who map
      (%error-length-mismatch-among-list-arguments)))

  (define ({err-invalid . <bottom>} all-lists)
    (with-who map
      (apply assertion-violation __who__ "invalid arguments" all-lists)))

;;; --------------------------------------------------------------------

  (define/typed ({%map1 <list>} {f (lambda (_) => (<top>))} a d {n <list-length>})
    (with-who map
      (cond ((pair? d)
	     (if (zero-fixnum? n)
		 (%error-list-was-altered-while-processing)
	       (cons (f a) (%map1 f ($car d) ($cdr d) (sub1 n)))))
	    ((null? d)
	     (if (zero-fixnum? n)
		 (cons (f a) '())
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define/typed ({%map2 <list>} {f (lambda (_ _) => (<top>))} a1 a2 d1 d2 {n <list-length>})
    (with-who map
      (cond ((pair? d1)
	     (cond ((pair? d2)
		    (if (zero-fixnum? n)
			(%error-list-was-altered-while-processing)
		      (cons (f a1 a2)
			    (%map2 f
				  ($car d1) ($car d2)
				  ($cdr d1) ($cdr d2)
				  (sub1 n)))))
		   ((null? d2)
		    (%error-length-mismatch-among-list-arguments))
		   (else
		    (%error-expected-proper-list-as-argument d2))))
	    ((null? d1)
	     (cond ((null? d2)
		    (if (zero-fixnum? n)
			(cons (f a1 a2) '())
		      (%error-list-was-altered-while-processing)))
		   (else
		    (if (list? d2)
			(%error-length-mismatch-among-list-arguments)
		      (%error-expected-proper-list-as-argument d2)))))
	    (else
	     (%error-list-was-altered-while-processing)))))

;;; --------------------------------------------------------------------

  (module (%mapm)

    (define/typed ({cars <list>} {ls* (list-of <list>)})
      (with-who map
	(if (pair? ls*)
	    (let ((a (car ls*)))
	      (if (pair? a)
		  (cons (car a) (cars (cdr ls*)))
		(%error-length-mismatch-among-list-arguments)))
	  '())))

    (define/typed ({cdrs <list>} {ls* (list-of <list>)})
      (with-who map
	(if (pair? ls*)
	    (let ((a (car ls*)))
	      (if (pair? a)
		  (cons (cdr a) (cdrs (cdr ls*)))
		(%error-length-mismatch-among-list-arguments)))
	  '())))

    (define/typed ({%mapm <list>} {f (lambda <list> => (<top>))} ls {ls* (list-of <list>)} {n <list-length>} {all-lists (nelist-of <list>)})
      (cond ((null? ls)
	     (if (andmap null? ls*)
		 (if (zero-fixnum? n)
		     '()
		   (err-mutated all-lists))
	       (err-mismatch all-lists)))
	    ((zero-fixnum? n)
	     (err-mutated all-lists))
	    (else
	     (cons (apply f (car ls) (cars ls*))
		   (%mapm f (cdr ls) (cdrs ls*) (fxsub1 n) all-lists)))))

    #| end of module: %MAPM |# )

;;; --------------------------------------------------------------------

  (case-define map
    (({_ <list>} {f (lambda (_) => (<top>))} ls)
     (cond ((pair? ls)
	    (let ((D ($cdr ls)))
	      (%map1 f ($car ls) D (%length D D 0 __who__))))
	   ((null? ls)
	    '())
	   (else
	    (err-invalid (list ls)))))

    (({_ <list>} {f (lambda (_ _) => (<top>))} ls ls2)
     (cond ((pair? ls)
	    (if (pair? ls2)
		(let ((D ($cdr ls)))
		  (%map2 f ($car ls) ($car ls2) D ($cdr ls2) (%length D D 0 __who__)))
	      (err-invalid (list ls ls2))))
	   ((and (null? ls) (null? ls2))
	    '())
	   (else
	    (err-invalid (list ls ls2)))))

    (({_ <list>} {f (lambda <list> => (<top>))} ls . ls*)
     (cond ((pair? ls)
	    (let ((n (%length ls ls 0 __who__)))
	      (%mapm f ls ls* n (cons ls ls*))))
	   ((and (null? ls) (andmap null? ls*))
	    '())
	   (else
	    (err-invalid (cons ls ls*)))))

    #| end of CASE-DEFINE |# )

  #| end of module: MAP |# )


(module (for-each)

  (define/typed ({for-each1} {f (lambda (_) => <list>)} a d n)
    (with-who for-each
      (cond ((pair? d)
	     (if (zero-fixnum? n)
		 (%error-list-was-altered-while-processing)
	       (begin
		 (f a)
		 (for-each1 f ($car d) ($cdr d) (sub1 n)))))
	    ((null? d)
	     (if (zero-fixnum? n)
		 (begin
		   (f a)
		   (values))
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define/typed ({for-each2} {f (lambda (_ _) => <list>)} a1 a2 d1 d2 n)
    (with-who for-each
      (cond ((pair? d1)
	     (if (pair? d2)
		 (if (zero-fixnum? n)
		     (%error-list-was-altered-while-processing)
		   (begin
		     (f a1 a2)
		     (for-each2 f
				($car d1) ($car d2)
				($cdr d1) ($cdr d2)
				(sub1 n))))
	       (%error-length-mismatch-among-list-arguments)))
	    ((null? d1)
	     (if (null? d2)
		 (if (zero-fixnum? n)
		     (begin
		       (f a1 a2)
		       (values))
		   (%error-list-was-altered-while-processing))
	       (%error-length-mismatch-among-list-arguments)))
	    (else
	     (%error-list-was-altered-while-processing)))))

;;; --------------------------------------------------------------------

  (case-define for-each
    (({_} {f (lambda (_) => <list>)} ls)
     (cond ((pair? ls)
	    (let ((d ($cdr ls)))
	      (for-each1 f ($car ls) d (%length d d 0 __who__))))
	   ((null? ls)
	    (values))
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    (({_} {f (lambda (_ _) => <list>)} ls ls2)
     (cond ((pair? ls)
	    (if (pair? ls2)
		(let ((d ($cdr ls)))
		  (for-each2 f ($car ls) ($car ls2) d ($cdr ls2) (%length d d 0 __who__)))
	      (%error-length-mismatch-among-list-arguments)))
	   ((null? ls)
	    (if (null? ls2)
		(values)
	      (%error-length-mismatch-among-list-arguments)))
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    (({_} {f <procedure>} ls . ls*)
     (let ((n (%length ls ls 0 __who__)))
       (for-each (lambda (x)
		   (unless (and (list? x) (= (%length x x 0 __who__) n))
		     (%error-expected-proper-list-as-argument x)))
	 ls*)
       (let loop ((n	(%length ls ls 0 __who__))
		  (ls	ls)
		  (ls*	ls*))
	 (if (zero-fixnum? n)
	     (unless (and (null? ls) (andmap null? ls*))
	       (%error-list-was-altered-while-processing))
	   (begin
	     (unless (and (pair? ls) (andmap pair? ls*))
	       (%error-list-was-altered-while-processing))
	     (apply f (car ls) (map car ls*))
	     (loop (fx- n 1) (cdr ls) (map cdr ls*)))))))

    #| end of CASE-DEFINE* |# )

  #| end of module: FOR-EACH |#)


(module (andmap)

  (define/typed ({andmap1 <top>} {f (lambda (_) => (<top>))} a d n)
    (with-who andmap
      (cond ((pair? d)
	     (if (zero-fixnum? n)
		 (%error-list-was-altered-while-processing)
	       (and (f a)
		    (andmap1 f ($car d) ($cdr d) (sub1 n)))))
	    ((null? d)
	     (if (zero-fixnum? n)
		 (f a)
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

  (define/typed ({andmap2 <top>} {f (lambda (_ _) => (<top>))} a1 a2 d1 d2 n)
    (with-who andmap
      (cond ((pair? d1)
	     (if (pair? d2)
		 (if (zero-fixnum? n)
		     (%error-list-was-altered-while-processing)
		   (and (f a1 a2)
			(andmap2 f
				 ($car d1) ($car d2)
				 ($cdr d1) ($cdr d2)
				 (sub1 n))))
	       (%error-length-mismatch-among-list-arguments)))
	    ((null? d1)
	     (if (null? d2)
		 (if (zero-fixnum? n)
		     (f a1 a2)
		   (%error-list-was-altered-while-processing))
	       (%error-length-mismatch-among-list-arguments)))
	    (else
	     (%error-list-was-altered-while-processing)))))

;;; --------------------------------------------------------------------

  (case-define andmap
    ;;ANDMAP should be the same as R6RS's FOR-ALL (Marco Maggi; Oct 28, 2011).
    ;;
    (({f (lambda (_) => (<top>))} ls)
     (cond ((pair? ls)
	    (let ((d ($cdr ls)))
	      (andmap1 f ($car ls) d (%length d d 0 __who__))))
	   ((null? ls)
	    #t)
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    (({f (lambda (_ _) => (<top>))} ls ls2)
     (cond ((pair? ls)
	    (if (pair? ls2)
		(let ((d ($cdr ls)))
		  (andmap2 f
			   ($car ls) ($car ls2) d ($cdr ls2) (%length d d 0 __who__)))
	      (%error-length-mismatch-among-list-arguments)))
	   ((null? ls)
	    (if (null? ls2)
		#t
	      (%error-length-mismatch-among-list-arguments)))
	   (else
	    (%error-expected-proper-list-as-argument ls))))

    #| end of CASE-DEFINE |# )

  #| end of module |# )


(module (ormap)

  (define ({ormap1 <top>} {f (lambda (_) => (<top>))} a d n)
    (with-who ormap
      (cond ((pair? d)
	     (if (zero-fixnum? n)
		 (%error-list-was-altered-while-processing)
	       (or (f a)
		   (ormap1 f ($car d) ($cdr d) (sub1 n)))))
	    ((null? d)
	     (if (zero-fixnum? n)
		 (f a)
	       (%error-list-was-altered-while-processing)))
	    (else
	     (%error-list-was-altered-while-processing)))))

;;; --------------------------------------------------------------------

  (define (ormap {f (lambda (_) => (<top>))} ls)
    ;;ORMAP should be the same as R6RS's EXISTS (Marco Maggi; Oct 28, 2011).
    ;;
    (cond ((pair? ls)
	   (let ((d (cdr ls)))
	     (ormap1 f (car ls) d (%length d d 0 __who__))))
	  ((null? ls)
	   #f)
	  (else
	   (%error-expected-proper-list-as-argument ls))))

  #| end of module |# )


(define ({partition <list> <list>} {pred (lambda (_) => (<top>))} ls)
  (define/typed ({%race <list> <list>} hare tortoise ls {pred (lambda (_) => (<top>))})
    (with-who partition
      (cond ((pair? hare)
	     (let ((a0   ($car hare))
		   (hare ($cdr hare)))
	       (cond ((pair? hare)
		      (if (eq? hare tortoise)
			  (%error-circular-list-is-invalid-as-argument ls)
			(let ((a1 ($car hare)))
			  (receive (a* b*)
			      (%race ($cdr hare) ($cdr tortoise) ls pred)
			    (cond ((pred a0)
				   (if (pred a1)
				       (values (cons* a0 a1 a*) b*)
				     (values (cons a0 a*) (cons a1 b*))))
				  ((pred a1)
				   (values (cons a1 a*) (cons a0 b*)))
				  (else
				   (values a* (cons* a0 a1 b*))))))))
		     ((null? hare)
		      (if (pred a0)
			  (values (list a0) '())
			(values '() (list a0))))
		     (else
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? hare)
	     (values '() '()))
	    (else
	     (%error-expected-proper-list-as-argument ls)))))
  (%race ls ls ls pred))


(define-syntax define-iterator
  (syntax-rules ()
    ((_ ?who ?combine)
     (module (?who)

       (define/typed ({null*? <boolean>} ls*)
	 (if (pair? ls*)
	     (and (null?  (car ls*))
		  (null*? (cdr ls*)))
	   #t))

       (define/typed ({err* . <bottom>} ls*)
	 (with-who ?who
	   (for-each (lambda (ls)
		       (unless (list? ls)
			 (%error-expected-proper-list-as-argument ls)))
	     ls*)
	   (debug-print __who__ 'err*)
	   (%error-length-mismatch-among-list-arguments)))

       (define/typed ({cars+cdrs <list> <list>} ls ls*)
	 (with-who ?who
	   (if (pair? ls)
	       (let ((a (car ls)))
		 (cond ((pair? a)
			(receive (cars cdrs)
			    (cars+cdrs (cdr ls) (cdr ls*))
			  (values (cons (car a) cars)
				  (cons (cdr a) cdrs))))
		       ((list? (car ls*))
			(%error-length-mismatch-among-list-arguments))
		       (else
			(%error-expected-proper-list-as-argument (car ls*)))))
	     (values '() '()))))

       (define/typed ({loop1 <top>} {f (lambda (_) => (<top>))} a hare tortoise ls)
	 (with-who ?who
	   (cond ((pair? hare)
		  (let ((b (car hare))
			(hare (cdr hare)))
		    (?combine (f a)
			      (cond ((pair? hare)
				     (if (eq? hare tortoise)
					 (%error-circular-list-is-invalid-as-argument hare)
				       (let ((c (car hare))
					     (hare (cdr hare)))
					 (?combine (f b) (loop1 f c hare (cdr tortoise) ls)))))
				    ((null? hare)
				     (f b))
				    (else
				     (?combine (f b)
					       (%error-expected-proper-list-as-argument ls)))))))
		 ((null? hare)
		  (f a))
		 (else
		  (?combine (f a)
			    (%error-expected-proper-list-as-argument ls))))))

       (define/typed ({loopn <top>} {f (lambda <list> => (<top>))} a a* h h* t ls ls*)
	 (with-who ?who
	   (cond ((pair? h)
		  (receive (b* h*)
		      (cars+cdrs h* ls*)
		    (let ((b (car h))
			  (h (cdr h)))
		      (?combine (apply f a a*)
				(if (pair? h)
				    (if (eq? h t)
					(%error-circular-list-is-invalid-as-argument h)
				      (receive (c* h*)
					  (cars+cdrs h* ls*)
					(let ((c (car h))
					      (h (cdr h)))
					  (?combine (apply f b b*)
						    (loopn f c c* h h* (cdr t) ls ls*)))))
				  (if (and (null? h) (null*? h*))
				      (apply f b b*)
				    (?combine (apply f b b*)
					      (err* (cons ls ls*)))))))))
		 ((and (null? h)
		       (null*? h*))
		  (apply f a a*))
		 (else
		  (?combine (apply f a a*)
			    (err* (cons ls ls*)))))))

       (case-define ?who
	 (({_ <top>} {f (lambda (_) => (<top>))} ls)
	  (cond ((pair? ls)
		 (loop1 f (car ls) (cdr ls) (cdr ls) ls))
		((null? ls)
		 (?combine))
		(else
		 (%error-expected-proper-list-as-argument ls))))

	 (({_ <top>} {f (lambda <list> => (<top>))} ls . ls*)
	  (cond ((pair? ls)
		 (receive (cars cdrs)
		     (cars+cdrs ls* ls*)
		   (loopn f (car ls) cars (cdr ls) cdrs (cdr ls) ls ls*)))
		((and (null?  ls)
		      (null*? ls*))
		 (?combine))
		(else
		 (err* ls*))))
	 #| end of CASE-DEFINE |# )

       #| end of module: ?WHO |# )
     )))

(define-iterator for-all and)
(define-iterator exists  or)


(module (fold-left)

  (define/typed ({null*? <boolean>} ls*)
    (if (pair? ls*)
	(and (null?  (car ls*))
	     (null*? (cdr ls*)))
      #t))

  (define/typed ({err* . <bottom>} ls*)
    (with-who fold-left
      (cond ((null? ls*)
	     (%error-length-mismatch-among-list-arguments))
	    ((list? (car ls*))
	     (err* (cdr ls*)))
	    (else
	     (%error-expected-proper-list-as-argument (car ls*))))))

  (define/typed ({cars+cdrs <list> <list>} ls ls*)
    (with-who fold-left
      (if (pair? ls)
	  (let ((a (car ls)))
	    (cond ((pair? a)
		   (receive (cars cdrs)
		       (cars+cdrs (cdr ls) (cdr ls*))
		     (values (cons (car a) cars)
			     (cons (cdr a) cdrs))))
		  ((list? (car ls*))
		   (%error-length-mismatch-among-list-arguments))
		  (else
		   (%error-expected-proper-list-as-argument (car ls*)))))
	(values '() '()))))

  (define/typed ({loop1 <top>} {f (lambda (_ _) => (<top>))} nil h t ls)
    (with-who fold-left
      (cond ((pair? h)
	     (let ((a (car h))
		   (h (cdr h)))
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

  (define/typed ({loopn <top>} {f (lambda (_ _ . <list>) => (<top>))} nil h h* t ls ls*)
    (with-who fold-left
      (cond ((pair? h)
	     (receive (a* h*)
		 (cars+cdrs h* ls*)
	       (let ((a (car h))
		     (h (cdr h)))
		 (cond ((pair? h)
			(if (eq? h t)
			    (%error-circular-list-is-invalid-as-argument ls)
			  (receive (b* h*)
			      (cars+cdrs h* ls*)
			    (let ((b (car h))
				  (h (cdr h))
				  (t (cdr t)))
			      (loopn f
				     (apply f (apply f nil a a*) b b*)
				     h h* t ls ls*)))))
		       ((and (null?  h)
			     (null*? h*))
			(apply f nil a a*))
		       (else
			(err* (cons ls ls*)))))))
	    ((and (null?  h)
		  (null*? h*))
	     nil)
	    (else
	     (err* (cons ls ls*))))))

  (case-define fold-left
    (({_ <top>} {f (lambda (_ _) => (<top>))} nil ls)
     (loop1 f nil ls ls ls))
    (({_ <top>} {f (lambda (_ _ . <list>) => (<top>))} nil ls . ls*)
     (loopn f nil ls ls* ls ls ls*))
    #| end of CASE-DEFINE |# )

  #| end of module: FOLD-LEFT |# )


(module (fold-right)

  (define/typed ({null*? <boolean>} ls*)
    (if (pair? ls*)
	(and (null?  (car ls*))
	     (null*? (cdr ls*)))
      #t))

  (define/typed ({err* . <bottom>} ls*)
    (with-who fold-right
      (cond ((null? ls*)
	     (%error-length-mismatch-among-list-arguments))
	    ((list? (car ls*))
	     (err* (cdr ls*)))
	    (else
	     (%error-expected-proper-list-as-argument (car ls*))))))

  (define/typed ({cars+cdrs <list> <list>} ls ls*)
    (with-who fold-right
      (if (pair? ls)
	  (let ((a (car ls)))
	    (cond ((pair? a)
		   (receive (cars cdrs)
		       (cars+cdrs (cdr ls) (cdr ls*))
		     (values (cons (car a) cars)
			     (cons (cdr a) cdrs))))
		  ((list? (car ls*))
		   (%error-length-mismatch-among-list-arguments))
		  (else
		   (%error-expected-proper-list-as-argument (car ls*)))))
	(values '() '()))))

  (define/typed ({loop1 <top>} {f (lambda (_ _) => (<top>))} nil hare tortoise ls)
    (with-who fold-right
      (cond ((pair? hare)
	     (let ((a (car hare))
		   (hare (cdr hare)))
	       (cond ((pair? hare)
		      (if (eq? hare tortoise)
			  (%error-circular-list-is-invalid-as-argument ls)
			(let ((b (car hare))
			      (hare (cdr hare))
			      (tortoise (cdr tortoise)))
			  (f a (f b (loop1 f nil hare tortoise ls))))))
		     ((null? hare)
		      (f a nil))
		     (else
		      (%error-expected-proper-list-as-argument ls)))))
	    ((null? hare)
	     nil)
	    (else
	     (%error-expected-proper-list-as-argument ls)))))

  (define/typed ({loopn <top>} {f (lambda (_ _ . <list>) => (<top>))} nil h h* t ls ls*)
    (with-who fold-right
      (cond ((pair? h)
	     (receive (a* h*)
		 (cars+cdrs h* ls*)
	       (let ((a (car h))
		     (h (cdr h)))
		 (cond ((pair? h)
			(if (eq? h t)
			    (%error-circular-list-is-invalid-as-argument ls)
			  (receive (b* h*)
			      (cars+cdrs h* ls*)
			    (let ((b (car h))
				  (h (cdr h))
				  (t (cdr t)))
			      (apply f a (append-lists a* (list (apply f b (append-lists b* (list (loopn f nil h h* t ls ls*)))))))))))
		       ((and (null?  h)
			     (null*? h*))
			(apply f a (append-lists a* (list nil))))
		       (else
			(err* (cons ls ls*)))))))
	    ((and (null? h)
		  (null*? h*))
	     nil)
	    (else
	     (err* (cons ls ls*))))))

  (case-define fold-right
    (({_ <top>} {f (lambda (_ _) => (<top>))} nil ls)
     (loop1 f nil ls ls ls))
    (({_ <top>} {f (lambda (_ _ . <list>) => (<top>))} nil ls . ls*)
     (loopn f nil ls ls* ls ls ls*))
    #| end of CASE-DEFINE |# )

  #| end of module: FOLD-RIGHT |# )


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
	    ($set-cdr! (queue) new-last-pair)
	    (values))
	(let ((Q (list item)))
	  (queue (cons Q Q))
	  (values))))

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
