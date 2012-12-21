;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.


;;; SRFI-1 list-processing library
;;Reference implementation
;;
;;Copyright (c)  1998, 1999 by  Olin Shivers. You  may do as  you please
;;with this code as  long as you do not remove  this copyright notice or
;;hold   me  liable   for  its   use.   Please   send  bug   reports  to
;;shivers@ai.mit.edu.
;;
;;  -Olin


#!r6rs
(library (srfi :1 lists)
  (export

;;; defined by this library
    xcons		tree-copy		make-list
    list-tabulate	list-copy		iota
    circular-list

    proper-list?	circular-list?		dotted-list?
    not-pair?		null-list?		list=

    length+		count

    first		second			third
    fourth		fifth			sixth
    seventh		eighth			ninth
    tenth

    car+cdr

    take		drop
    take-right		drop-right
    take!		drop-right!
    split-at		split-at!
    last		last-pair
    zip
    unzip1		unzip2			unzip3
    unzip4		unzip5

    append!		append-reverse		append-reverse!
    concatenate		concatenate!

    unfold		fold			pair-fold
    reduce		unfold-right		pair-fold-right
    reduce-right
    append-map		append-map!		map!
    pair-for-each	filter-map		map-in-order
    filter!		partition!		remove!
    find-tail		any			every
    list-index

    take-while		drop-while		take-while!
    span		break
    span!		break!
    delete		delete!
    alist-cons		alist-copy
    delete-duplicates	delete-duplicates!
    alist-delete	alist-delete!

    reverse!
    lset<=		lset=			lset-adjoin
    lset-union		lset-intersection	lset-difference
    lset-xor		lset-diff+intersection
    lset-union!		lset-intersection!	lset-difference!
    lset-xor!		lset-diff+intersection!

;;; --------------------------------------------------------------------
;;; reexported from R6RS libraries

    append		assq			assv
    car			cdr
    caar		cadr
    cdar		cddr
    caaar		caadr			cadar
    caddr		cdaar			cdadr
    cddar		cdddr
    caaaar		caaadr			caadar
    caaddr		cadaar			cadadr
    caddar		cadddr			cdaaar
    cdaadr		cdadar			cdaddr
    cddaar		cddadr			cdddar
    cddddr
    cons		cons*
    length		reverse
    list		list-ref
    memq		memv
    null?		pair?
    set-car!		set-cdr!

;;; --------------------------------------------------------------------
;;; different from R6RS

    assoc		filter			find
    fold-right		for-each		map
    member		partition		remove)
  (import (except (rnrs)
		  assoc		filter
		  find		fold-right
		  for-each	map
		  member	partition
		  remove)
    (rnrs mutable-pairs)
    (srfi :8 receive)
    (vicare arguments validation)
    (ikarus system $pairs))


;;;; constructors

(define (xcons d a)
  ;;Occasionally  useful as  a value  to be  passed to  a fold  or other
  ;;higher-order procedure.
  (cons a d))

(define (tree-copy x)
  ;;Recursively copy every cons.
  ;;
  (let recur ((x x))
    (if (pair? x)
	(cons (recur ($car x))
	      (recur ($cdr x)))
      x)))

(define make-list
  ;;Make a list of length LEN.
  ;;
  (case-lambda
   ((len)
    (make-list len #f))
   ((len elt)
    (define who 'make-list)
    (with-arguments-validation (who)
	((non-negative-exact-integer	len))
      (do ((i len (- i 1))
	   (ans '() (cons elt ans)))
	  ((<= i 0)
	   ans))))))

(define (list-tabulate len proc)
  ;;Make a list of length LEN.  The element at position I is:
  ;;
  ;;   (PROC I)
  ;;
  ;;for 0 <= I < LEN.
  ;;
  (define who 'list-tabulate)
  (with-arguments-validation (who)
      ((non-negative-exact-integer	len)
       (procedure			proc))
    (do ((i (- len 1) (- i 1))
	 (ans '() (cons (proc i) ans)))
	((< i 0)
	 ans))))

(define (list-copy lis)
  (let recur ((lis lis))
    (if (pair? lis)
	(cons ($car lis) (recur ($cdr lis)))
      lis)))

(define iota
  (case-lambda
   ((count)
    (iota count 0 1))
   ((count start)
    (iota count start 1))
   ((count start step)
    (define who 'iota)
    (with-arguments-validation (who)
	((non-negative-exact-integer	count)
	 (non-negative-exact-integer	start)
	 (non-negative-exact-integer	step))
      (let loop ((n 0) (r '()))
	(if (= n count)
	    (reverse r)
	  (loop (+ 1 n)
		(cons (+ start (* n step)) r))))))))

(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    ($set-cdr! (last-pair ans) ans)
    ans))

(define (proper-list? x)
  ;;Definition of proper list:
  ;;
  ;; <proper-list> ::= ()			; Empty proper list
  ;;		  |   (cons <x> <proper-list>)	; Proper-list pair
  ;;
  ;;Note  that  this  definition  rules out  circular  lists,  and  this
  ;;function is required to detect this case and return false.
  ;;
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x ($cdr x)))
	  (if (pair? x)
	      (let ((x   ($cdr x))
		    (lag ($cdr lag)))
		(and (not (eq? x lag))
		     (lp x lag)))
	    (null? x)))
      (null? x))))

(define (dotted-list? x)
  ;;A dotted list is a finite  list (possibly of length 0) terminated by
  ;;a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5) is a
  ;;dotted list of length 0.
  ;;
  ;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
  ;;               |   (cons <x> <dotted-list>)	; Proper-list pair
  ;;
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x ($cdr x)))
	  (if (pair? x)
	      (let ((x   ($cdr x))
		    (lag ($cdr lag)))
		(and (not (eq? x lag))
		     (lp x lag)))
	    (not (null? x))))
      (not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x ($cdr x)))
	   (and (pair? x)
		(let ((x   ($cdr x))
		      (lag ($cdr lag)))
		  (or (eq? x lag)
		      (lp x lag))))))))

(define (not-pair? x)
  (not (pair? x)))

(define (null-list? l)
  (define who 'null-list?)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else
	 (error who "argument out of domain" l))))

(define (list= elt= . lists)
  (or (null? lists) ; special case
      (let lp1 ((list-a ($car lists))
		(others ($cdr lists)))
	(or (null? others)
	    (let ((list-b-orig ($car others))
		  (others      ($cdr others)))
	      (if (eq? list-a list-b-orig) ; EQ? => LIST=
		  (lp1 list-b-orig others)
		(let lp2 ((list-a list-a)
			  (list-b list-b-orig))
		  (if (null-list? list-a)
		      (and (null-list? list-b)
			   (lp1 list-b-orig others))
		    (and (not (null-list? list-b))
			 (elt= ($car list-a) ($car list-b))
			 (lp2  ($cdr list-a) ($cdr list-b)))))))))))


(define (length+ x) ; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x   ($cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   ($cdr x))
		    (lag ($cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag))
		     (lp x lag len)))
	    len))
      len)))

(define (zip list1 . more-lists)
  (apply map list list1 more-lists))


;;;; selectors

;;; R4RS non-primitives:
;(define (caar   x) (car (car x)))
;(define (cadr   x) (car (cdr x)))
;(define (cdar   x) (cdr (car x)))
;(define (cddr   x) (cdr (cdr x)))
;
;(define (caaar  x) (caar (car x)))
;(define (caadr  x) (caar (cdr x)))
;(define (cadar  x) (cadr (car x)))
;(define (caddr  x) (cadr (cdr x)))
;(define (cdaar  x) (cdar (car x)))
;(define (cdadr  x) (cdar (cdr x)))
;(define (cddar  x) (cddr (car x)))
;(define (cdddr  x) (cddr (cdr x)))
;
;(define (caaaar x) (caaar (car x)))
;(define (caaadr x) (caaar (cdr x)))
;(define (caadar x) (caadr (car x)))
;(define (caaddr x) (caadr (cdr x)))
;(define (cadaar x) (cadar (car x)))
;(define (cadadr x) (cadar (cdr x)))
;(define (caddar x) (caddr (car x)))
;(define (cadddr x) (caddr (cdr x)))
;(define (cdaaar x) (cdaar (car x)))
;(define (cdaadr x) (cdaar (cdr x)))
;(define (cdadar x) (cdadr (car x)))
;(define (cdaddr x) (cdadr (cdr x)))
;(define (cddaar x) (cddar (car x)))
;(define (cddadr x) (cddar (cdr x)))
;(define (cdddar x) (cdddr (car x)))
;(define (cddddr x) (cdddr (cdr x)))

(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (car+cdr pair) (values (car pair) (cdr pair)))


;;; take & drop

(define (take lis k)
  (define who 'take)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (let recur ((lis lis) (k k))
      (if (zero? k)
	  '()
	(cons (car lis)
	      (recur (cdr lis) (- k 1)))))))

(define (drop lis k)
  (define who 'drop)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (let iter ((lis lis) (k k))
      (if (zero? k)
	  lis
	(iter (cdr lis) (- k 1))))))

(define (take! lis k)
  (define who 'take!)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (if (zero? k) '()
      (begin
	(set-cdr! (drop lis (- k 1)) '())
	lis))))

;;TAKE-RIGHT and DROP-RIGHT work by  getting two pointers into the list,
;;off by K, then chasing down the  list until the lead pointer falls off
;;the end.

(define (take-right lis k)
  (define who 'take-right)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (let lp ((lag  lis)
	     (lead (drop lis k)))
      (if (pair? lead)
	  (lp ($cdr lag) ($cdr lead))
	lag))))

(define (drop-right lis k)
  (define who 'drop-right)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (let recur ((lag  lis)
		(lead (drop lis k)))
      (if (pair? lead)
	  (cons ($car lag)
		(recur ($cdr lag) ($cdr lead)))
	'()))))

(define (drop-right! lis k)
  ;;In this  function, LEAD is actually  K+1 ahead of LAG.  This lets us
  ;;stop LAG one step early, in time to smash its cdr to ().
  ;;
  (define who 'drop-right!)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (let ((lead (drop lis k)))
      (if (pair? lead)
	  (let lp ((lag  lis)
		   (lead ($cdr lead))) ; Standard case
	    (if (pair? lead)
		(lp ($cdr lag) ($cdr lead))
	      (begin
		($set-cdr! lag '())
		lis)))
	'())))) ; Special case dropping everything -- no cons to side-effect.

(define (split-at x k)
  (define who 'split-at)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (let recur ((lis x) (k k))
      (if (zero? k)
	  (values '() lis)
	(receive (prefix suffix)
	    (recur (cdr lis) (- k 1))
	  (values (cons (car lis) prefix) suffix))))))

(define (split-at! x k)
  (define who 'split-at!)
  (with-arguments-validation (who)
      ((exact-integer	k))
    (if (zero? k)
	(values '() x)
      (let* ((prev   (drop x (- k 1)))
	     (suffix (cdr prev)))
	(set-cdr! prev '())
	(values x suffix)))))

(define (last lis)
  (car (last-pair lis)))

(define (last-pair lis)
  (define who 'last-pair)
  (with-arguments-validation (who)
      ((pair	lis))
    (let lp ((lis lis))
      (let ((tail (cdr lis)))
	(if (pair? tail)
	    (lp tail)
	  lis)))))


;;;; unzippers

(define (unzip1 lis)
  (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis)
	(values lis lis) ;Use NOT-PAIR? to handle dotted lists.
      (let ((elt ($car lis)))
	(receive (a b)
	    (recur ($cdr lis))
	  (values (cons (car  elt) a)
		  (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis)
	(values lis lis lis)
      (let ((elt ($car lis)))
	(receive (a b c) (recur ($cdr lis))
	  (values (cons (car   elt) a)
		  (cons (cadr  elt) b)
		  (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis)
	(values lis lis lis lis)
	(let ((elt ($car lis)))
	  (receive (a b c d)
	      (recur ($cdr lis))
	    (values (cons (car    elt) a)
		    (cons (cadr   elt) b)
		    (cons (caddr  elt) c)
		    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis)
	(values lis lis lis lis lis)
	(let ((elt ($car lis)))
	  (receive (a b c d e)
	      (recur ($cdr lis))
	    (values (cons (car     elt) a)
		    (cons (cadr    elt) b)
		    (cons (caddr   elt) c)
		    (cons (cadddr  elt) d)
		    (cons (car (cddddr  elt)) e)))))))


;;;; append! append-reverse append-reverse! concatenate concatenate!

(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists)
	   (prev  '()))
    (if (not (pair? lists))
	prev
      (let ((first ($car lists))
	    (rest  ($cdr lists)))
	(if (not (pair? first))
	    (lp rest first)
	  ;;Now, do the splicing.
	  (let lp2 ((tail-cons (last-pair first))
		    (rest      rest))
	    (if (pair? rest)
		(let ((next ($car rest))
		      (rest ($cdr rest)))
		  ($set-cdr! tail-cons next)
		  (lp2 (if (pair? next)
			   (last-pair next)
			 tail-cons)
		       rest))
	      first)))))))

;;; Hand-inline the FOLD and PAIR-FOLD ops for speed.

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head)
	   (tail     tail))
    (if (null-list? rev-head)
	tail
      (lp ($cdr rev-head)
	  (cons ($car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head)
	   (tail     tail))
    (if (null-list? rev-head)
	tail
      (let ((next-rev ($cdr rev-head)))
	($set-cdr! rev-head tail)
	(lp next-rev rev-head)))))

(define (concatenate  lists)
  (reduce-right append  '() lists))

(define (concatenate! lists)
  (reduce-right append! '() lists))


;;;; fold/map internal utilities
;;
;;These little internal utilities are used  by the general fold & mapper
;;funs for the n-ary cases.  It'd be  nice if they got inlined.  One the
;;other hand,  the n-ary cases are  painfully inefficient as it  is.  An
;;aggressive implementation  should simply re-write these  functions for
;;raw efficiency; I have written  them for as much clarity, portability,
;;and simplicity as can be achieved.
;;
;;These functions have funky definitions that are precisely tuned to the
;;needs of the fold/map procs --  for example, to minimize the number of
;;times the argument lists need to be examined.

(define (%cdrs lists)
  ;;Return (map cdr lists).  However, if  any element of LISTS is empty,
  ;;just abort and return '().
  ;;
  (let f ((ls lists) (d* '()))
    (if (pair? ls)
	(let ((x ($car ls)))
	  (if (null-list? x)
	      '()
	    (f ($cdr ls) (cons (cdr x) d*))))
      (reverse d*))))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists)
	(cons (caar lists) (recur (cdr lists)))
      (list last-elt))))

(define (%cars+cdrs lists)
  ;;LISTS is  a (not  very long)  non-empty list  of lists.   Return two
  ;;lists: the  cars & the  cdrs of the lists.   However, if any  of the
  ;;lists is empty, just abort and return [() ()].
  ;;
  (let f ((ls lists) (a* '()) (d* '()))
    (if (pair? ls)
	(let ((x ($car ls)))
	  (if (null-list? x)
	      (values '() '())
	    (f ($cdr ls)
	       (cons (car x) a*)
	       (cons (cdr x) d*))))
      (values (reverse a*) (reverse d*)))))

(define (%cars+cdrs+ lists cars-final)
  ;;Like %CARS+CDRS, but we  pass in a final elt tacked  onto the end of
  ;;the cars list.  What a hack.
  ;;
  (let f ((ls lists) (a* '()) (d* '()))
    (if (pair? ls)
	(let ((x ($car ls)))
	  (if (null-list? x)
	      (values '() '())
	    (f ($cdr ls)
	       (cons (car x) a*)
	       (cons (cdr x) d*))))
      (values (reverse (cons cars-final a*))
	      (reverse d*)))))

(define (%cars+cdrs/no-test lists)
  ;;Like %CARS+CDRS, but blow up if any list is empty.
  ;;
  (let recur ((lists lists))
    (if (pair? lists)
	(receive (list other-lists)
	    (car+cdr lists)
	  (receive (a d)
	      (car+cdr list)
	    (receive (cars cdrs)
		(recur other-lists)
	      (values (cons a cars)
		      (cons d cdrs)))))
	(values '() '()))))


(define (count pred list1 . lists)
  (define who 'count)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (pair? lists)

	;; N-ary case
	(let lp ((list1 list1)
		 (lists lists)
		 (i     0))
	  (if (null-list? list1) i
	    (receive (as ds)
		(%cars+cdrs lists)
	      (if (null? as) i
		(lp (cdr list1) ds
		    (if (apply pred (car list1) as) (+ i 1) i))))))

      ;; Fast path
      (let lp ((lis list1)
	       (i   0))
	(if (null-list? lis)
	    i
	  (lp (cdr lis)
	      (if (pred (car lis))
		  (+ i 1)
		i)))))))


(define unfold-right
  (case-lambda
   ((p f g seed)
    (unfold-right p f g seed '()))
   ((p f g seed tail)
    (define who 'unfold-right)
    (with-arguments-validation (who)
	((procedure	p)
	 (procedure	f)
	 (procedure	g))
      (let lp ((seed seed)
	       (ans  tail))
	(if (p seed) ans
	  (lp (g seed)
	      (cons (f seed) ans))))))))

(define unfold
  (case-lambda

   ((stop? map-to-knil seed-step seed)
    (define who 'unfold)
    (with-arguments-validation (who)
	((procedure	stop?)
	 (procedure	map-to-knil)
	 (procedure	seed-step))
      (let loop ((seed seed))
	(if (stop? seed)
	    '()
	  (cons (map-to-knil seed)
		(loop (seed-step seed)))))))

   ((stop? map-to-knil seed-step seed tail-gen)
    (define who 'unfold)
    (with-arguments-validation (who)
	((procedure	stop?)
	 (procedure	map-to-knil)
	 (procedure	seed-step))
      (let loop ((seed seed))
	(if (stop? seed)
	    (tail-gen seed)
	  (cons (map-to-knil seed)
		(loop (seed-step seed)))))))))

(define (fold kons knil lis1 . lists)
  (define who 'fold)
  (with-arguments-validation (who)
      ((procedure	kons))
    (if (pair? lists)
	(let lp ((lists (cons lis1 lists))
		 (ans   knil)) ; N-ary case
	  (receive (cars+ans cdrs)
	      (%cars+cdrs+ lists ans)
	    (if (null? cars+ans) ans ; Done.
	      (lp cdrs (apply kons cars+ans)))))

      (let lp ((lis lis1) (ans knil)) ; Fast path
	(if (null-list? lis) ans
	  (lp (cdr lis) (kons (car lis) ans)))))))

(define (fold-right kons knil lis1 . lists)
  (define who 'fold-right)
  (with-arguments-validation (who)
      ((procedure	kons))
    (if (pair? lists)
	(let recur ((lists (cons lis1 lists))) ; N-ary case
	  (let ((cdrs (%cdrs lists)))
	    (if (null? cdrs) knil
	      (apply kons (%cars+ lists (recur cdrs))))))

      (let recur ((lis lis1)) ; Fast path
	(if (null-list? lis) knil
	  (let ((head (car lis)))
	    (kons head (recur (cdr lis)))))))))


(define (pair-fold-right f zero lis1 . lists)
  (define who 'pair-fold-right)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (pair? lists)
	(let recur ((lists (cons lis1 lists))) ; N-ary case
	  (let ((cdrs (%cdrs lists)))
	    (if (null? cdrs) zero
	      (apply f (append! lists (list (recur cdrs)))))))

      (let recur ((lis lis1)) ; Fast path
	(if (null-list? lis) zero (f lis (recur (cdr lis))))))))

(define (pair-fold f zero lis1 . lists)
  (define who 'pair-fold)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (pair? lists)
	(let lp ((lists (cons lis1 lists)) (ans zero)) ; N-ary case
	  (let ((tails (%cdrs lists)))
	    (if (null? tails) ans
	      (lp tails (apply f (append! lists (list ans)))))))

      (let lp ((lis lis1) (ans zero))
	(if (null-list? lis) ans
	  (let ((tail (cdr lis)))	 ; Grab the cdr now,
	    (lp tail (f lis ans))))))))	 ; in case F SET-CDR!s LIS.


;;REDUCE and  REDUCE-RIGHT only  use RIDENTITY  in the  empty-list case.
;;These cannot meaningfully be n-ary.

(define (reduce f ridentity lis)
  (define who 'reduce)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (null-list? lis)
	ridentity
      (fold f (car lis) (cdr lis)))))

(define (reduce-right f ridentity lis)
  (define who 'reduce-right)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (null-list? lis)
	ridentity
      (let recur ((head (car lis))
		  (lis  (cdr lis)))
	(if (pair? lis)
	    (f head (recur ($car lis) ($cdr lis)))
	  head)))))


;;;; mappers

(define (append-map f lis1 . lists)
  (%really-append-map append-map  append  f lis1 lists))

(define (append-map! f lis1 . lists)
  (%really-append-map append-map! append! f lis1 lists))

(define (%really-append-map who appender f lis1 lists)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (pair? lists)
	(receive (cars cdrs)
	    (%cars+cdrs (cons lis1 lists))
	  (if (null? cars) '()
	    (let recur ((cars cars)
			(cdrs cdrs))
	      (let ((vals (apply f cars)))
		(receive (cars2 cdrs2)
		    (%cars+cdrs cdrs)
		  (if (null? cars2)
		      vals
		    (appender vals (recur cars2 cdrs2))))))))

      ;; Fast path
      (if (null-list? lis1)
	  '()
	(let recur ((elt  (car lis1))
		    (rest (cdr lis1)))
	  (let ((vals (f elt)))
	    (if (null-list? rest)
		vals
	      (appender vals (recur (car rest) (cdr rest))))))))))

(define (pair-for-each proc lis1 . lists)
  (define who 'pair-for-each)
  (with-arguments-validation (who)
      ((procedure	proc))
    (if (pair? lists)

	(let lp ((lists (cons lis1 lists)))
	  (let ((tails (%cdrs lists)))
	    (if (pair? tails)
		(begin
		  (apply proc lists)
		  (lp tails)))))

      ;; Fast path.
      (let lp ((lis lis1))
	(if (not (null-list? lis))
	    (let ((tail (cdr lis))) ; Grab the cdr now,
	      (proc lis)	    ; in case PROC SET-CDR!s LIS.
	      (lp tail)))))))

(define (map! f lis1 . lists)
  ;;We stop when LIS1 runs out, not when any list runs out.
  ;;
  (define who 'map!)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (pair? lists)
	(let lp ((lis1 lis1) (lists lists))
	  (if (not (null-list? lis1))
	      (receive (heads tails)
		  (%cars+cdrs/no-test lists)
		(set-car! lis1 (apply f (car lis1) heads))
		(lp (cdr lis1) tails))))

      ;; Fast path.
      (pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
    lis1))

(define (filter-map f lis1 . lists)
  ;;Map F across L, and save up all the non-false results.
  ;;
  (define who 'filter-map)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (pair? lists)
	(let recur ((lists (cons lis1 lists)))
	  (receive (cars cdrs) (%cars+cdrs lists)
	    (if (pair? cars)
		(cond ((apply f cars)
		       => (lambda (x) (cons x (recur cdrs))))
		      (else
		       (recur cdrs))) ; Tail call in this arm.
	      '())))

      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	  (let ((tail (recur (cdr lis))))
	    (cond ((f (car lis))
		   => (lambda (x) (cons x tail)))
		  (else tail))))))))

(define (map-in-order f lis1 . lists)
  ;;Map F across lists, guaranteeing to go left-to-right.
  ;;
  ;;NOTE: Some implementations of R5RS MAP are compliant with this spec;
  ;;in which case this procedure may  simply be defined as a synonym for
  ;;MAP.
  ;;
  (define who 'map-in-order)
  (with-arguments-validation (who)
      ((procedure	f))
    (if (pair? lists)
	(let recur ((lists (cons lis1 lists)))
	  (receive (cars cdrs)
	      (%cars+cdrs lists)
	    (if (pair? cars)
		(let ((x (apply f cars))) ; Do head first,
		  (cons x (recur cdrs)))  ; then tail.
	      '())))

      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	  (let ((tail (cdr lis))
		(x (f (car lis))))	 ; Do head first,
	    (cons x (recur tail))))))))	 ; then tail.

;; We extend MAP to handle arguments of unequal length.
(define map map-in-order)

(define (for-each f lis1 . lists)
  ;;Contributed  by  Michael  Sperber  since it  was  missing  from  the
  ;;reference implementation.
  ;;
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
		 (if (pair? cars)
		     (begin
		       (apply f cars)	; Do head first,
		       (recur cdrs)))))	; then tail.

      ;; Fast path.
      (let recur ((lis lis1))
	(if (not (null-list? lis))
	    (begin
	      (f (car lis))		; Do head first,
	      (recur (cdr lis)))))))	; then tail.


;;;; filter, remove, partition
;;
;;FILTER, REMOVE,  PARTITION and  their destructive counterparts  do not
;;disorder the elements of their argument.
;;

(define (filter pred lis)
  ;;This  FILTER shares  the  longest  tail of  L  that  has no  deleted
  ;;elements.   If Scheme  had multi-continuation  calls, they  could be
  ;;made more efficient.
  ;;
  ;;Sleazing with EQ? makes this one faster.
  ;;
  (define who 'filter)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let recur ((lis lis))
      (if (null-list? lis)
	  lis ;Use NOT-PAIR? to handle dotted lists.
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail))) ; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		  (cons head new-tail)))
	    (recur tail))))))) ; this one can be a tail call.

(define (filter! pred lis)
  ;;This implementation of FILTER!
  ;;
  ;;- doesn't cons, and uses no stack;
  ;;
  ;;- is  careful not  to do  redundant  SET-CDR! writes,  as writes  to
  ;;  memory are  usually  expensive  on modern  machines,  and can  be
  ;;  extremely  expensive on  modern  Schemes  (e.g., ones  that  have
  ;;  generational GC's).
  ;;
  ;;It just zips  down contiguous runs of  in and out elts  in LIS doing
  ;;the minimal number of SET-CDR!s to splice the tail of one run of ins
  ;;to the beginning of the next.
  ;;
  (define who 'filter!)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let lp ((ans lis))
      (cond ((null-list? ans)
	     ans)	  ; Scan looking for

	    ((not (pred (car ans)))
	     (lp (cdr ans))) ; first cons of result.

	    ;; ANS is the eventual answer.
	    ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	    ;;          Scan over a contiguous segment of the list that
	    ;;          satisfies PRED.
	    ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	    ;;           segment of the list that *doesn't* satisfy PRED.
	    ;;           When the segment ends, patch in a link from PREV
	    ;;           to the start of the next good segment, and jump to
	    ;;           SCAN-IN.
	    (else (letrec ((scan-in (lambda (prev lis)
				      (if (pair? lis)
					  (if (pred ($car lis))
					      (scan-in lis ($cdr lis))
					    (scan-out prev ($cdr lis))))))
			   (scan-out (lambda (prev lis)
				       (let lp ((lis lis))
					 (if (pair? lis)
					     (if (pred ($car lis))
						 (begin
						   (set-cdr! prev lis)
						   (scan-in lis ($cdr lis)))
					       (lp ($cdr lis)))
					   (set-cdr! prev lis))))))
		    (scan-in ans (cdr ans))
		    ans))))))

;;Answers share  common tail with  LIS where possible; the  technique is
;;slightly subtle.

(define (partition pred lis)
  (define who 'partition)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let recur ((lis lis))
      (if (null-list? lis)
	  (values lis lis) ; Use NOT-PAIR? to handle dotted lists.
	(let ((elt  (car lis))
	      (tail (cdr lis)))
	  (receive (in out)
	      (recur tail)
	    (if (pred elt)
		(values (if (pair? out)
			    (cons elt in)
			  lis)
			out)
	      (values in
		      (if (pair? in)
			  (cons elt out)
			lis)))))))))

(define (partition! pred lis)
  ;;This implementation of PARTITION!
  ;;
  ;;- doesn't cons, and uses no stack;
  ;;
  ;;-  is careful  not to  do redundant  SET-CDR! writes,  as writes  to
  ;;   memory are  usually  expensive  on modern  machines,  and can  be
  ;;   extremely  expensive on  modern  Schemes  (e.g., ones  that  have
  ;;  generational GC's).
  ;;
  ;;It just zips  down contiguous runs of  in and out elts  in LIS doing
  ;;the minimal number  of SET-CDR!s to splice these  runs together into
  ;;the result lists.
  ;;
  (define who 'partition!)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (null-list? lis)
	(values lis lis)

      ;; This pair of loops zips down contiguous in & out runs of the
      ;; list, splicing the runs together. The invariants are
      ;;   SCAN-IN:  (cdr in-prev)  = LIS.
      ;;   SCAN-OUT: (cdr out-prev) = LIS.
      (letrec ((scan-in (lambda (in-prev out-prev lis)
			  (let lp ((in-prev in-prev)
				   (lis     lis))
			    (if (pair? lis)
				(if (pred ($car lis))
				    (lp lis ($cdr lis))
				  (begin
				    (set-cdr! out-prev lis)
				    (scan-out in-prev lis ($cdr lis))))
			      (set-cdr! out-prev lis))))) ; Done.

	       (scan-out (lambda (in-prev out-prev lis)
			   (let lp ((out-prev out-prev)
				    (lis      lis))
			     (if (pair? lis)
				 (if (pred ($car lis))
				     (begin
				       (set-cdr! in-prev lis)
				       (scan-in lis out-prev ($cdr lis)))
				   (lp lis ($cdr lis)))
			       (set-cdr! in-prev lis)))))) ; Done.

	;; Crank up the scan&splice loops.
	(if (pred (car lis))
	    ;; LIS begins in-list. Search for out-list's first pair.
	    (let lp ((prev-l lis)
		     (l      (cdr lis)))
	      (cond ((not (pair? l))
		     (values lis l))
		    ((pred (car l))
		     (lp l (cdr l)))
		    (else
		     (scan-out prev-l l (cdr l))
		     (values lis l)))) ; Done.

	  ;; LIS begins out-list. Search for in-list's first pair.
	  (let lp ((prev-l lis)
		   (l      (cdr lis)))
	    (cond ((not (pair? l))
		   (values l lis))
		  ((pred (car l))
		   (scan-in l prev-l (cdr l))
		   (values l lis)) ; Done.
		  (else
		   (lp l (cdr l))))))))))

(define (remove  pred l)
  (filter  (lambda (x) (not (pred x))) l))

(define (remove! pred l)
  (filter! (lambda (x) (not (pred x))) l))


;;Here's the taxonomy for the DELETE/ASSOC/MEMBER functions.
;;
;;(I don't actually think these are the world's most important functions
;;-- the  procedural FILTER/REMOVE/FIND/FIND-TAIL variants are  far more
;;general.)
;;
;;Function			Action
;;---------------------------------------------------------------------------
;;remove pred lis		Delete by general predicate
;;delete x lis [=]		Delete by element comparison
;;
;;find pred lis			Search by general predicate
;;find-tail pred lis		Search by general predicate
;;member x lis [=]		Search by element comparison
;;
;;assoc key lis [=]		Search alist by key comparison
;;alist-delete key alist [=]	Alist-delete by key comparison

(define delete
  (case-lambda
   ((x lis)
    (delete x lis equal?))
   ((x lis =)
    (filter (lambda (y) (not (= x y))) lis))))

(define delete!
  (case-lambda
   ((x lis)
    (delete! x lis equal?))
   ((x lis =)
    (filter! (lambda (y) (not (= x y))) lis))))

(define member
  ;;Extended from R4RS to take an optional comparison argument.
  ;;
  (case-lambda
   ((x lis)
    (member x lis equal?))
   ((x lis =)
    (find-tail (lambda (y) (= x y)) lis))))


;;;; right-duplicate deletion
;;
;;Beware -- these  are N^2 algorithms. To  efficiently remove duplicates
;;in long lists, sort the list  to bring duplicates together, then use a
;;linear-time algorithm to  kill the dups. Or use an  algorithm based on
;;element-marking. The former gives you O(n lg n), the latter is linear.

(define delete-duplicates
  (case-lambda
   ((lis)
    (delete-duplicates lis equal?))
   ((lis elt=)
    (define who 'delete-duplicates)
    (with-arguments-validation (who)
	((procedure	elt=))
      (let recur ((lis lis))
	(if (null-list? lis)
	    lis
	  (let* ((x		(car lis))
		 (tail		(cdr lis))
		 (new-tail	(recur (delete x tail elt=))))
	    (if (eq? tail new-tail)
		lis
	      (cons x new-tail)))))))))

(define delete-duplicates!
  (case-lambda
   ((lis)
    (delete-duplicates! lis equal?))
   ((lis elt=)
    (define who 'fold-right)
    (with-arguments-validation (who)
	((procedure	elt=))
      (let recur ((lis lis))
	(if (null-list? lis)
	    lis
	  (let* ((x		(car lis))
		 (tail		(cdr lis))
		 (new-tail	(recur (delete! x tail elt=))))
	    (if (not (eq? tail new-tail))
		(set-cdr! lis new-tail))
	    lis)))))))


;;;; alist stuff

(define assoc
  ;;Extended from R4RS to take an optional comparison argument.
  ;;
  (case-lambda
   ((x lis)
    (assoc x lis equal?))
   ((x lis =)
    (find (lambda (entry)
	    (= x (car entry)))
      lis))))

(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define (alist-copy alist)
  (map (lambda (elt)
	 (cons (car elt) (cdr elt)))
    alist))

(define alist-delete
  (case-lambda
   ((key alist)
    (alist-delete key alist equal?))
   ((key alist =)
    (filter (lambda (elt)
	      (not (= key (car elt))))
      alist))))

(define alist-delete!
  (case-lambda
   ((key alist)
    (alist-delete! key alist equal?))
   ((key alist =)
    (filter! (lambda (elt)
	       (not (= key (car elt))))
	     alist))))


;;;; find find-tail take-while drop-while span break any every list-index

(define (find pred list)
  (cond ((find-tail pred list)
	 => car)
	(else #f)))

(define (find-tail pred list)
  (define who 'find-tail)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let lp ((list list))
      (and (not (null-list? list))
	   (if (pred (car list))
	       list
	     (lp (cdr list)))))))

(define (take-while pred lis)
  (define who 'take-while)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let recur ((lis lis))
      (if (null-list? lis)
	  '()
	(let ((x (car lis)))
	  (if (pred x)
	      (cons x (recur (cdr lis)))
	    '()))))))

(define (drop-while pred lis)
  (define who 'drop-while)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let lp ((lis lis))
      (if (null-list? lis)
	  '()
	(if (pred (car lis))
	    (lp (cdr lis))
	  lis)))))

(define (take-while! pred lis)
  (define who 'take-while!)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (or (null-list? lis)
	    (not (pred (car lis))))
	'()
      (begin
	(let lp ((prev lis)
		 (rest (cdr lis)))
	  (if (pair? rest)
	      (let ((x (car rest)))
		(if (pred x)
		    (lp rest (cdr rest))
		  (set-cdr! prev '())))))
	lis))))

(define (span pred lis)
  (define who 'span)
  (with-arguments-validation (who)
      ((procedure	pred))
    (let recur ((lis lis))
      (if (null-list? lis)
	  (values '() '())
	(let ((x (car lis)))
	  (if (pred x)
	      (receive (prefix suffix)
		  (recur (cdr lis))
		(values (cons x prefix) suffix))
	    (values '() lis)))))))

(define (span! pred lis)
  (define who 'span!)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (or (null-list? lis)
	    (not (pred (car lis))))
	(values '() lis)
      (let ((suffix (let lp ((prev lis)
			     (rest (cdr lis)))
		      (if (null-list? rest)
			  rest
			(let ((x (car rest)))
			  (if (pred x)
			      (lp rest (cdr rest))
			    (begin
			      (set-cdr! prev '())
			      rest)))))))
	(values lis suffix)))))

(define (break  pred lis)
  (span  (lambda (x) (not (pred x))) lis))

(define (break! pred lis)
  (span! (lambda (x) (not (pred x))) lis))

(define (any pred lis1 . lists)
  (define who 'any)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (pair? lists)

	;; N-ary case
	(receive (heads tails)
	    (%cars+cdrs (cons lis1 lists))
	  (and (pair? heads)
	       (let lp ((heads heads)
			(tails tails))
		 (receive (next-heads next-tails)
		     (%cars+cdrs tails)
		   (if (pair? next-heads)
		       (or (apply pred heads)
			   (lp next-heads next-tails))
		     (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (and (not (null-list? lis1))
	   (let lp ((head (car lis1))
		    (tail (cdr lis1)))
	     (if (null-list? tail)
		 (pred head) ; Last PRED app is tail call.
	       (or (pred head)
		   (lp (car tail)
		       (cdr tail)))))))))

(define (every pred lis1 . lists)
  (define who 'every)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (pair? lists)

	;; N-ary case
	(receive (heads tails)
	    (%cars+cdrs (cons lis1 lists))
	  (or (not (pair? heads))
	      (let lp ((heads heads)
		       (tails tails))
		(receive (next-heads next-tails)
		    (%cars+cdrs tails)
		  (if (pair? next-heads)
		      (and (apply pred heads)
			   (lp next-heads next-tails))
		    (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (or (null-list? lis1)
	  (let lp ((head (car lis1))  (tail (cdr lis1)))
	    (if (null-list? tail)
		(pred head) ; Last PRED app is tail call.
	      (and (pred head) (lp (car tail) (cdr tail)))))))))

(define (list-index pred lis1 . lists)
  (define who 'list-index)
  (with-arguments-validation (who)
      ((procedure	pred))
    (if (pair? lists)

	;; N-ary case
	(let lp ((lists (cons lis1 lists)) (n 0))
	  (receive (heads tails)
	      (%cars+cdrs lists)
	    (and (pair? heads)
		 (if (apply pred heads)
		     n
		   (lp tails (+ n 1))))))

      ;; Fast path
      (let lp ((lis lis1) (n 0))
	(and (not (null-list? lis))
	     (if (pred (car lis))
		 n
	       (lp (cdr lis) (+ n 1))))))))


;;;; reverse

(define (reverse! lis)
  (let lp ((lis lis)
	   (ans '()))
    (if (null-list? lis)
	ans
      (let ((tail (cdr lis)))
	(set-cdr! lis ans)
	(lp tail lis)))))


;;;; lists-as-sets
;;
;;This is carefully tuned code; do not modify casually.
;;
;;* It is careful to share storage when possible;
;;
;;* Side-effecting code tries not to perform redundant writes.
;;
;;* It  tries  to  avoid  linear-time   scans  in  special  cases  where
;;  constant-time computations can be performed.
;;
;;* It relies  on similar  properties from the  other list-lib  procs it
;;  calls.  For  example, it uses  the fact that the  implementations of
;;  MEMBER and  FILTER in  this source code  share longest  common tails
;;  between  args and  results  to  get structure  sharing  in the  lset
;;  procedures.

(define (%lset2<= = lis1 lis2)
  (every (lambda (x) (member x lis2 =)) lis1))

(define (lset<= = . lists)
  (define who 'lset<=)
  (with-arguments-validation (who)
      ((procedure	=))
    (or (not (pair? lists)) ; 0-ary case
	(let lp ((s1   ($car lists))
		 (rest ($cdr lists)))
	  (or (not (pair? rest))
	      (let ((s2   ($car rest))
		    (rest ($cdr rest)))
		(and (or (eq? s2 s1)	   ; Fast path
			 (%lset2<= = s1 s2)) ; Real test
		     (lp s2 rest))))))))

(define (lset= = . lists)
  (define who 'lset=)
  (with-arguments-validation (who)
      ((procedure	=))
    (or (not (pair? lists)) ; 0-ary case
	(let lp ((s1   ($car lists))
		 (rest ($cdr lists)))
	  (or (not (pair? rest))
	      (let ((s2   ($car rest))
		    (rest ($cdr rest)))
		(and (or (eq? s1 s2) ; Fast path
			 (and (%lset2<= = s1 s2)
			      (%lset2<= = s2 s1))) ; Real test
		     (lp s2 rest))))))))

(define (lset-adjoin = lis . elts)
  (define who 'lset-adjoin)
  (with-arguments-validation (who)
      ((procedure	=))
    (fold (lambda (elt ans)
	    (if (member elt ans =)
		ans
	      (cons elt ans)))
	  lis elts)))

(define (lset-union = . lists)
  (define who 'lset-union)
  (with-arguments-validation (who)
      ((procedure	=))
    (reduce (lambda (lis ans)	    ; Compute ANS + LIS.
	      (cond ((null? lis) ans) ; Don't copy any lists
		    ((null? ans) lis) ; if we don't have to.
		    ((eq? lis ans) ans)
		    (else
		     (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
					    ans
					  (cons elt ans)))
			   ans lis))))
	    '() lists)))

(define (lset-union! = . lists)
  (define who 'lset-union!)
  (with-arguments-validation (who)
      ((procedure	=))
    (reduce (lambda (lis ans) ; Splice new elts of LIS onto the front of ANS.
	      (cond ((null? lis) ans) ; Don't copy any lists
		    ((null? ans) lis) ; if we don't have to.
		    ((eq? lis ans) ans)
		    (else
		     (pair-fold (lambda (pair ans)
				  (let ((elt (car pair)))
				    (if (any (lambda (x) (= x elt)) ans)
					ans
				      (begin (set-cdr! pair ans) pair))))
				ans lis))))
	    '() lists)))

(define (lset-intersection = lis1 . lists)
  (define who 'lset-intersection)
  (with-arguments-validation (who)
      ((procedure	=))
    (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
      (cond ((any null-list? lists) '())   ; Short cut
	    ((null? lists)          lis1)  ; Short cut
	    (else (filter (lambda (x)
			    (every (lambda (lis) (member x lis =)) lists))
		    lis1))))))

(define (lset-intersection! = lis1 . lists)
  (define who 'lset-intersection!)
  (with-arguments-validation (who)
      ((procedure	=))
    (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
      (cond ((any null-list? lists) '())   ; Short cut
	    ((null? lists)          lis1)  ; Short cut
	    (else (filter! (lambda (x)
			     (every (lambda (lis) (member x lis =)) lists))
			   lis1))))))

(define (lset-difference = lis1 . lists)
  (define who 'lset-difference)
  (with-arguments-validation (who)
      ((procedure	=))
    (let ((lists (filter pair? lists)))	; Throw out empty lists.
      (cond ((null? lists)     lis1)	; Short cut
	    ((memq lis1 lists) '())	; Short cut
	    (else (filter (lambda (x)
			    (every (lambda (lis) (not (member x lis =)))
			      lists))
		    lis1))))))

(define (lset-difference! = lis1 . lists)
  (define who 'lset-difference!)
  (with-arguments-validation (who)
      ((procedure	=))
    (let ((lists (filter pair? lists)))	; Throw out empty lists.
      (cond ((null? lists)     lis1)	; Short cut
	    ((memq lis1 lists) '())	; Short cut
	    (else (filter! (lambda (x)
			     (every (lambda (lis) (not (member x lis =)))
			       lists))
			   lis1))))))

(define (lset-xor = . lists)
  (define who 'lset-xor)
  (with-arguments-validation (who)
      ((procedure	=))
    (reduce (lambda (b a) ; Compute A xor B:
	      ;; Note that this code relies on the constant-time
	      ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	      ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	      ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	      ;; a careful case analysis to see it, but it's carefully
	      ;; built in.

	      ;; Compute a-b and a^b, then compute b-(a^b) and
	      ;; cons it onto the front of a-b.
	      (receive (a-b a-int-b)   (lset-diff+intersection = a b)
		(cond ((null? a-b)     (lset-difference = b a))
		      ((null? a-int-b) (append b a))
		      (else (fold (lambda (xb ans)
				    (if (member xb a-int-b =) ans (cons xb ans)))
				  a-b
				  b)))))
	    '() lists)))

(define (lset-xor! = . lists)
  (define who 'lset-xor!)
  (with-arguments-validation (who)
      ((procedure	=))
    (reduce (lambda (b a) ; Compute A xor B:
	      ;; Note that this code relies on the constant-time
	      ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	      ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	      ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	      ;; a careful case analysis to see it, but it's carefully
	      ;; built in.

	      ;; Compute a-b and a^b, then compute b-(a^b) and
	      ;; cons it onto the front of a-b.
	      (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
		(cond ((null? a-b)     (lset-difference! = b a))
		      ((null? a-int-b) (append! b a))
		      (else (pair-fold (lambda (b-pair ans)
					 (if (member (car b-pair) a-int-b =) ans
					   (begin (set-cdr! b-pair ans) b-pair)))
				       a-b
				       b)))))
	    '() lists)))

(define (lset-diff+intersection = lis1 . lists)
  (define who 'lset-diff+intersection)
  (with-arguments-validation (who)
      ((procedure	=))
    (cond ((every null-list? lists) (values lis1 '())) ; Short cut
	  ((memq lis1 lists)        (values '() lis1)) ; Short cut
	  (else (partition (lambda (elt)
			     (not (any (lambda (lis) (member elt lis =))
				    lists)))
		  lis1)))))

(define (lset-diff+intersection! = lis1 . lists)
  (define who 'lset-diff+intersection!)
  (with-arguments-validation (who)
      ((procedure	=))
    (cond ((every null-list? lists) (values lis1 '())) ; Short cut
	  ((memq lis1 lists)        (values '() lis1)) ; Short cut
	  (else (partition! (lambda (elt)
			      (not (any (lambda (lis) (member elt lis =))
				     lists)))
			    lis1)))))


;;;; done

)

;;; end of file
