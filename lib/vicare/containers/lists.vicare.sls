;;;Derived from SRFI-1 list-processing library, reference implementation
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 1998, 1999 by Olin Shivers <shivers@ai.mit.edu>.
;;;Modified by Abdulaziz Ghuloum to port to Ikarus.
;;;Modified by Derick Eddington to port to R6RS.
;;;Modified by Marco Maggi for inclusion in Nausicaa.
;;;
;;;You may do as you please with  this code as long as you do not remove
;;;this copyright notice or hold me liable for its use.  Please send bug
;;;reports to <shivers@ai.mit.edu>.
;;;
;;;This is  a library of list-processing  and pair-processing functions.
;;;I wrote it after carefully  considering the functions provided by the
;;;libraries  found in  R4RS/R5RS Scheme,  MIT Scheme,  Gambit, RScheme,
;;;MzScheme,  slib, Common  Lisp,  Bigloo,  guile, T,  APL  and the  SML
;;;standard basis.  It is a pretty rich toolkit, providing a superset of
;;;the functionality found in any of the various Schemes I considered.
;;;
;;;This   implementation   is   intended   as   a   portable   reference
;;;implementation  for SRFI-1.   See the  porting notes  below  for more
;;;information.
;;;


#!r6rs
(library (vicare containers lists)
  (export

    ;; constructors
    xcons
    make-list
    list-copy			tree-copy
    list-tabulate		list-tabulate/reverse
    iota

    ;; circular lists
    circular-list		list->circular-list!
    circular-list->list!	circular-list-copy
    circular-list-length	circular-list=

    ;; predicates
    circular-list?		circular-list?/or-null
    dotted-list?		dotted-list?/or-null
    not-pair?
    and-null?			or-null?
    (rename (%and/or-null? and/or-null?))

    ;; comparison
    list=?

    ;; selectors
    car+cdr
    first		second			third
    fourth		fifth			sixth
    seventh		eighth			ninth
    tenth

    take-left		take-right		take-left!
    drop-left		drop-right		drop-right!
    split-at		split-at!
    last		last-pair

    ;; misc
    length+
    append!
    concatenate		concatenate!
    reverse!
    append-reverse	append-reverse!
    zip			zip*
    unzip1		unzip2			unzip3
    unzip4		unzip5
    count

    ;; fold
    fold		fold*
    fold-left*		fold-right*
    and-fold-left*	and-fold-right*
    fold-left/pred
    pair-fold		pair-fold*
    reduce		reduce*
    unfold		unfold-right

    ;; map
			map*
			map-in-order*
    map!		map*!
    for-each*
    append-map		append-map!
    pair-for-each	pair-for-each*
    filter-map		filter-map*

    ;; filtering
    filter!		partition!
    remove*		remove*!

    ;;searching
    find-tail
    take-while		take-while!
    drop-while
    span		span!
    break		break!
    any			any*
    every		every*
    list-index		list-index*
    member*
    position

    ;; deletion
    delete		delete!
    delete-duplicates	delete-duplicates!

    ;; sorted lists
    sorted-list-insert		sorted-list-insert/uniq
    union-of-sorted-lists	union-of-sorted-lists/uniq

    ;; alists
    assoc*
    alist-cons		alist-copy
    alist-delete	alist-delete!

    ;; sets
    lset<=?			lset=?
    lset-adjoin
    lset-union			lset-union!
    lset-intersection		lset-intersection!
    lset-difference		lset-difference!
    lset-xor			lset-xor!
    lset-diff+intersection	lset-diff+intersection!)
  (import (except (vicare)
		  break)
    (vicare containers lists stx)
    (vicare containers lists low))


;;;; constructors

(define (xcons d a)
  (cons a d))

;;Better to use the Vicare built-in.
;;
;; (define make-list
;;   (case-lambda
;;    ((len)
;;     (make-list len #f))
;;    ((len fill)
;;     (do ((i 0 (+ 1 i))
;; 	 (l '() (cons fill l)))
;; 	((= i len)
;; 	 l)))))

(define (list-copy ell)
  (if (pair? ell)
      (cons (car ell) (list-copy (cdr ell)))
    ell))

(define (tree-copy x)
  (if (pair? x)
      (cons (tree-copy (car x))
	    (tree-copy (cdr x)))
    x))

(define (list-tabulate len proc)
  (do ((i   0   (+ 1 i))
       (ell '() (cons (proc i) ell)))
      ((= i len)
       (reverse ell))))

(define (list-tabulate/reverse len proc)
  (do ((i (- len 1) (- i 1))
       (ret '() (cons (proc i) ret)))
      ((< i 0)
       ret)))

(define iota
  (case-lambda
   ((count)
    (iota count 0 1))
   ((count start)
    (iota count start 1))
   ((count start step)
    (if (< count 0)
	(assertion-violation 'iota
	  "expected non-negative count argument" count)
      (do ((count count (- count 1))
	   (val (+ start (* (- count 1) step)) (- val step))
	   (ret '() (cons val ret)))
	  ((<= count 0)
	   ret))))))


;;;; circular lists

(define (circular-list val1 . vals)
  (list->circular-list! (cons val1 vals)))

(define (list->circular-list! ell)
  (set-cdr! (last-pair ell) ell)
  ell)

(define (circular-last-pair circ)
  ;;This LOOP  is needed because  we must keep  a referencde to  CIRC to
  ;;check for circularity.
  (let loop ((p circ))
    (if (eq? circ (cdr p))
	p
      (loop (cdr p)))))

(define (circular-list->list! circ)
  (if (null? circ)
      circ
    (begin
      (set-cdr! (circular-last-pair circ) '())
      circ)))

(define (circular-list-copy circ)
  (if (null? circ)
      circ
    (let loop ((cir circ)
	       (ell '()))
      (if (eq? circ (cdr cir))
	  (list->circular-list! (reverse (cons (car cir) ell)))
	(loop (cdr cir) (cons (car cir) ell))))))

(define (circular-list-length circ)
  (if (null? circ)
      0
    (do ((i 1 (+ 1 i))
	 (p circ (cdr p)))
	((eq? circ (cdr p))
	 i))))

(define (circular-list= item= . list-of-clists)
  (let-values (((and-nil or-nil) (apply %and/or-null? list-of-clists)))
    (cond (and-nil #t)
	  (or-nil  #f)
	  (else
	   (let loop ((circs list-of-clists))
	     (let-values (((cars cdrs) (%cars/cdrs* circs)))
	       (or (null? cars)
		   (and (apply item= cars)
			(let-values (((and-eq or-eq) (%and/or-eq? list-of-clists cdrs)))
			  (cond (and-eq #t)
				(or-eq  #f)
				(else (loop cdrs))))))))))))


;;;; predicates

(define (not-pair? obj)
  (not (pair? obj)))

;;IMPLEMENTATION NOTE
;;
;;We can detect if a list is circular with the simple:
;;
;; (define (simple-circular-list? obj)
;;   (let loop ((ell obj))
;;     (and (pair? ell)
;; 	 (let ((ell (cdr ell)))
;; 	   (or (eq? obj ell)
;; 	       (loop ell))))))
;;
;;which works fine for circular lists being "rings" like:
;;
;;  (circular-list 1 2 3 4 5)
;;
;;but fails with lists which have a circular tail like:
;;
;;  (cons 1 (cons 2 (cons 3 (cons 4 (circular-list 5 6 7 8)))))
;;

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

(define (circular-list?/or-null obj)
  (or (null? obj) (circular-list? obj)))

(define (dotted-list? obj)
  (and (pair? obj)
       ;;At every iteration ELL is CDR-ed twice, LAG is CDR-ed once.  So
       ;;if  OBJ is a  pair, member  of circular  list, EQ?   below will
       ;;detect   it   with   the   same  number   of   comparisons   of
       ;;CIRCULAR-LIST?.
       (let loop ((ell obj)
		  (lag obj))
	 (if (pair? ell)
	     (let ((ell (cdr ell)))
	       (if (pair? ell)
		   (let ((ell (cdr ell))
			 (lag (cdr lag)))
		     (and (not (eq? ell lag))
			  (loop ell lag)))
		 (not (null? ell))))
	   (not (null? ell))))))

(define (dotted-list?/or-null obj)
  (or (null? obj) (dotted-list? obj)))

(define (and-null? . list-of-lists)
  (or (null? list-of-lists)
      (and (null? (car list-of-lists))
	   (apply and-null? (cdr list-of-lists)))))

(define (or-null? . list-of-lists)
  (if (null? list-of-lists)
      #f
    (or (null? (car list-of-lists))
	(apply or-null? (cdr list-of-lists)))))


;;;; comparison

(define list=?
  (case-lambda
   ((item=)
    #t)
   ((item= ell)
    #t)
   ((item= ell1 ell2)
    (cond ((eq? ell1 ell2)
	   #t)
	  ((null? ell1)
	   (or (null? ell2)
	       #f))
	  ((null? ell2)
	   #f)
	  ((item= (car ell1) (car ell2))
	   (list=? item= (cdr ell1) (cdr ell2)))
	  (else
	   #f)))
   ((item= . ells)
    (and (list=? item= (car ells) (cadr ells))
	 (apply list=? item= (cdr ells))))))


;;;; selectors

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

(define (car+cdr pair)
  (values (car pair) (cdr pair)))

;;; --------------------------------------------------------------------

(define (take-left dotted k)
  (let loop ((ret '())
	     (dotted dotted)
	     (k k))
    (if (zero? k)
	(reverse ret)
      (loop (cons (car dotted) ret)
	    (cdr dotted)
	    (- k 1)))))

(define (drop-left dotted k)
  (if (fixnum? k)
      (if (fxzero? k)
	  dotted
	(drop-left (cdr dotted) (fx- k 1)))
    (if (zero? k)
	dotted
      (drop-left (cdr dotted) (- k 1)))))

;;; --------------------------------------------------------------------

(define (take-right dotted k)
  (let loop ((lag	dotted)
	     (lead	(drop-left dotted k)))
    (if (pair? lead)
	(loop (cdr lag) (cdr lead))
      lag)))

(define (drop-right dotted k)
  (let loop ((lag	dotted)
	     (lead	(drop-left dotted k)))
    (if (pair? lead)
	(cons (car lag)
	      (loop (cdr lag) (cdr lead)))
      '())))

;;; --------------------------------------------------------------------

(define (take-left! dotted k)
  (if (zero? k)
      '()
    (begin
      (set-cdr! (drop-left dotted (- k 1)) '())
      dotted)))

(define (drop-right! dotted k)
  ;;In this  function, LEAD is actually  K+1 ahead of LAG.  This lets us
  ;;stop LAG one step early, in time to smash its cdr to ().
  (let ((lead (drop-left dotted k)))
    (if (pair? lead)
	(let loop ((lag  dotted)
		   (lead (cdr lead))) ; Standard case
	  (if (pair? lead)
	      (loop (cdr lag) (cdr lead))
	    (begin
	      (set-cdr! lag '())
	      dotted)))
      '()))) ; Special case dropping everything -- no cons to side-effect.

;;; --------------------------------------------------------------------

(define (split-at ell k)
  (if (zero? k)
      (values '() ell)
    (receive (prefix suffix)
	(split-at (cdr ell) (- k 1))
      (values (cons (car ell) prefix)
	      suffix))))

(define (split-at! x k)
  (if (zero? k) (values '() x)
    (let* ((prev   (drop-left x (- k 1)))
	   (suffix (cdr prev)))
      (set-cdr! prev '())
      (values x suffix))))

(define (last ell)
  (car (last-pair ell)))

;;Better to use the Vicare built-in.
;;
;; (define (last-pair x)
;;   (if (pair? (cdr x))
;;       (last-pair (cdr x))
;;     x))


;;;; miscellaneous

(define (length+ x)
  ;;returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x   (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	    len))
      len)))

;;; --------------------------------------------------------------------

(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists)
	   (prev '()))
    (if (not (pair? lists))
	prev
      (let ((first (car lists))
	    (rest (cdr lists)))
	(if (not (pair? first))
	    (lp rest first)
	  ;; Now, do the splicing.
	  (let lp2 ((tail-cons (last-pair first))
		    (rest rest))
	    (if (pair? rest)
		(let ((next (car rest))
		      (rest (cdr rest)))
		  (set-cdr! tail-cons next)
		  (lp2 (if (pair? next) (last-pair next) tail-cons)
		       rest))
	      first)))))))

(define (concatenate  lists)
  (reduce* append  '() lists))

(define (concatenate! lists)
  (reduce* append! '() lists))

(define (append-reverse rev-head tail)
  (if (null? rev-head)
      tail
    (append-reverse (cdr rev-head)
		    (cons (car rev-head) tail))))

(define (append-reverse! rev-head tail)
  (if (null? rev-head)
      tail
    (let ((next-rev (cdr rev-head)))
      (set-cdr! rev-head tail)
      (append-reverse! next-rev rev-head))))

;;; --------------------------------------------------------------------

(define (zip list1 . more-lists)
  (apply map list list1 more-lists))

(define (zip* list1 . more-lists)
  (apply map* list list1 more-lists))

(define (unzip1 lis)
  (map car lis))

(define (unzip2 lis)
  (if (null? lis)
      (values lis lis)	     ; Use NOT-PAIR? to handle
    (let ((elt (car lis)))   ; dotted lists.
      (receive (a b)
	  (unzip2 (cdr lis))
	(values (cons (car  elt) a)
		(cons (cadr elt) b))))))

(define (unzip3 lis)
  (if (null? lis)
      (values lis lis lis)
    (let ((elt (car lis)))
      (receive (a b c)
	  (unzip3 (cdr lis))
	(values (cons (car   elt) a)
		(cons (cadr  elt) b)
		(cons (caddr elt) c))))))

(define (unzip4 lis)
  (if (null? lis)
      (values lis lis lis lis)
    (let ((elt (car lis)))
      (receive (a b c d)
	  (unzip4 (cdr lis))
	(values (cons (car    elt) a)
		(cons (cadr   elt) b)
		(cons (caddr  elt) c)
		(cons (cadddr elt) d))))))

(define (unzip5 lis)
  (if (null? lis)
      (values lis lis lis lis lis)
    (let ((elt (car lis)))
      (receive (a b c d e)
	  (unzip5 (cdr lis))
	(values (cons (car     elt) a)
		(cons (cadr    elt) b)
		(cons (caddr   elt) c)
		(cons (cadddr  elt) d)
		(cons (car (cddddr  elt)) e))))))

;;; --------------------------------------------------------------------

(define count
  (case-lambda

   ((pred ell)
    (let loop ((ell ell)
	       (i   0))
      (if (null? ell) i
	(loop (cdr ell)
	      (if (pred (car ell))
		  (+ i 1)
		i)))))

   ((pred ell . ells)
    (let loop ((ell  ell)
	       (ells ells)
	       (i    0))
      (if (null? ell)
	  i
	(let-values (((as ds) (%cars/cdrs* ells)))
	  (if (null? as)
	      i
	    (loop (cdr ell)
		  ds
		  (if (apply pred (car ell) as)
		      (+ i 1)
		    i)))))))))

;;; --------------------------------------------------------------------

(define (reverse! lis)
  (let lp ((lis lis) (ret '()))
    (if (null? lis) ret
      (let ((tail (cdr lis)))
	(set-cdr! lis ret)
	(lp tail lis)))))


;;;; fold/unfold functions

(define fold
  (case-lambda

   ((kons knil ell)
    (if (null? ell)
	knil
      (fold kons
	    (kons (car ell) knil)
	    (cdr ell))))

   ((kons knil ell0 . ells)
    (let loop ((ells	(cons ell0 ells))
	       (knil	knil))
      (receive (cars+knil cdrs)
	  (%cars+knil/cdrs* ells knil)
	(if (null? cars+knil)
	    knil
	  (loop cdrs (apply kons cars+knil))))))))

(define fold*
  (case-lambda

   ((kons knil ell)
    (if (null? ell)
	knil
      (kons (car ell)
	    (fold* kons knil (cdr ell)))))

   ((kons knil ell0 . ells)
    (let loop ((ells (cons ell0 ells)))
      (if (null? ells)
	  knil
	(let ((cdrs (%cdrs* ells)))
	  (if (null? cdrs)
	      knil
	    (apply kons (%cars+knil* ells (loop cdrs))))))))))

;;; --------------------------------------------------------------------

(define fold-left*
  (case-lambda

   ((combine knil ell)
    (if (null? ell)
	knil
      (fold-left* combine
		  (combine knil (car ell))
		  (cdr ell))))

   ((combine knil ell0 . ells)
    (let loop ((knil	knil)
	       (ells	(cons ell0 ells)))
      (receive (knil+cars cdrs)
	  (%knil+cars/cdrs* ells knil)
	(if (null? knil+cars)
	    knil
	  (loop (apply combine knil+cars) cdrs)))))))

(define fold-right*
  (case-lambda

   ((combine knil ell)
    (if (null? ell)
	knil
      (combine (car ell)
	       (fold-right* combine knil (cdr ell)))))

   ((combine knil ell0 . ells)
    (let loop ((ells (cons ell0 ells)))
      (if (null? ells)
	  knil
	(let ((cdrs (%cdrs* ells)))
	  (if (null? cdrs)
	      knil
	    (apply combine (%cars+knil* ells (loop cdrs))))))))))

;;; --------------------------------------------------------------------

(define pair-fold
  (case-lambda

   ((f knil ell)
    (if (null? ell)
	knil
      ;;Make sure we acquire the tail before applying the function F!!!
      (let ((tail (cdr ell)))
	(pair-fold f (f ell knil) tail))))

   ((f knil ell0 . ells)
    (let loop ((ells	(cons ell0 ells))
	       (return	knil))
      (let ((tails (%cdrs* ells)))
	(if (null? tails)
	    return
	  (loop tails (apply f (append! ells (list return))))))))))

(define (pair-fold* f knil lis1 . ells)
  (if (pair? ells)
      (let loop ((ells (cons lis1 ells))) ; N-ary case
	(let ((cdrs (%cdrs* ells)))
	  (if (null? cdrs) knil
	    (apply f (append! ells (list (loop cdrs)))))))
    (let loop ((lis lis1)) ; Fast path
      (if (null? lis) knil (f lis (loop (cdr lis)))))))

;;; --------------------------------------------------------------------

(define (reduce f ridentity ell)
  (if (null? ell)
      ridentity
    (fold f (car ell) (cdr ell))))

(define (reduce* f ridentity ell)
  (if (null? ell)
      ridentity
    (let loop ((head	(car ell))
	       (ell	(cdr ell)))
      (if (pair? ell)
	  (f head (loop (car ell) (cdr ell)))
	head))))

;;; --------------------------------------------------------------------

(define unfold-right
  (case-lambda

   ((stop? map-to-knil seed-step seed)
    (unfold-right stop? map-to-knil seed-step seed '()))

   ((stop? map-to-knil seed-step seed tail)
    (if (stop? seed)
	tail
      (unfold-right stop? map-to-knil seed-step
		    (seed-step seed)
		    (cons (map-to-knil seed) tail))))))

(define unfold
  (case-lambda

   ((stop? map-to-knil seed-step seed)
    (if (stop? seed)
	'()
      (cons (map-to-knil seed)
	    (unfold stop? map-to-knil seed-step (seed-step seed)))))

   ((stop? map-to-knil seed-step seed tail-gen)
    (let loop ((seed seed))
      (if (stop? seed)
	  (tail-gen seed)
	(cons (map-to-knil seed)
	      (loop (seed-step seed))))))))


;;;; derived fold functions

(define and-fold-left*
  (case-lambda

   ((combine knil ell)
    (if (null? ell)
	knil
      (let ((knil (combine knil (car ell))))
	(and knil
	     (and-fold-left* combine knil (cdr ell))))))

   ((combine knil ell0 . ells)
    (let loop ((knil	knil)
	       (ells	(cons ell0 ells)))
      (receive (knil+cars cdrs)
	  (%knil+cars/cdrs ells knil)
	(if (null? knil+cars)
	    knil
	  (let ((knil (apply combine knil+cars)))
	    (and knil
		 (loop knil cdrs)))))))))

(define and-fold-right*
  (case-lambda

   ((combine knil ell)
    (if (null? ell)
	knil
      (let ((knil (and-fold-right* combine knil (cdr ell))))
	(and knil
	     (combine (car ell) knil)))))

   ((combine knil ell0 . ells)
    (let loop ((ells (cons ell0 ells)))
      (if (null? ells)
	  knil
	(let ((cdrs (%cdrs* ells)))
	  (if (null? cdrs)
	      knil
	    (let ((knil (loop cdrs)))
	      (and knil
		   (apply combine (%cars+knil* ells knil)))))))))))

;;; --------------------------------------------------------------------

(define (fold-left/pred pred knil ell)
  (and-fold-left*/stx (lambda (knil item)
			(and (pred knil item) item))
		      knil ell))


;;;; mappers

(define for-each*
  (case-lambda

   ((f ell)
    (unless (null? ell)
      (f (car ell))
      (for-each* f (cdr ell))))

   ((f ell . ells)
    (let loop ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs* ells)
	(when (pair? cars)
	  (apply f cars)
	  (loop cdrs)))))))

;;; --------------------------------------------------------------------

(define map-in-order*
  (case-lambda

   ((f ell)
    (if (null? ell)
	ell
      (let ((tail (cdr ell))
	    (x    (f (car ell))))	   ;Do head first,
	(cons x (map-in-order* f tail))))) ;then tail.

   ((f ell . ells)
    (let recur ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs* ells)
	(if (pair? cars)
	    (let ((x (apply f cars))) ; Do head first,
	      (cons x (recur cdrs)))  ; then tail.
	  '()))))))

(define map* map-in-order*)

;;; --------------------------------------------------------------------

(define (append-map f lis1 . lists)
  (really-append-map append  f lis1 lists))

(define (append-map! f lis1 . lists)
  (really-append-map append! f lis1 lists))

(define really-append-map
  (case-lambda

   ((appender f ell)
    (if (null? ell)
	'()
      (let recur ((elt (car ell))
		  (ell (cdr ell)))
	(let ((vals (f elt)))
	  (if (null? ell)
	      vals
	    (appender vals (recur (car ell) (cdr ell))))))))

   ((appender f lis1 lists)
    (receive (cars cdrs)
	(%cars/cdrs* (cons lis1 lists))
      (if (null? cars) '()
	(let recur ((cars cars)
		    (cdrs cdrs))
	  (let ((vals (apply f cars)))
	    (receive (cars2 cdrs2)
		(%cars/cdrs* cdrs)
	      (if (null? cars2)
		  vals
		(appender vals (recur cars2 cdrs2)))))))))))

;;; --------------------------------------------------------------------

(define pair-for-each
  (case-lambda

   ((f ell)
    (unless (null? ell)
      (let ((tail (cdr ell))) ; Grab the cdr now,
	(f ell)		      ; in case PROC SET-CDR!s ELL.
	(pair-for-each f tail))))

   ((f ell . ells)
    (let loop ((ells (cons ell ells)))
      (let ((tails (%cdrs ells)))
	(when (pair? tails)
	  (begin (apply f ells)
		 (loop tails))))))))

(define pair-for-each*
  (case-lambda

   ((f ell)
    (unless (null? ell)
      (let ((tail (cdr ell))) ; Grab the cdr now,
	(f ell)		      ; in case PROC SET-CDR!s ELL.
	(pair-for-each* f tail))))

   ((f ell . ells)
    (let loop ((ells (cons ell ells)))
      (let ((tails (%cdrs* ells)))
	(when (pair? tails)
	  (begin (apply f ells)
		 (loop tails))))))))

(define map!
  (case-lambda

   ((f ell)
    (pair-for-each (lambda (pair)
		     (set-car! pair (f (car pair))))
		    ell)
    ell)

   ((f ell . ells)
    (let loop ((ell  ell)
	       (ells ells))
      (unless (null? ell)
	(receive (heads tails)
	    (%cars+cdrs*/no-test ells)
	  (set-car! ell (apply f (car ell) heads))
	  (loop (cdr ell) tails))))
    ell)))

;;; We stop when ELL runs out, not when any list runs out.
(define map*!
  (case-lambda

   ((f ell)
    (pair-for-each* (lambda (pair)
		      (set-car! pair (f (car pair))))
		    ell)
    ell)

   ((f ell . ells)
    (let loop ((ell  ell)
	       (ells ells))
      (unless (null? ell)
	(receive (heads tails)
	    (%cars+cdrs*/no-test ells)
	  (set-car! ell (apply f (car ell) heads))
	  (loop (cdr ell) tails))))
    ell)))

;;; Map F across L, and save up all the non-false results.
(define filter-map
  (case-lambda

   ((f ell)
    (if (null? ell)
	ell
      (let ((tail (filter-map f (cdr ell))))
	(cond ((f (car ell))
	       => (lambda (x)
		    (cons x tail)))
	      (else
	       tail)))))

   ((f ell . ells)
    (let recur ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs ells)
	(if (pair? cars)
	    (cond
	     ((apply f cars)
	      => (lambda (x)
		   (cons x (recur cdrs))))
	     (else
	      (recur cdrs))) ; Tail call in this arm.
	  '()))))))

(define filter-map*
  (case-lambda

   ((f ell)
    (if (null? ell)
	ell
      (let ((tail (filter-map* f (cdr ell))))
	(cond ((f (car ell))
	       => (lambda (x)
		    (cons x tail)))
	      (else
	       tail)))))

   ((f ell . ells)
    (let recur ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs* ells)
	(if (pair? cars)
	    (cond
	     ((apply f cars)
	      => (lambda (x)
		   (cons x (recur cdrs))))
	     (else
	      (recur cdrs))) ; Tail call in this arm.
	  '()))))))


;;;; filter, remove, partition

(define (filter! pred lis)
  (cond ((null? lis)
	 lis)
	((not (pred (car lis)))
	 (filter! pred (cdr lis)))
	(else
	 (letrec ((scan-in (lambda (prev ell)
			     (if (pair? ell)
				 (if (pred (car ell))
				     (scan-in ell (cdr ell))
				   (scan-out prev (cdr ell))))))
		  (scan-out (lambda (prev ell)
			      (let lp ((ell ell))
				(if (pair? ell)
				    (if (pred (car ell))
					(begin (set-cdr! prev ell)
					       (scan-in ell (cdr ell)))
				      (lp (cdr ell)))
				  (set-cdr! prev ell))))))
	   (scan-in lis (cdr lis))
	   lis))))

(define (partition! pred lis)
  (if (null? lis)
      (values lis lis)
    (letrec ((scan-in (lambda (in-prev out-prev lis)
			(let lp ((in-prev in-prev) (lis lis))
			  (if (pair? lis)
			      (if (pred (car lis))
				  (lp lis (cdr lis))
				(begin (set-cdr! out-prev lis)
				       (scan-out in-prev lis (cdr lis))))
			    (set-cdr! out-prev lis))))) ; Done.

	     (scan-out (lambda (in-prev out-prev lis)
			 (let lp ((out-prev out-prev) (lis lis))
			   (if (pair? lis)
			       (if (pred (car lis))
				   (begin (set-cdr! in-prev lis)
					  (scan-in lis out-prev (cdr lis)))
				 (lp lis (cdr lis)))
			     (set-cdr! in-prev lis)))))) ; Done.
      (if (pred (car lis))
	  (let lp ((prev-l lis) (l (cdr lis)))
	    (cond ((not (pair? l)) (values lis l))
		  ((pred (car l)) (lp l (cdr l)))
		  (else (scan-out prev-l l (cdr l))
			(values lis l)))) ; Done.
	(let lp ((prev-l lis) (l (cdr lis)))
	  (cond ((not (pair? l)) (values l lis))
		((pred (car l))
		 (scan-in l prev-l (cdr l))
		 (values l lis)) ; Done.
		(else (lp l (cdr l)))))))))

(define (remove*  pred l)
  (filter (lambda (x)
	    (not (pred x)))
    l))

(define (remove*! pred l)
  (filter! (lambda (x)
	     (not (pred x)))
	   l))


;;;; searching

(define (find-tail pred list)
  (and (not (null? list))
       (if (pred (car list))
	   list
	 (find-tail pred (cdr list)))))

(define (take-while pred lis)
  (if (null? lis)
      '()
    (let ((x (car lis)))
      (if (pred x)
	  (cons x (take-while pred (cdr lis)))
	'()))))

(define (drop-while pred lis)
  (cond ((null? lis)
	 '())
	((pred (car lis))
	 (drop-while pred (cdr lis)))
	(else
	 lis)))

(define (take-while! pred lis)
  (if (or (null? lis)
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
      lis)))

(define (span pred lis)
  (if (null? lis)
      (values '() '())
    (let ((x (car lis)))
      (if (pred x)
	  (receive (prefix suffix)
	      (span pred (cdr lis))
	    (values (cons x prefix) suffix))
	(values '() lis)))))

(define (span! pred lis)
  (if (or (null? lis)
	  (not (pred (car lis))))
      (values '() lis)
    (let ((suffix (let lp ((prev lis)
			   (rest (cdr lis)))
		    (if (null? rest) rest
		      (let ((x (car rest)))
			(if (pred x)
			    (lp rest (cdr rest))
			  (begin
			    (set-cdr! prev '())
			    rest)))))))
      (values lis suffix))))

(define (break  pred lis)
  (span  (lambda (x) (not (pred x))) lis))

(define (break! pred lis)
  (span! (lambda (x) (not (pred x))) lis))

(define any
  (case-lambda
   ((pred ell)
    (and (not (null? ell))
	 (let loop ((head (car ell))
		    (tail (cdr ell)))
	   (if (null? tail)
	       (pred head) ; Last PRED app is tail call.
	     (or (pred head)
		 (loop (car tail) (cdr tail)))))))

   ((pred ell . lists)
    (receive (heads tails)
	(%cars/cdrs* (cons ell lists))
      (and (pair? heads)
	   (let loop ((heads heads) (tails tails))
	     (receive (next-heads next-tails)
		 (%cars/cdrs tails)
	       (if (pair? next-heads)
		   (or (apply pred heads)
		       (loop next-heads next-tails))
		 (apply pred heads))))))))) ; Last PRED app is tail call.

(define any*
  (case-lambda
   ((pred ell)
    (and (not (null? ell))
	 (let loop ((head (car ell))
		    (tail (cdr ell)))
	   (if (null? tail)
	       (pred head) ; Last PRED app is tail call.
	     (or (pred head)
		 (loop (car tail) (cdr tail)))))))

   ((pred ell . lists)
    (receive (heads tails)
	(%cars/cdrs* (cons ell lists))
      (and (pair? heads)
	   (let loop ((heads heads) (tails tails))
	     (receive (next-heads next-tails)
		 (%cars/cdrs* tails)
	       (if (pair? next-heads)
		   (or (apply pred heads)
		       (loop next-heads next-tails))
		 (apply pred heads))))))))) ; Last PRED app is tail call.

(define every
  (case-lambda

   ((p ls)
    (or (null? ls)
	(let recur ((p p)
		    (a (car ls))
		    (d (cdr ls)))
	  (if (pair? d)
	      (and (p a)
		   (recur p (car d) (cdr d)))
	    (p a)))))

   ((p ls1 ls2)
    (if (and (pair? ls1)
	     (pair? ls2))
	(let recur ((p  p)
		    (a1 (car ls1))
		    (d1 (cdr ls1))
		    (a2 (car ls2))
		    (d2 (cdr ls2)))
	  (cond ((or (and (pair? d1) (null? d2))
		     (and (null? d1) (pair? d2)))
		 (assertion-violation 'every "expected lists of equal length"))
		((and (pair? d1) (pair? d2))
		 (and (p a1 a2)
		      (recur p (car d1) (cdr d1) (car d2) (cdr d2))))
		(else
		 (p a1 a2))))
      #t))

   ((pred lis1 . lists)
    (receive (heads tails)
	(%cars/cdrs (cons lis1 lists))
      (or (not (pair? heads))
	  (let loop ((heads heads)
		     (tails tails))
	    (receive (next-heads next-tails)
		(%cars/cdrs tails)
	      (if (pair? next-heads)
		  (and (apply pred heads)
		       (loop next-heads next-tails))
		(apply pred heads)))))))))

(define every*
  (case-lambda

   ((p ls)
    (or (null? ls)
	(let recur ((p p)
		    (a (car ls))
		    (d (cdr ls)))
	  (if (pair? d)
	      (and (p a)
		   (recur p (car d) (cdr d)))
	    (p a)))))

   ((p ls1 ls2)
    (if (and (pair? ls1) (pair? ls2))
	(let recur ((p  p)
		    (a1 (car ls1))
		    (d1 (cdr ls1))
		    (a2 (car ls2))
		    (d2 (cdr ls2)))
	  (if (and (pair? d1)
		   (pair? d2))
	      (and (p a1 a2)
		   (recur p (car d1) (cdr d1) (car d2) (cdr d2)))
	    (p a1 a2)))
      #t))

   ((pred lis1 . lists)
    (receive (heads tails)
	(%cars/cdrs* (cons lis1 lists))
      (or (not (pair? heads))
	  (let loop ((heads heads)
		     (tails tails))
	    (receive (next-heads next-tails) (%cars/cdrs* tails)
	      (if (pair? next-heads)
		  (and (apply pred heads)
		       (loop next-heads next-tails))
		(apply pred heads)))))))))

(define list-index
  (case-lambda
   ((pred lis1)
    (let loop ((lis lis1)
	       (n 0))
      (and (not (null? lis))
	   (if (pred (car lis))
	       n
	     (loop (cdr lis) (+ n 1))))))

   ((pred lis1 . lists)
    (let loop ((lists (cons lis1 lists))
	       (n     0))
      (receive (heads tails)
	  (%cars/cdrs lists)
	(and (pair? heads)
	     (if (apply pred heads) n
	       (loop tails (+ n 1)))))))))

(define list-index*
  (case-lambda
   ((pred lis1)
    (let loop ((lis lis1) (n 0))
      (and (not (null? lis))
	   (if (pred (car lis))
	       n
	     (loop (cdr lis) (+ n 1))))))

   ((pred lis1 . lists)
    (let loop ((lists (cons lis1 lists))
	       (n     0))
      (receive (heads tails)
	  (%cars/cdrs* lists)
	(and (pair? heads)
	     (if (apply pred heads)
		 n
	       (loop tails (+ n 1)))))))))

(define (position item ell)
  (let loop ((ell ell)
	     (idx 0))
    (and (not (null? ell))
	 (if (eq? item (car ell))
	     idx
	   (loop (cdr ell) (+ 1 idx))))))

(define member*
  (case-lambda
   ((x lis)
    (member x lis))
   ((x lis =)
    (find-tail (lambda (y) (= x y)) lis))))


;;;; deletion

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

(define delete-duplicates
  (case-lambda
   ((lis)
    (delete-duplicates lis equal?))
   ((lis elt=)
    (if (null? lis)
	lis
      (let* ((x        (car lis))
	     (tail     (cdr lis))
	     (new-tail (delete-duplicates (delete x tail elt=)
					  elt=)))
	(if (eq? tail new-tail)
	    lis
	  (cons x new-tail)))))))

(define delete-duplicates!
  (case-lambda
   ((lis)
    (delete-duplicates! lis equal?))
   ((lis elt=)
    (if (null? lis)
	lis
      (let* ((x        (car lis))
	     (tail     (cdr lis))
	     (new-tail (delete-duplicates! (delete! x tail elt=)
					   elt=)))
	(if (eq? tail new-tail)
	    lis
	  (cons x new-tail)))))))


;;;; sorted lists

(define (sorted-list-insert item ell item>)
  (let loop ((result '())
	     (ell    ell))
    (if (null? ell)
	(append-reverse (cons item result) ell)
      (let ((x (car ell)))
	(cond ((item> item x)
	       (loop (cons x result) (cdr ell)))
	      (else
	       (append-reverse (cons item result) ell)))))))

(define (sorted-list-insert/uniq item ell item< item>)
  (let loop ((reversed-head '())
	     (tail          ell))
    (if (null? tail)
	(append-reverse (cons item reversed-head) tail)
      (let ((x (car tail)))
	(cond ((item< item x)
	       (append-reverse (cons item reversed-head) tail))
	      ((item> item x)
	       (loop (cons x reversed-head) (cdr tail)))
	      (else
	       ell))))))

(define (union-of-sorted-lists ell1 ell2 item< item>)
  (let loop ((result '())
	     (ell1 ell1)
	     (ell2 ell2))
    (cond ((null? ell1)    (append-reverse result ell2))
	  ((null? ell2)    (append-reverse result ell1))
	  (else
	   (let ((x (car ell1))
		 (y (car ell2)))
	     (cond
	      ((item> x y)
	       (loop (cons y result) ell1 (cdr ell2)))
	      ((item< x y)
	       (loop (cons x result) (cdr ell1) ell2))
	      (else ;equal items
	       ;;First the ones from ELL1, then the ones from ELL2.
	       (loop (cons x result) (cdr ell1) ell2))))))))

(define (union-of-sorted-lists/uniq ell1 ell2 item< item>)
  (let loop ((result '())
	     (ell1 ell1)
	     (ell2 ell2))
    (cond ((null? ell1)    (append-reverse result ell2))
	  ((null? ell2)    (append-reverse result ell1))
	  (else
	   (let ((x (car ell1))
		 (y (car ell2)))
	     (cond
	      ((item> x y)
	       (loop (cons y result) ell1 (cdr ell2)))
	      ((item< x y)
	       (loop (cons x result) (cdr ell1) ell2))
	      (else
	       (loop result (cdr ell1) ell2))))))))


;;;; alists

(define assoc*
  (case-lambda
   ((x lis)
    (assoc x lis))
   ((x lis =)
    (find (lambda (entry) (= x (car entry))) lis))))

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
    (filter (lambda (elt) (not (= key (car elt)))) alist))))

(define alist-delete!
  (case-lambda
   ((key alist)
    (alist-delete! key alist equal?))
   ((key alist =)
    (filter! (lambda (elt) (not (= key (car elt)))) alist))))


;;; sets

(define (%lset2<= = lis1 lis2)
  (every (lambda (x) (member* x lis2 =)) lis1))

(define (lset<=? = . lists)
  (or (not (pair? lists))
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1)
		       (%lset2<= = s1 s2))
		   (lp s2 rest)))))))

(define (lset=? = . lists)
  (or (not (pair? lists))
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2   (car rest))
		  (rest (cdr rest)))
	      (and (or (eq? s1 s2)
		       (and (%lset2<= = s1 s2)
			    (%lset2<= = s2 s1)))
		   (lp s2 rest)))))))


(define (lset-adjoin = lis . elts)
  (fold (lambda (elt ret) (if (member* elt ret =) ret (cons elt ret)))
	lis elts))

(define (lset-union = . lists)
  (reduce (lambda (lis ret)
	    (cond ((null? lis) ret)
		  ((null? ret) lis)
		  ((eq? lis ret) ret)
		  (else
		   (fold (lambda (elt ret)
			   (if (any (lambda (x) (= x elt)) ret)
			       ret
			     (cons elt ret)))
			 ret lis))))
	  '() lists))

(define (lset-union! = . lists)
  (reduce (lambda (lis ret)
	    (cond ((null? lis) ret)
		  ((null? ret) lis)
		  ((eq? lis ret) ret)
		  (else
		   (pair-fold
		    (lambda (pair ret)
		      (let ((elt (car pair)))
			(if (any (lambda (x) (= x elt)) ret)
			    ret
			  (begin (set-cdr! pair ret) pair))))
		    ret lis))))
	  '() lists))

(define (lset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?)))
    (cond ((any null? lists) '())
	  ((null? lists)          lis1)
	  (else (filter (lambda (x)
			  (every (lambda (lis) (member* x lis =)) lists))
		  lis1)))))

(define (lset-intersection! = lis1 . lists)
  (let ((lists (delete lis1 lists eq?)))
    (cond ((any null? lists) '())
	  ((null? lists)          lis1)
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (member* x lis =)) lists))
			 lis1)))))

(define (lset-difference = lis1 . lists)
  (let ((lists (filter pair? lists)))
    (cond ((null? lists)     lis1)
	  ((memq lis1 lists) '())
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member* x lis =)))
			    lists))
		  lis1)))))

(define (lset-difference! = lis1 . lists)
  (let ((lists (filter pair? lists)))
    (cond ((null? lists)     lis1)
	  ((memq lis1 lists) '())
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (not (member* x lis =)))
			     lists))
			 lis1)))))

(define (lset-xor = . lists)
  (reduce (lambda (b a)
	    (receive (a-b a-int-b)   (lset-diff+intersection = a b)
	      (cond ((null? a-b)     (lset-difference = b a))
		    ((null? a-int-b) (append b a))
		    (else (fold (lambda (xb ret)
				  (if (member* xb a-int-b =) ret (cons xb ret)))
				a-b
				b)))))
	  '() lists))

(define (lset-xor! = . lists)
  (reduce
   (lambda (b a)
     (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
       (cond ((null? a-b)     (lset-difference! = b a))
	     ((null? a-int-b) (append! b a))
	     (else (pair-fold (lambda (b-pair ret)
				(if (member* (car b-pair) a-int-b =) ret
				  (begin (set-cdr! b-pair ret) b-pair)))
			      a-b
			      b)))))
   '() lists))

(define (lset-diff+intersection = lis1 . lists)
  (cond ((every null? lists) (values lis1 '()))
	((memq lis1 lists)        (values '() lis1))
	(else (partition (lambda (elt)
			   (not (any (lambda (lis) (member* elt lis =))
				     lists)))
			 lis1))))

(define (lset-diff+intersection! = lis1 . lists)
  (cond ((every null? lists) (values lis1 '()))
	((memq lis1 lists)        (values '() lis1))
	(else (partition! (lambda (elt)
			    (not (any (lambda (lis) (member* elt lis =))
				   lists)))
			  lis1))))



;;;; done

)

;;; end of library
