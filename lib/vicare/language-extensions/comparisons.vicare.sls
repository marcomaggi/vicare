;;;Copyright (c) 2008 Derick Eddington
;;;Copyright (c) 2005 Sebastian Egner and Jens Axel S{\o}gaard.
;;;
;;;Nausicaa integration by Marco Maggi.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(library (vicare language-extensions comparisons)
  (export
    </<=? </<? <=/<=? <=/<? <=? <? =?
    >/>=? >/>? >=/>=? >=/>? >=? >?
    boolean-compare chain<=? chain<? chain=? chain>=? chain>?
    char-compare char-compare-ci
    compare-by< compare-by<= compare-by=/< compare-by=/> compare-by>
    compare-by>= complex-compare cond-compare
    debug-compare default-compare
    if-not=? if3 if<=? if<? if=? if>=? if>? integer-compare
    kth-largest list-compare list-compare-as-vector
    max-compare min-compare not=? number-compare
    pair-compare pair-compare-car pair-compare-cdr
    pairwise-not=? rational-compare real-compare
    refine-compare select-compare string-compare string-compare-ci
    symbol-compare vector-compare vector-compare-as-list)
  (import (rnrs)
    (vicare crypto randomisations)) ; for random-integer


;;;; helpers

(define (%checked result compare . args)
  (for-each (lambda (x) (compare x x)) args)
  result)

(define-syntax %type-check
  ;;If ?OBJ does not satisfy ?PRED raise an assertion violation.
  ;;
  (syntax-rules ()
    ((_ ?func-name ?pred ?type-name ?obj)
     (unless (?pred ?obj)
       (assertion-violation (quote ?func-name)
	 (string-append "expected an " ?type-name " value")
	 ?obj)))
    ((_ ?func-name ?pred ?type-name ?obj1 ?obj2)
     (begin
       (%type-check ?func-name ?pred ?type-name ?obj1)
       (%type-check ?func-name ?pred ?type-name ?obj2)))))

(define-syntax %define-by=/<
  ;;Define the  comparison procedure ?FUNC-NAME using  the supplied atom
  ;;type-specific predicates.
  ;;
  (syntax-rules ()
    ((_ ?func-name ?= ?< ?type-pred ?type-name)
     (define (?func-name obj1 obj2)
       (cond ((not (?type-pred obj1))
	      (assertion-violation (quote ?func-name)
		(string-append "expected " ?type-name " value")
		obj1))
	     ((eq? obj1 obj2)
	      0)
	     ((?type-pred obj2)
	      (if (?= obj1 obj2)
		  0
		(if (?< obj1 obj2)
		    -1
		  1)))
	     (else
	      (assertion-violation (quote ?func-name)
		(string-append "expected " ?type-name " value")
		obj2)))))))

(define-syntax %define-numeric-by=/<
  ;;Define the  comparison procedure ?FUNC-NAME using  the supplied atom
  ;;type-specific predicates.
  ;;
  (syntax-rules ()
    ((_ ?func-name ?= ?< ?type-pred ?type-name)
     (define (?func-name obj1 obj2)
       (cond ((not (?type-pred obj1))
	      (assertion-violation (quote ?func-name)
		(string-append "expected " ?type-name " value")
		obj1))
	     ((or (nan? obj1) (nan? obj2))
	      +nan.0)
	     ((eq? obj1 obj2)
	      0)
	     ((?type-pred obj2)
	      (if (?= obj1 obj2)
		  0
		(if (?< obj1 obj2)
		    -1
		  1)))
	     (else
	      (assertion-violation (quote ?func-name)
		(string-append "expected " ?type-name " value")
		obj2)))))))


;;;; default and debug compare

(define (default-compare x y)
  (select-compare
   x y
   (null?    0)
   (pair?    (default-compare (car x) (car y))
             (default-compare (cdr x) (cdr y)))
   (boolean? (boolean-compare x y))
   (char?    (char-compare    x y))
   (string?  (string-compare  x y))
   (symbol?  (symbol-compare  x y))
   (number?  (number-compare  x y))
   (vector?  (vector-compare default-compare x y))
   (else
    (assertion-violation 'default-compare
      "unrecognized type in default-compare" x y))))

;;Note that we  pass default-compare to compare-{pair,vector} explictly.
;;This makes  sure recursion  proceeds with this  default-compare, which
;;need not be the one in the lexical scope of compare-{pair,vector}.

(define (debug-compare c)

  (define (checked-value c x y)
    (let ((c-xy (c x y)))
      (if (or (eqv? c-xy -1) (eqv? c-xy 0) (eqv? c-xy 1))
          c-xy
	(error "compare value not in {-1,0,1}" c-xy (list c x y)))))

  (define (random-boolean)
    (zero? (random-integer 2)))

  (define q	; (u v w) such that u <= v, v <= w, and not u <= w
    '#(
		;x < y   x = y   x > y   [x < z]
       0       0       0	    ; y < z
       0    (z y x) (z y x)	    ; y = z
       0    (z y x) (z y x)	    ; y > z

		;x < y   x = y   x > y   [x = z]
       (y z x) (z x y)    0	       ; y < z
       (y z x)    0    (x z y)	       ; y = z
       0    (y x z) (x z y)	       ; y > z

		;x < y   x = y   x > y   [x > z]
       (x y z) (x y z)    0	    ; y < z
       (x y z) (x y z)    0	    ; y = z
       0       0       0	    ; y > z
       ))

  (let ((z? #f) (z #f)) ; stored element from previous call
    (lambda (x y)
      (let ((c-xx (checked-value c x x))
            (c-yy (checked-value c y y))
            (c-xy (checked-value c x y))
            (c-yx (checked-value c y x)))
        (if (not (zero? c-xx))
            (error "compare error: not reflexive" c x))
        (if (not (zero? c-yy))
            (error "compare error: not reflexive" c y))
        (if (not (zero? (+ c-xy c-yx)))
            (error "compare error: not anti-symmetric" c x y))
        (if z?
            (let ((c-xz (checked-value c x z))
                  (c-zx (checked-value c z x))
                  (c-yz (checked-value c y z))
                  (c-zy (checked-value c z y)))
              (if (not (zero? (+ c-xz c-zx)))
                  (error "compare error: not anti-symmetric" c x z))
              (if (not (zero? (+ c-yz c-zy)))
                  (error "compare error: not anti-symmetric" c y z))
              (let ((ijk (vector-ref q (+ c-xy (* 3 c-yz) (* 9 c-xz) 13))))
                (if (list? ijk)
                    (apply error
                           "compare error: not transitive"
                           c
                           (map (lambda (i) (case i ((x) x) ((y) y) ((z) z)))
			     ijk)))))
	  (set! z? #t))
        (set! z (if (random-boolean) x y)) ; randomized testing
        c-xy))))


;;;; atomic types

(define (boolean-compare x y)
  (%type-check boolean-compare boolean? "boolean" x y)
  (if x
      (if y 0 1)
    (if y -1 0)))

(%define-by=/< char-compare		char=?      char<?	char?   "char")
(%define-by=/< char-compare-ci		char-ci=?   char-ci<?	char?   "char")
(%define-by=/< string-compare		string=?    string<?	string? "string")
(%define-by=/< string-compare-ci	string-ci=? string-ci<?	string? "string")

(define (symbol-compare x y)
  (%type-check symbol-compare symbol? "symbol" x y)
  (string-compare (symbol->string x) (symbol->string y)))

(%define-numeric-by=/< integer-compare	= < integer?  "integer")
(%define-numeric-by=/< rational-compare	= < rational? "rational")
(%define-numeric-by=/< real-compare	= < real?     "real")

(define (complex-compare x y)
  (%type-check complex-compare complex? "complex" x y)
  (real-compare (magnitude x) (magnitude y)))

(define (number-compare x y)
  (%type-check number-compare number? "number" x y)
  (if (or (complex? x) (complex? x))
      (complex-compare x y)
    (real-compare x y)))


;;;; compound data structures

(define (pair-compare-car compare)
  (lambda (x y)
    (compare (car x) (car y))))

(define (pair-compare-cdr compare)
  (lambda (x y)
    (compare (cdr x) (cdr y))))

(define pair-compare
  (case-lambda
    ((pair-compare-car pair-compare-cdr x y) ;dotted pair
     (refine-compare (pair-compare-car (car x) (car y))
                     (pair-compare-cdr (cdr x) (cdr y))))

    ((compare x y) ;possibly improper lists
     (cond-compare
      (((null? x) (null? y)) 0)
      (((pair? x) (pair? y))
       (compare              (car x) (car y))
       (pair-compare compare (cdr x) (cdr y)))
      (else                  (compare x y))))

    ((x y) ;for convenience
     (pair-compare default-compare x y))))

(define list-compare
  (case-lambda
    ((compare x y empty? head tail)
     (cond-compare
      (((empty? x) (empty? y)) 0)
      (else (compare              (head x) (head y))
            (list-compare compare (tail x) (tail y) empty? head tail))))

    ((        x y empty? head tail)
     (list-compare default-compare x y empty? head tail))
    ((compare x y              )
     (list-compare compare         x y null? car   cdr))
    ((        x y              )
     (list-compare default-compare x y null? car   cdr))))

(define list-compare-as-vector
  (case-lambda
    ((compare x y empty? head tail)
     (refine-compare
      (let compare-length ((x x) (y y))
        (cond-compare
         (((empty? x) (empty? y)) 0)
         (else (compare-length (tail x) (tail y)))))
      (list-compare compare x y empty? head tail)))

    ; for convenience
    ((        x y empty? head tail)
     (list-compare-as-vector default-compare x y empty? head tail))
    ((compare x y              )
     (list-compare-as-vector compare         x y null?  car  cdr))
    ((        x y              )
     (list-compare-as-vector default-compare x y null?  car  cdr))))

(define vector-compare
  (let ((= =))
    (case-lambda
      ((compare x y size ref)
       (let ((n (size x)) (m (size y)))
         (refine-compare
          (integer-compare n m)
          (let compare-rest ((i 0)) ; compare x[i..n-1] y[i..n-1]
            (if (= i n)
                0
                (refine-compare (compare (ref x i) (ref y i))
                                (compare-rest (+ i 1))))))))

      ; for convenience
      ((        x y size ref)
       (vector-compare default-compare x y size          ref))
      ((compare x y           )
       (vector-compare compare         x y vector-length vector-ref))
      ((        x y           )
       (vector-compare default-compare x y vector-length vector-ref)))))

(define vector-compare-as-list
  (let ((= =))
    (case-lambda
      ((compare x y size ref)
       (let ((nx (size x)) (ny (size y)))
         (let ((n (min nx ny)))
           (let compare-rest ((i 0)) ; compare x[i..n-1] y[i..n-1]
             (if (= i n)
                 (integer-compare nx ny)
                 (refine-compare (compare (ref x i) (ref y i))
                                 (compare-rest (+ i 1))))))))

      ; for convenience
      ((        x y size ref)
       (vector-compare-as-list default-compare x y size          ref))
      ((compare x y           )
       (vector-compare-as-list compare         x y vector-length vector-ref))
      ((        x y           )
       (vector-compare-as-list default-compare x y vector-length vector-ref)))))


;;;; constructors

(define-syntax refine-compare
  (syntax-rules ()
    ((refine-compare)
     0)
    ((refine-compare c1)
     c1)
    ((refine-compare c1 c2 cs ...)
     (if3 c1 -1 (refine-compare c2 cs ...) 1))))

(define-syntax select-compare
  (syntax-rules (else)
    ((_ x y clause ...)
     (let ((x-val x) (y-val y))
       (select-compare (x-val y-val clause ...))))

    ((_ (x y))	;used internally: (select-compare (x y clause ...))
     0)
    ((_ (x y (else c ...)))
     (refine-compare c ...))
    ((_ (x y (t? c ...) clause ...))
     (let ((t?-val t?))
       (let ((tx (t?-val x)) (ty (t?-val y)))
         (if tx
             (if ty (refine-compare c ...) -1)
	   (if ty 1 (select-compare (x y clause ...)))))))))

(define-syntax cond-compare
  (syntax-rules (else)
    ((_)
     0)
    ((_ (else cs ...))
     (refine-compare cs ...))
    ((_ ((tx ty) cs ...) clause ...)
     (let ((tx-val tx) (ty-val ty))
       (if tx-val
           (if ty-val (refine-compare cs ...) -1)
	 (if ty-val 1 (cond-compare clause ...)))))))


;;;; comparison branching

(define-syntax if3
  (syntax-rules ()
    ((_ ?c ?less ?equal ?greater)
     (case ?c
       ((-1) ?less)
       (( 0) ?equal)
       (( 1) ?greater)
       (else
	(assertion-violation 'if3
	  "comparison value not in {-1,0,1}"))))))

(define-syntax %if-rel?
  (syntax-rules ()
    ((_ func-name c-cases a-cases c consequence)
     (%if-rel? func-name c-cases a-cases c consequence (if #f #f)))
    ((_ func-name c-cases a-cases c consequence alternate)
     (case c
       (c-cases consequence)
       (a-cases alternate)
       (else
	(assertion-violation (quote func-name)
	  "comparison value not in {-1,0,1}"))))))

(define-syntax if=?
  (syntax-rules ()
    ((if=? arg ...)
     (%if-rel? if=? (0) (-1 1) arg ...))))

(define-syntax if<?
  (syntax-rules ()
    ((if<? arg ...)
     (%if-rel? if<? (-1) (0 1) arg ...))))

(define-syntax if>?
  (syntax-rules ()
    ((if>? arg ...)
     (%if-rel? if>? (1) (-1 0) arg ...))))

(define-syntax if<=?
  (syntax-rules ()
    ((if<=? arg ...)
     (%if-rel? if<=? (-1 0) (1) arg ...))))

(define-syntax if>=?
  (syntax-rules ()
    ((if>=? arg ...)
     (%if-rel? if>=? (0 1) (-1) arg ...))))

(define-syntax if-not=?
  (syntax-rules ()
    ((if-not=? arg ...)
     (%if-rel? if-not=? (-1 1) (0) arg ...))))


;;;; predicates from compare procedures

(define-syntax %define-rel?
  (syntax-rules ()
    ((%define-rel? rel? if-rel?)
     (define rel?
       (case-lambda
	(()        (lambda (x y) (if-rel? (default-compare x y) #t #f)))
	((compare) (lambda (x y) (if-rel? (compare         x y) #t #f)))
	((x y)                   (if-rel? (default-compare x y) #t #f))
	((compare x y)
	 (if (procedure? compare)
	     (if-rel? (compare x y) #t #f)
	   (assertion-violation (quote rel?)
	     "not a procedure (Did you mean rel/rel??): " compare))))))))

(%define-rel? =?    if=?)
(%define-rel? <?    if<?)
(%define-rel? >?    if>?)
(%define-rel? <=?   if<=?)
(%define-rel? >=?   if>=?)
(%define-rel? not=? if-not=?)

;;; --------------------------------------------------------------------

(define-syntax %define-rel1/rel2?
  (syntax-rules ()
    ((%define-rel1/rel2? rel1/rel2? if-rel1? if-rel2?)
     (define rel1/rel2?
       (case-lambda
	(()
	 (lambda (x y z)
	   (if-rel1? (default-compare x y)
		     (if-rel2? (default-compare y z) #t #f)
		     (%checked #f default-compare z))))
	((compare)
	 (lambda (x y z)
	   (if-rel1? (compare x y)
		     (if-rel2? (compare y z) #t #f)
		     (%checked #f compare z))))
	((x y z)
	 (if-rel1? (default-compare x y)
		   (if-rel2? (default-compare y z) #t #f)
		   (%checked #f default-compare z)))
	((compare x y z)
	 (if-rel1? (compare x y)
		   (if-rel2? (compare y z) #t #f)
		   (%checked #f compare z))))))))

(%define-rel1/rel2? </<?   if<?  if<?)
(%define-rel1/rel2? </<=?  if<?  if<=?)
(%define-rel1/rel2? <=/<?  if<=? if<?)
(%define-rel1/rel2? <=/<=? if<=? if<=?)
(%define-rel1/rel2? >/>?   if>?  if>?)
(%define-rel1/rel2? >/>=?  if>?  if>=?)
(%define-rel1/rel2? >=/>?  if>=? if>?)
(%define-rel1/rel2? >=/>=? if>=? if>=?)


;;;; constructors

(define-syntax %define-chain-rel?
  (syntax-rules ()
    ((%define-chain-rel? chain-rel? if-rel?)
     (define chain-rel?
       (case-lambda
         ((compare)
          #t)
         ((compare x1)
          (%checked #t compare x1))
         ((compare x1 x2)
          (if-rel? (compare x1 x2) #t #f))
         ((compare x1 x2 x3)
          (if-rel? (compare x1 x2)
                   (if-rel? (compare x2 x3) #t #f)
                   (%checked #f compare x3)))
         ((compare x1 x2 . x3+)
          (if-rel? (compare x1 x2)
                   (let chain? ((head x2) (tail x3+))
                     (if (null? tail)
                         #t
                         (if-rel? (compare head (car tail))
                                  (chain? (car tail) (cdr tail))
                                  (apply %checked #f
                                         compare (cdr tail)))))
                   (apply %checked #f compare x3+))))))))

(%define-chain-rel? chain=?  if=?)
(%define-chain-rel? chain<?  if<?)
(%define-chain-rel? chain>?  if>?)
(%define-chain-rel? chain<=? if<=?)
(%define-chain-rel? chain>=? if>=?)


; pairwise inequality

(define pairwise-not=?
  (let ((= =) (<= <=))
    (case-lambda
     ((compare)
      #t)
     ((compare x1)
      (%checked #t compare x1))
     ((compare x1 x2)
      (if-not=? (compare x1 x2) #t #f))
     ((compare x1 x2 x3)
      (if-not=? (compare x1 x2)
		(if-not=? (compare x2 x3)
			  (if-not=? (compare x1 x3) #t #f)
			  #f)
		(%checked #f compare x3)))
     ((compare . x1+)
      (let unequal? ((x x1+) (n (length x1+)) (unchecked? #t))
	(if (< n 2)
	    (if (and unchecked? (= n 1))
		(%checked #t compare (car x))
	      #t)
	  (let* ((i-pivot (random-integer n))
		 (x-pivot (list-ref x i-pivot)))
	    (let split ((i 0) (x x) (x< '()) (x> '()))
	      (if (null? x)
		  (and (unequal? x< (length x<) #f)
		       (unequal? x> (length x>) #f))
		(if (= i i-pivot)
		    (split (+ i 1) (cdr x) x< x>)
		  (if3 (compare (car x) x-pivot)
		       (split (+ i 1) (cdr x) (cons (car x) x<) x>)
		       (if unchecked?
			   (apply %checked #f compare (cdr x))
			 #f)
		       (split (+ i 1) (cdr x) x< (cons (car x) x>)))))))))))))


; min/max

(define min-compare
  (case-lambda
    ((compare x1)
     (%checked x1 compare x1))
    ((compare x1 x2)
     (if<=? (compare x1 x2) x1 x2))
    ((compare x1 x2 x3)
     (if<=? (compare x1 x2)
            (if<=? (compare x1 x3) x1 x3)
            (if<=? (compare x2 x3) x2 x3)))
    ((compare x1 x2 x3 x4)
     (if<=? (compare x1 x2)
            (if<=? (compare x1 x3)
                   (if<=? (compare x1 x4) x1 x4)
                   (if<=? (compare x3 x4) x3 x4))
            (if<=? (compare x2 x3)
                   (if<=? (compare x2 x4) x2 x4)
                   (if<=? (compare x3 x4) x3 x4))))
    ((compare x1 x2 . x3+)
     (let min ((xmin (if<=? (compare x1 x2) x1 x2)) (xs x3+))
       (if (null? xs)
           xmin
           (min (if<=? (compare xmin (car xs)) xmin (car xs))
                (cdr xs)))))))

(define max-compare
  (case-lambda
    ((compare x1)
     (%checked x1 compare x1))
    ((compare x1 x2)
     (if>=? (compare x1 x2) x1 x2))
    ((compare x1 x2 x3)
     (if>=? (compare x1 x2)
            (if>=? (compare x1 x3) x1 x3)
            (if>=? (compare x2 x3) x2 x3)))
    ((compare x1 x2 x3 x4)
     (if>=? (compare x1 x2)
            (if>=? (compare x1 x3)
                   (if>=? (compare x1 x4) x1 x4)
                   (if>=? (compare x3 x4) x3 x4))
            (if>=? (compare x2 x3)
                   (if>=? (compare x2 x4) x2 x4)
                   (if>=? (compare x3 x4) x3 x4))))
    ((compare x1 x2 . x3+)
     (let max ((xmax (if>=? (compare x1 x2) x1 x2)) (xs x3+))
       (if (null? xs)
           xmax
           (max (if>=? (compare xmax (car xs)) xmax (car xs))
                (cdr xs)))))))


; kth-largest

(define kth-largest
  (let ((= =) (< <))
    (case-lambda
      ((compare k x0)
       (case (mod k 1)
         ((0)  (%checked x0 compare x0))
         (else (error "bad index" k))))
      ((compare k x0 x1)
       (case (mod k 2)
         ((0) (if<=? (compare x0 x1) x0 x1))
         ((1) (if<=? (compare x0 x1) x1 x0))
         (else (error "bad index" k))))
      ((compare k x0 x1 x2)
       (case (mod k 3)
         ((0) (if<=? (compare x0 x1)
                     (if<=? (compare x0 x2) x0 x2)
                     (if<=? (compare x1 x2) x1 x2)))
         ((1) (if3 (compare x0 x1)
                   (if<=? (compare x1 x2)
                          x1
                          (if<=? (compare x0 x2) x2 x0))
                   (if<=? (compare x0 x2) x1 x0)
                   (if<=? (compare x0 x2)
                          x0
                          (if<=? (compare x1 x2) x2 x1))))
         ((2) (if<=? (compare x0 x1)
                     (if<=? (compare x1 x2) x2 x1)
                     (if<=? (compare x0 x2) x2 x0)))
         (else (error "bad index" k))))
      ((compare k x0 . x1+) ; |x1+| >= 1
       (if (not (and (integer? k) (exact? k)))
           (error "bad index" k))
       (let ((n (+ 1 (length x1+))))
         (let kth ((k   (mod k n))
                   (n   n)  ; = |x|
                   (rev #t) ; are x<, x=, x> reversed?
                   (x   (cons x0 x1+)))
           (let ((pivot (list-ref x (random-integer n))))
             (let split ((x x) (x< '()) (n< 0) (x= '()) (n= 0) (x> '()) (n> 0))
               (if (null? x)
                   (cond
                     ((< k n<)
                      (kth k n< (not rev) x<))
                     ((< k (+ n< n=))
                      (if rev
                          (list-ref x= (- (- n= 1) (- k n<)))
                          (list-ref x= (- k n<))))
                     (else
                      (kth (- k (+ n< n=)) n> (not rev) x>)))
                   (if3 (compare (car x) pivot)
                        (split (cdr x) (cons (car x) x<) (+ n< 1) x= n= x> n>)
                        (split (cdr x) x< n< (cons (car x) x=) (+ n= 1) x> n>)
                        (split (cdr x) x< n< x= n= (cons (car x) x>) (+ n> 1))))))))))))


; compare functions from predicates

(define compare-by<
  (case-lambda
    ((lt)     (lambda (x y) (if (lt x y) -1 (if (lt y x)  1 0))))
    ((lt x y)               (if (lt x y) -1 (if (lt y x)  1 0)))))

(define compare-by>
  (case-lambda
    ((gt)     (lambda (x y) (if (gt x y) 1 (if (gt y x)  -1 0))))
    ((gt x y)               (if (gt x y) 1 (if (gt y x)  -1 0)))))

(define compare-by<=
  (case-lambda
    ((le)     (lambda (x y) (if (le x y) (if (le y x) 0 -1) 1)))
    ((le x y)               (if (le x y) (if (le y x) 0 -1) 1))))

(define compare-by>=
  (case-lambda
    ((ge)     (lambda (x y) (if (ge x y) (if (ge y x) 0 1) -1)))
    ((ge x y)               (if (ge x y) (if (ge y x) 0 1) -1))))

(define compare-by=/<
  (case-lambda
    ((eq lt)     (lambda (x y) (if (eq x y) 0 (if (lt x y) -1 1))))
    ((eq lt x y)               (if (eq x y) 0 (if (lt x y) -1 1)))))

(define compare-by=/>
  (case-lambda
    ((eq gt)     (lambda (x y) (if (eq x y) 0 (if (gt x y) 1 -1))))
    ((eq gt x y)               (if (eq x y) 0 (if (gt x y) 1 -1)))))


;;;; done

)

;;; end of file
