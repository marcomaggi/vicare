;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

;;; SRFI-14 character-sets library
;;;
;;; - Ported from MIT Scheme runtime by Brian D. Carlstrom.
;;; - Massively rehacked & extended by Olin Shivers 6/98.
;;; - Massively redesigned and rehacked 5/2000 during SRFI process.
;;; At this point, the code bears the following relationship to the
;;; MIT Scheme code: "This is my grandfather's axe. My father replaced
;;; the head, and I have replaced the handle." Nonetheless, we preserve
;;; the MIT Scheme copyright:
;;;     Copyright (c) 1988-1995 Massachusetts Institute of Technology

;;;Copyright notice
;;;
;;;Copyright (c) 1988-1995 Massachusetts Institute of Technology
;;;
;;;This  material   was  developed   by  the   Scheme  project   at  the
;;;Massachusetts  Institute  of  Technology,  Department  of  Electrical
;;;Engineering and Computer Science.  Permission to copy and modify this
;;;software, to redistribute either the  original software or a modified
;;;version, and to use this software for any purpose is granted, subject
;;;to the following restrictions and understandings.
;;;
;;;1. Any copy made of this  software must include this copyright notice
;;;in full.
;;;
;;;2. Users  of this software  agree to make  their best efforts  (a) to
;;;return to the MIT Scheme  project any improvements or extensions that
;;;they make, so that these may  be included in future releases; and (b)
;;;to inform MIT of noteworthy uses of this software.
;;;
;;;3.  All materials  developed  as a  consequence of  the  use of  this
;;;software  shall duly  acknowledge such  use, in  accordance with  the
;;;usual standards of acknowledging credit in academic research.
;;;
;;;4. MIT has made no warrantee  or representation that the operation of
;;;this software will  be error-free, and MIT is under  no obligation to
;;;provide any services, by way of maintenance, update, or otherwise.
;;;
;;;5.  In  conjunction  with  products  arising from  the  use  of  this
;;;material, there  shall be  no use  of the  name of  the Massachusetts
;;;Institute  of  Technology  nor  of  any  adaptation  thereof  in  any
;;;advertising, promotional,  or sales literature without  prior written
;;;consent from MIT in each case.


#!r6rs
(library (srfi :14 char-sets)
  (export
    ;; predicates & comparison
    char-set? char-set= char-set<= char-set-hash

    ;; iterating over character sets
    char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
    char-set-fold char-set-unfold char-set-unfold!
    char-set-for-each char-set-map

    ;; creating character sets
    char-set-copy char-set
    list->char-set  string->char-set
    list->char-set! string->char-set!
    char-set-filter  ucs-range->char-set
    char-set-filter! ucs-range->char-set!
    ->char-set

    ;; querying character sets
    char-set->list char-set->string
    char-set-size char-set-count char-set-contains?
    char-set-every char-set-any

    ;; character-set algebra
    char-set-adjoin  char-set-delete
    char-set-adjoin! char-set-delete!
    char-set-complement  char-set-union  char-set-intersection
    char-set-complement! char-set-union! char-set-intersection!
    char-set-difference  char-set-xor  char-set-diff+intersection
    char-set-difference! char-set-xor! char-set-diff+intersection!

    ;; standard character sets
    char-set:lower-case  char-set:upper-case  char-set:title-case
    char-set:letter      char-set:digit       char-set:letter+digit
    char-set:graphic     char-set:printing    char-set:whitespace
    char-set:iso-control char-set:punctuation char-set:symbol
    char-set:hex-digit   char-set:blank       char-set:ascii
    char-set:empty       char-set:full)
  (import (rnrs)
    (only (vicare)
	  module)
    (rnrs mutable-strings)
    (rnrs r5rs)
    (vicare arguments validation))


;;;; helpers

;; *FIXME* These two seem incorrect.  (Derick Eddington)
(define (%latin1->char i)
  (integer->char i))
(define (%char->latin1 c)
  (char->integer c))

(define-argument-validation (char-set who obj)
  (char-set? obj)
  (assertion-violation who "expected char-set as argument" obj))


(define-record-type (:char-set make-char-set char-set?)
  (fields (immutable str char-set-str)))

(define (%make-default-base-cs)
  (make-char-set (make-string 256 (%latin1->char 0))))

;;; These internal functions hide a lot of the dependency on the
;;; underlying string representation of char sets. They should be
;;; inlined if possible.

(define (si=0? s i) (zero? (%char->latin1 (string-ref s i))))
(define (si=1? s i) (not (si=0? s i)))
(define c0 (%latin1->char 0))
(define c1 (%latin1->char 1))
(define (si s i) (%char->latin1 (string-ref s i)))
(define (%set0! s i) (string-set! s i c0))
(define (%set1! s i) (string-set! s i c1))

;;; These do various "s[i] := s[i] op val" operations -- see
;;; %CHAR-SET-ALGEBRA. They are used to implement the various
;;; set-algebra procedures.
(define (setv!   s i v) (string-set! s i (%latin1->char v))) ; SET to a Value.
(define (%not!   s i v) (setv! s i (- 1 v)))
(define (%and!   s i v) (if (zero? v) (%set0! s i)))
(define (%or!    s i v) (if (not (zero? v)) (%set1! s i)))
(define (%minus! s i v) (if (not (zero? v)) (%set0! s i)))
(define (%xor!   s i v) (if (not (zero? v)) (setv! s i (- 1 (si s i)))))


(define (char-set-copy cs)
  (define who 'char-set-copy)
  (with-arguments-validation (who)
      ((char-set	cs))
    (make-char-set (string-copy ($:char-set-str cs)))))

(define char-set=
  (case-lambda
   (()
    #t)
   ((cs)
    (define who 'char-set=)
    (with-arguments-validation (who)
	((char-set	cs))
      #t))
   ((cs1 cs2)
    (define who 'char-set=)
    (with-arguments-validation (who)
	((char-set	cs1)
	 (char-set	cs2))
      (string=? ($:char-set-str cs1)
		($:char-set-str cs2))))
   ((cs1 . rest)
    (if (null? rest)
	(char-set= cs1)
      (let ((cs2 (car rest)))
	(and (char-set= cs1 cs2)
	     (apply char-set= cs2 (cdr rest))))))))

(define char-set<=
  (case-lambda
   (()
    #t)
   ((cs)
    (define who 'char-set<=)
    (with-arguments-validation (who)
	((char-set	cs))
      #t))
   ((cs1 cs2)
    (define who 'char-set<=)
    (with-arguments-validation (who)
	((char-set	cs1)
	 (char-set	cs2))
      (let ((str1 ($:char-set-str cs1))
	    (str2 ($:char-set-str cs2)))
	(or (eq? str1 str2)
	    (let next-char ((i 255))
	      (or (< i 0)
		  (and (<= (si str1 i)
			   (si str2 i))
		       (next-char (- i 1)))))))))
   ((cs1 . rest)
    (if (null? rest)
	(char-set<= cs1)
      (let ((cs2 (car rest)))
	(and (char-set<= cs1 cs2)
	     (apply char-set<= cs2 (cdr rest))))))))


;;;; Hash
;;
;;Compute (c + 37 c + 37^2 c  + ...) modulo BOUND, with sleaze thrown in
;;to keep  the intermediate values  small.  (We do the  calculation with
;;just enough  bits to represent  BOUND, masking  off high bits  at each
;;step in  calculation.  If this  screws up any important  properties of
;;the hash function I'd like to hear about it. -Olin)
;;
;;If you  keep BOUND  small enough,  the intermediate  calculations will
;;always be  fixnums. How  small is dependent  on the  underlying Scheme
;;system; we use a default BOUND of 2^22 = 4194304, which should hack it
;;in Schemes that give you at least 29 signed bits for fixnums. The core
;;calculation that you don't want to overflow is, worst case,
;;
;;   (+ 65535 (* 37 (- bound 1)))
;;
;;where 65535 is the max character code.  Choose the default BOUND to be
;;the biggest  power of two that  won't cause this expression  to fixnum
;;overflow, and everything will be copacetic.

(define char-set-hash
  (case-lambda
   ((cs)
    (char-set-hash cs 4194304))
   ((cs bound)
    (define who 'char-set-hash)
    (with-arguments-validation (who)
	((char-set		cs)
	 (non-negative-fixnum	bound))
      (let* ((bound (if (zero? bound) 4194304 bound)) ; 0 means default.
	     (s     ($:char-set-str cs))
	     ;; Compute a 111...1 mask that will cover BOUND-1:
	     (mask  (let lp ((i #x10000)) ; Let's skip first 16 iterations, eh?
		      (if (>= i bound)
			  (- i 1)
			(lp (+ i i))))))
	(let lp ((i 255) (ans 0))
	  (if (< i 0) (modulo ans bound)
	    (lp (- i 1)
		(if (si=0? s i) ans
		  (bitwise-and mask (+ (* 37 ans) i)))))))))))

(define (char-set-contains? cs char)
  (define who 'char-set-contains?)
  (with-arguments-validation (who)
      ((char-set	cs)
       (char		char))
    (si=1? ($:char-set-str cs)
	   (%char->latin1 char))))

(define (char-set-size cs)
  (define who 'char-set-size)
  (with-arguments-validation (who)
      ((char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i    255)
	       (size 0))
	(if (< i 0)
	    size
	  (lp (- i 1) (+ size (si s i))))))))

(define (char-set-count pred cs)
  (define who 'char-set-count)
  (with-arguments-validation (who)
      ((procedure	pred)
       (char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i     255)
	       (count 0))
	(if (< i 0) count
	  (lp (- i 1)
	      (if (and (si=1? s i) (pred (%latin1->char i)))
		  (+ count 1)
		count)))))))


;;;; adjoin & delete

(module (char-set-adjoin
	 char-set-adjoin!
	 char-set-delete
	 char-set-delete!)

  (define-argument-validation (list-of-chars who obj)
    (or (null? obj)
	(and (list? obj)
	     (for-all char? obj)))
    (assertion-violation who "expected list of chars" obj))

  (define (%set-char-set set who cs chars)
    (with-arguments-validation (who)
	((char-set	cs))
      (let ((s (string-copy ($:char-set-str cs))))
	(for-each (lambda (c)
		    (set s (%char->latin1 c)))
	  chars)
	(make-char-set s))))

  (define (%set-char-set! set who cs chars)
    (with-arguments-validation (who)
	((char-set	cs))
      (let ((s ($:char-set-str cs)))
	(for-each (lambda (c)
		    (set s (%char->latin1 c)))
	  chars))
      cs))

  (define (char-set-adjoin cs . chars)
    (%set-char-set  %set1! 'char-set-adjoin cs chars))

  (define (char-set-adjoin! cs . chars)
    (%set-char-set! %set1! 'char-set-adjoin! cs chars))

  (define (char-set-delete cs . chars)
    (%set-char-set  %set0! 'char-set-delete cs chars))

  (define (char-set-delete! cs . chars)
    (%set-char-set! %set0! 'char-set-delete! cs chars))

  #| end of module |# )


;;;; cursors
;;
;;Simple implementation.  A cursors  is an integer  index into  the mark
;;vector, and -1 for the end-of-char-set cursor.
;;
;;If we represented  char sets as a  bit set, we could  do the following
;;trick to pick the lowest bit out of the set:
;;
;;   (count-bits (xor (- cset 1) cset))
;;
;;(But first mask out the bits already scanned by the cursor first.)

(module (char-set-cursor
	 end-of-char-set?
	 char-set-ref
	 char-set-cursor-next)

  (define (char-set-cursor cset)
    (%char-set-cursor-next cset 256 'char-set-cursor))

  (define (end-of-char-set? cursor)
    (fxnegative? cursor))

  (define (char-set-ref cset cursor)
    (%latin1->char cursor))

  (define (char-set-cursor-next cs cursor)
    (define who 'char-set-cursor-next)
    (with-arguments-validation (who)
	((cursor	cursor))
      (%char-set-cursor-next cs cursor who)))

  (define-argument-validation (cursor who obj)
    (and (fixnum? obj)
	 (fx<=? 0 obj 255))
    (assertion-violation who "invalid cursor value" obj))

  (define (%char-set-cursor-next cs cursor who)
    (with-arguments-validation (who)
	((char-set	cs))
      (let ((s ($:char-set-str cs)))
	(let lp ((cur cursor))
	  (let ((cur (- cur 1)))
	    (if (or (< cur 0) (si=1? s cur))
		cur
	      (lp cur)))))))

  #| end of module |# )


;;;; for-each map fold unfold every any

(define (char-set-for-each proc cs)
  (define who 'char-set-for-each)
  (with-arguments-validation (who)
      ((procedure	proc)
       (char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i 255))
	(when (>= i 0)
	  (when (si=1? s i)
	    (proc (%latin1->char i)))
	  (lp (- i 1)))))))

(define (char-set-map proc cs)
  (define who 'char-set-map)
  (with-arguments-validation (who)
      ((procedure	proc)
       (char-set	cs))
    (let ((s   ($:char-set-str cs))
	  (ans (make-string 256 c0)))
      (let lp ((i 255))
	(when (>= i 0)
	  (when (si=1? s i)
	    (%set1! ans (%char->latin1 (proc (%latin1->char i)))))
	  (lp (- i 1))))
      (make-char-set ans))))

(define (char-set-fold kons knil cs)
  (define who 'char-set-fold)
  (with-arguments-validation (who)
      ((procedure	kons)
       (char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i   255)
	       (ans knil))
	(if (< i 0) ans
	  (lp (- i 1)
	      (if (si=0? s i)
		  ans
		(kons (%latin1->char i) ans))))))))

(define (char-set-every pred cs)
  (define who 'char-set-every)
  (with-arguments-validation (who)
      ((procedure	pred)
       (char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i 255))
	(or (< i 0)
	    (and (or (si=0? s i)
		     (pred (%latin1->char i)))
		 (lp (- i 1))))))))

(define (char-set-any pred cs)
  (define who 'char-set-any)
  (with-arguments-validation (who)
      ((procedure	pred)
       (char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i 255))
	(and (>= i 0)
	     (or (and (si=1? s i)
		      (pred (%latin1->char i)))
		 (lp (- i 1))))))))

(module (char-set-unfold
	 char-set-unfold!)

  (define char-set-unfold
    (case-lambda
     ((p f g seed)
      (char-set-unfold p f g seed (%make-default-base-cs)))
     ((p f g seed base-cs)
      (define who 'char-set-unfold)
      (with-arguments-validation (who)
	  ((char-set	base-cs))
	(let ((bs ($:char-set-str base-cs)))
	  (%char-set-unfold! who p f g bs seed)
	  (make-char-set bs))))))

  (define (char-set-unfold! p f g seed base-cs)
    (define who 'char-set-unfold!)
    (with-arguments-validation (who)
	((char-set	base-cs))
      (%char-set-unfold! who p f g
			 ($:char-set-str base-cs)
			 seed)
      base-cs))

  (define (%char-set-unfold! who p f g s seed)
    (with-arguments-validation (who)
	((procedure	p)
	 (procedure	f)
	 (procedure	g))
      (let lp ((seed seed))
	(when (not (p seed))		    ; P says we are done.
	  (%set1! s (%char->latin1 (f seed))) ; Add (F SEED) to set.
	  (lp (g seed))))))		      ; Loop on (G SEED).

  #| end of module |# )


;;;; list <--> char-set

(module (char-set
	 list->char-set
	 list->char-set!)

  (define (char-set . chars)
    (let ((s (make-string 256 c0)))
      (%list->char-set! chars s)
      (make-char-set s)))

  (define list->char-set
    (case-lambda
     ((chars)
      (list->char-set chars (%make-default-base-cs)))
     ((chars base-cs)
      (define who 'list->char-set)
      (with-arguments-validation (who)
	  ((char-set	base-cs))
	(let ((bs ($:char-set-str base-cs)))
	  (%list->char-set! chars bs)
	  (make-char-set bs))))))

  (define (list->char-set! chars base-cs)
    (define who 'list->char-set!)
    (with-arguments-validation (who)
	((list-of-chars	chars)
	 (char-set	base-cs))
      (%list->char-set! chars ($:char-set-str base-cs))
      base-cs))

  (define-argument-validation (list-of-chars who obj)
    (or (null? obj)
	(and (list? obj)
	     (for-all char? obj)))
    (assertion-violation who "expected list of chars" obj))

  (define (%list->char-set! chars s)
    (for-each (lambda (char)
		(%set1! s (%char->latin1 char)))
      chars))

  #| end of module |# )

(define (char-set->list cs)
  (define who 'char-set->list)
  (with-arguments-validation (who)
      ((char-set	cs))
    (let ((s ($:char-set-str cs)))
      (let lp ((i   255)
	       (ans '()))
	(if (< i 0)
	    ans
	  (lp (- i 1)
	      (if (si=0? s i)
		  ans
		(cons (%latin1->char i) ans))))))))


;;;; string <--> char-set

(module (string->char-set
	 string->char-set!)

  (define string->char-set
    (case-lambda
     ((str)
      (string->char-set str (%make-default-base-cs)))
     ((str base-cs)
      (define who 'string->char-set)
      (with-arguments-validation (who)
	  ((char-set	base-cs))
	(let ((bs ($:char-set-str base-cs)))
	  (%string->char-set! str bs who)
	  (make-char-set bs))))))

  (define (string->char-set! str base-cs)
    (define who 'string->char-set!)
    (with-arguments-validation (who)
	((char-set	base-cs))
      (%string->char-set! str ($:char-set-str base-cs) who)
      base-cs))

  (define (%string->char-set! str bs who)
    (with-arguments-validation (who)
	((string	str))
      (do ((i (- (string-length str) 1) (- i 1)))
	  ((< i 0))
	(%set1! bs (%char->latin1 (string-ref str i))))))

  #| end of module |# )

(define (char-set->string cs)
  (define who 'char-set->string)
  (with-arguments-validation (who)
      ((char-set	cs))
    (let* ((s   ($:char-set-str cs))
	   (ans (make-string (char-set-size cs))))
      (let lp ((i 255) (j 0))
	(if (< i 0) ans
	  (let ((j (if (si=0? s i) j
		     (begin
		       (string-set! ans j (%latin1->char i))
		       (+ j 1)))))
	    (lp (- i 1) j)))))))


;;;; -- UCS-range -> char-set

(module (ucs-range->char-set
	 ucs-range->char-set!)

  (define ucs-range->char-set
    (case-lambda
     ((lower upper)
      (ucs-range->char-set lower upper #f (%make-default-base-cs)))
     ((lower upper error?)
      (ucs-range->char-set lower upper error? (%make-default-base-cs)))
     ((lower upper error? base-cs)
      (define who 'ucs-range->char-set)
      (with-arguments-validation (who)
	  ((char-set	base-cs))
	(let ((bs ($:char-set-str base-cs)))
	  (%ucs-range->char-set! lower upper error? bs who)
	  (make-char-set bs))))))

  (define (ucs-range->char-set! lower upper error? base-cs)
    (define who 'ucs-range->char-set!)
    (with-arguments-validation (who)
	((char-set	base-cs))
      (%ucs-range->char-set! lower upper error?
			     ($:char-set-str base-cs)
			     ucs-range->char-set)
      base-cs))

  (define (%ucs-range->char-set! lower upper error? bs proc)
    (define who '%ucs-range->char-set!)
    (define-argument-validation (lower who obj)
      (and (fixnum? obj)
	   (fx<=? 0 obj))
      (assertion-violation who "invalid lower bound" obj))
    (define-argument-validation (upper who obj)
      (and (fixnum? obj)
	   (fx<=? lower obj))
      (assertion-violation who "invalid upper bound" obj))
    (with-arguments-validation (who)
	((lower	lower)
	 (upper	upper))
      (when (and (< lower upper)
		 (< 256   upper)
		 error?)
	(error who
	  "requested UCS range contains unavailable characters, \
         this implementation only supports Latin-1"
	  proc lower upper))
      (let lp ((i (- (min upper 256) 1)))
	(when (<= lower i)
	  (%set1! bs i)
	  (lp (- i 1))))))

  #| end of module |# )


;;;; predicate -> char-set

(module (char-set-filter
	 char-set-filter!)

  (define char-set-filter
    (case-lambda
     ((predicate domain)
      (char-set-filter predicate domain (%make-default-base-cs)))
     ((predicate domain base-cs)
      (define who 'char-set-filter)
      (with-arguments-validation (who)
	  ((char-set	domain)
	   (char-set	base-cs))
	(let ((bs ($:char-set-str base-cs)))
	  (%char-set-filter! predicate ($:char-set-str domain) bs who)
	  (make-char-set bs))))))

  (define (char-set-filter! predicate domain base-cs)
    (define who 'char-set-filter!)
    (with-arguments-validation (who)
	((char-set	domain)
	 (char-set	base-cs))
      (%char-set-filter! predicate ($:char-set-str domain) ($:char-set-str base-cs) who)
      base-cs))

  (define (%char-set-filter! pred ds bs who)
    (with-arguments-validation (who)
	((procedure	pred))
      (let lp ((i 255))
	(when (>= i 0)
	  (when (and (si=1? ds i)
		     (pred (%latin1->char i)))
	    (%set1! bs i))
	  (lp (- i 1))))))

  #| end of module |# )


;;;; {string, char, char-set, char predicate} -> char-set

(define (->char-set x)
  (define who '->char-set)
  (cond ((char-set? x)
	 x)
	((string? x)
	 (string->char-set x))
	((char? x)
	 (char-set x))
	(else
	 (error who "not a charset, string or char" x))))


;;;; set algebra
;;
;;The exported ! procs are "linear update" -- allowed, but not required,
;;to side-effect  their first argument  when computing their  result. In
;;other words, you must use them  as if they were completely functional,
;;just like their  non-! counterparts, and you  must additionally ensure
;;that their first arguments are "dead" at the point of call. In return,
;;we promise a more efficient result, plus allowing you to always assume
;;char-sets are unchangeable values.

;;; Apply P to each index and its char code in S: (P I VAL).
;;; Used by the set-algebra ops.

(define (%string-iter p s)
  (let lp ((i (- (string-length s) 1)))
    (cond ((>= i 0)
	   (p i (%char->latin1 (string-ref s i)))
	   (lp (- i 1))))))

;;; String S represents some initial char-set. (OP s i val) does some
;;; kind of s[i] := s[i] op val update. Do
;;;     S := S OP CSETi
;;; for all the char-sets in the list CSETS. The n-ary set-algebra ops
;;; all use this internal proc.

(define (%char-set-algebra s csets op who)
  (for-each (lambda (cs)
	      (with-arguments-validation (who)
		  ((char-set	cs))
		(let ((s2 ($:char-set-str cs)))
		  (let lp ((i 255))
		    (when (>= i 0)
		      (op s i (si s2 i))
		      (lp (- i 1)))))))
	    csets))


;;;; complement

(define (char-set-complement cs)
  (define who 'char-set-complement)
  (with-arguments-validation (who)
      ((char-set	cs))
    (let ((s ($:char-set-str cs))
	  (ans (make-string 256)))
      (%string-iter (lambda (i v) (%not! ans i v)) s)
      (make-char-set ans))))

(define (char-set-complement! cs)
  (define who 'char-set-complement!)
  (with-arguments-validation (who)
      ((char-set	cs))
    (let ((s ($:char-set-str cs)))
      (%string-iter (lambda (i v) (%not! s i v)) s))
    cs))


;;;; union

(define (char-set-union! cs . csets)
  (define who 'char-set-union!)
  (with-arguments-validation (who)
      ((char-set	cs))
    (%char-set-algebra ($:char-set-str cs) csets %or! who)
    cs))

(define (char-set-union . csets)
  (define who 'char-set-union)
  (if (pair? csets)
      (let ((cs (car csets)))
	(with-arguments-validation (who)
	    ((char-set	cs))
	  (let ((s (string-copy ($:char-set-str cs))))
	    (%char-set-algebra s (cdr csets) %or! who)
	    (make-char-set s))))
    (char-set-copy char-set:empty)))


;;;; intersection

(define (char-set-intersection! cs . csets)
  (define who 'char-set-intersection!)
  (with-arguments-validation (who)
      ((char-set	cs))
    (%char-set-algebra ($:char-set-str cs) csets %and! who)
    cs))

(define (char-set-intersection . csets)
  (define who 'char-set-intersection)
  (if (pair? csets)
      (let ((cs (car csets)))
	(with-arguments-validation (who)
	    ((char-set	cs))
	  (let ((s (string-copy ($:char-set-str cs))))
	    (%char-set-algebra s (cdr csets) %and! who)
	    (make-char-set s))))
    (char-set-copy char-set:full)))


;;;; difference

(define (char-set-difference! cs . csets)
  (define who 'char-set-difference!)
  (with-arguments-validation (who)
      ((char-set	cs))
    (%char-set-algebra ($:char-set-str cs) csets %minus! who)
    cs))

(define (char-set-difference cs1 . csets)
  (define who 'char-set-difference)
  (with-arguments-validation (who)
      ((char-set	cs1))
    (if (pair? csets)
	(let ((s (string-copy ($:char-set-str cs1))))
	  (%char-set-algebra s csets %minus! who)
	  (make-char-set s))
      (char-set-copy cs1))))


;;;; xor

(define (char-set-xor! cs . csets)
  (define who 'char-set-xor!)
  (with-arguments-validation (who)
      ((char-set	cs))
    (%char-set-algebra ($:char-set-str cs) csets %xor! who)
    cs))

(define (char-set-xor . csets)
  (define who 'char-set-xor)
  (if (pair? csets)
      (let ((cs (car csets)))
	(with-arguments-validation (who)
	    ((char-set	cs))
	  (let ((s (string-copy ($:char-set-str cs))))
	    (%char-set-algebra s (cdr csets) %xor! who)
	    (make-char-set s))))
    (char-set-copy char-set:empty)))


;;;; difference & intersection

(module (char-set-diff+intersection
	 char-set-diff+intersection!)

  (define (char-set-diff+intersection! cs1 cs2 . csets)
    (define who 'char-set-diff+intersection!)
    (with-arguments-validation (who)
	((char-set	cs1)
	 (char-set	cs2))
      (let ((s1 ($:char-set-str cs1))
	    (s2 ($:char-set-str cs2)))
	(%string-iter (lambda (i v)
			(if (zero? v)
			    (%set0! s2 i)
			  (if (si=1? s2 i)
			      (%set0! s1 i))))
		      s1)
	(%char-set-diff+intersection! s1 s2 csets who))
      (values cs1 cs2)))

  (define (char-set-diff+intersection cs1 . csets)
    (define who 'char-set-diff+intersection)
    (with-arguments-validation (who)
	((char-set	cs1))
      (let ((diff (string-copy ($:char-set-str cs1)))
	    (int  (make-string 256 c0)))
	(%char-set-diff+intersection! diff int csets who)
	(values (make-char-set diff) (make-char-set int)))))

  (define (%char-set-diff+intersection! diff int csets who)
    (for-each (lambda (cs)
		(with-arguments-validation (who)
		    ((char-set	cs))
		  (%string-iter (lambda (i v)
				  (if (not (zero? v))
				      (when (si=1? diff i)
					(%set0! diff i)
					(%set1! int  i))))
				($:char-set-str cs))))
      csets))

  #| end of module |# )


;;;; System character sets
;;
;;These definitions are for Latin-1.
;;
;;If  your  Scheme implementation  allows  you  to mark  the  underlying
;;strings as immutable, you  should do so -- it would  be very, very bad
;;if a client's buggy code corrupted these constants.

(define char-set:empty
  (char-set))

(define char-set:full
  (char-set-complement char-set:empty))

(define char-set:lower-case
  (let* ((a-z    (ucs-range->char-set #x61 #x7B))
	 (latin1 (ucs-range->char-set! #xdf #xf7  #t a-z))
	 (latin2 (ucs-range->char-set! #xf8 #x100 #t latin1)))
    (char-set-adjoin! latin2 (%latin1->char #xb5))))

(define char-set:upper-case
  (let ((A-Z (ucs-range->char-set #x41 #x5B)))
    ;; Add in the Latin-1 upper-case chars.
    (ucs-range->char-set! #xd8 #xdf #t
			  (ucs-range->char-set! #xc0 #xd7 #t A-Z))))

(define char-set:title-case
  char-set:empty)

(define char-set:letter
  (let ((u/l (char-set-union char-set:upper-case char-set:lower-case)))
    (char-set-adjoin! u/l
		      (%latin1->char #xaa)	; FEMININE ORDINAL INDICATOR
		      (%latin1->char #xba))))	; MASCULINE ORDINAL INDICATOR

(define char-set:digit
  (string->char-set "0123456789"))

(define char-set:hex-digit
  (string->char-set "0123456789abcdefABCDEF"))

(define char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))

(define char-set:punctuation
  (let ((ascii (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))
	(latin-1-chars (map %latin1->char '(#xA1 ; INVERTED EXCLAMATION MARK
					    #xAB ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
					    #xAD ; SOFT HYPHEN
					    #xB7 ; MIDDLE DOT
					    #xBB ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
					    #xBF)))) ; INVERTED QUESTION MARK
    (list->char-set! latin-1-chars ascii)))

(define char-set:symbol
  (let ((ascii (string->char-set "$+<=>^`|~"))
	(latin-1-chars (map %latin1->char '(#x00A2 ; CENT SIGN
					    #x00A3 ; POUND SIGN
					    #x00A4 ; CURRENCY SIGN
					    #x00A5 ; YEN SIGN
					    #x00A6 ; BROKEN BAR
					    #x00A7 ; SECTION SIGN
					    #x00A8 ; DIAERESIS
					    #x00A9 ; COPYRIGHT SIGN
					    #x00AC ; NOT SIGN
					    #x00AE ; REGISTERED SIGN
					    #x00AF ; MACRON
					    #x00B0 ; DEGREE SIGN
					    #x00B1 ; PLUS-MINUS SIGN
					    #x00B4 ; ACUTE ACCENT
					    #x00B6 ; PILCROW SIGN
					    #x00B8 ; CEDILLA
					    #x00D7 ; MULTIPLICATION SIGN
					    #x00F7)))) ; DIVISION SIGN
    (list->char-set! latin-1-chars ascii)))


(define char-set:graphic
  (char-set-union char-set:letter+digit char-set:punctuation char-set:symbol))

(define char-set:whitespace
  (list->char-set (map %latin1->char '(#x09 ; HORIZONTAL TABULATION
				       #x0A ; LINE FEED
				       #x0B ; VERTICAL TABULATION
				       #x0C ; FORM FEED
				       #x0D ; CARRIAGE RETURN
				       #x20 ; SPACE
				       #xA0))))

(define char-set:printing (char-set-union char-set:whitespace char-set:graphic)) ; NO-BREAK SPACE

(define char-set:blank
  (list->char-set (map %latin1->char '(#x09 ; HORIZONTAL TABULATION
				       #x20 ; SPACE
				       #xA0)))) ; NO-BREAK SPACE


(define char-set:iso-control
  (ucs-range->char-set! #x7F #xA0 #t (ucs-range->char-set 0 32)))

(define char-set:ascii (ucs-range->char-set 0 128))


;;;; done

)

;;; end of file
