;;;
;;;Part of: Vicare Scheme
;;;Contents: char-sets library
;;;Date: Fri Jun 12, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2010, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare containers char-sets)
  (export

    ;; bounds
    char-set-lower-bound	char-set-upper-bound
    char-set-inner-upper-bound	char-set-inner-lower-bound

    ;; constructors
    char-set			char-set-copy
    char-set-add		char-set-add!

    ;; inspection
    char-set-size (rename (domain-ref char-set-domain-ref))
    char-set-write
    char-set-hash

    ;; predicates
    char-set?
    char-set-empty? char-set-contains?
    char-set=? char-set<?
    char-set-superset? char-set-superset?/strict
    char-set-subset? char-set-subset?/strict

    ;; set operations
    char-set-intersection char-set-union
    char-set-difference char-set-complement

    ;; string operations
    string->char-set

    ;; list operations
    char-set-for-each char-set-every
    char-set-any char-set-fold
    char-set->list

    ;; iterations
    char-set-cursor		char-set-ref
    char-set-cursor-next	end-of-char-set?

    ;; predefined
    char-set:empty       char-set:full

    char-set:ascii
    char-set:ascii/dec-digit	(rename (char-set:ascii/dec-digit char-set:ascii/digit))
    char-set:ascii/oct-digit	char-set:ascii/hex-digit
    char-set:ascii/lower-case	char-set:ascii/upper-case
    char-set:ascii/letter	char-set:ascii/letter+digit
    char-set:ascii/punctuation	char-set:ascii/symbol
    char-set:ascii/control	char-set:ascii/whitespace
    char-set:ascii/graphic	char-set:ascii/printable
    char-set:ascii/blank

    char-set:ascii/vowels		char-set:ascii/consonants
    char-set:ascii/vowels/lower-case	char-set:ascii/consonants/lower-case
    char-set:ascii/vowels/upper-case	char-set:ascii/consonants/upper-case
    )
  (import (vicare))


;;;; helpers

(define (%last ell)
  ;;Return the last element in the list ELL.
  ;;
  (car (let last-pair ((x ell))
	 (if (pair? (cdr x))
	     (last-pair (cdr x))
	   x))))

(define (%append-reverse rev-head tail)
  ;;Reverse the list REV-HEAD and prepend it to the list TAIL.
  ;;
  (if (null? rev-head)
      tail
    (%append-reverse (cdr rev-head)
		     (cons (car rev-head) tail))))

(define (%cons-head-tail head tail result)
  ;;This is an internal helper for set operations.
  ;;
  (let ((result (if head (cons head result) result)))
    (if tail (cons tail result) result)))


(define-record-type (:char-set :make-char-set :char-set?)
  (nongenerative nausicaa:char-sets:char-set)
  (fields (mutable domain domain-ref domain-set!)
		;Null or a  list of pairs, each representing  a range of
		;characters left-inclusive and right-inclusive.
	  ))

(define (char-set . args)
  ;;Build and return a new instance of :CHAR-SET.  ARGS can be a list of
  ;;characters and or ranges.
  ;;
  (:make-char-set (apply make-domain args)))

(define (char-set-copy cs)
  ;;Return a new  instance of :CHAR-SET containing a copy  of the fields
  ;;of CS.
  ;;
  (char-set (domain-copy (domain-ref cs))))

(define (char-set-add cs obj)
  ;;Return a new  instance of :CHAR-SET containing a copy  of the fields
  ;;of CS with the addition of OBJ, which can be a character or range.
  ;;
  (cond ((item? obj)
	 (:make-char-set (domain-add-item  (domain-ref cs) obj)))
	((range? obj)
	 (:make-char-set (domain-add-range (domain-ref cs) obj)))
	(else
	 (assertion-violation 'char-set-add
	   "attempt to add an invalid object to a char-set" obj))))

(define (char-set-add! cs obj)
  ;;Return CS itself after  adding OBJ to it; OBJ can  be a character or
  ;;range.
  ;;
  (domain-set! cs (char-set-add cs obj))
  cs)

;;; --------------------------------------------------------------------

(define (char-set? cs)
  ;;Return #t if  CS is an instance of :CHAR-SET  and its field contents
  ;;are valid.
  ;;
  (and (:char-set? cs)
       (domain? (domain-ref cs))))

(define (char-set-empty? cs)
  (assert (char-set? cs))
  (domain-empty? (domain-ref cs)))

(define (char-set-contains? cs item)
  (assert (char-set? cs))
  (assert (item? item))
  (domain-contains? (domain-ref cs) item))

(define (char-set=? cs-a cs-b)
  (assert (char-set? cs-a))
  (assert (char-set? cs-b))
  (domain=? (domain-ref cs-a)
	    (domain-ref cs-b)))

(define (char-set<? cs-a cs-b)
  (assert (char-set? cs-a))
  (assert (char-set? cs-b))
  (domain<? (domain-ref cs-a)
	    (domain-ref cs-b)))

(define char-set-superset?
  (case-lambda
   (()
    #t)
   ((cs)
    (assert (char-set? cs))
    #t)
   ((cs1 cs2)
    (assert (char-set? cs1))
    (assert (char-set? cs2))
    (domain-superset? (domain-ref cs1)
		      (domain-ref cs2)))
   ((cs1 cs2 cs3 . cs-args)
    (and (char-set-superset? cs1 cs2)
	 (char-set-superset? cs2 cs3)
	 (apply char-set-superset? cs3 cs-args)))
   ))

(define (char-set-subset? . cs-args)
  (apply char-set-superset? (reverse cs-args)))

(define char-set-superset?/strict
  (case-lambda
   (()
    #t)
   ((cs)
    (assert (char-set? cs))
    #t)
   ((cs1 cs2)
    (assert (char-set? cs1))
    (assert (char-set? cs2))
    (domain-superset?/strict (domain-ref cs1)
			     (domain-ref cs2)))
   ((cs1 cs2 cs3 . cs-args)
    (and (char-set-superset?/strict cs1 cs2)
	 (char-set-superset?/strict cs2 cs3)
	 (apply char-set-superset?/strict cs3 cs-args)))
   ))

(define (char-set-subset?/strict cs-args)
  (apply char-set-superset?/strict (reverse cs-args)))

(define (char-set-intersection cs . cs-args)
  (:make-char-set (fold-left (lambda (domain-prev domain)
			      (domain-intersection domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define (char-set-union cs . cs-args)
  (:make-char-set (fold-left (lambda (domain-prev domain)
			      (domain-union domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define (char-set-difference cs . cs-args)
  (:make-char-set (fold-left (lambda (domain-prev domain)
			      (domain-difference domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define char-set-complement
  (case-lambda
   ((cs)
    (char-set-complement cs char-set:full))
   ((cs universe)
    (:make-char-set (domain-complement (domain-ref cs) (domain-ref universe))))))

(define (char-set-for-each proc cs)
  (domain-for-each proc (domain-ref cs)))

(define (char-set-every proc cs)
  (domain-every proc (domain-ref cs)))

(define (char-set-any proc cs)
  (domain-any proc (domain-ref cs)))

(define (char-set-fold kons knil cs)
  (domain-fold kons knil (domain-ref cs)))

(define (char-set->list cs)
  (domain->list (domain-ref cs)))

(define (string->char-set str)
  (:make-char-set (string->domain str)))

;;; --------------------------------------------------------------------

(define (char-set-size cs)
  (domain-size (domain-ref cs)))

(define char-set-write
  (case-lambda
   ((cs)
    (char-set-write cs (current-output-port)))
   ((cs port)
    (display "(char-set " port)
    (for-each (lambda (range)
		(display (string-append
			  "'("
			  "#\\x" (number->string (item->integer (car range)) 16)
			  " . "
			  "#\\x" (number->string (item->integer (cdr range)) 16)
			  ") ") port))
      (domain-ref cs))
    (display #\) port))))

;;; --------------------------------------------------------------------

(define char-set-hash
  (case-lambda
   ((cs)
    (char-set-hash cs (greatest-fixnum)))
   ((cs bound)
    (assert (char-set? cs))
    (assert (and (exact bound)
		 (integer? bound)
		 (or (zero? bound)
		     (positive? bound))))
    (let ((R (fold-left (lambda (knil range)
			  (+ knil
			     (item->integer (car range))
			     (item->integer (cdr range))))
	       0
	       (domain-ref cs))))
      (if (null? R)
	  0
	(mod R bound))))))


;;;; iteration

(define-record-type cursor
  (nongenerative nausicaa:char-sets:cursor)
  (fields (immutable cs)
		;An instance of CHAR-SET.
	  (immutable ranges)
		;Null or  the list of  ranges still to be  visited.  The
		;first element is  the range we are  visiting.  When all
		;the ranges have been visited,  or the char-set is empty
		;to begin with: this field is set to null.
	  (immutable next char-set-ref)
		;False  or the  next character  to return.   It must  be
		;inside  the first  range from  the list  in the  RANGES
		;field.   When this  field is  set to  false, the  field
		;RANGES is set to null.
	  ))

(define (char-set-cursor cs)
  (assert (char-set? cs))
  ;;RANGES is null or a list of ranges.
  (let ((ranges (domain-ref cs)))
    (make-cursor cs ranges (if (null? ranges)
				#f
			      (caar ranges)))))

(define (char-set-cursor-next cursor)
  (assert (cursor? cursor))
  (let ((cur (char-set-ref cursor)))
    (and cur
	 (let ((next (integer->item (+ 1 (item->integer cur)))))
	   (let loop ((ranges (cursor-ranges cursor)))
	     (if (item<=? next (cdar ranges))
		 (make-cursor (cursor-cs cursor) ranges next)
	       (let ((ranges (cdr ranges)))
		 (if (null? ranges)
		     (make-cursor (cursor-cs cursor) '() #f)
		   (make-cursor (cursor-cs cursor) ranges (caar ranges))))))))))

(define (end-of-char-set? cursor)
  (not (char-set-ref cursor)))


;;;; characters as items

(define inclusive-lower-bound		0)
(define exclusive-inner-upper-bound	#xD800)
(define exclusive-inner-lower-bound	#xDFFF)
(define inclusive-upper-bound		#x10FFFF)

(define char-set-lower-bound		(integer->char inclusive-lower-bound))
(define char-set-inner-upper-bound	(integer->char (- exclusive-inner-upper-bound 1)))
(define char-set-inner-lower-bound	(integer->char (+ 1 exclusive-inner-lower-bound)))
(define char-set-upper-bound		(integer->char inclusive-upper-bound))

(define item?		char?)
(define item=?		char=?)
(define item<?		char<?)
;;;(define item>?	char>?)
(define item<=?		char<=?)
(define item>=?		char>=?)
(define item->integer	char->integer)
(define integer->item	integer->char)

(define (%number-in-item-range? x)
  (or (and (<= inclusive-lower-bound x)
	   (<  x exclusive-inner-upper-bound))
      (and (<  exclusive-inner-lower-bound x)
	   (<= x inclusive-upper-bound))))

(define (item-minus a b)
  (+ 1 (- (item->integer a)
	  (item->integer b))))

(define (item-min a b)
  (if (item<? a b) a b))

(define (item-max a b)
  (if (item<? a b) b a))

(define (item-next ch range)
  (let* ((x  (+ 1 (item->integer ch))))
    (and (%number-in-item-range? x)
	 (let ((ch (integer->char x)))
	   (if range
	       (and (<= x (item->integer (cdr range)))
		    ch)
	     ch)))))

(define (item-prev ch range)
  (let* ((x  (- (item->integer ch) 1)))
    (and (%number-in-item-range? x)
	 (let ((ch (integer->char x)))
	   (if range
	       (and (<= (item->integer (car range)) x)
		    ch)
	     ch)))))


;;;; ranges of items
;;
;;A range is a pair of items:  the car being the inclusive leftmost, the
;;cdr being the inclusive rightmost.  Ranges must *never* be mutated.
;;

(define (make-range start last)
  ;;Build and retur a  new range of items.  START must  be a item object
  ;;representing the first in the range (inclusive); LAST must be a item
  ;;object representing the last in the range (inclusive).
  ;;
  (if (and (item? start)
	   (item? last)
	   (item<=? start last))
      (cons start last)
    (assertion-violation 'make-range "invalid range limits" start last)))

(define (range? obj)
  ;;Return #t if OBJ is a valid range of items, else return #f.
  ;;
  (and (pair? obj)
       (and (item? (car obj))
	    (item? (cdr obj))
	    (item<=? (car obj) (cdr obj)))))

(define (range-contains? range obj)
  ;;Return #t if OBJ is contained in RANGE, else return #f.
  ;;
  (and (item>=? obj (car range))
       (item<=? obj (cdr range))))

(define (range-length range)
  ;;Return  an exact  integer representing  the number  of items  in the
  ;;RANGE.
  ;;
  (+ 1 (- (item->integer (cdr range))
	  (item->integer (car range)))))

(define (range=? range-a range-b)
  ;;Return #t if the arguments represent the same range; else return #f.
  ;;
  (or (eq? range-a range-b)
      (and (item=? (car range-a) (car range-b))
	   (item=? (cdr range-a) (cdr range-b)))))

(define (range<? range-a range-b)
  ;;Return #t if all the items  in RANGE-A have code point strictly less
  ;;than all the items in RANGE-B; else return #f.
  ;;
  (item<? (cdr range-a) (car range-b)))

(define (range<=? range-a range-b)
  ;;Return #t if all  the items in RANGE-A have code  point less than or
  ;;equal to the rightmost item in RANGE-B; else return #f.
  ;;
  (item<=? (cdr range-a) (cdr range-b)))

(define (range-contiguous? range-a range-b)
  ;;Return #t  if the  rightmost item  in RANGE-A is  one less  than the
  ;;leftmost item in RANGE-B; else return #f.
  ;;
  (or (= 2 (item-minus (car range-b) (cdr range-a)))
      (= 2 (item-minus (car range-a) (cdr range-b)))))

(define (range-superset? range-a range-b)
  ;;Return  true if  RANGE-A is  a superset  of RANGE-B  or is  equal to
  ;;RANGE-B: all the items in RANGE-B are in RANGE-A; else return #f.
  ;;
  (item<=? (car range-a) (car range-b) (cdr range-b) (cdr range-a)))

(define (range-superset?/strict range-a range-b)
  ;;Return true  if RANGE-A is strictly  a superset of RANGE-B:  all the
  ;;items in RANGE-B  are in RANGE-A, and some items  of RANGE-A are not
  ;;in RANGE-B; else return #f.
  ;;
  (or (and (item<=? (car range-a) (car range-b))
	   (item<?  (cdr range-b) (cdr range-a)))
      (and (item<?  (car range-a) (car range-b))
	   (item<=? (cdr range-b) (cdr range-a)))))

(define (range-start<? range-a range-b)
  ;;Return #t if the leftmost item  in RANGE-A is less than the leftmost
  ;;item in RANGE-B:
  ;;
  ;;   |---------| range-a
  ;;       |---------| range-b
  ;;
  ;;else return #f.
  ;;
  (item<? (car range-a) (car range-b)))

(define (range-start<=? range-a range-b)
  ;;Return #t if the  leftmost item in RANGE-A is less  than or equal to
  ;;the leftmost item in RANGE-B:
  ;;
  ;;   |---------| range-a
  ;;       |---------| range-b
  ;;
  ;;or:
  ;;
  ;;   |---------| range-a
  ;;   |-----------| range-b
  ;;
  ;;else return #f.
  ;;
  (item<=? (car range-a) (car range-b)))

(define (range-last<? range-a range-b)
  ;;Return  #t if  the  righttmost  item in  RANGE-A  is  less than  the
  ;;leftmost item in RANGE-B:
  ;;
  ;;   |---------| range-a
  ;;       |---------| range-b
  ;;
  ;;else return #f.
  ;;
  (item<? (cdr range-a) (cdr range-b)))

(define (range-last<=? range-a range-b)
  ;;Return #t if the rightmost item in  RANGE-A is less than or equal to
  ;;the leftmost item in RANGE-B:
  ;;
  ;;   |---------| range-a
  ;;       |---------| range-b
  ;;
  ;;or:
  ;;
  ;;   |-------------| range-a
  ;;     |-----------| range-b
  ;;
  ;;else return #f.
  ;;
  (item<=? (cdr range-a) (cdr range-b)))

(define (range-overlapping? range-a range-b)
  ;;Return #t if the two ranges are overlapping:
  ;;
  ;;   |--------------| range-a
  ;;      |--------------| range-b
  ;;
  ;;or:
  ;;
  ;;       |--------------| range-a
  ;;   |--------------| range-b
  ;;
  ;;or:
  ;;
  ;;   |--------------| range-a
  ;;      |-------| range-b
  ;;
  ;;or:
  ;;
  ;;      |-------| range-a
  ;;   |--------------| range-b
  ;;
  ;;or:
  ;;
  ;;   |--------------| range-a
  ;;   |--------------| range-b
  ;;
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (or (and (item<=? start-a start-b last-a))
	(and (item<=? start-b start-a last-b)))))

(define (range-concatenate range-a range-b)
  ;;Return a  new range having:  as leftmost  item the leftmost  item in
  ;;RANGE-A and RANGE-B; as rightmost item the rightmost item in RANGE-A
  ;;and RANGE-B a.  Example:
  ;;
  ;;    |--------| range-a
  ;;       |--------| range-b
  ;;    |-----------| result
  ;;
  ;;another example:
  ;;
  ;;    |--------| range-a
  ;;                   |--------| range-b
  ;;    |-----------------------| result
  ;;
  (cons (item-min (car range-a) (car range-b))
	(item-max (cdr range-a) (cdr range-b))))

(define (range-intersection range-a range-b)
  ;;If  the arguments  have some  items in  common: return  a new  range
  ;;representing their intersection, else return false.  Example:
  ;;
  ;;   |-------| range-a
  ;;      |-------| range-b
  ;;      |----| result
  ;;
  ;;another example:
  ;;
  ;;         |-------| range-a
  ;;   |-------| range-b
  ;;         |-| result
  ;;
  ;;for the following example the return value is false:
  ;;
  ;;   |-------| range-a
  ;;               |-------| range-b
  ;;
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (and (or (item<=? start-a start-b last-a)
	     (item<=? start-b start-a last-b))
	 (cons (item-max start-a start-b)
	       (item-min last-a  last-b)))))

(define (range-union range-a range-b)
  ;;Return two values  representing the union between  the given ranges:
  ;;all the items that are in one or both the ranges.
  ;;
  ;;When  the ranges  do not  overlap and  are not  contiguous: the  two
  ;;returned values  are both ranges,  the first being the  leftmost and
  ;;the second being the rightmost.
  ;;
  ;;When the  ranges do  overlap or are  contiguous: the  first returned
  ;;value  is  #f  and  the  second   returned  value  is  a  new  range
  ;;representing the actual union between the arguments.
  ;;
  ;;Example:
  ;;
  ;;   |------| range-a
  ;;          |------| range-b
  ;;   |-------------| second result     first result = #f
  ;;
  ;;Example:
  ;;
  ;;          |------| range-a
  ;;   |------| range-b
  ;;   |-------------| second result     first result = #f
  ;;
  ;;Example:
  ;;
  ;;   |------| range-a
  ;;            |------| range-b
  ;;   |------| first result
  ;;            |------| second result
  ;;
  ;;Example:
  ;;
  ;;            |------| range-a
  ;;   |------| range-b
  ;;   |------| first result
  ;;            |------| second result
  ;;
  ;;For  this function  it is  mandatory that:  if one  of the  returned
  ;;values if #f, it must be the first one; this property is used in the
  ;;domain functions below.
  ;;
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (cond
     ;;Contiguous: RANGE-A < RANGE-B.
     ((= 2 (item-minus start-b last-a))	(values #f (cons start-a last-b)))

     ;;Contiguous: RANGE-B < RANGE-A.
     ((= 2 (item-minus start-a last-b))	(values #f (cons start-b last-a)))

     ;;Disjoint: RANGE-A < RANGE-B.
     ((item<? last-a start-b)			(values range-a range-b))

     ;;Disjoint: RANGE-B < RANGE-A.
     ((item<? last-b start-a)			(values range-b range-a))

     ;;Here we know they are overlapping.
     (else
      (values #f (cons (item-min start-a start-b)
		       (item-max last-a  last-b)))))))

(define (range-difference range-a range-b)
  ;;Return  two values  representing  the difference  between the  given
  ;;ranges: all the items  that are in RANGE-A or in  RANGE-B but not in
  ;;both.
  ;;
  ;;When  the ranges  do not  overlap and  are not  contiguous: the  two
  ;;returned values  are both ranges,  the first being the  leftmost and
  ;;the second being the rightmost.
  ;;
  ;;When  the  ranges do  not  overlap  and  are contiguous:  the  first
  ;;returned value  is #f and the  second returned value is  a new range
  ;;representing the actual union between the arguments.
  ;;
  ;;When the ranges  do overlap:
  ;;
  ;;Example:
  ;;
  ;;   |------| range-a
  ;;          |------| range-b
  ;;   |-------------| second result     first result = #f
  ;;
  ;;Example:
  ;;
  ;;          |------| range-a
  ;;   |------| range-b
  ;;   |-------------| second result     first result = #f
  ;;
  ;;Example:
  ;;
  ;;   |------| range-a
  ;;            |------| range-b
  ;;   |------| first result
  ;;            |------| second result
  ;;
  ;;Example:
  ;;
  ;;            |------| range-a
  ;;   |------| range-b
  ;;   |------| first result
  ;;            |------| second result
  ;;
  ;;Example:
  ;;
  ;;   |------| range-a
  ;;       |------| range-b
  ;;   |---| first result
  ;;          |---| second result
  ;;
  ;;Example:
  ;;
  ;;   |------| range-a
  ;;   |---------| range-b
  ;;          |--| second result           first result = #f
  ;;
  ;;Example:
  ;;
  ;;      |------| range-a
  ;;   |---------| range-b
  ;;   |--| second result           first result = #f
  ;;
  ;;For  this function  it is  mandatory that:  if one  of the  returned
  ;;values if #f, it must be the first one; this property is used in the
  ;;domain functions below.
  ;;
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (cond
     ;;Contiguous: RANGE-A < RANGE-B.
     ((= 2 (item-minus start-b last-a))	(values #f (cons start-a last-b)))

     ;;Contiguous: RANGE-B < RANGE-A.
     ((= 2 (item-minus start-a last-b))	(values #f (cons start-b last-a)))

     ;;Disjoint: RANGE-A < RANGE-B.
     ((item<? last-a start-b)			(values range-a range-b))

     ;;Disjoint: RANGE-B < RANGE-A.
     ((item<? last-b start-a)			(values range-b range-a))

     ;;Here we know they are overlapping.
     ((item=? start-a start-b) ; same start
      (cond ((item=? last-a last-b)
	     (values #f #f))
	    ((item<? last-a last-b)
	     (values #f (let ((last-a/next (item-next last-a range-b)))
			  (and (item<=? last-a/next last-b)
			       (cons last-a/next last-b)))))
	    ((item<? last-b last-a)
	     (values #f (let ((last-b/next (item-next last-b range-a)))
			  (and (item<=? last-b/next last-a)
			       (cons last-b/next last-a)))))))

     ((item=? last-a last-b) ; same last
      (cond ((item=? start-a start-b)
	     (values #f #f))
	    ((item<? start-a start-b)
	     (values #f (let ((start-b/prev (item-prev start-b range-a)))
			  (and (item<=? start-a start-b/prev)
			       (cons start-a start-b/prev)))))
	    ((item<? start-b start-a)
	     (values #f (let ((start-a/prev (item-prev start-a range-b)))
			  (and (item<=? start-b start-a/prev)
			       (cons start-b start-a/prev)))))))
     ;;Here we know that START-A != START-B and LAST-A != LAST-B.
     ((item<? start-a start-b) ; overlapping, a < b
      (values (let ((start-b/prev (item-prev start-b range-a)))
		(and (item<=? start-a start-b/prev)
		     (cons start-a start-b/prev)))
	      (if (item<=? last-a last-b)
		  (let ((last-a/next (item-next last-a range-b)))
		    (and (item<=? last-a/next last-b)
			 (cons last-a/next last-b)))
		(let ((last-b/next (item-next last-b range-a)))
		  (and (item<=? last-b/next last-a)
		       (cons last-b/next last-a))))))

     (else	; overlapping, a > b
      (assert (item<? start-b start-a))
      (values (let ((start-a/prev (item-prev start-a range-b)))
		(and (item<=? start-b start-a/prev)
		     (cons start-b start-a/prev)))
	      (if (item<? last-a last-b)
		  (let ((last-a/next (item-next last-a range-b)))
		    (and (item<=? last-a/next last-b)
			 (cons last-a/next last-b)))
		(let ((last-b/next (item-next last-b range-a)))
		  (and (item<=? last-b/next last-a)
		       (cons last-b/next last-a)))))))))

(define (range-for-each proc range)
  ;;Apply PROC to each item in RANGE, discard the results.
  ;;
  (let loop ((i (car range)))
    (and i (proc i)
	 (loop (item-next i range)))))

(define (range-every proc range)
  ;;Apply  PROC  to  every  item  in  RANGE;  return  true  if  all  the
  ;;applications returned  true, else return  #f.  Stop applying  at the
  ;;first false result.
  ;;
  (let loop ((i (car range)))
    (if i
	(and (proc i)
	     (loop (item-next i range)))
      #t)))

(define (range-any proc range)
  ;;Apply PROC to  every item in RANGE; stop applying  at the first true
  ;;result  and return  true.   If all  the  applications return  false:
  ;;return false.
  ;;
  (let loop ((i (car range)))
    (and i
	 (or (proc i)
	     (loop (item-next i range))))))

(define (range-fold kons knil range)
  (let loop ((i    (car range))
	     (knil knil))
    (if i
	(loop (item-next i range) (kons i knil))
      knil)))

(define (range->list range)
  (range-fold cons '() range))


;;;; domains of items
;;
;;A domain is null  or a list of ranges sorted from  the leftmost to the
;;rightmost.
;;

(define empty-domain '())

(define (make-domain . items/ranges)
  ;;Given a list of items and/or ranges return a new domain.
  ;;
  (fold-left (lambda (domain item)
	       (cond ((item? item)
		      (domain-add-item  domain item))
		     ((range? item)
		      (domain-add-range domain item))
		     (else
		      (assertion-violation 'make-domain "invalid element for domain" item))))
    empty-domain items/ranges))

(define (domain-copy domain)
  ;;Return a new domain equal to DOMAIN but having a new list structure.
  ;;
  (let loop ((x domain))
    (if (pair? x)
	(cons (loop (car x))
	      (loop (cdr x)))
      x)))

(define (domain-add-item domain obj)
  ;;Return  a  new  domain  having  the  same  elements  of  DOMAIN  and
  ;;containing also OBJ.
  ;;
  (domain-add-range domain (make-range obj obj)))

(define (domain-add-range domain new-R)
  ;;Add a new range NEW-R to the DOMAIN.  Return a domain that may share
  ;;structure with DOMAIN.
  ;;
  (if (domain-empty? domain)
      (list new-R)
    (let ((next-R (car domain)))
      (cond ((range=? new-R next-R)
	     ;;The new range is equal to the next range; just return the
	     ;;previous domain unchanged.
	     domain)

	    ((range-contiguous? new-R next-R)
	     ;;The  new  range  is   contiguous  with  the  next  range.
	     ;;Concatenate the new  and the next ranges,  the recurse to
	     ;;see if  the new  range can be  composed with  the further
	     ;;first range.
	     (domain-add-range (cdr domain) (range-concatenate new-R next-R)))

	    ((range<? new-R next-R)
	     ;;The  new range  is completely  to  the left  of the  next
	     ;;range.  Just prepend the new range to the domain.
	     (cons new-R domain))

	    ((range-overlapping? new-R next-R)
	     ;;The new range overlaps with the next range.  Join the new
	     ;;and the next ranges, the recurse  to see if the new range
	     ;;can be composed with the further first range.
	     (receive (first second)
		 (range-union new-R next-R)
	       (let ((new-domain (domain-add-range (cdr domain) second)))
		 (if first
		     (cons first new-domain)
		   new-domain))))

	    (else
	     ;;The  new range  is completely  to the  right of  the next
	     ;;range.  Keep the next range and recurse.
	     (cons next-R (domain-add-range (cdr domain) new-R)))))))

(define (domain? domain)
  ;;Return #t if DOMAIN is a valid domain, else return #f.
  ;;
  (or (null? domain)
      (let ((range1  (car domain))
	    (domain1 (cdr domain)))
	(and (range? range1)
	     (or (null? domain1)
		 (let ((range2 (car domain1)))
		   (and (range? range2)
			(range<? range1 range2)
			(domain? domain1))))))))

(define (domain-size domain)
  ;;Return  an exact  integer representing  the number  of items  in the
  ;;DOMAIN.
  ;;
  (fold-right (lambda (range size)
		(+ size (range-length range)))
    0 domain))

(define domain-empty? null?)

(define (domain-contains? domain item)
  ;;Return true if the DOMAIN contains the ITEM.
  ;;
  (exists (lambda (range)
	    (range-contains? range item))
    domain))

(define (domain=? domain-a domain-b)
  ;;Return true if the given domains are equal, range by range.
  ;;
  (or (eq? domain-a domain-b)
      (cond ((null? domain-a)	(null? domain-b))
	    ((null? domain-b)	(null? domain-a))
	    (else
	     (and (range=? (car domain-a)
			   (car domain-b))
		  (domain=? (cdr domain-a)
			    (cdr domain-b)))))))

(define (domain<? domain-a domain-b)
  ;;Return true if all the items  in DOMAIN-A are strictly less than all
  ;;the items in DOMAIN-B.
  ;;
  (and (not (null? domain-a))
       (not (null? domain-b))
       (range<? (%last domain-a) (car domain-b))))

;;; --------------------------------------------------------------------

(define (domain-superset? domain-a domain-b)
  ;;Return #t is  DOMAIN-A contains all the items in  DOMAIN-B, in other
  ;;words: if  DOMAIN-A is  equal to  DOMAIN-B or  a strict  superset of
  ;;DOMAIN-B.
  ;;
  ;;Recurse looking for RANGE-B in DOMAIN-A.
  ;;
  (or (domain-empty? domain-b)
      (and (not (domain-empty? domain-a))
	   (if (range-superset? (car domain-a)
				(car domain-b))
	       (domain-superset? domain-a (cdr domain-b))
	     (domain-superset? (cdr domain-a) domain-b)))))

(define (domain-superset?/strict domain-a domain-b)
  ;;Return #t  is DOMAIN-A contains all  the items in DOMAIN-B  and some
  ;;items from DOMAIN-A are not in DOMAIN-B, in other words: if DOMAIN-A
  ;;is a strict superset of DOMAIN-B.
  ;;
  ;;Recurse looking for RANGE-B in DOMAIN-A.
  ;;
  (let look-for-range-b-in-domain-a ((superset? #f)
				     (domain-a domain-a)
				     (domain-b domain-b))
    (if (domain-empty? domain-b)
	superset?
      (and (not (domain-empty? domain-a))
	   (let ((range-a (car domain-a))
		 (range-b (car domain-b)))
	     (cond ((range<? range-a range-b)
		    (look-for-range-b-in-domain-a #t (cdr domain-a) domain-b))
		   ((range-superset?/strict range-a range-b)
		    (look-for-range-b-in-domain-a #t domain-a (cdr domain-b)))
		   ((range=? range-a range-b)
		    (look-for-range-b-in-domain-a superset? (cdr domain-a) (cdr domain-b)))
		   ((range-superset? range-a range-b)
		    (look-for-range-b-in-domain-a superset? domain-a (cdr domain-b)))
		   (else #f)))))))

(define (domain-intersection domain-a domain-b)
  (let loop ((result	'())
	     (domain-a	domain-a)
	     (domain-b	domain-b))
    (if (or (domain-empty? domain-a)
	    (domain-empty? domain-b))
	(reverse result)
      (let ((range-a	(car domain-a))
	    (range-b	(car domain-b)))
	(cond
	 ((range=? range-a range-b)
	  (loop (cons range-a result)
		(cdr domain-a) (cdr domain-b)))
	 ((range-overlapping? range-a range-b)
	  (let ((result (cons (range-intersection range-a range-b) result)))
	    (if (range-last<? range-a range-b)
		(loop result (cdr domain-a) domain-b)
	      (loop result domain-a (cdr domain-b)))))
	 ((range<? range-a range-b)
	  (loop result (cdr domain-a) domain-b))
	 ((range<? range-b range-a)
	  (loop result domain-a (cdr domain-b)))
	 (else
	  (assertion-violation 'domain-intersection
	    "internal error processing ranges" (list range-a range-b))))))))

(define (domain-union domain-a domain-b)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (cond
	     ((or (range-overlapping? top range)
		  (range-contiguous?  top range))
	      (let-values (((head tail) (range-union top range)))
		(loop (%cons-head-tail head tail (cdr result)) (cdr domain))))
	     (else
	      (loop (cons range result) (cdr domain)))))))))
  (let loop ((result '())
	     (domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((domain-empty? domain-a)
      (finish result domain-b))
     ((domain-empty? domain-b)
      (finish result domain-a))
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond
	 ((and (not (null? result)) (range-contiguous? (car result) range-a))
	  (loop (cons (range-concatenate (car result) range-a) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (range-contiguous? (car result) range-b))
	  (loop (cons (range-concatenate (car result) range-b) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (range=? (car result) range-a))
	  (loop result (cdr domain-a) domain-b))

	 ((and (not (null? result)) (range=? (car result) range-b))
	  (loop result domain-a (cdr domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-a))
	  (let-values (((head tail) (range-union (car result) range-a)))
	    (loop (cons tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-b))
	  (let-values (((head tail) (range-union (car result) range-b)))
	    (loop (cons tail (cdr result)) domain-a (cdr domain-b))))

	 ((range=? range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) (cdr domain-b)))

	 ((range-contiguous? range-a range-b)
	  (loop (cons (range-concatenate range-a range-b) result) (cdr domain-a) (cdr domain-b)))

	 ((range-overlapping? range-a range-b)
	  (let-values (((head tail) (range-union range-a range-b)))
	    (loop (cons tail result) (cdr domain-a) (cdr domain-b))))

	 ((range<? range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) domain-b))

	 ((range<? range-b range-a)
          (loop (cons range-b result) domain-a (cdr domain-b)))

	 (else
	  (assertion-violation 'domain-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (domain-difference domain-a domain-b)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (cond ((range-overlapping? top range)
		   (let-values (((head tail) (range-difference top range)))
		     (loop (%cons-head-tail head tail (cdr result))
			   (cdr domain))))
		  ((range-contiguous? top range)
		   (let-values (((head tail) (range-union top range)))
		     (loop (%cons-head-tail head tail (cdr result))
			   (cdr domain))))
		  (else
		   (loop (cons range result) (cdr domain)))))))))
  (let loop ((result '())
	     (domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((and (domain-empty? domain-a) (domain-empty? domain-b))
      (reverse result))
     ((domain-empty? domain-a)
      (finish result domain-b))
     ((domain-empty? domain-b)
      (finish result domain-a))
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond
	 ((and (not (null? result)) (range-contiguous? (car result) range-a))
	  (loop (cons (range-concatenate (car result) range-a) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (range-contiguous? (car result) range-b))
	  (loop (cons (range-concatenate (car result) range-b) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-a))
	  (let-values (((head tail) (range-difference (car result) range-a)))
	    (loop (%cons-head-tail head tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-b))
	  (let-values (((head tail) (range-difference (car result) range-b)))
	    (loop (%cons-head-tail head tail (cdr result)) domain-a (cdr domain-b))))

	 ((range=? range-a range-b)
	  (loop result (cdr domain-a) (cdr domain-b)))

	 ((range-contiguous? range-a range-b)
	  (loop (cons (range-concatenate range-a range-b) result) (cdr domain-a) (cdr domain-b)))

	 ((range-overlapping? range-a range-b)
	  (let-values (((head tail) (range-difference range-a range-b)))
	    (loop (%cons-head-tail head tail result) (cdr domain-a) (cdr domain-b))))

	 ((range<? range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) domain-b))

	 ((range<? range-b range-a)
	  (loop (cons range-b result) domain-a (cdr domain-b)))

	 (else
	  (assertion-violation 'domain-difference
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (domain-complement domain universe)
  (if (null? domain)
      universe
    (let loop ((result		'())
	       (universe	universe)
	       (domain		domain))
      (cond ((domain-empty? universe)
	     (reverse result))
	    ((domain-empty? domain)
	     (reverse (%append-reverse universe result)))
	    (else
	     (let ((range-a (car universe))
		   (range-b (car domain)))
	       (cond ((range<? range-b range-a)
		      (loop result universe (cdr domain)))

		     ((range<? range-a range-b)
		      (loop (cons range-a result) (cdr universe) domain))

		     ((range=? range-a range-b)
		      (loop result (cdr universe) (cdr domain)))

		     ((range-overlapping? range-a range-b)
		      (let-values (((head tail)
				    (%range-in-first-only range-a range-b)))
			(if (range-last<? range-b range-a)
			    (loop (if head (cons head result) result)
				  (cons tail (cdr universe)) (cdr domain))
			  (let ((result (%cons-head-tail head tail result)))
			    (cond ((range-last<? range-a range-b)
				   (loop result (cdr universe) domain))
				  (else
				   (loop result (cdr universe) (cdr domain))))))))
		     (else
		      ;;just discard RANGE-A
		      (assertion-violation 'domain-complement
			"internal error processing ranges" (list range-a range-b)))
		     )))))))

(define (%range-in-first-only range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (if (or (item<? last-b start-a)
	    (item<? last-a start-b)) ; disjoint (including contiguous)
	(values #f range-a)
      ;;Here we know they are overlapping.
      (values
       (and (item<? start-a start-b)
	    (let ((start-b/prev (item-prev start-b range-a)))
	      (and (item<? start-a start-b/prev)
		   (cons start-a start-b/prev))))
       (and (item<? last-b last-a)
	    (let ((last-b/next (item-next last-b range-a)))
	      (and (item<? last-b/next last-a)
		   (cons last-b/next last-a))))))))

(define (domain-for-each proc domain)
  (for-each (lambda (range)
	      (range-for-each proc range))
    domain))

(define (domain-every proc domain)
  (for-all (lambda (range)
	     (range-every proc range))
    domain))

(define (domain-any proc domain)
  (exists (lambda (range)
	    (range-any proc range))
    domain))

(define (domain-fold kons knil domain)
  (let loop ((domain domain)
	     (knil knil))
    (if (null? domain)
	knil
      (loop (cdr domain) (range-fold kons knil (car domain))))))

(define (domain->list domain)
  (reverse (apply append (map range->list domain))))

(define (string->domain str)
  (apply make-domain (string->list str)))


;;;; basic predefined char sets

(define char-set:empty (:make-char-set '()))

(define char-set:full
  (:make-char-set `((,char-set-lower-bound       . ,char-set-inner-upper-bound)
		    (,char-set-inner-lower-bound . ,char-set-upper-bound))))


;;;; ASCII predefined char sets

(define char-set:ascii
  ;;Notice  that ASCII  has numeric  codes in  the range  [0,  127]; the
  ;;numeric code 127 is included, and the number of codes is 128.
  (:make-char-set '((#\x0 . #\x127))))

(define char-set:ascii/dec-digit
  (:make-char-set '((#\0 . #\9))))

(define char-set:ascii/oct-digit
  (:make-char-set '((#\0 . #\7))))

(define char-set:ascii/hex-digit
  (:make-char-set '((#\0 . #\9)	;this must be the first
		    (#\A . #\F)	;this must be the second
		    (#\a . #\f) ;this must be the third
		    )))

(define char-set:ascii/lower-case
  (:make-char-set '((#\a . #\z))))

(define char-set:ascii/upper-case
  (:make-char-set '((#\A . #\Z))))

(define char-set:ascii/letter
  (char-set-union char-set:ascii/lower-case
		  char-set:ascii/upper-case))

(define char-set:ascii/letter+digit
  (char-set-union char-set:ascii/letter
		  char-set:ascii/dec-digit))

(define char-set:ascii/punctuation
  ;;Yes I have verified that all of these have numeric code in the range
  ;;[0, 127] (Marco Maggi, Tue Jun 23, 2009).
  (char-set #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
	    #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\}))

(define char-set:ascii/symbol
  ;;Yes I have verified that all of these have numeric code in the range
  ;;[0, 127] (Marco Maggi, Tue Jun 23, 2009).
  (char-set #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~))

(define char-set:ascii/control
  ;;Notice that control characters are the ones whose numeric code is in
  ;;the range [0, 31] plus 127; the number of control characters is 33.
  (char-set '(#\x0 . #\x31)
	    (integer->char 127)))

(define char-set:ascii/whitespace
  (char-set #\x0009	   ; HORIZONTAL TABULATION
	    #\x000A	   ; LINE FEED
	    #\x000B	   ; VERTICAL TABULATION
	    #\x000C	   ; FORM FEED
	    #\x000D	   ; CARRIAGE RETURN
	    #\x0020))	   ; SPACE

(define char-set:ascii/blank
  (char-set #\tab #\space))

(define char-set:ascii/graphic
  (char-set-union char-set:ascii/letter+digit
		  char-set:ascii/punctuation
		  char-set:ascii/symbol))

(define char-set:ascii/printable
  (char-set-union char-set:ascii/whitespace
		  char-set:ascii/graphic)) ; NO-BREAK SPACE

(define char-set:ascii/vowels
  (char-set #\a #\e #\i #\o #\u
	    #\A #\E #\I #\O #\U))

(define char-set:ascii/vowels/lower-case
  (char-set #\a #\e #\i #\o #\u))

(define char-set:ascii/vowels/upper-case
  (char-set #\A #\E #\I #\O #\U))

(define char-set:ascii/consonants
  (char-set-complement char-set:ascii/vowels
		       char-set:ascii/letter))

(define char-set:ascii/consonants/lower-case
  (char-set-complement char-set:ascii/vowels/lower-case
		       char-set:ascii/lower-case))

(define char-set:ascii/consonants/upper-case
  (char-set-complement char-set:ascii/vowels/upper-case
		       char-set:ascii/upper-case))


;;;; done

)

;;; end of file
