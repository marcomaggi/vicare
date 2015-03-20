;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 113
;;;Date: Sun Mar  8, 2015
;;;
;;;Abstract
;;;
;;;	Implementation of general sets and bags for SRFI 113.
;;;
;;;     A "sob" object is the representation of both sets and bags.  This allows each
;;;     set-* and  bag-* procedure  to be  implemented using  the same  code, without
;;;     having to  deal in  ugly indirections  over the  field accessors.   There are
;;;     three fields, "sob-multi?", "sob-hash-table", and "sob-comparator."
;;;
;;;     The value of  "sob-multi?" is #t for bags and  #f for sets.  "sob-hash-table"
;;;     maps the  elements of  the sob to  the number of  times the  element appears,
;;;     which is always 1 for a set,  any positive value for a bag.  "sob-comparator"
;;;     is the comparator for the elements of the set.
;;;
;;;     Note that sob-* procedures do not do type checking or (typically) the copying
;;;     required for supporting pure functional update.  These things are done by the
;;;     set-* and  bag-* procedures,  which are externally  exposed (but  trivial and
;;;     mostly uncommented below).
;;;
;;;Copyright (C) John Cowan 2013.  All Rights Reserved.
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software  and associated documentation  files (the ``Software''), to  deal in
;;;the Software without restriction, including  without limitation the rights to use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED ``AS  IS'', WITHOUT  WARRANTY OF  ANY KIND,  EXPRESS OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;


#!vicare
(library (srfi :113 sets-and-bags)
  (export
    ;;
    set set-unfold
    ;;
    set? set-contains? set-empty? set-disjoint?
    ;;
    set-member set-element-comparator
    ;;
    set-adjoin set-adjoin! set-replace set-replace!
    set-delete set-delete! set-delete-all set-delete-all! set-search!
    ;;
    set-size set-find set-count set-any? set-every?
    ;;
    set-map set-for-each set-fold
    set-filter  set-remove  set-partition
    set-filter! set-remove! set-partition!
    ;;
    set-copy set->list list->set list->set!
    ;;
    set=? set<? set>? set<=? set>=?
    ;;
    set-union set-intersection set-difference set-xor
    set-union! set-intersection! set-difference! set-xor!
    ;;
    set-comparator

    ;;
    bag bag-unfold
    ;;
    bag? bag-contains? bag-empty? bag-disjoint?
    ;;
    bag-member bag-element-comparator
    ;;
    bag-adjoin bag-adjoin! bag-replace bag-replace!
    bag-delete bag-delete! bag-delete-all bag-delete-all! bag-search!
    ;;
    bag-size bag-find bag-count bag-any? bag-every?
    ;;
    bag-map bag-for-each bag-fold
    bag-filter bag-remove bag-partition
    bag-filter! bag-remove! bag-partition!
    ;;
    bag-copy bag->list list->bag list->bag!
    ;;
    bag=? bag<? bag>? bag<=? bag>=?
    ;;
    bag-union bag-intersection bag-difference bag-xor
    bag-union! bag-intersection! bag-difference! bag-xor!
    ;;
    bag-comparator
    ;;
    bag-sum bag-sum! bag-product bag-product!
    bag-unique-size bag-element-count bag-for-each-unique bag-fold-unique
    bag-increment! bag-decrement! bag->set set->bag set->bag!
    bag->alist alist->bag)
  (import (vicare)
    (srfi :114))


;;;; helpers

(define (hashtable-replace-key! table old-key new-key)
  ;;If OLD-KEY is  a key in TABLE: delete  the entry associated to OLD-KEY  and add a
  ;;new entry  associated to  NEW-KEY having the  same value of  the old  one; return
  ;;OLD-KEY.  If OLD-KEY is not a key in TABLE: do nothing and return false.
  ;;
  (let ((val (hashtable-ref table old-key (void))))
    (if (eq? val (void))
	#f
      (begin
	(hashtable-delete! table old-key)
	(hashtable-set!    table new-key val)
	old-key))))

(define (hashtable-replace-key-representation! table new-key)
  ;;Search an entry in TABLE associated with NEW-KEY:
  ;;
  ;;* If one is  found: delete it and add a new entry  associated to NEW-KEY with the
  ;;  same value of the old one, then return the old key.
  ;;
  ;;* If none is found: do nothing and return false.
  ;;
  ;;It makes sense to call this function when the equality predicate of TABLE returns
  ;;true when applied  to two values with different representation  but for which the
  ;;hash function of TABLE returns the same value.
  ;;
  ;;For example, the following code replaces the key "ciao" with the key "CIAO":
  ;;
  ;;   (import (vicare))
  ;;   (define T
  ;;     (make-hashtable string-ci=? string-ci-hash))
  ;;   (hashtable-set! T "ciao" 1)
  ;;   (hashtable-replace-key-representation! T "CIAO")
  ;;   (hashtable-ref T "CIAO" #f)
  ;;   => 1
  ;;
  (define equiv
    (hashtable-equivalence-function table))
  (cond ((hashtable-find-key (lambda (old-key)
			   (equiv old-key new-key))
			 table)
	 => (lambda (old-key)
	      (hashtable-replace-key! table old-key new-key)))
	(else #f)))

(define (hashtable-find-and-replace-key! pred new-key table)
  ;;Search an entry in TABLE whose key and value satisfy the predicate PRED:
  ;;
  ;;* If one is  found: delete it and add a new entry  associated to NEW-KEY with the
  ;;  same value of the old one, then return the old key.
  ;;
  ;;* If none is found: do nothing and return false.
  ;;
  ;;For example, the following code replaces the key "ciao" with the key "CIAO":
  ;;
  ;;   (import (vicare))
  ;;   (define T
  ;;     (make-hashtable string=? string-hash))
  ;;   (hashtable-set! T "ciao" 1)
  ;;   (hashtable-find-and-replace-key!
  ;;       (lambda (old-key val)
  ;;         (string-ci=? "CIAO" old-key))
  ;;     "CIAO" T)
  ;;   (hashtable-ref T "CIAO" #f)
  ;;   => 1
  ;;
  (cond ((hashtable-find-key pred table)
	 => (lambda (old-key)
	      (hashtable-replace-key! table old-key new-key)))
	(else #f)))

;;; --------------------------------------------------------------------

(define (max-one n multi?)
  ;;Upper-bound N by one if MULTI? is false.
  ;;
  ;;   (max-one -3 #f)	=> -3
  ;;   (max-one -2 #f)	=> -2
  ;;   (max-one -1 #f)	=> -1
  ;;   (max-one  0 #f)	=>  0
  ;;   (max-one +1 #f)	=> +1
  ;;   (max-one +2 #f)	=> +1
  ;;   (max-one +3 #f)	=> +1
  ;;
  (cond (multi?		n)
	((> n 1)	1)
	(else		n)))


;;;; record definition and core typing

(define-record-type (sob %make-sob sob?)
  (fields (immutable hash-table)
	  (immutable comparator)
	  (immutable multi?))
  (protocol
   (lambda (make-record)
     (lambda* ({hash-table hashtable?} {comparator comparator?} multi?)
       (make-record hash-table comparator (and multi? #t)))))
  #| end of DEFINE-RECORD-TYPE |# )

(define (set? obj)
  (and (sob? obj)
       (not (sob-multi? obj))))

(define (bag? obj)
  (and (sob? obj)
       (sob-multi? obj)))


;;;; procedure arguments validation

(module (%list-of-sets?
	 %list-of-bags?
	 %check-same-comparator)

  (define (%list-of-sets? obj)
    ;;Used to validate rest arguments, as in:
    ;;
    ;;   (define* (operation . {args %list-of-sets?})
    ;;     ---)
    ;;
    (or (null? obj)
	(and (list? obj)
	     (for-all set? obj)
	     (%sob-check-comparators obj))))

  (define (%list-of-bags? obj)
    ;;Used to validate rest arguments, as in:
    ;;
    ;;   (define* (operation . {args %list-of-bags?})
    ;;     ---)
    ;;
    (or (null? obj)
	(and (list? obj)
	     (for-all bag? obj)
	     (%sob-check-comparators obj))))

  (case-define %check-same-comparator
    ;;Used to validate SOB arguments, as in:
    ;;
    ;;   (define* (operation {set1 set?} {set2 set?} {set3 set?} . {sets %list-of-sets?})
    ;;     (%check-same-comparator __who__ set1 set2 set3 sets)
    ;;     ---)
    ;;
    ((who sob1 sob2)
     (unless (eq? (sob-comparator sob1)
		  (sob-comparator sob2))
       (procedure-argument-violation who
	 "expected SOB objects with equal comparator" sob1 sob2)))
    ((who sob1 sob2 sob3 sobs)
     (unless (%sob-check-comparators (cons* sob1 sob2 sob3 sobs))
       (procedure-argument-violation who
	 "expected SOB objects with equal comparator" sob1 sob2 sob3 sobs))))

  (define (%sob-check-comparators sobs)
    ;;Given a  proper list of SOB  objects: return true if  all the SOBs have  the same
    ;;comparator according to EQ?.
    ;;
    (when (pair? sobs)
      (let ((first-compar (sob-comparator (car sobs))))
	(for-all (lambda (sob)
		   (eq? first-compar (sob-comparator sob)))
	  (cdr sobs)))))

  #| end of module |# )

(define (%check-element who sob element)
  ;;This procedure defends against inserting an  ELEMENT into a SOB that violates its
  ;;constructor, since typical hashtable implementations don't check for us.
  ;;
  (comparator-check-type (sob-comparator sob) element who))


;;;; constructors

(define (make-sob comparator multi?)
  ;;Construct an arbitrary empty SOB out of nothing.
  ;;
  (%make-sob (make-comparator-hashtable comparator) comparator multi?))

(define (sob-copy sob)
  ;;Copy a SOB, sharing the constructor.
  ;;
  (%make-sob (hashtable-copy (sob-hash-table sob) #t)
	     (sob-comparator sob)
	     (sob-multi? sob)))

(define* (set-copy {set set?})
  (sob-copy set))

(define* (bag-copy {bag bag?})
  (sob-copy bag))

(define (sob-empty-copy sob)
  ;;Construct an empty sob that shares the constructor of an existing sob.
  ;;
  (make-sob (sob-comparator sob) (sob-multi? sob)))

(define* (set {comparator comparator?} . elements)
  ;;Construct a set and insert elements into it.
  ;;
  (receive-and-return (result)
      (make-sob comparator #f)
    (for-each (lambda (x)
		(sob-increment! __who__ result x 1))
      elements)))

(define* (bag {comparator comparator?} . elements)
  ;;Construct a set and insert elements into it.
  ;;
  (receive-and-return (result)
      (make-sob comparator #t)
    (for-each (lambda (x)
		(sob-increment! __who__ result x 1))
      elements)))

(define (sob-unfold K stop? mapper successor seed multi?)
  ;;The  fundamental (as  opposed to  simplest)  constructor: unfold  the results  of
  ;;iterating a function as a set.  In line with SRFI 1, we provide an opportunity to
  ;;map the sequence of seeds through a mapper function.
  ;;
  (receive-and-return (result)
      (make-sob K multi?)
    (let loop ((seed seed))
      (unless (stop? seed)
	(sob-increment! __who__ result (mapper seed) 1)
	(loop (successor seed))))))

(define* (set-unfold {K comparator?} {stop? procedure?} {mapper procedure?} {successor procedure?} seed)
  (sob-unfold K stop? mapper successor seed #f))

(define* (bag-unfold {K comparator?} {stop? procedure?} {mapper procedure?} {successor procedure?} seed)
  (sob-unfold K stop? mapper successor seed #t))


;;;; predicates

(define (sob-contains? sob member)
  (hashtable-contains? (sob-hash-table sob) member))

(define* (set-contains? {set set?} member)
  (sob-contains? set member))

(define* (bag-contains? {bag bag?} member)
  (sob-contains? bag member))

(define (sob-empty? sob)
  ;;A SOB is empty if its size is 0.
  ;;
  (zero? (hashtable-size (sob-hash-table sob))))

(define* (set-empty? {set set?})
  (sob-empty? set))

(define* (bag-empty? {bag bag?})
  (sob-empty? bag))

(define (sob-half-disjoint? A B)
  ;;Two SOBs  are disjoint if,  when looping  through one, we  can't find any  of its
  ;;elements in the other.  We have  to try both ways: SOB-HALF-DISJOINT? checks just
  ;;one direction for simplicity.
  ;;
  (let ((HB (sob-hash-table B)))
    (not (hashtable-exists-key (lambda (key)
				 (hashtable-contains? HB key))
			       (sob-hash-table A)))))

(define* (set-disjoint? {a set?} {b set?})
  (%check-same-comparator __who__ a b)
  (and (sob-half-disjoint? a b)
       (sob-half-disjoint? b a)))

(define* (bag-disjoint? {a bag?} {b bag?})
  (%check-same-comparator __who__ a b)
  (and (sob-half-disjoint? a b)
       (sob-half-disjoint? b a)))


;;;; accessors

(define (sob-member sob element default)
  ;;If two objects are indistinguishable by the comparator's equality procedure, only
  ;;one of  them will be  represented in  the SOB.  This  procedure lets us  find out
  ;;which one it is; it will return the value  stored in the SOB that is equal to the
  ;;element.  Note  that we  have to search  the whole hashtable  item by  item.  The
  ;;DEFAULT is returned if there is no such element.
  ;;
  (define-constant COMPAR
    (sob-comparator sob))
  (or (vector-find (lambda (key)
		     (=? COMPAR key element))
	(hashtable-keys (sob-hash-table sob)))
      default))

(define* (set-member {set set?} element default)
  (sob-member set element default))

(define* (bag-member {bag bag?} element default)
  (sob-member bag element default))

(define* (set-element-comparator {set set?})
  (sob-comparator set))

(define* (bag-element-comparator {bag bag?})
  (sob-comparator bag))


;;;; updaters (pure functional and linear update)

(define (sob-increment! who sob element count)
  ;;Primitive operation to add  an element to a SOB.  There are a  few cases where we
  ;;bypass this for efficiency.
  ;;
  (%check-element who sob element)
  (hashtable-update! (sob-hash-table sob)
		     element
		     (if (sob-multi? sob)
			 (lambda (value) (+ value count))
		       (lambda (value) 1))
		     0))

(define (sob-decrement! who sob element count)
  ;;Primitive operation to remove an element from a SOB.
  ;;
  ;;Note this  procedure is  incomplete: it allows  the count of  an element  to drop
  ;;below 1.  Therefore, whenever it is used it is necessary to call SOB-CLEANUP!  to
  ;;fix things  up.  This is  done because it  is unsafe to  remove an object  from a
  ;;hashtable while iterating through it.
  ;;
  (%check-element who sob element)
  (hashtable-update! (sob-hash-table sob)
		     element
		     (lambda (value)
		       (- value count))
		     0))

(module (sob-cleanup!)

  (define (sob-cleanup! sob)
    ;;This is the cleanup procedure, which happens in two passes: it iterates through
    ;;the SOB,  deciding which elements  to remove (those with  non-positive counts),
    ;;and collecting  them in  a list.   When the iteration  is done,  it is  safe to
    ;;remove the elements using the list, because we are no longer iterating over the
    ;;hash table.   It returns its argument,  because it is often  tail-called at the
    ;;end of some procedure that wants to return the clean SOB.
    ;;
    (let ((ht (sob-hash-table sob)))
      (for-each (lambda (key)
		  (hashtable-delete! ht key))
	(%hashtable-keys-with-non-positive-value ht))
      sob))

  (define (%hashtable-keys-with-non-positive-value ht)
    (hashtable-fold-entries
	(lambda (key val nil)
	  (if (non-positive? val)
	      (cons key nil)
	    nil))
      '()
      ht))

  #| end of module |# )

(define* (bag-increment! {bag bag?} element {count non-negative-exact-integer?})
  (sob-increment! __who__ bag element count)
  bag)

(define* (bag-decrement! {bag bag?} element {count non-negative-exact-integer?})
  (sob-decrement! __who__ bag element count)
  ;;This returns BAG.
  (sob-cleanup! bag))

;;;

(define (sob-adjoin-all! sob elements)
  ;;The primitive operation to add elements from a list.  Return SOB itself.
  ;;
  (for-each
      (lambda (elem)
	(sob-increment! __who__ sob elem 1))
    elements)
  sob)

(define* (set-adjoin! {set set?} . elements)
  (sob-adjoin-all! set elements))

(define* (bag-adjoin! {bag bag?} . elements)
  (sob-adjoin-all! bag elements))

;;;

(define* (set-adjoin {set set?} . elements)
  (receive-and-return (result)
      (sob-copy set)
    (sob-adjoin-all! result elements)))

(define* (bag-adjoin {bag bag?} . elements)
  (receive-and-return (result)
      (sob-copy bag)
    (sob-adjoin-all! result elements)))

;;;

(define (sob-replace! who sob new-element)
  ;;Given an  element which  resides in  a set,  this makes  sure that  the specified
  ;;element is  represented by  the form  given.  Thus if  a SOB  contains 2  and the
  ;;equality predicate is "=", then calling:
  ;;
  ;;   (sob-replace! __who__ sob 2.0)
  ;;
  ;;will replace  the 2 with 2.0.   Does nothing if there  is no such element  in the
  ;;SOB.
  ;;
  ;;Return SOB itself.
  ;;
  (%check-element who sob new-element)
  (hashtable-replace-key-representation! (sob-hash-table sob) new-element)
  sob)

(define* (set-replace! {set set?} element)
  (sob-replace! __who__ set element))

(define* (bag-replace! {bag bag?} element)
  (sob-replace! __who__ bag element))

(define* (set-replace {set set?} element)
  (sob-replace! __who__ (sob-copy set) element))

(define* (bag-replace {bag bag?} element)
  (sob-replace! __who__ (sob-copy bag) element))

;;;

(define (sob-delete-all! who sob elements)
  ;;The primitive  operation to delete  elements from a list.   Like SOB-ADJOIN-ALL!,
  ;;this is  exposed two ways.  It  calls SOB-CLEANUP!  itself, so  its callers don't
  ;;need to (though it is safe to do so).  Return SOB itself.
  ;;
  (for-each (lambda (element)
	      (sob-decrement! who sob element 1))
    elements)
  ;;This returns SOB.
  (sob-cleanup! sob))

(define* (set-delete! {set set?} . elements)
  (sob-delete-all! __who__ set elements))

(define* (bag-delete! {bag bag?} . elements)
  (sob-delete-all! __who__ bag elements))

(define* (set-delete-all! {set set?} {elements list?})
  (sob-delete-all! __who__ set elements))

(define* (bag-delete-all! {bag bag?} {elements list?})
  (sob-delete-all! __who__ bag elements))

(define* (set-delete {set set?} . elements)
  (sob-delete-all! __who__ (sob-copy set) elements))

(define* (bag-delete {bag bag?} . elements)
  (sob-delete-all! __who__ (sob-copy bag) elements))

(define* (set-delete-all {set set?} {elements list?})
  (sob-delete-all! __who__ (sob-copy set) elements))

(define* (bag-delete-all {bag bag?} {elements list?})
  (sob-delete-all! __who__ (sob-copy bag) elements))

;;;

;;Flag used by SOB-SEARCH! to represent a missing object.  We need a unique object.
;;
(define-constant MISSING
  (string))

(define (sob-search! who sob element failure success)
  ;;Searches and then  dispatches to user-defined procedures on  failure and success,
  ;;which in turn should reinvoke a procedure to take some action on the set (insert,
  ;;ignore, replace, or remove).
  ;;
  (define (insert obj)
    (sob-increment! who sob element 1)
    (values sob obj))
  (define (ignore obj)
    (values sob obj))
  (define (update new-elem obj)
    (sob-decrement! who sob element 1)
    (sob-increment! who sob new-elem 1)
    (values (sob-cleanup! sob) obj))
  (define (remove obj)
    (sob-decrement! who sob element 1)
    (values (sob-cleanup! sob) obj))
  (let ((true-element (sob-member sob element MISSING)))
    (if (eq? true-element MISSING)
	(failure insert ignore)
      (success true-element update remove))))

(define* (set-search! {set set?} element {failure procedure?} {success procedure?})
  (sob-search! __who__ set element failure success))

(define* (bag-search! {bag bag?} element {failure procedure?} {success procedure?})
  (sob-search! __who__ bag element failure success))


;;;

(define (sob-size sob)
  ;;Return  the size  of  a SOB.   If it's  a  set, we  can  just use  the number  of
  ;;associations in the hashtable, but if it's a bag, we have to add up the counts.
  ;;
  (if (sob-multi? sob)
      (receive-and-return (result)
	  0
	(hashtable-for-each-entry (lambda (elem count)
			      (set! result (+ count result)))
	  (sob-hash-table sob)))
    (hashtable-size (sob-hash-table sob))))

(define* (set-size {set set?})
  (sob-size set))

(define* (bag-size {bag bag?})
  (sob-size bag))

;;;

(define (sob-find pred sob failure)
  ;;Search a  SOB to find  something that matches a  predicate PRED.  You  don't know
  ;;which element you will  get, so this is not as useful as  finding an element in a
  ;;list or other ordered container.  If it's not there, tail-call the FAILURE thunk.
  ;;
  (or (hashtable-find-key pred (sob-hash-table sob))
      (failure)))

(case-define* set-find
  (({pred procedure?} {set set?})
   (sob-find pred set void))
  (({pred procedure?} {set set?} {failure procedure?})
   (sob-find pred set failure)))

(case-define* bag-find
  (({pred procedure?} {bag bag?})
   (sob-find pred bag void))
  (({pred procedure?} {bag bag?} {failure procedure?})
   (sob-find pred bag failure)))

;;;

(define (sob-count pred sob)
  ;;Count the number  of elements in the  sob that satisfy the predicate.   This is a
  ;;special case of folding.
  ;;
  (sob-fold (lambda (elem total)
	      (if (pred elem)
		  (add1 total)
		total))
	    0 sob))

(define* (set-count {pred procedure?} {set set?})
  (sob-count pred set))

(define* (bag-count {pred procedure?} {bag bag?})
  (sob-count pred bag))

;;;

(define-syntax-rule (sob-any? ?pred ?sob)
  ;;Check if  any of the  elements in  a sob satisfy  a predicate.  Breaks  out early
  ;;(with call/cc) if a success is found.
  ;;
  (and (hashtable-find-key ?pred (sob-hash-table ?sob))
       #t))

(define* (set-any? {pred procedure?} {set set?})
  (sob-any? pred set))

(define* (bag-any? {pred procedure?} {bag bag?})
  (sob-any? pred bag))

;;;

(define-syntax-rule (sob-every? ?pred ?sob)
  ;;Analogous to SET-ANY?.  Breaks out early if a failure is found.
  ;;
  (hashtable-for-all-keys ?pred (sob-hash-table ?sob)))

(define* (set-every? {pred procedure?} {set set?})
  (sob-every? pred set))

(define* (bag-every? {pred procedure?} {bag bag?})
  (sob-every? pred bag))


;;;; mapping and folding

(define (do-n-times cmd n)
  ;;A utility  for iterating  a command  N times.   This is  used by  SOB-FOR-EACH to
  ;;execute  a  procedure over  the  repeated  elements in  a  bag.   Because of  the
  ;;representation of sets, it works for them too.
  ;;
  (when (positive? n)
    (cmd)
    (do-n-times cmd (sub1 n))))

(define (sob-for-each proc sob)
  ;;Basic iterator over a sob.
  ;;
  (hashtable-for-each-entry (lambda (key value)
			(do-n-times (lambda ()
				      (proc key))
				    value))
    (sob-hash-table sob)))

(define* (set-for-each {proc procedure?} {set set?})
  (sob-for-each proc set))

(define* (bag-for-each {proc procedure?} {bag bag?})
  (sob-for-each proc bag))

;;;

(define (sob-map comparator proc sob)
  ;;Fundamental mapping  operator.  We  map over  the associations  directly, because
  ;;each instance  of an  element in  a bag  will be  treated identically  anyway; we
  ;;insert them all at once with SOB-INCREMENT!.
  ;;
  (receive-and-return (result)
      (make-sob comparator (sob-multi? sob))
    (hashtable-for-each-entry (lambda (key value)
			  (sob-increment! __who__ result (proc key) value))
      (sob-hash-table sob))))

(define* (set-map {comparator comparator?} {proc procedure?} {set set?})
  (sob-map comparator proc set))

(define* (bag-map {comparator comparator?} {proc procedure?} {bag bag?})
  (sob-map comparator proc bag))

;;;

(define (sob-fold proc nil sob)
  ;;The  fundamental deconstructor.   Note that  there are  no left  vs. right  folds
  ;;because  there  is no  order.   Each  element  in a  bag  is  fed into  the  fold
  ;;separately.
  ;;
  (receive-and-return (result)
      nil
    (sob-for-each (lambda (elem)
		    (set! result (proc elem result)))
		  sob)))

(define* (set-fold {proc procedure?} nil {set set?})
  (sob-fold proc nil set))

(define* (bag-fold {proc procedure?} nil {bag bag?})
  (sob-fold proc nil bag))

;;;

(define (sob-filter pred sob)
  ;;Process every  element and copy the  ones that satisfy the  predicate.  Identical
  ;;elements are processed all at once.  This is used for both filter and remove.
  ;;
  (receive-and-return (result)
      (sob-empty-copy sob)
    (hashtable-for-each-entry (lambda (key value)
			  (when (pred key)
			    (sob-increment! __who__ result key value)))
      (sob-hash-table sob))))

(define* (set-filter {pred procedure?} {set set?})
  (sob-filter pred set))

(define* (bag-filter {pred procedure?} {bag bag?})
  (sob-filter pred bag))

(define* (set-remove {pred procedure?} {set set?})
  (sob-filter (lambda (x)
		(not (pred x)))
	      set))

(define* (bag-remove {pred procedure?} {bag bag?})
  (sob-filter (lambda (x)
		(not (pred x)))
	      bag))

;;;

(define (sob-filter! pred sob)
  ;;Process each element  and remove those that don't satisfy  the filter.  This does
  ;;its own cleanup, and is used for both filter! and remove!.
  ;;
  (hashtable-for-each-entry (lambda (key value)
			(unless (pred key)
			  (sob-decrement! __who__ sob key value)))
    (sob-hash-table sob))
  ;;This returns SOB.
  (sob-cleanup! sob))

(define* (set-filter! {pred procedure?} {set set?})
  (sob-filter! pred set))

(define* (bag-filter! {pred procedure?} {bag bag?})
  (sob-filter! pred bag))

(define* (set-remove! {pred procedure?} {set set?})
  (sob-filter! (lambda (x)
		 (not (pred x)))
	       set))

(define* (bag-remove! {pred procedure?} {bag bag?})
  (sob-filter! (lambda (x)
		 (not (pred x)))
	       bag))

;;;

(define (sob-partition pred sob)
  ;;Create two  sobs and  copy the elements  that satisfy the  predicate into  one of
  ;;them,  all others  into the  other.  This  is more  efficient than  filtering and
  ;;removing separately.
  ;;
  (let ((res1 (sob-empty-copy sob))
        (res2 (sob-empty-copy sob)))
    (hashtable-for-each-entry (lambda (key value)
			  (if (pred key)
			      (sob-increment! __who__ res1 key value)
			    (sob-increment! __who__ res2 key value)))
      (sob-hash-table sob))
    (values res1 res2)))

(define* (set-partition {pred procedure?} {set set?})
  (sob-partition pred set))

(define* (bag-partition {pred procedure?} {bag bag?})
  (sob-partition pred bag))

;;;

(define (sob-partition! pred sob)
  ;;Create a  sob and  iterate through  the given sob.   Anything that  satisfies the
  ;;predicate is left alone; anything that doesn't  is removed from the given sob and
  ;;added to the new sob.
  ;;
  (let ((result (sob-empty-copy sob)))
    (hashtable-for-each-entry
	(lambda (key value)
	  (unless (pred key)
	    (sob-decrement! __who__ sob    key value)
	    (sob-increment! __who__ result key value)))
      (sob-hash-table sob))
    (values (sob-cleanup! sob) result)))

(define* (set-partition! {pred procedure?} {set set?})
  (sob-partition! pred set))

(define* (bag-partition! {pred procedure?} {bag bag?})
  (sob-partition! pred bag))


;;;; copying and conversion

(case-define sob->list
  ;;Convert a sob to a list; a special case of SOB-FOLD.
  ;;
  ((who sob)
   (sob-fold cons '() sob))
  ((who sob compar)
   (cond ((not compar)
	  (sob-fold cons '() sob))
	 ((eq? #t compar)
	  (let ((compar (comparator-comparison-procedure (sob-comparator sob))))
	    (list-sort (lambda (x y)
			 (= -1 (compar x y)))
		       (sob-fold cons '() sob))))
	 ((procedure? compar)
	  (list-sort compar (sob-fold cons '() sob)))
	 (else
	  (procedure-argument-violation who
	    "expected boolean or comparison procedure as compar argument"
	    compar)))))


(case-define* set->list
  (({set set?})
   (sob->list __who__ set))
  (({set set?} compar)
   (sob->list __who__ set compar)))

(case-define* bag->list
  (({bag bag?})
   (sob->list __who__ bag))
  (({bag bag?} compar)
   (sob->list __who__ bag compar)))

;;;

(define (list->sob! sob list)
  ;;Convert a list to a sob.  Probably could be done using unfold, but since sobs are
  ;;mutable anyway, it's just as easy to add the elements by side effect.
  ;;
  (for-each (lambda (elem)
	      (sob-increment! __who__ sob elem 1))
    list)
  sob)

(define* (list->set {comparator comparator?} {list list?})
  (list->sob! (make-sob comparator #f) list))

(define* (list->bag {comparator comparator?} {list list?})
  (list->sob! (make-sob comparator #t) list))

(define* (list->set! {set set?} {list list?})
  (list->sob! set list))

(define* (list->bag! {bag bag?} {list list?})
  (list->sob! bag list))


;;;; subsets

(define-syntax define-subset-proc
  (syntax-rules ()
    ((_ ?who ?dyadic-who ?arg-pred ?arg-list-pred)
     (case-define* ?who
       (({sob ?arg-pred})
	#t)
       (({sob1 ?arg-pred} {sob2 ?arg-pred})
	(%check-same-comparator __who__ sob1 sob2)
	(?dyadic-who sob1 sob2))
       (({sob1 ?arg-pred} {sob2 ?arg-pred} {sob3 ?arg-pred})
	(%check-same-comparator __who__ sob1 sob2 sob3 '())
	(and (?dyadic-who sob1 sob2)
	     (?dyadic-who sob2 sob3)))
       (({sob1 ?arg-pred} {sob2 ?arg-pred} {sob3 ?arg-pred} {sob4 ?arg-pred} . {sobs ?arg-list-pred})
	(%check-same-comparator __who__ sob1 sob2 sob3 sobs)
	(and (?dyadic-who sob1 sob2)
	     (?dyadic-who sob2 sob3)
	     (apply ?who sob3 sob4 sobs)))))
    ))

;;; --------------------------------------------------------------------

(module (set=? bag=?)

  (define-subset-proc set=? dyadic-sob=? set? %list-of-sets?)
  (define-subset-proc bag=? dyadic-sob=? bag? %list-of-bags?)

  (define (dyadic-sob=? sob1 sob2)
    ;;First we check that  there are the same number of entries  in the hashtables of
    ;;the two sobs; if that's not true, they  can't be equal.  Then we check that for
    ;;each key, the values are the same (where  being absent counts as a value of 0).
    ;;If any values aren't equal, again they can't be equal.
    ;;
    (let ((ht1 (sob-hash-table sob1))
	  (ht2 (sob-hash-table sob2)))
      (and (= (hashtable-size ht1)
	      (hashtable-size ht2))
	   (hashtable-for-all-entries (lambda (key1 val1)
					(= val1 (hashtable-ref ht2 key1 0)))
				      ht1))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module SET-LESS-THAN/EQUAL-TO
  (set<=? bag<=? dyadic-sob<=?)

  (define-subset-proc set<=? dyadic-sob<=? set? %list-of-sets?)
  (define-subset-proc bag<=? dyadic-sob<=? bag? %list-of-bags?)

  (define (dyadic-sob<=? sob1 sob2)
    ;;Every key in SOB1's hashtable must be a key in SOB2's hashtable.
    ;;
    (let ((ht1 (sob-hash-table sob1))
	  (ht2 (sob-hash-table sob2)))
      (and (<= (hashtable-size ht1)
	       (hashtable-size ht2))
	   (hashtable-for-all-entries (lambda (key1 val1)
					(<= val1 (hashtable-ref ht2 key1 0)))
				      ht1))))

  #| end of module |# )

(module (set<=? bag<=?)
  (import SET-LESS-THAN/EQUAL-TO))

;;; --------------------------------------------------------------------

(module SET-GREATER-THAN
  (set>? bag>? dyadic-sob>?)
  (module (dyadic-sob<=?)
    (import SET-LESS-THAN/EQUAL-TO))

  (define-subset-proc set>? dyadic-sob>? set? %list-of-sets?)
  (define-subset-proc bag>? dyadic-sob>? bag? %list-of-bags?)

  (define (dyadic-sob>? sob1 sob2)
    ;;This is the negation  of <=.  Note that this is only true  at the dyadic level;
    ;;we can't just replace SOB>? with a negation of SOB<=?.
    ;;
    (not (dyadic-sob<=? sob1 sob2)))

  #| end of module |# )

(module (set>? bag>?)
  (import SET-GREATER-THAN))

;;; --------------------------------------------------------------------

(module SET-LESS-THAN
  (set<? bag<? dyadic-sob<?)
  (module (dyadic-sob>?)
    (import SET-GREATER-THAN))

  (define-subset-proc set<? dyadic-sob<? set? %list-of-sets?)
  (define-subset-proc bag<? dyadic-sob<? bag? %list-of-bags?)

  (define (dyadic-sob<? sob1 sob2)
    ;;This is the inverse of >.  Note that  this is only true at the dyadic level; we
    ;;can't just replace SOB<? with the inverse of SOB>?.
    ;;
    (dyadic-sob>? sob2 sob1))

  #| end of module |# )

(module (set<? bag<?)
  (import SET-LESS-THAN))

;;; --------------------------------------------------------------------

(module (set>=? bag>=?)
  (module (dyadic-sob<?)
    (import SET-LESS-THAN))

  (define-subset-proc set>=? dyadic-sob>=? set? %list-of-sets?)
  (define-subset-proc bag>=? dyadic-sob>=? bag? %list-of-bags?)

  (define (dyadic-sob>=? sob1 sob2)
    ;;This is the negation of <.  Note that this is only true at the dyadic level; we
    ;;can't just replace SOB>=? with the negation of SOB<?.
    ;;
    (not (dyadic-sob<? sob1 sob2)))

  #| end of module |# )


;;;; set theory operations
;;
;;The logic of  union, intersection, difference, and  sum is the same:  the SOB-* and
;;SOB-*!   procedures  do  the  reduction  to  the  DYADIC-SOB-*!   procedures.   The
;;difference is that the SOB-* procedures allocate  an empty copy of the first SOB to
;;accumulate the  results in,  whereas the  SOB-*!  procedures  work directly  in the
;;first SOB.
;;
;;Note that there is no SET-SUM, as it is the same as SET-UNION.
;;

(define-syntax define-set-theory-proc
  (syntax-rules ()
    ((_ ?who ?sob-who ?arg-pred ?arg-list-pred)
     (case-define* ?who
       (({sob ?arg-pred})
	(?sob-who sob))
       (({sob1 ?arg-pred} {sob2 ?arg-pred})
	(%check-same-comparator __who__ sob1 sob2)
	(?sob-who sob1 sob2))
       (({sob1 ?arg-pred} {sob2 ?arg-pred} {sob3 ?arg-pred} . {sobs ?arg-list-pred})
	(%check-same-comparator __who__ sob1 sob2 sob3 sobs)
	(?sob-who sob1 sob2 sob3 sobs))))
    ))

;;; --------------------------------------------------------------------

(module (set-union bag-union set-union! bag-union!)

  (define-set-theory-proc set-union sob-union set? %list-of-sets?)
  (define-set-theory-proc bag-union sob-union bag? %list-of-bags?)

  (define-set-theory-proc set-union! sob-union! set? %list-of-sets?)
  (define-set-theory-proc bag-union! sob-union! bag? %list-of-bags?)

  (case-define sob-union
    ((sob)
     (sob-copy sob))
    ((sob1 sob2)
     (receive-and-return (result)
	 (sob-empty-copy sob1)
       (dyadic-sob-union! result sob1 sob2)))
    ((sob1 sob2 sob3 sobs)
     (receive-and-return (result)
	 (sob-empty-copy sob1)
       (dyadic-sob-union! result sob1 sob2)
       (for-each
	   (lambda (sob)
	     (dyadic-sob-union! result result sob))
	 (cons sob3 sobs)))))

  (case-define sob-union!
    ((sob)
     sob)
    ((sob1 sob2)
     (dyadic-sob-union! sob1 sob1 sob2)
     sob1)
    ((sob1 sob2 sob3 sobs)
     (dyadic-sob-union! sob1 sob1 sob2)
     (for-each
	 (lambda (sob)
	   (dyadic-sob-union! sob1 sob1 sob))
       (cons sob3 sobs))
     sob1))

  (define (dyadic-sob-union! result sob1 sob2)
    ;;For union, we take  the max of the counts of each element  found in either SOBs
    ;;and  put that  in the  result.  On  the  pass through  SOB2, we  know that  the
    ;;intersection is already accounted for, so  we just copy over things that aren't
    ;;in SOB1.
    ;;
    (let ((T1 (sob-hash-table sob1))
	  (T2 (sob-hash-table sob2))
	  (T  (sob-hash-table result)))
      (hashtable-for-each-entry
	  (lambda (key value1)
	    (hashtable-set! T key (max value1 (hashtable-ref T2 key 0))))
	T1)
      (hashtable-for-each-entry
	  (lambda (key value2)
	    (when (zero? (hashtable-ref T1 key 0))
	      (hashtable-set! T key value2)))
	T2)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (set-intersection bag-intersection set-intersection! bag-intersection!)

  (define-set-theory-proc set-intersection sob-intersection set? %list-of-sets?)
  (define-set-theory-proc bag-intersection sob-intersection bag? %list-of-bags?)

  (define-set-theory-proc set-intersection! sob-intersection! set? %list-of-sets?)
  (define-set-theory-proc bag-intersection! sob-intersection! bag? %list-of-bags?)

  (case-define sob-intersection
    ((sob)
     (sob-copy sob))
    ((sob1 sob2)
     (let ((result (sob-empty-copy sob1)))
       (dyadic-sob-intersection! result sob1 sob2)
       ;;This returns RESULT.
       (sob-cleanup! result)))
    ((sob1 sob2 sob3 sobs)
     (let ((result (sob-empty-copy sob1)))
       (dyadic-sob-intersection! result sob1 sob2)
       (for-each
	   (lambda (sob)
	     (dyadic-sob-intersection! result result sob))
	 (cons sob3 sobs))
       ;;This returns RESULT.
       (sob-cleanup! result))))

  (case-define sob-intersection!
    ((sob)
     sob)
    ((sob1 sob2)
     (dyadic-sob-intersection! sob1 sob1 sob2)
     ;;This returns SOB1.
     (sob-cleanup! sob1))
    ((sob1 sob2 sob3 sobs)
     (dyadic-sob-intersection! sob1 sob1 sob2)
     (for-each
	 (lambda (sob)
	   (dyadic-sob-intersection! sob1 sob1 sob))
       (cons sob3 sobs))
     ;;This returns SOB1.
     (sob-cleanup! sob1)))

  (define (dyadic-sob-intersection! result sob1 sob2)
    ;;For intersection, we  compute the min of  the counts of each  element.  We only
    ;;have to scan SOB1.  We clean up the result  when we are done, in case it is the
    ;;same as SOB1.
    ;;
    (let ((T1 (sob-hash-table sob1))
	  (T2 (sob-hash-table sob2))
	  (T  (sob-hash-table result)))
      (hashtable-for-each-entry
	  (lambda (key value1)
	    (hashtable-set! T key (min value1 (hashtable-ref T2 key 0))))
	T1)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (set-difference bag-difference set-difference! bag-difference!)

  (define-set-theory-proc set-difference sob-difference set? %list-of-sets?)
  (define-set-theory-proc bag-difference sob-difference bag? %list-of-bags?)

  (define-set-theory-proc set-difference! sob-difference! set? %list-of-sets?)
  (define-set-theory-proc bag-difference! sob-difference! bag? %list-of-bags?)

  (case-define sob-difference
    ((sob)
     (sob-copy sob))
    ((sob1 sob2)
     (let ((result (sob-empty-copy sob1)))
       (dyadic-sob-difference! result sob1 sob2)
       ;;This returns RESULT.
       (sob-cleanup! result)))
    ((sob1 sob2 sob3 sobs)
     (let ((result (sob-empty-copy sob1)))
       (dyadic-sob-difference! result sob1 sob2)
       (for-each
	   (lambda (sob) (dyadic-sob-difference! result result sob))
	 (cons sob3 sobs))
       ;;This returns RESULT.
       (sob-cleanup! result))))

  (case-define sob-difference!
    ((sob)
     sob)
    ((sob1 sob2)
     (dyadic-sob-difference! sob1 sob1 sob2)
     ;;This returns SOB1.
     (sob-cleanup! sob1))
    ((sob1 sob2 sob3 . sobs)
     (dyadic-sob-difference! sob1 sob1 sob2)
     (for-each
	 (lambda (sob) (dyadic-sob-difference! sob1 sob1 sob))
       sobs)
     ;;This returns SOB1.
     (sob-cleanup! sob1)))

  (define (dyadic-sob-difference! result sob1 sob2)
    ;;For difference, we use (big surprise)  the numeric difference, bounded by zero.
    ;;We only need to scan SOB1, but we clean up the result in case it is the same as
    ;;SOB1.
    ;;
    (let ((sob1-ht (sob-hash-table sob1))
	  (sob2-ht (sob-hash-table sob2))
	  (result-ht (sob-hash-table result)))
      (hashtable-for-each-entry
	  (lambda (key value1)
	    (let ((value2 (hashtable-ref sob2-ht key 0)))
	      (hashtable-set! result-ht key (- value1 value2))))
	sob1-ht)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (set-xor bag-xor set-xor! bag-xor!)

  (define-syntax define-xor-proc
    (syntax-rules ()
      ((_ ?who ?sob-who ?arg-pred)
       (define* (?who {sob1 ?arg-pred} {sob2 ?arg-pred})
	 (%check-same-comparator __who__ sob1 sob2)
	 (?sob-who sob1 sob2)))
      ))

  (define-syntax define-xor-proc!
    (syntax-rules ()
      ((_ ?who ?sob-who ?arg-pred)
       (define* (?who {sob1 ?arg-pred} {sob2 ?arg-pred})
	 (%check-same-comparator __who__ sob1 sob2)
	 (?sob-who sob1 sob1 sob2)))
      ))

  (define-xor-proc set-xor sob-xor set?)
  (define-xor-proc bag-xor sob-xor bag?)


  ;;It should be this, according to the intentions of the reference implementation:
  ;;
  ;; (define-xor-proc! set-xor! sob-xor! set?)
  ;; (define-xor-proc! bag-xor! sob-xor! bag?)
  ;;
  ;;but SOB-XOR! does not work correctly when RESULT and SOB1 are the same, so:
  (define-xor-proc set-xor! sob-xor set?)
  (define-xor-proc bag-xor! sob-xor bag?)

  (define (sob-xor sob1 sob2)
    (sob-xor! (sob-empty-copy sob1) sob1 sob2))

  (define (sob-xor! result sob1 sob2)
    ;;For XOR  exactly two arguments  are required, so  the above structures  are not
    ;;necessary.   This  version accepts  a  result  SOB  and computes  the  absolute
    ;;difference between the counts in the  first SOB and the corresponding counts in
    ;;the second.
    ;;
    ;;We start by  copying the entries in the  second SOB but not the  first into the
    ;;first.  Then  we scan the first  SOB, computing the absolute  difference of the
    ;;values and writing  them back into the  first SOB.  It's essential  to scan the
    ;;second SOB first, as  we are not going to damage it in  the process.  (Hat tip:
    ;;Sam Tobin-Hochstadt.)
    ;;
    (let ((T1 (sob-hash-table sob1))
	  (T2 (sob-hash-table sob2))
	  (T  (sob-hash-table result)))
      (hashtable-for-each-entry
	  (lambda (key2 value2)
	    ;;If KEY2 is not in T1: add it to the result.
	    (when (zero? (hashtable-ref T1 key2 0))
	      (hashtable-set! T key2 value2)))
	T2)
      (hashtable-for-each-entry
	  (lambda (key1 value1)
	    ;;Examples:
	    ;;
	    ;;  (let ((value1 1)
	    ;;        (value2 0)
	    ;;    (abs (- value1 value2))) => 1
	    ;;
	    ;;  (let ((value1 1)
	    ;;        (value2 1)
	    ;;    (abs (- value1 value2))) => 0
	    ;;
	    ;;  (let ((value1 3)
	    ;;        (value2 1)
	    ;;    (abs (- value1 value2))) => 2
	    ;;
	    ;;  (let ((value1 1)
	    ;;        (value2 2)
	    ;;    (abs (- value1 value2))) => 1
	    ;;
	    ;;  (let ((value1 1)
	    ;;        (value2 4)
	    ;;    (abs (- value1 value2))) => 3
	    ;;
	    (let ((value2 (hashtable-ref T2 key1 0)))
	      (hashtable-set! T key1 (abs (- value1 value2)))))
	T1)
      ;;This returns RESULT.
      (sob-cleanup! result)))

  #| end of module |# )


;;;; bag-specific procedures

(module (bag-sum bag-sum!)
  ;;Sum is defined for bags only; for sets, it is the same as union.

  (define-set-theory-proc bag-sum  sob-sum  bag? %list-of-bags?)
  (define-set-theory-proc bag-sum! sob-sum! bag? %list-of-bags?)

  (case-define sob-sum
    ((sob)
     (sob-copy sob))
    ((sob1 sob2)
     (receive-and-return (result)
	 (sob-empty-copy sob1)
       (dyadic-sob-sum! result sob1 sob2)))
    ((sob1 sob2 sob3 sobs)
     (receive-and-return (result)
	 (sob-empty-copy sob1)
       (dyadic-sob-sum! result sob1 sob2)
       (for-each
	   (lambda (sob)
	     (dyadic-sob-sum! result result sob))
	 (cons sob3 sobs)))))

  (case-define sob-sum!
    ((sob)
     sob)
    ((sob1 sob2)
     (dyadic-sob-sum! sob1 sob1 sob2)
     sob1)
    ((sob1 sob2 sob3 sobs)
     (dyadic-sob-sum! sob1 sob1 sob2)
     (for-each
	 (lambda (sob)
	   (dyadic-sob-sum! sob1 sob1 sob))
       (cons sob3 sobs))
     sob1))

  (define (dyadic-sob-sum! result sob1 sob2)
    ;;Sum is just like union, except that we take the sum rather than the max.
    ;;
    (let ((T1 (sob-hash-table sob1))
	  (T2 (sob-hash-table sob2))
	  (T  (sob-hash-table result)))
      (hashtable-for-each-entry
	  (lambda (key value1)
	    (hashtable-set! T key (+ value1 (hashtable-ref T2 key 0))))
	T1)
      (hashtable-for-each-entry
	  (lambda (key value2)
	    (when (zero? (hashtable-ref T1 key 0))
	      (hashtable-set! T key value2)))
	T2)))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (bag-product bag-product!)

  (define* (bag-product {n non-negative-exact-integer?} {bag bag?})
    (sob-product! (sob-empty-copy bag) bag n))

  (define* (bag-product! {n non-negative-exact-integer?} {bag bag?})
    (sob-product! bag bag n))

  (define (sob-product! result sob n)
    (let ((rht (sob-hash-table result)))
      (hashtable-for-each-entry
	  (lambda (elem count)
	    (hashtable-set! rht elem (* count n)))
	(sob-hash-table sob))
      result))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (bag-unique-size {bag bag?})
  (hashtable-size (sob-hash-table bag)))

(define* (bag-element-count {bag bag?} elem)
  (hashtable-ref (sob-hash-table bag) elem 0))

(define* (bag-for-each-unique {proc procedure?} {bag bag?})
  (hashtable-for-each-entry
      proc
    (sob-hash-table bag)))

(define* (bag-fold-unique {proc procedure?} nil {bag bag?})
  (hashtable-fold-entries proc nil (sob-hash-table bag)))

;;; --------------------------------------------------------------------

(define* (bag->set {bag bag?})
  (hashtable-fold-entries
      (lambda (key value nil)
	(sob-increment! __who__ nil key value)
	nil)
    (set (sob-comparator bag))
    (sob-hash-table bag)))

(define* (set->bag {set set?})
  (hashtable-fold-entries
      (lambda (key val nil)
	(sob-increment! __who__ nil key val)
	nil)
    (make-sob (sob-comparator set) #t)
    (sob-hash-table set)))

(define* (set->bag! {bag bag?} {set set?})
  (%check-same-comparator __who__ set bag)
  (hashtable-fold-entries
      (lambda (key value nil)
	(sob-increment! __who__ nil key value)
	nil)
    bag
    (sob-hash-table set)))

;;; --------------------------------------------------------------------

(case-define* bag->alist
  (({bag bag?})
   (bag-fold-unique
       (lambda (elem count list)
	 (cons (cons elem count) list))
     '()
     bag))
  (({bag bag?} compar)
   (let ((al (bag-fold-unique
		 (lambda (elem count list)
		   (cons (cons elem count) list))
	       '()
	       bag)))
     (cond ((not compar)
	    al)
	   ((eq? compar #t)
	    (let ((compar (comparator-comparison-procedure (sob-comparator bag))))
	      (list-sort (lambda (x y)
			   (= -1 (compar (car x) (car y))))
			 al)))
	   ((procedure? compar)
	    (list-sort (lambda (x y)
			 (compar (car x) (car y)))
		       al))
	   (else
	    (procedure-argument-violation __who__
	      "expected boolean or procedure as COMPAR argument"
	      compar))))))

(define* (alist->bag {comparator comparator?} alist)
  (receive-and-return (result)
      (bag comparator)
    (let ((T (sob-hash-table result)))
      (for-each
	  (lambda (key.val)
	    (let ((element (car key.val)))
	      (unless (hashtable-contains? T element)
		(sob-increment! __who__ result element (cdr key.val)))))
	alist))))


;;;; set and bag comparator

(module (set-comparator bag-comparator)

  (define (sob-hash sob)
    (let ((hash (comparator-hash-function (sob-comparator sob))))
      (sob-fold (lambda (element result)
		  (+ (hash element) (* result 33)))
		5381
		sob)))

  (define set-comparator
    (make-comparator set? set=? #f sob-hash))

  (define bag-comparator
    (make-comparator bag? bag=? #f sob-hash))

  ;;Register above comparators for use by DEFAULT-COMPARATOR.
  (comparator-register-default! set-comparator)
  (comparator-register-default! bag-comparator)

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; eval: (put 'bag-fold-unique		'scheme-indent-function 1)
;; End:
