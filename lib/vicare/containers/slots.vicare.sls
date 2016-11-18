;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: low-level deque of arbitrary objects
;;;Date: Mon Aug 10, 2015
;;;
;;;Abstract
;;;
;;;	An instance of <slots> is a chain (doubly-linked list) of built-in vectors.
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; slots internals
;;
;;A "<slots>"  instance is a  doubly-linked list of  built-in Scheme vectors  used as
;;buffers for objects of any type.  The doubly-linked list is implemented with chains
;;from  the library  "(vicare containers  chains)".
;;
;;All the buffers have  the same length; the buffers length must be  at least one; at
;;most a buffer can hold "(greatest-fixnum)" objects.
;;
;;The layout of a "<slots>" instance with 3 chain links is:
;;
;;   --- first link
;;    +---> ()
;;    +---> |---|---|---|---|---|---|---| buffer
;;    +---
;;   ---  |
;;    ^   v
;;    |  --- second link
;;     ---+
;;        +---> |---|---|---|---|---|---|---| buffer
;;        +---
;;       ---  |
;;        ^   v
;;        |  --- third link
;;         ---+
;;            +---> |---|---|---|---|---|---|---| buffer
;;            +---> ()
;;           ---
;;
;;Middle buffers are always fully used: every slot contains a Scheme object.
;;
;;The first buffer can  be partially used; the objects are stored  towards the end of
;;the vector; new obects are put in from the left:
;;
;;     free slots    used slots
;;   |...........|...............|
;;
;;   |---|---|---|+++|+++|+++|+++| buffer
;;             ^
;;
;;        fir-link-next-index
;;
;;the "<slots>" instance keeps the index of the next free slot in the first buffer.
;;
;;The last buffer can be partially used; the objects are stored towards the beginning
;;of the vector; new obects are put in from the right:
;;
;;       used slots   free slots
;;   |...............|...........|
;;
;;   |+++|+++|+++|+++|---|---|---| buffer
;;                     ^
;;            las-link-next-index
;;
;;the "<slots>" instance keeps the index of the next free slot in the last buffer.
;;
;;When the chain  contains only one link  (and so one buffer) a  possible scenario is
;;this:
;;
;;     free slots          used slots          free slots
;;   |...........|...........................|...........|
;;
;;   |---|---|---|+++|+++|+++|+++|+++|+++|+++|---|---|---| buffer
;;             ^                               ^
;;     fir-link-next-index              las-link-next-index
;;
;;when the chain  contains only one link (and  so one buffer) and the  only buffer is
;;empty, the scenario is this:
;;
;;                         free slots
;;   |...................................................|
;;
;;   |---|---|---|---|---|---|---|---|---|---|---|---|---| buffer
;;                             ^
;;           fir-link-next-index == las-link-next-index
;;
;;When the  chain contains two  link (and  so two buffers)  and both the  buffers are
;;empty, the following scenario is possible:
;;
;;                         free slots
;;   |...................................................|
;;
;;   |---|---|---|---|---|---|---|---|---|---|---|---|---| first buffer
;;                                                     ^
;;                                           fir-link-next-index
;;
;;                         free slots
;;   |...................................................|
;;
;;   |---|---|---|---|---|---|---|---|---|---|---|---|---| last buffer
;;     ^
;;   las-link-next-index
;;
;;
;;About the size of vectors used as buffers
;;-----------------------------------------
;;
;;Built-in Scheme  vectors use a machine  word to store  the number of slots,  so the
;;total memory block size of a vector is: one plus num-of-slots machine words.
;;
;;Vicare's garbage collector  does not move Scheme objects whose  size exceeds a page
;;size: 4096 bytes.   In a page there  are: on 32-bit systems, 4096/4  = 1024 machine
;;words; on 64-bit systems, 4096/8 = 512 machine words.
;;
;;So if we want to  allocate a vector with total size less than  a page size (so that
;;it is  moved by  the GC  and it does  not cause  memory fragmentation)  the maximum
;;number of slots is: on 32-bit systems, 1023 words; on 64-bit systems, 511 words.
;;
;;By using  15 as default buffer  length: we make  the total number of  machine words
;;allocated for each buffer equal to 16, which is nice.
;;


#!vicare
(library (vicare containers slots)
  (options typed-language)
  (export
    <slots>
    make-slots			slots?

    slots-empty?		$slots-empty?
    slots-not-empty?		$slots-not-empty?
    slots-size			$slots-size

    slots-front			$slots-front
    slots-rear			$slots-rear
    slots-push-front!		$slots-push-front!
    slots-push-rear!		$slots-push-rear!
    slots-pop-front!		$slots-pop-front!
    slots-pop-rear!		$slots-pop-rear!
    slots-purge!		$slots-purge!

    slots-fold-left		$slots-fold-left
    slots-fold-right		$slots-fold-right

    slots-map-left		$slots-map-left
    slots-map-right		$slots-map-right
    slots-for-each-left		$slots-for-each-left
    slots-for-each-right	$slots-for-each-right

    slots-for-all		$slots-for-all
    slots-find-left		$slots-find-left
    slots-find-right		$slots-find-right
    slots-exists-left		$slots-exists-left
    slots-exists-right		$slots-exists-right

    slots-filter		$slots-filter
    slots-partition		$slots-partition

    slots-copy!			$slots-copy!
    slots-reverse!		$slots-reverse!

    slots->list			$slots->list
    list->slots			$list->slots
    slots->vector		$slots->vector
    vector->slots		$vector->slots)
  (import (vicare)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $vectors)
    (vicare containers chains))


;;;; helpers

(define-type <kons>
  (lambda (<top> <top>) => (<top>)))

;;; --------------------------------------------------------------------

;;Default length of vectors used as buffers.  By using 15 we make the total number of
;;machine words allocated for each buffer equal to 16; which is nice.
;;
(define-constant DEFAULT-BUFFER-LENGTH
  15)

(define-syntax-rule ($link-ref ?lnk ?idx)
  ($vector-ref ($chain-link-ref ?lnk) ?idx))

(define-syntax-rule ($link-set! ?lnk ?idx ?obj)
  ($vector-set! ($chain-link-ref ?lnk) ?idx ?obj))

(define-inline ($link-ref-and-reset! lnk idx)
  ;;Extract and return the object at index IDX in the chain link LNK.  Reset the slot
  ;;to void, so that the extracted object can be garbage collected.
  ;;
  (let ((vec ($chain-link-ref lnk)))
    (receive-and-return (obj)
	($vector-ref vec idx)
      ($vector-set-void! vec idx))))

;;; --------------------------------------------------------------------

(define ($slots-buffer-greatest-index slots)
  ;;Return a non-negative fixnum representing the greatest valid index in a buffer.
  ;;
  ($fxsub1 ($<slots>-buffer-length slots)))

;;; --------------------------------------------------------------------

(define ($slots-detach-and-cache-front-link! slots lnk)
  ;;If SLOTS  does not already  has a cached front  link: cache LNK.   Otherwise just
  ;;discard LNK.  This  way we keep cached  the older link object, which  is good for
  ;;garbage collection.
  ;;
  ($chain-link-remove! lnk)
  (unless ($<slots>-front-cache slots)
    ($<slots>-front-cache-set! slots lnk)))

(define ($slots-detach-and-cache-rear-link! slots lnk)
  ;;If SLOTS  does not  already has a  cached rear link:  cache LNK.   Otherwise just
  ;;discard LNK.  This  way we keep cached  the older link object, which  is good for
  ;;garbage collection.
  ;;
  ($chain-link-remove! lnk)
  (unless ($<slots>-rear-cache slots)
    ($<slots>-rear-cache-set! slots lnk)))


(define-record-type (<slots> make-slots slots?)
  (nongenerative vicare:containers:<slots>)

  (fields (mutable fir-link)
		;The first link in a chain of buffers.
	  (mutable las-link)
		;The last link in a chain of buffers.
		;
		;NOTE The  strings "fir-link"  and "las-link"  have the  same length,
		;which  is  good for  code  indentation;  so  they are  preferred  to
		;"first-link" and "last-link".
	  (mutable fir-link-next-idx)
		;A fixnum representing the index in  the first link's vector in which
		;the next object must be put.   When negative: the first link is full
		;in the backwards direction.
	  (mutable las-link-next-idx)
		;A fixnum representing  the index in the last link's  vector in which
		;the next object  must be put.  When equal to  the vector length: the
		;last link is full in the forwards direction.
	  (mutable front-cache)
		;False or  a previously built  chain link that  is cached here  to be
		;reused when the deque grows from the front side.
	  (mutable rear-cache)
		;False or  a previously built  chain link that  is cached here  to be
		;reused when the deque grows from the rear side.
	  (immutable buffer-length)
	  (mutable size))

  (protocol
   (lambda (make-record)
     (case-define* make-<slots>
       (()
	(make-<slots> DEFAULT-BUFFER-LENGTH))

       (({buffer-length positive-fixnum?})
	;;BUFFER-LENGTH is the number of slots of each vector (at least 1).
	;;
	(define (mk-link)
	  (make-chain-link (make-vector buffer-length)))
	(let* ((first-link (mk-link))
	       (last-link  first-link)
	       (middle-idx (div buffer-length 2)))
	  (make-record first-link last-link
		       middle-idx ;first-link-next-idx
		       middle-idx ;last-link-next-idx
		       #f	  ;front-cache
		       #f	  ;rear-cache
		       buffer-length
		       0 ;size
		       )))
       #| end of CASE-DEFINE* |# )
     make-<slots>))

  #| end of DEFINE-RECORD-TYPE |# )


;;;; size

(define-alias slots-size	<slots>-size)
(define-alias $slots-size	$<slots>-size)

(define ($slots-incr-size! slots)
  ($<slots>-size-set! slots (add1 ($<slots>-size slots))))

(define ($slots-decr-size! slots)
  ($<slots>-size-set! slots (sub1 ($<slots>-size slots))))


;;;; emptyness predicates

(define* (slots-empty? {slots slots?})
  ($slots-empty? slots))

(define* ($slots-empty? slots)
  ($fxzero? (<slots>-size slots)))

;;; --------------------------------------------------------------------

(define* (slots-not-empty? {slots slots?})
  ($slots-not-empty? slots))

(define ($slots-not-empty? slots)
  ($fxpositive? (<slots>-size slots)))

;;; --------------------------------------------------------------------

(define* (slots-first-buffer-full? {slots slots?})
  ($slots-first-buffer-full? slots))

(define ($slots-first-buffer-full? slots)
  ($fxnegative? ($<slots>-fir-link-next-idx slots)))

(define* (slots-last-buffer-full? {slots slots?})
  ($slots-last-buffer-full? slots))

(define ($slots-last-buffer-full? slots)
  ($fx= ($<slots>-las-link-next-idx slots)
	($<slots>-buffer-length     slots)))

;;; --------------------------------------------------------------------

(define* (slots-first-buffer-empty? {slots slots?})
  ($slots-first-buffer-empty? slots))

(define ($slots-first-buffer-empty? slots)
  (let ((idx ($<slots>-fir-link-next-idx slots)))
    (or ($fx= idx ($fxsub1 ($<slots>-buffer-length slots)))
	($fx= idx ($<slots>-las-link-next-idx slots)))))

(define* (slots-last-buffer-empty? {slots slots?})
  ($slots-last-buffer-empty? slots))

(define ($slots-last-buffer-empty? slots)
  (let ((idx ($<slots>-las-link-next-idx slots)))
    (or ($fxzero? idx)
	($fx= idx ($<slots>-fir-link-next-idx slots)))))


;;;; adding and removing buffers

(define* (slots-add-front-buffer! {slots slots?})
  ;;Add a new  slots buffer to the front of  the chain; set the last slot  in the new
  ;;buffer as next  slot to be used.  It  is an error to call this  function when the
  ;;first buffer is not full.
  ;;
  (if ($slots-first-buffer-full? slots)
      ($slots-add-front-buffer! slots)
    (assertion-violation __who__
      "attempt to add a new front buffer when the old one is not full"
      slots)))

(define ($slots-add-front-buffer! slots)
  (let* ((old-first-link ($<slots>-fir-link slots))
	 (buffer-length  ($<slots>-buffer-length slots))
	 (new-first-link (let ((cached ($<slots>-front-cache slots)))
			   (if cached
			       (begin
				 ($<slots>-front-cache-set! slots #f)
				 cached)
			     (make-chain-link (make-vector buffer-length))))))
    ($chain-link-next-set! new-first-link old-first-link)
    ($<slots>-fir-link-set!          slots new-first-link)
    ($<slots>-fir-link-next-idx-set! slots (fxsub1 buffer-length))))

;;; --------------------------------------------------------------------

(define* (slots-add-rear-buffer! {slots slots?})
  ;;Add a new  slots buffer to the rear of  the chain; set the first slot  in the new
  ;;buffer as next  slot to be used.  It  is an error to call this  function when the
  ;;last buffer is not full.
  ;;
  (if ($slots-last-buffer-full? slots)
      ($slots-add-rear-buffer! slots)
    (assertion-violation __who__
      "attempt to add a new rear buffer when the old one is not full"
      slots)))

(define ($slots-add-rear-buffer! slots)
  (let* ((old-last-link ($<slots>-las-link     slots))
	 (buffer-length ($<slots>-buffer-length slots))
	 (new-last-link (let ((cached ($<slots>-rear-cache slots)))
			  (if cached
			      (begin
				($<slots>-rear-cache-set! slots #f)
				cached)
			  (make-chain-link (make-vector buffer-length))))))
    ($chain-link-prev-set! new-last-link old-last-link)
    ($<slots>-las-link-set!          slots new-last-link)
    ($<slots>-las-link-next-idx-set! slots 0)))


;;;; front and rear accessors and mutators

(define* (slots-front {slots slots?})
  ($slots-front slots))

(define* ($slots-front slots)
  (let ((fir-lnk ($<slots>-fir-link slots))
	(las-lnk ($<slots>-las-link slots))
	(fir-idx ($<slots>-fir-link-next-idx slots))
	(las-idx ($<slots>-las-link-next-idx slots)))
    (cond ((eq? fir-lnk las-lnk)
	   ;;There is only one buffer.
	   (if ($fx= fir-idx las-idx)
	       ;;The only buffer is empty.
	       (assertion-violation __who__ "the container is empty" slots)
	     ;;There is at least one object in the only buffer.  The scenario is:
	     ;;
	     ;;         used slot
	     ;;            v
	     ;;  |---|---|---| ... |---|---|
	     ;;        ^             ^
	     ;;     fir-idx       las-idx
	     ;;
	     ;;FIR-IDX cannot  reference last slot  in the  buffer, so we  can safely
	     ;;increment it.
	     (begin
	       #;(assert (fx<? fir-idx ($slots-buffer-greatest-index slots)))
	       ($link-ref fir-lnk ($fxadd1 fir-idx)))))

	  (($fx= fir-idx ($slots-buffer-greatest-index slots))
	   ;;There are multiple  buffers and the first one is  empty.  Get the object
	   ;;from the next link in the chain.
	   (let ((lnk ($chain-link-next fir-lnk)))
	     (if (and (eq? lnk las-lnk)
		      ($fxzero? las-idx))
		 ;;The next buffer is empty.
		 (assertion-violation __who__ "the container is empty" slots)
	       ;;There is at least  one object in the next buffer,  we pick the first
	       ;;one.
	       ($link-ref lnk 0))))

	  (else
	   ;;There are multiple buffers and the first one is not empty.
	   ($link-ref fir-lnk ($fxadd1 fir-idx))))))

;;; --------------------------------------------------------------------

(define* (slots-rear {slots slots?})
  ($slots-rear slots))

(define* ($slots-rear slots)
  (let ((fir-lnk ($<slots>-fir-link slots))
	(las-lnk ($<slots>-las-link slots))
	(fir-idx ($<slots>-fir-link-next-idx slots))
	(las-idx ($<slots>-las-link-next-idx slots)))
    (cond ((eq? fir-lnk las-lnk)
	   ;;There is only one buffer
	   (if ($fx= fir-idx las-idx)
	       ;;The only buffer is empty.
	       (assertion-violation __who__ "the container is empty" slots)
	     ;;There is at least one object in the only buffer.  The scenario is:
	     ;;
	     ;;         used slot
	     ;;            v
	     ;;  |---|---|---| ... |---|---|
	     ;;        ^             ^
	     ;;     fir-idx       las-idx
	     ;;
	     ;;LAS-IDX cannot  reference first slot in  the buffer, so we  can safely
	     ;;decrement it.
	     (begin
	       #;(assert (fxpositive? las-idx))
	       ($link-ref las-lnk ($fxsub1 las-idx)))))

	  (($fxzero? las-idx)
	   ;;There are  multiple buffers and the  last one is empty.   Get the object
	   ;;from the prev link in the chain.
	   (let ((pre-lnk      ($chain-link-prev las-lnk))
		 (greatest-idx ($slots-buffer-greatest-index slots)))
	     (if (and (eq? fir-lnk pre-lnk)
		      ($fx= fir-idx greatest-idx))
		 ;;The prev buffer is empty.
		 (assertion-violation __who__ "the container is empty" slots)
	       ($link-ref pre-lnk greatest-idx))))

	  (else
	   ;;There are multiple buffers and the last one is not empty.
	   ($link-ref fir-lnk ($fxsub1 fir-idx))))))

;;; --------------------------------------------------------------------

(define* (slots-push-front! {slots slots?} obj)
  ($slots-push-front! slots obj))

(define ($slots-push-front! slots obj)
  (when ($slots-first-buffer-full? slots)
    ($slots-add-front-buffer! slots))
  (let ((fir-lnk ($<slots>-fir-link          slots))
	(fir-idx ($<slots>-fir-link-next-idx slots)))
    ($link-set! fir-lnk fir-idx obj)
    ($<slots>-fir-link-next-idx-set! slots ($fxsub1 fir-idx))
    ($slots-incr-size! slots)
    (let ((las-lnk ($<slots>-las-link slots)))
      (when (eq? fir-lnk las-lnk)
	(let ((las-idx ($<slots>-las-link-next-idx slots)))
	  (when ($fx= fir-idx las-idx)
	    ($<slots>-las-link-next-idx-set! slots ($fxadd1 las-idx)))))))
  (values))

;;; --------------------------------------------------------------------

(define* (slots-push-rear! {slots slots?} obj)
  ($slots-push-rear! slots obj))

(define ($slots-push-rear! slots obj)
  (when ($slots-last-buffer-full? slots)
    ($slots-add-rear-buffer! slots))
  (let ((las-lnk ($<slots>-las-link          slots))
	(las-idx ($<slots>-las-link-next-idx slots)))
    ($link-set! las-lnk las-idx obj)
    ($<slots>-las-link-next-idx-set! slots ($fxadd1 las-idx))
    ($slots-incr-size! slots)
    (let ((fir-lnk ($<slots>-fir-link slots)))
      (when (eq? fir-lnk las-lnk)
	(let ((fir-idx ($<slots>-fir-link-next-idx slots)))
	  (when ($fx= fir-idx las-idx)
	    ($<slots>-fir-link-next-idx-set! slots ($fxsub1 fir-idx)))))))
  (values))

;;; --------------------------------------------------------------------

(define* (slots-pop-front! {slots slots?})
  ($slots-pop-front! slots))

(define ($slots-pop-front! slots)
  (let ((fir-lnk ($<slots>-fir-link slots))
	(las-lnk ($<slots>-las-link slots))
	(fir-idx ($<slots>-fir-link-next-idx slots))
	(las-idx ($<slots>-las-link-next-idx slots)))
    (cond ((eq? fir-lnk las-lnk)
	   ;;There is only one buffer.
	   (if (= fir-idx las-idx)
	       ;;The only buffer is empty.
	       (assertion-violation __who__ "the container is empty" slots)
	     (let ((obj-idx ($fxadd1 fir-idx)))
	       (receive-and-return (obj)
		   ($link-ref-and-reset! fir-lnk obj-idx)
		 ($<slots>-fir-link-next-idx-set! slots obj-idx)
		 ($slots-decr-size! slots)))))

	  ((= fir-idx ($slots-buffer-greatest-index slots))
	   ;;There are multiple  buffers and the first one is  empty.  Get the object
	   ;;from the next link in the chain.
	   (let ((lnk ($chain-link-next fir-lnk)))
	     (if (and (eq? lnk las-lnk)
		      (fxzero? las-idx))
		 ;;The next buffer is empty.
		 (assertion-violation __who__ "the container is empty" slots)
	       (receive-and-return (obj)
		   ($link-ref-and-reset! lnk 0)
		 ;;The next link becomes the first link.
		 ($<slots>-fir-link-set!          slots lnk)
		 ($<slots>-fir-link-next-idx-set! slots 0)
		 ;;The old first link is detached and cached.
		 ($slots-detach-and-cache-front-link! slots fir-lnk)
		 ($slots-decr-size! slots)))))

	  (else
	   ;;There are multiple buffers and the first one is not empty.
	   (receive-and-return (obj)
	       ($link-ref-and-reset! fir-lnk ($fxadd1 fir-idx))
	     ($<slots>-fir-link-next-idx-set! slots ($fxadd1 fir-idx))
	     ($slots-decr-size! slots))))))

;;; --------------------------------------------------------------------

(define* (slots-pop-rear! {slots slots?})
  ($slots-pop-rear! slots))

(define ($slots-pop-rear! slots)
  (let ((fir-lnk ($<slots>-fir-link slots))
	(las-lnk ($<slots>-las-link slots))
	(fir-idx ($<slots>-fir-link-next-idx slots))
	(las-idx ($<slots>-las-link-next-idx slots)))
    (cond ((eq? fir-lnk las-lnk)
	   ;;There is only one buffer.
	   (if (= fir-idx las-idx)
	       ;;The only buffer is empty.
	       (assertion-violation __who__ "the container is empty" slots)
	     ;;There is at least one object in the buffer.  The scenario is:
	     ;;
	     ;;         used slot
	     ;;            v
	     ;;  |---|---|---| ... |---|---|
	     ;;        ^             ^
	     ;;     fir-idx       las-idx
	     ;;
	     ;;LAS-IDX cannot  reference the first  slot, so we can  safely decrement
	     ;;it.
	     (begin
	       #;(assert (fx<? 1 las-idx))
	       (let ((obj-idx ($fxsub1 las-idx)))
		 (receive-and-return (obj)
		     ($link-ref-and-reset! las-lnk obj-idx)
		   ($<slots>-las-link-next-idx-set! slots obj-idx)
		   ($slots-decr-size! slots))))))

	  (($fxzero? las-idx)
	   ;;There are  multiple buffers and the  last one is empty.   Get the object
	   ;;from the prev link in the chain.
	   (let ((pre-lnk       ($chain-link-prev las-lnk))
		 (greatest-idx  ($slots-buffer-greatest-index slots)))
	     (if (and (eq? fir-lnk pre-lnk)
		      ($fx= fir-idx greatest-idx))
		 ;;The prev buffer is empty.
		 (assertion-violation __who__ "the container is empty" slots)
	       (receive-and-return (obj)
		   ($link-ref-and-reset! pre-lnk greatest-idx)
		 ;;The prev link becomes the last link.
		 (begin
		   ($<slots>-las-link-set!          slots pre-lnk)
		   ($<slots>-las-link-next-idx-set! slots greatest-idx))
		 ;;The old last link is detached and cached.
		 ($slots-detach-and-cache-rear-link! slots las-lnk)
		 ($slots-decr-size! slots)))))

	  (else
	   ;;There are multiple buffers and the last one is not empty.
	   (let ((obj-idx ($fxsub1 las-idx)))
	     (receive-and-return (obj)
		 ($link-ref-and-reset! las-lnk obj-idx)
	       ($<slots>-las-link-next-idx-set! slots obj-idx)
	       ($slots-decr-size! slots)))))))

;;; --------------------------------------------------------------------

(define* (slots-purge! {slots slots?})
  ($slots-purge! slots))

(define ($slots-purge! slots)
  (let ((lnk		($<slots>-fir-link slots))
	(middle-idx	(div ($<slots>-buffer-length slots) 2)))
    ($<slots>-las-link-set!          slots lnk)
    ($<slots>-fir-link-next-idx-set! slots middle-idx)
    ($<slots>-las-link-next-idx-set! slots middle-idx)
    ($<slots>-front-cache-set!       slots #f)
    ($<slots>-rear-cache-set!        slots #f)
    ($<slots>-size-set!              slots 0))
  (values))


;;;; folding from front

(define (slots-fold-left {kons <kons>} knil {slots <slots>})
  ($slots-fold-left kons knil slots))

(module ($slots-fold-left)

  (define/typed ($slots-fold-left {kons <kons>} knil {slots <slots>})
    (let ((fir-lnk ($<slots>-fir-link slots))
	  (las-lnk ($<slots>-las-link slots))
	  (fir-idx ($<slots>-fir-link-next-idx slots))
	  (las-idx ($<slots>-las-link-next-idx slots)))
      (if (eq? fir-lnk las-lnk)
	  ;;There is only one link in the chain.
	  (if ($fx= fir-idx las-idx)
	      ;;The only buffer is empty.
	      knil
	    ;;The only buffer is not empty.
	    (let loop ((vec   ($chain-link-ref fir-lnk))
		       (i     ($fxadd1 fir-idx))
		       (knil  knil))
	      (if ($fx= i las-idx)
		  knil
		(loop vec ($fxadd1 i) (kons knil ($vector-ref vec i))))))
	;;There are multiple links in the chain.
	(let ((vec.len  ($<slots>-buffer-length slots)))
	  (let loop ((knil  (%link-object-loop kons knil ($chain-link-ref fir-lnk) ($fxadd1 fir-idx) vec.len))
		     (lnk   ($chain-link-next fir-lnk)))
	    (if (eq? lnk las-lnk)
		(%link-object-loop kons knil ($chain-link-ref las-lnk) 0 las-idx)
	      (loop (%link-object-loop kons knil ($chain-link-ref lnk) 0 vec.len)
		    ($chain-link-next lnk))))))))

  (define (%link-object-loop kons knil vec i i.past)
    (if ($fx< i i.past)
	(%link-object-loop kons (kons knil ($vector-ref vec i))
			   vec ($fxadd1 i) i.past)
      knil))

  #| end of module: $SLOTS-FOLD-LEFT |# )


;;;; folding from rear

(define (slots-fold-right {kons <kons>} knil {slots <slots>})
  ($slots-fold-right kons knil slots))

(module ($slots-fold-right)

  (define/typed ($slots-fold-right {kons <kons>} knil {slots <slots>})
    (let ((fir-lnk ($<slots>-fir-link slots))
	  (las-lnk ($<slots>-las-link slots))
	  (fir-idx ($<slots>-fir-link-next-idx slots))
	  (las-idx ($<slots>-las-link-next-idx slots)))
      (if (eq? fir-lnk las-lnk)
	  ;;There is only one link in the chain.
	  (if ($fx= fir-idx las-idx)
	      ;;The only buffer is empty.
	      knil
	    ;;The only buffer is not empty.
	    (let loop ((vec   ($chain-link-ref las-lnk))
		       (i     ($fxsub1 las-idx))
		       (knil  knil))
	      (if ($fx> i fir-idx)
		  (loop vec ($fxsub1 i) (kons ($vector-ref vec i) knil))
		knil)))
	;;There are multiple links in the chain.
	(let ((first-idx ($fxsub1 ($<slots>-buffer-length slots))))
	  (let loop ((knil  (%link-object-loop kons knil ($chain-link-ref las-lnk) ($fxsub1 las-idx) 0))
		     (lnk   ($chain-link-prev las-lnk)))
	    (if (eq? lnk fir-lnk)
		(%link-object-loop kons knil ($chain-link-ref fir-lnk) first-idx ($fxadd1 fir-idx))
	      (loop (%link-object-loop kons knil ($chain-link-ref lnk) first-idx 0)
		    ($chain-link-prev lnk))))))))

  (define (%link-object-loop kons knil vec i i.first)
    (if ($fx>= i i.first)
	(%link-object-loop kons (kons ($vector-ref vec i) knil)
			   vec ($fxsub1 i) i.first)
      knil))

  #| end of module: $SLOTS-FOLD-RIGHT |# )


;;;; misc operations

(define* (slots-copy! {dst-slots slots?} {src-slots slots?})
  ($slots-copy! dst-slots src-slots))

(define ($slots-copy! dst-slots src-slots)
  ($slots-fold-left (lambda (knil obj)
		       ($slots-push-rear! dst-slots obj)
		       knil)
    dst-slots src-slots))

;;; --------------------------------------------------------------------

(define* (slots-reverse! {dst-slots slots?} {src-slots slots?})
  ($slots-reverse! dst-slots src-slots))

(define ($slots-reverse! dst-slots src-slots)
  ($slots-fold-left (lambda (knil obj)
		      ($slots-push-front! dst-slots obj)
		      knil)
    dst-slots src-slots))


;;;; mapping

(define* (slots-map-left {dst-slots slots?} {fun procedure?} {src-slots slots?})
  ($slots-map-left dst-slots fun src-slots))

(define ($slots-map-left dst-slots fun src-slots)
  ($slots-fold-left (lambda (dst-slots obj)
		       ($slots-push-rear! dst-slots (fun obj))
		       dst-slots)
    dst-slots src-slots))

;;; --------------------------------------------------------------------

(define* (slots-map-right {dst-slots slots?} {fun procedure?} {src-slots slots?})
  ($slots-map-right dst-slots fun src-slots))

(define ($slots-map-right dst-slots fun src-slots)
  ($slots-fold-right (lambda (obj dst-slots)
		      ($slots-push-front! dst-slots (fun obj))
		      dst-slots)
    dst-slots src-slots))

;;; --------------------------------------------------------------------

(define* (slots-for-each-left {fun procedure?} {slots slots?})
  ($slots-for-each-left fun slots))

(define ($slots-for-each-left fun slots)
  ($slots-fold-left (lambda (knil obj)
		      (fun obj)
		      knil)
    #f slots)
  (void))

;;; --------------------------------------------------------------------

(define* (slots-for-each-right {fun procedure?} {slots slots?})
  ($slots-for-each-right fun slots))

(define ($slots-for-each-right fun slots)
  ($slots-fold-right (lambda (obj knil)
		       (fun obj)
		       knil)
    #f slots)
  (void))


;;;; searching

(case-define* slots-find-left
  (({fun procedure?} {slots slots?})
   ($slots-find-left fun slots #f))
  (({fun procedure?} {slots slots?} not-found-rv)
   ($slots-find-left fun slots not-found-rv)))

(define ($slots-find-left fun slots not-found-rv)
  (returnable
    ($slots-fold-left (lambda ({_ <top>} knil obj)
			 (if (fun obj)
			     (return obj)
			   knil))
      not-found-rv slots)))

;;; --------------------------------------------------------------------

(case-define* slots-find-right
  (({fun procedure?} {slots slots?})
   ($slots-find-right fun slots #f))
  (({fun procedure?} {slots slots?} not-found-rv)
   ($slots-find-right fun slots not-found-rv)))

(define ($slots-find-right fun slots not-found-rv)
  (returnable
    ($slots-fold-right (lambda ({_ <top>} obj knil)
			(if (fun obj)
			    (return obj)
			  knil))
      not-found-rv slots)))

;;; --------------------------------------------------------------------

(define* (slots-for-all {fun procedure?} {slots slots?})
  ($slots-for-all fun slots))

(define ($slots-for-all fun slots)
  (returnable
    ($slots-fold-left (lambda ({_ <top>} knil obj)
			 (or (fun obj)
			     (return #f)))
      #t slots)))

;;; --------------------------------------------------------------------

(define* (slots-exists-left {fun procedure?} {slots slots?})
  ($slots-exists-left fun slots))

(define ($slots-exists-left fun slots)
  (call/cc
      (lambda (escape)
	($slots-fold-left (lambda ({_ <top>} knil obj)
			    (cond ((fun obj)
				   => escape)
				  (else knil)))
	  #f slots))))

;;; --------------------------------------------------------------------

(define* (slots-exists-right {fun procedure?} {slots slots?})
  ($slots-exists-right fun slots))

(define ($slots-exists-right fun slots)
  (call/cc
      (lambda (escape)
	($slots-fold-right (lambda ({_ <top>} obj knil)
			     (cond ((fun obj)
				    => escape)
				   (else knil)))
	  #f slots))))


;;;; filtering

(define* (slots-filter {dst-slots slots?} {pred procedure?} {src-slots slots?})
  ($slots-filter dst-slots pred src-slots))

(define ($slots-filter dst-slots pred src-slots)
  ($slots-fold-left (lambda (dst-slots obj)
		      (when (pred obj)
			($slots-push-rear! dst-slots obj))
			dst-slots)
    dst-slots src-slots))

;;; --------------------------------------------------------------------

(define* (slots-partition {matching-slots slots?} {not-matching-slots slots?} {pred procedure?} {src-slots slots?})
  ($slots-partition matching-slots not-matching-slots pred src-slots))

(define ($slots-partition matching-slots not-matching-slots pred src-slots)
  ($slots-fold-left (lambda (knil obj)
		      ($slots-push-rear! (if (pred obj)
					     (car knil)
					   (cdr knil))
					 obj)
		      knil)
    (cons matching-slots not-matching-slots) src-slots)
  (values matching-slots not-matching-slots))


;;;; conversion

(define* (slots->list {slots slots?})
  ($slots->list slots))

(define ($slots->list slots)
  ($slots-fold-right cons '() slots))

;;; --------------------------------------------------------------------

(define* (list->slots {ell list?})
  ($list->slots (make-slots) ell))

(define* ($list->slots dst-slots ell)
  (fold-left (lambda (slots item)
	       ($slots-push-rear! slots item)
	       slots)
    dst-slots ell))

;;; --------------------------------------------------------------------

(define* (slots->vector {slots slots?})
  ($slots->vector slots))

(define ($slots->vector slots)
  (receive-and-return (vec)
      (make-vector ($slots-size slots))
    ($slots-fold-left (lambda (knil item)
			 (let ((vec ($car knil))
			       (idx ($cdr knil)))
			   ($vector-set! vec idx item)
			   ($set-cdr! knil ($fxadd1 idx))
			   knil))
		       (cons vec 0)
		       slots)))

;;; --------------------------------------------------------------------

(define* (vector->slots {vec vector?})
  ($vector->slots (make-slots) vec))

(define ($vector->slots dst-slots vec)
  (vector-fold-left (lambda (slots item)
		      ($slots-push-rear! slots item)
		      slots)
    dst-slots vec))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'slots-fold-left		'scheme-indent-function 1)
;; eval: (put 'slots-fold-right		'scheme-indent-function 1)
;; eval: (put 'slots-map-left		'scheme-indent-function 1)
;; eval: (put 'slots-map-right		'scheme-indent-function 1)
;; eval: (put 'slots-for-each-left	'scheme-indent-function 1)
;; eval: (put 'slots-for-each-right	'scheme-indent-function 1)
;; eval: (put '$slots-fold-left		'scheme-indent-function 1)
;; eval: (put '$slots-fold-right	'scheme-indent-function 1)
;; eval: (put '$slots-map-left		'scheme-indent-function 1)
;; eval: (put '$slots-map-right		'scheme-indent-function 1)
;; eval: (put '$slots-for-each-left	'scheme-indent-function 1)
;; eval: (put '$slots-for-each-right	'scheme-indent-function 1)
;; eval: (put 'slots-for-all		'scheme-indent-function 1)
;; eval: (put 'slots-find-left		'scheme-indent-function 1)
;; eval: (put 'slots-find-right		'scheme-indent-function 1)
;; eval: (put 'slots-exists		'scheme-indent-function 1)
;; eval: (put 'slots-exists-left	'scheme-indent-function 1)
;; eval: (put 'slots-exists-right	'scheme-indent-function 1)
;; eval: (put '$slots-for-all		'scheme-indent-function 1)
;; eval: (put '$slots-find-left		'scheme-indent-function 1)
;; eval: (put '$slots-find-right	'scheme-indent-function 1)
;; eval: (put '$slots-exists		'scheme-indent-function 1)
;; eval: (put '$slots-exists-left	'scheme-indent-function 1)
;; eval: (put '$slots-exists-right	'scheme-indent-function 1)
;; End:
