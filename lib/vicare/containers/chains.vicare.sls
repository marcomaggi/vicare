;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: simple doubly-linked list containers
;;;Date: Thu Aug  6, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (vicare containers chains)
  (export
    <chain-link>
    make-chain-link			chain-link?
    chain				chain?

    chain-link-hash			$chain-link-hash
    chain-link-putprop			$chain-link-putprop
    chain-link-getprop			$chain-link-getprop
    chain-link-remprop			$chain-link-remprop
    chain-link-property-list		$chain-link-property-list

    chain-length			$chain-length
    chain-forwards-length		$chain-forwards-length
    chain-backwards-length		$chain-backwards-length

    chain-link-ref			$chain-link-ref
    chain-link-set!			$chain-link-set!

    chain-link-next			$chain-link-next
    chain-link-prev			$chain-link-prev
    chain-link-next-set!		$chain-link-next-set!
    chain-link-prev-set!		$chain-link-prev-set!

    chain-front				$chain-front
    chain-rear				$chain-rear
    chain-push-front!			$chain-push-front!
    chain-push-rear!			$chain-push-rear!
    chain-pop-front!			$chain-pop-front!
    chain-pop-rear!			$chain-pop-rear!
    chain-link-remove!			$chain-link-remove!
    chain-index-forwards		$chain-index-forwards
    chain-index-backwards		$chain-index-backwards

    chain-copy-forwards			$chain-copy-forwards
    chain-reverse-forwards		$chain-reverse-forwards
    chain-append-forwards		$chain-append-forwards

    chain-fold-left-forwards		$chain-fold-left-forwards
    chain-fold-right-forwards		$chain-fold-right-forwards
    chain-fold-left-backwards		$chain-fold-left-backwards
    chain-fold-right-backwards		$chain-fold-right-backwards

    chain-map-forwards			$chain-map-forwards
    chain-for-each-forwards		$chain-for-each-forwards
    chain-for-all-forwards		$chain-for-all-forwards
    chain-exists-forwards		$chain-exists-forwards
    chain-find-forwards			$chain-find-forwards
    chain-filter-forwards		$chain-filter-forwards
    chain-partition-forwards		$chain-partition-forwards

    chain->list				$chain->list
    list->chain				$list->chain
    chain->vector			$chain->vector
    vector->chain			$vector->chain

    make-chain-forwards-iteration-thunk
    make-chain-backwards-iteration-thunk)
  (import (vicare)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $vectors))


;;;; data structure

(define-record-type (<chain-link> make-chain-link chain-link?)
  (nongenerative vicare:containers:<chain-link>)
  (protocol
   (lambda (make-record)
     (lambda (obj)
       (make-record #f '() '() obj))))
  (fields (mutable uid)
	  (mutable prev)
		;Null or a  reference to the previous link in  the chain.  References
		;the link in the backwards direction.
	  (mutable next)
		;Null or a  reference to the next link in  the chain.  References the
		;link in the forwards direction.
	  (mutable object)
		;The payload object.
	  ))

;;; --------------------------------------------------------------------

(define-alias  chain-link-uid	 <chain-link>-uid)
(define-alias $chain-link-uid	$<chain-link>-uid)

(define-alias  chain-link-next	 <chain-link>-next)
(define-alias $chain-link-next	$<chain-link>-next)

(define-alias  chain-link-prev	 <chain-link>-prev)
(define-alias $chain-link-prev	$<chain-link>-prev)

(define-alias  chain-link-ref	 <chain-link>-object)
(define-alias $chain-link-ref	$<chain-link>-object)

(define-alias  chain-link-set!	 <chain-link>-object-set!)
(define-alias $chain-link-set!	$<chain-link>-object-set!)

;;; --------------------------------------------------------------------

(define-alias  chain-link-uid-set!	 <chain-link>-uid-set!)
(define-alias $chain-link-uid-set!	$<chain-link>-uid-set!)

;;; --------------------------------------------------------------------

(define* (chain-link-next-set! {C chain-link?} {N chain?})
  ($chain-link-next-set! C N))

(define ($chain-link-next-set! C N)
  ($<chain-link>-next-set! C N)
  (unless (null? N)
    ($<chain-link>-prev-set! N C))
  N)

;;; --------------------------------------------------------------------

(define* (chain-link-prev-set! {C chain-link?} {P chain?})
  ($chain-link-prev-set! C P))

(define ($chain-link-prev-set! C P)
  ($<chain-link>-prev-set! C P)
  (unless (null? P)
    ($<chain-link>-next-set! P C))
  P)


;;;; UID stuff

(define* (chain-link-hash {S chain-link?})
  ($chain-link-hash S))

(define ($chain-link-hash S)
  (unless ($chain-link-uid S)
    ($chain-link-uid-set! S (gensym)))
  (symbol-hash ($chain-link-uid S)))

;;; --------------------------------------------------------------------

(define* (chain-link-putprop {S chain-link?} {key symbol?} value)
  ($chain-link-putprop S key value))

(define ($chain-link-putprop S key value)
  (unless ($chain-link-uid S)
    ($chain-link-uid-set! S (gensym)))
  (putprop ($chain-link-uid S) key value))

;;; --------------------------------------------------------------------

(define* (chain-link-getprop {S chain-link?} {key symbol?})
  ($chain-link-getprop S key))

(define ($chain-link-getprop S key)
  (unless ($chain-link-uid S)
    ($chain-link-uid-set! S (gensym)))
  (getprop ($chain-link-uid S) key))

;;; --------------------------------------------------------------------

(define* (chain-link-remprop {S chain-link?} {key symbol?})
  ($chain-link-remprop S key))

(define ($chain-link-remprop S key)
  (unless ($chain-link-uid S)
    ($chain-link-uid-set! S (gensym)))
  (remprop ($chain-link-uid S) key))

;;; --------------------------------------------------------------------

(define* (chain-link-property-list {S chain-link?})
  ($chain-link-property-list S))

(define ($chain-link-property-list S)
  (unless ($chain-link-uid S)
    ($chain-link-uid-set! S (gensym)))
  (property-list ($chain-link-uid S)))


;;;; inspection

(define* (chain-length {S chain?})
  ($chain-length S))

(define ($chain-length C)
  ;;Return a positive exact integer representing the  number of links in the chain of
  ;;which C is a link.  In counting the links: proceed both in forwards and backwards
  ;;directions.  Circular chains are forbidden.
  ;;
  (if (null? C)
      0
    (+ ($chain-forwards-length  C)
       ($chain-backwards-length C)
       -1)))

;;; --------------------------------------------------------------------

(define* (chain-forwards-length  {C chain?})
  ;;Return a  positive exact integer  representing the number  of links in  the chain
  ;;starting from C in the forwards direction.  Circular chains are forbidden.
  ;;
  ($chain-forwards-length C))

(define ($chain-forwards-length C)
  (if (null? C)
      0
    (let next-link ((C C)
		    (N 1))
      (let ((next ($chain-link-next C)))
	(if (null? next)
	    N
	  (next-link next (add1 N)))))))

;;; --------------------------------------------------------------------

(define* (chain-backwards-length  {C chain?})
  ;;Return a  positive exact integer  representing the number  of links in  the chain
  ;;starting from C in the backwards direction.  Circular chains are forbidden.
  ;;
  ($chain-backwards-length C))

(define ($chain-backwards-length C)
  (if (null? C)
      0
    (let prev-link ((C C)
		    (N 1))
      (let ((prev ($chain-link-prev C)))
	(if (null? prev)
	    N
	  (prev-link prev (add1 N)))))))


;;;; accessors and mutators

(define* (chain-front {C chain?})
  ($chain-front C))

(define ($chain-front C)
  ;;Return the first link in the chain of which C is a link.  Return C itself if C is
  ;;null or the first link.  Circular chains are forbidden.
  ;;
  (if (null? C)
      '()
    (let ((K ($chain-link-prev C)))
      (if (null? K)
	  C
	($chain-front K)))))

;;; --------------------------------------------------------------------

(define* (chain-rear {C chain?})
  ($chain-rear C))

(define ($chain-rear C)
  ;;Return the last link in the chain of which  C is a link.  Return C itself if C is
  ;;null or the last link.  Circular chains are forbidden.
  ;;
  (if (null? C)
      '()
    (let ((K ($chain-link-next C)))
      (if (null? K)
	  C
	($chain-rear K)))))

;;; --------------------------------------------------------------------

(define* (chain-push-front! {C chain?} {new-front-link chain-link?})
  ($chain-push-front! C new-front-link))

(define* ($chain-push-front! C new-front-link)
  (unless (null? C)
    ($chain-link-prev-set! ($chain-front C) new-front-link))
  new-front-link)

;;; --------------------------------------------------------------------

(define* (chain-push-rear! {C chain?} {new-rear-link chain-link?})
  ($chain-push-rear! C new-rear-link))

(define ($chain-push-rear! C new-rear-link)
  (unless (null? C)
    ($chain-link-next-set! ($chain-rear C) new-rear-link))
  new-rear-link)

;;; --------------------------------------------------------------------

(define* (chain-pop-front! {C chain?})
  ($chain-pop-front! C))

(define* ($chain-pop-front! C)
  (if (null? C)
      (assertion-violation __who__ "chain is empty" C)
    (let* ((old-front ($chain-front C))
	   (new-front ($chain-link-next old-front)))
      (unless (null? new-front)
	($<chain-link>-next-set! old-front '())
	($<chain-link>-prev-set! new-front '()))
      (values old-front new-front))))

;;; --------------------------------------------------------------------

(define* (chain-pop-rear! {C chain?})
  ($chain-pop-rear! C))

(define* ($chain-pop-rear! C)
  (if (null? C)
      (assertion-violation __who__ "chain is empty" C)
    (let* ((old-rear ($chain-rear C))
	   (new-rear ($chain-link-prev old-rear)))
      (unless (null? new-rear)
	($<chain-link>-prev-set! old-rear '())
	($<chain-link>-next-set! new-rear '()))
      (values old-rear new-rear))))

;;; --------------------------------------------------------------------

(define* (chain-link-remove! {C chain-link?})
  ($chain-link-remove! C))

(define ($chain-link-remove! link)
  (let* ((prev   ($chain-link-prev link))
	 (next   ($chain-link-next link))
	 (prev?  (not (null? prev)))
	 (next?  (not (null? next))))
    (if prev?
	(begin
	  ($<chain-link>-prev-set! link '())
	  (if next?
	      (begin
		($<chain-link>-next-set! link '())
		($<chain-link>-next-set! prev next)
		($<chain-link>-prev-set! next prev))
	    ($<chain-link>-next-set! prev '())))
      (when next?
	($<chain-link>-next-set! link '())
	($<chain-link>-prev-set! next '()))))
  link)

;;; --------------------------------------------------------------------

(define* (chain-index-forwards {C chain?} {I non-negative-exact-integer?})
  ($chain-index-forwards C I))

(define* ($chain-index-forwards C I)
  (let next-link ((link C)
		  (idx  I))
    (cond ((null? link)
	   (assertion-violation __who__ "index out of range for chain" C I))
	  ((zero? idx)
	   ($chain-link-ref link))
	  (else
	   (next-link ($chain-link-next link) (sub1 idx))))))

;;; --------------------------------------------------------------------

(define* (chain-index-backwards {C chain?} {I non-negative-exact-integer?})
  ($chain-index-backwards C I))

(define* ($chain-index-backwards C I)
  (let next-link ((link C)
		  (idx  I))
    (cond ((null? link)
	   (assertion-violation __who__ "index out of range for chain" C I))
	  ((zero? idx)
	   ($chain-link-ref link))
	  (else
	   (next-link ($chain-link-prev link) (sub1 idx))))))


;;;; copy, reverse, append

(define* (chain-copy-forwards {C chain?})
  ($chain-copy-forwards C))

(define ($chain-copy-forwards C)
  (let recur ((new-prev '())
	      (link     C))
    (if (null? link)
	'()
      (receive-and-return (new-link)
	  (make-chain-link ($chain-link-ref link))
	($chain-link-prev-set! new-link new-prev)
	($chain-link-next-set! new-link (recur new-link ($chain-link-next link)))))))

;;; --------------------------------------------------------------------

(define* (chain-reverse-forwards {C chain?})
  ($chain-reverse-forwards C))

(define ($chain-reverse-forwards C)
  (let recur ((new-next '())
	      (link     C))
    (if (null? link)
	'()
      (receive-and-return (new-link)
	  (make-chain-link ($chain-link-ref link))
	($chain-link-next-set! new-link new-next)
	($chain-link-prev-set! new-link (recur new-link ($chain-link-next link)))))))

;;; --------------------------------------------------------------------

(case-define* chain-append-forwards
  (()
   '())

  (({C chain?})
   ($chain-copy-forwards C))

  (({C1 chain?} {C2 chain?})
   ($chain-append-forwards C1 C2))

  (({C1 chain?} {C2 chain?} . {C* chain?})
   ($chain-append-forwards/two-and-more C1 C2 C*))

  #| end of CASE-DEFINE* |# )

(case-define* $chain-append-forwards
  (()
   '())

  ((C)
   ($chain-copy-forwards C))

  ((C1 C2)
   (let outer-recur ((outer-new-prev '())
		     (outer-link     C1))
     (if (null? outer-link)
	 (let recur ((inner-new-prev outer-new-prev)
		     (inner-link     C2))
	   (if (null? inner-link)
	       '()
	     (receive-and-return (new-link)
		 (make-chain-link ($chain-link-ref inner-link))
	       ($chain-link-prev-set! new-link inner-new-prev)
	       ($chain-link-next-set! new-link (recur new-link ($chain-link-next inner-link))))))
       (receive-and-return (new-link)
	   (make-chain-link ($chain-link-ref outer-link))
	 ($chain-link-prev-set! new-link outer-new-prev)
	 ($chain-link-next-set! new-link (outer-recur new-link ($chain-link-next outer-link)))))))

  ((C1 C2 . C*)
   ($chain-append-forwards/two-and-more C1 C2 C*))

  #| end of CASE-DEFINE* |# )

(define ($chain-append-forwards/two-and-more C1 C2 C*)
  (let recur ((new-prev '())
	      (link     C1)
	      (link*    (cons C2 C*)))
    (if (null? link)
	(if (null? link*)
	    '()
	  (recur new-prev (car link*) (cdr link*)))
      (receive-and-return (new-link)
	  (make-chain-link ($chain-link-ref link))
	($chain-link-prev-set! new-link new-prev)
	($chain-link-next-set! new-link (recur new-link ($chain-link-next link) link*))))))


;;;; basic list operations: folding

(define* (chain-fold-left-forwards {kons procedure?} knil {C chain?})
  ($chain-fold-left-forwards kons knil C))

(define ($chain-fold-left-forwards kons knil C)
  (if (null? C)
      knil
    (let ((next-link ($chain-link-next C)))
      (if (null? next-link)
	  (kons knil ($chain-link-ref C))
	($chain-fold-left-forwards kons
				  (kons knil ($chain-link-ref C))
				  next-link)))))

;;; --------------------------------------------------------------------

(define* (chain-fold-right-forwards {kons procedure?} knil {C chain?})
  ($chain-fold-right-forwards kons knil C))

(define ($chain-fold-right-forwards kons knil C)
  (if (null? C)
      knil
    (let ((next-link ($chain-link-next C)))
      (if (null? next-link)
	  (kons ($chain-link-ref C) knil)
	(kons ($chain-link-ref C)
	      ($chain-fold-right-forwards kons knil next-link))))))

;;; --------------------------------------------------------------------

(define* (chain-fold-left-backwards {kons procedure?} knil {C chain?})
  ($chain-fold-left-backwards kons knil C))

(define ($chain-fold-left-backwards kons knil C)
  (if (null? C)
      knil
    (let ((prev-link ($chain-link-prev C)))
      (if (null? prev-link)
	  (kons knil ($chain-link-ref C))
	($chain-fold-left-backwards kons
				  (kons knil ($chain-link-ref C))
				  prev-link)))))

;;; --------------------------------------------------------------------

(define* (chain-fold-right-backwards {kons procedure?} knil {C chain?})
  ($chain-fold-right-backwards kons knil C))

(define ($chain-fold-right-backwards kons knil C)
  (if (null? C)
      knil
    (let ((prev-link ($chain-link-prev C)))
      (if (null? prev-link)
	  (kons ($chain-link-ref C) knil)
	(kons ($chain-link-ref C)
	      ($chain-fold-right-backwards kons knil prev-link))))))


;;;; basic list operations: mapping

(module (chain-map-forwards $chain-map-forwards)

  (case-define* chain-map-forwards

    (({fun procedure?} {C chain?})
     ($chain-map-forwards fun C))

    (({fun procedure?} {C1 chain?} {C2 chain?})
     ($chain-map-forwards fun C1 C2))

    (({fun procedure?} {C1 chain?} {C2 chain?} . {C* chain?})
     ($chain-map-forwards/two-and-list fun C1 C2 C*))

    #| end of CASE-DEFINE* |# )

  (case-define $chain-map-forwards
    ((fun link)
     (let recur ((new-prev  '())
		 (old-link  link))
       (if (null? old-link)
	   '()
	 (receive-and-return (new-link)
	     (make-chain-link (fun ($chain-link-ref old-link)))
	   ($chain-link-prev-set! new-link new-prev)
	   ($chain-link-next-set! new-link (recur new-link ($chain-link-next old-link)))))))

    ((fun link1 link2)
     (let recur ((new-prev   '())
		 (old-link1  link1)
		 (old-link2  link2))
       (cond ((null? old-link1)
	      '())
	     ((null? old-link2)
	      '())
	     (else
	      (receive-and-return (new-link)
		  (make-chain-link (fun ($chain-link-ref old-link1)
					($chain-link-ref old-link2)))
		($chain-link-prev-set! new-link new-prev)
		($chain-link-next-set! new-link (recur new-link
						       ($chain-link-next old-link1)
						       ($chain-link-next old-link2))))))))

    ((fun link1 link2 . link*)
     ($chain-map-forwards/two-and-list fun link1 link2 link*))

    #| end of CASE-DEFINE* |# )

  (define ($chain-map-forwards/two-and-list fun link1 link2 link*)
    (let recur ((new-prev   '())
		(old-link1  link1)
		(old-link2  link2)
		(old-link*  link*))
      (cond ((null? old-link1)
	     '())
	    ((null? old-link2)
	     '())
	    ((find null? old-link*)
	     '())
	    (else
	     (receive-and-return (new-link)
		 (make-chain-link (apply fun
					 ($chain-link-ref old-link1)
					 ($chain-link-ref old-link2)
					 (map (lambda (lnk)
						($chain-link-ref lnk))
					   old-link*)))
	       ($<chain-link>-prev-set! new-link new-prev)
	       ($<chain-link>-next-set! new-link (recur new-link
							($chain-link-next old-link1)
							($chain-link-next old-link2)
							(map (lambda (lnk)
							       ($chain-link-next lnk))
							  old-link*))))))))

  #| end of module |# )


;;;; basic list operations: for-each

(module (chain-for-each-forwards $chain-for-each-forwards)

  (case-define* chain-for-each-forwards

    (({fun procedure?} {C chain?})
     ($chain-for-each-forwards fun C))

    (({fun procedure?} {C1 chain?} {C2 chain?})
     ($chain-for-each-forwards fun C1 C2))

    (({fun procedure?} {C1 chain?} {C2 chain?} . {C* chain?})
     ($chain-for-each-forwards/two-and-list fun C1 C2 C*))

    #| end of CASE-DEFINE* |# )

  (case-define $chain-for-each-forwards
    ((fun link)
     (if (null? link)
	 (void)
       (let loop ((link link))
	 (let ((next ($chain-link-next link)))
	   (if (null? next)
	       ;;Last application in tail position.
	       (fun ($chain-link-ref link))
	     (begin
	       (fun ($chain-link-ref link))
	       (loop next)))))))

    ((fun link1 link2)
     (if (or (null? link1)
	     (null? link2))
	 (void)
       (let loop ((link1  link1)
		  (link2  link2))
	 (let ((next1 ($chain-link-next link1))
	       (next2 ($chain-link-next link2)))
	   (if (or (null? next1)
		   (null? next2))
	       ;;Last application in tail position.
	       (fun ($chain-link-ref link1)
		    ($chain-link-ref link2))
	     (begin
	       (fun ($chain-link-ref link1)
		    ($chain-link-ref link2))
	       (loop next1 next2)))))))

    ((fun link1 link2 . link*)
     ($chain-for-each-forwards/two-and-list fun link1 link2 link*))

    #| end of CASE-DEFINE* |# )

  (define ($chain-for-each-forwards/two-and-list fun link1 link2 link*)
    (if (or (null? link1)
	    (null? link2)
	    (find null? link*))
	(void)
      (let loop ((link1  link1)
		 (link2  link2)
		 (link*  link*))
	(let ((next1  ($chain-link-next link1))
	      (next2  ($chain-link-next link2))
	      (next*  (map (lambda (lnk)
			     ($chain-link-next lnk))
			link*)))
	  (if (or (null? next1)
		  (null? next2)
		  (find null? next*))
	      ;;Last application in tail position.
	      (apply fun
		     ($chain-link-ref link1)
		     ($chain-link-ref link2)
		     (map (lambda (lnk)
			    ($chain-link-ref lnk))
		       link*))
	    (begin
	      (apply fun
		     ($chain-link-ref link1)
		     ($chain-link-ref link2)
		     (map (lambda (lnk)
			    ($chain-link-ref lnk))
		       link*))
	      (loop next1 next2 next*)))))))

  #| end of module |# )


;;;; basic list operations: for-all

(module (chain-for-all-forwards $chain-for-all-forwards)

  (case-define* chain-for-all-forwards

    (({fun procedure?} {C chain?})
     ($chain-for-all-forwards fun C))

    (({fun procedure?} {C1 chain?} {C2 chain?})
     ($chain-for-all-forwards fun C1 C2))

    (({fun procedure?} {C1 chain?} {C2 chain?} . {C* chain?})
     ($chain-for-all-forwards/two-and-list fun C1 C2 C*))

    #| end of CASE-DEFINE* |# )

  (case-define $chain-for-all-forwards
    ((fun link)
     (if (null? link)
	 #t
       (let loop ((link link))
	 (let ((next ($chain-link-next link)))
	   (if (null? next)
	       ;;Last application in tail position.
	       (fun ($chain-link-ref link))
	     (and (fun ($chain-link-ref link))
		  (loop next)))))))

    ((fun link1 link2)
     (if (or (null? link1)
	     (null? link2))
	 #t
       (let loop ((link1  link1)
		  (link2  link2))
	 (let ((next1 ($chain-link-next link1))
	       (next2 ($chain-link-next link2)))
	   (if (or (null? next1)
		   (null? next2))
	       ;;Last application in tail position.
	       (fun ($chain-link-ref link1)
		    ($chain-link-ref link2))
	     (and (fun ($chain-link-ref link1)
		       ($chain-link-ref link2))
		  (loop next1 next2)))))))

    ((fun link1 link2 . link*)
     ($chain-for-all-forwards/two-and-list fun link1 link2 link*))

    #| end of CASE-DEFINE* |# )

  (define ($chain-for-all-forwards/two-and-list fun link1 link2 link*)
    (if (or (null? link1)
	    (null? link2)
	    (find null? link*))
	#t
      (let loop ((link1  link1)
		 (link2  link2)
		 (link*  link*))
	(let ((next1  ($chain-link-next link1))
	      (next2  ($chain-link-next link2))
	      (next*  (map (lambda (lnk)
			     ($chain-link-next lnk))
			link*)))
	  (if (or (null? next1)
		  (null? next2)
		  (find null? next*))
	      ;;Last application in tail position.
	      (apply fun
		     ($chain-link-ref link1)
		     ($chain-link-ref link2)
		     (map (lambda (lnk)
			    ($chain-link-ref lnk))
		       link*))
	    (and (apply fun
			($chain-link-ref link1)
			($chain-link-ref link2)
			(map (lambda (lnk)
			       ($chain-link-ref lnk))
			  link*))
		 (loop next1 next2 next*)))))))

  #| end of module |# )


;;;; basic list operations: exists

(module (chain-exists-forwards $chain-exists-forwards)

  (case-define* chain-exists-forwards

    (({fun procedure?} {C chain?})
     ($chain-exists-forwards fun C))

    (({fun procedure?} {C1 chain?} {C2 chain?})
     ($chain-exists-forwards fun C1 C2))

    (({fun procedure?} {C1 chain?} {C2 chain?} . {C* chain?})
     ($chain-exists-forwards/two-and-list fun C1 C2 C*))

    #| end of CASE-DEFINE* |# )

  (case-define $chain-exists-forwards
    ((fun link)
     (if (null? link)
	 #f
       (let loop ((link link))
	 (let ((next ($chain-link-next link)))
	   (if (null? next)
	       ;;Last application in tail position.
	       (fun ($chain-link-ref link))
	     (or (fun ($chain-link-ref link))
		 (loop next)))))))

    ((fun link1 link2)
     (if (or (null? link1)
	     (null? link2))
	 #f
       (let loop ((link1  link1)
		  (link2  link2))
	 (let ((next1 ($chain-link-next link1))
	       (next2 ($chain-link-next link2)))
	   (if (or (null? next1)
		   (null? next2))
	       ;;Last application in tail position.
	       (fun ($chain-link-ref link1)
		    ($chain-link-ref link2))
	     (or (fun ($chain-link-ref link1)
		      ($chain-link-ref link2))
		 (loop next1 next2)))))))

    ((fun link1 link2 . link*)
     ($chain-exists-forwards/two-and-list fun link1 link2 link*))

    #| end of CASE-DEFINE* |# )

  (define ($chain-exists-forwards/two-and-list fun link1 link2 link*)
    (if (or (null? link1)
	    (null? link2)
	    (find null? link*))
	#f
      (let loop ((link1  link1)
		 (link2  link2)
		 (link*  link*))
	(let ((next1  ($chain-link-next link1))
	      (next2  ($chain-link-next link2))
	      (next*  (map (lambda (lnk)
			     ($chain-link-next lnk))
			link*)))
	  (if (or (null? next1)
		  (null? next2)
		  (find null? next*))
	      ;;Last application in tail position.
	      (apply fun
		     ($chain-link-ref link1)
		     ($chain-link-ref link2)
		     (map (lambda (lnk)
			    ($chain-link-ref lnk))
		       link*))
	    (or (apply fun
		       ($chain-link-ref link1)
		       ($chain-link-ref link2)
		       (map (lambda (lnk)
			      ($chain-link-ref lnk))
			 link*))
		(loop next1 next2 next*)))))))

  #| end of module |# )


;;;; basic list operations: find

(case-define* chain-find-forwards
  (({fun procedure?} {C chain?})
   ($chain-find-forwards fun C #f))
  (({fun procedure?} {C chain?} not-found-return-value)
   ($chain-find-forwards fun C  not-found-return-value))
  #| end of CASE-DEFINE* |# )

(case-define $chain-find-forwards
  ((fun chain)
   ($chain-find-forwards fun chain #f))

  ((fun chain not-found-return-value)
   (let loop ((link chain))
     (if (null? link)
	 not-found-return-value
       (let ((obj ($chain-link-ref link)))
	 (if (fun obj)
	     obj
	   (loop ($chain-link-next link)))))))

  #| end of CASE-DEFINE |# )


;;;; basic list operations: filter and partition

(define* (chain-filter-forwards {fun procedure?} {C chain?})
  ($chain-filter-forwards fun C))

(define ($chain-filter-forwards fun link)
  (let recur ((new-prev  '())
	      (old-link  link))
    (if (null? old-link)
	'()
      (let ((obj ($chain-link-ref old-link)))
	(if (fun obj)
	    (receive-and-return (new-link)
		(make-chain-link obj)
	      ($chain-link-prev-set! new-link new-prev)
	      ($chain-link-next-set! new-link (recur new-link ($chain-link-next old-link))))
	  (recur new-prev ($chain-link-next old-link)))))))

;;; --------------------------------------------------------------------

(define* (chain-partition-forwards {fun procedure?} {C chain?})
  ($chain-partition-forwards fun C))

(define ($chain-partition-forwards fun link)
  (let recur ((new-in-prev  '())
	      (new-ou-prev  '())
	      (old-link     link))
    (if (null? old-link)
	(values '() '())
      (let ((obj ($chain-link-ref old-link)))
	(if (fun obj)
	    (let ((new-in-link (make-chain-link obj)))
	      ($chain-link-prev-set! new-in-link new-in-prev)
	      (receive (in-next ou-next)
		  (recur new-in-link new-ou-prev ($chain-link-next old-link))
		($chain-link-next-set! new-in-link in-next)
		(values new-in-link ou-next)))
	  (let ((new-ou-link (make-chain-link obj)))
	    ($chain-link-prev-set! new-ou-link new-ou-prev)
	    (receive (in-next ou-next)
		(recur new-in-prev new-ou-link ($chain-link-next old-link))
	      ($chain-link-next-set! new-ou-link ou-next)
	      (values in-next new-ou-link))))))))


;;;; conversion

(define (chain? obj)
  (or (chain-link? obj)
      (null?       obj)))

;; (define (chain-first? obj)
;;   (or (null? obj)
;;       (and (chain-link? obj)
;; 	   (null? ($chain-link-prev obj)))))

(define (chain . item*)
  ($list->chain item*))

;;; --------------------------------------------------------------------

(define* (chain->list {C chain?})
  ($chain->list C))

(define* ($chain->list C)
  (if (null? C)
      '()
    (let* ((first-link ($chain-front C))
	   (first-pair (cons ($chain-link-ref first-link) '())))
      (let loop ((pair  first-pair)
		 (link  ($chain-link-next first-link)))
	(if (null? link)
	    first-pair
	  (let ((next-pair (cons ($chain-link-ref link) '())))
	    ($set-cdr! pair next-pair)
	    (loop next-pair ($chain-link-next link))))))))

;;; --------------------------------------------------------------------

(define* (list->chain {item* list?})
  ($list->chain item*))

(define ($list->chain item*)
  ;;Build and  return a  new chain  containing the  items in  the proper  list ITEM*.
  ;;Return null if ITEM* is empty.
  ;;
  (fold-right (lambda (item next-link)
		(receive-and-return (this-link)
		    (make-chain-link item)
		  ($chain-link-next-set! this-link next-link)))
    '()
    item*))

;;; --------------------------------------------------------------------

(define* (chain->vector {C chain?})
  ($chain->vector C))

(define* ($chain->vector C)
  (if (null? C)
      '#()
    (let ((first-link ($chain-front C)))
      (let loop ((vec   (make-vector ($chain-forwards-length first-link)))
		 (i     0)
		 (link  first-link))
	(if (null? link)
	    vec
	  (begin
	    ($vector-set! vec i ($chain-link-ref link))
	    (loop vec ($fxadd1 i) ($chain-link-next link))))))))

;;; --------------------------------------------------------------------

(define* (vector->chain {V vector?})
  ($vector->chain V))

(define* ($vector->chain V)
  ;;Build and return a  new chain containing the items in the  vector V.  Return null
  ;;if V is empty.
  ;;
  (vector-fold-right (lambda (item next-link)
		       (receive-and-return (this-link)
			   (make-chain-link item)
			 ($chain-link-next-set! this-link next-link)))
    '() V))


;;;; iteration thunks

(define* (make-chain-forwards-iteration-thunk {C chain?})
  (lambda ()
    (if (null? C)
	(void)
      (receive-and-return (obj)
	  ($<chain-link>-object C)
	(set! C ($chain-link-next C))))))

(define* (make-chain-backwards-iteration-thunk {C chain?})
  (lambda ()
    (if (null? C)
	(void)
      (receive-and-return (obj)
	  ($<chain-link>-object C)
	(set! C ($chain-link-prev C))))))


;;;; done

#| end of library |# )

;;; end of file
