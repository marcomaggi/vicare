;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: simple double-ended queue containers
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
(library (vicare containers deques)
  (export
    <deque>			make-deque
    deque			deque?

    deque-hash			$deque-hash
    deque-putprop		$deque-putprop
    deque-getprop		$deque-getprop
    deque-remprop		$deque-remprop
    deque-property-list		$deque-property-list


    deque-empty?		$deque-empty?
    deque-not-empty?		$deque-not-empty?
    deque-size			$deque-size

    deque-front			$deque-front
    deque-rear			$deque-rear
    deque-push-front!		$deque-push-front!
    deque-push-rear!		$deque-push-rear!
    deque-pop-front!		$deque-pop-front!
    deque-pop-rear!		$deque-pop-rear!
    deque-purge!		$deque-purge!

    deque->list			list->deque
    deque->vector		vector->deque)
  (import (vicare)
    (vicare system $fx)
    (vicare system $pairs))


;;; helpers

(define-inline (list-copy/stx ?ell)
  (let recur ((ell ?ell))
    (if (pair? ell)
	(cons ($car ell) (recur ($cdr ell)))
      ell)))

(define-inline (last-pair/stx ?x)
  ;;*WARNING* Do  not rename LAST-PAIR/STX to LAST-PAIR,  it would clash
  ;;with the LAST-PAIR field of <deque> records.
  ;;
  (let ((x ?x))
    (if (null? x)
	#f
      (let loop ((x x))
	(if (pair? ($cdr x))
	    (loop ($cdr x))
	  x)))))


;;;; data structure

(define-record-type (<deque> make-deque deque?)
  (nongenerative vicare:containers:<deque>)
  (protocol
   (lambda (make-record)
     (lambda ()
       (make-record #f '() #f))))
  (fields (mutable uid)
	  (mutable first-pair)
		;First node in the doubly-linked list.
	  (mutable last-pair)
		;Last node in the doubly-linked list.
	  ))

;;; --------------------------------------------------------------------

(define-alias  deque-uid		 <deque>-uid)
(define-alias $deque-uid		$<deque>-uid)

(define-alias  deque-first-pair		 <deque>-first-pair)
(define-alias $deque-first-pair		$<deque>-first-pair)

(define-alias  deque-last-pair		 <deque>-last-pair)
(define-alias $deque-last-pair		$<deque>-last-pair)

(define-alias  deque-uid-set!		 <deque>-uid-set!)
(define-alias $deque-uid-set!		$<deque>-uid-set!)

(define-alias  deque-first-pair-set!	 <deque>-first-pair-set!)
(define-alias $deque-first-pair-set!	$<deque>-first-pair-set!)

(define-alias  deque-last-pair-set!	 <deque>-last-pair-set!)
(define-alias $deque-last-pair-set!	$<deque>-last-pair-set!)

;;; --------------------------------------------------------------------

(define (deque . item*)
  (receive-and-return (D)
      (make-deque)
    (for-each (lambda (obj)
		($deque-push-rear! D obj))
      item*)))


;;;; UID stuff

(define* (deque-hash {Q deque?})
  ($deque-hash Q))

(define ($deque-hash Q)
  (unless ($deque-uid Q)
    ($deque-uid-set! Q (gensym)))
  (symbol-hash ($deque-uid Q)))

;;; --------------------------------------------------------------------

(define* (deque-putprop {Q deque?} {key symbol?} value)
  ($deque-putprop Q key value))

(define ($deque-putprop Q key value)
  (unless ($deque-uid Q)
    ($deque-uid-set! Q (gensym)))
  (putprop ($deque-uid Q) key value))

;;; --------------------------------------------------------------------

(define* (deque-getprop {Q deque?} {key symbol?})
  ($deque-getprop Q key))

(define ($deque-getprop Q key)
  (unless ($deque-uid Q)
    ($deque-uid-set! Q (gensym)))
  (getprop ($deque-uid Q) key))

;;; --------------------------------------------------------------------

(define* (deque-remprop {Q deque?} {key symbol?})
  ($deque-remprop Q key))

(define ($deque-remprop Q key)
  (unless ($deque-uid Q)
    ($deque-uid-set! Q (gensym)))
  (remprop ($deque-uid Q) key))

;;; --------------------------------------------------------------------

(define* (deque-property-list {Q deque?})
  ($deque-property-list Q))

(define ($deque-property-list Q)
  (unless ($deque-uid Q)
    ($deque-uid-set! Q (gensym)))
  (property-list ($deque-uid Q)))


;;;; inspection

(define* (deque-empty? {Q deque?})
  ($deque-empty? Q))

(define ($deque-empty? Q)
  ($fxzero? ($deque-size Q)))

;;; --------------------------------------------------------------------

(define* (deque-not-empty? {Q deque?})
  ($deque-not-empty? Q))

(define ($deque-not-empty? Q)
  ($fxpositive? ($deque-size Q)))

;;; --------------------------------------------------------------------

(define* (deque-size {Q deque?})
  ($deque-size Q))

(define ($deque-size Q)
  (length ($deque-first-pair Q)))


;;;; accessors and mutators

(define* (deque-front {Q deque?})
  ($deque-front Q))

(define* ($deque-front Q)
  (let ((first-pair ($deque-first-pair Q)))
    (if (pair? first-pair)
	(car first-pair)
      (assertion-violation __who__ "deque is empty" Q))))

;;; --------------------------------------------------------------------

(define* (deque-rear {Q deque?})
  ($deque-rear Q))

(define* ($deque-rear Q)
  (let ((last-pair ($deque-last-pair Q)))
    (if (pair? last-pair)
	(car last-pair)
      (assertion-violation __who__ "deque is empty" Q))))

;;; --------------------------------------------------------------------

(define* (deque-push-front! {S deque?} obj)
  ($deque-push-front! S obj))

(define ($deque-push-front! S obj)
  ($deque-first-pair-set! S (cons obj ($deque-first-pair S))))

;;; --------------------------------------------------------------------

(define* (deque-push-rear! {Q deque?} obj)
  ($deque-push-rear! Q obj))

(define ($deque-push-rear! Q obj)
  (let ((new-last-pair (list obj)))
    (if (pair? ($deque-first-pair Q))
	;;The deque  is not empty: append  the new last-pair to  the old
	;;last-pair.
	($set-cdr! ($deque-last-pair Q) new-last-pair)
      ;;The  deque  is  empty:  the   new  last-pair  is  also  the  new
      ;;first-pair, so store it the first-pair slot.
      ($deque-first-pair-set! Q new-last-pair))
    ;;Store the new last-pair in the last-pair slot.
    ($deque-last-pair-set! Q new-last-pair)))

;;; --------------------------------------------------------------------

(define* (deque-pop-front! {Q deque?})
  ($deque-pop-front! Q))

(define* ($deque-pop-front! Q)
  (let ((old-first-pair ($deque-first-pair Q)))
    (if (pair? old-first-pair)
	(begin
	  ;;The new first-pair is the  next of the old first-pair, which
	  ;;can be null.
	  ($deque-first-pair-set! Q ($cdr old-first-pair))
	  ;;If the old  first-pair is also the old  last-pair: reset the
	  ;;last-pair so that the deque results as empty.
	  (when (eq? ($deque-last-pair Q) old-first-pair)
	    ($deque-last-pair-set! Q #f)))
      (error __who__ "deque is empty" Q))
    ($car old-first-pair)))

;;; --------------------------------------------------------------------

(define* (deque-pop-rear! {Q deque?})
  ($deque-pop-rear! Q))

(define* ($deque-pop-rear! Q)
  (void))

;;; --------------------------------------------------------------------

(define* (deque-purge! {Q deque?})
  ($deque-purge! Q))

(define ($deque-purge! Q)
  ($deque-first-pair-set! Q '())
  ($deque-last-pair-set!  Q #f))


;;;; conversion

(define* (deque->list {Q deque?})
  (list-copy/stx ($deque-first-pair Q)))

(define* (list->deque {ell list?})
  (apply deque ell))

;;; --------------------------------------------------------------------

(define* (deque->vector {Q deque?})
  (list->vector ($deque-first-pair Q)))

(define* (vector->deque {vec vector?})
  (apply deque (vector->list vec)))


;;;; done

#| end of library |# )

;;; end of file
