;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: simple queue containers
;;;Date: Wed Sep 25, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare containers queues)
  (export
    queue

    queue.vicare-arguments-validation
    queue/false.vicare-arguments-validation

    make-queue			queue?

    queue-hash			$queue-hash
    queue-putprop		$queue-putprop
    queue-getprop		$queue-getprop
    queue-remprop		$queue-remprop
    queue-property-list		$queue-property-list


    queue-empty?		$queue-empty?
    queue-not-empty?		$queue-not-empty?
    queue-size			$queue-size

    queue-front			$queue-front
    queue-rear			$queue-rear
    queue-push!		$queue-push!
    queue-pop!		$queue-pop!
    queue-purge!		$queue-purge!

    queue->list			list->queue
    queue->vector		vector->queue)
  (import (vicare)
    (vicare unsafe operations)
    (vicare system $numerics)
    (vicare arguments validation))


;;; helpers

(define-inline (list-copy/stx ?ell)
  (let recur ((ell ?ell))
    (if (pair? ell)
	(cons ($car ell) (recur ($cdr ell)))
      ell)))

(define-inline (last-pair/stx ?x)
  ;;*WARNING* Do  not rename LAST-PAIR/STX to LAST-PAIR,  it would clash
  ;;with the LAST-PAIR field of <queue> records.
  ;;
  (let ((x ?x))
    (if (null? x)
	#f
      (let loop ((x x))
	(if (pair? ($cdr x))
	    (loop ($cdr x))
	  x)))))


;;;; data structure

(define-record-type queue
  (nongenerative vicare:containers:queue)
  (protocol
   (lambda (make-record)
     (lambda items
       (make-record #f items (last-pair/stx items)))))
  (fields (mutable uid)
	  (mutable first-pair)
	  (mutable last-pair)))

(define-argument-validation (queue who obj)
  (queue? obj)
  (procedure-argument-violation who "expected queue object as argument" obj))

(define-argument-validation (queue/false who obj)
  (or (not obj) (queue? obj))
  (procedure-argument-violation who "expected false or queue object as argument" obj))


;;;; UID stuff

(define (queue-hash Q)
  (define who 'queue-hash)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-hash Q)))

(define ($queue-hash Q)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (symbol-hash ($queue-uid Q)))

;;; --------------------------------------------------------------------

(define (queue-putprop Q key value)
  (define who 'queue-putprop)
  (with-arguments-validation (who)
      ((queue		Q)
       (symbol		key))
    ($queue-putprop Q key value)))

(define ($queue-putprop Q key value)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (putprop ($queue-uid Q) key value))

;;; --------------------------------------------------------------------

(define (queue-getprop Q key)
  (define who 'queue-getprop)
  (with-arguments-validation (who)
      ((queue		Q)
       (symbol		key))
    ($queue-getprop Q key)))

(define ($queue-getprop Q key)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (getprop ($queue-uid Q) key))

;;; --------------------------------------------------------------------

(define (queue-remprop Q key)
  (define who 'queue-remprop)
  (with-arguments-validation (who)
      ((queue		Q)
       (symbol		key))
    ($queue-remprop Q key)))

(define ($queue-remprop Q key)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (remprop ($queue-uid Q) key))

;;; --------------------------------------------------------------------

(define (queue-property-list Q)
  (define who 'queue-property-list)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-property-list Q)))

(define ($queue-property-list Q)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (property-list ($queue-uid Q)))


;;;; inspection

(define (queue-empty? Q)
  (define who 'queue-empty?)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-empty? Q)))

(define ($queue-empty? Q)
  (null? ($queue-first-pair Q)))

;;; --------------------------------------------------------------------

(define (queue-not-empty? Q)
  (define who 'queue-not-empty?)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-not-empty? Q)))

(define ($queue-not-empty? Q)
  (pair? ($queue-first-pair Q)))

;;; --------------------------------------------------------------------

(define (queue-size Q)
  (define who 'queue-size)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-size Q)))

(define ($queue-size Q)
  (let loop ((ell ($queue-first-pair Q))
	     (len 0))
    (if (pair? ell)
	(loop ($cdr ell) ($add-fixnum-number 1 len))
      len)))


;;;; accessors and mutators

(define (queue-front Q)
  (define who 'queue-front)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-front Q)))

(define ($queue-front Q)
  (if (pair? ($queue-first-pair Q))
      ($car ($queue-first-pair Q))
    (assertion-violation 'queue-front "queue is empty" Q)))

;;; --------------------------------------------------------------------

(define (queue-rear Q)
  (define who 'queue-rear)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-rear Q)))

(define ($queue-rear Q)
  (if (pair? ($queue-last-pair Q))
      ($car ($queue-last-pair Q))
    (assertion-violation 'queue-rear "queue is empty" Q)))

;;; --------------------------------------------------------------------

(define (queue-push! Q obj)
  (define who 'queue-push!)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-push! Q obj)))

(define ($queue-push! Q obj)
  (let ((new-last-pair (list obj)))
    (if (pair? ($queue-first-pair Q))
	;;The queue  is not empty: append  the new last-pair to  the old
	;;last-pair.
	($set-cdr! ($queue-last-pair Q) new-last-pair)
      ;;The  queue  is  empty:  the   new  last-pair  is  also  the  new
      ;;first-pair, so store it the first-pair slot.
      ($queue-first-pair-set! Q new-last-pair))
    ;;Store the new last-pair in the last-pair slot.
    ($queue-last-pair-set! Q new-last-pair)))

;;; --------------------------------------------------------------------

(define (queue-pop! Q)
  (define who 'queue-pop!)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-pop! Q)))

(define ($queue-pop! Q)
  (define who '$queue-pop!)
  (let ((old-first-pair ($queue-first-pair Q)))
    (if (pair? old-first-pair)
	(begin
	  ;;The new first-pair is the  next of the old first-pair, which
	  ;;can be null.
	  ($queue-first-pair-set! Q ($cdr old-first-pair))
	  ;;If the old  first-pair is also the old  last-pair: reset the
	  ;;last-pair so that the queue results as empty.
	  (when (eq? ($queue-last-pair Q) old-first-pair)
	    ($queue-last-pair-set! Q #f)))
      (error 'queue-pop! "queue is empty" Q))
    ($car old-first-pair)))

;;; --------------------------------------------------------------------

(define (queue-purge! Q)
  (define who 'queue-purge!)
  (with-arguments-validation (who)
      ((queue		Q))
    ($queue-purge! Q)))

(define ($queue-purge! Q)
  ($queue-first-pair-set! Q '())
  ($queue-last-pair-set!  Q #f))


;;;; conversion

(define (queue->list Q)
  (define who 'queue->list)
  (with-arguments-validation (who)
      ((queue		Q))
    (list-copy/stx ($queue-first-pair Q))))

(define (list->queue ell)
  (define who 'list->queue)
  (with-arguments-validation (who)
      ((list		ell))
    (apply make-queue ell)))

;;; --------------------------------------------------------------------

(define (queue->vector Q)
  (define who 'queue->vector)
  (with-arguments-validation (who)
      ((queue		Q))
    (list->vector ($queue-first-pair Q))))

(define (vector->queue vec)
  (define who 'vector->queue)
  (with-arguments-validation (who)
      ((vector		vec))
    (apply make-queue (vector->list vec))))


;;;; done

)

;;; end of file
