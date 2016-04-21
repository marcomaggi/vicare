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
;;;Copyright (c) 2009, 2010, 2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers queues)
  (export
    <queue>			make-queue
    queue			queue?

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
    queue-push!			$queue-push!
    queue-pop!			$queue-pop!
    queue-purge!		$queue-purge!

    queue-fold-left		$queue-fold-left
    queue-fold-right		$queue-fold-right

    queue-map-left		$queue-map-left
    queue-map-right		$queue-map-right
    queue-for-each-left		$queue-for-each-left
    queue-for-each-right	$queue-for-each-right

    (rename
     (queue-map-left		queue-map)
     ($queue-map-left		$queue-map)
     (queue-for-each-left	queue-for-each)
     ($queue-for-each-left	$queue-for-each))

    queue-find-left		$queue-find-left
    queue-find-right		$queue-find-right
    queue-for-all		$queue-for-all
    queue-exists-left		$queue-exists-left
    queue-exists-right		$queue-exists-right

    (rename
     (queue-find-left		queue-find)
     ($queue-find-left		$queue-find)
     (queue-exists-left		queue-exists)
     ($queue-exists-left	$queue-exists))

    queue-filter		$queue-filter
    queue-partition		$queue-partition

    queue-copy!			$queue-copy!
    queue-reverse!		$queue-reverse!

    queue->list			list->queue
    queue->vector		vector->queue

    make-queue-iteration-thunk)
  (import (vicare)
    (vicare containers slots))


;;;; helpers

(define-syntax-rule (declare-operation-unary ?safe ?unsafe)
  (define* (?safe {D queue?})
    (?unsafe D)))


;;;; data structure

(define-record-type (<queue> make-queue queue?)
  (nongenerative vicare:containers:<queue>)
  (parent <slots>)
  (protocol
   (lambda (make-slots)
     (case-lambda
      (()
       ((make-slots) #f))
      ((buffer-length)
       ((make-slots buffer-length) #f)))))
  (fields (mutable uid)))

;;; --------------------------------------------------------------------

(define-alias  queue-uid		 <queue>-uid)
(define-alias $queue-uid		$<queue>-uid)

(define-alias  queue-uid-set!		 <queue>-uid-set!)
(define-alias $queue-uid-set!		$<queue>-uid-set!)

;;; --------------------------------------------------------------------

(define (queue . item*)
  (receive-and-return (D)
      (make-queue)
    (for-each (lambda (obj)
		($slots-push-rear! D obj))
      item*)))


;;;; UID stuff

(define* (queue-hash {Q queue?})
  ($queue-hash Q))

(define ($queue-hash Q)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (symbol-hash ($queue-uid Q)))

;;; --------------------------------------------------------------------

(define* (queue-putprop {Q queue?} {key symbol?} value)
  ($queue-putprop Q key value))

(define ($queue-putprop Q key value)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (putprop ($queue-uid Q) key value))

;;; --------------------------------------------------------------------

(define* (queue-getprop {Q queue?} {key symbol?})
  ($queue-getprop Q key))

(define ($queue-getprop Q key)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (getprop ($queue-uid Q) key))

;;; --------------------------------------------------------------------

(define* (queue-remprop {Q queue?} {key symbol?})
  ($queue-remprop Q key))

(define ($queue-remprop Q key)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (remprop ($queue-uid Q) key))

;;; --------------------------------------------------------------------

(define* (queue-property-list {Q queue?})
  ($queue-property-list Q))

(define ($queue-property-list Q)
  (unless ($queue-uid Q)
    ($queue-uid-set! Q (gensym)))
  (property-list ($queue-uid Q)))


;;;; inspection

(declare-operation-unary queue-empty?		$queue-empty?)
(declare-operation-unary queue-not-empty?	$queue-not-empty?)
(declare-operation-unary queue-size		$queue-size)

(define-alias $queue-empty?			$slots-empty?)
(define-alias $queue-not-empty?			$slots-not-empty?)
(define-alias $queue-size			$slots-size)


;;;; accessors and mutators

(declare-operation-unary queue-front		$queue-front)
(declare-operation-unary queue-rear		$queue-rear)

(define-alias $queue-front			$slots-front)
(define-alias $queue-rear			$slots-rear)

;;; --------------------------------------------------------------------

(define* (queue-push! {D queue?} obj)
  ($queue-push! D obj))

(define-alias $queue-push!			$slots-push-rear!)

;;; --------------------------------------------------------------------

(declare-operation-unary queue-pop!		$queue-pop!)

(define-alias $queue-pop!			$slots-pop-front!)

;;; --------------------------------------------------------------------

(declare-operation-unary queue-purge!		$queue-purge!)
(define-alias $queue-purge!			$slots-purge!)


;;;; mapping

(define* (queue-map-left {dst queue?} {fun procedure?} {src queue?})
  ($queue-map-left dst fun src))

(define* (queue-map-right  {dst queue?} {fun procedure?} {src queue?})
  ($queue-map-right dst fun src))

(define* (queue-for-each-left {fun procedure?} {src queue?})
  ($queue-for-each-left fun src))

(define* (queue-for-each-right  {fun procedure?} {src queue?})
  ($queue-for-each-right fun src))

(define-alias $queue-map-left		$slots-map-left)
(define-alias $queue-map-right		$slots-map-right)
(define-alias $queue-for-each-left	$slots-for-each-left)
(define-alias $queue-for-each-right	$slots-for-each-right)


;;;; folding

(define* (queue-fold-left {kons procedure?} knil {D queue?})
  ($queue-fold-left kons knil D))

(define* (queue-fold-right {kons procedure?} knil {D queue?})
  ($queue-fold-right kons knil D))

(define-alias $queue-fold-left		$slots-fold-left)
(define-alias $queue-fold-right		$slots-fold-right)


;;;; searching

(case-define* queue-find-left
  (({fun procedure?} {D queue?})
   ($queue-find-left fun D #f))
  (({fun procedure?} {D queue?} not-found-rv)
   ($queue-find-left fun D not-found-rv)))

(case-define* queue-find-right
  (({fun procedure?} {D queue?})
   ($queue-find-right fun D #f))
  (({fun procedure?} {D queue?} not-found-rv)
   ($queue-find-right fun D not-found-rv)))

(define* (queue-for-all {fun procedure?} {D queue?})
  ($queue-for-all fun D))

(define* (queue-exists-left {fun procedure?} {D queue?})
  ($queue-exists-left fun D))

(define* (queue-exists-right {fun procedure?} {D queue?})
  ($queue-exists-right fun D))

(define-alias $queue-for-all		$slots-for-all)
(define-alias $queue-find-left		$slots-find-left)
(define-alias $queue-find-right		$slots-find-right)
(define-alias $queue-exists-left	$slots-exists-left)
(define-alias $queue-exists-right	$slots-exists-right)


;;;; filtering

(define* (queue-filter {dst-queue queue?} {fun procedure?} {src-queue queue?})
  ($queue-filter dst-queue fun src-queue))

(define-alias $queue-filter		$slots-filter)

;;; --------------------------------------------------------------------

(define* (queue-partition {matching-queue queue?} {not-matching-queue queue?} {fun procedure?} {src-queue queue?})
  ($queue-partition matching-queue not-matching-queue fun src-queue))

(define-alias $queue-partition		$slots-partition)


;;;; conversion

(declare-operation-unary queue->list	$queue->list)
(declare-operation-unary queue->vector	$queue->vector)

(define* (list->queue {ell list?})
  ($list->slots (make-queue) ell))

(define* (vector->queue {vec vector?})
  ($vector->slots (make-queue) vec))

(define-alias $queue->list		$slots->list)
(define-alias $queue->vector		$slots->vector)
(define-alias $list->queue		$list->slots)
(define-alias $vector->queue		$vector->slots)


;;;; miscellaneous operations

(define* (queue-copy! {dst queue?} {src queue?})
  ($queue-copy! dst src))

(define-alias $queue-copy!		$slots-copy!)

;;; --------------------------------------------------------------------

(define* (queue-reverse! {dst queue?} {src queue?})
  ($queue-reverse! dst src))

(define-alias $queue-reverse!		$slots-reverse!)


;;;; iteration thunks

(define* (make-queue-iteration-thunk {S queue?})
  (lambda ()
    (if ($queue-empty? S)
	(sentinel)
      ($queue-pop! S))))


;;;; done

#| end of library |# )

;;; end of file
