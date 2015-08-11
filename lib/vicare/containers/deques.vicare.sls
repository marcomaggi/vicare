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

    deque-fold-front		$deque-fold-front
    deque-fold-rear		$deque-fold-rear

    deque->list			list->deque
    deque->vector		vector->deque)
  (import (vicare)
    (vicare containers slots))


;;;; data structure

(define-record-type (<deque> make-deque deque?)
  (nongenerative vicare:containers:<deque>)
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

(define-alias  deque-uid		 <deque>-uid)
(define-alias $deque-uid		$<deque>-uid)

(define-alias  deque-uid-set!		 <deque>-uid-set!)
(define-alias $deque-uid-set!		$<deque>-uid-set!)

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


;;;; aliases

(define-alias  deque-empty?		 slots-empty?)
(define-alias $deque-empty?		$slots-empty?)

(define-alias  deque-not-empty?		 slots-not-empty?)
(define-alias $deque-not-empty?		$slots-not-empty?)

(define-alias  deque-size		 slots-size)
(define-alias $deque-size		$slots-size)

;;; --------------------------------------------------------------------

(define-alias  deque-front		 slots-front)
(define-alias $deque-front		$slots-front)

(define-alias  deque-rear		 slots-rear)
(define-alias $deque-rear		$slots-rear)

(define-alias  deque-push-front!	 slots-push-front!)
(define-alias $deque-push-front!	$slots-push-front!)

(define-alias  deque-push-rear!		 slots-push-rear!)
(define-alias $deque-push-rear!		$slots-push-rear!)

(define-alias  deque-pop-front!		 slots-pop-front!)
(define-alias $deque-pop-front!		$slots-pop-front!)

(define-alias  deque-pop-rear!		 slots-pop-rear!)
(define-alias $deque-pop-rear!		$slots-pop-rear!)

(define-alias  deque-purge!		 slots-purge!)
(define-alias $deque-purge!		$slots-purge!)

;;; --------------------------------------------------------------------

(define-alias  deque-fold-front		 slots-fold-front)
(define-alias $deque-fold-front		$slots-fold-front)

(define-alias  deque-fold-rear		 slots-fold-rear)
(define-alias $deque-fold-rear		$slots-fold-rear)

;;; --------------------------------------------------------------------

(define-alias  deque->list		 slots->list)
(define-alias $deque->list		$slots->list)

(define-alias  deque->vector		 slots->vector)
(define-alias $deque->vector		$slots->vector)

(define-alias  list->deque		 list->slots)
(define-alias $list->deque		$list->slots)

(define-alias  vector->deque		 vector->slots)
(define-alias $vector->deque		$vector->slots)


;;;; done

#| end of library |# )

;;; end of file
