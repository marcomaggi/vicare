;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: simple stack containers
;;;Date: Wed Sep 25, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers stacks)
  (export
    <stack>			make-stack
    stack			stack?

    stack-hash			$stack-hash
    stack-putprop		$stack-putprop
    stack-getprop		$stack-getprop
    stack-remprop		$stack-remprop
    stack-property-list		$stack-property-list


    stack-empty?		$stack-empty?
    stack-not-empty?		$stack-not-empty?
    stack-size			$stack-size

    stack-top			$stack-top
    stack-push!			$stack-push!
    stack-pop!			$stack-pop!
    stack-purge!		$stack-purge!

    stack-fold-left		$stack-fold-left
    stack-fold-right		$stack-fold-right

    stack-map-left		$stack-map-left
    stack-map-right		$stack-map-right
    stack-for-each-left		$stack-for-each-left
    stack-for-each-right	$stack-for-each-right

    (rename
     (stack-map-left		stack-map)
     ($stack-map-left		$stack-map)
     (stack-for-each-left	stack-for-each)
     ($stack-for-each-left	$stack-for-each))

    stack-find-left		$stack-find-left
    stack-find-right		$stack-find-right
    stack-for-all		$stack-for-all
    stack-exists-left		$stack-exists-left
    stack-exists-right		$stack-exists-right

    (rename
     (stack-find-left		stack-find)
     ($stack-find-left		$stack-find)
     (stack-exists-left		stack-exists)
     ($stack-exists-left	$stack-exists))

    stack-filter		$stack-filter
    stack-partition		$stack-partition

    stack-copy!			$stack-copy!
    stack-reverse!		$stack-reverse!

    stack->list			list->stack
    stack->vector		vector->stack

    make-stack-iteration-thunk)
  (import (vicare)
    (vicare containers slots))


;;;; helpers

(define-syntax-rule (declare-operation-unary ?safe ?unsafe)
  (define* (?safe {D stack?})
    (?unsafe D)))


;;;; data structure

(define-record-type (<stack> make-stack stack?)
  (nongenerative vicare:containers:<stack>)
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

(define-alias  stack-uid		 <stack>-uid)
(define-alias $stack-uid		$<stack>-uid)

(define-alias  stack-uid-set!		 <stack>-uid-set!)
(define-alias $stack-uid-set!		$<stack>-uid-set!)

;;; --------------------------------------------------------------------

(define (stack . item*)
  (receive-and-return (D)
      (make-stack)
    (for-each (lambda (obj)
		($slots-push-rear! D obj))
      item*)))


;;;; UID stuff

(define* (stack-hash {Q stack?})
  ($stack-hash Q))

(define ($stack-hash Q)
  (unless ($stack-uid Q)
    ($stack-uid-set! Q (gensym)))
  (symbol-hash ($stack-uid Q)))

;;; --------------------------------------------------------------------

(define* (stack-putprop {Q stack?} {key symbol?} value)
  ($stack-putprop Q key value))

(define ($stack-putprop Q key value)
  (unless ($stack-uid Q)
    ($stack-uid-set! Q (gensym)))
  (putprop ($stack-uid Q) key value))

;;; --------------------------------------------------------------------

(define* (stack-getprop {Q stack?} {key symbol?})
  ($stack-getprop Q key))

(define ($stack-getprop Q key)
  (unless ($stack-uid Q)
    ($stack-uid-set! Q (gensym)))
  (getprop ($stack-uid Q) key))

;;; --------------------------------------------------------------------

(define* (stack-remprop {Q stack?} {key symbol?})
  ($stack-remprop Q key))

(define ($stack-remprop Q key)
  (unless ($stack-uid Q)
    ($stack-uid-set! Q (gensym)))
  (remprop ($stack-uid Q) key))

;;; --------------------------------------------------------------------

(define* (stack-property-list {Q stack?})
  ($stack-property-list Q))

(define ($stack-property-list Q)
  (unless ($stack-uid Q)
    ($stack-uid-set! Q (gensym)))
  (property-list ($stack-uid Q)))


;;;; inspection

(declare-operation-unary stack-empty?		$stack-empty?)
(declare-operation-unary stack-not-empty?	$stack-not-empty?)
(declare-operation-unary stack-size		$stack-size)

(define-alias $stack-empty?			$slots-empty?)
(define-alias $stack-not-empty?			$slots-not-empty?)
(define-alias $stack-size			$slots-size)


;;;; accessors and mutators

(declare-operation-unary stack-top		$stack-top)

(define-alias $stack-top			$slots-front)

;;; --------------------------------------------------------------------

(define* (stack-push! {D stack?} obj)
  ($stack-push! D obj))

(define-alias $stack-push!			$slots-push-front!)

;;; --------------------------------------------------------------------

(declare-operation-unary stack-pop!		$stack-pop!)

(define-alias $stack-pop!			$slots-pop-front!)

;;; --------------------------------------------------------------------

(declare-operation-unary stack-purge!		$stack-purge!)
(define-alias $stack-purge!			$slots-purge!)


;;;; mapping

(define* (stack-map-left {dst stack?} {fun procedure?} {src stack?})
  ($stack-map-left dst fun src))

(define* (stack-map-right  {dst stack?} {fun procedure?} {src stack?})
  ($stack-map-right dst fun src))

(define* (stack-for-each-left {fun procedure?} {src stack?})
  ($stack-for-each-left fun src))

(define* (stack-for-each-right  {fun procedure?} {src stack?})
  ($stack-for-each-right fun src))

(define-alias $stack-map-left		$slots-map-left)
(define-alias $stack-map-right		$slots-map-right)
(define-alias $stack-for-each-left	$slots-for-each-left)
(define-alias $stack-for-each-right	$slots-for-each-right)


;;;; folding

(define* (stack-fold-left {kons procedure?} knil {D stack?})
  ($stack-fold-left kons knil D))

(define* (stack-fold-right {kons procedure?} knil {D stack?})
  ($stack-fold-right kons knil D))

(define-alias $stack-fold-left		$slots-fold-left)
(define-alias $stack-fold-right		$slots-fold-right)


;;;; searching

(case-define* stack-find-left
  (({fun procedure?} {D stack?})
   ($stack-find-left fun D #f))
  (({fun procedure?} {D stack?} not-found-rv)
   ($stack-find-left fun D not-found-rv)))

(case-define* stack-find-right
  (({fun procedure?} {D stack?})
   ($stack-find-right fun D #f))
  (({fun procedure?} {D stack?} not-found-rv)
   ($stack-find-right fun D not-found-rv)))

(define* (stack-for-all {fun procedure?} {D stack?})
  ($stack-for-all fun D))

(define* (stack-exists-left {fun procedure?} {D stack?})
  ($stack-exists-left fun D))

(define* (stack-exists-right {fun procedure?} {D stack?})
  ($stack-exists-right fun D))

(define-alias $stack-for-all		$slots-for-all)
(define-alias $stack-find-left		$slots-find-left)
(define-alias $stack-find-right		$slots-find-right)
(define-alias $stack-exists-left	$slots-exists-left)
(define-alias $stack-exists-right	$slots-exists-right)


;;;; filtering

(define* (stack-filter {dst-stack stack?} {fun procedure?} {src-stack stack?})
  ($stack-filter dst-stack fun src-stack))

(define-alias $stack-filter		$slots-filter)

;;; --------------------------------------------------------------------

(define* (stack-partition {matching-stack stack?} {not-matching-stack stack?} {fun procedure?} {src-stack stack?})
  ($stack-partition matching-stack not-matching-stack fun src-stack))

(define-alias $stack-partition		$slots-partition)


;;;; conversion

(declare-operation-unary stack->list	$stack->list)
(declare-operation-unary stack->vector	$stack->vector)

(define* (list->stack {ell list?})
  ($list->slots (make-stack) ell))

(define* (vector->stack {vec vector?})
  ($vector->slots (make-stack) vec))

(define-alias $stack->list		$slots->list)
(define-alias $stack->vector		$slots->vector)
(define-alias $list->stack		$list->slots)
(define-alias $vector->stack		$vector->slots)


;;;; miscellaneous operations

(define* (stack-copy! {dst stack?} {src stack?})
  ($stack-copy! dst src))

(define-alias $stack-copy!		$slots-copy!)

;;; --------------------------------------------------------------------

(define* (stack-reverse! {dst stack?} {src stack?})
  ($stack-reverse! dst src))

(define-alias $stack-reverse!		$slots-reverse!)


;;;; iteration thunks

(define* (make-stack-iteration-thunk {S stack?})
  (lambda ()
    (if ($stack-empty? S)
	(void)
      ($stack-pop! S))))


;;;; done

#| end of library |# )

;;; end of file
