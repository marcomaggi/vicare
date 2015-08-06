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


#!vicare
(library (vicare containers stacks)
  (export
    stack
    make-stack			stack?

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

    stack->list			list->stack
    stack->vector		vector->stack)
  (import (vicare)
    (vicare system $pairs)
    (vicare system $numerics))


;;; helpers

(define-inline (list-copy/stx ?ell)
  (let recur ((ell ?ell))
    (if (pair? ell)
	(cons ($car ell) (recur ($cdr ell)))
      ell)))


;;;; data structure

(define-record-type stack
  (nongenerative vicare:containers:stack)
  (protocol
   (lambda (make-record)
     (lambda items
       (make-record #f items))))
  (fields (mutable uid)
	  (mutable first-pair)))


;;;; UID stuff

(define* (stack-hash {S stack?})
  ($stack-hash S))

(define ($stack-hash S)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (symbol-hash ($stack-uid S)))

;;; --------------------------------------------------------------------

(define* (stack-putprop {S stack?} {key symbol?} value)
  ($stack-putprop S key value))

(define ($stack-putprop S key value)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (putprop ($stack-uid S) key value))

;;; --------------------------------------------------------------------

(define* (stack-getprop {S stack?} {key symbol?})
  ($stack-getprop S key))

(define ($stack-getprop S key)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (getprop ($stack-uid S) key))

;;; --------------------------------------------------------------------

(define* (stack-remprop {S stack?} {key symbol?})
  ($stack-remprop S key))

(define ($stack-remprop S key)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (remprop ($stack-uid S) key))

;;; --------------------------------------------------------------------

(define* (stack-property-list {S stack?})
  ($stack-property-list S))

(define ($stack-property-list S)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (property-list ($stack-uid S)))


;;;; inspection

(define* (stack-empty? {S stack?})
  ($stack-empty? S))

(define ($stack-empty? S)
  (null? ($stack-first-pair S)))

;;; --------------------------------------------------------------------

(define* (stack-not-empty? {S stack?})
  ($stack-not-empty? S))

(define ($stack-not-empty? S)
  (pair? ($stack-first-pair S)))

;;; --------------------------------------------------------------------

(define* (stack-size {S stack?})
  ($stack-size S))

(define ($stack-size S)
  (let loop ((ell ($stack-first-pair S))
	     (len 0))
    (if (pair? ell)
	(loop ($cdr ell) ($add-fixnum-number 1 len))
      len)))


;;;; accessors and mutators

(define* (stack-top {S stack?})
  ($stack-top S))

(define ($stack-top S)
  (if (pair? ($stack-first-pair S))
      ($car ($stack-first-pair S))
    (assertion-violation 'stack-top "stack is empty" S)))

;;; --------------------------------------------------------------------

(define* (stack-push! {S stack?} obj)
  ($stack-push! S obj))

(define ($stack-push! S obj)
  ($stack-first-pair-set! S (cons obj ($stack-first-pair S))))

;;; --------------------------------------------------------------------

(define* (stack-pop! {S stack?})
  ($stack-pop! S))

(define* ($stack-pop! S)
  (if (pair? ($stack-first-pair S))
      (receive-and-return (value)
	  ($car ($stack-first-pair S))
	($stack-first-pair-set! S ($cdr ($stack-first-pair S))))
    (assertion-violation __who__ "stack is empty" S)))

;;; --------------------------------------------------------------------

(define* (stack-purge! {S stack?})
  ($stack-purge! S))

(define ($stack-purge! S)
  ($stack-first-pair-set! S '()))


;;;; conversion

(define* (stack->list {S stack?})
  (list-copy/stx ($stack-first-pair S)))

(define* (list->stack {ell list?})
  (apply make-stack ell))

;;; --------------------------------------------------------------------

(define* (stack->vector {S stack?})
  (list->vector ($stack-first-pair S)))

(define* (vector->stack {vec vector?})
  (apply make-stack (vector->list vec)))


;;;; done

#| end of library |# )

;;; end of file
