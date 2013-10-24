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
(library (vicare containers stacks)
  (export
    stack

    stack.vicare-arguments-validation

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
    (vicare unsafe operations)
    (vicare system $numerics)
    (vicare arguments validation))


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

(define-argument-validation (stack who obj)
  (stack? obj)
  (procedure-argument-violation who "expected stack object as argument" obj))


;;;; UID stuff

(define (stack-hash S)
  (define who 'stack-hash)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-hash S)))

(define ($stack-hash S)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (symbol-hash ($stack-uid S)))

;;; --------------------------------------------------------------------

(define (stack-putprop S key value)
  (define who 'stack-putprop)
  (with-arguments-validation (who)
      ((stack		S)
       (symbol		key))
    ($stack-putprop S key value)))

(define ($stack-putprop S key value)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (putprop ($stack-uid S) key value))

;;; --------------------------------------------------------------------

(define (stack-getprop S key)
  (define who 'stack-getprop)
  (with-arguments-validation (who)
      ((stack		S)
       (symbol		key))
    ($stack-getprop S key)))

(define ($stack-getprop S key)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (getprop ($stack-uid S) key))

;;; --------------------------------------------------------------------

(define (stack-remprop S key)
  (define who 'stack-remprop)
  (with-arguments-validation (who)
      ((stack		S)
       (symbol		key))
    ($stack-remprop S key)))

(define ($stack-remprop S key)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (remprop ($stack-uid S) key))

;;; --------------------------------------------------------------------

(define (stack-property-list S)
  (define who 'stack-property-list)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-property-list S)))

(define ($stack-property-list S)
  (unless ($stack-uid S)
    ($stack-uid-set! S (gensym)))
  (property-list ($stack-uid S)))


;;;; inspection

(define (stack-empty? S)
  (define who 'stack-empty?)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-empty? S)))

(define ($stack-empty? S)
  (null? ($stack-first-pair S)))

;;; --------------------------------------------------------------------

(define (stack-not-empty? S)
  (define who 'stack-not-empty?)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-not-empty? S)))

(define ($stack-not-empty? S)
  (pair? ($stack-first-pair S)))

;;; --------------------------------------------------------------------

(define (stack-size S)
  (define who 'stack-size)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-size S)))

(define ($stack-size S)
  (let loop ((ell ($stack-first-pair S))
	     (len 0))
    (if (pair? ell)
	(loop ($cdr ell) ($add-fixnum-number 1 len))
      len)))


;;;; accessors and mutators

(define (stack-top S)
  (define who 'stack-top)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-top S)))

(define ($stack-top S)
  (if (pair? ($stack-first-pair S))
      ($car ($stack-first-pair S))
    (assertion-violation 'stack-top "stack is empty" S)))

;;; --------------------------------------------------------------------

(define (stack-push! S obj)
  (define who 'stack-push!)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-push! S obj)))

(define ($stack-push! S obj)
  ($stack-first-pair-set! S (cons obj ($stack-first-pair S))))

;;; --------------------------------------------------------------------

(define (stack-pop! S)
  (define who 'stack-pop!)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-pop! S)))

(define ($stack-pop! S)
  (define who '$stack-pop!)
  (if (pair? ($stack-first-pair S))
      (receive-and-return (value)
	  ($car ($stack-first-pair S))
	($stack-first-pair-set! S ($cdr ($stack-first-pair S))))
    (assertion-violation who "stack is empty" S)))

;;; --------------------------------------------------------------------

(define (stack-purge! S)
  (define who 'stack-purge!)
  (with-arguments-validation (who)
      ((stack		S))
    ($stack-purge! S)))

(define ($stack-purge! S)
  ($stack-first-pair-set! S '()))


;;;; conversion

(define (stack->list S)
  (define who 'stack->list)
  (with-arguments-validation (who)
      ((stack		S))
    (list-copy/stx ($stack-first-pair S))))

(define (list->stack ell)
  (define who 'list->stack)
  (with-arguments-validation (who)
      ((list		ell))
    (apply make-stack ell)))

;;; --------------------------------------------------------------------

(define (stack->vector S)
  (define who 'stack->vector)
  (with-arguments-validation (who)
      ((stack		S))
    (list->vector ($stack-first-pair S))))

(define (vector->stack vec)
  (define who 'vector->stack)
  (with-arguments-validation (who)
      ((vector		vec))
    (apply make-stack (vector->list vec))))


;;;; done

)

;;; end of file
