;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: miscellaneous increment syntaxes
;;;Date: Fri Sep 27, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language increments)
  (export
    (rename (pre-incr!	incr!)
	    (pre-decr!	decr!))
    pre-incr!		post-incr!
    pre-decr!		post-decr!
    (rename ($pre-incr!	$incr!)
	    ($pre-decr!	$decr!))
    $pre-incr!		$post-incr!
    $pre-decr!		$post-decr!)
  (import (vicare)
    (only (nausicaa language oopp)
	  set!/tags)
    (only (vicare system $numerics)
	  $add-number-fixnum
	  $sub-number-fixnum))


(define-syntax (pre-incr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id (+ ?id 1))
	 ?id))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id (+ ?id ?step))
	 ?id))
    ((_ ?expr)
     #'(+ ?expr 1))
    ((_ ?expr ?step)
     #'(+ ?expr ?step))
    (_
     (syntax-violation 'pre-incr! "invalid pre-increment operation" stx))))

(define-syntax (pre-decr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id (- ?id 1))
	 ?id))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id (- ?id ?step))
	 ?id))
    ((_ ?expr)
     #'(- ?expr 1))
    ((_ ?expr ?step)
     #'(- ?expr ?step))
    (_
     (syntax-violation 'pre-decr! "invalid pre-decrement operation" stx))))

(define-syntax (post-incr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id (+ ?id 1))))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id (+ ?id ?step))))
    ((_ ?expr)
     #'(+ ?expr 1))
    ((_ ?expr ?step)
     #'(+ ?expr ?step))
    (_
     (syntax-violation 'post-incr! "invalid post-increment operation" stx))))

(define-syntax (post-decr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id (- ?id 1))))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id (- ?id ?step))))
    ((_ ?expr)
     #'(- ?expr 1))
    ((_ ?expr ?step)
     #'(- ?expr ?step))
    (_
     (syntax-violation 'post-decr! "invalid post-decrement operation" stx))))


(define-syntax ($pre-incr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id ($add-number-fixnum ?id 1))
	 ?id))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id ($add-number-number ?id ?step))
	 ?id))
    ((_ ?expr)
     #'($add-number-fixnum ?expr 1))
    ((_ ?expr ?step)
     #'($add-number-number ?expr ?step))
    (_
     (syntax-violation '$pre-incr! "invalid pre-increment operation" stx))))

(define-syntax ($pre-decr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id ($sub-number-fixnum ?id 1))
	 ?id))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(begin
	 (set!/tags ?id ($sub-number-number ?id ?step))
	 ?id))
    ((_ ?expr)
     #'($add-number-fixnum ?expr 1))
    ((_ ?expr ?step)
     #'($add-number-number ?expr ?step))
    (_
     (syntax-violation '$pre-decr! "invalid pre-decrement operation" stx))))

(define-syntax ($post-incr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id ($add-number-fixnum ?id 1))))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id ($add-number-number ?id ?step))))
    ((_ ?expr)
     #'($add-number-fixnum ?expr 1))
    ((_ ?expr ?step)
     #'($add-number-number ?expr ?step))
    (_
     (syntax-violation '$post-incr! "invalid post-increment operation" stx))))

(define-syntax ($post-decr! stx)
  (syntax-case stx ()
    ((_ ?id)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id ($sub-number-fixnum ?id 1))))
    ((_ ?id ?step)
     (identifier? #'?id)
     #'(receive-and-return (v)
	   ?id
	 (set!/tags ?id ($sub-number-number ?id ?step))))
    ((_ ?expr)
     #'($sub-number-fixnum ?expr 1))
    ((_ ?expr ?step)
     #'($sub-number-number ?expr ?step))
    (_
     (syntax-violation '$post-decr! "invalid post-decrement operation" stx))))


;;;; done

)

;;; end of file
