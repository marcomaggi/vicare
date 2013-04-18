;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: McCarthy's amb operator
;;;Date: Thu Apr 18, 2013
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
(library (vicare language amb)
  (export
    amb
    amb-assert
    amb-random
    amb-random-fixnum-maker)
  (import (vicare)
    (prefix (vicare unsafe operations)
	    $))


;;;; core syntax

(define %current-fail-escape
  (make-parameter
      (lambda ()
	(error 'amb "search tree tree exhausted"))))

(define %previous-fail-escape
  (make-parameter
      (lambda ()
	(assertion-violation 'amb "no saved continuation"))))

(define-syntax amb
  (syntax-rules ()
    ((_)
     ((%current-fail-escape)))
    ((_ ?expr0 ?expr ...)
     (call/cc
    	 (lambda (return)
	   (parametrise ((%previous-fail-escape (%current-fail-escape)))
	     (call/cc
		 (lambda (escape)
		   (%current-fail-escape escape)
		   (return ?expr0)))
	     (%current-fail-escape (%previous-fail-escape))
	     (amb ?expr ...)))))
    ))


;;;; extensions

(define-syntax amb-assert
  (syntax-rules ()
    ((_ ?expr)
     (or ?expr (amb)))))

(module (amb-random amb-random-fixnum-maker)
  ;;Like  AMB  but  randomly  select   the  order  in  which  the  given
  ;;expressions are tried.
  ;;
  (define-syntax amb-random
    (syntax-rules ()
      ((_)
       ((%current-fail-escape)))
      ((_ ?expr0 ?expr ...)
       (%amb-random `#(,(lambda () ?expr0) ,(lambda () ?expr) ...)))
      ))

  (define amb-random-fixnum-maker
    ;;Hold  a procedure  accepting  a fixnum  as  single argument:  when
    ;;applied to the fixnum N it must  returns a fixnum in the range [0,
    ;;N).
    ;;
    (make-parameter
	random
      (lambda (obj)
	(assert (procedure? obj))
	obj)))

  (define (%amb-random thunks)
    (let* ((thunks.len  ($vector-length thunks))
	   (order       (%make-order-vector thunks.len)))
      (call/cc
	  (lambda (return)
	    (let next-choice ((idx 0))
	      (if ($fx= idx thunks.len)
		  (amb)
		(parametrise ((%previous-fail-escape (%current-fail-escape)))
		  (call/cc
		      (lambda (escape)
			(%current-fail-escape escape)
			(return (($vector-ref thunks ($vector-ref order idx))))))
		  (%current-fail-escape (%previous-fail-escape))
		  (next-choice ($fxadd1 idx)))))))))

  (define (%make-order-vector N)
    ;;Return a  vector of  length N  holding a  random permutation  of the
    ;;fixnums in the range [0, N).
    ;;
    ;;For  the   algorithm  refer  to   Knuth's  ``The  Art   of  Computer
    ;;Programming'', Vol. II, 2nd ed., Algorithm P of Section 3.4.2.
    ;;
    (assert (and (fixnum? N) (fxpositive? N)))
    (let ((vec (make-vector N 0)))
      (do ((i 0 ($fxadd1 i)))
	  (($fx= i N))
	($vector-set! vec i i))
      (do ((k N ($fxsub1 k)))
	  (($fx= k 1)
	   vec)
	(let* ((i  ($fxsub1 k))
	       (j  ((amb-random-fixnum-maker) k))
	       (xi ($vector-ref vec i))
	       (xj ($vector-ref vec j)))
	  ($vector-set! vec i xj)
	  ($vector-set! vec j xi)))))

  #| end of module: AMB-RANDOM |# )


;;;; done

)

;;; end of file
