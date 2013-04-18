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
(library (vicare language-extensions amb)
  (export
    &amb-exhaustion
    make-amb-exhaustion			amb-exhaustion?
    with-ambiguous-choices		amb
    amb-assert
    amb-random				amb-random-fixnum-maker)
  (import (vicare)
    (prefix (vicare unsafe operations)
	    $))


;;;; type definitions

;;Tag the compound condition object  raised whenever the search three is
;;exhausted.
;;
(define-condition-type &amb-exhaustion
    &condition
  make-amb-exhaustion
  amb-exhaustion?)

;;Tag the compound condition object  raised whenever AMB is used outside
;;the dynamic environment prepared by WITH-AMBIGUOUS-CHOICES.
;;
(define-condition-type &amb-not-initialised
    &assertion
  make-amb-not-initialised
  amb-not-initialised?)


;;;; error handling

(define (%amb-correctly-initialised?)
  ;;Invoked  whenever  AMB  is  used  outside  the  dynamic  environment
  ;;prepared by WITH-AMBIGUOUS-CHOICES.
  ;;
  (unless (procedure? (%current-fail-escape))
    (raise
     (condition
      (make-amb-not-initialised)
      (make-who-condition 'amb)
      (make-message-condition "missing initialisation of ambiguous choices")))))

(define %raise-exhausted-search-tree
  ;;Invoked whenever the search three is exhausted.
  ;;
  (let ((E (condition (make-amb-exhaustion)
		      (make-who-condition 'amb)
		      (make-message-condition "search tree exhausted"))))
    (lambda ()
      (raise-continuable E))))

(define (%raise-internal-error)
  ;;Raised in case of internal error.
  ;;
  (assertion-violation 'amb
    "internal error while , attempt to escape to next choice with no choice"))


;;;; core syntaxes

(define-syntax with-ambiguous-choices
  ;;Initialised the dynamic environment for a new AMB search.
  ;;
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (parametrise ((%current-fail-escape  %raise-exhausted-search-tree)
		   (%previous-fail-escape %raise-internal-error))
       ?body0 ?body ...))))

(define-syntax amb
  (syntax-rules ()
    ((_)
     ((%current-fail-escape)))
    ((_ ?expr0 ?expr ...)
     (begin
       (%amb-correctly-initialised?)
       (call/cc
	   (lambda (return)
	     (parametrise ((%previous-fail-escape (%current-fail-escape)))
	       (call/cc
		   (lambda (escape)
		     (%current-fail-escape escape)
		     (return ?expr0)))
	       (%current-fail-escape (%previous-fail-escape))
	       (amb ?expr ...))))))
    ))

(define %current-fail-escape
  (make-parameter #f))

(define %previous-fail-escape
  (make-parameter #f))


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
    (%amb-correctly-initialised?)
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
