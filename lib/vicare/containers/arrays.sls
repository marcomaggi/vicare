;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: multidimensional arrays
;;;Date: Wed Sep 18, 2013
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
(library (vicare containers arrays)
  (export

    ;; array position coordinates
    coordinate?
    coordinate.vicare-arguments-validation
    coordinate/false.vicare-arguments-validation
    list-of-coordinates.vicare-arguments-validation
    vector-of-coordinates.vicare-arguments-validation

    ;; array positions
    vector->position			$vector->position
    list->position			$list->position
    make-position			$make-position
    position				$position
    position?
    position-index?			$position-index?
    position-dimension			$position-dimension
    position-ref			$position-ref
    position-set!			$position-set!
    position=?				$position=?
    position-copy			$position-copy

    position.vicare-arguments-validation
    position/false.vicare-arguments-validation
    position-index.vicare-arguments-validation
    list-of-positions.vicare-arguments-validation

    ;; array shapes

    ;; arrays

    ;; misc stuff
    number-of-dimensions.vicare-arguments-validation

    )
  (import (vicare)
    (vicare arguments validation)
    (vicare unsafe operations))


;;;; helpers

(define-syntax-rule ($vector=? ?item= ?vec1 ?vec2)
  ;;Compare  the  built-in Scheme  vectors  ?VEC1  and ?VEC2  using  the
  ;;predicate operation ?ITEM= to compare the items.
  ;;
  (let* ((vec1 ?vec1)
	 (vec2 ?vec2)
	 (len1 ($vector-length vec1)))
    (and ($fx= len1 ($vector-length vec2))
	 (let loop ((i 0))
	   (or ($fx= i len1)
	       (and (?item= ($vector-ref vec1 i)
			    ($vector-ref vec2 i))
		    (loop ($fxadd1 i))))))))


;;;; coordinates

(define (coordinate? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

(define-argument-validation (coordinate who obj)
  (coordinate? obj)
  (assertion-violation who "expected array position coordinate as argument" obj))

(define-argument-validation (coordinate/false who obj)
  (or (not obj)
      (coordinate? obj))
  (assertion-violation who "expected false or array position coordinate as argument" obj))

(define-argument-validation (list-of-coordinates who obj)
  (and (list? obj)
       (for-all coordinate? obj))
  (assertion-violation who "expected list of array position coordinates as argument" obj))

(define-argument-validation (vector-of-coordinates who obj)
  (and (vector? obj)
       (vector-for-all coordinate? obj))
  (assertion-violation who "expected vector of array position coordinates as argument" obj))


;;;; array position

(define-record-type (:position make-:position position?)
  (nongenerative vicare:containers:arrays:position)
  (opaque #t)
  (sealed #t)
  (fields (immutable coordinates))
  (protocol
   (lambda (make-record)
     make-record)))

;;; --------------------------------------------------------------------
;;; predicates

(define (position-index? pos obj)
  (define who 'position-index?)
  (with-arguments-validation (who)
      ((position	pos))
    ($position-index? pos obj)))

(define ($position-index? pos obj)
  (and (fixnum? obj)
       ($fx>= obj 0)
       ($fx<= obj ($position-dimension pos))))

;;; --------------------------------------------------------------------
;;; arguments validation

(define-argument-validation (position who obj)
  (position? obj)
  (assertion-violation who "expected array position as argument" obj))

(define-argument-validation (position/false who obj)
  (or (not obj)
      (position? obj))
  (assertion-violation who "expected false or array position as argument" obj))

(define-argument-validation (position-index who pos obj)
  ($position-index? pos obj)
  (assertion-violation who "expected valid coordinate index for array position" pos obj))

(define-argument-validation (list-of-positions who obj)
  (and (list? obj)
       (for-all position? obj))
  (assertion-violation who "expected list of array position positions as argument" obj))

;;; --------------------------------------------------------------------
;;; constructors

(define (vector->position vec)
  (define who 'vector->position)
  (with-arguments-validation (who)
      ((vector-of-coordinates	vec))
    ($vector->position)))

(define ($vector->position vec)
  (make-:position vec))

(define (list->position ell)
  (define who 'list->position)
  (with-arguments-validation (who)
      ((list-of-coordinates	ell))
    ($vector->position)))

(define ($list->position ell)
  (make-:position (list->vector ell)))

(define (make-position dim)
  (define who 'make-position)
  (with-arguments-validation (who)
      ((number-of-dimensions	dim))
    ($make-position dim)))

(define ($make-position dim)
  (make-:position (make-vector dim 0)))

(define (position coord . coords)
  (list->position (cons coord coords)))

(define ($position coord . coords)
  ($list->position (cons coord coords)))

;;; --------------------------------------------------------------------

(define (position-dimension pos)
  (define who 'position-dimension)
  (with-arguments-validation (who)
      ((position	pos))
    ($position-dimension pos)))

(define ($position-dimension pos)
  ($vector-length ($:position-coordinates pos)))

;;; --------------------------------------------------------------------

(define (position-ref pos idx)
  (define who 'position-ref)
  (with-arguments-validation (who)
      ((position	pos)
       (position-index	pos idx))
    ($position-ref pos idx)))

(define ($position-ref pos idx)
  ($vector-ref ($:position-coordinates pos) idx))

;;; --------------------------------------------------------------------

(define (position-set! pos idx coord)
  (define who 'position-set!)
  (with-arguments-validation (who)
      ((position	pos)
       (position-index	pos idx)
       (coordinate	coord))
    ($position-set! pos idx coord)))

(define ($position-set! pos idx coord)
  ($vector-set! ($:position-coordinates pos) idx coord))

;;; --------------------------------------------------------------------

(define position=?
  (case-lambda
   ((pos)
    #t)
   ((pos1 pos2)
    (define who 'position=?)
    (with-arguments-validation (who)
	((position	pos1)
	 (position	pos2))
      ($position=? pos1 pos2)))
   ((pos0 . poses)
    (define who 'position=?)
    (with-arguments-validation (who)
	((position		pos0)
	 (list-of-positions	poses))
      ;;We know that if this branch is evaluated: POSES is not null.
      ($position=? pos0 ($car poses) ($cdr poses))))
   ))

(define $position=?
  (case-lambda
   ((pos1 pos2)
    ($vector=? $fx=
	       ($:position-coordinates pos1)
	       ($:position-coordinates pos2)))
   ((pos1 pos2 poses)
    (and ($position=? pos1 pos2)
	 (if (null? poses)
	     #t
	   ($position=? pos2 ($car poses) ($cdr poses)))))
   ))

;;; --------------------------------------------------------------------

(define (position-copy pos)
  (define who 'position-copy)
  (with-arguments-validation (who)
      ((position	pos))
    ($position-copy pos)))

(define ($position-copy pos)
  ($vector->position ($vector-copy ($:position-coordinates pos))))


;;;; miscellaneous utilities

(define-argument-validation (number-of-dimensions who obj)
  (coordinate? obj)
  (assertion-violation who "expected array position number of dimensions as argument" obj))


;;;; done

)

;;; end of file
