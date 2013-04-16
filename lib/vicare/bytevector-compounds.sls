;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: bytevector compounds
;;;Date: Tue Apr 16, 2013
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
(library (vicare bytevector-compounds)
  (export

    ;; data type
    bytevector-compound
    (rename (true-make-bytevector-compound	make-bytevector-compound))
    bytevector-compound?
    bytevector-compound.vicare-arguments-validation
    false-or-bytevector-compound.vicare-arguments-validation

    ;; queue operations
    bytevector-compound-empty?
    bytevector-compound-enqueue!	bytevector-compound-dequeue!
    )
  (import (vicare)
    (vicare syntactic-extensions)
    (vicare arguments validation)
    (prefix (vicare unsafe operations)
	    $))


;;;; helpers

(define-argument-validation (list-of-bytevectors who obj)
  (and (list? obj)
       (for-all bytevector? obj))
  (assertion-violation who "expected list of bytevectors as argument" obj))


;;;; type definitions

(define-struct-extended bytevector-compound
  (first-pair
		;List  representing the  queue  of  bytevectors in  this
		;compound; false if the compound is empty.
		;
		;Each  item in  the  queue  is a  pair  whose  car is  a
		;bytevector  and whose  cdr  is an  exact integer.   The
		;exact integer  is the index  of the first octet  in the
		;bytevector the total sequence  of octets represented by
		;this compound.
   last-pair
		;Last pair in the list of referenced by FIRST-PAIR.
   total-length
		;Exact  integer.  Represents  the total  length of  this
		;compound,  taking into  account  bytevectors that  have
		;already been dequeued.
   ))

(define ($bytevector-compound-empty? bc)
  (not ($bytevector-compound-first-pair bc)))

(define ($bytevector-compound-filled? bc)
  (and ($bytevector-compound-first-pair bc) #t))


;;;; constructors

(define (true-make-bytevector-compound . bvs)
  (define who 'make-bytevector-compound)
  (with-arguments-validation (who)
      ((list-of-bytevectors	bvs))
    (let ((bvs (filter (lambda (bv)
			 (not ($bytevector-empty? bv)))
		 bvs)))
      (receive (first-pair last-pair total-length)
	  (if (null? bvs)
	      (values #f #f 0)
	    (let recur ((bvs bvs)
			(idx 0))
	      (if (null? ($cdr bvs))
		  (let* ((last-pair	(cons ($car bvs) idx))
			 (total-length	(+ idx ($bytevector-length ($car bvs)))))
		    (values last-pair last-pair total-length))
		(receive (first-pair last-pair total-length)
		    (recur (cdr bvs) (+ idx ($bytevector-length ($car bvs))))
		  (values (cons ($car bvs) idx) last-pair total-length)))))
	(make-bytevector-compound first-pair last-pair total-length)))))


;;;; bytevector queue

(define (bytevector-compound-empty? bc)
  (define who 'bytevector-compound-empty?)
  (with-arguments-validation (who)
      ((bytevector-compound	bc))
    ($bytevector-compound-empty? bc)))

(define-argument-validation (bytevector-compound/filled who obj)
  (and (bytevector-compound? obj)
       ($bytevector-compound-filled? bc))
  (assertion-violation who
    "expected non-empty bytevector compound as argument" obj))

;;; --------------------------------------------------------------------

(define (bytevector-compound-enqueue! bc item)
  ;;Enqueue the bytevector ITEM into BC.  Return unspecified values.
  ;;
  (define who 'bytevector-compound-enqueue!)
  (with-arguments-validation (who)
      ((bytevector-compound	bc)
       (bytevector		item))
    (if ($bytevector-compound-filled? bc)
	(let* ((old-total-length	($bytevector-compound-total-length bc))
	       (new-total-length	(+ old-total-length ($bytevector-length item)))
	       (old-last-pair		($bytevector-compound-last-pair bc))
	       (new-last-pair		(cons item old-total-length)))
	  ($set-cdr! old-last-pair new-last-pair)
	  ($set-bytevector-compound-last-pair!    bc new-last-pair)
	  ($set-bytevector-compound-total-length! bc new-total-length))
      (let ((P (cons item 0)))
	($set-bytevector-compound-first-pair!   bc P)
	($set-bytevector-compound-last-pair!    bc P)
	($set-bytevector-compound-total-length! bc ($bytevector-length item))))))

(define (bytevector-compound-dequeue! bc)
  ;;Dequeue the next  bytevector from BC and return it;  return false if
  ;;BC is empty.
  ;;
  (define who 'bytevector-compound-dequeue!)
  (with-arguments-validation (who)
      ((bytevector-compound	bc))
    (cond (($bytevector-compound-empty? bc)
	   #f)
	  (else
	   (let ((head ($bytevector-compound-first-pair bc))
		 (tail ($bytevector-compound-last-pair  bc)))
	     (begin0
		 ($car head)
	       (let ((new-head ($cdr head)))
		 (if (null? new-head)
		     (begin
		       ($set-bytevector-compound-first-pair! bc #f)
		       ($set-bytevector-compound-last-pair!  bc #f))
		   ($set-bytevector-compound-first-pair! bc new-head)))))))))


;;;; done

)

;;; end of file
