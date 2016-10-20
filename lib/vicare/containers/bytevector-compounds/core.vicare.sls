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
;;;Copyright (C) 2013, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers bytevector-compounds core (0 4 2016 10 20))
  (options typed-language)
  (export

    ;; data type
    bytevector-compound
    make-bytevector-compound
    bytevector-compound?

    ;; inspection
    bytevector-compound-empty?		bytevector-compound-filled?
    bytevector-compound-length		bytevector-compound-total-length
    bytevector-compound-data

    ;; queue operations
    bytevector-compound-enqueue!	bytevector-compound-dequeue!

    ;; accessors and mutators
    bytevector-compound-u8-set!		bytevector-compound-u8-ref
    bytevector-compound-s8-set!		bytevector-compound-s8-ref

;;; --------------------------------------------------------------------

    ;; inspection
    $bytevector-compound-empty?		$bytevector-compound-filled?
    $bytevector-compound-length		$bytevector-compound-total-length
    $bytevector-compound-data

    ;; queue operations
    $bytevector-compound-enqueue!	$bytevector-compound-dequeue!

    ;; accessors and mutators
    $bytevector-compound-u8-set!	$bytevector-compound-u8-ref
    $bytevector-compound-s8-set!	$bytevector-compound-s8-ref

    #| end of EXPORT |# )
  (import (vicare)
    (vicare language-extensions syntaxes)
    (vicare arguments validation)
    (vicare unsafe operations)
    (vicare system $bytevectors)
    (only (vicare system $numerics)
	  $add-number-fixnum)
    (vicare language-extensions labels)
    (prefix (vicare platform words) words::))


;;;; label types

(define-label-type <word-u8>
  (parent <non-negative-fixnum>)
  (type-predicate
    (lambda (parent-predicate)
      (lambda (obj)
	(and (parent-predicate obj)
	     (words::word-u8? obj))))))

(define-label-type <word-s8>
  (parent <fixnum>)
  (type-predicate
    (lambda (parent-predicate)
      (lambda (obj)
	(and (parent-predicate obj)
	     (words::word-s8? obj))))))


;;;; type definitions

(define-record-type bytevector-compound
  (nongenerative vicare:bytevector-compounds:bytevector-compound)
  (fields (mutable first-pair)
		;List  representing the  queue  of  bytevectors in  this
		;compound; false if the compound is empty.
		;
		;Each  item in  the  queue  is a  pair  whose  car is  a
		;bytevector  and whose  cdr  is an  exact integer.   The
		;exact integer  is the index  of the first octet  in the
		;bytevector the total sequence  of octets represented by
		;this compound.
	  (mutable last-pair)
		;Last pair in the list of referenced by FIRST-PAIR.
	  (mutable length)
		;Exact integer.  Represents the current number of octets
		;in all the bytevectors.
	  (mutable total-length)
		;Exact  integer.  Represents  the total  length of  this
		;compound,  taking into  account  bytevectors that  have
		;already been dequeued.
	  )
  (protocol
   (lambda (maker)
     (lambda bvs
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
		       (let* ((last-pair	(list (cons ($car bvs) idx)))
			      (total-length	(+ idx ($bytevector-length ($car bvs)))))
			 (values last-pair last-pair total-length))
		     (receive (first-pair last-pair total-length)
			 (recur (cdr bvs) (+ idx ($bytevector-length ($car bvs))))
		       (values (cons (cons ($car bvs) idx) first-pair)
			       last-pair total-length)))))
	     (maker first-pair last-pair
		    total-length total-length))))))
   ))

(define ($bytevector-compound-empty? bvcom)
  (not ($bytevector-compound-first-pair bvcom)))

(define ($bytevector-compound-filled? bvcom)
  (and ($bytevector-compound-first-pair bvcom) #t))

(define ($bytevector-compound-incr-length! bvcom delta)
  ($bytevector-compound-length-set! bvcom (+ ($bytevector-compound-length bvcom) delta)))

(define ($bytevector-compound-decr-length! bvcom delta)
  ($bytevector-compound-length-set! bvcom (- ($bytevector-compound-length bvcom) delta)))


;;;; inspection

(define (bytevector-compound-empty? {bvcom bytevector-compound})
  ($bytevector-compound-empty? bvcom))

(define (bytevector-compound-filled? {bvcom bytevector-compound})
  ($bytevector-compound-filled? bvcom))

;;; --------------------------------------------------------------------

(define (bytevector-compound-data {bvcom bytevector-compound})
  ;;Return a list of bytevectors being the data in BVCOM.
  ;;
  ($bytevector-compound-data bvcom))

(define ($bytevector-compound-data bvcom)
  (let recur ((P ($bytevector-compound-first-pair bvcom)))
    (if (null? P)
	'()
      (cons ($caar P) (recur ($cdr P))))))

;;; --------------------------------------------------------------------

(define-argument-validation (bytevector-compound/filled who obj)
  (and (bytevector-compound? obj)
       ($bytevector-compound-filled? obj))
  (assertion-violation who
    "expected non-empty bytevector-compound as argument" obj))


;;;; bytevector queue

(define (bytevector-compound-enqueue! {bvcom bytevector-compound} {item <bytevector>})
  ;;Enqueue the bytevector ITEM into BVCOM.  Return unspecified values.
  ;;
  ($bytevector-compound-enqueue! bvcom item))

(define ($bytevector-compound-enqueue! bvcom item)
  (if ($bytevector-compound-filled? bvcom)
      (let* ((old-length	($bytevector-compound-length bvcom))
	     (new-length	(+ old-length ($bytevector-length item)))
	     (old-total-length	($bytevector-compound-total-length bvcom))
	     (new-total-length	(+ old-total-length ($bytevector-length item)))
	     (old-last-pair	($bytevector-compound-last-pair bvcom))
	     (new-last-pair	(list (cons item old-total-length))))
	($set-cdr! old-last-pair new-last-pair)
	($bytevector-compound-last-pair-set!    bvcom new-last-pair)
	($bytevector-compound-length-set!       bvcom new-length)
	($bytevector-compound-total-length-set! bvcom new-total-length))
    (let ((Q (list (cons item 0))))
      ($bytevector-compound-first-pair-set!   bvcom Q)
      ($bytevector-compound-last-pair-set!    bvcom Q)
      ($bytevector-compound-length-set!       bvcom ($bytevector-length item))
      ($bytevector-compound-total-length-set! bvcom ($bytevector-length item)))))

;;; --------------------------------------------------------------------

(define (bytevector-compound-dequeue! {bvcom bytevector-compound})
  ;;Dequeue the next  bytevector from BVCOM and return  it; return false
  ;;if BVCOM is empty.
  ;;
  ($bytevector-compound-dequeue! bvcom))

(define ($bytevector-compound-dequeue! bvcom)
  (cond (($bytevector-compound-empty? bvcom)
	 #f)
	(else
	 (let ((head ($bytevector-compound-first-pair bvcom))
	       (tail ($bytevector-compound-last-pair  bvcom)))
	   (begin0
	       ($caar head)
	     ($bytevector-compound-decr-length! bvcom ($bytevector-length ($caar head)))
	     (let ((new-head ($cdr head)))
	       (if (null? new-head)
		   (begin
		     ($bytevector-compound-first-pair-set! bvcom #f)
		     ($bytevector-compound-last-pair-set!  bvcom #f))
		 ($bytevector-compound-first-pair-set! bvcom new-head))))))))


;;;; accessors and mutators, u8

(define (bytevector-compound-u8-set! {bvcom bytevector-compound} {idx <non-negative-exact-integer>} {val <word-u8>})
  ($bytevector-compound-u8-set! bvcom idx val))

(define ($bytevector-compound-u8-set! bvcom idx val)
  (define who 'bytevector-compound-u8-set!)
  (if (>= idx ($bytevector-compound-total-length bvcom))
      (assertion-violation who
	(string-append "index out of range for bytevector-compound of length "
		       (number->string ($bytevector-compound-total-length bvcom)))
	idx)
    (let loop ((pairs ($bytevector-compound-first-pair bvcom)))
      (if (< idx (+ ($cdar pairs)
		    ($bytevector-length ($caar pairs))))
	  ($bytevector-u8-set! ($caar pairs) (- idx ($cdar pairs)) val)
	(loop (cdr pairs))))))

;;; --------------------------------------------------------------------

(define (bytevector-compound-u8-ref {bvcom bytevector-compound} {idx <non-negative-exact-integer>})
  ($bytevector-compound-u8-ref bvcom idx))

(define ($bytevector-compound-u8-ref bvcom idx)
  (define who 'bytevector-compound-u8-ref)
  (if (>= idx ($bytevector-compound-total-length bvcom))
      (assertion-violation who
	(string-append "index out of range for bytevector-compound of length "
		       (number->string ($bytevector-compound-total-length bvcom)))
	idx)
    (let loop ((pairs ($bytevector-compound-first-pair bvcom)))
      (if (< idx ($add-number-fixnum ($cdar pairs)
				     ($bytevector-length ($caar pairs))))
	  ($bytevector-u8-ref ($caar pairs) (- idx ($cdar pairs)))
	(loop ($cdr pairs))))))


;;;; accessors and mutators, s8

(define (bytevector-compound-s8-set! {bvcom bytevector-compound} {idx <non-negative-exact-integer>} {val <word-s8>})
  ($bytevector-compound-s8-set! bvcom idx val))

(define ($bytevector-compound-s8-set! bvcom idx val)
  (define who 'bytevector-compound-s8-set!)
  (if (>= idx ($bytevector-compound-total-length bvcom))
      (assertion-violation who
	(string-append "index out of range for bytevector-compound of length "
		       (number->string ($bytevector-compound-total-length bvcom)))
	idx)
    (let loop ((pairs ($bytevector-compound-first-pair bvcom)))
      (if (< idx (+ ($cdar pairs)
		    ($bytevector-length ($caar pairs))))
	  (bytevector-s8-set! ($caar pairs) (- idx ($cdar pairs)) val)
	(loop (cdr pairs))))))

;;; --------------------------------------------------------------------

(define (bytevector-compound-s8-ref {bvcom bytevector-compound} {idx <non-negative-exact-integer>})
  ($bytevector-compound-s8-ref bvcom idx))

(define ($bytevector-compound-s8-ref bvcom idx)
  (define who 'bytevector-compound-s8-ref)
  (if (>= idx ($bytevector-compound-total-length bvcom))
      (assertion-violation who
	(string-append "index out of range for bytevector-compound of length "
		       (number->string ($bytevector-compound-total-length bvcom)))
	idx)
    (let loop ((pairs ($bytevector-compound-first-pair bvcom)))
      (if (< idx (+ ($cdar pairs)
		    ($bytevector-length ($caar pairs))))
	  ($bytevector-s8-ref ($caar pairs) (- idx ($cdar pairs)))
	(loop (cdr pairs))))))


;;;; done

#| end of library |# )

;;; end of file
