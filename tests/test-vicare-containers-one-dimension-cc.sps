;;;
;;;Part of: Vicare Scheme
;;;Contents: test for one-dimension-cc library
;;;Date: Wed Jun 10, 2009
;;;
;;;Abstract
;;;
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare checks)
  (vicare containers one-dimension-cc))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: one-dimension-cc\n")


;;;; helpers

(define type
  (%make-type-descriptor integer? = < <= min max
			 (lambda (x range) ; item-prev
			   (let ((x (- x 1)))
			     (if range
				 (and (<= (car range) x)
				      x)
			       x)))
			 (lambda (x range) ; item-next
			   (let ((x (+ 1 x)))
			     (if range
				 (and (<= x (cdr range))
				      x)
			       x)))
			 (lambda (a b) (+ 1 (- a b))) ; minus
			 (lambda (x) x)))		 ; copy


;;;; range wrappers for integers

(define (make-range a b)
  (%make-range type a b))

(define (range-copy a)
  (%range-copy type a))

(define (range? a)
  (%range? type a))

(define (range-contains? range obj)
  (%range-contains? type range obj))

(define (range-length range)
  (%range-length type range))

(define (range=? range-a range-b)
  (%range=? type range-a range-b))

(define (range<? range-a range-b)
  (%range<? type range-a range-b))

(define (range<=? range-a range-b)
  (%range<=? type range-a range-b))

(define (range-contiguous? range-a range-b)
  (%range-contiguous? type range-a range-b))

(define (range-superset? range-a range-b)
  (%range-superset? type range-a range-b))

(define (range-superset?/strict range-a range-b)
  (%range-superset?/strict type range-a range-b))

(define (range-start<? range-a range-b)
  (%range-start<? type range-a range-b))

(define (range-start<=? range-a range-b)
  (%range-start<=? type range-a range-b))

(define (range-overlapping? range-a range-b)
  (%range-overlapping? type range-a range-b))

(define (range-concatenate range-a range-b)
  (%range-concatenate type range-a range-b))

(define (range-intersection range-a range-b)
  (%range-intersection type range-a range-b))

(define (range-union range-a range-b)
  (%range-union type range-a range-b))

(define (range-difference range-a range-b)
  (%range-difference type range-a range-b))

(define (range-in-first-only range-a range-b)
  (%range-in-first-only type range-a range-b))

(define (range-for-each proc range)
  (%range-for-each type proc range))

(define (range-every proc range)
  (%range-every type proc range))

(define (range-any proc range)
  (%range-any type proc range))

(define (range-fold kons knil range)
  (%range-fold type kons knil range))

(define (range->list range)
  (%range->list type range))


;;;; domain wrappers for integers

(define (make-domain . args)
  (apply %make-domain type args))

(define (domain-copy domain)
  (%domain-copy type domain))

(define (domain-add-item domain obj)
  (%domain-add-item type domain obj))

(define (domain-add-range domain new-range)
  (%domain-add-range type domain new-range))

(define (domain? domain)
  (%domain? type domain))

(define (domain-size domain)
  (%domain-size type domain))

(define domain-empty? %domain-empty?)

(define (domain-contains? domain obj)
  (%domain-contains? type domain obj))

(define (domain=? domain-a domain-b)
  (%domain=? type domain-a domain-b))

(define (domain<? domain-a domain-b)
  (%domain<? type domain-a domain-b))

(define (domain-superset? domain-a domain-b)
  (%domain-superset? type domain-a domain-b))

(define (domain-superset?/strict domain-a domain-b)
  (%domain-superset?/strict type domain-a domain-b))

(define (domain-intersection domain-a domain-b)
  (%domain-intersection type domain-a domain-b))

(define (domain-union domain-a domain-b)
  (%domain-union type domain-a domain-b))

(define (domain-difference domain-a domain-b)
  (%domain-difference type domain-a domain-b))

(define (domain-complement domain universe)
  (%domain-complement type domain universe))

(define (domain-for-each proc domain)
  (%domain-for-each type proc domain))

(define (domain-every proc domain)
  (%domain-every type proc domain))

(define (domain-any proc domain)
  (%domain-any type proc domain))

(define (domain-fold kons knil domain)
  (%domain-fold type kons knil domain))

(define (domain->list domain)
  (%domain->list type domain))


(parameterise ((check-test-name	'range-constructors))

  ;;Correct: start < last
  (check (make-range 10 20) => '(10 . 20))

  ;;Correct: start = last
  (check (make-range 10 10) => '(10 . 10))

  ;;Wrong: start > last
  (check (guard (exc ((assertion-violation? exc) #t)) (make-range 123 66)) => #t)

  )


(parameterise ((check-test-name	'range-inspection))

  (check (range-length (make-range 10 20)) => 11)
  (check (range-length (make-range 10 11)) => 2)
  (check (range-length (make-range 10 10)) => 1)

  (check (range-contains? (make-range 10 20) 10) => #t) ; start
  (check (range-contains? (make-range 10 20) 20) => #t) ; last
  (check (range-contains? (make-range 10 20) 15) => #t) ; inner
  (check (range-contains? (make-range 10 20)  5) => #f) ; lesser
  (check (range-contains? (make-range 10 20) 30) => #f) ; greater

  (check (range-superset? (make-range 40 60) (make-range 40 60)) => #t) ; equal
  (check (range-superset? (make-range 40 60) (make-range 50 60)) => #t) ; same last, start <
  (check (range-superset? (make-range 50 60) (make-range 40 60)) => #f) ; same last, start >
  (check (range-superset? (make-range 40 50) (make-range 40 60)) => #f) ; same start, last <
  (check (range-superset? (make-range 40 60) (make-range 40 50)) => #t) ; same start, last >
  (check (range-superset? (make-range 10 20) (make-range 30 40)) => #f) ; disjoint <
  (check (range-superset? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range-superset? (make-range 10 30) (make-range 20 40)) => #f) ; overlapping, start <
  (check (range-superset? (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range-superset? (make-range 10 20) (make-range 20 30)) => #f) ; overlapping, last=start
  (check (range-superset? (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=last
  (check (range-superset? (make-range 10 20) (make-range 21 30)) => #f) ; contiguous <
  (check (range-superset? (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range-superset? (make-range 10 40) (make-range 20 30)) => #t) ; 1st includes 2nd
  (check (range-superset? (make-range 20 30) (make-range 10 40)) => #f) ; 2nd includes 1st

  (check (range-superset?/strict (make-range 40 60) (make-range 40 60)) => #f) ; equal
  (check (range-superset?/strict (make-range 40 60) (make-range 50 60)) => #t) ; same last, start <
  (check (range-superset?/strict (make-range 50 60) (make-range 40 60)) => #f) ; same last, start >
  (check (range-superset?/strict (make-range 40 50) (make-range 40 60)) => #f) ; same start, last <
  (check (range-superset?/strict (make-range 40 60) (make-range 40 50)) => #t) ; same start, last >
  (check (range-superset?/strict (make-range 10 20) (make-range 30 40)) => #f) ; disjoint <
  (check (range-superset?/strict (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range-superset?/strict (make-range 10 30) (make-range 20 40)) => #f) ; overlapping, start <
  (check (range-superset?/strict (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range-superset?/strict (make-range 10 20) (make-range 20 30)) => #f) ; overlapping, last=start
  (check (range-superset?/strict (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=past
  (check (range-superset?/strict (make-range 10 20) (make-range 21 30)) => #f) ; contiguous <
  (check (range-superset?/strict (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range-superset?/strict (make-range 10 40) (make-range 20 30)) => #t) ; 1st includes 2nd
  (check (range-superset?/strict (make-range 20 30) (make-range 10 40)) => #f) ; 2nd includes 1st

  )


(parameterise ((check-test-name	'range-comparison))

  (check (range=? (make-range 40 60) (make-range 40 60)) => #t)	; equal
  (check (range=? (make-range 40 60) (make-range 50 60)) => #f)	; same last, start <
  (check (range=? (make-range 50 60) (make-range 40 60)) => #f)	; same last, start >
  (check (range=? (make-range 40 50) (make-range 40 60)) => #f) ; same start, last <
  (check (range=? (make-range 40 60) (make-range 40 50)) => #f) ; same start, last >
  (check (range=? (make-range 10 20) (make-range 30 40)) => #f) ; disjoint <
  (check (range=? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range=? (make-range 10 30) (make-range 20 40)) => #f) ; overlapping, start <
  (check (range=? (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range=? (make-range 10 20) (make-range 20 30)) => #f) ; overlapping, last=start
  (check (range=? (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=last
  (check (range=? (make-range 10 20) (make-range 21 30)) => #f) ; contiguous <
  (check (range=? (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range=? (make-range 10 40) (make-range 20 30)) => #f) ; first includes second
  (check (range=? (make-range 20 30) (make-range 10 40)) => #f) ; second includes first

  (check (range<? (make-range 40 60) (make-range 40 60)) => #f)	; equal
  (check (range<? (make-range 40 60) (make-range 50 60)) => #f)	; same last, start <
  (check (range<? (make-range 50 60) (make-range 40 60)) => #f)	; same last, start >
  (check (range<? (make-range 40 50) (make-range 40 60)) => #f) ; same start, last <
  (check (range<? (make-range 40 60) (make-range 40 50)) => #f) ; same start, last >
  (check (range<? (make-range 10 20) (make-range 30 40)) => #t) ; disjoint <
  (check (range<? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range<? (make-range 10 30) (make-range 20 40)) => #f) ; overlapping, start <
  (check (range<? (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range<? (make-range 10 20) (make-range 20 30)) => #f) ; overlapping, last=start
  (check (range<? (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=last
  (check (range<? (make-range 10 20) (make-range 21 30)) => #t) ; contiguous <
  (check (range<? (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range<? (make-range 10 40) (make-range 20 30)) => #f) ; first includes second
  (check (range<? (make-range 20 30) (make-range 10 40)) => #f) ; second includes first

  (check (range-start<? (make-range 40 60) (make-range 40 60)) => #f) ; equal
  (check (range-start<? (make-range 40 60) (make-range 50 60)) => #t) ; same last, start <
  (check (range-start<? (make-range 50 60) (make-range 40 60)) => #f) ; same last, start >
  (check (range-start<? (make-range 40 50) (make-range 40 60)) => #f) ; same start, last <
  (check (range-start<? (make-range 40 60) (make-range 40 50)) => #f) ; same start, last >
  (check (range-start<? (make-range 10 20) (make-range 30 40)) => #t) ; disjoint <
  (check (range-start<? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range-start<? (make-range 10 30) (make-range 20 40)) => #t) ; overlapping, start <
  (check (range-start<? (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range-start<? (make-range 10 20) (make-range 20 30)) => #t) ; overlapping, last=start
  (check (range-start<? (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=last
  (check (range-start<? (make-range 10 20) (make-range 21 30)) => #t) ; contiguous <
  (check (range-start<? (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range-start<? (make-range 10 40) (make-range 20 30)) => #t) ; first includes second
  (check (range-start<? (make-range 20 30) (make-range 10 40)) => #f) ; second includes first

  (check (range-start<=? (make-range 40 60) (make-range 40 60)) => #t) ; equal
  (check (range-start<=? (make-range 40 60) (make-range 50 60)) => #t) ; same last, start <
  (check (range-start<=? (make-range 50 60) (make-range 40 60)) => #f) ; same last, start >
  (check (range-start<=? (make-range 40 50) (make-range 40 60)) => #t) ; same start, last <
  (check (range-start<=? (make-range 40 60) (make-range 40 50)) => #t) ; same start, last >
  (check (range-start<=? (make-range 10 20) (make-range 30 40)) => #t) ; disjoint <
  (check (range-start<=? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range-start<=? (make-range 10 30) (make-range 20 40)) => #t) ; overlapping, start <
  (check (range-start<=? (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range-start<=? (make-range 10 20) (make-range 20 30)) => #t) ; overlapping, last=start
  (check (range-start<=? (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=last
  (check (range-start<=? (make-range 10 20) (make-range 21 30)) => #t) ; contiguous <
  (check (range-start<=? (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range-start<=? (make-range 10 40) (make-range 20 30)) => #t) ; first includes second
  (check (range-start<=? (make-range 20 30) (make-range 10 40)) => #f) ; second includes first

  (check (range-contiguous? (make-range 40 60) (make-range 40 60)) => #f) ; equal
  (check (range-contiguous? (make-range 40 60) (make-range 50 60)) => #f) ; same last, start <
  (check (range-contiguous? (make-range 50 60) (make-range 40 60)) => #f) ; same last, start >
  (check (range-contiguous? (make-range 40 50) (make-range 40 60)) => #f) ; same start, last <
  (check (range-contiguous? (make-range 40 60) (make-range 40 50)) => #f) ; same start, last >
  (check (range-contiguous? (make-range 10 20) (make-range 30 40)) => #f) ; disjoint <
  (check (range-contiguous? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range-contiguous? (make-range 10 30) (make-range 20 40)) => #f) ; overlapping, start <
  (check (range-contiguous? (make-range 20 40) (make-range 10 30)) => #f) ; overlapping, start >
  (check (range-contiguous? (make-range 10 20) (make-range 20 30)) => #f) ; overlapping, last=start
  (check (range-contiguous? (make-range 20 30) (make-range 10 20)) => #f) ; overlapping, start=last
  (check (range-contiguous? (make-range 10 20) (make-range 21 30)) => #t) ; contiguous <
  (check (range-contiguous? (make-range 21 30) (make-range 10 20)) => #t) ; contiguous >
  (check (range-contiguous? (make-range 10 40) (make-range 20 30)) => #f) ; first includes second
  (check (range-contiguous? (make-range 20 30) (make-range 10 40)) => #f) ; second includes first

  (check (range-overlapping? (make-range 40 60) (make-range 40 60)) => #t) ; equal
  (check (range-overlapping? (make-range 40 60) (make-range 50 60)) => #t) ; same last, start <
  (check (range-overlapping? (make-range 50 60) (make-range 40 60)) => #t) ; same last, start >
  (check (range-overlapping? (make-range 40 50) (make-range 40 60)) => #t) ; same start, last <
  (check (range-overlapping? (make-range 40 60) (make-range 40 50)) => #t) ; same start, last >
  (check (range-overlapping? (make-range 10 20) (make-range 30 40)) => #f) ; disjoint <
  (check (range-overlapping? (make-range 30 40) (make-range 10 20)) => #f) ; disjoint >
  (check (range-overlapping? (make-range 10 30) (make-range 20 40)) => #t) ; overlapping, start <
  (check (range-overlapping? (make-range 20 40) (make-range 10 30)) => #t) ; overlapping, start >
  (check (range-overlapping? (make-range 10 20) (make-range 20 30)) => #t) ; overlapping, last=start
  (check (range-overlapping? (make-range 20 30) (make-range 10 20)) => #t) ; overlapping, start=last
  (check (range-overlapping? (make-range 10 20) (make-range 21 30)) => #f) ; contiguous <
  (check (range-overlapping? (make-range 21 30) (make-range 10 20)) => #f) ; contiguous >
  (check (range-overlapping? (make-range 10 40) (make-range 20 30)) => #t) ; first includes second
  (check (range-overlapping? (make-range 20 30) (make-range 10 40)) => #t) ; second includes first

  )


(parameterise ((check-test-name	'range-set-operation))

  (let ((R make-range)
	(= range=?)
	(F range-concatenate))
    (check (F (R 40 60) (R 40 60)) (=> =) (R 40 60)) ; equal
    (check (F (R 40 60) (R 50 60)) (=> =) (R 40 60)) ; same last, start <
    (check (F (R 50 60) (R 40 60)) (=> =) (R 40 60)) ; same last, start >
    (check (F (R 40 50) (R 40 60)) (=> =) (R 40 60)) ; same start, last <
    (check (F (R 40 60) (R 40 50)) (=> =) (R 40 60)) ; same start, last >
    (check (F (R 10 20) (R 30 40)) (=> =) (R 10 40)) ; disjoint <
    (check (F (R 30 40) (R 10 20)) (=> =) (R 10 40)) ; disjoint >
    (check (F (R 10 30) (R 20 40)) (=> =) (R 10 40)) ; overlapping, start <
    (check (F (R 20 40) (R 10 30)) (=> =) (R 10 40)) ; overlapping, start >
    (check (F (R 10 20) (R 20 30)) (=> =) (R 10 30)) ; overlapping, last=start
    (check (F (R 20 30) (R 10 20)) (=> =) (R 10 30)) ; overlapping, start=last
    (check (F (R 10 20) (R 21 30)) (=> =) (R 10 30)) ; contiguous <
    (check (F (R 21 30) (R 10 20)) (=> =) (R 10 30)) ; contiguous >
    (check (F (R 10 40) (R 20 30)) (=> =) (R 10 40)) ; first includes second
    (check (F (R 20 30) (R 10 40)) (=> =) (R 10 40)) ; second includes first
    )

  (let* ((R make-range)
	 (= equal?))
    (let-syntax ((F (syntax-rules ()
		      ((_ ?a ?b ?expected-head ?expected-tail)
		       (check
			   (let-values (((head tail) (range-union ?a ?b))) (list head tail))
			 (=> =) (list ?expected-head ?expected-tail))))))
      (F (R 40 60) (R 40 60)	#f (R 40 60)) ; equal
      (F (R 40 60) (R 50 60)	#f (R 40 60)) ; same last, start <
      (F (R 50 60) (R 40 60)	#f (R 40 60)) ; same last, start >
      (F (R 40 50) (R 40 60)	#f (R 40 60)) ; same start, last <
      (F (R 40 60) (R 40 50)	#f (R 40 60)) ; same start, last >
      (F (R 10 20) (R 30 40)	(R 10 20) (R 30 40)) ; disjoint <
      (F (R 30 40) (R 10 20)	(R 10 20) (R 30 40)) ; disjoint >
      (F (R 10 30) (R 20 40)	#f (R 10 40)) ; overlapping, start <
      (F (R 20 40) (R 10 30)	#f (R 10 40)) ; overlapping, start >
      (F (R 10 20) (R 20 30)	#f (R 10 30)) ; overlapping, last=start
      (F (R 20 30) (R 10 20)	#f (R 10 30)) ; overlapping, start=last
      (F (R 10 20) (R 21 30)	#f (R 10 30)) ; contiguous <
      (F (R 21 30) (R 10 20)	#f (R 10 30)) ; contiguous >
      (F (R 10 40) (R 20 30)	#f (R 10 40)) ; 1st includes 2nd
      (F (R 20 30) (R 10 40)	#f (R 10 40)) ; 2nd includes 1st
      ))

  (let* ((R make-range)
	 (= equal?))
    (let-syntax ((F (syntax-rules ()
		      ((_ ?a ?b ?expected-head ?expected-tail)
		       (check
			   (let-values (((head tail) (range-difference ?a ?b))) (list head tail))
			 (=> =) (list ?expected-head ?expected-tail))))))
      (F (R 40 60) (R 40 60)	#f #f)	      ; equal
      (F (R 40 60) (R 50 60)	#f (R 40 49)) ; same last, start <
      (F (R 50 60) (R 40 60)	#f (R 40 49)) ; same last, start >
      (F (R 40 50) (R 40 60)	#f (R 51 60)) ; same start, last <
      (F (R 40 60) (R 40 50)	#f (R 51 60)) ; same start, last >
      (F (R 10 20) (R 30 40)	(R 10 20) (R 30 40)) ; disjoint <
      (F (R 30 40) (R 10 20)	(R 10 20) (R 30 40)) ; disjoint >
      (F (R 10 30) (R 20 40)	(R 10 19) (R 31 40)) ; overlapping, start <
      (F (R 20 40) (R 10 30)	(R 10 19) (R 31 40)) ; overlapping, start >
      (F (R 10 20) (R 20 30)	(R 10 19) (R 21 30)) ; overlapping, last=start
      (F (R 20 30) (R 10 20)	(R 10 19) (R 21 30)) ; overlapping, start=last
      (F (R 10 20) (R 21 30)	#f (R 10 30)) ; contiguous <
      (F (R 21 30) (R 10 20)	#f (R 10 30)) ; contiguous >
      (F (R 10 40) (R 20 30)	(R 10 19) (R 31 40)) ; first includes second
      (F (R 20 30) (R 10 40)	(R 10 19) (R 31 40)) ; second includes first
      ))

  (let* ((R make-range)
	 (= equal?))
    (let-syntax ((F (syntax-rules ()
		      ((_ ?a ?b ?expected-head ?expected-tail)
		       (check
			   (let-values (((head tail)
					 (range-in-first-only ?a ?b)))
			     (list head tail))
			 (=> =) (list ?expected-head ?expected-tail))))))
      (F (R 40 60) (R 40 60)	#f #f)	      ; equal
      (F (R 40 60) (R 50 60)	(R 40 49) #f) ; same last, start <
      (F (R 50 60) (R 40 60)	#f #f)	      ; same last, start >
      (F (R 40 50) (R 40 60)	#f #f)	      ; same start, last <
      (F (R 40 60) (R 40 50)	#f (R 51 60)) ; same start, last >
      (F (R 10 20) (R 30 40)	#f (R 10 20)) ; disjoint <
      (F (R 30 40) (R 10 20)	#f (R 30 40)) ; disjoint >
      (F (R 10 30) (R 20 40)	(R 10 19) #f) ; overlapping, start <
      (F (R 20 40) (R 10 30)	#f (R 31 40)) ; overlapping, start >
      (F (R 10 20) (R 20 30)	(R 10 19) #f) ; overlapping, last=start
      (F (R 20 30) (R 10 20)	#f (R 21 30)) ; overlapping, start=last
      (F (R 10 20) (R 21 30)	#f (R 10 20)) ; contiguous <
      (F (R 21 30) (R 10 20)	#f (R 21 30)) ; contiguous >
      (F (R 10 40) (R 20 30)	(R 10 19) (R 31 40)) ; first includes second
      (F (R 20 30) (R 10 40)	#f #f) ; second includes first
      ))

  )


(parameterise ((check-test-name	'range-list-operation))

  (check (cadr (with-result (range-for-each add-result (make-range 10 15)))) => '(10 11 12 13 14 15))
  (check (cadr (with-result (range-for-each add-result (make-range 10 11)))) => '(10 11))
  (check (cadr (with-result (range-for-each add-result (make-range 10 10)))) => '(10))

  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 10 20)) => #t)
  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 10 15)) => #t)
  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 15 20)) => #t)
  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range  5 15)) => #f)
  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 15 25)) => #f)
  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range  0  5)) => #f)
  (check (range-every (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 30 40)) => #f)

  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 10 20)) => #t)
  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 10 15)) => #t)
  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 15 20)) => #t)
  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range  5 15)) => #t)
  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 15 25)) => #t)
  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range  0  5)) => #f)
  (check (range-any (lambda (x) (and (<= 10 x) (<= x 20))) (make-range 30 40)) => #f)

  (check (range-fold (lambda (x knil) (cons x knil)) '() (make-range 10 13)) => '(13 12 11 10))
  (check (range-fold (lambda (x knil) (cons x knil)) '() (make-range 10 11)) => '(11 10))
  (check (range-fold (lambda (x knil) (cons x knil)) '() (make-range 10 10)) => '(10))

  (check (range->list (make-range 10 15)) => (reverse '(10 11 12 13 14 15)))
  (check (range->list (make-range 10 11)) => '(11 10))
  (check (range->list (make-range 10 10)) => '(10))

  )


(parameterise ((check-test-name	'domain-predicate))

  ;;Correct: empty list is the empty domain.
  (check (domain? '()) => #t)

  ;;Correct: domain with single range.
  (check (domain? '((10 . 20))) => #t)

  ;;Correct: domain with two non-overlapping, non-contiguous ranges.
  (check (domain? '((10 . 20) (30 . 40))) => #t)
  (check (domain? '((10 . 20) (22 . 40))) => #t)

  ;;Wrong: domain with two contiguous ranges.
  (check (domain? '((10 . 20) (21 . 30))) => #f)
  (check (domain? '((10 . 20) (30 . 40) (41 . 60))) => #f)

  ;;Wrong: domain with two overlapping ranges.
  (check (domain? '((10 . 20) (20 . 30))) => #f)
  (check (domain? '((10 . 30) (20 . 40))) => #f)
  (check (domain? `((10 . 20) (30 . 40) (40 . 60))) => #f)

  ;;Correct: domain with three non-overlapping, non-contiguous ranges.
  (check (domain? '((10 . 20) (30 . 40) (50 . 60))) => #t)
  (check (domain? '((10 . 20) (30 . 40) (42 . 60))) => #t)

  ;;Wrong: domain with two contiguous ranges.
  (check (domain? `((10 . 20) (30 . 40) (41 . 60))) => #f)

  ;;Wrong: domain with two overlapping ranges.
  (check (domain? `((10 . 20) (30 . 50) (40 . 60))) => #f)

  )


(parameterise ((check-test-name	'domain-constructor))

  (check (make-domain) => '())
  (check (make-domain 65) => '((65 . 65)))
  (check (make-domain 65 65) => '((65 . 65)))
  (check (make-domain 65 66) => '((65 . 66)))
  (check (make-domain 66 65) => '((65 . 66)))
  (check (make-domain 65 67) => '((65 . 65) (67 . 67)))
  (check (make-domain 67 65) => '((65 . 65) (67 . 67)))
  (check (make-domain 65 65 65) => '((65 . 65)))
  (check (make-domain 65 66 65) => '((65 . 66)))
  (check (make-domain 65 66 67) => '((65 . 67)))
  (check (make-domain 66 65 67) => '((65 . 67)))
  (check (make-domain 67 65 66) => '((65 . 67)))
  (check (make-domain 66 67 65) => '((65 . 67)))
  (check (make-domain 65 65 66 65) => '((65 . 66)))

  (check (make-domain '(10 . 11)) => '((10 . 11)))
  (check (make-domain '(40 . 60) '(50 . 60)) => '((40 . 60))) ; same last, start <
  (check (make-domain '(50 . 60) '(40 . 60)) => '((40 . 60))) ; same last, start >
  (check (make-domain '(40 . 50) '(40 . 60)) => '((40 . 60))) ; same start, last <
  (check (make-domain '(40 . 60) '(40 . 50)) => '((40 . 60))) ; same start, last >
  (check (make-domain '(10 . 20) '(30 . 40)) => '((10 . 20) (30 . 40))) ; disjoint <
  (check (make-domain '(30 . 40) '(10 . 20)) => '((10 . 20) (30 . 40))) ; disjoint >
  (check (make-domain '(10 . 30) '(20 . 40)) => '((10 . 40))) ; overlapping, start <
  (check (make-domain '(20 . 40) '(10 . 30)) => '((10 . 40))) ; overlapping, start >
  (check (make-domain '(10 . 20) '(20 . 30)) => '((10 . 30))) ; overlapping, last=start
  (check (make-domain '(20 . 30) '(10 . 20)) => '((10 . 30))) ; overlapping, start=last
  (check (make-domain '(10 . 20) '(21 . 30)) => '((10 . 30))) ; contiguous <
  (check (make-domain '(21 . 30) '(10 . 20)) => '((10 . 30))) ; contiguous >
  (check (make-domain '(10 . 40) '(20 . 30)) => '((10 . 40))) ; first includes second
  (check (make-domain '(20 . 30) '(10 . 40)) => '((10 . 40))) ; second includes first

  (check (make-domain 19 30 '(20 . 30)) => '((19 . 30)))
  (check (make-domain 19 31 '(20 . 30)) => '((19 . 31)))
  (check (make-domain 19 '(20 . 30) 31) => '((19 . 31)))
  (check (make-domain '(20 . 30) 19 31) => '((19 . 31)))
  (check (make-domain '(20 . 30) 31 19) => '((19 . 31)))

  )


(parameterise ((check-test-name	'domain-inspection))

  (check (domain-empty? (make-domain)) => #t)
  (check (domain-empty? (make-domain 65)) => #f)

  (check (domain-size (make-domain)) => 0)
  (check (domain-size (make-domain 65)) => 1)
  (check (domain-size (make-domain 65 67)) => 2)
  (check (domain-size (make-domain '(65 . 68) '(70 . 72))) => (+ 4 3))

  (check (domain-contains? (make-domain '(20 . 40)) 20) => #t)
  (check (domain-contains? (make-domain '(20 . 40)) 40) => #t)
  (check (domain-contains? (make-domain '(20 . 40)) 19) => #f)
  (check (domain-contains? (make-domain '(20 . 40)) 41) => #f)
  (check (domain-contains? (make-domain '(20 . 40)) 10) => #f)
  (check (domain-contains? (make-domain '(20 . 40)) 50) => #f)
  (check (domain-contains? (make-domain '(20 . 40)) 30) => #t)

;;; --------------------------------------------------------------------

  (check (domain-superset? (make-domain '(10 . 20))
			   (make-domain '(10 . 20))) => #t) ; equal
  (check (domain-superset? (make-domain '(10 . 20) '(30 . 40))
			   (make-domain '(10 . 20) '(30 . 40))) => #t) ; equal
  (check (domain-superset? (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
			   (make-domain '(10 . 20) '(30 . 40) '(50 . 60))) => #t) ; equal

  (check (domain-superset? (make-domain '(40 . 60))
			   (make-domain '(50 . 60))) => #t) ; same last, start <
  (check (domain-superset? (make-domain '(50 . 60))
			   (make-domain '(40 . 60))) => #f) ; same last, start >
  (check (domain-superset? (make-domain '(40 . 50))
			   (make-domain '(40 . 60))) => #f) ; same start, last <
  (check (domain-superset? (make-domain '(40 . 60))
			   (make-domain '(40 . 50))) => #t) ; same start, last >
  (check (domain-superset? (make-domain '(10 . 20))
			   (make-domain '(30 . 40))) => #f) ; disjoint <
  (check (domain-superset? (make-domain '(30 . 40))
			   (make-domain '(10 . 20))) => #f) ; disjoint >
  (check (domain-superset? (make-domain '(10 . 30))
			   (make-domain '(20 . 40))) => #f) ; overlapping, start <
  (check (domain-superset? (make-domain '(20 . 40))
			   (make-domain '(10 . 30))) => #f) ; overlapping, start >
  (check (domain-superset? (make-domain '(10 . 20))
			   (make-domain '(20 . 30))) => #f) ; overlapping, last=start
  (check (domain-superset? (make-domain '(20 . 30))
			   (make-domain '(10 . 20))) => #f) ; overlapping, start=last
  (check (domain-superset? (make-domain '(10 . 20))
			   (make-domain '(21 . 30))) => #f) ; contiguous <
  (check (domain-superset? (make-domain '(21 . 30))
			   (make-domain '(10 . 20))) => #f) ; contiguous >
  (check (domain-superset? (make-domain '(10 . 40))
			   (make-domain '(20 . 30))) => #t) ; first includes second
  (check (domain-superset? (make-domain '(20 . 30))
			   (make-domain '(10 . 40))) => #f) ; second includes first

  ;;                                    superset     superset
  (check (domain-superset? (make-domain '(10 . 40) '(50 . 80))
			   (make-domain '(20 . 30) '(60 . 70))) => #t)
  ;;                                    superset     overlapping
  (check (domain-superset? (make-domain '(10 . 40) '(50 . 70))
			   (make-domain '(20 . 30) '(60 . 80))) => #f)
  (check (domain-superset? (make-domain '(10 . 40) '(60 . 80))
			   (make-domain '(20 . 30) '(50 . 70))) => #f)

  ;;                                               superset     equal
  (check (domain-superset? (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
			   (make-domain            '(40 . 50) '(70 . 90))) => #t)

  ;;                                               superset     superset
  (check (domain-superset? (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
			   (make-domain            '(40 . 50) '(80 . 85))) => #t)

  ;;                                               superset   equal
  (check (domain-superset? (make-domain '(10 . 20) '(40 . 50) '(70 . 90))
			   (make-domain            '(30 . 60) '(70 . 90))) => #f)

  ;;                                               overlap    equal
  (check (domain-superset? (make-domain '(10 . 20) '(30 . 50) '(70 . 90))
			   (make-domain            '(40 . 60) '(70 . 90))) => #f)

  ;;                                               equal         equal
  (check (domain-superset? (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
			   (make-domain            '(30 . 40) 45 '(50 . 60))) => #f)

;;; --------------------------------------------------------------------

  (check (domain-superset?/strict (make-domain '(10 . 20))
				  (make-domain '(10 . 20))) => #f) ; equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 40))
				  (make-domain '(10 . 20) '(30 . 40))) => #f) ; equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
				  (make-domain '(10 . 20) '(30 . 40) '(50 . 60))) => #f) ; equal

  (check (domain-superset?/strict (make-domain '(40 . 60))
				  (make-domain '(50 . 60))) => #t) ; same last, start <
  (check (domain-superset?/strict (make-domain '(50 . 60))
				  (make-domain '(40 . 60))) => #f) ; same last, start >
  (check (domain-superset?/strict (make-domain '(40 . 50))
				  (make-domain '(40 . 60))) => #f) ; same start, last <
  (check (domain-superset?/strict (make-domain '(40 . 60))
				  (make-domain '(40 . 50))) => #t) ; same start, last >
  (check (domain-superset?/strict (make-domain '(10 . 20))
				  (make-domain '(30 . 40))) => #f) ; disjoint <
  (check (domain-superset?/strict (make-domain '(30 . 40))
				  (make-domain '(10 . 20))) => #f) ; disjoint >
  (check (domain-superset?/strict (make-domain '(10 . 30))
				  (make-domain '(20 . 40))) => #f) ; overlapping, start <
  (check (domain-superset?/strict (make-domain '(20 . 40))
				  (make-domain '(10 . 30))) => #f) ; overlapping, start >
  (check (domain-superset?/strict (make-domain '(10 . 20))
				  (make-domain '(20 . 30))) => #f) ; overlapping, last=start
  (check (domain-superset?/strict (make-domain '(20 . 30))
				  (make-domain '(10 . 20))) => #f) ; overlapping, start=last
  (check (domain-superset?/strict (make-domain '(10 . 20))
				  (make-domain '(21 . 30))) => #f) ; contiguous <
  (check (domain-superset?/strict (make-domain '(21 . 30))
				  (make-domain '(10 . 20))) => #f) ; contiguous >
  (check (domain-superset?/strict (make-domain '(10 . 40))
				  (make-domain '(20 . 30))) => #t) ; first includes second
  (check (domain-superset?/strict (make-domain '(20 . 30))
				  (make-domain '(10 . 40))) => #f) ; second includes first

  ;;                                           superset     superset
  (check (domain-superset?/strict (make-domain '(10 . 40) '(50 . 80))
				  (make-domain '(20 . 30) '(60 . 70))) => #t)
  ;;                                           superset     overlapping
  (check (domain-superset?/strict (make-domain '(10 . 40) '(50 . 70))
				  (make-domain '(20 . 30) '(60 . 80))) => #f)
  (check (domain-superset?/strict (make-domain '(10 . 40) '(60 . 80))
				  (make-domain '(20 . 30) '(50 . 70))) => #f)

  ;;                                                      superset     equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
				  (make-domain            '(40 . 50) '(70 . 90))) => #t)

  ;;                                                      superset     superset
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
				  (make-domain            '(40 . 50) '(80 . 85))) => #t)

  ;;                                                      equal      superset
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 40) '(50 . 90))
				  (make-domain            '(30 . 40) '(60 . 70))) => #t)


  ;;                                           superset     equal
  (check (domain-superset?/strict (make-domain '(30 . 60) '(70 . 90) 100)
				  (make-domain '(40 . 50) '(70 . 90))) => #t)

  ;;                                           superset     superset
  (check (domain-superset?/strict (make-domain '(30 . 60) '(70 . 90) 100)
				  (make-domain '(40 . 50) '(80 . 85))) => #t)

  ;;                                           equal      superset
  (check (domain-superset?/strict (make-domain '(30 . 40) '(50 . 90) 100)
				  (make-domain '(30 . 40) '(60 . 70))) => #t)

  ;;                                           superset     equal
  (check (domain-superset?/strict (make-domain '(30 . 60) '(70 . 90))
				  (make-domain '(40 . 50) '(70 . 90) 100)) => #f)

  ;;                                           superset     superset
  (check (domain-superset?/strict (make-domain '(30 . 60) '(70 . 90))
				  (make-domain '(40 . 50) '(80 . 85) 100)) => #f)

  ;;                                           equal      superset
  (check (domain-superset?/strict (make-domain '(30 . 40) '(50 . 90))
				  (make-domain '(30 . 40) '(60 . 70) 100)) => #f)

  ;;                                                      superset   equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(40 . 50) '(70 . 90))
				  (make-domain            '(30 . 60) '(70 . 90))) => #f)

  ;;                                                      overlap    equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 50) '(70 . 90))
				  (make-domain            '(40 . 60) '(70 . 90))) => #f)

  ;;                                                      equal      equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
				  (make-domain            '(30 . 40) '(50 . 60))) => #t)

  ;;                                                      equal         equal
  (check (domain-superset?/strict (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
				  (make-domain            '(30 . 40) 45 '(50 . 60))) => #f)

  )


(parameterise ((check-test-name	'domain-comparison))

  (check (domain=? (make-domain)    (make-domain 20)) => #f)
  (check (domain=? (make-domain 20) (make-domain))    => #f)
  (check (domain=? (make-domain 20) (make-domain 20)) => #t)
  (check (domain=? (make-domain 20) (make-domain 21)) => #f)

  (check (domain=? (make-domain '(20 . 40)) (make-domain '(20 . 40))) => #t)
  (check (domain=? (make-domain '(20 . 40)) (make-domain '(30 . 40))) => #f)
  (check (domain=? (make-domain '(30 . 40)) (make-domain '(20 . 40))) => #f)
  (check (domain=? (make-domain '(20 . 30)) (make-domain '(30 . 40))) => #f)
  (check (domain=? (make-domain '(30 . 40)) (make-domain '(20 . 30))) => #f)

  (check (domain<? (make-domain)    (make-domain 20)) => #f)
  (check (domain<? (make-domain 20) (make-domain))    => #f)

  (check (domain<? (make-domain 20) (make-domain 30)) => #t)
  (check (domain<? (make-domain 20) (make-domain 20)) => #f)
  (check (domain<? (make-domain 30) (make-domain 20)) => #f)

  )


(parameterise ((check-test-name	'domain-intersection))

  (check (domain-intersection (make-domain '(10 . 20))
			      (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20))) ; equal
  (check (domain-intersection (make-domain '(10 . 20) '(30 . 40))
			      (make-domain '(10 . 20) '(30 . 40)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40))) ; equal
  (check (domain-intersection (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
			      (make-domain '(10 . 20) '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(50 . 60))) ; equal

;;; --------------------------------------------------------------------

  (check (domain-intersection (make-domain '(40 . 60))
			      (make-domain '(50 . 60)))
    (=> domain=?) (make-domain '(50 . 60))) ; same last, start <

  (check (domain-intersection (make-domain '(50 . 60))
			      (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(50 . 60))) ; same last, start >

  (check (domain-intersection (make-domain '(40 . 50))
			      (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(40 . 50))) ; same start, last <

  (check (domain-intersection (make-domain '(40 . 60))
			      (make-domain '(40 . 50)))
    (=> domain=?) (make-domain '(40 . 50))) ; same start, last >

  (check (domain-intersection (make-domain '(10 . 20))
			      (make-domain '(30 . 40)))
    (=> domain=?) (make-domain)) ; disjoint <

  (check (domain-intersection (make-domain '(30 . 40))
			      (make-domain '(10 . 20)))
    (=> domain=?) (make-domain)) ; disjoint >

  (check (domain-intersection (make-domain '(10 . 30))
			      (make-domain '(20 . 40)))
    (=> domain=?) (make-domain '(20 . 30))) ; overlapping, start <

  (check (domain-intersection (make-domain '(20 . 40))
			      (make-domain '(10 . 30)))
    (=> domain=?) (make-domain '(20 . 30))) ; overlapping, start >

  (check (domain-intersection (make-domain '(10 . 20))
			      (make-domain '(20 . 30)))
    (=> domain=?) (make-domain 20)) ; overlapping, last=start

  (check (domain-intersection (make-domain '(20 . 30))
			      (make-domain '(10 . 20)))
    (=> domain=?) (make-domain 20)) ; overlapping, start=last

  (check (domain-intersection (make-domain '(10 . 20))
			      (make-domain '(21 . 30)))
    (=> domain=?) (make-domain)) ; contiguous <

  (check (domain-intersection (make-domain '(21 . 30))
			      (make-domain '(10 . 20)))
    (=> domain=?) (make-domain)) ; contiguous >

  (check (domain-intersection (make-domain '(10 . 40))
			      (make-domain '(20 . 30)))
    (=> domain=?) (make-domain '(20 . 30))) ; first includes second

  (check (domain-intersection (make-domain '(20 . 30))
			      (make-domain '(10 . 40)))
    (=> domain=?) (make-domain '(20 . 30))) ; second includes first

;;; --------------------------------------------------------------------

  (check (domain-intersection
	  ;;             subset     subset
	  (make-domain '(10 . 40) '(50 . 80))
	  (make-domain '(20 . 30) '(60 . 70)))
    (=> domain=?) (make-domain '(20 . 30) '(60 . 70)))

  (check (domain-intersection
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(50 . 70))
	  (make-domain '(20 . 30) '(60 . 80)))
    (=> domain=?) (make-domain '(20 . 30) '(60 . 70)))

  (check (domain-intersection
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(60 . 80))
	  (make-domain '(20 . 30) '(50 . 70)))
    (=> domain=?) (make-domain '(20 . 30) '(60 . 70)))

  (check (domain-intersection
	  ;;                        subset     equal
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain '(40 . 50) '(70 . 90)))

  (check (domain-intersection
	  ;;                        subset     subset
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain '(40 . 50) '(80 . 85)))

  (check (domain-intersection
	  ;;                        equal      subset
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 90))

	  (make-domain            '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain '(30 . 40) '(60 . 70)))


  (check (domain-intersection
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain '(40 . 50) '(70 . 90)))

  (check (domain-intersection
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain '(40 . 50) '(80 . 85)))

  (check (domain-intersection
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90) 100)
	  (make-domain '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain '(30 . 40) '(60 . 70)))

  (check (domain-intersection
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(70 . 90) 100))
    (=> domain=?) (make-domain '(40 . 50) '(70 . 90)))

  (check (domain-intersection
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(80 . 85) 100))
    (=> domain=?) (make-domain '(40 . 50) '(80 . 85)))

  (check (domain-intersection
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90))
	  (make-domain '(30 . 40) '(60 . 70) 100))
    (=> domain=?) (make-domain '(30 . 40) '(60 . 70)))

  (check (domain-intersection
	  ;;                        superset   equal
	  (make-domain '(10 . 20) '(40 . 50) '(70 . 90))
	  (make-domain            '(30 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(40 . 50) '(70 . 90)))

  (check (domain-intersection
	  ;;                        overlap    equal
	  (make-domain '(10 . 20) '(30 . 50) '(70 . 90))
	  (make-domain            '(40 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(40 . 50) '(70 . 90)))

  (check (domain-intersection
	  ;;                        equal      equal
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
	  (make-domain            '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain '(30 . 40) '(50 . 60)))

  (check (domain-intersection
	  ;;                        equal         equal
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(30 . 40) '(50 . 60)))

  (check (domain-intersection
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80))
	  (make-domain            '(30 . 40) 45      '(55 . 75)))
    (=> domain=?) (make-domain '(30 . 40) '(55 . 60) '(70 . 75)))

  (check (domain-intersection
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 . 75))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80)))
    (=> domain=?) (make-domain '(30 . 40) '(55 . 60) '(70 . 75)))

  (check (domain-intersection
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80) '(90 . 100))
	  (make-domain            '(30 . 40) 45      '(55 .                      120)))
    (=> domain=?) (make-domain '(30 . 40) '(55 . 60) '(70 . 80) '(90 . 100)))

  (check (domain-intersection
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 .                      120))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80) '(90 . 100)))
    (=> domain=?) (make-domain '(30 . 40) '(55 . 60) '(70 . 80) '(90 . 100)))

  (check (domain-intersection
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45            '(61 . 70)))
    (=> domain=?) (make-domain '(30 . 40)))

  (check (domain-intersection
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)               '(61 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(30 . 40)))

  (check (domain-intersection
	  ;;                        equal             overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45            '(60 . 70)))
    (=> domain=?) (make-domain '(30 . 40) 60))

  (check (domain-intersection
	  ;;                        equal             overlapping
	  (make-domain '(10 . 20) '(30 . 40)               '(60 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(30 . 40) 60))

  (check (domain-intersection
	  ;;                        equal             contiguous    contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(71 . 80))
	  (make-domain            '(30 . 40) 45           '(61 . 70)))
    (=> domain=?) (make-domain '(30 . 40)))

  (check (domain-intersection
	  ;;                        equal             contiguous    contiguous
	  (make-domain            '(30 . 40) 45           '(61 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(71 . 80)))
    (=> domain=?) (make-domain '(30 . 40)))

  (check (domain-intersection
	  ;;                        equal             contiguous  overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 59)     '(65 . 80))
	  (make-domain            '(30 . 40) 45           '(60 . 70)))
    (=> domain=?) (make-domain '(30 . 40) '(65 . 70)))

  (check (domain-intersection
	  ;;                        equal             contiguous  overlapping
	  (make-domain            '(30 . 40) 45           '(60 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 59)     '(65 . 80)))
    (=> domain=?) (make-domain '(30 . 40) '(65 . 70)))

  )


(parameterise ((check-test-name	'domain-union))

  (check (domain-union (make-domain '(10 . 20))
		       (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20))) ; equal
  (check (domain-union (make-domain '(10 . 20) '(30 . 40))
		       (make-domain '(10 . 20) '(30 . 40)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40))) ; equal
  (check (domain-union (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
		       (make-domain '(10 . 20) '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(50 . 60))) ; equal

;;; --------------------------------------------------------------------

  (check (domain-union (make-domain '(40 . 60))
		       (make-domain '(50 . 60)))
    (=> domain=?) (make-domain '(40 . 60))) ; same last, start <

  (check (domain-union (make-domain '(50 . 60))
		       (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(40 . 60))) ; same last, start >

  (check (domain-union (make-domain '(40 . 50))
		       (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(40 . 60))) ; same start, last <

  (check (domain-union (make-domain '(40 . 60))
		       (make-domain '(40 . 50)))
    (=> domain=?) (make-domain '(40 . 60))) ; same start, last >

  (check (domain-union (make-domain '(10 . 20))
		       (make-domain '(30 . 40)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40))) ; disjoint <

  (check (domain-union (make-domain '(30 . 40))
		       (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40))) ; disjoint >

  (check (domain-union (make-domain '(10 . 30))
		       (make-domain '(20 . 40)))
    (=> domain=?) (make-domain '(10 . 40))) ; overlapping, start <

  (check (domain-union (make-domain '(20 . 40))
		       (make-domain '(10 . 30)))
    (=> domain=?) (make-domain '(10 . 40))) ; overlapping, start >

  (check (domain-union (make-domain '(10 . 20))
		       (make-domain '(20 . 30)))
    (=> domain=?) (make-domain '(10 . 30))) ; overlapping, last=start

  (check (domain-union (make-domain '(20 . 30))
		       (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 30))) ; overlapping, start=last

  (check (domain-union (make-domain '(10 . 20))
		       (make-domain '(21 . 30)))
    (=> domain=?) (make-domain '(10 . 30))) ; contiguous <

  (check (domain-union (make-domain '(20 . 30))
		       (make-domain '(10 . 21)))
    (=> domain=?) (make-domain '(10 . 30))) ; contiguous >

  (check (domain-union (make-domain '(10 . 40))
		       (make-domain '(20 . 30)))
    (=> domain=?) (make-domain '(10 . 40))) ; first includes second

  (check (domain-union (make-domain '(20 . 30))
		       (make-domain '(10 . 40)))
    (=> domain=?) (make-domain '(10 . 40))) ; second includes first

;;; --------------------------------------------------------------------

  (check (domain-union
	  ;;             subset     subset
	  (make-domain '(10 . 40) '(50 . 80))
	  (make-domain '(20 . 30) '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 40) '(50 . 80)))

  (check (domain-union
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(50 . 70))
	  (make-domain '(20 . 30) '(60 . 80)))
    (=> domain=?) (make-domain '(10 . 40) '(50 . 80)))

  (check (domain-union
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(60 . 80))
	  (make-domain '(20 . 30) '(50 . 70)))
    (=> domain=?) (make-domain '(10 . 40) '(50 . 80)))

  (check (domain-union
	  ;;                        subset     equal
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 60) '(70 . 90)))

  (check (domain-union
	  ;;                        subset     subset
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 60) '(70 . 90)))

  (check (domain-union
	  ;;                        equal      subset
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 90))
	  (make-domain            '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(50 . 90)))

  (check (domain-union
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain '(30 . 60) '(70 . 90) '(100 . 100)))

  (check (domain-union
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain '(30 . 60) '(70 . 90) '(100 . 100)))

  (check (domain-union
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90) 100)
	  (make-domain '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain '(30 . 40) '(50 . 90) '(100 . 100)))

  (check (domain-union
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(70 . 90) 100))
    (=> domain=?) (make-domain '(30 . 60) '(70 . 90) '(100 . 100)))

  (check (domain-union
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(80 . 85) 100))
    (=> domain=?) (make-domain '(30 . 60) '(70 . 90) '(100 . 100)))

  (check (domain-union
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90))
	  (make-domain '(30 . 40) '(60 . 70) 100))
    (=> domain=?) (make-domain '(30 . 40) '(50 . 90) '(100 . 100)))

  (check (domain-union
	  ;;                        superset   equal
	  (make-domain '(10 . 20) '(40 . 50) '(70 . 90))
	  (make-domain            '(30 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 60) '(70 . 90)))

  (check (domain-union
	  ;;                        overlap    equal
	  (make-domain '(10 . 20) '(30 . 50) '(70 . 90))
	  (make-domain            '(40 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 60) '(70 . 90)))

  (check (domain-union
	  ;;                        equal      equal
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
	  (make-domain            '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(50 . 60)))

  (check (domain-union
	  ;;                        equal         equal
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 60)))

  (check (domain-union
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80))
	  (make-domain            '(30 . 40) 45      '(55 . 75)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 80)))

  (check (domain-union
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 . 75))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 80)))

  (check (domain-union
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80) '(90 . 100))
	  (make-domain            '(30 . 40) 45      '(55 .                      120)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 120)))

  (check (domain-union
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 .                      120))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80) '(90 . 100)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 120)))

  (check (domain-union
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45            '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 70)))

  (check (domain-union
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)               '(60 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 70)))

  (check (domain-union
	  ;;                        equal             contiguous    contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(70 . 80))
	  (make-domain            '(30 . 40) 45           '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 80)))

  (check (domain-union
	  ;;                        equal             contiguous    contiguous
	  (make-domain            '(30 . 40) 45           '(60 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(70 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 80)))

  (check (domain-union
	  ;;                        equal             contiguous  overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 59)     '(65 . 80))
	  (make-domain            '(30 . 40) 45           '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 80)))

  (check (domain-union
	  ;;                        equal             contiguous  overlapping
	  (make-domain            '(30 . 40) 45           '(60 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 59)     '(65 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40) '(45 . 45) '(50 . 80)))

  )


(parameterise ((check-test-name	'domain-difference))

  (check (domain-difference (make-domain '(10 . 20))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain)) ; equal
  (check (domain-difference (make-domain '(10 . 20) '(30 . 40))
			    (make-domain '(10 . 20) '(30 . 40)))
    (=> domain=?) (make-domain)) ; equal
  (check (domain-difference (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
			    (make-domain '(10 . 20) '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain)) ; equal

;;; --------------------------------------------------------------------

  (check (domain-difference (make-domain '(40 . 60))
			    (make-domain '(50 . 60)))
    (=> domain=?) (make-domain '(40 . 49))) ; same last, start <

  (check (domain-difference (make-domain '(50 . 60))
			    (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(40 . 49))) ; same last, start >

  (check (domain-difference (make-domain '(40 . 50))
			    (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(51 . 60))) ; same start, last <

  (check (domain-difference (make-domain '(40 . 60))
			    (make-domain '(40 . 50)))
    (=> domain=?) (make-domain '(51 . 60))) ; same start, last >

  (check (domain-difference (make-domain '(10 . 20))
			    (make-domain '(30 . 40)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40))) ; disjoint <

  (check (domain-difference (make-domain '(30 . 40))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 40))) ; disjoint >

  (check (domain-difference (make-domain '(10 . 30))
			    (make-domain '(20 . 40)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40))) ; overlapping, start <

  (check (domain-difference (make-domain '(20 . 40))
			    (make-domain '(10 . 30)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40))) ; overlapping, start >

  (check (domain-difference (make-domain '(10 . 20))
			    (make-domain '(20 . 30)))
    (=> domain=?) (make-domain '(10 . 19) '(21 . 30))) ; overlapping, last=start

  (check (domain-difference (make-domain '(20 . 30))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 19) '(21 . 30))) ; overlapping, start=last

  (check (domain-difference (make-domain '(10 . 20))
			    (make-domain '(21 . 30)))
    (=> domain=?) (make-domain '(10 . 30))) ; contiguous <

  (check (domain-difference (make-domain '(21 . 30))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 30))) ; contiguous >

  (check (domain-difference (make-domain '(10 . 40))
			    (make-domain '(20 . 30)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40))) ; first includes second

  (check (domain-difference (make-domain '(20 . 30))
			    (make-domain '(10 . 40)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40))) ; second includes first

;;; --------------------------------------------------------------------

  (check (domain-difference
	  ;;             subset     subset
	  (make-domain '(10 . 40) '(50 . 80))
	  (make-domain '(20 . 30) '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40) '(50 . 59) '(71 . 80)))

  (check (domain-difference
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(50 . 70))
	  (make-domain '(20 . 30) '(60 . 80)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40) '(50 . 59) '(71 . 80)))

  (check (domain-difference
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(60 . 80))
	  (make-domain '(20 . 30) '(50 . 70)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40) '(50 . 59) '(71 . 80)))

  (check (domain-difference
	  ;;                        subset     equal
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 39) '(51 . 60)))

  (check (domain-difference
	  ;;                        subset     subset
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 39) '(51 . 60) '(70 . 79) '(86 . 90)))

  (check (domain-difference
	  ;;                        equal      subset
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 90))
	  (make-domain            '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(50 . 59) '(71 . 90)))

  (check (domain-difference
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain '(30 . 39) '(51 . 60) '(100 . 100)))

  (check (domain-difference
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain '(30 . 39) '(51 . 60) '(70 . 79) '(86 . 90) '(100 . 100)))

  (check (domain-difference
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90) 100)
	  (make-domain '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain '(50 . 59) '(71 . 90) '(100 . 100)))

  (check (domain-difference
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(70 . 90) 100))
    (=> domain=?) (make-domain '(30 . 39) '(51 . 60) '(100 . 100)))

  (check (domain-difference
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(80 . 85) 100))
    (=> domain=?) (make-domain '(30 . 39) '(51 . 60) '(70 . 79) '(86 . 90) '(100 . 100)))

  (check (domain-difference
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90))
	  (make-domain '(30 . 40) '(60 . 70) 100))
    (=> domain=?) (make-domain '(50 . 59) '(71 . 90) '(100 . 100)))

  (check (domain-difference
	  ;;                        superset   equal
	  (make-domain '(10 . 20) '(40 . 50) '(70 . 90))
	  (make-domain            '(30 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 39) '(51 . 60)))

  (check (domain-difference
	  ;;                        overlap    equal
	  (make-domain '(10 . 20) '(30 . 50) '(70 . 90))
	  (make-domain            '(40 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(10 . 20) '(30 . 39) '(51 . 60)))

  (check (domain-difference
	  ;;                        equal      equal
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
	  (make-domain            '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20)))

  (check (domain-difference
	  ;;                        equal         equal
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45)))

  (check (domain-difference
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80))
	  (make-domain            '(30 . 40) 45      '(55 . 75)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 54) '(61 . 69) '(76 . 80)))

  (check (domain-difference
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 . 75))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 54) '(61 . 69) '(76 . 80)))

  (check (domain-difference
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80) '(90 . 100))
	  (make-domain            '(30 . 40) 45      '(55 .                      120)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 54) '(61 . 69) '(81 . 89) '(101 . 120)))

  (check (domain-difference
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 .                      120))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80) '(90 . 100)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 54) '(61 . 69) '(81 . 89) '(101 . 120)))

  (check (domain-difference
	  ;;                        equal             overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45         '(60 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 59) '(61 . 70)))

  (check (domain-difference
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)               '(60 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 59) '(61 . 70)))

  (check (domain-difference
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45         '(61 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 70)))

  (check (domain-difference
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)               '(61 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 70)))

  (check (domain-difference
	  ;;                        equal             contiguous    contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(71 . 80))
	  (make-domain            '(30 . 40) 45           '(61 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 80)))

  (check (domain-difference
	  ;;                        equal             contiguous    contiguous
	  (make-domain            '(30 . 40) 45           '(61 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(71 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 80)))

  (check (domain-difference
	  ;;                        equal        contiguous  overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)  '(65 . 80))
	  (make-domain            '(30 . 40) 45         '(61 . 70)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 64) '(71 . 80)))

  (check (domain-difference
	  ;;                        equal             contiguous  overlapping
	  (make-domain            '(30 . 40) 45           '(61 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)     '(65 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(45 . 45) '(50 . 64) '(71 . 80)))

  )


(parameterise ((check-test-name	'domain-complement))

  (check (domain-complement (make-domain)
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20))) ; equal

  (check (domain-complement (make-domain '(10 . 20))
			    (make-domain))
    (=> domain=?) (make-domain)) ; equal

  (check (domain-complement (make-domain '(10 . 20))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain)) ; equal
  (check (domain-complement (make-domain '(10 . 20) '(30 . 40))
			    (make-domain '(10 . 20) '(30 . 40)))
    (=> domain=?) (make-domain)) ; equal
  (check (domain-complement (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
			    (make-domain '(10 . 20) '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain)) ; equal

;;; --------------------------------------------------------------------

  (check (domain-complement (make-domain '(40 . 60))
			    (make-domain '(50 . 60)))
    (=> domain=?) (make-domain)) ; same last, start <

  (check (domain-complement (make-domain '(50 . 60))
			    (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(40 . 49))) ; same last, start >

  (check (domain-complement (make-domain '(40 . 50))
			    (make-domain '(40 . 60)))
    (=> domain=?) (make-domain '(51 . 60))) ; same start, last <

  (check (domain-complement (make-domain '(40 . 60))
			    (make-domain '(40 . 50)))
    (=> domain=?) (make-domain)) ; same start, last >

  (check (domain-complement (make-domain '(10 . 20))
			    (make-domain '(30 . 40)))
    (=> domain=?) (make-domain '(30 . 40))) ; disjoint <

  (check (domain-complement (make-domain '(30 . 40))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20))) ; disjoint >

  (check (domain-complement (make-domain '(10 . 30))
			    (make-domain '(20 . 40)))
    (=> domain=?) (make-domain '(31 . 40))) ; overlapping, start <

  (check (domain-complement (make-domain '(20 . 40))
			    (make-domain '(10 . 30)))
    (=> domain=?) (make-domain '(10 . 19))) ; overlapping, start >

  (check (domain-complement (make-domain '(10 . 20))
			    (make-domain '(20 . 30)))
    (=> domain=?) (make-domain '(21 . 30))) ; overlapping, last=start

  (check (domain-complement (make-domain '(20 . 30))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 19))) ; overlapping, last=start

  (check (domain-complement (make-domain '(10 . 20))
			    (make-domain '(21 . 30)))
    (=> domain=?) (make-domain '(21 . 30))) ; contiguous <

  (check (domain-complement (make-domain '(21 . 30))
			    (make-domain '(10 . 20)))
    (=> domain=?) (make-domain '(10 . 20))) ; contiguous >

  (check (domain-complement (make-domain '(10 . 40))
			    (make-domain '(20 . 30)))
    (=> domain=?) (make-domain)) ; first includes second

  (check (domain-complement (make-domain '(20 . 30))
			    (make-domain '(10 . 40)))
    (=> domain=?) (make-domain '(10 . 19) '(31 . 40))) ; second includes first

;;; --------------------------------------------------------------------

  (check (domain-complement
	  ;;             subset     subset
	  (make-domain '(10 . 40) '(50 . 80))
	  (make-domain '(20 . 30) '(60 . 70)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(50 . 70))
	  (make-domain '(20 . 30) '(60 . 80)))
    (=> domain=?) (make-domain '(71 . 80)))

  (check (domain-complement
	  ;;             subset     overlapping
	  (make-domain '(10 . 40) '(60 . 80))
	  (make-domain '(20 . 30) '(50 . 70)))
    (=> domain=?) (make-domain '(50 . 59)))

  (check (domain-complement
	  ;;                        subset     equal
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;                        subset     subset
	  (make-domain '(10 . 20) '(30 . 60) '(70 . 90))
	  (make-domain            '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;                        equal      subset
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 90))
	  (make-domain            '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(70 . 90)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90) 100)
	  (make-domain '(40 . 50) '(80 . 85)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90) 100)
	  (make-domain '(30 . 40) '(60 . 70)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;             subset     equal
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(70 . 90) 100))
    (=> domain=?) (make-domain '(100 . 100)))

  (check (domain-complement
	  ;;             subset     subset
	  (make-domain '(30 . 60) '(70 . 90))
	  (make-domain '(40 . 50) '(80 . 85) 100))
    (=> domain=?) (make-domain '(100 . 100)))

  (check (domain-complement
	  ;;             equal      subset
	  (make-domain '(30 . 40) '(50 . 90))
	  (make-domain '(30 . 40) '(60 . 70) 100))
    (=> domain=?) (make-domain '(100 . 100)))

  (check (domain-complement
	  ;;                        superset   equal
	  (make-domain '(10 . 20) '(40 . 50) '(70 . 90))
	  (make-domain            '(30 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(30 . 39) '(51 . 60)))

  (check (domain-complement
	  ;;                        overlap    equal
	  (make-domain '(10 . 20) '(30 . 50) '(70 . 90))
	  (make-domain            '(40 . 60) '(70 . 90)))
    (=> domain=?) (make-domain '(51 . 60)))

  (check (domain-complement
	  ;;                        equal      equal
	  (make-domain '(10 . 20) '(30 . 40) '(50 . 60))
	  (make-domain            '(30 . 40) '(50 . 60)))
    (=> domain=?) (make-domain))

  (check (domain-complement
	  ;;                        equal         equal
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain 45))

  (check (domain-complement
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80))
	  (make-domain            '(30 . 40) 45      '(55 . 75)))
    (=> domain=?) (make-domain 45 '(61 . 69)))

  (check (domain-complement
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 . 75))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80)))
    (=> domain=?) (make-domain 45 '(50 . 54) '(76 . 80)))

  (check (domain-complement
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60) '(70 . 80) '(90 . 100))
	  (make-domain            '(30 . 40) 45      '(55 .                      120)))
    (=> domain=?) (make-domain 45 '(61 . 69) '(81 . 89) '(101 . 120)))

  (check (domain-complement
	  ;;                        equal             overlap
	  (make-domain '(10 . 20) '(30 . 40)         '(55 .                      120))
	  (make-domain            '(30 . 40) 45 '(50 . 60) '(70 . 80) '(90 . 100)))
    (=> domain=?) (make-domain 45 '(50 . 54)))

  (check (domain-complement
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45         '(61 . 70)))
    (=> domain=?) (make-domain 45 '(61 . 70)))

  (check (domain-complement
	  ;;                        equal             contiguous
	  (make-domain '(10 . 20) '(30 . 40)               '(61 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain 45 '(50 . 60)))

  (check (domain-complement
	  ;;                        equal             overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60))
	  (make-domain            '(30 . 40) 45         '(60 . 70)))
    (=> domain=?) (make-domain 45 '(61 . 70)))

  (check (domain-complement
	  ;;                        equal             overlapping
	  (make-domain '(10 . 20) '(30 . 40)               '(60 . 70))
	  (make-domain            '(30 . 40) 45 '(50 . 60)))
    (=> domain=?) (make-domain 45 '(50 . 59)))

  (check (domain-complement
	  ;;                        equal             contiguous    contiguous
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(71 . 80))
	  (make-domain            '(30 . 40) 45           '(61 . 70)))
    (=> domain=?) (make-domain 45 '(61 . 70)))

  (check (domain-complement
	  ;;                        equal             contiguous    contiguous
	  (make-domain            '(30 . 40) 45           '(61 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)         '(71 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(50 . 60) '(71 . 80)))

  (check (domain-complement
	  ;;                        equal        contiguous  overlapping
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)  '(65 . 80))
	  (make-domain            '(30 . 40) 45         '(61 . 70)))
    (=> domain=?) (make-domain 45 '(61 . 64)))

  (check (domain-complement
	  ;;                        equal             contiguous  overlapping
	  (make-domain            '(30 . 40) 45          '(61 . 70))
	  (make-domain '(10 . 20) '(30 . 40)    '(50 . 60)     '(65 . 80)))
    (=> domain=?) (make-domain '(10 . 20) '(50 . 60) '(71 . 80)))

  )


(parameterise ((check-test-name	'domain-list-operations))

  (check
      (cadr (with-result
	     (domain-for-each (lambda (ch)
				(add-result ch))
			      (make-domain 65 66 67))))
    => '(65 66 67))

;;; --------------------------------------------------------------------

  (check
      (domain-every (lambda (ch) (<= 65 ch))
		    (make-domain 65 66 67))
    => #t)

  (check
      (domain-every (lambda (ch) (<= 67 ch))
		    (make-domain 65 66 67))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (domain-any (lambda (ch)
		    (= 66 ch))
		  (make-domain 65 66 67))
    => #t)

  (check
      (domain-any (lambda (ch)
		    (= 100 ch))
		  (make-domain 65 66 67))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (domain-fold (lambda (ch knil)
		     (cons ch knil))
		   '()
		   (make-domain 65 66 67))
    => '(67 66 65))

  (check
      (domain-fold (lambda (ch knil)
		     (cons ch knil))
		   '()
		   (make-domain))
    => '())

;;; --------------------------------------------------------------------

  (check
      (domain->list (make-domain))
    => '())

  (check
      (domain->list (make-domain 65))
    => '(65))

  (check
      (domain->list (make-domain 65 66 67 68))
    => '(65 66 67 68))

  )


;;;; done

(check-report)

;;; end of file
