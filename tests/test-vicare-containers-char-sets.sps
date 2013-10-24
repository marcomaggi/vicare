;;;
;;;Part of: Vicare Scheme
;;;Contents: test for char-set library
;;;Date: Thu Jan 22, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (vicare)
  (vicare checks)
  (vicare containers char-sets)
  (vicare containers char-sets blocks)
  (vicare containers char-sets categories)
  (except (vicare containers lists)
	  break))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: char-sets library\n")


;;;; helpers

(define (domain=? actual-result expected-result)
  (equal? (char-set-domain-ref actual-result) expected-result))

(define (char-prev ch)
  (integer->char (- (char->integer ch) 1)))

(define (char-next ch)
  (integer->char (+ 1 (char->integer ch))))


(parameterise ((check-test-name	'constructor))

  (check
      (char-set)
    (=> domain=?) '())

  (check
      (char-set #\A)
    (=> domain=?) '((#\A . #\A)))

  (check (char-set #\A #\A) (=> domain=?) '((#\A . #\A)))
  (check (char-set #\A #\A #\A) (=> domain=?) '((#\A . #\A)))
  (check (char-set #\A #\B #\A) (=> domain=?) '((#\A . #\B)))
  (check (char-set #\A #\A #\B #\A) (=> domain=?) '((#\A . #\B)))

  (check (char-set #\A #\B) (=> domain=?) '((#\A . #\B)))
  (check (char-set #\B #\A) (=> domain=?) '((#\A . #\B)))

  (check (char-set #\A #\B #\C) (=> domain=?) '((#\A . #\C)))
  (check (char-set #\B #\A #\C) (=> domain=?) '((#\A . #\C)))
  (check (char-set #\C #\A #\B) (=> domain=?) '((#\A . #\C)))
  (check (char-set #\B #\C #\A) (=> domain=?) '((#\A . #\C)))

  (check (char-set #\A #\C) (=> domain=?) '((#\A . #\A) (#\C . #\C)))
  (check (char-set #\C #\A) (=> domain=?) '((#\A . #\A) (#\C . #\C)))

  (check (char-set #\a #\b #\c #\A #\B #\C) (=> domain=?) '((#\A . #\C)
							    (#\a . #\c)))

;;; --------------------------------------------------------------------

  (check (char-set '(#\A . #\B)) (=> domain=?) '((#\A . #\B)))

  ;; equal
  (check (char-set '(#\B . #\C) '(#\B . #\C)) (=> domain=?) '((#\B . #\C)))

  ;; overlapping
  (check (char-set '(#\A . #\B) '(#\B . #\C)) (=> domain=?) '((#\A . #\C)))
  (check (char-set '(#\B . #\C) '(#\A . #\B)) (=> domain=?) '((#\A . #\C)))

  ;; contiguous
  (check (char-set '(#\A . #\B) '(#\C . #\D)) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\C . #\D) '(#\A . #\B)) (=> domain=?) '((#\A . #\D)))

  ;; included
  (check (char-set '(#\A . #\D) '(#\B . #\C)) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\B . #\C) '(#\A . #\D)) (=> domain=?) '((#\A . #\D)))

  ;; distanced
  (check (char-set '(#\A . #\B) '(#\D . #\E)) (=> domain=?) '((#\A . #\B) (#\D . #\E)))
  (check (char-set '(#\D . #\E) '(#\A . #\B)) (=> domain=?) '((#\A . #\B) (#\D . #\E)))

  (check (char-set #\A #\D '(#\B . #\C)) (=> domain=?) '((#\A . #\D)))
  (check (char-set #\A '(#\B . #\C) #\D) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\B . #\C) #\A #\D) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\B . #\C) #\D #\A) (=> domain=?) '((#\A . #\D)))

  )


(parameterise ((check-test-name	'predicate))

  (check
      (char-set? (char-set))
    => #t)

  (check
      (char-set? (char-set '(#\A . #\C)))
    => #t)

  (check
      (char-set? 123)
    => #f)

  )


(parameterise ((check-test-name	'inspection))

  (check
      (char-set-empty? (char-set))
    => #t)

  (check
      (char-set-empty? (char-set #\A))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char-set-size (char-set))
    => 0)

  (check
      (char-set-size (char-set #\A))
    => 1)

  (check
      (char-set-size (char-set #\A #\C))
    => 2)

  (check
      (char-set-size (char-set '(#\A . #\D) '(#\F . #\H)))
    => (+ 4 3))

  (check
      (char-set-size char-set:empty)
    => 0)

  (check
      (char-set-size char-set:full)
    => (+ (- #xD800 0)
	  (- #x10FFFF #xDFFF)))

;;; --------------------------------------------------------------------

  (check
      (char-set-contains? (char-set '(#\A . #\F)) #\C)
    => #t)

  (check
      (char-set-contains? (char-set '(#\A . #\F)) #\M)
    => #f)

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (char-set-superset? (char-set '(#\A . #\F))
			  (char-set '(#\A . #\F)))
    => #t)

  (check
      ;; included
      (char-set-superset? (char-set '(#\A . #\M))
			  (char-set '(#\B . #\F)))
    => #t)

  (check
      ;;                              included     included
      (char-set-superset? (char-set '(#\A . #\M) '(#\P . #\Z))
			  (char-set '(#\B . #\F) '(#\S . #\X)))
    => #t)

  (check
      ;;                              included     overlapping
      (char-set-superset? (char-set '(#\A . #\M) '(#\P . #\X))
			  (char-set '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                              overlapping
      (char-set-superset? (char-set '(#\A . #\D) '(#\P . #\X))
			  (char-set '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                                           included     included
      (char-set-superset? (char-set '(#\0 . #\6) '(#\A . #\D) '(#\P . #\Z))
			  (char-set              '(#\B . #\F) '(#\S . #\X)))
    => #f)

  (check
      ;;                                           included        included
      (char-set-superset? (char-set '(#\0 . #\6) '(#\A . #\G) #\M '(#\P . #\Z))
			  (char-set              '(#\B . #\F)     '(#\S . #\X)))
    => #t)

  (check
      (char-set-superset? (char-set '(#\0 . #\6) '(#\A . #\D)     '(#\P . #\Z))
			  (char-set              '(#\B . #\F) #\M '(#\S . #\X)))
    => #f)

  )


(parameterise ((check-test-name	'comparison))

  (check (char-set=? (char-set) (char-set #\A)) => #f)
  (check (char-set=? (char-set #\A) (char-set)) => #f)

  (check
      (char-set=? (char-set #\A)
		  (char-set #\A))
    => #t)

  (check
      (char-set=? (char-set #\A)
		  (char-set #\B))
    => #f)

  (check
      (char-set=? (char-set '(#\A . #\G))
		  (char-set '(#\A . #\G)))
    => #t)

  (check (char-set=? (char-set '(#\A . #\G)) (char-set '(#\D . #\G))) => #f)
  (check (char-set=? (char-set '(#\D . #\G)) (char-set '(#\A . #\G))) => #f)

  (check (char-set=? (char-set '(#\A . #\D)) (char-set '(#\F . #\M))) => #f)
  (check (char-set=? (char-set '(#\F . #\M)) (char-set '(#\A . #\D))) => #f)

;;; --------------------------------------------------------------------

  (check (char-set<? (char-set)     (char-set #\B)) => #f)
  (check (char-set<? (char-set #\A) (char-set))     => #f)

  (check (char-set<? (char-set #\A) (char-set #\B)) => #t)
  (check (char-set<? (char-set #\A) (char-set #\A)) => #f)
  (check (char-set<? (char-set #\B) (char-set #\A)) => #f)

  )


(parameterise ((check-test-name	'intersection))

  (check
      ;; empty
      (char-set-intersection (char-set)
			     (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; empty
      (char-set-intersection (char-set '(#\A . #\H))
			     (char-set))
    (=> char-set=?) (char-set))

  (check
      ;; equal
      (char-set-intersection (char-set '(#\A . #\H))
			     (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; disjoint
      (char-set-intersection (char-set '(#\A . #\C))
			     (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; disjoint
      (char-set-intersection (char-set '(#\E . #\H))
			     (char-set '(#\A . #\C)))
    (=> char-set=?) (char-set))

  (check
      ;; contiguous
      (char-set-intersection (char-set '(#\A . #\D))
			     (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; contiguous
      (char-set-intersection (char-set '(#\E . #\H))
			     (char-set '(#\A . #\D)))
    (=> char-set=?) (char-set))

  (check
      ;; inclusion
      (char-set-intersection (char-set '(#\C . #\F))
			     (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\C . #\F)))

  (check
      ;; inclusion
      (char-set-intersection (char-set '(#\A . #\H))
			     (char-set '(#\C . #\F)))
    (=> char-set=?) (char-set '(#\C . #\F)))

  (check
      (char-set-intersection (char-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			     (char-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> char-set=?) (char-set '(#\C . #\D)
			      '(#\I . #\L)
			      '(#\O . #\P)))

  (check
      ;; disjoint tail
      (char-set-intersection (char-set '(#\A . #\D) #\F)
			     (char-set '(#\C . #\E) #\F #\M #\P #\R))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; disjoint tail
      (char-set-intersection (char-set '(#\A . #\D) #\F #\M #\P #\R)
			     (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; contiguous tail
      (char-set-intersection (char-set '(#\A . #\D) #\F #\G #\H #\I)
			     (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; contiguous tail
      (char-set-intersection (char-set '(#\A . #\D) #\F)
			     (char-set '(#\C . #\E) #\F #\G #\H #\I))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; overlapping tail
      (char-set-intersection (char-set '(#\A . #\D) #\F)
			     (char-set '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; overlapping tail
      (char-set-intersection (char-set '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
			     (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  )


(parameterise ((check-test-name	'union))

  (check
      ;; empty
      (char-set-union (char-set)
		      (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; empty
      (char-set-union (char-set '(#\A . #\H))
		      (char-set))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; equal
      (char-set-union (char-set '(#\A . #\H))
		      (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; disjoint
      (char-set-union (char-set '(#\A . #\C))
		      (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (char-set-union (char-set '(#\E . #\H))
		      (char-set '(#\A . #\C)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (char-set-union (char-set '(#\A . #\D))
		      (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; contiguous
      (char-set-union (char-set '(#\E . #\H))
		      (char-set '(#\A . #\D)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; inclusion
      (char-set-union (char-set '(#\C . #\F))
		      (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; inclusion
      (char-set-union (char-set '(#\A . #\H))
		      (char-set '(#\C . #\F)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      (char-set-union (char-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
		      (char-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> char-set=?) (char-set '(#\A . #\F)
			      '(#\H . #\Q)))

  (check
      ;; disjoint tail
      (char-set-union (char-set '(#\A . #\D) #\F)
		      (char-set '(#\C . #\E) #\F #\M #\P #\R))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\M #\P #\R))

  (check
      ;; disjoint tail
      (char-set-union (char-set '(#\A . #\D) #\F #\M #\P #\R)
		      (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\M #\P #\R))

  (check
      ;; contiguous tail
      (char-set-union (char-set '(#\A . #\D) #\F #\G #\H #\I)
		      (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\G #\H #\I))

  (check
      ;; contiguous tail
      (char-set-union (char-set '(#\A . #\D) #\F)
		      (char-set '(#\C . #\E) #\F #\G #\H #\I))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\G #\H #\I))

  (check
      ;; overlapping tail
      (char-set-union (char-set '(#\A . #\D) #\F)
		      (char-set '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> char-set=?) (char-set '(#\A . #\E) #\F '(#\H . #\N) '(#\L . #\P)))

  (check
      ;; overlapping tail
      (char-set-union (char-set '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
		      (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\E) #\F '(#\H . #\N) '(#\L . #\P)))

  )


(parameterise ((check-test-name	'difference))

  (check
      ;; empty
      (char-set-difference (char-set)
			   (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; empty
      (char-set-difference (char-set '(#\A . #\H))
			   (char-set))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; equal
      (char-set-difference (char-set '(#\A . #\H))
			   (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; disjoint
      (char-set-difference (char-set '(#\A . #\C))
			   (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (char-set-difference (char-set '(#\E . #\H))
			   (char-set '(#\A . #\C)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (char-set-difference (char-set '(#\A . #\D))
			   (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; contiguous
      (char-set-difference (char-set '(#\E . #\H))
			   (char-set '(#\A . #\D)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; inclusion
      (char-set-difference (char-set '(#\C . #\F))
			   (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\B)
			      '(#\G . #\H)))

  (check
      ;; inclusion
      (char-set-difference (char-set '(#\A . #\H))
			   (char-set '(#\C . #\F)))
    (=> char-set=?) (char-set '(#\A . #\B)
			      '(#\G . #\H)))

  (check
      (char-set-difference (char-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			   (char-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> char-set=?) (char-set #\A #\B #\E #\F #\H #\M #\N #\Q))

  (check
      ;; disjoint tail
      (char-set-difference (char-set '(#\A . #\D) #\F)
			   (char-set '(#\C . #\E) #\F #\M #\P #\R))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\M #\P #\R))

  (check
      ;; disjoint tail
      (char-set-difference (char-set '(#\A . #\D) #\F #\M #\P #\R)
			   (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\M #\P #\R))

  (check
      ;; contiguous tail
      (char-set-difference (char-set '(#\A . #\D) #\F #\G #\H #\I)
			   (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\G #\H #\I))

  (check
      ;; contiguous tail
      (char-set-difference (char-set '(#\A . #\D) #\F)
			   (char-set '(#\C . #\E) #\F #\G #\H #\I))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\G #\H #\I))

  (check
      ;; overlapping tail
      (char-set-difference (char-set '(#\A . #\D) #\F)
			   (char-set '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> char-set=?) (char-set '(#\A . #\B) #\E '(#\H . #\N) '(#\L . #\P)))

  (check
      ;; overlapping tail
      (char-set-difference (char-set '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
			   (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\B) #\E '(#\H . #\N) '(#\L . #\P)))

  )


(parameterise ((check-test-name	'complement))

  (check
      (char-set-complement (char-set))
    (=> char-set=?)
    char-set:full)

  (check
      (char-set-complement (char-set #\A))
    (=> char-set=?)
    (let* ((ch		#\A)
	   (chup	(char-next ch))
	   (chdn	(char-prev ch)))
      (char-set (cons char-set-lower-bound chdn)
		(cons chup char-set-inner-upper-bound)
		(cons char-set-inner-lower-bound char-set-upper-bound))))

  (check
      (char-set-complement (char-set '(#\A . #\D) '(#\M . #\Z)))
    (=> char-set=?)
    (char-set (cons char-set-lower-bound (char-prev #\A))
	      (cons (char-next #\D) (char-prev #\M))
	      (cons (char-next #\Z) char-set-inner-upper-bound)
	      (cons char-set-inner-lower-bound char-set-upper-bound)))

  (check
      (char-set-complement (char-set '(#\A . #\D) '(#\M . #\Z) '(#\4 . #\9)))
    (=> char-set=?)
    (char-set (cons char-set-lower-bound (char-prev #\4))
	      (cons (char-next #\9) (char-prev #\A))
	      (cons (char-next #\D) (char-prev #\M))
	      (cons (char-next #\Z) char-set-inner-upper-bound)
	      (cons char-set-inner-lower-bound char-set-upper-bound)))

  )


(parameterise ((check-test-name	'list-operations))

  (check
      (cadr (with-result
	     (char-set-for-each (lambda (ch)
				  (add-result ch))
				(char-set #\A #\B #\C))
	     #t))
    => '(#\A #\B #\C))

;;; --------------------------------------------------------------------

  (check
      (char-set-every (lambda (ch)
			(<= 65 (char->integer ch)))
		      (char-set #\A #\B #\C))
    => #t)

  (check
      (char-set-every (lambda (ch)
			(<= 67 (char->integer ch)))
		      (char-set #\A #\B #\C))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (char-set-any (lambda (ch)
		      (= 66 (char->integer ch)))
		    (char-set #\A #\B #\C))
    => #t)

  (check
      (char-set-any (lambda (ch)
		      (= 100 (char->integer ch)))
		    (char-set #\A #\B #\C))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char-set-fold (lambda (ch knil)
		       (cons (char->integer ch) knil))
		     '()
		     (char-set #\A #\B #\C))
    => '(67 66 65))

  (check
      (char-set-fold (lambda (ch knil)
		       (cons (char->integer ch) knil))
		     '()
		     (char-set))
    => '())

;;; --------------------------------------------------------------------

  (check
      (char-set->list (char-set))
    => '())

  (check
      (char-set->list (char-set #\A))
    => '(#\A))

  (check
      (char-set->list (char-set #\A #\B #\C #\D))
    => '(#\A #\B #\C #\D))

  )


(parameterise ((check-test-name	'string-operations))

  (check
      (string->char-set "")
    (=> char-set=?)
    (char-set))

  (check
      (string->char-set "ABCD")
    (=> char-set=?)
    (char-set #\A #\B #\C #\D))

  (check
      (string->char-set "ABCDBBCC")
    (=> char-set=?)
    (char-set #\A #\B #\C #\D))

  )


(parameterise ((check-test-name	'hash))

  (define cs1
    (char-set #\a #\b #\c
	      #\A #\B #\C))

  #;(pretty-print cs1 (current-error-port))

  (check (char-set-hash (char-set))	=> 0)
  (check (char-set-hash (char-set) 2)	=> 0)

  (check (char-set-hash cs1)		=> 328)
  (check (char-set-hash cs1 10)		=> 8)

  #t)


(parameterise ((check-test-name	'hash))

  (check
      (let ((cs  (char-set))
	    (ell '()))
	(do ((iter (char-set-cursor cs) (char-set-cursor-next iter)))
	    ((end-of-char-set? iter)
	     ell)
	  (set! ell (cons (char-set-ref iter) ell))))
    => '())

  (check
      (let ((cs  (char-set #\a))
	    (ell '()))
	(do ((iter (char-set-cursor cs) (char-set-cursor-next iter)))
	    ((end-of-char-set? iter)
	     ell)
	  (set! ell (cons (char-set-ref iter) ell))))
    => '(#\a))

  (check
      (let ((cs  (char-set '(#\a . #\d)))
	    (ell '()))
	(do ((iter (char-set-cursor cs) (char-set-cursor-next iter)))
	    ((end-of-char-set? iter)
	     ell)
	  (set! ell (cons (char-set-ref iter) ell))))
    => (reverse '(#\a #\b #\c #\d)))

  (check
      (let ((cs  (char-set '(#\a . #\c)
			   '(#\e . #\g)
			   '(#\i . #\m)))
	    (ell '()))
	(do ((iter (char-set-cursor cs) (char-set-cursor-next iter)))
	    ((end-of-char-set? iter)
	     ell)
	  (set! ell (cons (char-set-ref iter) ell))))
    => (reverse '(#\a #\b #\c #\e #\f #\g #\i #\j #\k #\l #\m)))

  #t)


(parameterise ((check-test-name 'categories))

  (check (char-set? char-set:category/letter-uppercase) => #t)
  (check (char-set? char-set:category/letter-lowercase) => #t)
  (check (char-set? char-set:category/letter-titlecase) => #t)
  (check (char-set? char-set:category/letter-modifier) => #t)
  (check (char-set? char-set:category/letter-other) => #t)
  (check (char-set? char-set:category/mark-nospacing) => #t)
  (check (char-set? char-set:category/mark-spacing-combining) => #t)
  (check (char-set? char-set:category/mark-enclosing) => #t)
  (check (char-set? char-set:category/number-decimal-digit) => #t)
  (check (char-set? char-set:category/number-letter) => #t)
  (check (char-set? char-set:category/number-other) => #t)
  (check (char-set? char-set:category/puncutation-connector) => #t)
  (check (char-set? char-set:category/punctuation-dash) => #t)
  (check (char-set? char-set:category/punctuation-open) => #t)
  (check (char-set? char-set:category/punctuation-close) => #t)
  (check (char-set? char-set:category/punctuation-initial-quote) => #t)
  (check (char-set? char-set:category/punctuation-final-quote) => #t)
  (check (char-set? char-set:category/punctuation-other) => #t)
  (check (char-set? char-set:category/symbol-math) => #t)
  (check (char-set? char-set:category/symbol-currency) => #t)
  (check (char-set? char-set:category/symbol-modifier) => #t)
  (check (char-set? char-set:category/symbol-other) => #t)
  (check (char-set? char-set:category/separator-space) => #t)
  (check (char-set? char-set:category/separator-line) => #t)
  (check (char-set? char-set:category/separator-paragraph) => #t)
  (check (char-set? char-set:category/control) => #t)
  (check (char-set? char-set:category/format) => #t)
  (check (char-set? char-set:category/private-use) => #t)
  (check (char-set? char-set:category/not-assigned) => #t)

  (check
      (char-set-intersection char-set:category/letter-uppercase
			     char-set:category/letter-lowercase
			     char-set:category/letter-titlecase
			     char-set:category/letter-modifier
			     char-set:category/letter-other
			     char-set:category/mark-nospacing
			     char-set:category/mark-spacing-combining
			     char-set:category/mark-enclosing
			     char-set:category/number-decimal-digit
			     char-set:category/number-letter
			     char-set:category/number-other
			     char-set:category/puncutation-connector
			     char-set:category/punctuation-dash
			     char-set:category/punctuation-open
			     char-set:category/punctuation-close
			     char-set:category/punctuation-initial-quote
			     char-set:category/punctuation-final-quote
			     char-set:category/punctuation-other
			     char-set:category/symbol-math
			     char-set:category/symbol-currency
			     char-set:category/symbol-modifier
			     char-set:category/symbol-other
			     char-set:category/separator-space
			     char-set:category/separator-line
			     char-set:category/separator-paragraph
			     char-set:category/control
			     char-set:category/format
			     char-set:category/private-use
			     char-set:category/not-assigned
			     char-set:category/surrogates)
    (=> char-set=?) char-set:empty)

  (check
      (char-set? (char-set-union char-set:category/letter-uppercase
				 char-set:category/letter-lowercase
				 char-set:category/letter-titlecase
				 char-set:category/letter-modifier
				 char-set:category/letter-other
				 char-set:category/mark-nospacing
				 char-set:category/mark-spacing-combining
				 char-set:category/mark-enclosing
				 char-set:category/number-decimal-digit
				 char-set:category/number-letter
				 char-set:category/number-other
				 char-set:category/puncutation-connector
				 char-set:category/punctuation-dash
				 char-set:category/punctuation-open
				 char-set:category/punctuation-close
				 char-set:category/punctuation-initial-quote
				 char-set:category/punctuation-final-quote
				 char-set:category/punctuation-other
				 char-set:category/symbol-math
				 char-set:category/symbol-currency
				 char-set:category/symbol-modifier
				 char-set:category/symbol-other
				 char-set:category/separator-space
				 char-set:category/separator-line
				 char-set:category/separator-paragraph
				 char-set:category/control
				 char-set:category/format
				 char-set:category/private-use
				 char-set:category/not-assigned
				 char-set:category/surrogates))
    => #t)

  (check
      (char-set? (char-set-difference char-set:full
				      char-set:category/letter-uppercase
				      char-set:category/letter-lowercase
				      char-set:category/letter-titlecase
				      char-set:category/letter-modifier
				      char-set:category/letter-other
				      char-set:category/mark-nospacing
				      char-set:category/mark-spacing-combining
				      char-set:category/mark-enclosing
				      char-set:category/number-decimal-digit
				      char-set:category/number-letter
				      char-set:category/number-other
				      char-set:category/puncutation-connector
				      char-set:category/punctuation-dash
				      char-set:category/punctuation-open
				      char-set:category/punctuation-close
				      char-set:category/punctuation-initial-quote
				      char-set:category/punctuation-final-quote
				      char-set:category/punctuation-other
				      char-set:category/symbol-math
				      char-set:category/symbol-currency
				      char-set:category/symbol-modifier
				      char-set:category/symbol-other
				      char-set:category/separator-space
				      char-set:category/separator-line
				      char-set:category/separator-paragraph
				      char-set:category/control
				      char-set:category/format
				      char-set:category/private-use
				      char-set:category/not-assigned
				      char-set:category/surrogates))
    => #t)

  (check
      (char-set? (char-set-complement
		  (char-set-union
		   char-set:category/letter-uppercase
		   char-set:category/letter-lowercase
		   char-set:category/letter-titlecase
		   char-set:category/letter-modifier
		   char-set:category/letter-other
		   char-set:category/mark-nospacing
		   char-set:category/mark-spacing-combining
		   char-set:category/mark-enclosing
		   char-set:category/number-decimal-digit
		   char-set:category/number-letter
		   char-set:category/number-other
		   char-set:category/puncutation-connector
		   char-set:category/punctuation-dash
		   char-set:category/punctuation-open
		   char-set:category/punctuation-close
		   char-set:category/punctuation-initial-quote
		   char-set:category/punctuation-final-quote
		   char-set:category/punctuation-other
		   char-set:category/symbol-math
		   char-set:category/symbol-currency
		   char-set:category/symbol-modifier
		   char-set:category/symbol-other
		   char-set:category/separator-space
		   char-set:category/separator-line
		   char-set:category/separator-paragraph
		   char-set:category/control
		   char-set:category/format
		   char-set:category/private-use
		   char-set:category/not-assigned
		   char-set:category/surrogates)
		  char-set:full))
    => #t)

  )


(parameterise ((check-test-name 'blocks))

  (check (char-set? char-set:block/basic-latin) => #t)
  (check (char-set? char-set:block/latin-1-supplement) => #t)
  (check (char-set? char-set:block/latin-extended-a) => #t)
  (check (char-set? char-set:block/latin-extended-b) => #t)
  (check (char-set? char-set:block/ipa-extensions) => #t)
  (check (char-set? char-set:block/spacing-modifier-letters) => #t)
  (check (char-set? char-set:block/combining-diacritical-marks) => #t)
  (check (char-set? char-set:block/greek-and-coptic) => #t)
  (check (char-set? char-set:block/cyrillic) => #t)
  (check (char-set? char-set:block/cyrillic-supplement) => #t)
  (check (char-set? char-set:block/armenian) => #t)
  (check (char-set? char-set:block/hebrew) => #t)
  (check (char-set? char-set:block/arabic) => #t)
  (check (char-set? char-set:block/syriac) => #t)
  (check (char-set? char-set:block/arabic-supplement) => #t)
  (check (char-set? char-set:block/thaana) => #t)
  (check (char-set? char-set:block/nko) => #t)
  (check (char-set? char-set:block/devanagari) => #t)
  (check (char-set? char-set:block/bengali) => #t)
  (check (char-set? char-set:block/gurmukhi) => #t)
  (check (char-set? char-set:block/gujarati) => #t)
  (check (char-set? char-set:block/oriya) => #t)
  (check (char-set? char-set:block/tamil) => #t)
  (check (char-set? char-set:block/telugu) => #t)
  (check (char-set? char-set:block/kannada) => #t)
  (check (char-set? char-set:block/malayalam) => #t)
  (check (char-set? char-set:block/sinhala) => #t)
  (check (char-set? char-set:block/thai) => #t)
  (check (char-set? char-set:block/lao) => #t)
  (check (char-set? char-set:block/tibetan) => #t)
  (check (char-set? char-set:block/myanmar) => #t)
  (check (char-set? char-set:block/georgian) => #t)
  (check (char-set? char-set:block/hangul-jamo) => #t)
  (check (char-set? char-set:block/ethiopic) => #t)
  (check (char-set? char-set:block/ethiopic-supplement) => #t)
  (check (char-set? char-set:block/cherokee) => #t)
  (check (char-set? char-set:block/unified-canadian-aboriginal-syllabics) => #t)
  (check (char-set? char-set:block/ogham) => #t)
  (check (char-set? char-set:block/runic) => #t)
  (check (char-set? char-set:block/tagalog) => #t)
  (check (char-set? char-set:block/hanunoo) => #t)
  (check (char-set? char-set:block/buhid) => #t)
  (check (char-set? char-set:block/tagbanwa) => #t)
  (check (char-set? char-set:block/khmer) => #t)
  (check (char-set? char-set:block/mongolian) => #t)
  (check (char-set? char-set:block/limbu) => #t)
  (check (char-set? char-set:block/tai-le) => #t)
  (check (char-set? char-set:block/new-tai-lue) => #t)
  (check (char-set? char-set:block/khmer-symbols) => #t)
  (check (char-set? char-set:block/buginese) => #t)
  (check (char-set? char-set:block/balinese) => #t)
  (check (char-set? char-set:block/sundanese) => #t)
  (check (char-set? char-set:block/lepcha) => #t)
  (check (char-set? char-set:block/ol-chiki) => #t)
  (check (char-set? char-set:block/phonetic-extensions) => #t)
  (check (char-set? char-set:block/phonetic-extensions-supplement) => #t)
  (check (char-set? char-set:block/combining-diacritical-marks-supplement) => #t)
  (check (char-set? char-set:block/latin-extended-additional) => #t)
  (check (char-set? char-set:block/greek-extended) => #t)
  (check (char-set? char-set:block/general-punctuation) => #t)
  (check (char-set? char-set:block/superscripts-and-subscripts) => #t)
  (check (char-set? char-set:block/currency-symbols) => #t)
  (check (char-set? char-set:block/combining-diacritical-mark-for-symbols) => #t)
  (check (char-set? char-set:block/letterlike-symbols) => #t)
  (check (char-set? char-set:block/number-forms) => #t)
  (check (char-set? char-set:block/arrows) => #t)
  (check (char-set? char-set:block/mathematical-operators) => #t)
  (check (char-set? char-set:block/miscellaneous-technical) => #t)
  (check (char-set? char-set:block/control-pictures) => #t)
  (check (char-set? char-set:block/optical-character-recognition) => #t)
  (check (char-set? char-set:block/enclosed-alphanumerics) => #t)
  (check (char-set? char-set:block/box-drawing) => #t)
  (check (char-set? char-set:block/block-elements) => #t)
  (check (char-set? char-set:block/geometric-shapes) => #t)
  (check (char-set? char-set:block/miscellaneous-symbols) => #t)
  (check (char-set? char-set:block/dingbats) => #t)
  (check (char-set? char-set:block/miscellaneous-mathematical-symbols-a) => #t)
  (check (char-set? char-set:block/supplemental-arrows-a) => #t)
  (check (char-set? char-set:block/braille-patterns) => #t)
  (check (char-set? char-set:block/supplemental-arrows-b) => #t)
  (check (char-set? char-set:block/miscellaneous-mathematical-symbols-b) => #t)
  (check (char-set? char-set:block/supplemental-mathematical-operators) => #t)
  (check (char-set? char-set:block/miscellaneous-symbols-and-arrows) => #t)
  (check (char-set? char-set:block/glagolitic) => #t)
  (check (char-set? char-set:block/latin-extended-c) => #t)
  (check (char-set? char-set:block/coptic) => #t)
  (check (char-set? char-set:block/georgian-supplement) => #t)
  (check (char-set? char-set:block/tifinagh) => #t)
  (check (char-set? char-set:block/ethiopic-extended) => #t)
  (check (char-set? char-set:block/cyrillic-extended-a) => #t)
  (check (char-set? char-set:block/supplemental-punctuation) => #t)
  (check (char-set? char-set:block/cjk-radicals-supplement) => #t)
  (check (char-set? char-set:block/kangxi-radicals) => #t)
  (check (char-set? char-set:block/ideographic-description-characters) => #t)
  (check (char-set? char-set:block/cjk-symbols-and-punctuation) => #t)
  (check (char-set? char-set:block/hiragana) => #t)
  (check (char-set? char-set:block/katakana) => #t)
  (check (char-set? char-set:block/bopomofo) => #t)
  (check (char-set? char-set:block/hangul-compatibility-jamo) => #t)
  (check (char-set? char-set:block/kanbun) => #t)
  (check (char-set? char-set:block/bopomofo-extended) => #t)
  (check (char-set? char-set:block/cjk-strokes) => #t)
  (check (char-set? char-set:block/katakana-phonetic-extensions) => #t)
  (check (char-set? char-set:block/enclosed-cjk-letters-and-months) => #t)
  (check (char-set? char-set:block/cjk-compatibility) => #t)
  (check (char-set? char-set:block/cjk-unified-Ideographs-extension-a) => #t)
  (check (char-set? char-set:block/yijing-hexagram-symbols) => #t)
  (check (char-set? char-set:block/cjk-unified-ideographs) => #t)
  (check (char-set? char-set:block/yi-syllables) => #t)
  (check (char-set? char-set:block/yi-radicals) => #t)
  (check (char-set? char-set:block/vai) => #t)
  (check (char-set? char-set:block/cyrillic-extended-b) => #t)
  (check (char-set? char-set:block/modifier-tone-letters) => #t)
  (check (char-set? char-set:block/latin-extended-d) => #t)
  (check (char-set? char-set:block/syloti-nagri) => #t)
  (check (char-set? char-set:block/phags-pa) => #t)
  (check (char-set? char-set:block/saurashtra) => #t)
  (check (char-set? char-set:block/kayah-li) => #t)
  (check (char-set? char-set:block/Rejang) => #t)
  (check (char-set? char-set:block/cham) => #t)
  (check (char-set? char-set:block/hangul-syllables) => #t)
  (check (char-set? char-set:block/private-use-area) => #t)
  (check (char-set? char-set:block/cjk-compatibility-ideographs) => #t)
  (check (char-set? char-set:block/alphabetic-presentation-forms) => #t)
  (check (char-set? char-set:block/arabic-presentation-forms-a) => #t)
  (check (char-set? char-set:block/variation-selectors) => #t)
  (check (char-set? char-set:block/vertical-forms) => #t)
  (check (char-set? char-set:block/combining-half-marks) => #t)
  (check (char-set? char-set:block/cjk-compatibility-forms) => #t)
  (check (char-set? char-set:block/small-form-variants) => #t)
  (check (char-set? char-set:block/arabic-presentation-forms-b) => #t)
  (check (char-set? char-set:block/halfwidth-and-fullwidth-forms) => #t)
  (check (char-set? char-set:block/specials) => #t)
  (check (char-set? char-set:block/linear-b-syllabary) => #t)
  (check (char-set? char-set:block/linear-b-ideograms) => #t)
  (check (char-set? char-set:block/aegean-numbers) => #t)
  (check (char-set? char-set:block/ancient-greek-numbers) => #t)
  (check (char-set? char-set:block/ancient-symbols) => #t)
  (check (char-set? char-set:block/phaistos-disc) => #t)
  (check (char-set? char-set:block/lycian) => #t)
  (check (char-set? char-set:block/carian) => #t)
  (check (char-set? char-set:block/old-italic) => #t)
  (check (char-set? char-set:block/gothic) => #t)
  (check (char-set? char-set:block/ugaritic) => #t)
  (check (char-set? char-set:block/old-persian) => #t)
  (check (char-set? char-set:block/deseret) => #t)
  (check (char-set? char-set:block/shavian) => #t)
  (check (char-set? char-set:block/osmanya) => #t)
  (check (char-set? char-set:block/cypriot-syllabary) => #t)
  (check (char-set? char-set:block/phoenician) => #t)
  (check (char-set? char-set:block/lydian) => #t)
  (check (char-set? char-set:block/kharoshthi) => #t)
  (check (char-set? char-set:block/cuneiform) => #t)
  (check (char-set? char-set:block/cuneiform-numbers-and-punctuation) => #t)
  (check (char-set? char-set:block/byzantine-musical-symbols) => #t)
  (check (char-set? char-set:block/musical-symbols) => #t)
  (check (char-set? char-set:block/ancient-greek-musical-notation) => #t)
  (check (char-set? char-set:block/tai-xuan-jing-symbols) => #t)
  (check (char-set? char-set:block/counting-rod-numerals) => #t)
  (check (char-set? char-set:block/mathematical-alphanumeric-symbols) => #t)
  (check (char-set? char-set:block/mahjong-tiles) => #t)
  (check (char-set? char-set:block/domino-tiles) => #t)
  (check (char-set? char-set:block/cjk-unified-ideographs-extension-b) => #t)
  (check (char-set? char-set:block/cjk-compatibility-ideographs-supplement) => #t)
  (check (char-set? char-set:block/tags) => #t)
  (check (char-set? char-set:block/variation-selectors-supplement) => #t)
  (check (char-set? char-set:block/supplementary-private-use-area-a) => #t)
  (check (char-set? char-set:block/supplementary-private-use-area-b) => #t)

  (check
      (char-set-intersection char-set:block/basic-latin
			     char-set:block/latin-1-supplement
			     char-set:block/latin-extended-a
			     char-set:block/latin-extended-b
			     char-set:block/ipa-extensions
			     char-set:block/spacing-modifier-letters
			     char-set:block/combining-diacritical-marks
			     char-set:block/greek-and-coptic
			     char-set:block/cyrillic
			     char-set:block/cyrillic-supplement
			     char-set:block/armenian
			     char-set:block/hebrew
			     char-set:block/arabic
			     char-set:block/syriac
			     char-set:block/arabic-supplement
			     char-set:block/thaana
			     char-set:block/nko
			     char-set:block/devanagari
			     char-set:block/bengali
			     char-set:block/gurmukhi
			     char-set:block/gujarati
			     char-set:block/oriya
			     char-set:block/tamil
			     char-set:block/telugu
			     char-set:block/kannada
			     char-set:block/malayalam
			     char-set:block/sinhala
			     char-set:block/thai
			     char-set:block/lao
			     char-set:block/tibetan
			     char-set:block/myanmar
			     char-set:block/georgian
			     char-set:block/hangul-jamo
			     char-set:block/ethiopic
			     char-set:block/ethiopic-supplement
			     char-set:block/cherokee
			     char-set:block/unified-canadian-aboriginal-syllabics
			     char-set:block/ogham
			     char-set:block/runic
			     char-set:block/tagalog
			     char-set:block/hanunoo
			     char-set:block/buhid
			     char-set:block/tagbanwa
			     char-set:block/khmer
			     char-set:block/mongolian
			     char-set:block/limbu
			     char-set:block/tai-le
			     char-set:block/new-tai-lue
			     char-set:block/khmer-symbols
			     char-set:block/buginese
			     char-set:block/balinese
			     char-set:block/sundanese
			     char-set:block/lepcha
			     char-set:block/ol-chiki
			     char-set:block/phonetic-extensions
			     char-set:block/phonetic-extensions-supplement
			     char-set:block/combining-diacritical-marks-supplement
			     char-set:block/latin-extended-additional
			     char-set:block/greek-extended
			     char-set:block/general-punctuation
			     char-set:block/superscripts-and-subscripts
			     char-set:block/currency-symbols
			     char-set:block/combining-diacritical-mark-for-symbols
			     char-set:block/letterlike-symbols
			     char-set:block/number-forms
			     char-set:block/arrows
			     char-set:block/mathematical-operators
			     char-set:block/miscellaneous-technical
			     char-set:block/control-pictures
			     char-set:block/optical-character-recognition
			     char-set:block/enclosed-alphanumerics
			     char-set:block/box-drawing
			     char-set:block/block-elements
			     char-set:block/geometric-shapes
			     char-set:block/miscellaneous-symbols
			     char-set:block/dingbats
			     char-set:block/miscellaneous-mathematical-symbols-a
			     char-set:block/supplemental-arrows-a
			     char-set:block/braille-patterns
			     char-set:block/supplemental-arrows-b
			     char-set:block/miscellaneous-mathematical-symbols-b
			     char-set:block/supplemental-mathematical-operators
			     char-set:block/miscellaneous-symbols-and-arrows
			     char-set:block/glagolitic
			     char-set:block/latin-extended-c
			     char-set:block/coptic
			     char-set:block/georgian-supplement
			     char-set:block/tifinagh
			     char-set:block/ethiopic-extended
			     char-set:block/cyrillic-extended-a
			     char-set:block/supplemental-punctuation
			     char-set:block/cjk-radicals-supplement
			     char-set:block/kangxi-radicals
			     char-set:block/ideographic-description-characters
			     char-set:block/cjk-symbols-and-punctuation
			     char-set:block/hiragana
			     char-set:block/katakana
			     char-set:block/bopomofo
			     char-set:block/hangul-compatibility-jamo
			     char-set:block/kanbun
			     char-set:block/bopomofo-extended
			     char-set:block/cjk-strokes
			     char-set:block/katakana-phonetic-extensions
			     char-set:block/enclosed-cjk-letters-and-months
			     char-set:block/cjk-compatibility
			     char-set:block/cjk-unified-Ideographs-extension-a
			     char-set:block/yijing-hexagram-symbols
			     char-set:block/cjk-unified-ideographs
			     char-set:block/yi-syllables
			     char-set:block/yi-radicals
			     char-set:block/vai
			     char-set:block/cyrillic-extended-b
			     char-set:block/modifier-tone-letters
			     char-set:block/latin-extended-d
			     char-set:block/syloti-nagri
			     char-set:block/phags-pa
			     char-set:block/saurashtra
			     char-set:block/kayah-li
			     char-set:block/Rejang
			     char-set:block/cham
			     char-set:block/hangul-syllables
			     char-set:block/private-use-area
			     char-set:block/cjk-compatibility-ideographs
			     char-set:block/alphabetic-presentation-forms
			     char-set:block/arabic-presentation-forms-a
			     char-set:block/variation-selectors
			     char-set:block/vertical-forms
			     char-set:block/combining-half-marks
			     char-set:block/cjk-compatibility-forms
			     char-set:block/small-form-variants
			     char-set:block/arabic-presentation-forms-b
			     char-set:block/halfwidth-and-fullwidth-forms
			     char-set:block/specials
			     char-set:block/linear-b-syllabary
			     char-set:block/linear-b-ideograms
			     char-set:block/aegean-numbers
			     char-set:block/ancient-greek-numbers
			     char-set:block/ancient-symbols
			     char-set:block/phaistos-disc
			     char-set:block/lycian
			     char-set:block/carian
			     char-set:block/old-italic
			     char-set:block/gothic
			     char-set:block/ugaritic
			     char-set:block/old-persian
			     char-set:block/deseret
			     char-set:block/shavian
			     char-set:block/osmanya
			     char-set:block/cypriot-syllabary
			     char-set:block/phoenician
			     char-set:block/lydian
			     char-set:block/kharoshthi
			     char-set:block/cuneiform
			     char-set:block/cuneiform-numbers-and-punctuation
			     char-set:block/byzantine-musical-symbols
			     char-set:block/musical-symbols
			     char-set:block/ancient-greek-musical-notation
			     char-set:block/tai-xuan-jing-symbols
			     char-set:block/counting-rod-numerals
			     char-set:block/mathematical-alphanumeric-symbols
			     char-set:block/mahjong-tiles
			     char-set:block/domino-tiles
			     char-set:block/cjk-unified-ideographs-extension-b
			     char-set:block/cjk-compatibility-ideographs-supplement
			     char-set:block/tags
			     char-set:block/variation-selectors-supplement
			     char-set:block/supplementary-private-use-area-a
			     char-set:block/supplementary-private-use-area-b)
    (=> char-set=?) char-set:empty)

  (check
      (char-set? (char-set-union char-set:block/basic-latin
				 char-set:block/latin-1-supplement
				 char-set:block/latin-extended-a
				 char-set:block/latin-extended-b
				 char-set:block/ipa-extensions
				 char-set:block/spacing-modifier-letters
				 char-set:block/combining-diacritical-marks
				 char-set:block/greek-and-coptic
				 char-set:block/cyrillic
				 char-set:block/cyrillic-supplement
				 char-set:block/armenian
				 char-set:block/hebrew
				 char-set:block/arabic
				 char-set:block/syriac
				 char-set:block/arabic-supplement
				 char-set:block/thaana
				 char-set:block/nko
				 char-set:block/devanagari
				 char-set:block/bengali
				 char-set:block/gurmukhi
				 char-set:block/gujarati
				 char-set:block/oriya
				 char-set:block/tamil
				 char-set:block/telugu
				 char-set:block/kannada
				 char-set:block/malayalam
				 char-set:block/sinhala
				 char-set:block/thai
				 char-set:block/lao
				 char-set:block/tibetan
				 char-set:block/myanmar
				 char-set:block/georgian
				 char-set:block/hangul-jamo
				 char-set:block/ethiopic
				 char-set:block/ethiopic-supplement
				 char-set:block/cherokee
				 char-set:block/unified-canadian-aboriginal-syllabics
				 char-set:block/ogham
				 char-set:block/runic
				 char-set:block/tagalog
				 char-set:block/hanunoo
				 char-set:block/buhid
				 char-set:block/tagbanwa
				 char-set:block/khmer
				 char-set:block/mongolian
				 char-set:block/limbu
				 char-set:block/tai-le
				 char-set:block/new-tai-lue
				 char-set:block/khmer-symbols
				 char-set:block/buginese
				 char-set:block/balinese
				 char-set:block/sundanese
				 char-set:block/lepcha
				 char-set:block/ol-chiki
				 char-set:block/phonetic-extensions
				 char-set:block/phonetic-extensions-supplement
				 char-set:block/combining-diacritical-marks-supplement
				 char-set:block/latin-extended-additional
				 char-set:block/greek-extended
				 char-set:block/general-punctuation
				 char-set:block/superscripts-and-subscripts
				 char-set:block/currency-symbols
				 char-set:block/combining-diacritical-mark-for-symbols
				 char-set:block/letterlike-symbols
				 char-set:block/number-forms
				 char-set:block/arrows
				 char-set:block/mathematical-operators
				 char-set:block/miscellaneous-technical
				 char-set:block/control-pictures
				 char-set:block/optical-character-recognition
				 char-set:block/enclosed-alphanumerics
				 char-set:block/box-drawing
				 char-set:block/block-elements
				 char-set:block/geometric-shapes
				 char-set:block/miscellaneous-symbols
				 char-set:block/dingbats
				 char-set:block/miscellaneous-mathematical-symbols-a
				 char-set:block/supplemental-arrows-a
				 char-set:block/braille-patterns
				 char-set:block/supplemental-arrows-b
				 char-set:block/miscellaneous-mathematical-symbols-b
				 char-set:block/supplemental-mathematical-operators
				 char-set:block/miscellaneous-symbols-and-arrows
				 char-set:block/glagolitic
				 char-set:block/latin-extended-c
				 char-set:block/coptic
				 char-set:block/georgian-supplement
				 char-set:block/tifinagh
				 char-set:block/ethiopic-extended
				 char-set:block/cyrillic-extended-a
				 char-set:block/supplemental-punctuation
				 char-set:block/cjk-radicals-supplement
				 char-set:block/kangxi-radicals
				 char-set:block/ideographic-description-characters
				 char-set:block/cjk-symbols-and-punctuation
				 char-set:block/hiragana
				 char-set:block/katakana
				 char-set:block/bopomofo
				 char-set:block/hangul-compatibility-jamo
				 char-set:block/kanbun
				 char-set:block/bopomofo-extended
				 char-set:block/cjk-strokes
				 char-set:block/katakana-phonetic-extensions
				 char-set:block/enclosed-cjk-letters-and-months
				 char-set:block/cjk-compatibility
				 char-set:block/cjk-unified-Ideographs-extension-a
				 char-set:block/yijing-hexagram-symbols
				 char-set:block/cjk-unified-ideographs
				 char-set:block/yi-syllables
				 char-set:block/yi-radicals
				 char-set:block/vai
				 char-set:block/cyrillic-extended-b
				 char-set:block/modifier-tone-letters
				 char-set:block/latin-extended-d
				 char-set:block/syloti-nagri
				 char-set:block/phags-pa
				 char-set:block/saurashtra
				 char-set:block/kayah-li
				 char-set:block/Rejang
				 char-set:block/cham
				 char-set:block/hangul-syllables
				 char-set:block/private-use-area
				 char-set:block/cjk-compatibility-ideographs
				 char-set:block/alphabetic-presentation-forms
				 char-set:block/arabic-presentation-forms-a
				 char-set:block/variation-selectors
				 char-set:block/vertical-forms
				 char-set:block/combining-half-marks
				 char-set:block/cjk-compatibility-forms
				 char-set:block/small-form-variants
				 char-set:block/arabic-presentation-forms-b
				 char-set:block/halfwidth-and-fullwidth-forms
				 char-set:block/specials
				 char-set:block/linear-b-syllabary
				 char-set:block/linear-b-ideograms
				 char-set:block/aegean-numbers
				 char-set:block/ancient-greek-numbers
				 char-set:block/ancient-symbols
				 char-set:block/phaistos-disc
				 char-set:block/lycian
				 char-set:block/carian
				 char-set:block/old-italic
				 char-set:block/gothic
				 char-set:block/ugaritic
				 char-set:block/old-persian
				 char-set:block/deseret
				 char-set:block/shavian
				 char-set:block/osmanya
				 char-set:block/cypriot-syllabary
				 char-set:block/phoenician
				 char-set:block/lydian
				 char-set:block/kharoshthi
				 char-set:block/cuneiform
				 char-set:block/cuneiform-numbers-and-punctuation
				 char-set:block/byzantine-musical-symbols
				 char-set:block/musical-symbols
				 char-set:block/ancient-greek-musical-notation
				 char-set:block/tai-xuan-jing-symbols
				 char-set:block/counting-rod-numerals
				 char-set:block/mathematical-alphanumeric-symbols
				 char-set:block/mahjong-tiles
				 char-set:block/domino-tiles
				 char-set:block/cjk-unified-ideographs-extension-b
				 char-set:block/cjk-compatibility-ideographs-supplement
				 char-set:block/tags
				 char-set:block/variation-selectors-supplement
				 char-set:block/supplementary-private-use-area-a
				 char-set:block/supplementary-private-use-area-b))
    => #t)

  (check
      (char-set? (char-set-difference char-set:full
				      char-set:block/basic-latin
				      char-set:block/latin-1-supplement
				      char-set:block/latin-extended-a
				      char-set:block/latin-extended-b
				      char-set:block/ipa-extensions
				      char-set:block/spacing-modifier-letters
				      char-set:block/combining-diacritical-marks
				      char-set:block/greek-and-coptic
				      char-set:block/cyrillic
				      char-set:block/cyrillic-supplement
				      char-set:block/armenian
				      char-set:block/hebrew
				      char-set:block/arabic
				      char-set:block/syriac
				      char-set:block/arabic-supplement
				      char-set:block/thaana
				      char-set:block/nko
				      char-set:block/devanagari
				      char-set:block/bengali
				      char-set:block/gurmukhi
				      char-set:block/gujarati
				      char-set:block/oriya
				      char-set:block/tamil
				      char-set:block/telugu
				      char-set:block/kannada
				      char-set:block/malayalam
				      char-set:block/sinhala
				      char-set:block/thai
				      char-set:block/lao
				      char-set:block/tibetan
				      char-set:block/myanmar
				      char-set:block/georgian
				      char-set:block/hangul-jamo
				      char-set:block/ethiopic
				      char-set:block/ethiopic-supplement
				      char-set:block/cherokee
				      char-set:block/unified-canadian-aboriginal-syllabics
				      char-set:block/ogham
				      char-set:block/runic
				      char-set:block/tagalog
				      char-set:block/hanunoo
				      char-set:block/buhid
				      char-set:block/tagbanwa
				      char-set:block/khmer
				      char-set:block/mongolian
				      char-set:block/limbu
				      char-set:block/tai-le
				      char-set:block/new-tai-lue
				      char-set:block/khmer-symbols
				      char-set:block/buginese
				      char-set:block/balinese
				      char-set:block/sundanese
				      char-set:block/lepcha
				      char-set:block/ol-chiki
				      char-set:block/phonetic-extensions
				      char-set:block/phonetic-extensions-supplement
				      char-set:block/combining-diacritical-marks-supplement
				      char-set:block/latin-extended-additional
				      char-set:block/greek-extended
				      char-set:block/general-punctuation
				      char-set:block/superscripts-and-subscripts
				      char-set:block/currency-symbols
				      char-set:block/combining-diacritical-mark-for-symbols
				      char-set:block/letterlike-symbols
				      char-set:block/number-forms
				      char-set:block/arrows
				      char-set:block/mathematical-operators
				      char-set:block/miscellaneous-technical
				      char-set:block/control-pictures
				      char-set:block/optical-character-recognition
				      char-set:block/enclosed-alphanumerics
				      char-set:block/box-drawing
				      char-set:block/block-elements
				      char-set:block/geometric-shapes
				      char-set:block/miscellaneous-symbols
				      char-set:block/dingbats
				      char-set:block/miscellaneous-mathematical-symbols-a
				      char-set:block/supplemental-arrows-a
				      char-set:block/braille-patterns
				      char-set:block/supplemental-arrows-b
				      char-set:block/miscellaneous-mathematical-symbols-b
				      char-set:block/supplemental-mathematical-operators
				      char-set:block/miscellaneous-symbols-and-arrows
				      char-set:block/glagolitic
				      char-set:block/latin-extended-c
				      char-set:block/coptic
				      char-set:block/georgian-supplement
				      char-set:block/tifinagh
				      char-set:block/ethiopic-extended
				      char-set:block/cyrillic-extended-a
				      char-set:block/supplemental-punctuation
				      char-set:block/cjk-radicals-supplement
				      char-set:block/kangxi-radicals
				      char-set:block/ideographic-description-characters
				      char-set:block/cjk-symbols-and-punctuation
				      char-set:block/hiragana
				      char-set:block/katakana
				      char-set:block/bopomofo
				      char-set:block/hangul-compatibility-jamo
				      char-set:block/kanbun
				      char-set:block/bopomofo-extended
				      char-set:block/cjk-strokes
				      char-set:block/katakana-phonetic-extensions
				      char-set:block/enclosed-cjk-letters-and-months
				      char-set:block/cjk-compatibility
				      char-set:block/cjk-unified-Ideographs-extension-a
				      char-set:block/yijing-hexagram-symbols
				      char-set:block/cjk-unified-ideographs
				      char-set:block/yi-syllables
				      char-set:block/yi-radicals
				      char-set:block/vai
				      char-set:block/cyrillic-extended-b
				      char-set:block/modifier-tone-letters
				      char-set:block/latin-extended-d
				      char-set:block/syloti-nagri
				      char-set:block/phags-pa
				      char-set:block/saurashtra
				      char-set:block/kayah-li
				      char-set:block/Rejang
				      char-set:block/cham
				      char-set:block/hangul-syllables
				      char-set:block/private-use-area
				      char-set:block/cjk-compatibility-ideographs
				      char-set:block/alphabetic-presentation-forms
				      char-set:block/arabic-presentation-forms-a
				      char-set:block/variation-selectors
				      char-set:block/vertical-forms
				      char-set:block/combining-half-marks
				      char-set:block/cjk-compatibility-forms
				      char-set:block/small-form-variants
				      char-set:block/arabic-presentation-forms-b
				      char-set:block/halfwidth-and-fullwidth-forms
				      char-set:block/specials
				      char-set:block/linear-b-syllabary
				      char-set:block/linear-b-ideograms
				      char-set:block/aegean-numbers
				      char-set:block/ancient-greek-numbers
				      char-set:block/ancient-symbols
				      char-set:block/phaistos-disc
				      char-set:block/lycian
				      char-set:block/carian
				      char-set:block/old-italic
				      char-set:block/gothic
				      char-set:block/ugaritic
				      char-set:block/old-persian
				      char-set:block/deseret
				      char-set:block/shavian
				      char-set:block/osmanya
				      char-set:block/cypriot-syllabary
				      char-set:block/phoenician
				      char-set:block/lydian
				      char-set:block/kharoshthi
				      char-set:block/cuneiform
				      char-set:block/cuneiform-numbers-and-punctuation
				      char-set:block/byzantine-musical-symbols
				      char-set:block/musical-symbols
				      char-set:block/ancient-greek-musical-notation
				      char-set:block/tai-xuan-jing-symbols
				      char-set:block/counting-rod-numerals
				      char-set:block/mathematical-alphanumeric-symbols
				      char-set:block/mahjong-tiles
				      char-set:block/domino-tiles
				      char-set:block/cjk-unified-ideographs-extension-b
				      char-set:block/cjk-compatibility-ideographs-supplement
				      char-set:block/tags
				      char-set:block/variation-selectors-supplement
				      char-set:block/supplementary-private-use-area-a
				      char-set:block/supplementary-private-use-area-b))
    => #t)

  (check
      (char-set? (char-set-complement
		  (char-set-union char-set:block/basic-latin
				  char-set:block/latin-1-supplement
				  char-set:block/latin-extended-a
				  char-set:block/latin-extended-b
				  char-set:block/ipa-extensions
				  char-set:block/spacing-modifier-letters
				  char-set:block/combining-diacritical-marks
				  char-set:block/greek-and-coptic
				  char-set:block/cyrillic
				  char-set:block/cyrillic-supplement
				  char-set:block/armenian
				  char-set:block/hebrew
				  char-set:block/arabic
				  char-set:block/syriac
				  char-set:block/arabic-supplement
				  char-set:block/thaana
				  char-set:block/nko
				  char-set:block/devanagari
				  char-set:block/bengali
				  char-set:block/gurmukhi
				  char-set:block/gujarati
				  char-set:block/oriya
				  char-set:block/tamil
				  char-set:block/telugu
				  char-set:block/kannada
				  char-set:block/malayalam
				  char-set:block/sinhala
				  char-set:block/thai
				  char-set:block/lao
				  char-set:block/tibetan
				  char-set:block/myanmar
				  char-set:block/georgian
				  char-set:block/hangul-jamo
				  char-set:block/ethiopic
				  char-set:block/ethiopic-supplement
				  char-set:block/cherokee
				  char-set:block/unified-canadian-aboriginal-syllabics
				  char-set:block/ogham
				  char-set:block/runic
				  char-set:block/tagalog
				  char-set:block/hanunoo
				  char-set:block/buhid
				  char-set:block/tagbanwa
				  char-set:block/khmer
				  char-set:block/mongolian
				  char-set:block/limbu
				  char-set:block/tai-le
				  char-set:block/new-tai-lue
				  char-set:block/khmer-symbols
				  char-set:block/buginese
				  char-set:block/balinese
				  char-set:block/sundanese
				  char-set:block/lepcha
				  char-set:block/ol-chiki
				  char-set:block/phonetic-extensions
				  char-set:block/phonetic-extensions-supplement
				  char-set:block/combining-diacritical-marks-supplement
				  char-set:block/latin-extended-additional
				  char-set:block/greek-extended
				  char-set:block/general-punctuation
				  char-set:block/superscripts-and-subscripts
				  char-set:block/currency-symbols
				  char-set:block/combining-diacritical-mark-for-symbols
				  char-set:block/letterlike-symbols
				  char-set:block/number-forms
				  char-set:block/arrows
				  char-set:block/mathematical-operators
				  char-set:block/miscellaneous-technical
				  char-set:block/control-pictures
				  char-set:block/optical-character-recognition
				  char-set:block/enclosed-alphanumerics
				  char-set:block/box-drawing
				  char-set:block/block-elements
				  char-set:block/geometric-shapes
				  char-set:block/miscellaneous-symbols
				  char-set:block/dingbats
				  char-set:block/miscellaneous-mathematical-symbols-a
				  char-set:block/supplemental-arrows-a
				  char-set:block/braille-patterns
				  char-set:block/supplemental-arrows-b
				  char-set:block/miscellaneous-mathematical-symbols-b
				  char-set:block/supplemental-mathematical-operators
				  char-set:block/miscellaneous-symbols-and-arrows
				  char-set:block/glagolitic
				  char-set:block/latin-extended-c
				  char-set:block/coptic
				  char-set:block/georgian-supplement
				  char-set:block/tifinagh
				  char-set:block/ethiopic-extended
				  char-set:block/cyrillic-extended-a
				  char-set:block/supplemental-punctuation
				  char-set:block/cjk-radicals-supplement
				  char-set:block/kangxi-radicals
				  char-set:block/ideographic-description-characters
				  char-set:block/cjk-symbols-and-punctuation
				  char-set:block/hiragana
				  char-set:block/katakana
				  char-set:block/bopomofo
				  char-set:block/hangul-compatibility-jamo
				  char-set:block/kanbun
				  char-set:block/bopomofo-extended
				  char-set:block/cjk-strokes
				  char-set:block/katakana-phonetic-extensions
				  char-set:block/enclosed-cjk-letters-and-months
				  char-set:block/cjk-compatibility
				  char-set:block/cjk-unified-Ideographs-extension-a
				  char-set:block/yijing-hexagram-symbols
				  char-set:block/cjk-unified-ideographs
				  char-set:block/yi-syllables
				  char-set:block/yi-radicals
				  char-set:block/vai
				  char-set:block/cyrillic-extended-b
				  char-set:block/modifier-tone-letters
				  char-set:block/latin-extended-d
				  char-set:block/syloti-nagri
				  char-set:block/phags-pa
				  char-set:block/saurashtra
				  char-set:block/kayah-li
				  char-set:block/Rejang
				  char-set:block/cham
				  char-set:block/hangul-syllables
				  char-set:block/private-use-area
				  char-set:block/cjk-compatibility-ideographs
				  char-set:block/alphabetic-presentation-forms
				  char-set:block/arabic-presentation-forms-a
				  char-set:block/variation-selectors
				  char-set:block/vertical-forms
				  char-set:block/combining-half-marks
				  char-set:block/cjk-compatibility-forms
				  char-set:block/small-form-variants
				  char-set:block/arabic-presentation-forms-b
				  char-set:block/halfwidth-and-fullwidth-forms
				  char-set:block/specials
				  char-set:block/linear-b-syllabary
				  char-set:block/linear-b-ideograms
				  char-set:block/aegean-numbers
				  char-set:block/ancient-greek-numbers
				  char-set:block/ancient-symbols
				  char-set:block/phaistos-disc
				  char-set:block/lycian
				  char-set:block/carian
				  char-set:block/old-italic
				  char-set:block/gothic
				  char-set:block/ugaritic
				  char-set:block/old-persian
				  char-set:block/deseret
				  char-set:block/shavian
				  char-set:block/osmanya
				  char-set:block/cypriot-syllabary
				  char-set:block/phoenician
				  char-set:block/lydian
				  char-set:block/kharoshthi
				  char-set:block/cuneiform
				  char-set:block/cuneiform-numbers-and-punctuation
				  char-set:block/byzantine-musical-symbols
				  char-set:block/musical-symbols
				  char-set:block/ancient-greek-musical-notation
				  char-set:block/tai-xuan-jing-symbols
				  char-set:block/counting-rod-numerals
				  char-set:block/mathematical-alphanumeric-symbols
				  char-set:block/mahjong-tiles
				  char-set:block/domino-tiles
				  char-set:block/cjk-unified-ideographs-extension-b
				  char-set:block/cjk-compatibility-ideographs-supplement
				  char-set:block/tags
				  char-set:block/variation-selectors-supplement
				  char-set:block/supplementary-private-use-area-a
				  char-set:block/supplementary-private-use-area-b)
		  char-set:full))
    => #t)


  )

;;;; done

(check-report)

;;; end of file
